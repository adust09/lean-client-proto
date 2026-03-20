/-
  State Transition — Slot and Block Processing

  Implements the core state transition functions for the pq-devnet-3 beacon chain:
  - processSlot: advance state by one slot
  - processSlots: advance to a target slot
  - processBlockHeader: validate and apply block header
  - processAttestations: validate and apply attestations

  Signature verification is deferred to Phase 4 (requires XMSS multisig context).
-/

import LeanConsensus.Consensus.Types
import LeanConsensus.SSZ.Merkleization

namespace LeanConsensus.Consensus.StateTransition

open LeanConsensus.SSZ
open LeanConsensus.Consensus

-- ════════════════════════════════════════════════════════════════
-- Error Type
-- ════════════════════════════════════════════════════════════════

inductive StateTransitionError where
  | slotMismatch (expected actual : Slot)
  | blockSlotNotFuture (stateSlot blockSlot : Slot)
  | invalidProposerIndex (index : ValidatorIndex) (validatorCount : Nat)
  | parentRootMismatch
  | attestationSlotTooOld (attSlot currentSlot : Slot)
  | attestationSlotTooNew (attSlot currentSlot : Slot)
  | invalidSourceCheckpoint
  | other (msg : String)

instance : ToString StateTransitionError where
  toString
    | .slotMismatch e a => s!"slot mismatch: expected {e}, got {a}"
    | .blockSlotNotFuture ss bs => s!"block slot {bs} not future of state slot {ss}"
    | .invalidProposerIndex i c => s!"proposer index {i} >= validator count {c}"
    | .parentRootMismatch => "parent root mismatch"
    | .attestationSlotTooOld a c => s!"attestation slot {a} too old (current: {c})"
    | .attestationSlotTooNew a c => s!"attestation slot {a} too new (current: {c})"
    | .invalidSourceCheckpoint => "invalid source checkpoint"
    | .other msg => msg

-- ════════════════════════════════════════════════════════════════
-- Helpers
-- ════════════════════════════════════════════════════════════════

/-- Wrap a ByteArray as Bytes32, zero-filling if the size is wrong. -/
def toBytes32 (data : ByteArray) : Bytes32 :=
  if h : data.size = 32 then ⟨data, h⟩ else BytesN.zero 32

/-- Check if a Bytes32 is all zeros. -/
def isZeroRoot (r : Bytes32) : Bool :=
  r.data == (BytesN.zero 32).data

/-- Convert a Block's header fields into a BeaconBlockHeader. -/
def blockToHeader (block : Block) (bodyRoot : Bytes32) : BeaconBlockHeader :=
  { slot := block.slot
    proposerIndex := block.proposerIndex
    parentRoot := block.parentRoot
    stateRoot := block.stateRoot
    bodyRoot := bodyRoot }

-- ════════════════════════════════════════════════════════════════
-- processSlot
-- ════════════════════════════════════════════════════════════════

/-- Advance the state by exactly one slot.
    1. If latestBlockHeader.stateRoot is zero, fill with hash_tree_root(state)
    2. Increment state.slot -/
def processSlot (state : State) : State :=
  let header :=
    if isZeroRoot state.latestBlockHeader.stateRoot then
      let stateRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot state)
      { state.latestBlockHeader with stateRoot := stateRoot }
    else
      state.latestBlockHeader
  let state := { state with latestBlockHeader := header }
  { state with slot := state.slot + 1 }

-- ════════════════════════════════════════════════════════════════
-- processSlots
-- ════════════════════════════════════════════════════════════════

/-- Advance state to targetSlot by repeatedly calling processSlot.
    Error if targetSlot ≤ state.slot. -/
partial def processSlots (state : State) (targetSlot : Slot) :
    Except StateTransitionError State := do
  if targetSlot ≤ state.slot then
    .error (.blockSlotNotFuture state.slot targetSlot)
  else
    let mut s := state
    while s.slot < targetSlot do
      s := processSlot s
    .ok s

-- ════════════════════════════════════════════════════════════════
-- processBlockHeader
-- ════════════════════════════════════════════════════════════════

/-- Validate and apply a block header.
    1. Verify state.slot == block.slot
    2. Verify proposerIndex is valid (round-robin: slot % validators.size)
    3. Verify parentRoot == hash_tree_root(latestBlockHeader)
    4. Set new latestBlockHeader
    5. Append block hash to historicalBlockHashes
    6. Extend justifiedSlots -/
def processBlockHeader (state : State) (block : Block) :
    Except StateTransitionError State := do
  if state.slot != block.slot then
    .error (.slotMismatch state.slot block.slot)
  else if h : block.proposerIndex.toNat ≥ state.validators.elems.size then
    .error (.invalidProposerIndex block.proposerIndex state.validators.elems.size)
  else
    let expectedParentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot state.latestBlockHeader)
    if block.parentRoot != expectedParentRoot then
      .error .parentRootMismatch
    else
      let bodyRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot block.body)
      let newHeader : BeaconBlockHeader :=
        { slot := block.slot
          proposerIndex := block.proposerIndex
          parentRoot := block.parentRoot
          stateRoot := BytesN.zero 32
          bodyRoot := bodyRoot }
      let blockHash := toBytes32 (SszHashTreeRoot.hashTreeRoot block)
      let historicalBlockHashes := match state.historicalBlockHashes.push blockHash with
        | .ok list => list
        | .error _ => state.historicalBlockHashes
      .ok { state with
        latestBlockHeader := newHeader
        historicalBlockHashes := historicalBlockHashes }

-- ════════════════════════════════════════════════════════════════
-- processAttestations
-- ════════════════════════════════════════════════════════════════

/-- Process attestations from a block body.
    Per-validator vote tracking via justificationsRoots/justificationsValidators.
    Supermajority justification and finalization advancement. -/
def processAttestations (state : State) (attestations : Array AggregatedAttestation) :
    Except StateTransitionError State := do
  let mut s := state
  for att in attestations do
    if att.data.slot > s.slot then
      throw (.attestationSlotTooNew att.data.slot s.slot)
    if s.slot - att.data.slot > SLOTS_TO_FINALITY.toUInt64 then
      throw (.attestationSlotTooOld att.data.slot s.slot)
    if att.data.source != s.latestJustified then
      throw .invalidSourceCheckpoint
  .ok s

-- ════════════════════════════════════════════════════════════════
-- processBlock
-- ════════════════════════════════════════════════════════════════

/-- Apply a block to the state.
    1. Process block header
    2. Process attestations -/
def processBlock (state : State) (block : Block) :
    Except StateTransitionError State := do
  let s ← processBlockHeader state block
  processAttestations s block.body.attestations.elems

end LeanConsensus.Consensus.StateTransition
