/-
  State Transition — Slot and Block Processing

  Implements the core state transition functions for the pq-devnet-3 beacon chain:
  - processSlot: advance state by one slot (fill roots, bump slot)
  - processSlots: advance to a target slot
  - processBlock: apply a block to the state
  - processAttestation: validate and apply attestations

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
  | proposerSlashed (index : ValidatorIndex)
  | proposerNotActive (index : ValidatorIndex)
  | parentRootMismatch
  | attestationSlotTooOld (attSlot currentSlot : Slot)
  | attestationSlotTooNew (attSlot currentSlot : Slot)
  | invalidSourceCheckpoint
  | attestationCapacityExceeded
  | other (msg : String)

instance : ToString StateTransitionError where
  toString
    | .slotMismatch e a => s!"slot mismatch: expected {e}, got {a}"
    | .blockSlotNotFuture ss bs => s!"block slot {bs} not future of state slot {ss}"
    | .invalidProposerIndex i c => s!"proposer index {i} >= validator count {c}"
    | .proposerSlashed i => s!"proposer {i} is slashed"
    | .proposerNotActive i => s!"proposer {i} is not active"
    | .parentRootMismatch => "parent root mismatch"
    | .attestationSlotTooOld a c => s!"attestation slot {a} too old (current: {c})"
    | .attestationSlotTooNew a c => s!"attestation slot {a} too new (current: {c})"
    | .invalidSourceCheckpoint => "invalid source checkpoint"
    | .attestationCapacityExceeded => "attestation capacity exceeded"
    | .other msg => msg

-- ════════════════════════════════════════════════════════════════
-- Helpers
-- ════════════════════════════════════════════════════════════════

/-- Set an element of an SszVector at index i. -/
def setSszVector {n : Nat} {α : Type} (v : SszVector n α) (i : Nat) (val : α)
    (h : i < n) : SszVector n α :=
  let idx : Fin v.elems.size := ⟨i, by rw [v.hlen]; exact h⟩
  ⟨v.elems.set idx val, by simp [v.hlen]⟩

/-- Wrap a ByteArray as Bytes32, zero-filling if the size is wrong. -/
def toBytes32 (data : ByteArray) : Bytes32 :=
  if h : data.size = 32 then ⟨data, h⟩ else BytesN.zero 32

/-- Check if a Bytes32 is all zeros. -/
def isZeroRoot (r : Bytes32) : Bool :=
  r.data == (BytesN.zero 32).data

/-- Convert a BeaconBlock header fields into a BeaconBlockHeader. -/
def blockToHeader (block : BeaconBlock) (bodyRoot : Bytes32) : BeaconBlockHeader :=
  { slot := block.slot
    proposerIndex := block.proposerIndex
    parentRoot := block.parentRoot
    stateRoot := block.stateRoot
    bodyRoot := bodyRoot }

/-- Check if a validator is active at a given slot. -/
def isActiveValidator (v : Validator) (slot : Slot) : Bool :=
  v.activationSlot ≤ slot && slot < v.exitSlot

-- ════════════════════════════════════════════════════════════════
-- processSlot
-- ════════════════════════════════════════════════════════════════

/-- Advance the state by exactly one slot.

    Pure function, no error possible:
    1. If latestBlockHeader.stateRoot is zero, fill with hash_tree_root(state)
    2. Record hash_tree_root(latestBlockHeader) into blockRoots[slot % SLOTS_PER_HISTORICAL_ROOT]
    3. Record hash_tree_root(state) into stateRoots[slot % SLOTS_PER_HISTORICAL_ROOT]
    4. Increment state.slot

    Steps 1-3 use the current slot for indexing; step 4 bumps it. -/
def processSlot (state : BeaconState) : BeaconState :=
  -- Step 1: Fill zero stateRoot in the latest block header
  let header :=
    if isZeroRoot state.latestBlockHeader.stateRoot then
      let stateRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot state)
      { state.latestBlockHeader with stateRoot := stateRoot }
    else
      state.latestBlockHeader
  let state := { state with latestBlockHeader := header }
  -- Step 2: Record block root
  let slotIdx := (state.slot.toNat % SLOTS_PER_HISTORICAL_ROOT)
  let blockRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot state.latestBlockHeader)
  have hIdx : slotIdx < SLOTS_PER_HISTORICAL_ROOT := Nat.mod_lt _ (by decide)
  let blockRoots := setSszVector state.blockRoots slotIdx blockRoot hIdx
  -- Step 3: Record state root
  let stateRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot state)
  let stateRoots := setSszVector state.stateRoots slotIdx stateRoot hIdx
  -- Step 4: Increment slot
  { state with
    blockRoots := blockRoots
    stateRoots := stateRoots
    slot := state.slot + 1 }

-- ════════════════════════════════════════════════════════════════
-- processSlots
-- ════════════════════════════════════════════════════════════════

/-- Advance state to targetSlot by repeatedly calling processSlot.
    Error if targetSlot ≤ state.slot. -/
partial def processSlots (state : BeaconState) (targetSlot : Slot) :
    Except StateTransitionError BeaconState := do
  if targetSlot ≤ state.slot then
    .error (.blockSlotNotFuture state.slot targetSlot)
  else
    let mut s := state
    while s.slot < targetSlot do
      s := processSlot s
    .ok s

-- ════════════════════════════════════════════════════════════════
-- processAttestation
-- ════════════════════════════════════════════════════════════════

/-- Validate and apply a single attestation to the state.

    1. Verify att.data.slot ≤ state.slot (not from future)
    2. Verify state.slot - att.data.slot ≤ SLOTS_TO_FINALITY (not too old)
    3. Verify att.data.sourceCheckpoint == state.justifiedCheckpoint
    4. Append to state.currentAttestations -/
def processAttestation (state : BeaconState) (att : SignedAggregatedAttestation) :
    Except StateTransitionError BeaconState := do
  -- Check not from future
  if att.data.slot > state.slot then
    .error (.attestationSlotTooNew att.data.slot state.slot)
  -- Check not too old
  else if state.slot - att.data.slot > SLOTS_TO_FINALITY.toUInt64 then
    .error (.attestationSlotTooOld att.data.slot state.slot)
  -- Check source checkpoint
  else if att.data.sourceCheckpoint != state.justifiedCheckpoint then
    .error .invalidSourceCheckpoint
  else
    -- Append attestation
    match state.currentAttestations.push att with
    | .ok newAtts => .ok { state with currentAttestations := newAtts }
    | .error _ => .error .attestationCapacityExceeded

-- ════════════════════════════════════════════════════════════════
-- processBlock
-- ════════════════════════════════════════════════════════════════

/-- Apply a block to the state.

    1. Verify state.slot == block.slot
    2. Verify proposerIndex < validators.size
    3. Verify proposer not slashed, is active at this slot
    4. Verify parentRoot == hash_tree_root(latestBlockHeader)
    5. Set new latestBlockHeader (stateRoot = zero, filled by next processSlot)
    6. Process each attestation in block.body.attestations

    Signature verification deferred to Phase 4. -/
def processBlock (state : BeaconState) (block : BeaconBlock) :
    Except StateTransitionError BeaconState := do
  -- Step 1: Slot match
  if state.slot != block.slot then
    .error (.slotMismatch state.slot block.slot)
  -- Step 2: Valid proposer index
  else if h : block.proposerIndex.toNat ≥ state.validators.elems.size then
    .error (.invalidProposerIndex block.proposerIndex state.validators.elems.size)
  else
    have hBound : block.proposerIndex.toNat < state.validators.elems.size := by omega
    let proposer := state.validators.elems[block.proposerIndex.toNat]
    -- Step 3a: Not slashed
    if Validator.slashed proposer then
      .error (.proposerSlashed block.proposerIndex)
    -- Step 3b: Is active
    else if !isActiveValidator proposer state.slot then
      .error (.proposerNotActive block.proposerIndex)
    else
      -- Step 4: Parent root check
      let expectedParentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot state.latestBlockHeader)
      if block.parentRoot != expectedParentRoot then
        .error .parentRootMismatch
      else
        -- Step 5: Set new block header (stateRoot = zero, bodyRoot = hash of body)
        let bodyRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot block.body)
        let newHeader : BeaconBlockHeader :=
          { slot := block.slot
            proposerIndex := block.proposerIndex
            parentRoot := block.parentRoot
            stateRoot := BytesN.zero 32
            bodyRoot := bodyRoot }
        let s := { state with latestBlockHeader := newHeader }
        -- Step 6: Process attestations
        let attestations := block.body.attestations.elems
        attestations.foldlM (init := s) fun acc att =>
          processAttestation acc att

end LeanConsensus.Consensus.StateTransition
