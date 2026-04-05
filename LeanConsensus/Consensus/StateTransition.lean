/-
  State Transition — Slot and Block Processing (leanSpec-aligned)

  Implements the core state transition functions for the pq-devnet-3 beacon chain
  aligned with leanSpec/src/lean_spec/subspecs/containers/state/state.py:
  - processSlot: advance state by one slot
  - processSlots: advance to a target slot
  - processBlockHeader: validate and apply block header (genesis, gap filling, justified slots)
  - processAttestations: full vote tracking, supermajority justification, finalization
  - processBlock: chain header + attestations processing

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
  | blockNotNewerThanParent (parentSlot blockSlot : Slot)
  | invalidProposerIndex (index : ValidatorIndex) (validatorCount : Nat)
  | incorrectProposer (expected actual : ValidatorIndex)
  | parentRootMismatch
  | noValidators
  | other (msg : String)

instance : ToString StateTransitionError where
  toString
    | .slotMismatch e a => s!"slot mismatch: expected {e}, got {a}"
    | .blockSlotNotFuture ss bs => s!"block slot {bs} not future of state slot {ss}"
    | .blockNotNewerThanParent ps bs => s!"block slot {bs} not newer than parent slot {ps}"
    | .invalidProposerIndex i c => s!"proposer index {i} >= validator count {c}"
    | .incorrectProposer e a => s!"incorrect proposer: expected {e}, got {a}"
    | .parentRootMismatch => "parent root mismatch"
    | .noValidators => "no validators in state"
    | .other msg => msg

instance {n : Nat} : Inhabited (BytesN n) where
  default := BytesN.zero n

-- ════════════════════════════════════════════════════════════════
-- Helpers
-- ════════════════════════════════════════════════════════════════

/-- Wrap a ByteArray as Bytes32, zero-filling if the size is wrong. -/
def toBytes32 (data : ByteArray) : Bytes32 :=
  if h : data.size = 32 then ⟨data, h⟩ else BytesN.zero 32

/-- Check if a Bytes32 is all zeros. -/
def isZeroRoot (r : Bytes32) : Bool :=
  r.data == (BytesN.zero 32).data

/-- The zero root constant. -/
def zeroRoot : Bytes32 := BytesN.zero 32

-- ════════════════════════════════════════════════════════════════
-- Slot Helpers (ported from leanSpec slot.py)
-- ════════════════════════════════════════════════════════════════

/-- Return the relative bitfield index for justification tracking.
    Slots at or before finalized have no index (return none). -/
def justifiedIndexAfter (slot finalized : Slot) : Option Nat :=
  if slot ≤ finalized then none
  else some (slot.toNat - finalized.toNat - 1)

/-- Integer square root. -/
private partial def isqrt (n : Nat) : Nat :=
  if n == 0 then 0
  else Id.run do
    let mut x := n
    let mut y := (x + 1) / 2
    while y < x do
      x := y
      y := (x + n / x) / 2
    return x

/-- Check if a slot is a valid candidate for justification after a given finalized slot.
    A slot is justifiable if delta = slot - finalized satisfies:
    1. delta ≤ 5 (immediate window)
    2. delta is a perfect square
    3. delta is a pronic number (n*(n+1)) -/
def isJustifiableAfter (slot finalized : Slot) : Bool :=
  if slot < finalized then false
  else
    let delta := slot.toNat - finalized.toNat
    delta ≤ 5
    || isqrt delta * isqrt delta == delta
    || (let d := 4 * delta + 1; let s := isqrt d; s * s == d && s % 2 == 1)

-- ════════════════════════════════════════════════════════════════
-- JustifiedSlots Helpers (ported from leanSpec state/types.py)
-- ════════════════════════════════════════════════════════════════

/-- Check if a slot is justified. Slots ≤ finalized are implicitly justified. -/
def isSlotJustified (justifiedSlots : Bitlist HISTORICAL_ROOTS_LIMIT)
    (finalized target : Slot) : Bool :=
  match justifiedIndexAfter target finalized with
  | none => true
  | some idx =>
    if h : idx < justifiedSlots.length then
      justifiedSlots.getBit idx h
    else false

/-- Return a new bitlist with the justification status updated for a slot. -/
def withJustified (justifiedSlots : Bitlist HISTORICAL_ROOTS_LIMIT)
    (finalized target : Slot) (val : Bool) : Bitlist HISTORICAL_ROOTS_LIMIT :=
  match justifiedIndexAfter target finalized with
  | none => justifiedSlots
  | some idx =>
    if h : idx < justifiedSlots.length then
      justifiedSlots.setBit idx h val
    else justifiedSlots

/-- Extend justifiedSlots so that the target slot is addressable. -/
def extendToSlot (justifiedSlots : Bitlist HISTORICAL_ROOTS_LIMIT)
    (finalized target : Slot) : Bitlist HISTORICAL_ROOTS_LIMIT :=
  match justifiedIndexAfter target finalized with
  | none => justifiedSlots
  | some idx =>
    let requiredLen := idx + 1
    if requiredLen ≤ justifiedSlots.length then justifiedSlots
    else match justifiedSlots.extendToLength requiredLen with
      | .ok bl => bl
      | .error _ => justifiedSlots

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
-- processBlockHeader (leanSpec-aligned)
-- ════════════════════════════════════════════════════════════════

/-- Push a root into an SszList, silently dropping on overflow. -/
private def pushRoot (list : SszList HISTORICAL_ROOTS_LIMIT Root) (r : Root)
    : SszList HISTORICAL_ROOTS_LIMIT Root :=
  match list.push r with
  | .ok l => l
  | .error _ => list

/-- Validate and apply a block header.
    Aligned with leanSpec process_block_header(). -/
def processBlockHeader (state : State) (block : Block) :
    Except StateTransitionError State := do
  let parentHeader := state.latestBlockHeader
  let parentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot parentHeader)

  -- Validation
  if state.slot != block.slot then
    .error (.slotMismatch state.slot block.slot)
  else if block.slot ≤ parentHeader.slot then
    .error (.blockNotNewerThanParent parentHeader.slot block.slot)
  else if state.validators.elems.size == 0 then
    .error .noValidators
  else
    let expectedProposer := (state.slot.toNat % state.validators.elems.size).toUInt64
    if block.proposerIndex != expectedProposer then
      .error (.incorrectProposer expectedProposer block.proposerIndex)
    else if block.parentRoot != parentRoot then
      .error .parentRootMismatch
    else
      -- Genesis parent detection
      let isGenesisParent := parentHeader.slot == 0
      let latestJustified :=
        if isGenesisParent then { state.latestJustified with root := parentRoot }
        else state.latestJustified
      let latestFinalized :=
        if isGenesisParent then { state.latestFinalized with root := parentRoot }
        else state.latestFinalized

      -- Historical data: append parent root + zero hashes for gaps
      let numEmptySlots := block.slot.toNat - parentHeader.slot.toNat - 1
      let mut hashes := pushRoot state.historicalBlockHashes parentRoot
      for _ in [:numEmptySlots] do
        hashes := pushRoot hashes zeroRoot

      -- Extend justified slots to cover up to block.slot - 1
      let lastMaterializedSlot := block.slot - 1
      let justifiedSlots := extendToSlot state.justifiedSlots latestFinalized.slot lastMaterializedSlot

      -- New header with empty state root
      let bodyRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot block.body)
      let newHeader : BeaconBlockHeader :=
        { slot := block.slot
          proposerIndex := block.proposerIndex
          parentRoot := block.parentRoot
          stateRoot := BytesN.zero 32
          bodyRoot := bodyRoot }

      .ok { state with
        latestBlockHeader := newHeader
        latestJustified := latestJustified
        latestFinalized := latestFinalized
        historicalBlockHashes := hashes
        justifiedSlots := justifiedSlots }

-- ════════════════════════════════════════════════════════════════
-- processAttestations (leanSpec-aligned)
-- ════════════════════════════════════════════════════════════════

/-- Unflatten justificationsValidators into per-root vote arrays.
    Returns Array (Root × Array Bool). -/
private def unflattenVotes (roots : Array Root) (validators : Bitlist (HISTORICAL_ROOTS_LIMIT * VALIDATOR_REGISTRY_LIMIT))
    (numValidators : Nat) : Array (Root × Array Bool) := Id.run do
  let mut result := #[]
  for i in [:roots.size] do
    let root := roots[i]!
    let start := i * numValidators
    let mut votes := Array.replicate numValidators false
    for j in [:numValidators] do
      let srcIdx := start + j
      if srcIdx < validators.length then
        if h : srcIdx < validators.length then
          votes := votes.set! j (validators.getBit srcIdx h)
    result := result.push (root, votes)
  return result

/-- Compare two BytesN lexicographically. -/
private def bytesLt (a b : Bytes32) : Bool := Id.run do
  for i in [:32] do
    let av := a.data.get! i
    let bv := b.data.get! i
    if av < bv then return true
    if av > bv then return false
  return false

/-- Re-flatten vote structure back to sorted roots + flat bitlist. -/
private def flattenVotes (justifications : Array (Root × Array Bool))
    : Array Root × Array Bool := Id.run do
  let sorted := justifications.qsort fun a b => bytesLt a.1 b.1
  let mut roots := #[]
  let mut allVotes := #[]
  for (root, votes) in sorted do
    roots := roots.push root
    allVotes := allVotes ++ votes
  return (roots, allVotes)

/-- Build a rootToSlot map from historical block hashes. -/
private def buildRootToSlot (hashes : Array Root) (startSlot : Nat)
    : Array (Root × Nat) := Id.run do
  let mut result := #[]
  for i in [startSlot:hashes.size] do
    result := result.push (hashes[i]!, i)
  return result

/-- Look up a slot for a root in the rootToSlot map. -/
private def lookupSlot (rootToSlot : Array (Root × Nat)) (root : Root) : Option Nat :=
  rootToSlot.foldl (fun acc (r, s) => if r == root then some s else acc) none

/-- Count true values in a Bool array. -/
private def countVotes (votes : Array Bool) : Nat :=
  votes.foldl (fun acc v => if v then acc + 1 else acc) 0

/-- Process attestations from a block body.
    Per-validator vote tracking via justificationsRoots/justificationsValidators.
    Supermajority justification and finalization advancement.

    Invalid attestations are silently skipped (soft-skip), matching leanSpec behavior. -/
def processAttestations (state : State) (attestations : Array AggregatedAttestation) :
    Except StateTransitionError State := do
  let numValidators := state.validators.elems.size
  let hashesArr := state.historicalBlockHashes.elems

  -- Unflatten vote structure
  let mut justifications := unflattenVotes state.justificationsRoots.elems
    state.justificationsValidators numValidators

  -- Track mutable state
  let mut latestJustified := state.latestJustified
  let mut latestFinalized := state.latestFinalized
  let mut finalizedSlot := latestFinalized.slot
  let mut justifiedSlots := state.justifiedSlots

  -- Build rootToSlot map
  let startSlot := finalizedSlot.toNat + 1
  let rootToSlot := buildRootToSlot hashesArr startSlot

  -- Process each attestation
  for att in attestations do
    let source := att.data.source
    let target := att.data.target

    -- Skip if source not justified
    if !isSlotJustified justifiedSlots finalizedSlot source.slot then
      continue

    -- Skip if target already justified
    if isSlotJustified justifiedSlots finalizedSlot target.slot then
      continue

    -- Skip zero-hash roots
    if isZeroRoot source.root || isZeroRoot target.root then
      continue

    -- Bounds-safe history lookups
    let sourceSlotNat := source.slot.toNat
    let targetSlotNat := target.slot.toNat
    if sourceSlotNat ≥ hashesArr.size || targetSlotNat ≥ hashesArr.size then
      continue

    -- Check roots match historical records
    if source.root != hashesArr[sourceSlotNat]! || target.root != hashesArr[targetSlotNat]! then
      continue

    -- Time must flow forward
    if target.slot ≤ source.slot then
      continue

    -- Target must be justifiable
    if !isJustifiableAfter target.slot finalizedSlot then
      continue

    -- Get validator indices from aggregation bits
    let validatorIndices := att.aggregationBits.toIndices
    if validatorIndices.isEmpty then
      continue

    -- Find or create vote entry for target root
    let mut found := false
    let mut justIdx := 0
    for i in [:justifications.size] do
      if (justifications[i]!).1 == target.root then
        found := true
        justIdx := i
        break

    if !found then
      justifications := justifications.push (target.root, Array.replicate numValidators false)
      justIdx := justifications.size - 1

    -- Record votes
    let entry := justifications[justIdx]!
    let entryRoot := entry.1
    let mut votes := entry.2
    for vi in validatorIndices do
      if vi < numValidators then
        votes := votes.set! vi true
    justifications := justifications.set! justIdx (entryRoot, votes)

    -- Check supermajority: 3 * count >= 2 * total
    let count := countVotes votes
    if 3 * count ≥ 2 * numValidators then
      -- Target becomes justified
      latestJustified := target
      justifiedSlots := withJustified justifiedSlots finalizedSlot target.slot true

      -- Remove vote entry for justified root
      justifications := justifications.filter fun (r, _) => r != target.root

      -- Check finalization: no justifiable slot gap between source and target
      let hasGap := Id.run do
        let mut gap := false
        for s in [source.slot.toNat + 1 : target.slot.toNat] do
          if isJustifiableAfter s.toUInt64 finalizedSlot then
            gap := true
            break
        return gap

      if !hasGap then
        let oldFinalizedSlot := finalizedSlot
        latestFinalized := source
        finalizedSlot := latestFinalized.slot
        let delta := finalizedSlot.toNat - oldFinalizedSlot.toNat
        if delta > 0 then
          justifiedSlots := justifiedSlots.shiftWindow delta
          -- Prune old vote entries
          justifications := justifications.filter fun (r, _) =>
            match lookupSlot rootToSlot r with
            | some s => s > finalizedSlot.toNat
            | none => false

  -- Re-flatten vote structure
  let (sortedRoots, allVotes) := flattenVotes justifications

  -- Build output SszLists and Bitlist
  let justRoots := match SszList.mkChecked (maxCap := HISTORICAL_ROOTS_LIMIT) sortedRoots with
    | .ok l => l
    | .error _ => SszList.empty
  let justVals := if h : allVotes.size ≤ HISTORICAL_ROOTS_LIMIT * VALIDATOR_REGISTRY_LIMIT then
      Bitlist.fromBools allVotes h
    else
      Bitlist.empty (HISTORICAL_ROOTS_LIMIT * VALIDATOR_REGISTRY_LIMIT)

  .ok { state with
    justificationsRoots := justRoots
    justificationsValidators := justVals
    justifiedSlots := justifiedSlots
    latestJustified := latestJustified
    latestFinalized := latestFinalized }

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
