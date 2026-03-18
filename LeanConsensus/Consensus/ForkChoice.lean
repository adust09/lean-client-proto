/-
  Fork Choice — 3SF-mini LMD-GHOST

  Implements the fork choice rule for the pq-devnet-3 beacon chain:
  - Store: in-memory block/state store with latest messages
  - onBlock: process a new block into the store
  - onAttestation: process a new attestation
  - getHead: LMD-GHOST head selection
  - Justification and finalization via checkpoint updates
-/

import LeanConsensus.Consensus.Types
import LeanConsensus.Consensus.StateTransition

namespace LeanConsensus.Consensus.ForkChoice

open LeanConsensus.SSZ
open LeanConsensus.Consensus
open LeanConsensus.Consensus.StateTransition

instance {n : Nat} : Inhabited (BytesN n) where
  default := BytesN.zero n

-- ════════════════════════════════════════════════════════════════
-- Error Type
-- ════════════════════════════════════════════════════════════════

inductive ForkChoiceError where
  | blockAlreadyKnown (root : Root)
  | unknownParent (parentRoot : Root)
  | stateTransitionFailed (err : StateTransitionError)
  | invalidAttestationSlot
  | equivocation (validatorIndex : ValidatorIndex)
  | other (msg : String)

instance : ToString ForkChoiceError where
  toString
    | .blockAlreadyKnown _ => "block already known"
    | .unknownParent _ => "unknown parent block"
    | .stateTransitionFailed e => s!"state transition failed: {e}"
    | .invalidAttestationSlot => "invalid attestation slot"
    | .equivocation idx => s!"equivocation by validator {idx}"
    | .other msg => msg

-- ════════════════════════════════════════════════════════════════
-- Helpers
-- ════════════════════════════════════════════════════════════════

/-- Wrap a ByteArray as Bytes32. -/
private def toBytes32 (data : ByteArray) : Bytes32 :=
  if h : data.size = 32 then ⟨data, h⟩ else BytesN.zero 32

/-- Compute hash_tree_root of a BeaconBlock as a Root. -/
def blockRoot (block : BeaconBlock) : Root :=
  toBytes32 (SszHashTreeRoot.hashTreeRoot block)

/-- Lexicographic comparison for ByteArray. -/
def byteArrayLt (a b : ByteArray) : Bool := Id.run do
  let minLen := min a.size b.size
  for i in [:minLen] do
    if a.get! i < b.get! i then return true
    if a.get! i > b.get! i then return false
  return decide (a.size < b.size)

-- ════════════════════════════════════════════════════════════════
-- initStore
-- ════════════════════════════════════════════════════════════════

/-- Create a new store from genesis state and block. -/
def initStore (genesisState : BeaconState) (genesisBlock : BeaconBlock) : Store :=
  let root := blockRoot genesisBlock
  let genesisCheckpoint : Checkpoint := { slot := 0, root := root }
  { justifiedCheckpoint := genesisCheckpoint
    finalizedCheckpoint := genesisCheckpoint
    blocks := (∅ : Std.HashMap Root BeaconBlock).insert root genesisBlock
    blockStates := (∅ : Std.HashMap Root BeaconState).insert root genesisState
    latestMessages := ∅
    currentSlot := 0 }

-- ════════════════════════════════════════════════════════════════
-- isAncestor
-- ════════════════════════════════════════════════════════════════

/-- Check if `ancestor` is an ancestor of `descendant` by walking the parent chain.
    Bounded to max 1000 steps to ensure termination. -/
def isAncestor (store : Store) (ancestor descendant : Root) : Bool := Id.run do
  if ancestor == descendant then return true
  let mut current := descendant
  for _ in [:1000] do
    match store.blocks.get? current with
    | none => return false
    | some block =>
      if block.parentRoot == ancestor then return true
      if block.parentRoot == current then return false  -- self-loop guard
      current := block.parentRoot
  return false

-- ════════════════════════════════════════════════════════════════
-- getChildren
-- ════════════════════════════════════════════════════════════════

/-- Get all direct children of a given root. -/
def getChildren (store : Store) (root : Root) : Array Root :=
  store.blocks.fold (init := #[]) fun acc r block =>
    if block.parentRoot == root then acc.push r else acc

-- ════════════════════════════════════════════════════════════════
-- computeWeight / computeTotalActiveBalance
-- ════════════════════════════════════════════════════════════════

/-- Compute the total effective balance of active, non-slashed validators. -/
def computeTotalActiveBalance (store : Store) (state : BeaconState) : Nat :=
  state.validators.foldl (init := 0) fun acc v =>
    if !v.slashed && v.activationSlot ≤ store.currentSlot && store.currentSlot < v.exitSlot then
      acc + v.effectiveBalance.toNat
    else acc

/-- Compute the weight (sum of effective balances) of validators whose latest
    message supports `root` or a descendant of `root`. -/
def computeWeight (store : Store) (state : BeaconState) (root : Root) : Nat :=
  let validators := state.validators.elems
  let result := validators.foldl (init := (0, 0)) (fun pair v =>
    let weight := pair.1
    let idx := pair.2
    let nextIdx := idx + 1
    if Validator.slashed v then (weight, nextIdx)
    else if !(Validator.activationSlot v ≤ store.currentSlot &&
              store.currentSlot < Validator.exitSlot v) then (weight, nextIdx)
    else
      match store.latestMessages.get? (idx : Nat).toUInt64 with
      | none => (weight, nextIdx)
      | some msg =>
        if isAncestor store root msg.root then
          (weight + (Validator.effectiveBalance v).toNat, nextIdx)
        else (weight, nextIdx))
  result.1

-- ════════════════════════════════════════════════════════════════
-- getHead (LMD-GHOST)
-- ════════════════════════════════════════════════════════════════

/-- LMD-GHOST head selection: starting from justified checkpoint root,
    greedily descend to the child with the most weight.
    Tie-break by lexicographic root comparison. -/
partial def getHead (store : Store) : Root :=
  let justifiedRoot := store.justifiedCheckpoint.root
  match store.blockStates.get? justifiedRoot with
  | none => justifiedRoot
  | some st => go store st justifiedRoot
where
  go (store : Store) (state : BeaconState) (current : Root) : Root :=
    let children := getChildren store current
    if children.isEmpty then current
    else
      let bestChild := children.foldl (init := children[0]!) fun best child =>
        let bestWeight := computeWeight store state best
        let childWeight := computeWeight store state child
        if childWeight > bestWeight then child
        else if childWeight == bestWeight && byteArrayLt child.data best.data then child
        else best
      go store state bestChild

-- ════════════════════════════════════════════════════════════════
-- computeAttestingBalance / updateCheckpoints
-- ════════════════════════════════════════════════════════════════

/-- Compute the total effective balance of validators attesting to a target checkpoint. -/
def computeAttestingBalance (state : BeaconState) (targetSlot : Slot) : Nat :=
  state.currentAttestations.foldl (init := 0) fun acc att =>
    if att.data.targetCheckpoint.slot == targetSlot then
      acc + att.aggregationBits.length * 32000000000
    else acc

/-- Update justified and finalized checkpoints based on attestation support.
    If current attestations give 2/3+ support for a target, justify it.
    If the justified checkpoint is far enough ahead, finalize. -/
def updateCheckpoints (store : Store) (state : BeaconState) : Store :=
  let totalBalance := computeTotalActiveBalance store state
  if totalBalance == 0 then store
  else
    let attestingBalance := computeAttestingBalance state state.slot
    let hasSupermajority := attestingBalance * FINALITY_THRESHOLD_DENOMINATOR ≥
      totalBalance * FINALITY_THRESHOLD_NUMERATOR
    if hasSupermajority then
      let head := getHead store
      let newJustified : Checkpoint := { slot := state.slot, root := head }
      let newStore := { store with justifiedCheckpoint := newJustified }
      if newJustified.slot ≥ store.finalizedCheckpoint.slot + SLOTS_TO_FINALITY.toUInt64 then
        { newStore with finalizedCheckpoint := store.justifiedCheckpoint }
      else newStore
    else store

-- ════════════════════════════════════════════════════════════════
-- onBlock
-- ════════════════════════════════════════════════════════════════

/-- Process a new block into the store.
    1. Compute blockRoot, reject if already known
    2. Look up parent state
    3. Run processSlots + processBlock
    4. Insert block + resulting state
    5. Update checkpoints -/
def onBlock (store : Store) (block : BeaconBlock) :
    Except ForkChoiceError Store := do
  let root := blockRoot block
  if store.blocks.contains root then
    .error (.blockAlreadyKnown root)
  else match store.blockStates.get? block.parentRoot with
    | none => .error (.unknownParent block.parentRoot)
    | some parentState =>
      let state ← match processSlots parentState block.slot with
        | .ok s => .ok s
        | .error e => .error (.stateTransitionFailed e)
      let state ← match processBlock state block with
        | .ok s => .ok s
        | .error e => .error (.stateTransitionFailed e)
      let store := { store with
        blocks := store.blocks.insert root block
        blockStates := store.blockStates.insert root state }
      let store := updateCheckpoints store state
      .ok store

-- ════════════════════════════════════════════════════════════════
-- onAttestation
-- ════════════════════════════════════════════════════════════════

/-- Process a new attestation.
    1. Reject future attestations
    2. Detect equivocation (same slot, different root)
    3. Update latestMessages if attestation is newer -/
def onAttestation (store : Store) (validatorIndex : ValidatorIndex)
    (att : AttestationData) : Except ForkChoiceError Store := do
  if att.slot > store.currentSlot then
    .error .invalidAttestationSlot
  else
    match store.latestMessages.get? validatorIndex with
    | some existing =>
      if existing.slot == att.slot && existing.root != att.headRoot then
        .error (.equivocation validatorIndex)
      else if att.slot > existing.slot then
        let msg : LatestMessage := { slot := att.slot, root := att.headRoot }
        .ok { store with latestMessages := store.latestMessages.insert validatorIndex msg }
      else
        .ok store
    | none =>
      let msg : LatestMessage := { slot := att.slot, root := att.headRoot }
      .ok { store with latestMessages := store.latestMessages.insert validatorIndex msg }

end LeanConsensus.Consensus.ForkChoice
