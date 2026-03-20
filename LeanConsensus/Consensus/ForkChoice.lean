/-
  Fork Choice — 3SF-mini LMD-GHOST

  Implements the fork choice rule for the pq-devnet-3 beacon chain:
  - Store: in-memory block/state store with latest messages
  - onBlock: process a new block into the store
  - onAttestation: process a new attestation
  - getHead: LMD-GHOST head selection
  - Interval-based tick system
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

/-- Compute hash_tree_root of a Block as a Root. -/
def blockRoot (block : Block) : Root :=
  toBytes32 (SszHashTreeRoot.hashTreeRoot block)

/-- Lexicographic comparison for ByteArray. -/
def byteArrayLt (a b : ByteArray) : Bool := Id.run do
  let minLen := min a.size b.size
  for i in [:minLen] do
    if a.get! i < b.get! i then return true
    if a.get! i > b.get! i then return false
  return decide (a.size < b.size)

-- ════════════════════════════════════════════════════════════════
-- fromAnchor
-- ════════════════════════════════════════════════════════════════

/-- Create a new store from an anchor state and block. -/
def fromAnchor (anchorState : State) (anchorBlock : Block)
    (validatorId : Option ValidatorIndex := none) : Store :=
  let root := blockRoot anchorBlock
  let anchorCheckpoint : Checkpoint := { root := root, slot := 0 }
  { time := 0
    config := anchorState.config
    head := root
    safeTarget := root
    latestJustified := anchorCheckpoint
    latestFinalized := anchorCheckpoint
    blocks := (∅ : Std.HashMap Root Block).insert root anchorBlock
    states := (∅ : Std.HashMap Root State).insert root anchorState
    validatorId := validatorId
    latestMessages := ∅ }

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
      if block.parentRoot == current then return false
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
-- computeWeight
-- ════════════════════════════════════════════════════════════════

/-- Compute the weight of a root based on validator votes. -/
def computeWeight (store : Store) (root : Root) : Nat :=
  store.latestMessages.fold (init := 0) fun acc _ msg =>
    if isAncestor store root msg.root then acc + 1 else acc

-- ════════════════════════════════════════════════════════════════
-- getHead (LMD-GHOST)
-- ════════════════════════════════════════════════════════════════

/-- LMD-GHOST head selection: starting from justified checkpoint root,
    greedily descend to the child with the most weight.
    Tie-break by lexicographic root comparison. -/
partial def getHead (store : Store) : Root :=
  go store store.latestJustified.root
where
  go (store : Store) (current : Root) : Root :=
    let children := getChildren store current
    if children.isEmpty then current
    else
      let bestChild := children.foldl (init := children[0]!) fun best child =>
        let bestWeight := computeWeight store best
        let childWeight := computeWeight store child
        if childWeight > bestWeight then child
        else if childWeight == bestWeight && byteArrayLt child.data best.data then child
        else best
      go store bestChild

-- ════════════════════════════════════════════════════════════════
-- onTick
-- ════════════════════════════════════════════════════════════════

/-- Process a time tick (interval since genesis). -/
def onTick (store : Store) (time : UInt64) : Store :=
  { store with time := time }

-- ════════════════════════════════════════════════════════════════
-- onBlock
-- ════════════════════════════════════════════════════════════════

/-- Process a new block into the store.
    1. Compute blockRoot, reject if already known
    2. Look up parent state
    3. Run processSlots + processBlock
    4. Insert block + resulting state
    5. Update head -/
def onBlock (store : Store) (block : Block) :
    Except ForkChoiceError Store := do
  let root := blockRoot block
  if store.blocks.contains root then
    .error (.blockAlreadyKnown root)
  else match store.states.get? block.parentRoot with
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
        states := store.states.insert root state
        head := getHead store }
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
  let currentSlot := store.time.toNat / (SECONDS_PER_SLOT * 1000 / INTERVALS_PER_SLOT)
  if att.slot > currentSlot.toUInt64 then
    .error .invalidAttestationSlot
  else
    match store.latestMessages.get? validatorIndex with
    | some existing =>
      if existing.slot == att.slot && existing.root != att.head.root then
        .error (.equivocation validatorIndex)
      else if att.slot > existing.slot then
        let msg : LatestMessage := { slot := att.slot, root := att.head.root }
        .ok { store with latestMessages := store.latestMessages.insert validatorIndex msg }
      else
        .ok store
    | none =>
      let msg : LatestMessage := { slot := att.slot, root := att.head.root }
      .ok { store with latestMessages := store.latestMessages.insert validatorIndex msg }

end LeanConsensus.Consensus.ForkChoice
