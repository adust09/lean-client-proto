/-
  Fork Choice — 3SF-mini LMD-GHOST (leanSpec-aligned)

  Implements the fork choice rule aligned with leanSpec `forkchoice/store.py`:
  - Store: interval-based time, aggregated payload tracking
  - onBlock: process a new block into the store
  - onGossipAttestation: process a single attestation
  - onGossipAggregatedAttestation: process an aggregated attestation
  - getHead: LMD-GHOST head selection with block weight from aggregated payloads
  - Interval-based tick system (5 intervals per slot)
  - Staged payload processing (new → known)
  - updateSafeTarget: supermajority threshold
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
  | attestationBlockUnknown (root : Root)
  | other (msg : String)

instance : ToString ForkChoiceError where
  toString
    | .blockAlreadyKnown _ => "block already known"
    | .unknownParent _ => "unknown parent block"
    | .stateTransitionFailed e => s!"state transition failed: {e}"
    | .invalidAttestationSlot => "invalid attestation slot"
    | .attestationBlockUnknown _ => "attestation references unknown block"
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
-- Interval / Slot conversions (leanSpec-aligned)
-- ════════════════════════════════════════════════════════════════

/-- Convert an interval to a slot (5 intervals per slot). -/
def intervalToSlot (interval : Interval) : Slot :=
  interval / INTERVALS_PER_SLOT.toUInt64

/-- Get the current slot from the store's time. -/
def currentSlot (store : Store) : Slot :=
  intervalToSlot store.time

/-- Get the interval within the current slot (0..4). -/
def slotInterval (store : Store) : UInt64 :=
  store.time % INTERVALS_PER_SLOT.toUInt64

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
    attestationSignatures := ∅
    latestNewAggregatedPayloads := ∅
    latestKnownAggregatedPayloads := ∅ }

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
-- computeBlockWeight (leanSpec-aligned)
-- ════════════════════════════════════════════════════════════════

/-- Population count (number of set bits) for a single byte. -/
private def UInt8.popcount (b : UInt8) : Nat :=
  let mut count := 0
  let mut v := b
  for _ in [:8] do
    if v &&& 1 != 0 then count := count + 1
    v := v >>> 1
  count

/-- Count the number of set bits in AggregationBits for weight. -/
private def countAggregationBits (bits : AggregationBits) : Nat :=
  bits.data.foldl (init := 0) fun acc byte =>
    acc + UInt8.popcount byte

/-- Compute the weight of a root based on aggregated payloads (leanSpec-aligned).
    Weight = sum of aggregation bits from known aggregated payloads
    for blocks that are descendants of root. -/
def computeBlockWeight (store : Store) (root : Root) : Nat :=
  store.latestKnownAggregatedPayloads.fold (init := 0) fun acc blockRoot att =>
    if isAncestor store root blockRoot then
      acc + countAggregationBits att.proof.participants
    else acc

-- ════════════════════════════════════════════════════════════════
-- getHead (LMD-GHOST, leanSpec-aligned)
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
        let bestWeight := computeBlockWeight store best
        let childWeight := computeBlockWeight store child
        if childWeight > bestWeight then child
        else if childWeight == bestWeight && byteArrayLt child.data best.data then child
        else best
      go store bestChild

-- ════════════════════════════════════════════════════════════════
-- Staged Payload Processing (new → known)
-- ════════════════════════════════════════════════════════════════

/-- Promote all new aggregated payloads to known. Called at interval boundaries. -/
def promotePayloads (store : Store) : Store :=
  let merged := store.latestNewAggregatedPayloads.fold
    store.latestKnownAggregatedPayloads
    fun acc root att => acc.insert root att
  { store with
    latestKnownAggregatedPayloads := merged
    latestNewAggregatedPayloads := ∅ }

-- ════════════════════════════════════════════════════════════════
-- updateSafeTarget (supermajority threshold)
-- ════════════════════════════════════════════════════════════════

/-- Update the safe target if there is supermajority (2/3) support.
    Counts total aggregation bits across all known payloads for
    blocks that are descendants of a candidate target. -/
def updateSafeTarget (store : Store) : Store :=
  let head := getHead store
  match store.blocks.get? head with
  | none => store
  | some headBlock =>
    match store.states.get? head with
    | none => store
    | some headState =>
      let totalValidators := headState.validators.elems.size
      if totalValidators == 0 then store
      else
        let weight := computeBlockWeight store headBlock.parentRoot
        let threshold := totalValidators * FINALITY_THRESHOLD_NUMERATOR / FINALITY_THRESHOLD_DENOMINATOR
        if weight ≥ threshold then
          { store with safeTarget := head }
        else store

-- ════════════════════════════════════════════════════════════════
-- onTick (interval-based, leanSpec-aligned)
-- ════════════════════════════════════════════════════════════════

/-- Process a time tick (interval since genesis).
    At each new slot boundary (interval % 5 == 0), promote payloads
    and update the safe target. -/
def onTick (store : Store) (time : Interval) : Store :=
  let store := { store with time := time }
  let atSlotBoundary := time % INTERVALS_PER_SLOT.toUInt64 == 0
  if atSlotBoundary then
    let store := promotePayloads store
    let store := updateSafeTarget store
    { store with head := getHead store }
  else store

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
        states := store.states.insert root state }
      let store := { store with head := getHead store }
      .ok store

-- ════════════════════════════════════════════════════════════════
-- onGossipAttestation (leanSpec-aligned)
-- ════════════════════════════════════════════════════════════════

/-- Process a gossipped single attestation.
    Validates slot range, stores the attestation signature. -/
def onGossipAttestation (store : Store) (att : SignedAttestation) :
    Except ForkChoiceError Store := do
  let slot := currentSlot store
  if att.data.slot > slot then
    .error .invalidAttestationSlot
  if !store.blocks.contains att.data.head.root then
    .error (.attestationBlockUnknown att.data.head.root)
  let existing := store.attestationSignatures.getD att.data.head.root #[]
  let updated := existing.push att
  .ok { store with
    attestationSignatures := store.attestationSignatures.insert att.data.head.root updated }

-- ════════════════════════════════════════════════════════════════
-- onGossipAggregatedAttestation (leanSpec-aligned)
-- ════════════════════════════════════════════════════════════════

/-- Process a gossipped aggregated attestation.
    Stores it as a new (not yet known) aggregated payload. -/
def onGossipAggregatedAttestation (store : Store)
    (att : SignedAggregatedAttestation) :
    Except ForkChoiceError Store := do
  let slot := currentSlot store
  if att.data.slot > slot then
    .error .invalidAttestationSlot
  if !store.blocks.contains att.data.head.root then
    .error (.attestationBlockUnknown att.data.head.root)
  .ok { store with
    latestNewAggregatedPayloads :=
      store.latestNewAggregatedPayloads.insert att.data.head.root att }

-- ════════════════════════════════════════════════════════════════
-- Legacy onAttestation (compatibility wrapper)
-- ════════════════════════════════════════════════════════════════

/-- Process a new attestation (compatibility wrapper).
    Wraps the attestation as a SignedAttestation and delegates to
    onGossipAttestation. -/
def onAttestation (store : Store) (validatorIndex : ValidatorIndex)
    (att : AttestationData) : Except ForkChoiceError Store := do
  let signed : SignedAttestation :=
    { data := att
      validatorIndex := validatorIndex
      signature := BytesN.zero XMSS_SIGNATURE_SIZE }
  onGossipAttestation store signed

end LeanConsensus.Consensus.ForkChoice
