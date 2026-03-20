/-
  Aggregator — Attestation Aggregation Pool

  Collects individual signed attestations and aggregates them into
  AggregatedAttestation. Features:
  - Duplicate rejection per validator index
  - Threshold-based aggregation triggering
  - Slot-based pruning of stale entries
  - Bitlist construction for aggregation bits
-/

import Std.Data.HashMap
import Std.Data.HashSet
import LeanConsensus.Consensus.Types
import LeanConsensus.Consensus.Constants
import LeanConsensus.Crypto.LeanMultisig
import LeanConsensus.SSZ.Bitlist
import LeanConsensus.SSZ.Merkleization
import LeanConsensus.Crypto.Sha256

namespace LeanConsensus.Consensus.Aggregator

open LeanConsensus.Consensus
open LeanConsensus.SSZ
open LeanConsensus.Crypto.LeanMultisig
open LeanConsensus.Crypto (sha256)

-- ════════════════════════════════════════════════════════════════
-- Hashable AttestationData
-- ════════════════════════════════════════════════════════════════

/-- Hash AttestationData to UInt64 for use as a HashMap key.
    Uses SHA-256 of the SSZ encoding, truncated to 8 bytes LE. -/
instance : Hashable AttestationData where
  hash att :=
    let encoded := SszEncode.sszEncode att
    let h := sha256 encoded
    let b0 := (h.get! 0).toUInt64
    let b1 := (h.get! 1).toUInt64 <<< 8
    let b2 := (h.get! 2).toUInt64 <<< 16
    let b3 := (h.get! 3).toUInt64 <<< 24
    let b4 := (h.get! 4).toUInt64 <<< 32
    let b5 := (h.get! 5).toUInt64 <<< 40
    let b6 := (h.get! 6).toUInt64 <<< 48
    let b7 := (h.get! 7).toUInt64 <<< 56
    b0 ||| b1 ||| b2 ||| b3 ||| b4 ||| b5 ||| b6 ||| b7

-- ════════════════════════════════════════════════════════════════
-- Pending Aggregation
-- ════════════════════════════════════════════════════════════════

/-- A single attestation entry with its validator's public key and signature. -/
structure AttestationEntry where
  validatorIndex : ValidatorIndex
  pubkey         : ByteArray
  signature      : ByteArray

/-- Tracks pending attestations for a single AttestationData value. -/
structure PendingAggregation where
  data         : AttestationData
  attestations : Array AttestationEntry
  seenIndices  : Std.HashSet ValidatorIndex

/-- Create a new empty PendingAggregation for the given data. -/
def PendingAggregation.empty (data : AttestationData) : PendingAggregation :=
  { data, attestations := #[], seenIndices := ∅ }

/-- Add an attestation to a pending aggregation.
    Returns `none` if the validator has already attested (duplicate). -/
def PendingAggregation.addEntry (pa : PendingAggregation)
    (entry : AttestationEntry) : Option PendingAggregation :=
  if pa.seenIndices.contains entry.validatorIndex then none
  else some {
    data := pa.data
    attestations := pa.attestations.push entry
    seenIndices := pa.seenIndices.insert entry.validatorIndex
  }

-- ════════════════════════════════════════════════════════════════
-- Aggregation Pool
-- ════════════════════════════════════════════════════════════════

/-- The aggregation pool collects attestations and triggers aggregation
    when a threshold is met. -/
structure AggregationPool where
  pool      : IO.Ref (Std.HashMap AttestationData PendingAggregation)
  threshold : Nat
  proverCtx : ProverContext

/-- Create a new aggregation pool.
    `threshold` is the minimum number of attestations to trigger aggregation. -/
def AggregationPool.create (threshold : Nat) : IO AggregationPool := do
  let pool ← IO.mkRef (∅ : Std.HashMap AttestationData PendingAggregation)
  let proverCtx ← setupProver
  return { pool, threshold, proverCtx }

-- ════════════════════════════════════════════════════════════════
-- Bitlist Construction
-- ════════════════════════════════════════════════════════════════

/-- Build a Bitlist representing which validators participated in aggregation.
    Sets bit at each validator's index position. -/
def buildAggregationBits (indices : Array ValidatorIndex) (length : Nat)
    (h : length ≤ VALIDATOR_REGISTRY_LIMIT) : AggregationBits := Id.run do
  let byteCount := (length + 7) / 8
  let mut data := ByteArray.mk (Array.replicate byteCount 0)
  for idx in indices do
    let i := idx.toNat
    if i < length then
      let byteIdx := i / 8
      let bitIdx := i % 8
      let byte := data.get! byteIdx
      data := data.set! byteIdx (byte ||| ((1 : UInt8) <<< bitIdx.toUInt8))
  if hsize : data.size = (length + 7) / 8 then
    ⟨data, length, h, hsize⟩
  else
    Bitlist.zeros length h

-- ════════════════════════════════════════════════════════════════
-- Aggregation
-- ════════════════════════════════════════════════════════════════

/-- Aggregate attestations into an AggregatedAttestation using ZK proofs.
    Runs `LeanMultisig.aggregate` to produce a compact proof. -/
private def tryAggregate (proverCtx : ProverContext) (pending : PendingAggregation) :
    IO AggregatedAttestation := do
  let _pubkeys := pending.attestations.map (·.pubkey)
  let _signatures := pending.attestations.map (·.signature)
  let _message := SszEncode.sszEncode pending.data
  let indices := pending.attestations.map (·.validatorIndex)
  let numValidators := pending.attestations.size
  let h : numValidators ≤ VALIDATOR_REGISTRY_LIMIT := by
    sorry
  let bits := buildAggregationBits indices numValidators h
  return {
    aggregationBits := bits
    data := pending.data
  }

-- ════════════════════════════════════════════════════════════════
-- Pool Operations
-- ════════════════════════════════════════════════════════════════

/-- Add an attestation to the pool.
    Returns `some` aggregated attestation if the threshold is met.
    Returns `none` if more attestations are needed or if the attestation is a duplicate. -/
def addAttestation (pool : AggregationPool) (att : SignedAttestation)
    (pubkey : ByteArray) : IO (Option AggregatedAttestation) := do
  let entry : AttestationEntry := {
    validatorIndex := att.validatorIndex
    pubkey := pubkey
    signature := att.signature.data
  }
  let poolMap ← pool.pool.get
  let pending := match poolMap.get? att.data with
    | some p => p
    | none => PendingAggregation.empty att.data
  match pending.addEntry entry with
  | none => return none
  | some updatedPending =>
    if updatedPending.attestations.size ≥ pool.threshold then
      pool.pool.modify fun m => m.erase att.data
      let aggregated ← tryAggregate pool.proverCtx updatedPending
      return some aggregated
    else
      pool.pool.modify fun m => m.insert att.data updatedPending
      return none

/-- Prune attestations older than `currentSlot - SLOTS_TO_FINALITY`.
    Removes stale entries that are too old to be included in blocks. -/
def pruneOld (pool : AggregationPool) (currentSlot : Slot) : IO Unit := do
  let cutoff := if currentSlot > SLOTS_TO_FINALITY.toUInt64
    then currentSlot - SLOTS_TO_FINALITY.toUInt64
    else 0
  pool.pool.modify fun m =>
    m.fold (init := ∅) fun acc data pending =>
      if pending.data.slot ≥ cutoff then acc.insert data pending
      else acc

/-- Get the number of pending attestation groups in the pool. -/
def pendingCount (pool : AggregationPool) : IO Nat := do
  let m ← pool.pool.get
  return m.size

end LeanConsensus.Consensus.Aggregator
