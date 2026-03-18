/-
  Aggregator Tests — Attestation Pool and Aggregation
-/

import LeanConsensus.Consensus.Aggregator
import LeanConsensus.SSZ.BytesN

namespace Test.Consensus.Aggregator

open LeanConsensus.Consensus.Aggregator
open LeanConsensus.Consensus
open LeanConsensus.SSZ

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

/-- Create a test AttestationData for the given slot. -/
private def mkTestAttData (slot : UInt64) : AttestationData := {
  slot := slot
  headRoot := BytesN.zero 32
  sourceCheckpoint := { slot := 0, root := BytesN.zero 32 }
  targetCheckpoint := { slot := slot, root := BytesN.zero 32 }
}

/-- Create a test SignedAttestation with the given validator index. -/
private def mkTestAtt (slot : UInt64) (idx : ValidatorIndex) : SignedAttestation := {
  data := mkTestAttData slot
  validatorIndex := idx
  signature := BytesN.zero XMSS_SIGNATURE_SIZE
}

def runTests : IO (Nat × Nat) := do
  IO.println "\n── Attestation Aggregator ──"
  let mut total := 0
  let mut failures := 0

  -- Test 1: PendingAggregation.empty creates empty state
  do
    let attData := mkTestAttData 42
    let pending := PendingAggregation.empty attData
    let (t, f) ← check "empty pending has no attestations"
      (pending.attestations.size == 0)
    total := total + t; failures := failures + f

  -- Test 2: PendingAggregation.addEntry adds attestation
  do
    let attData := mkTestAttData 42
    let pending := PendingAggregation.empty attData
    let entry : AttestationEntry := {
      validatorIndex := 0
      pubkey := ByteArray.mk (Array.replicate 32 0)
      signature := ByteArray.mk (Array.replicate 64 0)
    }
    match pending.addEntry entry with
    | some updated =>
      let (t, f) ← check "addEntry increases count"
        (updated.attestations.size == 1)
      total := total + t; failures := failures + f
    | none =>
      let (t, f) ← check "addEntry increases count" false
      total := total + t; failures := failures + f

  -- Test 3: Duplicate rejection
  do
    let attData := mkTestAttData 42
    let pending := PendingAggregation.empty attData
    let entry : AttestationEntry := {
      validatorIndex := 5
      pubkey := ByteArray.mk (Array.replicate 32 0)
      signature := ByteArray.mk (Array.replicate 64 0)
    }
    match pending.addEntry entry with
    | some updated =>
      -- Try to add same validator again
      let result := updated.addEntry entry
      let (t, f) ← check "duplicate rejection" result.isNone
      total := total + t; failures := failures + f
    | none =>
      let (t, f) ← check "duplicate rejection" false
      total := total + t; failures := failures + f

  -- Test 4: Multiple entries from different validators
  do
    let attData := mkTestAttData 10
    let mut pending := PendingAggregation.empty attData
    for i in [:5] do
      let entry : AttestationEntry := {
        validatorIndex := i.toUInt64
        pubkey := ByteArray.mk (Array.replicate 32 i.toUInt8)
        signature := ByteArray.mk (Array.replicate 64 i.toUInt8)
      }
      match pending.addEntry entry with
      | some updated => pending := updated
      | none => pure ()
    let (t, f) ← check "multiple entries accepted" (pending.attestations.size == 5)
    total := total + t; failures := failures + f

  -- Test 5: buildAggregationBits sets correct bits
  do
    let indices : Array ValidatorIndex := #[0, 2, 5]
    let bits := buildAggregationBits indices 8 (by decide)
    -- Bit 0, 2, 5 should be set
    -- Byte 0: bit0=1, bit2=1, bit5=1 => 0b00100101 = 0x25
    let byte0 := bits.data.get! 0
    let (t, f) ← check "aggregation bits correct" (byte0 == 0x25)
    total := total + t; failures := failures + f

  -- Test 6: buildAggregationBits with empty indices
  do
    let indices : Array ValidatorIndex := #[]
    let bits := buildAggregationBits indices 4 (by decide)
    let byte0 := bits.data.get! 0
    let (t, f) ← check "empty aggregation bits all zero" (byte0 == 0)
    total := total + t; failures := failures + f

  -- Test 7: Hashable AttestationData produces consistent hashes
  do
    let att1 := mkTestAttData 42
    let att2 := mkTestAttData 42
    let h1 := Hashable.hash att1
    let h2 := Hashable.hash att2
    let (t, f) ← check "same AttestationData same hash" (h1 == h2)
    total := total + t; failures := failures + f

  -- Test 8: Different AttestationData produces different hashes
  do
    let att1 := mkTestAttData 42
    let att2 := mkTestAttData 43
    let h1 := Hashable.hash att1
    let h2 := Hashable.hash att2
    let (t, f) ← check "different AttestationData different hash" (h1 != h2)
    total := total + t; failures := failures + f

  return (total, failures)

end Test.Consensus.Aggregator
