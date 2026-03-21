/-
  SSZ Roundtrip Tests — encode then decode should return the original value
-/

import LeanConsensus.SSZ
import LeanConsensus.Consensus.Types
import LeanConsensus.Consensus.Constants

namespace Test.SSZ.Roundtrip

open LeanConsensus.SSZ
open LeanConsensus.Consensus

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def roundtrip {α : Type} [BEq α] [SszEncode α] [SszDecode α] (v : α) : Bool :=
  match SszDecode.sszDecode (SszEncode.sszEncode v) with
  | .ok decoded => decoded == v
  | .error _ => false

def runTests : IO (Nat × Nat) := do
  IO.println "\n── SSZ Roundtrip ──"
  let mut total := 0
  let mut failures := 0

  let (t, f) ← check "UInt8 roundtrip" (roundtrip (42 : UInt8))
  total := total + t; failures := failures + f
  let (t, f) ← check "UInt16 roundtrip" (roundtrip (1234 : UInt16))
  total := total + t; failures := failures + f
  let (t, f) ← check "UInt32 roundtrip" (roundtrip (0xDEADBEEF : UInt32))
  total := total + t; failures := failures + f
  let (t, f) ← check "UInt64 roundtrip" (roundtrip (0x0102030405060708 : UInt64))
  total := total + t; failures := failures + f
  let (t, f) ← check "Bool true roundtrip" (roundtrip true)
  total := total + t; failures := failures + f
  let (t, f) ← check "Bool false roundtrip" (roundtrip false)
  total := total + t; failures := failures + f

  let (t, f) ← check "Bytes32 zero roundtrip" (roundtrip Bytes32.zero)
  total := total + t; failures := failures + f

  let cfg : Config := { genesisTime := 1700000000 }
  let (t, f) ← check "Config roundtrip" (roundtrip cfg)
  total := total + t; failures := failures + f

  let cfgZero : Config := { genesisTime := 0 }
  let (t, f) ← check "Config zero roundtrip" (roundtrip cfgZero)
  total := total + t; failures := failures + f

  let cp : Checkpoint := { root := Bytes32.zero, slot := 100 }
  let (t, f) ← check "Checkpoint roundtrip" (roundtrip cp)
  total := total + t; failures := failures + f

  let ad : AttestationData := {
    slot := 42
    head := { root := Bytes32.zero, slot := 10 }
    source := { root := Bytes32.zero, slot := 10 }
    target := { root := Bytes32.zero, slot := 20 }
  }
  let (t, f) ← check "AttestationData roundtrip" (roundtrip ad)
  total := total + t; failures := failures + f

  let hdr : BeaconBlockHeader := {
    slot := 1, proposerIndex := 0
    parentRoot := Bytes32.zero, stateRoot := Bytes32.zero, bodyRoot := Bytes32.zero
  }
  let (t, f) ← check "BeaconBlockHeader roundtrip" (roundtrip hdr)
  total := total + t; failures := failures + f

  let val : Validator := {
    attestationPubkey := BytesN.zero XMSS_PUBKEY_SIZE
    proposalPubkey := BytesN.zero XMSS_PUBKEY_SIZE
    index := 0
  }
  let (t, f) ← check "Validator roundtrip" (roundtrip val)
  total := total + t; failures := failures + f

  let aspEmpty : AggregatedSignatureProof := {
    participants := Bitlist.empty VALIDATOR_REGISTRY_LIMIT
    proofData := ⟨ByteArray.empty, by decide⟩
  }
  let (t, f) ← check "AggregatedSignatureProof empty roundtrip" (roundtrip aspEmpty)
  total := total + t; failures := failures + f

  let aspWithData : AggregatedSignatureProof := {
    participants := Bitlist.empty VALIDATOR_REGISTRY_LIMIT
    proofData := ⟨ByteArray.mk #[0xDE, 0xAD, 0xBE, 0xEF], by decide⟩
  }
  let (t, f) ← check "AggregatedSignatureProof with data roundtrip" (roundtrip aspWithData)
  total := total + t; failures := failures + f

  return (total, failures)

end Test.SSZ.Roundtrip
