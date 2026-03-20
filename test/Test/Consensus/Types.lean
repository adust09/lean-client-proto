/-
  Consensus Types Tests — verify type construction and SSZ instances
-/

import LeanConsensus.Consensus.Types
import LeanConsensus.Consensus.Constants

namespace Test.Consensus.Types

open LeanConsensus.SSZ
open LeanConsensus.Consensus

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── Consensus Types ──"
  let mut total := 0
  let mut failures := 0

  let cp : Checkpoint := { root := Bytes32.zero, slot := 0 }
  let encoded := SszEncode.sszEncode cp
  let (t, f) ← check "Checkpoint encode size = 40" (encoded.size == 40)
  total := total + t; failures := failures + f

  let ad : AttestationData := {
    slot := 1, head := cp, target := cp, source := cp
  }
  let encoded := SszEncode.sszEncode ad
  let (t, f) ← check "AttestationData encode size = 128" (encoded.size == 128)
  total := total + t; failures := failures + f

  let hdr : BeaconBlockHeader := {
    slot := 0, proposerIndex := 0
    parentRoot := Bytes32.zero, stateRoot := Bytes32.zero, bodyRoot := Bytes32.zero
  }
  let encoded := SszEncode.sszEncode hdr
  let (t, f) ← check "BeaconBlockHeader encode size = 112" (encoded.size == 112)
  total := total + t; failures := failures + f

  let val : Validator := {
    attestationPubkey := BytesN.zero XMSS_PUBKEY_SIZE
    proposalPubkey := BytesN.zero XMSS_PUBKEY_SIZE
    index := 0
  }
  let encoded := SszEncode.sszEncode val
  let (t, f) ← check "Validator encode size = 112" (encoded.size == 112)
  total := total + t; failures := failures + f

  let sa : SignedAttestation := {
    data := ad, validatorIndex := 0
    signature := BytesN.zero XMSS_SIGNATURE_SIZE
  }
  let encoded := SszEncode.sszEncode sa
  let (t, f) ← check "SignedAttestation encode size = 560" (encoded.size == 560)
  total := total + t; failures := failures + f

  let (t, f) ← check "SECONDS_PER_SLOT = 4" (SECONDS_PER_SLOT == 4)
  total := total + t; failures := failures + f
  let (t, f) ← check "SLOTS_TO_FINALITY = 3" (SLOTS_TO_FINALITY == 3)
  total := total + t; failures := failures + f
  let (t, f) ← check "XMSS_SIGNATURE_SIZE = 424" (XMSS_SIGNATURE_SIZE == 424)
  total := total + t; failures := failures + f

  return (total, failures)

end Test.Consensus.Types
