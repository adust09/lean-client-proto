/-
  Consensus Signing Tests — computeDomain and computeSigningRoot
-/

import LeanConsensus.Consensus.Signing
import LeanConsensus.Consensus.Constants
import LeanConsensus.Consensus.Types

namespace Test.Consensus.Signing

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
  IO.println "\n── Consensus Signing ──"
  let mut total := 0
  let mut failures := 0

  -- ForkData SSZ: 4 + 32 = 36 bytes fixed
  let fv : Bytes4 := BytesN.zero 4
  let gr : Bytes32 := Bytes32.zero
  let fd : ForkData := { currentVersion := fv, genesisValidatorsRoot := gr }
  let encoded := SszEncode.sszEncode fd
  let (t, f) ← check "ForkData encode size = 36" (encoded.size == 36)
  total := total + t; failures := failures + f

  -- ForkData roundtrip
  let decoded := SszDecode.sszDecode (α := ForkData) encoded
  let (t, f) ← check "ForkData roundtrip" (match decoded with
    | .ok fd' => SszEncode.sszEncode fd' == encoded
    | .error _ => false)
  total := total + t; failures := failures + f

  -- SigningData SSZ: 32 + 32 = 64 bytes fixed
  let sd : SigningData := { objectRoot := Bytes32.zero, domain := Bytes32.zero }
  let encoded := SszEncode.sszEncode sd
  let (t, f) ← check "SigningData encode size = 64" (encoded.size == 64)
  total := total + t; failures := failures + f

  -- SigningData roundtrip
  let decoded := SszDecode.sszDecode (α := SigningData) encoded
  let (t, f) ← check "SigningData roundtrip" (match decoded with
    | .ok sd' => SszEncode.sszEncode sd' == encoded
    | .error _ => false)
  total := total + t; failures := failures + f

  -- computeDomain produces a 32-byte result
  let domain := computeDomain DOMAIN_BEACON_PROPOSER fv gr
  let (t, f) ← check "computeDomain produces Bytes32" (domain.data.size == 32)
  total := total + t; failures := failures + f

  -- computeDomain: first 4 bytes are the domain type (LE)
  let (t, f) ← check "computeDomain first 4 bytes = domainType LE" (
    domain.data.get! 0 == 0x00 &&
    domain.data.get! 1 == 0x00 &&
    domain.data.get! 2 == 0x00 &&
    domain.data.get! 3 == 0x00)
  total := total + t; failures := failures + f

  -- computeDomain with attester domain type
  let attDomain := computeDomain DOMAIN_BEACON_ATTESTER fv gr
  let (t, f) ← check "computeDomain attester first byte = 0x00" (
    attDomain.data.get! 0 == 0x00 &&
    attDomain.data.get! 1 == 0x00 &&
    attDomain.data.get! 2 == 0x00 &&
    attDomain.data.get! 3 == 0x01)
  total := total + t; failures := failures + f

  -- Different domain types produce different domains
  let (t, f) ← check "different domain types → different domains" (
    !(domain == attDomain))
  total := total + t; failures := failures + f

  -- computeSigningRoot produces a 32-byte result
  let cp : Checkpoint := { slot := 42, root := Bytes32.zero }
  let sigRoot := computeSigningRoot cp domain
  let (t, f) ← check "computeSigningRoot produces Bytes32" (sigRoot.data.size == 32)
  total := total + t; failures := failures + f

  -- computeSigningRoot is deterministic
  let sigRoot2 := computeSigningRoot cp domain
  let (t, f) ← check "computeSigningRoot is deterministic" (sigRoot == sigRoot2)
  total := total + t; failures := failures + f

  -- Different objects produce different signing roots
  let cp2 : Checkpoint := { slot := 43, root := Bytes32.zero }
  let sigRoot3 := computeSigningRoot cp2 domain
  let (t, f) ← check "different objects → different signing roots" (!(sigRoot == sigRoot3))
  total := total + t; failures := failures + f

  -- Different domains produce different signing roots
  let sigRoot4 := computeSigningRoot cp attDomain
  let (t, f) ← check "different domains → different signing roots" (!(sigRoot == sigRoot4))
  total := total + t; failures := failures + f

  return (total, failures)

end Test.Consensus.Signing
