/-
  SSZ roundtrip tests using leanSpec-generated fixtures.

  For each SSZ fixture with a matching type, decodes the hex SSZ bytes,
  re-encodes, and verifies roundtrip.
-/

import LeanConsensus.SSZ
import LeanConsensus.Consensus.Types
import Test.LeanSpec.Types
import Test.LeanSpec.Loader

namespace Test.LeanSpec.SSZ

open LeanConsensus.SSZ
open LeanConsensus.Consensus
open Test.LeanSpec

/-- Test assertion helper. -/
def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

/-- Generic roundtrip test for any SSZ type. -/
def testRoundtrip {α : Type} [SszDecode α] [SszEncode α] (typeName : String)
    (fixture : SSZFixture) : IO (Nat × Nat) := do
  match hexToBytes fixture.serialized with
  | none => check s!"{typeName} hex decode" false
  | some sszBytes =>
    match @SszDecode.sszDecode α _ sszBytes with
    | .error _ => check s!"{typeName} SSZ decode" false
    | .ok val =>
      let reEncoded := @SszEncode.sszEncode α _ val
      check s!"{typeName} roundtrip" (reEncoded == sszBytes)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── LeanSpec SSZ ──"
  let mut total := 0
  let mut failures := 0

  let available ← Loader.fixturesAvailable
  if !available then
    IO.println "  SKIP: leanSpec fixtures not generated (run scripts/gen-leanspec-fixtures.sh)"
    return (0, 0)

  let sszDir := s!"{Loader.fixturesDir}/ssz"
  let files ← Loader.discoverFixtures sszDir
  IO.println s!"  Found {files.size} SSZ fixture files"

  for file in files do
    let result ← Loader.loadFixture (α := SSZFixture) file
    match result with
    | .error e =>
      IO.println s!"  ✗ parse error: {e}"
      total := total + 1; failures := failures + 1
    | .ok fixture =>
      match fixture.typeName with
      | "Checkpoint" =>
        let (t, f) ← testRoundtrip (α := Checkpoint) "Checkpoint" fixture
        total := total + t; failures := failures + f
      | "BlockHeader" =>
        let (t, f) ← testRoundtrip (α := BeaconBlockHeader) "BlockHeader" fixture
        total := total + t; failures := failures + f
      | "Config" =>
        let (t, f) ← testRoundtrip (α := Config) "Config" fixture
        total := total + t; failures := failures + f
      | "Validator" =>
        let (t, f) ← testRoundtrip (α := Validator) "Validator" fixture
        total := total + t; failures := failures + f
      | "AttestationData" =>
        let (t, f) ← testRoundtrip (α := AttestationData) "AttestationData" fixture
        total := total + t; failures := failures + f
      | "AggregatedSignatureProof" =>
        let (t, f) ← testRoundtrip (α := AggregatedSignatureProof) "AggregatedSignatureProof" fixture
        total := total + t; failures := failures + f
      | "AggregatedAttestation" =>
        let (t, f) ← testRoundtrip (α := AggregatedAttestation) "AggregatedAttestation" fixture
        total := total + t; failures := failures + f
      | "SignedAggregatedAttestation" =>
        let (t, f) ← testRoundtrip (α := SignedAggregatedAttestation) "SignedAggregatedAttestation" fixture
        total := total + t; failures := failures + f
      | "Attestation" =>
        let (t, f) ← testRoundtrip (α := Attestation) "Attestation" fixture
        total := total + t; failures := failures + f
      | "SignedAttestation" =>
        let (t, f) ← testRoundtrip (α := SignedAttestation) "SignedAttestation" fixture
        total := total + t; failures := failures + f
      | "Block" =>
        let (t, f) ← testRoundtrip (α := Block) "Block" fixture
        total := total + t; failures := failures + f
      | "BlockBody" =>
        let (t, f) ← testRoundtrip (α := BlockBody) "BlockBody" fixture
        total := total + t; failures := failures + f
      | "BlockSignatures" =>
        let (t, f) ← testRoundtrip (α := BlockSignatures) "BlockSignatures" fixture
        total := total + t; failures := failures + f
      | "SignedBlock" =>
        let (t, f) ← testRoundtrip (α := SignedBlock) "SignedBlock" fixture
        total := total + t; failures := failures + f
      | "State" =>
        let (t, f) ← testRoundtrip (α := State) "State" fixture
        total := total + t; failures := failures + f
      | "PublicKey" =>
        let (t, f) ← testRoundtrip (α := BytesN XMSS_PUBKEY_SIZE) "PublicKey" fixture
        total := total + t; failures := failures + f
      | "Signature" =>
        let (t, f) ← testRoundtrip (α := BytesN XMSS_SIGNATURE_SIZE) "Signature" fixture
        total := total + t; failures := failures + f
      | _ => pure ()

  IO.println s!"  SSZ: {total - failures}/{total} passed"
  return (total, failures)

end Test.LeanSpec.SSZ
