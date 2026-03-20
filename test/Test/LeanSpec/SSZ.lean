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

/-- Convert a single hex character to its numeric value. -/
private def hexDigit (c : Char) : Option UInt8 :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat).toUInt8
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10).toUInt8
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10).toUInt8
  else none

/-- Convert a hex string (with 0x prefix) to ByteArray. -/
def hexToBytes (hex : String) : Option ByteArray := do
  let s : String := if hex.startsWith "0x" then String.ofList (hex.toList.drop 2) else hex
  let chars : List Char := s.toList
  if chars.length % 2 != 0 then .none
  else
    let mut result := ByteArray.empty
    let mut i := 0
    while h : i + 1 < chars.length do
      let hi ← hexDigit (chars[i]'(by omega))
      let lo ← hexDigit (chars[i + 1]'(by omega))
      result := result.push (hi * 16 + lo)
      i := i + 2
    return result

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
