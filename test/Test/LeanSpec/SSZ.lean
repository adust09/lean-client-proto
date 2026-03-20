/-
  SSZ roundtrip tests using leanSpec-generated fixtures.

  For each SSZ fixture with a matching type (Checkpoint, BlockHeader),
  decodes the hex SSZ bytes, re-encodes, and verifies roundtrip.
  Types with structural divergence (Validator, Block) are skipped.
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

/-- Try roundtrip for Checkpoint type. -/
def testCheckpointRoundtrip (fixture : SSZFixture) : IO (Nat × Nat) := do
  match hexToBytes fixture.serialized with
  | none => check s!"Checkpoint hex decode" false
  | some sszBytes =>
    match @SszDecode.sszDecode Checkpoint _ sszBytes with
    | .error _ => check s!"Checkpoint SSZ decode" false
    | .ok val =>
      let reEncoded := @SszEncode.sszEncode Checkpoint _ val
      check s!"Checkpoint roundtrip" (reEncoded == sszBytes)

/-- Try roundtrip for BlockHeader type. -/
def testBlockHeaderRoundtrip (fixture : SSZFixture) : IO (Nat × Nat) := do
  match hexToBytes fixture.serialized with
  | none => check s!"BlockHeader hex decode" false
  | some sszBytes =>
    match @SszDecode.sszDecode BeaconBlockHeader _ sszBytes with
    | .error _ => check s!"BlockHeader SSZ decode" false
    | .ok val =>
      let reEncoded := @SszEncode.sszEncode BeaconBlockHeader _ val
      check s!"BlockHeader roundtrip" (reEncoded == sszBytes)

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
        let (t, f) ← testCheckpointRoundtrip fixture
        total := total + t; failures := failures + f
      | "BlockHeader" =>
        let (t, f) ← testBlockHeaderRoundtrip fixture
        total := total + t; failures := failures + f
      | _ => pure () -- skip divergent types

  IO.println s!"  SSZ: {total - failures}/{total} passed"
  return (total, failures)

end Test.LeanSpec.SSZ
