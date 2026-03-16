/-
  SSZ Bitvector Tests
-/

import LeanConsensus.SSZ

namespace Test.SSZ.Bitvector

open LeanConsensus.SSZ

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── SSZ Bitvector ──"
  let mut total := 0
  let mut failures := 0

  let bv8 := LeanConsensus.SSZ.Bitvector.zero 8
  let (t, f) ← check "Bitvector(8) zero has 1 byte" (bv8.data.size == 1)
  total := total + t; failures := failures + f

  let bv16 := LeanConsensus.SSZ.Bitvector.zero 16
  let (t, f) ← check "Bitvector(16) zero has 2 bytes" (bv16.data.size == 2)
  total := total + t; failures := failures + f

  let bv10 := LeanConsensus.SSZ.Bitvector.zero 10
  let (t, f) ← check "Bitvector(10) zero has 2 bytes" (bv10.data.size == 2)
  total := total + t; failures := failures + f

  let bv := LeanConsensus.SSZ.Bitvector.zero 8
  let bv := bv.setBit 0 (by omega) true
  let bv := bv.setBit 3 (by omega) true
  let (t, f) ← check "Bitvector getBit 0" (bv.getBit 0 (by omega) == true)
  total := total + t; failures := failures + f
  let (t, f) ← check "Bitvector getBit 1" (bv.getBit 1 (by omega) == false)
  total := total + t; failures := failures + f
  let (t, f) ← check "Bitvector getBit 3" (bv.getBit 3 (by omega) == true)
  total := total + t; failures := failures + f

  let (t, f) ← check "Bitvector popcount" (bv.popcount == 2)
  total := total + t; failures := failures + f

  let encoded := SszEncode.sszEncode bv
  let result := SszDecode.sszDecode (α := LeanConsensus.SSZ.Bitvector 8) encoded
  let (t, f) ← check "Bitvector(8) roundtrip" (match result with | .ok d => d == bv | .error _ => false)
  total := total + t; failures := failures + f

  return (total, failures)

end Test.SSZ.Bitvector
