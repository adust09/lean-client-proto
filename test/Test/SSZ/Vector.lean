/-
  SSZ Vector Tests
-/

import LeanConsensus.SSZ

namespace Test.SSZ.Vector

open LeanConsensus.SSZ

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def isError {α : Type} : Except SszError α → Bool
  | .error _ => true
  | .ok _ => false

def runTests : IO (Nat × Nat) := do
  IO.println "\n── SSZ Vector ──"
  let mut total := 0
  let mut failures := 0

  -- Vector of 3 UInt32s
  let arr := #[(1 : UInt32), 2, 3]
  match SszVector.mkChecked (n := 3) arr with
  | .ok vec =>
    let encoded := SszEncode.sszEncode vec
    let (t, f) ← check "Vector(3, UInt32) encode size" (encoded.size == 12)
    total := total + t; failures := failures + f

    let (t, f) ← check "Vector(3, UInt32) first bytes"
      (encoded.get! 0 == 0x01 && encoded.get! 1 == 0x00)
    total := total + t; failures := failures + f

    let result := SszDecode.sszDecode (α := SszVector 3 UInt32) encoded
    let (t, f) ← check "Vector(3, UInt32) roundtrip" (match result with
      | .ok v => v == vec
      | .error _ => false)
    total := total + t; failures := failures + f
  | .error _ =>
    let (t, f) ← check "Vector creation failed" false
    total := total + t; failures := failures + f

  -- Wrong size array
  let (t, f) ← check "Vector wrong size rejected"
    (isError (SszVector.mkChecked (n := 5) #[(1 : UInt32), 2, 3]))
  total := total + t; failures := failures + f

  -- Vector of Bytes32
  let roots := #[Bytes32.zero, Bytes32.zero]
  match SszVector.mkChecked (n := 2) roots with
  | .ok vec =>
    let encoded := SszEncode.sszEncode vec
    let (t, f) ← check "Vector(2, Bytes32) encode size" (encoded.size == 64)
    total := total + t; failures := failures + f

    let result := SszDecode.sszDecode (α := SszVector 2 Bytes32) encoded
    let (t, f) ← check "Vector(2, Bytes32) roundtrip" (match result with
      | .ok v => v == vec
      | .error _ => false)
    total := total + t; failures := failures + f
  | .error _ =>
    let (t, f) ← check "Vector(2, Bytes32) creation failed" false
    total := total + t; failures := failures + f

  return (total, failures)

end Test.SSZ.Vector
