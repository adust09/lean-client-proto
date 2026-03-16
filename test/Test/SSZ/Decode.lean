/-
  SSZ Decode Tests — verify primitive decoding from little-endian bytes
-/

import LeanConsensus.SSZ

namespace Test.SSZ.Decode

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
  IO.println "\n── SSZ Decode ──"
  let mut total := 0
  let mut failures := 0

  -- UInt8 decode
  let (t, f) ← check "UInt8 decode 42"
    (match SszDecode.sszDecode (ByteArray.mk #[42]) with
     | .ok (v : UInt8) => v == 42
     | .error _ => false)
  total := total + t; failures := failures + f

  -- UInt16 decode (little-endian)
  let (t, f) ← check "UInt16 decode 0x0100"
    (match SszDecode.sszDecode (ByteArray.mk #[0x00, 0x01]) with
     | .ok (v : UInt16) => v == 256
     | .error _ => false)
  total := total + t; failures := failures + f

  -- UInt32 decode
  let (t, f) ← check "UInt32 decode 1"
    (match SszDecode.sszDecode (ByteArray.mk #[0x01, 0x00, 0x00, 0x00]) with
     | .ok (v : UInt32) => v == 1
     | .error _ => false)
  total := total + t; failures := failures + f

  -- UInt64 decode
  let expected64 := ByteArray.mk #[0x2A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
  let (t, f) ← check "UInt64 decode 42"
    (match SszDecode.sszDecode expected64 with
     | .ok (v : UInt64) => v == 42
     | .error _ => false)
  total := total + t; failures := failures + f

  -- Bool decode
  let (t, f) ← check "Bool decode true"
    (match SszDecode.sszDecode (ByteArray.mk #[0x01]) with
     | .ok (v : Bool) => v == true
     | .error _ => false)
  total := total + t; failures := failures + f
  let (t, f) ← check "Bool decode false"
    (match SszDecode.sszDecode (ByteArray.mk #[0x00]) with
     | .ok (v : Bool) => v == false
     | .error _ => false)
  total := total + t; failures := failures + f

  -- Bool decode invalid
  let (t, f) ← check "Bool decode invalid byte"
    (isError (SszDecode.sszDecode (α := Bool) (ByteArray.mk #[0x02])))
  total := total + t; failures := failures + f

  -- Wrong size
  let (t, f) ← check "UInt32 decode wrong size"
    (isError (SszDecode.sszDecode (α := UInt32) (ByteArray.mk #[0x01, 0x02])))
  total := total + t; failures := failures + f

  -- BytesN decode
  let b4data := ByteArray.mk #[0x01, 0x02, 0x03, 0x04]
  let (t, f) ← check "Bytes4 decode"
    (match SszDecode.sszDecode (α := Bytes4) b4data with
     | .ok b => b.data == b4data
     | .error _ => false)
  total := total + t; failures := failures + f

  -- BytesN decode wrong size
  let (t, f) ← check "Bytes4 decode wrong size"
    (isError (SszDecode.sszDecode (α := Bytes4) (ByteArray.mk #[0x01, 0x02])))
  total := total + t; failures := failures + f

  return (total, failures)

end Test.SSZ.Decode
