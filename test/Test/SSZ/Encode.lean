/-
  SSZ Encode Tests — verify primitive encoding to little-endian bytes
-/

import LeanConsensus.SSZ

namespace Test.SSZ.Encode

open LeanConsensus.SSZ

/-- Simple test assertion helper. Returns (total, failures). -/
def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── SSZ Encode ──"
  let mut total := 0
  let mut failures := 0

  -- UInt8 encoding
  let (t, f) ← check "UInt8 encode 0" (SszEncode.sszEncode (0 : UInt8) == ByteArray.mk #[0])
  total := total + t; failures := failures + f
  let (t, f) ← check "UInt8 encode 255" (SszEncode.sszEncode (255 : UInt8) == ByteArray.mk #[0xFF])
  total := total + t; failures := failures + f

  -- UInt16 encoding (little-endian)
  let (t, f) ← check "UInt16 encode 0x0100" (SszEncode.sszEncode (256 : UInt16) == ByteArray.mk #[0x00, 0x01])
  total := total + t; failures := failures + f

  -- UInt32 encoding (little-endian)
  let (t, f) ← check "UInt32 encode 1" (SszEncode.sszEncode (1 : UInt32) == ByteArray.mk #[0x01, 0x00, 0x00, 0x00])
  total := total + t; failures := failures + f

  -- UInt64 encoding (little-endian)
  let expected := ByteArray.mk #[0x2A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
  let (t, f) ← check "UInt64 encode 42" (SszEncode.sszEncode (42 : UInt64) == expected)
  total := total + t; failures := failures + f

  -- Bool encoding
  let (t, f) ← check "Bool encode true" (SszEncode.sszEncode true == ByteArray.mk #[0x01])
  total := total + t; failures := failures + f
  let (t, f) ← check "Bool encode false" (SszEncode.sszEncode false == ByteArray.mk #[0x00])
  total := total + t; failures := failures + f

  -- BytesN encoding
  let b32 := Bytes32.zero
  let (t, f) ← check "Bytes32 encode zero" (SszEncode.sszEncode b32 == ByteArray.mk (Array.replicate 32 0))
  total := total + t; failures := failures + f

  return (total, failures)

end Test.SSZ.Encode
