/-
  SSZ Bitlist Tests — encoding with sentinel bit
-/

import LeanConsensus.SSZ

namespace Test.SSZ.Bitlist

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
  IO.println "\n── SSZ Bitlist ──"
  let mut total := 0
  let mut failures := 0

  -- Empty bitlist encoding: sentinel only → 0b00000001 = 0x01
  let bl := Bitlist.empty 256
  let encoded := SszEncode.sszEncode bl
  let (t, f) ← check "empty bitlist encodes to [0x01]"
    (encoded == ByteArray.mk #[0x01])
  total := total + t; failures := failures + f

  -- Decode the encoded empty bitlist
  let result := SszDecode.sszDecode (α := Bitlist 256) encoded
  let (t, f) ← check "empty bitlist roundtrip" (match result with
    | .ok bl' => bl'.length == 0
    | .error _ => false)
  total := total + t; failures := failures + f

  -- 3 bits: [1, 0, 1] → sentinel at bit 3 → 0b1101 = 0x0D
  match Bitlist.sszDecodeImpl (maxCap := 256) (ByteArray.mk #[0x0D]) with
  | .ok bl3' =>
    let (t, f) ← check "bitlist [1,0,1] length" (bl3'.length == 3)
    total := total + t; failures := failures + f
    -- Use Nat comparison for bounds instead of omega on runtime values
    let (t, f) ← check "bitlist [1,0,1] bit 0"
      (if h : 0 < bl3'.length then bl3'.getBit 0 h == true else false)
    total := total + t; failures := failures + f
    let (t, f) ← check "bitlist [1,0,1] bit 1"
      (if h : 1 < bl3'.length then bl3'.getBit 1 h == false else false)
    total := total + t; failures := failures + f
    let (t, f) ← check "bitlist [1,0,1] bit 2"
      (if h : 2 < bl3'.length then bl3'.getBit 2 h == true else false)
    total := total + t; failures := failures + f
  | .error e =>
    let (t, f) ← check s!"bitlist decode failed: {e}" false
    total := total + t; failures := failures + f

  -- Capacity exceeded
  let (t, f) ← check "bitlist capacity exceeded"
    (isError (Bitlist.sszDecodeImpl (maxCap := 2) (ByteArray.mk #[0x0D])))
  total := total + t; failures := failures + f

  return (total, failures)

end Test.SSZ.Bitlist
