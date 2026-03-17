/-
  Bitvector — Fixed-Length Bit Array

  Packed LSB-first into bytes. No sentinel bit (unlike Bitlist).
  Serialized size: ceil(n / 8) bytes.
-/

import LeanConsensus.SSZ.Error
import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.BytesN
import LeanConsensus.SSZ.Merkleization

namespace LeanConsensus.SSZ

/-- Fixed-length bit array with `n` bits, packed LSB-first. -/
structure Bitvector (n : Nat) where
  data : ByteArray
  hsize : data.size = (n + 7) / 8

instance (n : Nat) : BEq (Bitvector n) where
  beq a b := a.data == b.data

instance (n : Nat) : ToString (Bitvector n) where
  toString bv := s!"Bitvector({n})"

namespace Bitvector

/-- Create a zero-filled bitvector. -/
def zero (n : Nat) : Bitvector n :=
  ⟨ByteArray.mk (Array.replicate ((n + 7) / 8) 0),
    by simp [ByteArray.size, Array.size_replicate]⟩

/-- Attempt to create a Bitvector from raw bytes. -/
def mkChecked {n : Nat} (data : ByteArray) : Except SszError (Bitvector n) :=
  let expected := (n + 7) / 8
  if h : data.size = expected then
    .ok ⟨data, h⟩
  else
    .error (.invalidLength expected data.size)

/-- Get a bit at index `i` (0-indexed, LSB-first within each byte). -/
def getBit {n : Nat} (bv : Bitvector n) (i : Nat) (_h : i < n) : Bool :=
  let byteIdx := i / 8
  let bitIdx := i % 8
  let byte := bv.data.get! byteIdx
  (byte >>> bitIdx.toUInt8) &&& 1 == 1

/-- Set a bit at index `i`. -/
def setBit {n : Nat} (bv : Bitvector n) (i : Nat) (_h : i < n) (v : Bool) : Bitvector n :=
  let byteIdx := i / 8
  let bitIdx := i % 8
  let byte := bv.data.get! byteIdx
  let mask := (1 : UInt8) <<< bitIdx.toUInt8
  let newByte := if v then byte ||| mask else byte &&& (~~~mask)
  let newData := bv.data.set! byteIdx newByte
  ⟨newData, by
    show newData.size = (n + 7) / 8
    simp only [newData, ByteArray.size, ByteArray.set!,
               Array.set!_eq_setIfInBounds, Array.size_setIfInBounds]
    exact bv.hsize⟩

/-- Count the number of set bits. -/
def popcount {n : Nat} (bv : Bitvector n) : Nat := Id.run do
  let mut count := 0
  for i in [:n] do
    let byteIdx := i / 8
    let bitIdx := i % 8
    let byte := bv.data.get! byteIdx
    if (byte >>> bitIdx.toUInt8) &&& 1 == 1 then
      count := count + 1
  return count

end Bitvector

-- SSZ instances
instance (n : Nat) : SszType (Bitvector n) where
  sszFixedSize := some ((n + 7) / 8).toUInt32

instance (n : Nat) : SszEncode (Bitvector n) where
  sszEncode bv := bv.data

instance (n : Nat) : SszDecode (Bitvector n) where
  sszDecode data := Bitvector.mkChecked data

instance (n : Nat) : SszHashTreeRoot (Bitvector n) where
  hashTreeRoot bv :=
    let chunks := packBits bv.data
    merkleize chunks ((n + 255) / 256)

end LeanConsensus.SSZ
