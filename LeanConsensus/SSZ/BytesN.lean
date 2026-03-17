/-
  BytesN — Fixed-Length Byte Arrays with Compile-Time Length Proofs

  The key advantage over Haskell: the length invariant `hsize : data.size = n`
  is a proof carried in the type. Invalid BytesN values cannot be constructed —
  the type checker rejects them at compile time.
-/

import LeanConsensus.SSZ.Error
import LeanConsensus.SSZ.Types

namespace LeanConsensus.SSZ

/-- Fixed-length byte array with compile-time length proof. -/
structure BytesN (n : Nat) where
  data : ByteArray
  hsize : data.size = n

instance (n : Nat) : BEq (BytesN n) where
  beq a b := a.data == b.data

instance (n : Nat) : Hashable (BytesN n) where
  hash b := hash b.data

instance (n : Nat) : ToString (BytesN n) where
  toString b := s!"BytesN({n}, {b.data.toList.map fun x => x.toNat})"

/-- The length invariant is trivially available from the structure. -/
theorem bytesN_size {n : Nat} (b : BytesN n) : b.data.size = n := b.hsize

namespace BytesN

/-- Create a BytesN filled with zeros. -/
def zero (n : Nat) : BytesN n :=
  ⟨ByteArray.mk (Array.replicate n 0), by simp [ByteArray.size, Array.size_replicate]⟩

/-- Attempt to create a BytesN from a raw ByteArray. Returns an error
    if the size does not match. -/
def mkChecked {n : Nat} (data : ByteArray) : Except SszError (BytesN n) :=
  if h : data.size = n then
    .ok ⟨data, h⟩
  else
    .error (.invalidLength n data.size)

/-- Get a byte at an index with bounds checking. -/
def get {n : Nat} (b : BytesN n) (i : Nat) (h : i < n) : UInt8 :=
  b.data.get! i

/-- Convert to raw ByteArray (drops the proof). -/
def toByteArray {n : Nat} (b : BytesN n) : ByteArray := b.data

end BytesN

-- Common aliases
abbrev Bytes4  := BytesN 4
abbrev Bytes32 := BytesN 32
abbrev Bytes48 := BytesN 48

/-- Convenience: zero-valued Bytes32 (used frequently in Merkleization). -/
def Bytes32.zero : Bytes32 := BytesN.zero 32

-- SSZ instances for BytesN
instance (n : Nat) : SszType (BytesN n) where
  sszFixedSize := some n.toUInt32

instance (n : Nat) : SszEncode (BytesN n) where
  sszEncode b := b.data

instance (n : Nat) : SszDecode (BytesN n) where
  sszDecode data := BytesN.mkChecked data

instance (n : Nat) : SszPackable (BytesN n) where

end LeanConsensus.SSZ
