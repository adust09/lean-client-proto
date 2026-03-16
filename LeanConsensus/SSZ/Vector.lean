/-
  SszVector — Fixed-Length Collection with Length Proof

  An SSZ Vector is a fixed-length homogeneous collection. The length `n`
  is part of the type, and the proof `hlen : elems.size = n` ensures the
  invariant at the type level.
-/

import LeanConsensus.SSZ.Error
import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.Encode
import LeanConsensus.SSZ.Decode

namespace LeanConsensus.SSZ

/-- Fixed-length array with compile-time length proof. -/
structure SszVector (n : Nat) (α : Type) where
  elems : Array α
  hlen : elems.size = n

instance {α : Type} [BEq α] (n : Nat) : BEq (SszVector n α) where
  beq a b := a.elems == b.elems

namespace SszVector

/-- Attempt to create an SszVector from a raw Array. -/
def mkChecked {n : Nat} {α : Type} (arr : Array α) : Except SszError (SszVector n α) :=
  if h : arr.size = n then
    .ok ⟨arr, h⟩
  else
    .error (.invalidLength n arr.size)

/-- Get element at index. -/
def get {n : Nat} {α : Type} (v : SszVector n α) (i : Nat) (h : i < n) : α :=
  v.elems[i]'(by rw [v.hlen]; exact h)

/-- Map a function over all elements. -/
def map {n : Nat} {α β : Type} (v : SszVector n α) (f : α → β) : SszVector n β :=
  ⟨v.elems.map f, by simp [v.hlen]⟩

/-- Fold over elements. -/
def foldl {n : Nat} {α β : Type} (v : SszVector n α) (init : β) (f : β → α → β) : β :=
  v.elems.foldl f init

end SszVector

-- SSZ instances for SszVector
instance {α : Type} [SszType α] (n : Nat) : SszType (SszVector n α) where
  sszFixedSize :=
    match SszType.sszFixedSize (α := α) with
    | some elemSize => some (n.toUInt32 * elemSize)
    | none => none

/-- Encode a vector of fixed-size elements: concatenate serialized elements. -/
def encodeFixedVector {α : Type} {n : Nat} [SszEncode α] (v : SszVector n α) : ByteArray :=
  v.elems.foldl (init := ByteArray.empty) fun acc elem =>
    acc ++ SszEncode.sszEncode elem

/-- Encode a vector of variable-size elements: offset table + data. -/
def encodeVariableVector {α : Type} {n : Nat} [SszEncode α] (v : SszVector n α) : ByteArray :=
  let encoded := v.elems.map SszEncode.sszEncode
  let offsetTableSize := n * 4
  let (offsets, dataRegion) := encoded.foldl (init := (ByteArray.empty, ByteArray.empty))
    fun (offs, dta) bs =>
      let offset := (offsetTableSize + dta.size).toUInt32
      (offs ++ encodeUInt32 offset, dta ++ bs)
  offsets ++ dataRegion

instance {α : Type} [SszEncode α] (n : Nat) : SszEncode (SszVector n α) where
  sszEncode v :=
    match SszType.sszFixedSize (α := α) with
    | some _ => encodeFixedVector v
    | none   => encodeVariableVector v

/-- Decode a vector of fixed-size elements. -/
def decodeFixedVector {α : Type} {n : Nat} [SszDecode α] (data : ByteArray) (elemSize : Nat) :
    Except SszError (SszVector n α) := Id.run do
  let expected := n * elemSize
  if data.size != expected then
    return .error (.invalidLength expected data.size)
  let mut elems : Array α := #[]
  for i in [:n] do
    let start := i * elemSize
    let slice := data.extract start (start + elemSize)
    match SszDecode.sszDecode slice with
    | .ok elem => elems := elems.push elem
    | .error e => return .error e
  match SszVector.mkChecked elems with
  | .ok v => return .ok v
  | .error e => return .error e

/-- Decode a vector of variable-size elements using offset table. -/
def decodeVariableVector {α : Type} {n : Nat} [SszDecode α] (data : ByteArray) :
    Except SszError (SszVector n α) := Id.run do
  if n == 0 then
    if data.size != 0 then
      return .error (.trailingBytes data.size)
    match SszVector.mkChecked (α := α) #[] with
    | .ok v => return .ok v
    | .error e => return .error e
  else
    -- Read offset table
    let mut offsets : Array UInt32 := #[]
    for i in [:n] do
      match decodeUInt32At data (i * 4) with
      | .ok off => offsets := offsets.push off
      | .error e => return .error e
    -- Decode each variable element from its offset-delimited slice
    let mut elems : Array α := #[]
    for i in [:n] do
      let start := offsets[i]!.toNat
      let stop := if i + 1 < n then offsets[i + 1]!.toNat else data.size
      if start > stop then
        return .error (.offsetsNotMonotonic offsets[i]! (if i + 1 < n then offsets[i+1]! else data.size.toUInt32))
      let slice := data.extract start stop
      match SszDecode.sszDecode slice with
      | .ok elem => elems := elems.push elem
      | .error e => return .error e
    match SszVector.mkChecked elems with
    | .ok v => return .ok v
    | .error e => return .error e

instance {α : Type} [SszDecode α] (n : Nat) : SszDecode (SszVector n α) where
  sszDecode data :=
    match SszType.sszFixedSize (α := α) with
    | some elemSize => decodeFixedVector data elemSize.toNat
    | none          => decodeVariableVector data

end LeanConsensus.SSZ
