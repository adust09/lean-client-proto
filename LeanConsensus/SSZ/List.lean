/-
  SszList — Variable-Length Bounded Collection

  An SSZ List is a variable-length collection with a maximum capacity.
  Unlike SszVector, the actual length can vary — the proof `hbound`
  ensures it never exceeds `maxCap`.
-/

import LeanConsensus.SSZ.Error
import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.Encode
import LeanConsensus.SSZ.Decode
import LeanConsensus.SSZ.Merkleization

namespace LeanConsensus.SSZ

/-- Variable-length array with maximum capacity bound proof. -/
structure SszList (maxCap : Nat) (α : Type) where
  elems : Array α
  hbound : elems.size ≤ maxCap

instance {α : Type} [BEq α] (maxCap : Nat) : BEq (SszList maxCap α) where
  beq a b := a.elems == b.elems

namespace SszList

/-- Create an empty SszList. -/
def empty {maxCap : Nat} {α : Type} : SszList maxCap α :=
  ⟨#[], Nat.zero_le _⟩

/-- Attempt to create an SszList from a raw Array. -/
def mkChecked {maxCap : Nat} {α : Type} (arr : Array α) : Except SszError (SszList maxCap α) :=
  if h : arr.size ≤ maxCap then
    .ok ⟨arr, h⟩
  else
    .error (.capacityExceeded maxCap arr.size)

/-- Get the number of elements. -/
def length {maxCap : Nat} {α : Type} (l : SszList maxCap α) : Nat := l.elems.size

/-- Push an element, returning error if at capacity. -/
def push {maxCap : Nat} {α : Type} (l : SszList maxCap α) (v : α) : Except SszError (SszList maxCap α) :=
  let newElems := l.elems.push v
  if h : newElems.size ≤ maxCap then
    .ok ⟨newElems, h⟩
  else
    .error (.capacityExceeded maxCap newElems.size)

/-- Map a function over all elements. -/
def map {maxCap : Nat} {α β : Type} (l : SszList maxCap α) (f : α → β) : SszList maxCap β :=
  ⟨l.elems.map f, by simp; exact l.hbound⟩

/-- Fold over elements. -/
def foldl {maxCap : Nat} {α β : Type} (l : SszList maxCap α) (init : β) (f : β → α → β) : β :=
  l.elems.foldl f init

end SszList

-- SszList is always variable-size
instance {α : Type} [SszType α] (maxCap : Nat) : SszType (SszList maxCap α) where
  sszFixedSize := none

/-- Encode a list of fixed-size elements: concatenate serialized elements. -/
def encodeFixedList {α : Type} {maxCap : Nat} [SszEncode α] (l : SszList maxCap α) : ByteArray :=
  l.elems.foldl (init := ByteArray.empty) fun acc elem =>
    acc ++ SszEncode.sszEncode elem

/-- Encode a list of variable-size elements: offset table + data. -/
def encodeVariableList {α : Type} {maxCap : Nat} [SszEncode α] (l : SszList maxCap α) : ByteArray :=
  let n := l.elems.size
  let encoded := l.elems.map SszEncode.sszEncode
  let offsetTableSize := n * 4
  let (offsets, dataRegion) := encoded.foldl (init := (ByteArray.empty, ByteArray.empty))
    fun (offs, dta) bs =>
      let offset := (offsetTableSize + dta.size).toUInt32
      (offs ++ encodeUInt32 offset, dta ++ bs)
  offsets ++ dataRegion

instance {α : Type} [SszEncode α] (maxCap : Nat) : SszEncode (SszList maxCap α) where
  sszEncode l :=
    match SszType.sszFixedSize (α := α) with
    | some _ => encodeFixedList l
    | none   => encodeVariableList l

/-- Decode a list of fixed-size elements. Length inferred from data.size / elemSize. -/
def decodeFixedList {α : Type} {maxCap : Nat} [SszDecode α] (data : ByteArray) (elemSize : Nat) :
    Except SszError (SszList maxCap α) := Id.run do
  if elemSize == 0 then
    return .error (.other "cannot decode list of zero-size elements")
  if data.size % elemSize != 0 then
    return .error (.invalidLength 0 data.size)
  let n := data.size / elemSize
  let mut elems : Array α := #[]
  for i in [:n] do
    let start := i * elemSize
    let slice := data.extract start (start + elemSize)
    match SszDecode.sszDecode slice with
    | .ok elem => elems := elems.push elem
    | .error e => return .error e
  match SszList.mkChecked elems with
  | .ok l => return .ok l
  | .error e => return .error e

/-- Decode a list of variable-size elements using offset table. -/
def decodeVariableList {α : Type} {maxCap : Nat} [SszDecode α] (data : ByteArray) :
    Except SszError (SszList maxCap α) := Id.run do
  if data.size == 0 then
    match SszList.mkChecked (α := α) #[] with
    | .ok l => return .ok l
    | .error e => return .error e
  else
    -- First offset tells us the offset table size, hence element count
    match decodeUInt32At data 0 with
    | .error e => return .error e
    | .ok firstOffset =>
      let n := firstOffset.toNat / 4
      if n * 4 != firstOffset.toNat then
        return .error (.invalidOffset firstOffset data.size)
      -- Read all offsets
      let mut offsets : Array UInt32 := #[]
      for i in [:n] do
        match decodeUInt32At data (i * 4) with
        | .ok off => offsets := offsets.push off
        | .error e => return .error e
      -- Decode each element
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
      match SszList.mkChecked elems with
      | .ok l => return .ok l
      | .error e => return .error e

instance {α : Type} [SszDecode α] (maxCap : Nat) : SszDecode (SszList maxCap α) where
  sszDecode data :=
    match SszType.sszFixedSize (α := α) with
    | some elemSize => decodeFixedList data elemSize.toNat
    | none          => decodeVariableList data

/-- SszHashTreeRoot for SszList of packable (basic) types: pack serialized data + mixInLength. -/
instance (priority := 100) {α : Type} [SszPackable α] (maxCap : Nat) : SszHashTreeRoot (SszList maxCap α) where
  hashTreeRoot l :=
    let serialized := l.elems.foldl (init := ByteArray.empty) fun acc elem =>
      acc ++ SszEncode.sszEncode elem
    let chunks := pack serialized
    let limit := match SszType.sszFixedSize (α := α) with
      | some elemSize => chunkCountFixed maxCap elemSize.toNat
      | none => maxCap
    mixInLength (merkleize chunks limit) l.elems.size

/-- SszHashTreeRoot for SszList of composite types: hash each element + mixInLength. -/
instance (priority := 50) {α : Type} [SszHashTreeRoot α] (maxCap : Nat) : SszHashTreeRoot (SszList maxCap α) where
  hashTreeRoot l :=
    let chunks := l.elems.map SszHashTreeRoot.hashTreeRoot
    mixInLength (merkleize chunks (chunkCountVariable maxCap)) l.elems.size

end LeanConsensus.SSZ
