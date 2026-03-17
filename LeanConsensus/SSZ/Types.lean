/-
  SSZ Typeclass Hierarchy

  Defines the core typeclasses that all SSZ-serializable types must implement:
  - SszType:        metadata (fixed vs variable size)
  - SszEncode:      serialization to ByteArray
  - SszDecode:      deserialization from ByteArray
  - SszHashTreeRoot: Merkle hash_tree_root computation

  The hierarchy uses extends to enforce that encoding/decoding types
  always carry their SSZ metadata.
-/

import LeanConsensus.SSZ.Error

namespace LeanConsensus.SSZ

/-- SSZ metadata: whether a type has a fixed or variable serialized size. -/
class SszType (α : Type) where
  /-- `some n` if fixed-size with `n` bytes, `none` if variable-size. -/
  sszFixedSize : Option UInt32

/-- SSZ encoding to a byte array. -/
class SszEncode (α : Type) extends SszType α where
  /-- Serialize a value to its SSZ byte representation. -/
  sszEncode : α → ByteArray

/-- SSZ decoding from a byte array. -/
class SszDecode (α : Type) extends SszType α where
  /-- Deserialize a value from its SSZ byte representation. -/
  sszDecode : ByteArray → Except SszError α

/-- A 32-byte hash value. Forward-declared here to avoid circular imports. -/
abbrev Bytes32Data := ByteArray

/-- SSZ Merkle hash_tree_root computation. -/
class SszHashTreeRoot (α : Type) extends SszType α where
  /-- Compute the Merkle hash tree root of a value. -/
  hashTreeRoot : α → Bytes32Data

/-- Helper: check if a type is fixed-size. -/
def isFixedSize {α : Type} [inst : SszType α] : Bool :=
  inst.sszFixedSize.isSome

/-- Helper: get the fixed size or panic. Only for use in known-fixed contexts. -/
def getFixedSize {α : Type} [inst : SszType α] : Option UInt32 :=
  inst.sszFixedSize

/-- Marker typeclass for SSZ "basic" types whose serialized bytes can be packed
    directly into chunks for hash_tree_root (as opposed to composite types where
    each element is hashed individually). -/
class SszPackable (α : Type) extends SszEncode α

end LeanConsensus.SSZ
