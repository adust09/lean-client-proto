/-
  SSZ Encoding — Primitive Instances + Two-Pass Container Encoder

  SSZ encoding rules:
  - All integers: little-endian byte order
  - Bool: single byte (0x00 or 0x01)
  - Fixed-size types: serialized inline
  - Variable-size types: 4-byte LE offset in fixed region, data appended after

  The SszEncoder is a two-pass container encoder:
    Pass 1: Collect fields as fixed or variable entries
    Pass 2: Compute offsets, write fixed region + offset table, append variable data
-/

import LeanConsensus.SSZ.Error
import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.BytesN

namespace LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- Little-endian encoding helpers
-- ════════════════════════════════════════════════════════════════

/-- Encode a UInt8 as a single byte. -/
def encodeUInt8 (v : UInt8) : ByteArray :=
  ByteArray.mk #[v]

/-- Encode a UInt16 as 2 bytes, little-endian. -/
def encodeUInt16 (v : UInt16) : ByteArray :=
  let n := v.toNat
  ByteArray.mk #[
    (n &&& 0xFF).toUInt8,
    ((n >>> 8) &&& 0xFF).toUInt8
  ]

/-- Encode a UInt32 as 4 bytes, little-endian. -/
def encodeUInt32 (v : UInt32) : ByteArray :=
  let n := v.toNat
  ByteArray.mk #[
    (n &&& 0xFF).toUInt8,
    ((n >>> 8) &&& 0xFF).toUInt8,
    ((n >>> 16) &&& 0xFF).toUInt8,
    ((n >>> 24) &&& 0xFF).toUInt8
  ]

/-- Encode a UInt64 as 8 bytes, little-endian. -/
def encodeUInt64 (v : UInt64) : ByteArray :=
  let n := v.toNat
  ByteArray.mk #[
    (n &&& 0xFF).toUInt8,
    ((n >>> 8) &&& 0xFF).toUInt8,
    ((n >>> 16) &&& 0xFF).toUInt8,
    ((n >>> 24) &&& 0xFF).toUInt8,
    ((n >>> 32) &&& 0xFF).toUInt8,
    ((n >>> 40) &&& 0xFF).toUInt8,
    ((n >>> 48) &&& 0xFF).toUInt8,
    ((n >>> 56) &&& 0xFF).toUInt8
  ]

/-- Encode a Bool as a single byte. -/
def encodeBool (v : Bool) : ByteArray :=
  ByteArray.mk #[if v then 1 else 0]

-- ════════════════════════════════════════════════════════════════
-- SszEncode instances for primitive types
-- ════════════════════════════════════════════════════════════════

instance : SszType UInt8 where sszFixedSize := some 1
instance : SszEncode UInt8 where sszEncode := encodeUInt8

instance : SszType UInt16 where sszFixedSize := some 2
instance : SszEncode UInt16 where sszEncode := encodeUInt16

instance : SszType UInt32 where sszFixedSize := some 4
instance : SszEncode UInt32 where sszEncode := encodeUInt32

instance : SszType UInt64 where sszFixedSize := some 8
instance : SszEncode UInt64 where sszEncode := encodeUInt64

instance : SszType Bool where sszFixedSize := some 1
instance : SszEncode Bool where sszEncode := encodeBool

-- ════════════════════════════════════════════════════════════════
-- Two-Pass Container Encoder
-- ════════════════════════════════════════════════════════════════

/-- A single field entry in the container encoder. -/
inductive FieldEntry where
  /-- A fixed-size field, serialized inline. -/
  | fixed (bs : ByteArray)
  /-- A variable-size field, serialized in the variable region. -/
  | variable (bs : ByteArray)

/-- Two-pass SSZ container encoder.
    Accumulates field entries, then finalizes into a single ByteArray. -/
structure SszEncoder where
  fields : Array FieldEntry := #[]

namespace SszEncoder

/-- Add a field to the encoder. Automatically determines fixed vs variable
    based on the type's SszType instance. -/
def addField {α : Type} [SszEncode α] (enc : SszEncoder) (v : α) : SszEncoder :=
  let encoded := SszEncode.sszEncode v
  match SszType.sszFixedSize (α := α) with
  | some _ => { fields := enc.fields.push (.fixed encoded) }
  | none   => { fields := enc.fields.push (.variable encoded) }

/-- Finalize the encoder into a single ByteArray.

    Pass 1: Compute the fixed region size. Each fixed field contributes its
    serialized size; each variable field contributes 4 bytes (offset slot).

    Pass 2: Write fixed fields inline and variable field offsets in the
    fixed region, then append all variable field data in order. -/
def finalize (enc : SszEncoder) : ByteArray :=
  -- Pass 1: compute fixed region size
  let fixedRegionSize := enc.fields.foldl (init := 0) fun acc entry =>
    match entry with
    | .fixed bs => acc + bs.size
    | .variable _ => acc + 4  -- 4-byte offset slot
  -- Pass 2: write output
  let (fixedPart, varPart) := enc.fields.foldl (init := (ByteArray.empty, ByteArray.empty))
    fun (fixedAcc, varAcc) entry =>
      match entry with
      | .fixed bs => (fixedAcc ++ bs, varAcc)
      | .variable bs =>
        let offset := (fixedRegionSize + varAcc.size).toUInt32
        (fixedAcc ++ encodeUInt32 offset, varAcc ++ bs)
  fixedPart ++ varPart

end SszEncoder

-- ════════════════════════════════════════════════════════════════
-- Convenience: ByteArray append helper
-- ════════════════════════════════════════════════════════════════

/-- Append raw bytes to a ByteArray (for building serialized output). -/
def appendBytes (buf : ByteArray) (data : ByteArray) : ByteArray :=
  buf ++ data

end LeanConsensus.SSZ
