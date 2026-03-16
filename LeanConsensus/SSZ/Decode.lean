/-
  SSZ Decoding — Primitive Instances + Container Decoder

  SSZ decoding rules:
  - All integers: little-endian byte order
  - Bool: single byte, must be exactly 0x00 or 0x01
  - Container decoding: read offset table from fixed region, then decode
    variable fields from their offset-delimited byte slices
-/

import LeanConsensus.SSZ.Error
import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.BytesN
import LeanConsensus.SSZ.Encode

namespace LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- Little-endian decoding helpers
-- ════════════════════════════════════════════════════════════════

/-- Read a UInt8 from a ByteArray at the given offset. -/
def decodeUInt8At (data : ByteArray) (off : Nat) : Except SszError UInt8 :=
  if off < data.size then
    .ok (data.get! off)
  else
    .error (.unexpectedEndOfInput 1 (data.size - off))

/-- Read a UInt16 (little-endian) from a ByteArray at the given offset. -/
def decodeUInt16At (data : ByteArray) (off : Nat) : Except SszError UInt16 :=
  if off + 2 ≤ data.size then
    let b0 := (data.get! off).toNat
    let b1 := (data.get! (off + 1)).toNat
    .ok (b0 ||| (b1 <<< 8)).toUInt16
  else
    .error (.unexpectedEndOfInput 2 (data.size - off))

/-- Read a UInt32 (little-endian) from a ByteArray at the given offset. -/
def decodeUInt32At (data : ByteArray) (off : Nat) : Except SszError UInt32 :=
  if off + 4 ≤ data.size then
    let b0 := (data.get! off).toNat
    let b1 := (data.get! (off + 1)).toNat
    let b2 := (data.get! (off + 2)).toNat
    let b3 := (data.get! (off + 3)).toNat
    .ok (b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24)).toUInt32
  else
    .error (.unexpectedEndOfInput 4 (data.size - off))

/-- Read a UInt64 (little-endian) from a ByteArray at the given offset. -/
def decodeUInt64At (data : ByteArray) (off : Nat) : Except SszError UInt64 :=
  if off + 8 ≤ data.size then
    let b0 := (data.get! off).toNat
    let b1 := (data.get! (off + 1)).toNat
    let b2 := (data.get! (off + 2)).toNat
    let b3 := (data.get! (off + 3)).toNat
    let b4 := (data.get! (off + 4)).toNat
    let b5 := (data.get! (off + 5)).toNat
    let b6 := (data.get! (off + 6)).toNat
    let b7 := (data.get! (off + 7)).toNat
    .ok (b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24) |||
         (b4 <<< 32) ||| (b5 <<< 40) ||| (b6 <<< 48) ||| (b7 <<< 56)).toUInt64
  else
    .error (.unexpectedEndOfInput 8 (data.size - off))

/-- Read a Bool from a ByteArray at the given offset. -/
def decodeBoolAt (data : ByteArray) (off : Nat) : Except SszError Bool :=
  if off < data.size then
    let b := data.get! off
    if b == 0 then .ok false
    else if b == 1 then .ok true
    else .error (.invalidBooleanByte b)
  else
    .error (.unexpectedEndOfInput 1 (data.size - off))

-- ════════════════════════════════════════════════════════════════
-- SszDecode instances for primitive types
-- ════════════════════════════════════════════════════════════════

instance : SszDecode UInt8 where
  sszDecode data :=
    if data.size != 1 then
      .error (.invalidLength 1 data.size)
    else
      decodeUInt8At data 0

instance : SszDecode UInt16 where
  sszDecode data :=
    if data.size != 2 then
      .error (.invalidLength 2 data.size)
    else
      decodeUInt16At data 0

instance : SszDecode UInt32 where
  sszDecode data :=
    if data.size != 4 then
      .error (.invalidLength 4 data.size)
    else
      decodeUInt32At data 0

instance : SszDecode UInt64 where
  sszDecode data :=
    if data.size != 8 then
      .error (.invalidLength 8 data.size)
    else
      decodeUInt64At data 0

instance : SszDecode Bool where
  sszDecode data :=
    if data.size != 1 then
      .error (.invalidLength 1 data.size)
    else
      decodeBoolAt data 0

-- ════════════════════════════════════════════════════════════════
-- Container Decoding Helpers
-- ════════════════════════════════════════════════════════════════

/-- A cursor for sequential reading from a ByteArray. -/
structure SszDecoder where
  data : ByteArray
  offset : Nat := 0
  /-- Offsets collected for variable-size fields. -/
  variableOffsets : Array UInt32 := #[]

namespace SszDecoder

/-- Create a decoder from a ByteArray. -/
def new (data : ByteArray) : SszDecoder :=
  { data := data }

/-- Read a fixed-size field and advance the cursor. -/
def readFixed (dec : SszDecoder) (size : Nat) : Except SszError (ByteArray × SszDecoder) :=
  if dec.offset + size ≤ dec.data.size then
    let slice := dec.data.extract dec.offset (dec.offset + size)
    .ok (slice, { dec with offset := dec.offset + size })
  else
    .error (.unexpectedEndOfInput size (dec.data.size - dec.offset))

/-- Read a 4-byte offset (for a variable-size field) and record it. -/
def readOffset (dec : SszDecoder) : Except SszError SszDecoder := do
  let offset ← decodeUInt32At dec.data dec.offset
  .ok {
    dec with
    offset := dec.offset + 4
    variableOffsets := dec.variableOffsets.push offset
  }

/-- After reading all fixed fields and offsets, extract variable-size field
    byte slices. The slices are delimited by consecutive offsets, with the
    last slice ending at the buffer end. -/
def extractVariableSlices (dec : SszDecoder) : Except SszError (Array ByteArray) := Id.run do
  let offsets := dec.variableOffsets
  let bufSize := dec.data.size
  if offsets.isEmpty then
    return .ok #[]
  let mut result : Array ByteArray := #[]
  for i in [:offsets.size] do
    let start := offsets[i]!.toNat
    let stop :=
      if i + 1 < offsets.size then offsets[i + 1]!.toNat
      else bufSize
    if start > stop then
      return .error (.offsetsNotMonotonic offsets[i]! (if i + 1 < offsets.size then offsets[i+1]! else bufSize.toUInt32))
    if stop > bufSize then
      return .error (.invalidOffset offsets[i]! bufSize)
    result := result.push (dec.data.extract start stop)
  return .ok result

end SszDecoder

end LeanConsensus.SSZ
