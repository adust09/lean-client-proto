/-
  SSZ Error Types

  Defines the error type hierarchy for SSZ encoding, decoding, and
  merkleization operations.
-/

namespace LeanConsensus.SSZ

/-- Errors that can occur during SSZ operations. -/
inductive SszError where
  /-- Byte buffer ended before expected data. -/
  | unexpectedEndOfInput (expected remaining : Nat)
  /-- A decoded length field did not match the expected fixed size. -/
  | invalidLength (expected actual : Nat)
  /-- A decoded offset was out of bounds or inconsistent. -/
  | invalidOffset (offset : UInt32) (bufferSize : Nat)
  /-- Offsets in a container were not monotonically increasing. -/
  | offsetsNotMonotonic (prev next : UInt32)
  /-- A boolean byte was neither 0x00 nor 0x01. -/
  | invalidBooleanByte (value : UInt8)
  /-- Bitlist sentinel bit was not found. -/
  | missingSentinelBit
  /-- A decoded collection exceeds its maximum capacity. -/
  | capacityExceeded (maxCap actual : Nat)
  /-- A variable-size region had leftover bytes after decoding. -/
  | trailingBytes (count : Nat)
  /-- Generic error with a description. -/
  | other (msg : String)
  deriving Repr, BEq

instance : ToString SszError where
  toString
    | .unexpectedEndOfInput e r => s!"SSZ: unexpected end of input (expected {e} bytes, {r} remaining)"
    | .invalidLength e a => s!"SSZ: invalid length (expected {e}, got {a})"
    | .invalidOffset o s => s!"SSZ: invalid offset {o} for buffer size {s}"
    | .offsetsNotMonotonic p n => s!"SSZ: offsets not monotonic ({p} >= {n})"
    | .invalidBooleanByte v => s!"SSZ: invalid boolean byte 0x{String.ofList (Nat.toDigits 16 v.toNat)}"
    | .missingSentinelBit => "SSZ: missing sentinel bit in bitlist"
    | .capacityExceeded m a => s!"SSZ: capacity exceeded (max {m}, got {a})"
    | .trailingBytes c => s!"SSZ: {c} trailing bytes after decode"
    | .other msg => s!"SSZ: {msg}"

end LeanConsensus.SSZ
