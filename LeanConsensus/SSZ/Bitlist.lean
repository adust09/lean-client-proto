/-
  Bitlist — Variable-Length Bit Array with Sentinel

  SSZ encoding: pack bits LSB-first, then append a sentinel `1` bit.
  The sentinel marks the end of data — on decode, find the highest set
  bit in the last byte to determine the true length.

  Total serialized size: ceil((length + 1) / 8) bytes.
-/

import LeanConsensus.SSZ.Error
import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.Merkleization

namespace LeanConsensus.SSZ

/-- Variable-length bit array with maximum capacity `maxCap`. -/
structure Bitlist (maxCap : Nat) where
  data : ByteArray
  length : Nat
  hbound : length ≤ maxCap
  hsize : data.size = (length + 7) / 8

instance (maxCap : Nat) : BEq (Bitlist maxCap) where
  beq a b := a.data == b.data && a.length == b.length

instance (maxCap : Nat) : ToString (Bitlist maxCap) where
  toString bl := s!"Bitlist(len={bl.length}, max={maxCap})"

namespace Bitlist

/-- Create an empty bitlist. -/
def empty (maxCap : Nat) : Bitlist maxCap :=
  ⟨ByteArray.empty, 0, Nat.zero_le _, by simp [ByteArray.size]⟩

/-- Create a zero-filled bitlist of given length. -/
def zeros {maxCap : Nat} (len : Nat) (h : len ≤ maxCap) : Bitlist maxCap :=
  ⟨ByteArray.mk (Array.replicate ((len + 7) / 8) 0), len, h,
    by simp [ByteArray.size, Array.size_replicate]⟩

/-- Get a bit at index `i`. -/
def getBit {maxCap : Nat} (bl : Bitlist maxCap) (i : Nat) (_h : i < bl.length) : Bool :=
  let byteIdx := i / 8
  let bitIdx := i % 8
  let byte := bl.data.get! byteIdx
  (byte >>> bitIdx.toUInt8) &&& 1 == 1

/-- Count set bits. -/
def popcount {maxCap : Nat} (bl : Bitlist maxCap) : Nat := Id.run do
  let mut count := 0
  for i in [:bl.length] do
    let byteIdx := i / 8
    let bitIdx := i % 8
    let byte := bl.data.get! byteIdx
    if (byte >>> bitIdx.toUInt8) &&& 1 == 1 then
      count := count + 1
  return count

/-- SSZ-encode a bitlist: pack bits + append sentinel bit. -/
def sszEncodeImpl {maxCap : Nat} (bl : Bitlist maxCap) : ByteArray := Id.run do
  let totalBits := bl.length + 1
  let totalBytes := (totalBits + 7) / 8
  let mut result := ByteArray.mk (Array.replicate totalBytes 0)
  -- Copy data bits
  for i in [:bl.length] do
    let byteIdx := i / 8
    let bitIdx := i % 8
    let srcByte := bl.data.get! byteIdx
    if (srcByte >>> bitIdx.toUInt8) &&& 1 == 1 then
      let dstByte := result.get! byteIdx
      result := result.set! byteIdx (dstByte ||| ((1 : UInt8) <<< bitIdx.toUInt8))
  -- Set sentinel bit at position `length`
  let sentByteIdx := bl.length / 8
  let sentBitIdx := bl.length % 8
  let sentByte := result.get! sentByteIdx
  result := result.set! sentByteIdx (sentByte ||| ((1 : UInt8) <<< sentBitIdx.toUInt8))
  return result

/-- SSZ-decode a bitlist: find sentinel, extract data bits. -/
def sszDecodeImpl {maxCap : Nat} (data : ByteArray) : Except SszError (Bitlist maxCap) := do
  if data.size == 0 then
    .error .missingSentinelBit
  else
    let lastByte := data.get! (data.size - 1)
    if lastByte == 0 then
      .error .missingSentinelBit
    else
      -- Find position of highest set bit in last byte
      let highBit := Id.run do
        let mut hb := 7
        while hb > 0 && (lastByte >>> hb.toUInt8) &&& 1 == 0 do
          hb := hb - 1
        return hb
      -- Total bit length = (data.size - 1) * 8 + highBit
      let length := (data.size - 1) * 8 + highBit
      if h : length ≤ maxCap then
        let dataByteCount := (length + 7) / 8
        let resultData := Id.run do
          let mut rd := ByteArray.mk (Array.replicate dataByteCount 0)
          for i in [:length] do
            let byteIdx := i / 8
            let bitIdx := i % 8
            let srcByte := data.get! byteIdx
            if (srcByte >>> bitIdx.toUInt8) &&& 1 == 1 then
              let dstByte := rd.get! byteIdx
              rd := rd.set! byteIdx (dstByte ||| ((1 : UInt8) <<< bitIdx.toUInt8))
          return rd
        if hsize : resultData.size = (length + 7) / 8 then
          .ok ⟨resultData, length, h, hsize⟩
        else
          .error (.other "bitlist internal: unexpected data size after decode")
      else
        .error (.capacityExceeded maxCap length)

/-- Set a bit at index `i` to the given value. Requires `i < bl.length`. -/
def setBit {maxCap : Nat} (bl : Bitlist maxCap) (i : Nat) (_h : i < bl.length) (val : Bool) : Bitlist maxCap :=
  let byteIdx := i / 8
  let bitIdx := i % 8
  let oldByte := bl.data.get! byteIdx
  let mask := (1 : UInt8) <<< bitIdx.toUInt8
  let newByte := if val then oldByte ||| mask else oldByte &&& mask.complement
  let newData := bl.data.set! byteIdx newByte
  if hsize : newData.size = (bl.length + 7) / 8 then
    ⟨newData, bl.length, bl.hbound, hsize⟩
  else
    bl

/-- Extend bitlist to `n` bits, zero-filling new positions. No-op if `n ≤ bl.length`. -/
def extendToLength {maxCap : Nat} (bl : Bitlist maxCap) (n : Nat) :
    Except SszError (Bitlist maxCap) :=
  if n ≤ bl.length then .ok bl
  else if h : n ≤ maxCap then
    let newByteCount := (n + 7) / 8
    let result := Id.run do
      let mut r := ByteArray.mk (Array.replicate newByteCount 0)
      for i in [:bl.length] do
        let byteIdx := i / 8
        let bitIdx := i % 8
        let srcByte := bl.data.get! byteIdx
        if (srcByte >>> bitIdx.toUInt8) &&& 1 == 1 then
          let dstByte := r.get! byteIdx
          r := r.set! byteIdx (dstByte ||| ((1 : UInt8) <<< bitIdx.toUInt8))
      return r
    if hsize : result.size = (n + 7) / 8 then
      .ok ⟨result, n, h, hsize⟩
    else
      .error (.other "bitlist extendToLength: unexpected size")
  else
    .error (.capacityExceeded maxCap n)

/-- Drop the first `delta` bits, returning a shorter bitlist. -/
def shiftWindow {maxCap : Nat} (bl : Bitlist maxCap) (delta : Nat) : Bitlist maxCap :=
  if delta == 0 then bl
  else if delta ≥ bl.length then empty maxCap
  else
    let newLen := bl.length - delta
    let newByteCount := (newLen + 7) / 8
    let result := Id.run do
      let mut r := ByteArray.mk (Array.replicate newByteCount 0)
      for i in [:newLen] do
        let srcIdx := i + delta
        let srcByteIdx := srcIdx / 8
        let srcBitIdx := srcIdx % 8
        let srcByte := bl.data.get! srcByteIdx
        if (srcByte >>> srcBitIdx.toUInt8) &&& 1 == 1 then
          let dstByteIdx := i / 8
          let dstBitIdx := i % 8
          let dstByte := r.get! dstByteIdx
          r := r.set! dstByteIdx (dstByte ||| ((1 : UInt8) <<< dstBitIdx.toUInt8))
      return r
    if hsize : result.size = (newLen + 7) / 8 then
      have hle : newLen ≤ maxCap := Nat.le_trans (Nat.sub_le bl.length delta) bl.hbound
      ⟨result, newLen, hle, hsize⟩
    else
      empty maxCap

/-- Return an array of indices where bits are set. -/
def toIndices {maxCap : Nat} (bl : Bitlist maxCap) : Array Nat := Id.run do
  let mut result := #[]
  for i in [:bl.length] do
    let byteIdx := i / 8
    let bitIdx := i % 8
    let byte := bl.data.get! byteIdx
    if (byte >>> bitIdx.toUInt8) &&& 1 == 1 then
      result := result.push i
  return result

/-- Extract bits `[start, start+len)` as a new Bitlist.
    Out-of-range source bits are treated as 0. -/
def sliceSegment {maxCap : Nat} (bl : Bitlist maxCap) (start len : Nat)
    {outCap : Nat} (hcap : len ≤ outCap) : Bitlist outCap :=
  let newByteCount := (len + 7) / 8
  let result := Id.run do
    let mut r := ByteArray.mk (Array.replicate newByteCount 0)
    for i in [:len] do
      let srcIdx := start + i
      if srcIdx < bl.length then
        let srcByteIdx := srcIdx / 8
        let srcBitIdx := srcIdx % 8
        let srcByte := bl.data.get! srcByteIdx
        if (srcByte >>> srcBitIdx.toUInt8) &&& 1 == 1 then
          let dstByteIdx := i / 8
          let dstBitIdx := i % 8
          let dstByte := r.get! dstByteIdx
          r := r.set! dstByteIdx (dstByte ||| ((1 : UInt8) <<< dstBitIdx.toUInt8))
    return r
  if hsize : result.size = (len + 7) / 8 then
    ⟨result, len, hcap, hsize⟩
  else
    zeros len hcap

/-- Create a bitlist from an array of Bool values. -/
def fromBools {maxCap : Nat} (vals : Array Bool) (hcap : vals.size ≤ maxCap) : Bitlist maxCap :=
  let len := vals.size
  let byteCount := (len + 7) / 8
  let result := Id.run do
    let mut r := ByteArray.mk (Array.replicate byteCount 0)
    for i in [:len] do
      if vals[i]! then
        let byteIdx := i / 8
        let bitIdx := i % 8
        let byte := r.get! byteIdx
        r := r.set! byteIdx (byte ||| ((1 : UInt8) <<< bitIdx.toUInt8))
    return r
  if hsize : result.size = (len + 7) / 8 then
    ⟨result, len, hcap, hsize⟩
  else
    zeros len hcap

/-- Concatenate two bitlists. -/
def concat {maxCap : Nat} (a b : Bitlist maxCap) : Except SszError (Bitlist maxCap) :=
  let newLen := a.length + b.length
  if h : newLen ≤ maxCap then
    let newByteCount := (newLen + 7) / 8
    let result := Id.run do
      let mut r := ByteArray.mk (Array.replicate newByteCount 0)
      for i in [:a.length] do
        let byteIdx := i / 8
        let bitIdx := i % 8
        let srcByte := a.data.get! byteIdx
        if (srcByte >>> bitIdx.toUInt8) &&& 1 == 1 then
          let dstByte := r.get! i / 8
          r := r.set! (i / 8) (dstByte ||| ((1 : UInt8) <<< (i % 8).toUInt8))
      for j in [:b.length] do
        let dstIdx := a.length + j
        let srcByteIdx := j / 8
        let srcBitIdx := j % 8
        let srcByte := b.data.get! srcByteIdx
        if (srcByte >>> srcBitIdx.toUInt8) &&& 1 == 1 then
          let dstByteIdx := dstIdx / 8
          let dstBitIdx := dstIdx % 8
          let dstByte := r.get! dstByteIdx
          r := r.set! dstByteIdx (dstByte ||| ((1 : UInt8) <<< dstBitIdx.toUInt8))
      return r
    if hsize : result.size = (newLen + 7) / 8 then
      .ok ⟨result, newLen, h, hsize⟩
    else
      .error (.other "bitlist concat: unexpected size")
  else
    .error (.capacityExceeded maxCap newLen)

end Bitlist

-- SSZ instances: Bitlist is variable-size
instance (maxCap : Nat) : SszType (Bitlist maxCap) where
  sszFixedSize := none

instance (maxCap : Nat) : SszEncode (Bitlist maxCap) where
  sszEncode := Bitlist.sszEncodeImpl

instance (maxCap : Nat) : SszDecode (Bitlist maxCap) where
  sszDecode := Bitlist.sszDecodeImpl

instance (maxCap : Nat) : SszHashTreeRoot (Bitlist maxCap) where
  hashTreeRoot bl :=
    let bitChunks := packBits bl.data
    let bitLimit := (maxCap + 255) / 256
    mixInLength (merkleize bitChunks bitLimit) bl.length

end LeanConsensus.SSZ
