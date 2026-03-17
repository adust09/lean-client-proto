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
