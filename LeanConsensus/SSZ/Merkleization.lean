/-
  SSZ Merkleization

  Implements the SSZ hash_tree_root algorithm:
  1. pack:         Serialize values and pad to 32-byte chunks
  2. merkleize:    Build a binary Merkle tree over chunks using SHA-256
  3. mixInLength:  Mix in the element count for variable-size types

  The Merkle tree is padded to the next power of 2 with zero-chunks.
  For variable-size types, the tree depth is determined by the *maximum*
  capacity, not the actual element count — this ensures consistent
  root computation regardless of current list length.
-/

import LeanConsensus.SSZ.Error
import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.BytesN
import LeanConsensus.SSZ.Encode
import LeanConsensus.Crypto.Sha256

namespace LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- Constants
-- ════════════════════════════════════════════════════════════════

/-- Bytes per Merkle tree chunk (SHA-256 output). -/
def bytesPerChunk : Nat := 32

-- ════════════════════════════════════════════════════════════════
-- Zero hashes (precomputed Merkle tree of zeros)
-- ════════════════════════════════════════════════════════════════

/-- Compute the next power of 2 ≥ n. Returns 1 for n = 0. -/
def nextPowerOfTwo (n : Nat) : Nat := Id.run do
  if n ≤ 1 then return 1
  let mut p := 1
  while p < n do
    p := p * 2
  return p

/-- A zero chunk (32 zero bytes). -/
def zeroChunk : ByteArray := ByteArray.mk (Array.replicate 32 0)

/-- Precompute zero hashes up to depth `maxDepth`.
    zeroHashes[0] = zero chunk
    zeroHashes[i] = sha256(zeroHashes[i-1] ++ zeroHashes[i-1]) -/
def computeZeroHashes (maxDepth : Nat) : Array ByteArray := Id.run do
  let mut hashes : Array ByteArray := #[zeroChunk]
  for _ in [:maxDepth] do
    let prev := hashes.back!
    hashes := hashes.push (Crypto.sha256 (prev ++ prev))
  return hashes

/-- Cached zero hashes up to depth 64 (sufficient for any SSZ type). -/
def zeroHashes : Array ByteArray := computeZeroHashes 64

-- ════════════════════════════════════════════════════════════════
-- Pack: serialize values into 32-byte aligned chunks
-- ════════════════════════════════════════════════════════════════

/-- Pad a ByteArray to a multiple of 32 bytes with trailing zeros. -/
def padToChunks (data : ByteArray) : ByteArray :=
  let remainder := data.size % bytesPerChunk
  if remainder == 0 then data
  else
    let padding := bytesPerChunk - remainder
    data ++ ByteArray.mk (Array.replicate padding 0)

/-- Split a padded ByteArray into 32-byte chunks. -/
def splitIntoChunks (data : ByteArray) : Array ByteArray := Id.run do
  let padded := padToChunks data
  let numChunks := padded.size / bytesPerChunk
  let mut chunks : Array ByteArray := #[]
  for i in [:numChunks] do
    chunks := chunks.push (padded.extract (i * bytesPerChunk) ((i + 1) * bytesPerChunk))
  return chunks

/-- Pack serialized data into chunks. For basic types, this is just
    the serialized value padded to 32 bytes. -/
def pack (serialized : ByteArray) : Array ByteArray :=
  if serialized.size == 0 then #[zeroChunk]
  else splitIntoChunks serialized

/-- Pack bits (for Bitvector/Bitlist) into chunks. -/
def packBits (bitData : ByteArray) : Array ByteArray :=
  if bitData.size == 0 then #[zeroChunk]
  else splitIntoChunks bitData

-- ════════════════════════════════════════════════════════════════
-- Merkleize: build the Merkle tree
-- ════════════════════════════════════════════════════════════════

/-- Merkleize an array of chunks with a given limit (determines tree depth).
    Pads to `nextPowerOfTwo(limit)` chunks using precomputed zero hashes. -/
def merkleize (chunks : Array ByteArray) (limit : Nat) : ByteArray := Id.run do
  let targetLeaves := nextPowerOfTwo limit
  -- Pad chunks to targetLeaves using zero chunks
  let mut padded := chunks
  while padded.size < targetLeaves do
    padded := padded.push zeroChunk
  -- Binary hash tree: repeatedly hash pairs
  let mut layer := padded
  while layer.size > 1 do
    let mut nextLayer : Array ByteArray := #[]
    let pairs := layer.size / 2
    for i in [:pairs] do
      let left := layer[2 * i]!
      let right := layer[2 * i + 1]!
      nextLayer := nextLayer.push (Crypto.sha256 (left ++ right))
    layer := nextLayer
  if layer.isEmpty then return zeroChunk
  else return layer[0]!

-- ════════════════════════════════════════════════════════════════
-- Mix in length (for variable-size types)
-- ════════════════════════════════════════════════════════════════

/-- Mix the element count into the Merkle root.
    Used for Lists and Bitlists to include length in the hash. -/
def mixInLength (root : ByteArray) (length : Nat) : ByteArray :=
  let lengthLE := encodeUInt64 length.toUInt64
  -- Pad length to 32 bytes
  let lengthChunk := lengthLE ++ ByteArray.mk (Array.replicate 24 0)
  Crypto.sha256 (root ++ lengthChunk)

-- ════════════════════════════════════════════════════════════════
-- Number of chunks needed for a type's max capacity
-- ════════════════════════════════════════════════════════════════

/-- Compute the chunk count limit for a list/vector of fixed-size elements. -/
def chunkCountFixed (maxItems : Nat) (elemSize : Nat) : Nat :=
  (maxItems * elemSize + bytesPerChunk - 1) / bytesPerChunk

/-- Compute the chunk count limit for a list/vector of variable-size elements.
    Each element contributes one chunk (its own hash_tree_root). -/
def chunkCountVariable (maxItems : Nat) : Nat := maxItems

-- ════════════════════════════════════════════════════════════════
-- SszHashTreeRoot instances for primitive types
-- ════════════════════════════════════════════════════════════════

instance : SszHashTreeRoot UInt8 where
  hashTreeRoot v := padToChunks (encodeUInt8 v)

instance : SszHashTreeRoot UInt16 where
  hashTreeRoot v := padToChunks (encodeUInt16 v)

instance : SszHashTreeRoot UInt32 where
  hashTreeRoot v := padToChunks (encodeUInt32 v)

instance : SszHashTreeRoot UInt64 where
  hashTreeRoot v := padToChunks (encodeUInt64 v)

instance : SszHashTreeRoot Bool where
  hashTreeRoot v := padToChunks (encodeBool v)

/-- BytesN hash_tree_root: merkleize the packed bytes. -/
instance (n : Nat) : SszHashTreeRoot (BytesN n) where
  hashTreeRoot b := merkleize (pack b.data) ((n + 31) / 32)

end LeanConsensus.SSZ
