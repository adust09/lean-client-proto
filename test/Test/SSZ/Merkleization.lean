/-
  SSZ Merkleization Tests

  Note: Full hash_tree_root tests require the SHA-256 FFI (C library).
  These tests verify the structural correctness of pack, splitIntoChunks,
  nextPowerOfTwo, and the overall flow.
-/

import LeanConsensus.SSZ

namespace Test.SSZ.Merkleization

open LeanConsensus.SSZ

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── SSZ Merkleization ──"
  let mut total := 0
  let mut failures := 0

  -- nextPowerOfTwo
  let (t, f) ← check "nextPowerOfTwo 0 = 1" (nextPowerOfTwo 0 == 1)
  total := total + t; failures := failures + f
  let (t, f) ← check "nextPowerOfTwo 1 = 1" (nextPowerOfTwo 1 == 1)
  total := total + t; failures := failures + f
  let (t, f) ← check "nextPowerOfTwo 2 = 2" (nextPowerOfTwo 2 == 2)
  total := total + t; failures := failures + f
  let (t, f) ← check "nextPowerOfTwo 3 = 4" (nextPowerOfTwo 3 == 4)
  total := total + t; failures := failures + f
  let (t, f) ← check "nextPowerOfTwo 5 = 8" (nextPowerOfTwo 5 == 8)
  total := total + t; failures := failures + f

  -- padToChunks
  let oneByteData := ByteArray.mk #[0x42]
  let padded := padToChunks oneByteData
  let (t, f) ← check "padToChunks pads 1 byte to 32" (padded.size == 32)
  total := total + t; failures := failures + f
  let (t, f) ← check "padToChunks preserves first byte" (padded.get! 0 == 0x42)
  total := total + t; failures := failures + f
  let (t, f) ← check "padToChunks zeros rest" (padded.get! 1 == 0x00 && padded.get! 31 == 0x00)
  total := total + t; failures := failures + f

  -- Already aligned data
  let aligned := ByteArray.mk (Array.replicate 32 0xAB)
  let padded := padToChunks aligned
  let (t, f) ← check "padToChunks no-op for 32 bytes" (padded.size == 32)
  total := total + t; failures := failures + f

  -- splitIntoChunks
  let data64 := ByteArray.mk (Array.replicate 64 0xFF)
  let chunks := splitIntoChunks data64
  let (t, f) ← check "splitIntoChunks 64 bytes → 2 chunks" (chunks.size == 2)
  total := total + t; failures := failures + f
  let (t, f) ← check "splitIntoChunks each chunk is 32 bytes" (chunks[0]!.size == 32 && chunks[1]!.size == 32)
  total := total + t; failures := failures + f

  -- pack empty
  let emptyChunks := pack ByteArray.empty
  let (t, f) ← check "pack empty → 1 zero chunk" (emptyChunks.size == 1 && emptyChunks[0]! == zeroChunk)
  total := total + t; failures := failures + f

  -- zeroChunk is 32 bytes of zeros
  let (t, f) ← check "zeroChunk size" (zeroChunk.size == 32)
  total := total + t; failures := failures + f

  -- Merkleize with single chunk returns the chunk itself (when SHA-256 FFI is linked)
  -- For now, test structure only
  let (t, f) ← check "merkleize 1 chunk with limit 1 returns chunk"
    (merkleize #[zeroChunk] 1 == zeroChunk)
  total := total + t; failures := failures + f

  return (total, failures)

end Test.SSZ.Merkleization
