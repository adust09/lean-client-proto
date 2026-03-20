/-
  Storage — File-Based SSZ Persistence with IO.Ref Cache

  Two-tier storage: IO.Ref HashMap hot cache + filesystem persistence.
  Each block/state is stored as a separate SSZ-encoded file named by
  hex-encoded root. A StorageBackend interface allows future drop-in
  replacement (e.g., RocksDB).

  Disk layout:
    <datadir>/blocks/<hex-root>.ssz
    <datadir>/states/<hex-root>.ssz
    <datadir>/store-snapshot.ssz
-/

import Std.Data.HashMap
import LeanConsensus.SSZ.BytesN
import LeanConsensus.SSZ.Encode
import LeanConsensus.SSZ.Decode
import LeanConsensus.Consensus.Types

namespace LeanConsensus.Storage

open LeanConsensus.SSZ
open LeanConsensus.Consensus

-- ════════════════════════════════════════════════════════════════
-- Hex Utilities
-- ════════════════════════════════════════════════════════════════

private def hexDigit (n : UInt8) : Char :=
  if n < 10 then Char.ofNat (48 + n.toNat)  -- '0'..'9'
  else Char.ofNat (97 + n.toNat - 10)        -- 'a'..'f'

/-- Convert a Bytes32 root to a hex string for use as filename. -/
def rootToHex (root : Root) : String := Id.run do
  let mut s := ""
  for i in [:root.data.size] do
    let b := root.data.get! i
    s := s.push (hexDigit (b / 16))
    s := s.push (hexDigit (b % 16))
  return s

private def hexCharToNibble (c : Char) : Option UInt8 :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat).toUInt8
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10).toUInt8
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10).toUInt8
  else none

/-- Convert a hex string back to a Bytes32 root. -/
def hexToRoot (hex : String) : Except String Root :=
  if hex.length != 64 then
    .error s!"hex string must be 64 chars, got {hex.length}"
  else
    let bytes := hex.toList.toArray |> parseHexPairs
    if hsize : bytes.size = 32 then
      .ok ⟨bytes, hsize⟩
    else
      .error s!"unexpected byte count: {bytes.size}"
where
  parseHexPairs (chars : Array Char) : ByteArray := Id.run do
    let mut acc := ByteArray.mk #[]
    let mut i := 0
    while i + 1 < chars.size do
      let c1 := chars.getD i ' '
      let c2 := chars.getD (i + 1) ' '
      match hexCharToNibble c1, hexCharToNibble c2 with
      | some hi, some lo => acc := acc.push (hi * 16 + lo)
      | _, _ => pure ()
      i := i + 2
    return acc

-- ════════════════════════════════════════════════════════════════
-- Configuration
-- ════════════════════════════════════════════════════════════════

/-- Storage configuration. -/
structure StorageConfig where
  dataDir   : System.FilePath
  cacheSize : Nat := 256

-- ════════════════════════════════════════════════════════════════
-- Storage Backend
-- ════════════════════════════════════════════════════════════════

/-- File-based storage backend with in-memory cache. -/
structure StorageBackend where
  config     : StorageConfig
  blockCache : IO.Ref (Std.HashMap Root ByteArray)
  stateCache : IO.Ref (Std.HashMap Root ByteArray)

/-- Ensure a directory exists, creating it if necessary. -/
private def ensureDir (path : System.FilePath) : IO Unit := do
  let doesExist ← path.pathExists
  if !doesExist then
    IO.FS.createDirAll path

/-- Open a storage backend, creating directories as needed. -/
def StorageBackend.open (config : StorageConfig) : IO StorageBackend := do
  ensureDir config.dataDir
  ensureDir (config.dataDir / "blocks")
  ensureDir (config.dataDir / "states")
  let blockCache ← IO.mkRef (∅ : Std.HashMap Root ByteArray)
  let stateCache ← IO.mkRef (∅ : Std.HashMap Root ByteArray)
  return { config, blockCache, stateCache }

-- ════════════════════════════════════════════════════════════════
-- Block Operations
-- ════════════════════════════════════════════════════════════════

/-- File path for a block given its root. -/
private def blockPath (config : StorageConfig) (root : Root) : System.FilePath :=
  config.dataDir / "blocks" / (rootToHex root ++ ".ssz")

/-- Store a block: encode to SSZ, write to cache and file. -/
def StorageBackend.putBlock (sb : StorageBackend) (root : Root)
    (block : Block) : IO Unit := do
  let encoded := SszEncode.sszEncode block
  sb.blockCache.modify (·.insert root encoded)
  IO.FS.writeBinFile (blockPath sb.config root) encoded

/-- Retrieve a block by root. Checks cache first, then disk. -/
def StorageBackend.getBlock (sb : StorageBackend) (root : Root) :
    IO (Option Block) := do
  let cache ← sb.blockCache.get
  let bytes ← match cache.get? root with
    | some data => pure (some data)
    | none =>
      let path := blockPath sb.config root
      let doesExist ← path.pathExists
      if doesExist then
        let data ← IO.FS.readBinFile path
        sb.blockCache.modify (·.insert root data)
        pure (some data)
      else
        pure none
  match bytes with
  | none => return none
  | some data =>
    match SszDecode.sszDecode data with
    | .ok block => return some block
    | .error _ => return none

/-- Check if a block exists (in cache or on disk). -/
def StorageBackend.hasBlock (sb : StorageBackend) (root : Root) : IO Bool := do
  let cache ← sb.blockCache.get
  if cache.contains root then return true
  (blockPath sb.config root).pathExists

-- ════════════════════════════════════════════════════════════════
-- State Operations
-- ════════════════════════════════════════════════════════════════

/-- File path for a state given its root. -/
private def statePath (config : StorageConfig) (root : Root) : System.FilePath :=
  config.dataDir / "states" / (rootToHex root ++ ".ssz")

/-- Store a beacon state: encode to SSZ, write to cache and file. -/
def StorageBackend.putState (sb : StorageBackend) (root : Root)
    (state : State) : IO Unit := do
  let encoded := SszEncode.sszEncode state
  sb.stateCache.modify (·.insert root encoded)
  IO.FS.writeBinFile (statePath sb.config root) encoded

/-- Retrieve a beacon state by root. Checks cache first, then disk. -/
def StorageBackend.getState (sb : StorageBackend) (root : Root) :
    IO (Option State) := do
  let cache ← sb.stateCache.get
  let bytes ← match cache.get? root with
    | some data => pure (some data)
    | none =>
      let path := statePath sb.config root
      let doesExist ← path.pathExists
      if doesExist then
        let data ← IO.FS.readBinFile path
        sb.stateCache.modify (·.insert root data)
        pure (some data)
      else
        pure none
  match bytes with
  | none => return none
  | some data =>
    match SszDecode.sszDecode data with
    | .ok st => return some st
    | .error _ => return none

/-- Check if a state exists (in cache or on disk). -/
def StorageBackend.hasState (sb : StorageBackend) (root : Root) : IO Bool := do
  let cache ← sb.stateCache.get
  if cache.contains root then return true
  (statePath sb.config root).pathExists

-- ════════════════════════════════════════════════════════════════
-- Store Snapshot (fork choice metadata)
-- ════════════════════════════════════════════════════════════════

/-- Path for the store snapshot file. -/
private def snapshotPath (config : StorageConfig) : System.FilePath :=
  config.dataDir / "store-snapshot.ssz"

/-- Persist the fork choice store's checkpoints and current slot.
    Block/state data is already persisted individually; this saves
    the metadata needed to reconstruct the Store on restart. -/
def StorageBackend.saveStoreSnapshot (sb : StorageBackend)
    (store : Store) : IO Unit := do
  -- Serialize checkpoint and slot metadata as a simple binary format:
  -- justified checkpoint (SSZ) ++ finalized checkpoint (SSZ) ++ currentSlot (8 bytes LE)
  let justified := SszEncode.sszEncode store.latestJustified
  let finalized := SszEncode.sszEncode store.latestFinalized
  let slotBytes := Id.run do
    let mut buf := ByteArray.mk #[]
    let slot := store.time
    for i in [:8] do
      buf := buf.push ((slot >>> (i * 8).toUInt64).toUInt8)
    return buf
  let data := justified ++ finalized ++ slotBytes
  IO.FS.writeBinFile (snapshotPath sb.config) data

/-- Load the store snapshot if it exists. Returns the justified/finalized
    checkpoints and current slot. The caller must rebuild the full Store
    by loading blocks and states from disk. -/
def StorageBackend.loadStoreSnapshot (sb : StorageBackend) :
    IO (Option (Checkpoint × Checkpoint × Slot)) := do
  let path := snapshotPath sb.config
  let doesExist ← path.pathExists
  if !doesExist then return none
  let data ← IO.FS.readBinFile path
  -- Checkpoint SSZ size: 8 (slot) + 32 (root) = 40 bytes each
  -- Total expected: 40 + 40 + 8 = 88 bytes
  if data.size < 88 then return none
  let justifiedBytes := data.extract 0 40
  let finalizedBytes := data.extract 40 80
  match SszDecode.sszDecode justifiedBytes, SszDecode.sszDecode finalizedBytes with
  | .ok justified, .ok finalized =>
    let slot : Slot := Id.run do
      let mut s : UInt64 := 0
      for i in [:8] do
        s := s ||| (data.get! (80 + i)).toUInt64 <<< (i * 8).toUInt64
      return s
    return some (justified, finalized, slot)
  | _, _ => return none

end LeanConsensus.Storage
