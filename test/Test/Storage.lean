/-
  Tests for Storage — put/get roundtrip, cache, file persistence
-/

import LeanConsensus.Storage
import LeanConsensus.Consensus.Types
import LeanConsensus.SSZ

open LeanConsensus.Storage
open LeanConsensus.Consensus
open LeanConsensus.SSZ

namespace Test.Storage

private def check (name : String) (cond : Bool) : IO (Nat × Nat) := do
  if cond then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

/-- Create a minimal test block with the given slot. -/
private def mkTestBlock (slot : UInt64) : BeaconBlock :=
  let emptyAtts : SszList MAX_ATTESTATIONS SignedAggregatedAttestation :=
    ⟨#[], Nat.zero_le _⟩
  { slot := slot
    proposerIndex := 0
    parentRoot := BytesN.zero 32
    stateRoot := BytesN.zero 32
    body := { attestations := emptyAtts } }

/-- Create a deterministic root for testing (slot-based). -/
private def mkRoot (n : UInt8) : Root :=
  let data := ByteArray.mk (#[n] ++ Array.replicate 31 0)
  if h : data.size = 32 then ⟨data, h⟩ else BytesN.zero 32

def runTests : IO (Nat × Nat) := do
  IO.println "── Storage tests ──"
  let mut total := 0
  let mut failures := 0

  -- Hex conversion roundtrip
  let root := mkRoot 0xAB
  let hex := rootToHex root
  let (t, f) ← check "rootToHex produces 64-char hex string" (hex.length == 64)
  total := total + t; failures := failures + f

  match hexToRoot hex with
  | .ok root' =>
    let (t, f) ← check "hexToRoot roundtrip" (root == root')
    total := total + t; failures := failures + f
  | .error e =>
    let (t, f) ← check s!"hexToRoot roundtrip (error: {e})" false
    total := total + t; failures := failures + f

  -- Invalid hex
  let (t, f) ← check "hexToRoot rejects short string"
    (match hexToRoot "abcd" with | .error _ => true | .ok _ => false)
  total := total + t; failures := failures + f

  -- Storage backend operations (using temp directory)
  let tmpDir := System.FilePath.mk "/tmp/lean-consensus-test-storage"
  -- Clean up any previous test run
  try IO.FS.removeDirAll tmpDir catch | _ => pure ()
  let config : StorageConfig := { dataDir := tmpDir }
  let sb ← StorageBackend.open config

  -- Put and get block
  let block := mkTestBlock 42
  let root1 := mkRoot 1
  sb.putBlock root1 block
  let retrieved ← sb.getBlock root1
  let (t, f) ← check "block put/get roundtrip" (match retrieved with
    | some b => b.slot == 42
    | none => false)
  total := total + t; failures := failures + f

  -- hasBlock
  let has ← sb.hasBlock root1
  let (t, f) ← check "hasBlock returns true for stored block" has
  total := total + t; failures := failures + f

  let hasMissing ← sb.hasBlock (mkRoot 99)
  let (t, f) ← check "hasBlock returns false for missing block" (!hasMissing)
  total := total + t; failures := failures + f

  -- File persistence: open a new backend on same dir, verify data persists
  let sb2 ← StorageBackend.open config
  let retrieved2 ← sb2.getBlock root1
  let (t, f) ← check "block persists across backend instances" (match retrieved2 with
    | some b => b.slot == 42
    | none => false)
  total := total + t; failures := failures + f

  -- Store snapshot roundtrip
  let checkpoint : Checkpoint := { slot := 10, root := mkRoot 10 }
  let store : Store := {
    justifiedCheckpoint := checkpoint
    finalizedCheckpoint := { slot := 5, root := mkRoot 5 }
    blocks := ∅
    blockStates := ∅
    latestMessages := ∅
    currentSlot := 100
  }
  sb.saveStoreSnapshot store
  let loaded ← sb.loadStoreSnapshot
  let (t, f) ← check "store snapshot roundtrip" (match loaded with
    | some (justified, finalized, slot) =>
      justified.slot == 10 && finalized.slot == 5 && slot == 100
    | none => false)
  total := total + t; failures := failures + f

  -- Cleanup
  try IO.FS.removeDirAll tmpDir catch | _ => pure ()

  return (total, failures)

end Test.Storage
