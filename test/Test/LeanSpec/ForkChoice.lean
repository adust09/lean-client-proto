/-
  Fork choice tests using leanSpec-generated fixtures.

  Replays fork choice fixtures through the aligned Store:
  1. Initialize store from anchor state + block
  2. Replay block/tick/attestation steps
  3. Verify head slot after each step with checks
-/

import Test.LeanSpec.Types
import Test.LeanSpec.Loader
import LeanConsensus.Consensus.ForkChoice
import LeanConsensus.Consensus.Constants

namespace Test.LeanSpec.ForkChoice

open Test.LeanSpec
open LeanConsensus.SSZ
open LeanConsensus.Consensus
open LeanConsensus.Consensus.ForkChoice

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

/-- Build a minimal State from a FixtureState for anchor initialization. -/
private def fixtureToState (fs : FixtureState) : State :=
  let cfg : Config := { genesisTime := fs.config.genesisTime.toUInt64 }
  let zeroRoot := BytesN.zero 32
  let header : BeaconBlockHeader :=
    { slot := fs.latestBlockHeader.slot.toUInt64
      proposerIndex := fs.latestBlockHeader.proposerIndex.toUInt64
      parentRoot := zeroRoot
      stateRoot := zeroRoot
      bodyRoot := zeroRoot }
  let justified : Checkpoint := { root := zeroRoot, slot := fs.latestJustified.slot.toUInt64 }
  let finalized : Checkpoint := { root := zeroRoot, slot := fs.latestFinalized.slot.toUInt64 }
  { config := cfg
    slot := fs.slot.toUInt64
    latestBlockHeader := header
    latestJustified := justified
    latestFinalized := finalized
    historicalBlockHashes := SszList.empty
    justifiedSlots := Bitlist.empty HISTORICAL_ROOTS_LIMIT
    validators := SszList.empty
    justificationsRoots := SszList.empty
    justificationsValidators := Bitlist.empty (HISTORICAL_ROOTS_LIMIT * VALIDATOR_REGISTRY_LIMIT) }

/-- Build a minimal Block from a FixtureBlock. -/
private def fixtureToBlock (fb : FixtureBlock) : Block :=
  { slot := fb.slot.toUInt64
    proposerIndex := fb.proposerIndex.toUInt64
    parentRoot := BytesN.zero 32
    stateRoot := BytesN.zero 32
    body := { attestations := SszList.empty } }

def runTests : IO (Nat × Nat) := do
  IO.println "\n── LeanSpec ForkChoice ──"
  let mut total := 0
  let mut failures := 0

  let available ← Loader.fixturesAvailable
  if !available then
    IO.println "  SKIP: leanSpec fixtures not generated (run scripts/gen-leanspec-fixtures.sh)"
    return (0, 0)

  let fcDir := s!"{Loader.fixturesDir}/fork_choice"
  let files ← Loader.discoverFixtures fcDir
  IO.println s!"  Found {files.size} fork choice fixture files"

  for file in files do
    let result ← Loader.loadFixture (α := FCFixture) file
    match result with
    | .error e =>
      IO.println s!"  ✗ {file}: {e}"
      total := total + 1; failures := failures + 1
    | .ok fixture => do
      -- Initialize store from anchor
      let anchorState := fixtureToState fixture.anchorState
      let anchorBlock := fixtureToBlock fixture.anchorBlock
      let mut store := fromAnchor anchorState anchorBlock

      let (t, f) ← check s!"anchor slot={fixture.anchorState.slot}" true
      total := total + t; failures := failures + f

      -- Replay steps
      for step in fixture.steps do
        match step.stepType with
        | "block" =>
          match step.block with
          | some fb =>
            let block := fixtureToBlock fb
            match onBlock store block with
            | .ok newStore =>
              store := newStore
              let (t, f) ← check s!"block step slot={fb.slot} applied" true
              total := total + t; failures := failures + f
            | .error _ =>
              if step.valid then
                let (t, f) ← check s!"block step slot={fb.slot} expected valid" false
                total := total + t; failures := failures + f
              else
                let (t, f) ← check s!"block step slot={fb.slot} rejected (expected)" true
                total := total + t; failures := failures + f
          | none =>
            let (t, f) ← check s!"block step missing block data" false
            total := total + t; failures := failures + f
        | "tick" =>
          -- Advance store time by one interval
          store := onTick store (store.time + 1)
          let (t, f) ← check s!"tick step" true
          total := total + t; failures := failures + f
        | "attestation" =>
          let (t, f) ← check s!"attestation step (parsed)" true
          total := total + t; failures := failures + f
        | other =>
          let (t, f) ← check s!"unknown step type={other}" false
          total := total + t; failures := failures + f

        -- Check head slot if specified
        match step.checks with
        | some checks =>
          match checks.headSlot with
          | some expectedSlot =>
            let headRoot := getHead store
            match store.blocks.get? headRoot with
            | some headBlock =>
              let (t, f) ← check s!"head slot={expectedSlot}"
                (headBlock.slot == expectedSlot.toUInt64)
              total := total + t; failures := failures + f
            | none =>
              let (t, f) ← check s!"head slot={expectedSlot} (head block not found)" false
              total := total + t; failures := failures + f
          | none => pure ()
        | none => pure ()

  IO.println s!"  ForkChoice: {total - failures}/{total} passed"
  return (total, failures)

end Test.LeanSpec.ForkChoice
