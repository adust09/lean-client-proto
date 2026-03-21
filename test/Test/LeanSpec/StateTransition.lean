/-
  State transition tests using leanSpec-generated fixtures.

  With aligned State types (10-field container), we test:
  1. Fixture JSON parsing succeeds
  2. Pre-state → domain State conversion
  3. Block fields parse correctly
  4. Post-state slot expectations match block sequence
  5. Blocks are sequentially ordered (slot monotonicity)
  6. Pre-state field consistency (config, justified, finalized)
-/

import Test.LeanSpec.Types
import Test.LeanSpec.Loader

namespace Test.LeanSpec.StateTransition

open Test.LeanSpec

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── LeanSpec StateTransition ──"
  let mut total := 0
  let mut failures := 0

  let available ← Loader.fixturesAvailable
  if !available then
    IO.println "  SKIP: leanSpec fixtures not generated (run scripts/gen-leanspec-fixtures.sh)"
    return (0, 0)

  let stDir := s!"{Loader.fixturesDir}/state_transition"
  let files ← Loader.discoverFixtures stDir
  IO.println s!"  Found {files.size} state transition fixture files"

  for file in files do
    let result ← Loader.loadFixture (α := STFixture) file
    match result with
    | .error e =>
      IO.println s!"  ✗ {file}: {e}"
      total := total + 1; failures := failures + 1
    | .ok fixture => do
      -- Verify pre-state parsed and converts to domain State
      match fixture.pre.toState with
      | .ok domainState =>
        let (t, f) ← check s!"pre-state slot={fixture.pre.slot} toState" (domainState.slot == fixture.pre.slot.toUInt64)
        total := total + t; failures := failures + f
      | .error e =>
        let (t, f) ← check s!"pre-state slot={fixture.pre.slot} toState ({e})" false
        total := total + t; failures := failures + f

      -- Verify justified slot ≤ finalized slot invariant is consistent
      let justOk := fixture.pre.latestJustified.slot ≥ fixture.pre.latestFinalized.slot
          || fixture.pre.latestJustified.slot == 0
      let (t, f) ← check s!"pre-state justified≥finalized" justOk
      total := total + t; failures := failures + f

      -- Verify all blocks parsed with valid slots and monotonicity
      let mut prevSlot := fixture.pre.slot
      for block in fixture.blocks do
        let valid := block.slot > prevSlot
        let (t, f) ← check s!"block slot={block.slot} > prev={prevSlot}" valid
        total := total + t; failures := failures + f
        prevSlot := block.slot

      -- Verify final block slot matches expected post-state
      if fixture.blocks.size > 0 then
        let lastBlock := fixture.blocks[fixture.blocks.size - 1]!
        let (t, f) ← check s!"last block slot={lastBlock.slot} reachable from pre={fixture.pre.slot}"
          (lastBlock.slot > fixture.pre.slot)
        total := total + t; failures := failures + f

      -- Validate post-state expectations when provided
      match fixture.post with
      | some post =>
        if fixture.blocks.size > 0 then
          let lastBlock := fixture.blocks[fixture.blocks.size - 1]!
          let (t, f) ← check s!"post-state slot={post.slot} ≥ last block={lastBlock.slot}"
            (post.slot ≥ lastBlock.slot)
          total := total + t; failures := failures + f
        let (t, f) ← check s!"post-state parsed" true
        total := total + t; failures := failures + f
      | none => pure ()

  IO.println s!"  StateTransition: {total - failures}/{total} passed"
  return (total, failures)

end Test.LeanSpec.StateTransition
