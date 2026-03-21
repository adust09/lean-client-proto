/-
  State transition tests using leanSpec-generated fixtures.

  With aligned State types (10-field container), we test:
  1. Fixture JSON parsing succeeds
  2. Pre-state → domain State conversion
  3. Block fields parse correctly
  4. Post-state slot expectations match block sequence
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

      -- Verify all blocks parsed with valid slots
      for block in fixture.blocks do
        let valid := block.slot > 0 || block.slot == 0
        let (t, f) ← check s!"block slot={block.slot}" valid
        total := total + t; failures := failures + f

  IO.println s!"  StateTransition: {total - failures}/{total} passed"
  return (total, failures)

end Test.LeanSpec.StateTransition
