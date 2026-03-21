/-
  Fork choice tests using leanSpec-generated fixtures.

  With aligned State types, we test:
  1. Fixture JSON parsing (anchor state, anchor block, steps)
  2. Anchor state → domain State conversion
  3. Block steps parse correctly
  4. Step structure validation
-/

import Test.LeanSpec.Types
import Test.LeanSpec.Loader

namespace Test.LeanSpec.ForkChoice

open Test.LeanSpec

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

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
      -- Verify anchor state parsed and converts to domain State
      match fixture.anchorState.toState with
      | .ok domainState =>
        let (t, f) ← check s!"anchor slot={fixture.anchorState.slot} toState" (domainState.slot == fixture.anchorState.slot.toUInt64)
        total := total + t; failures := failures + f
      | .error e =>
        let (t, f) ← check s!"anchor slot={fixture.anchorState.slot} toState ({e})" false
        total := total + t; failures := failures + f

      -- Verify all steps parsed
      for step in fixture.steps do
        let valid := step.stepType == "block" || step.stepType == "tick" || step.stepType == "attestation"
        let (t, f) ← check s!"step type={step.stepType}" valid
        total := total + t; failures := failures + f

  IO.println s!"  ForkChoice: {total - failures}/{total} passed"
  return (total, failures)

end Test.LeanSpec.ForkChoice
