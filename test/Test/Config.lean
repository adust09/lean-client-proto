/-
  Tests for Config — CLI argument parsing
-/

import LeanConsensus.Config

open LeanConsensus.Config

namespace Test.Config

private def check (name : String) (cond : Bool) : IO (Nat × Nat) := do
  if cond then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "── Config tests ──"
  let mut total := 0
  let mut failures := 0

  -- Default config (no args)
  match parseArgs [] with
  | .ok config =>
    let (t, f) ← check "empty args yields defaults"
      (config.beaconApiPort == 5052 && config.metricsPort == 9090 &&
       config.logLevel == "info")
    total := total + t; failures := failures + f
  | .error e =>
    let (t, f) ← check s!"empty args (error: {e})" false
    total := total + t; failures := failures + f

  -- Parse all flags
  match parseArgs ["--datadir", "/tmp/data", "--genesis", "/tmp/g.json",
                    "--beacon-api-host", "0.0.0.0", "--beacon-api-port", "9596",
                    "--validator-index", "42", "--metrics-port", "8080",
                    "--log-level", "debug"] with
  | .ok config =>
    let (t, f) ← check "datadir parsed"
      (config.dataDir == System.FilePath.mk "/tmp/data")
    total := total + t; failures := failures + f

    let (t, f) ← check "genesis file parsed"
      (config.genesisFile == System.FilePath.mk "/tmp/g.json")
    total := total + t; failures := failures + f

    let (t, f) ← check "beacon-api-host parsed" (config.beaconApiHost == "0.0.0.0")
    total := total + t; failures := failures + f

    let (t, f) ← check "beacon-api-port parsed" (config.beaconApiPort == 9596)
    total := total + t; failures := failures + f

    let (t, f) ← check "validator-index parsed" (config.validatorIndex == some 42)
    total := total + t; failures := failures + f

    let (t, f) ← check "metrics-port parsed" (config.metricsPort == 8080)
    total := total + t; failures := failures + f

    let (t, f) ← check "log-level parsed" (config.logLevel == "debug")
    total := total + t; failures := failures + f
  | .error e =>
    for _ in [:7] do
      let (t, f) ← check s!"parse all flags (error: {e})" false
      total := total + t; failures := failures + f

  -- Unknown flag
  let (t, f) ← check "unknown flag returns error"
    (match parseArgs ["--unknown"] with | .error _ => true | .ok _ => false)
  total := total + t; failures := failures + f

  -- Invalid port
  let (t, f) ← check "invalid port returns error"
    (match parseArgs ["--beacon-api-port", "abc"] with | .error _ => true | .ok _ => false)
  total := total + t; failures := failures + f

  -- Invalid log level
  let (t, f) ← check "invalid log level returns error"
    (match parseArgs ["--log-level", "verbose"] with | .error _ => true | .ok _ => false)
  total := total + t; failures := failures + f

  -- Help flag
  let (t, f) ← check "help flag returns error 'help'"
    (match parseArgs ["--help"] with | .error "help" => true | _ => false)
  total := total + t; failures := failures + f

  return (total, failures)

end Test.Config
