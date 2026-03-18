/-
  Beacon API Tests — Configuration and Client Construction
-/

import LeanConsensus.Network.BeaconAPI

namespace Test.Network.BeaconAPI

open LeanConsensus.Network.BeaconAPI

def hasSubstr (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── Beacon API ──"
  let mut total := 0
  let mut failures := 0

  -- Test 1: Default config values
  do
    let config : BeaconAPIConfig := {}
    let (t, f) ← check "default config host" (config.host == "127.0.0.1")
    total := total + t; failures := failures + f
    let (t, f) ← check "default config port" (config.port == 5052)
    total := total + t; failures := failures + f
    let (t, f) ← check "default config no auth" (config.authToken.isNone)
    total := total + t; failures := failures + f

  -- Test 2: Custom config
  do
    let config : BeaconAPIConfig := {
      host := "10.0.0.1"
      port := 9090
      authToken := some "secret-token"
    }
    let (t, f) ← check "custom config host" (config.host == "10.0.0.1")
    total := total + t; failures := failures + f
    let (t, f) ← check "custom config port" (config.port == 9090)
    total := total + t; failures := failures + f
    let (t, f) ← check "custom config auth" (config.authToken == some "secret-token")
    total := total + t; failures := failures + f

  -- Test 3: Client creation
  do
    let config : BeaconAPIConfig := { host := "localhost", port := 5052 }
    let client := mkClient config
    let (t, f) ← check "client wraps config" (client.config.host == "localhost")
    total := total + t; failures := failures + f

  -- Test 4: Config toString
  do
    let config : BeaconAPIConfig := { host := "127.0.0.1", port := 5052 }
    let s := toString config
    let (t, f) ← check "config toString" (hasSubstr s "127.0.0.1" && hasSubstr s "5052")
    total := total + t; failures := failures + f

  -- Test 5: Health check on non-existent server returns false
  do
    let config : BeaconAPIConfig := { host := "127.0.0.1", port := 19999 }
    let client := mkClient config
    let healthy ← checkHealth client
    let (t, f) ← check "health check on bad port returns false" (!healthy)
    total := total + t; failures := failures + f

  return (total, failures)

end Test.Network.BeaconAPI
