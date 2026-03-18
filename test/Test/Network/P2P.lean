/-
  P2P Tests — Configuration and Node Construction
-/

import LeanConsensus.Network.P2P
import LeanConsensus.Network.BeaconAPI

namespace Test.Network.P2P

open LeanConsensus.Network.P2P
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
  IO.println "\n── P2P Sidecar Wrapper ──"
  let mut total := 0
  let mut failures := 0

  -- Test 1: P2PConfig creation
  do
    let apiConfig : BeaconAPIConfig := { host := "127.0.0.1", port := 5052 }
    let p2pConfig : P2PConfig := { beaconAPI := apiConfig }
    let (t, f) ← check "P2PConfig wraps BeaconAPIConfig"
      (p2pConfig.beaconAPI.port == 5052)
    total := total + t; failures := failures + f

  -- Test 2: P2PConfig toString
  do
    let config : P2PConfig := { beaconAPI := { host := "localhost", port := 8080 } }
    let s := toString config
    let (t, f) ← check "P2PConfig toString" (hasSubstr s "P2PConfig")
    total := total + t; failures := failures + f

  -- Test 3: startNode fails on unhealthy sidecar
  do
    let config : P2PConfig := { beaconAPI := { host := "127.0.0.1", port := 19998 } }
    let result ← try
      let _ ← startNode config
      pure false
    catch _ =>
      pure true
    let (t, f) ← check "startNode fails on unhealthy sidecar" result
    total := total + t; failures := failures + f

  -- Test 4: P2PConfig with auth token
  do
    let config : P2PConfig := {
      beaconAPI := {
        host := "127.0.0.1"
        port := 5052
        authToken := some "test-token"
      }
    }
    let (t, f) ← check "P2PConfig with auth token"
      (config.beaconAPI.authToken == some "test-token")
    total := total + t; failures := failures + f

  return (total, failures)

end Test.Network.P2P
