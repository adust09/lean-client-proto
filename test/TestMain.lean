/-
  Test Runner — LSpec-based test suite

  Runs all SSZ and Consensus tests.
  Execute via: lake exe test-runner
-/

import Test.SSZ.Encode
import Test.SSZ.Decode
import Test.SSZ.Roundtrip
import Test.SSZ.Bitvector
import Test.SSZ.Bitlist
import Test.SSZ.Vector
import Test.SSZ.Merkleization
import Test.Consensus.Types
import Test.Consensus.Signing
import Test.Crypto.LeanSig
import Test.Crypto.KeyState
import Test.Crypto.LeanMultisig
import Test.Actor
import Test.Consensus.StateTransition
import Test.Consensus.ForkChoice
import Test.Network.HttpClient
import Test.Network.BeaconAPI
import Test.Network.P2P
import Test.Consensus.Aggregator
import Test.Actor.WireActors
import Test.Metrics
import Test.Storage
import Test.Genesis
import Test.Config
import Test.LeanSpec.SSZ
import Test.LeanSpec.StateTransition
import Test.LeanSpec.ForkChoice

def main : IO Unit := do
  IO.println "═══════════════════════════════════════════"
  IO.println "  lean-consensus test suite"
  IO.println "═══════════════════════════════════════════"

  let mut failures := 0
  let mut total := 0

  -- SSZ Encode tests
  let (t, f) ← Test.SSZ.Encode.runTests
  total := total + t; failures := failures + f

  -- SSZ Decode tests
  let (t, f) ← Test.SSZ.Decode.runTests
  total := total + t; failures := failures + f

  -- SSZ Roundtrip tests
  let (t, f) ← Test.SSZ.Roundtrip.runTests
  total := total + t; failures := failures + f

  -- SSZ Bitvector tests
  let (t, f) ← Test.SSZ.Bitvector.runTests
  total := total + t; failures := failures + f

  -- SSZ Bitlist tests
  let (t, f) ← Test.SSZ.Bitlist.runTests
  total := total + t; failures := failures + f

  -- SSZ Vector tests
  let (t, f) ← Test.SSZ.Vector.runTests
  total := total + t; failures := failures + f

  -- SSZ Merkleization tests
  let (t, f) ← Test.SSZ.Merkleization.runTests
  total := total + t; failures := failures + f

  -- Consensus Types tests
  let (t, f) ← Test.Consensus.Types.runTests
  total := total + t; failures := failures + f

  -- Consensus Signing tests
  let (t, f) ← Test.Consensus.Signing.runTests
  total := total + t; failures := failures + f

  -- Crypto LeanSig tests
  let (t, f) ← Test.Crypto.LeanSig.runTests
  total := total + t; failures := failures + f

  -- Crypto KeyState tests
  let (t, f) ← Test.Crypto.KeyState.runTests
  total := total + t; failures := failures + f

  -- Crypto LeanMultisig tests
  let (t, f) ← Test.Crypto.LeanMultisig.runTests
  total := total + t; failures := failures + f

  -- Actor framework tests
  let (t, f) ← Test.Actor.runTests
  total := total + t; failures := failures + f

  -- State Transition tests
  let (t, f) ← Test.Consensus.StateTransition.runTests
  total := total + t; failures := failures + f

  -- Fork Choice tests
  let (t, f) ← Test.Consensus.ForkChoice.runTests
  total := total + t; failures := failures + f

  -- HTTP Client tests
  let (t, f) ← Test.Network.HttpClient.runTests
  total := total + t; failures := failures + f

  -- Beacon API tests
  let (t, f) ← Test.Network.BeaconAPI.runTests
  total := total + t; failures := failures + f

  -- P2P tests
  let (t, f) ← Test.Network.P2P.runTests
  total := total + t; failures := failures + f

  -- Aggregator tests
  let (t, f) ← Test.Consensus.Aggregator.runTests
  total := total + t; failures := failures + f

  -- Wire Actor tests
  let (t, f) ← Test.Actor.WireActors.runTests
  total := total + t; failures := failures + f

  -- Metrics tests
  let (t, f) ← Test.Metrics.runTests
  total := total + t; failures := failures + f

  -- Storage tests
  let (t, f) ← Test.Storage.runTests
  total := total + t; failures := failures + f

  -- Genesis tests
  let (t, f) ← Test.Genesis.runTests
  total := total + t; failures := failures + f

  -- Config tests
  let (t, f) ← Test.Config.runTests
  total := total + t; failures := failures + f

  -- LeanSpec SSZ tests
  let (t, f) ← Test.LeanSpec.SSZ.runTests
  total := total + t; failures := failures + f

  -- LeanSpec StateTransition tests
  let (t, f) ← Test.LeanSpec.StateTransition.runTests
  total := total + t; failures := failures + f

  -- LeanSpec ForkChoice tests
  let (t, f) ← Test.LeanSpec.ForkChoice.runTests
  total := total + t; failures := failures + f

  IO.println "═══════════════════════════════════════════"
  if failures == 0 then
    IO.println s!"  All {total} tests passed!"
  else
    IO.println s!"  {failures}/{total} tests FAILED"
    IO.Process.exit 1
