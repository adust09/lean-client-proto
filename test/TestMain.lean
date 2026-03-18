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
import Test.Consensus.StateTransition

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

  -- State Transition tests
  let (t, f) ← Test.Consensus.StateTransition.runTests
  total := total + t; failures := failures + f

  IO.println "═══════════════════════════════════════════"
  if failures == 0 then
    IO.println s!"  All {total} tests passed!"
  else
    IO.println s!"  {failures}/{total} tests FAILED"
    IO.Process.exit 1
