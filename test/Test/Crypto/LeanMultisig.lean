/-
  LeanMultisig FFI Tests — prover/verifier setup, aggregate, verify
-/

import LeanConsensus.Crypto.LeanMultisig

namespace Test.Crypto.LeanMultisig

open LeanConsensus.Crypto.LeanMultisig

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def checkIO (name : String) (action : IO Bool) : IO (Nat × Nat) := do
  try
    let result ← action
    check name result
  catch e =>
    IO.println s!"  ✗ {name} (exception: {e})"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── Crypto LeanMultisig ──"
  let mut total := 0
  let mut failures := 0

  -- setupProver succeeds
  let (t, f) ← checkIO "setupProver succeeds" do
    let _ctx ← setupProver
    return true
  total := total + t; failures := failures + f

  -- setupVerifier succeeds
  let (t, f) ← checkIO "setupVerifier succeeds" do
    let _ctx ← setupVerifier
    return true
  total := total + t; failures := failures + f

  return (total, failures)

end Test.Crypto.LeanMultisig
