/-
  KeyState Tests — creation, signing, epoch monotonicity, persistence
-/

import LeanConsensus.Crypto.KeyState
import LeanConsensus.Crypto.LeanSig

namespace Test.Crypto.KeyState

open LeanConsensus.Crypto

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

/-- A dummy 32-byte message for signing. -/
private def testMsg : ByteArray := ByteArray.mk (Array.replicate 32 0xAA)

/-- Create a temp file path for key persistence tests. -/
private def tempKeyPath (suffix : String) : System.FilePath :=
  s!"/tmp/lean_keystate_test_{suffix}.bin"

def runTests : IO (Nat × Nat) := do
  IO.println "\n── Crypto KeyState ──"
  let mut total := 0
  let mut failures := 0

  -- Create and sign epoch 0, then verify
  let (t, f) ← checkIO "create → sign epoch 0 → verify" do
    let ks ← KeyState.create 0 10 (tempKeyPath "create_sign")
    let pkBytes ← KeyState.getExportedPublicKey ks
    let sig ← KeyState.signAndAdvance ks 0 testMsg
    LeanSig.verify pkBytes 0 testMsg sig
  total := total + t; failures := failures + f

  -- Sign same epoch twice should fail
  let (t, f) ← checkIO "sign same epoch twice → error" do
    let ks ← KeyState.create 0 10 (tempKeyPath "double_sign")
    let _ ← KeyState.signAndAdvance ks 0 testMsg
    try
      let _ ← KeyState.signAndAdvance ks 0 testMsg
      return false  -- should have thrown
    catch _ =>
      return true
  total := total + t; failures := failures + f

  -- Sign epochs 0, 1, 2 sequentially
  let (t, f) ← checkIO "sign epochs 0, 1, 2 sequentially" do
    let ks ← KeyState.create 0 10 (tempKeyPath "sequential")
    let pkBytes ← KeyState.getExportedPublicKey ks
    let sig0 ← KeyState.signAndAdvance ks 0 testMsg
    let sig1 ← KeyState.signAndAdvance ks 1 testMsg
    let sig2 ← KeyState.signAndAdvance ks 2 testMsg
    let v0 ← LeanSig.verify pkBytes 0 testMsg sig0
    let v1 ← LeanSig.verify pkBytes 1 testMsg sig1
    let v2 ← LeanSig.verify pkBytes 2 testMsg sig2
    return v0 && v1 && v2
  total := total + t; failures := failures + f

  -- Sign epoch 1 then epoch 0 (backwards) should fail
  let (t, f) ← checkIO "sign epoch 1 then epoch 0 → error" do
    let ks ← KeyState.create 0 10 (tempKeyPath "backwards")
    let _ ← KeyState.signAndAdvance ks 1 testMsg
    try
      let _ ← KeyState.signAndAdvance ks 0 testMsg
      return false
    catch _ =>
      return true
  total := total + t; failures := failures + f

  -- Persist and reload: signing continues from correct epoch
  let (t, f) ← checkIO "persist → load → sign continues" do
    let path := tempKeyPath "persist_load"
    let ks ← KeyState.create 0 10 path
    let pkBytes ← KeyState.getExportedPublicKey ks
    let _ ← KeyState.signAndAdvance ks 0 testMsg
    let _ ← KeyState.signAndAdvance ks 1 testMsg
    -- Load from disk
    let ks2 ← KeyState.load path
    -- Epoch 1 should be rejected (already signed)
    try
      let _ ← KeyState.signAndAdvance ks2 1 testMsg
      return false
    catch _ =>
      -- Epoch 2 should succeed
      let sig2 ← KeyState.signAndAdvance ks2 2 testMsg
      LeanSig.verify pkBytes 2 testMsg sig2
  total := total + t; failures := failures + f

  -- getLastEpoch returns correct value
  let (t, f) ← checkIO "getLastEpoch tracks signing" do
    let ks ← KeyState.create 0 10 (tempKeyPath "last_epoch")
    let e0 ← KeyState.getLastEpoch ks
    let _ ← KeyState.signAndAdvance ks 3 testMsg
    let e1 ← KeyState.getLastEpoch ks
    return e0.isNone && e1 == some 3
  total := total + t; failures := failures + f

  return (total, failures)

end Test.Crypto.KeyState
