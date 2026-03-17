/-
  LeanSig FFI Tests — XMSS keygen, sign, verify, serialization
-/

import LeanConsensus.Crypto.LeanSig

namespace Test.Crypto.LeanSig

open LeanConsensus.Crypto.LeanSig

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
private def testMessage : ByteArray :=
  ByteArray.mk (Array.replicate 32 0x42)

/-- A different 32-byte message for tamper tests. -/
private def tamperedMessage : ByteArray :=
  ByteArray.mk (Array.replicate 32 0x43)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── Crypto LeanSig ──"
  let mut total := 0
  let mut failures := 0

  -- keygen succeeds
  let (t, f) ← checkIO "keygen succeeds" do
    let (_sk, _pk) ← keygen 0 10
    return true
  total := total + t; failures := failures + f

  -- exportPublicKey returns non-empty bytes
  let (t, f) ← checkIO "exportPublicKey returns bytes" do
    let (_sk, pk) ← keygen 0 10
    let pkBytes ← exportPublicKey pk
    return pkBytes.size > 0
  total := total + t; failures := failures + f

  -- sign produces a signature
  let (t, f) ← checkIO "sign produces signature" do
    let (sk, _pk) ← keygen 0 10
    let sig ← sign sk 0 testMessage
    return sig.size > 0
  total := total + t; failures := failures + f

  -- verify valid signature returns true
  let (t, f) ← checkIO "verify valid signature" do
    let (sk, pk) ← keygen 0 10
    let pkBytes ← exportPublicKey pk
    let sig ← sign sk 0 testMessage
    verify pkBytes 0 testMessage sig
  total := total + t; failures := failures + f

  -- verify tampered message returns false
  let (t, f) ← checkIO "verify tampered message → false" do
    let (sk, pk) ← keygen 0 10
    let pkBytes ← exportPublicKey pk
    let sig ← sign sk 0 testMessage
    let result ← verify pkBytes 0 tamperedMessage sig
    return !result
  total := total + t; failures := failures + f

  -- serializePrivateKey / deserializePrivateKey roundtrip
  let (t, f) ← checkIO "private key serialize/deserialize roundtrip" do
    let (sk, pk) ← keygen 0 10
    let pkBytes ← exportPublicKey pk
    let skData ← serializePrivateKey sk
    let sk2 ← deserializePrivateKey skData
    -- Sign with deserialized key at epoch 0 and verify
    let sig ← sign sk2 0 testMessage
    verify pkBytes 0 testMessage sig
  total := total + t; failures := failures + f

  -- getPreparedInterval returns valid range
  let (t, f) ← checkIO "getPreparedInterval returns valid range" do
    let (sk, _pk) ← keygen 0 10
    let (start, stop) ← getPreparedInterval sk
    return start ≤ stop
  total := total + t; failures := failures + f

  -- advancePreparation succeeds
  let (t, f) ← checkIO "advancePreparation succeeds" do
    let (sk, _pk) ← keygen 0 10
    advancePreparation sk
    return true
  total := total + t; failures := failures + f

  return (total, failures)

end Test.Crypto.LeanSig
