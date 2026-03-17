/-
  leanMultisig — ZK Aggregation FFI Bindings

  Declares opaque handle types and @[extern] functions for ZK proof
  aggregation of XMSS signatures. The C wrappers in ffi/lean_multisig.c
  bridge Lean4's object model to the Rust leanMultisig library.

  The prover/verifier contexts must be initialized once before use via
  setupProver/setupVerifier. These perform expensive precomputation
  (bytecode compilation, DFT twiddle precomputation).
-/

namespace LeanConsensus.Crypto.LeanMultisig

-- ════════════════════════════════════════════════════════════════
-- Opaque handle types (standard Lean 4 FFI pattern)
-- ════════════════════════════════════════════════════════════════

/-- Opaque prover context handle. Holds compiled aggregation bytecode
    and precomputed DFT twiddles. -/
opaque ProverContextPointed : NonemptyType
def ProverContext : Type := ProverContextPointed.type
instance : Nonempty ProverContext := ProverContextPointed.property

/-- Opaque verifier context handle. Holds compiled aggregation bytecode. -/
opaque VerifierContextPointed : NonemptyType
def VerifierContext : Type := VerifierContextPointed.type
instance : Nonempty VerifierContext := VerifierContextPointed.property

-- ════════════════════════════════════════════════════════════════
-- @[extern] bindings (map to lean_multisig.c wrappers)
-- ════════════════════════════════════════════════════════════════

/-- Initialize the prover context. Must be called once before `aggregate`.
    This is expensive (compiles aggregation bytecode, precomputes twiddles). -/
@[extern "lean_multisig_setup_prover_wrapper"]
opaque setupProver : IO ProverContext

/-- Initialize the verifier context. Must be called once before `verify`.
    Not needed if `setupProver` was already called. -/
@[extern "lean_multisig_setup_verifier_wrapper"]
opaque setupVerifier : IO VerifierContext

/-- Aggregate multiple XMSS signatures into a single ZK proof.

    - `pubkeys`: array of serialized public keys (one per signer)
    - `signatures`: array of serialized signatures (one per signer)
    - `message`: the message that was signed (36 bytes = 9 field elements × 4 bytes LE)

    Returns the serialized aggregated proof (postcard + lz4 compressed). -/
@[extern "lean_multisig_aggregate_wrapper"]
opaque aggregate (ctx : ProverContext) (pubkeys : Array ByteArray)
    (signatures : Array ByteArray) (message : ByteArray) : IO ByteArray

/-- Verify an aggregated ZK proof.

    - `proof`: serialized aggregated proof (from `aggregate`)
    - `message`: the original message (36 bytes)
    - `pubkeys`: array of serialized public keys of all signers

    Returns `true` if the proof is valid, `false` otherwise. -/
@[extern "lean_multisig_verify_wrapper"]
opaque verify (ctx : VerifierContext) (proof : ByteArray)
    (message : ByteArray) (pubkeys : Array ByteArray) : IO Bool

end LeanConsensus.Crypto.LeanMultisig
