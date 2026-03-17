/-
  leanSig — XMSS Signature FFI Bindings

  Declares opaque handle types and @[extern] functions matching the C ABI
  in ffi/lean_sig.c. All operations are IO since they call into native code
  that manages mutable XMSS key state.

  The C wrappers take owned references (lean_object *), so no @& annotations
  are used for opaque handle arguments.
-/

namespace LeanConsensus.Crypto.LeanSig

-- ════════════════════════════════════════════════════════════════
-- Opaque handle types (standard Lean 4 FFI pattern)
-- ════════════════════════════════════════════════════════════════

/-- Opaque XMSS private key handle. Backed by a C pointer with a finalizer. -/
opaque PrivateKeyPointed : NonemptyType
def PrivateKey : Type := PrivateKeyPointed.type
instance : Nonempty PrivateKey := PrivateKeyPointed.property

/-- Opaque XMSS public key handle. Backed by a C pointer with a finalizer. -/
opaque PublicKeyPointed : NonemptyType
def PublicKey : Type := PublicKeyPointed.type
instance : Nonempty PublicKey := PublicKeyPointed.property

-- ════════════════════════════════════════════════════════════════
-- @[extern] bindings (map to lean_sig.c wrappers)
-- ════════════════════════════════════════════════════════════════

/-- Generate an XMSS keypair for the given epoch range.
    Returns (privateKey, publicKey). -/
@[extern "lean_sig_keygen_wrapper"]
opaque keygen (activationEpoch : UInt32) (numActiveEpochs : UInt32) : IO (PrivateKey × PublicKey)

/-- Sign a 32-byte message with the private key at the given epoch.
    Each epoch can only be signed once (XMSS is stateful). -/
@[extern "lean_sig_sign_wrapper"]
opaque sign (sk : PrivateKey) (epoch : UInt32) (message : ByteArray) : IO ByteArray

/-- Verify a signature against a serialized public key, epoch, and message.
    The public key is passed as raw bytes (from exportPublicKey), not as an opaque handle. -/
@[extern "lean_sig_verify_wrapper"]
opaque verify (pk : ByteArray) (epoch : UInt32) (message : ByteArray) (signature : ByteArray) : IO Bool

/-- Export a public key handle to serialized bytes. -/
@[extern "lean_sig_export_public_key_wrapper"]
opaque exportPublicKey (pk : PublicKey) : IO ByteArray

/-- Serialize a private key to bytes for persistence. -/
@[extern "lean_sig_serialize_private_key_wrapper"]
opaque serializePrivateKey (sk : PrivateKey) : IO ByteArray

/-- Deserialize a private key from bytes. -/
@[extern "lean_sig_deserialize_private_key_wrapper"]
opaque deserializePrivateKey (data : ByteArray) : IO PrivateKey

/-- Get the prepared epoch interval [start, end) for this private key. -/
@[extern "lean_sig_get_prepared_interval_wrapper"]
opaque getPreparedInterval (sk : PrivateKey) : IO (UInt64 × UInt64)

/-- Advance the key's preparation state by one epoch. -/
@[extern "lean_sig_advance_preparation_wrapper"]
opaque advancePreparation (sk : PrivateKey) : IO Unit

end LeanConsensus.Crypto.LeanSig
