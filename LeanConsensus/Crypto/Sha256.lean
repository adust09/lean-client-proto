/-
  SHA-256 via FFI

  Wraps a C function `lean_sha256` that calls OpenSSL's SHA-256.
  The C wrapper (in ffi/lean_sha256.c) converts between Lean4's
  boxed `lean_object*` (ByteArray) and raw `uint8_t*` pointers.

  The `@& ByteArray` annotation passes the ByteArray by reference
  (borrowed) to avoid unnecessary reference count operations.
-/

namespace LeanConsensus.Crypto

/-- SHA-256 hash via FFI to OpenSSL. Takes a ByteArray, returns 32-byte hash.
    The `@&` annotation indicates borrowed reference (no refcount bump). -/
@[extern "lean_sha256"]
opaque sha256 : @& ByteArray → ByteArray

/-- SHA-256 of the concatenation of two 32-byte chunks.
    This is the core operation in Merkle tree construction. -/
def sha256Pair (left right : ByteArray) : ByteArray :=
  sha256 (left ++ right)

end LeanConsensus.Crypto
