/*
 * lean_sig.h — leanSig C ABI shim (Phase 2 stub)
 *
 * Wraps the leanSig XMSS signature library for Lean4 FFI.
 * See dev/06-ffi-abi-proposal.md for the full ABI specification.
 */

#ifndef LEAN_SIG_H
#define LEAN_SIG_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Error codes */
#define LEAN_OK                     0
#define LEAN_NOT_VALID              1
#define LEAN_ERR_INVALID_PARAM     -1
#define LEAN_ERR_BUFFER_TOO_SMALL  -2
#define LEAN_ERR_SIGNING_FAILED    -3
#define LEAN_ERR_KEY_EXHAUSTED     -4
#define LEAN_ERR_INTERNAL          -99

/* Size queries */
int32_t lean_sig_pubkey_size(size_t *out);
int32_t lean_sig_signature_size(size_t *out);

/* Key generation */
int32_t lean_sig_keygen(
    uint32_t tree_height,
    void **private_key_out,
    void **public_key_out
);

/* Signing (stateful: leaf_index managed externally) */
int32_t lean_sig_sign(
    void *private_key,
    const uint8_t *message, size_t message_len,
    uint32_t leaf_index,
    uint8_t *signature_out, size_t signature_len,
    size_t *sig_written_out
);

/* Verification (predicate: 0=valid, 1=invalid, <0=error) */
int32_t lean_sig_verify(
    const uint8_t *public_key, size_t public_key_len,
    const uint8_t *message, size_t message_len,
    const uint8_t *signature, size_t signature_len
);

/* Key lifecycle */
void lean_sig_free_private_key(void *private_key);
void lean_sig_free_public_key(void *public_key);

/* Key serialization */
int32_t lean_sig_serialize_private_key(
    const void *private_key,
    uint8_t *buf_out, size_t buf_capacity,
    size_t *bytes_written_out
);

int32_t lean_sig_deserialize_private_key(
    const uint8_t *buf, size_t buf_len,
    void **private_key_out
);

int32_t lean_sig_export_public_key(
    const void *public_key,
    uint8_t *buf_out, size_t buf_capacity,
    size_t *bytes_written_out
);

#ifdef __cplusplus
}
#endif

#endif /* LEAN_SIG_H */
