/*
 * lean_sig.h — leanSig C ABI (epoch-based signing)
 *
 * Wraps the leanSig signature library for Lean4 FFI.
 * Backed by the lean-sig-ffi Rust crate (ffi/lean_sig_ffi/).
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

/* Key generation (epoch-based) */
int32_t lean_sig_keygen(
    uint32_t activation_epoch,
    uint32_t num_active_epochs,
    void **private_key_out,
    void **public_key_out
);

/* Signing (epoch-based: each epoch may only be used once) */
int32_t lean_sig_sign(
    void *private_key,
    uint32_t epoch,
    const uint8_t *message, size_t message_len,
    uint8_t *signature_out, size_t signature_cap,
    size_t *sig_written_out
);

/* Verification (predicate: 0=valid, 1=invalid, <0=error) */
int32_t lean_sig_verify(
    const uint8_t *public_key, size_t public_key_len,
    uint32_t epoch,
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

/* Epoch preparation (sliding window management) */
int32_t lean_sig_get_prepared_interval(
    const void *private_key,
    uint64_t *start_out,
    uint64_t *end_out
);

int32_t lean_sig_advance_preparation(void *private_key);

#ifdef __cplusplus
}
#endif

#endif /* LEAN_SIG_H */
