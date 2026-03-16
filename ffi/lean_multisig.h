/*
 * lean_multisig.h — leanMultisig C ABI shim (Phase 2 stub)
 *
 * Wraps the leanMultisig ZK aggregation library for Lean4 FFI.
 */

#ifndef LEAN_MULTISIG_H
#define LEAN_MULTISIG_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Error codes (shared with lean_sig.h) */
#ifndef LEAN_OK
#define LEAN_OK                         0
#define LEAN_NOT_VALID                  1
#define LEAN_ERR_INVALID_PARAM         -1
#define LEAN_ERR_BUFFER_TOO_SMALL      -2
#define LEAN_ERR_COUNT_MISMATCH        -6
#define LEAN_ERR_AGGREGATION_FAILED    -7
#define LEAN_ERR_SETUP_FAILED          -9
#define LEAN_ERR_INTERNAL              -99
#endif

/* Context setup */
int32_t lean_multisig_setup_prover(void **prover_ctx_out);
int32_t lean_multisig_setup_verifier(void **verifier_ctx_out);

/* Context teardown */
void lean_multisig_free_prover(void *prover_ctx);
void lean_multisig_free_verifier(void *verifier_ctx);

/* Aggregation */
int32_t lean_multisig_aggregate(
    void *prover_ctx,
    const uint8_t *pubkeys, size_t pubkeys_byte_len, size_t pubkey_count,
    const uint8_t *signatures, size_t signatures_byte_len, size_t signature_count,
    const uint8_t *message, size_t message_len,
    uint8_t *proof_out, size_t proof_capacity,
    size_t *proof_len_out
);

/* Verification (predicate: 0=valid, 1=invalid, <0=error) */
int32_t lean_multisig_verify(
    void *verifier_ctx,
    const uint8_t *proof, size_t proof_len,
    const uint8_t *message, size_t message_len,
    const uint8_t *pubkeys, size_t pubkeys_byte_len, size_t pubkey_count
);

/* Versioning */
int32_t lean_ffi_abi_version(uint32_t *major, uint32_t *minor);

#ifdef __cplusplus
}
#endif

#endif /* LEAN_MULTISIG_H */
