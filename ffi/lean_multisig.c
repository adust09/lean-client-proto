/*
 * lean_multisig.c — leanMultisig Lean4 FFI wrapper
 *
 * Each @[extern "lean_multisig_xxx_wrapper"] in Lean4 maps to a function here
 * that converts between lean_object* and the raw C types in lean_multisig.h.
 *
 * Pattern follows lean_sig.c: extract lean_object* → call C ABI → wrap result.
 */

#include <lean/lean.h>
#include <stdlib.h>
#include <string.h>
#include "lean_multisig.h"

/* ── Opaque handle helpers ────────────────────────────────────── */

static void lean_multisig_prover_finalizer(void *ptr) {
    lean_multisig_free_prover(ptr);
}

static void lean_multisig_verifier_finalizer(void *ptr) {
    lean_multisig_free_verifier(ptr);
}

static lean_external_class *g_prover_class = NULL;
static lean_external_class *g_verifier_class = NULL;

static lean_external_class *get_prover_class(void) {
    if (g_prover_class == NULL) {
        g_prover_class = lean_register_external_class(lean_multisig_prover_finalizer, NULL);
    }
    return g_prover_class;
}

static lean_external_class *get_verifier_class(void) {
    if (g_verifier_class == NULL) {
        g_verifier_class = lean_register_external_class(lean_multisig_verifier_finalizer, NULL);
    }
    return g_verifier_class;
}

static lean_object *mk_io_error(const char *msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

/* ── Lean4 wrappers ──────────────────────────────────────────── */

/*
 * @[extern "lean_multisig_setup_prover_wrapper"]
 * opaque setupProver : IO ProverContext
 */
LEAN_EXPORT lean_object *lean_multisig_setup_prover_wrapper(lean_object *unused) {
    void *ctx = NULL;
    int32_t rc = lean_multisig_setup_prover(&ctx);
    if (rc != LEAN_OK) {
        return mk_io_error("lean_multisig_setup_prover failed");
    }
    lean_object *ctx_obj = lean_alloc_external(get_prover_class(), ctx);
    return lean_io_result_mk_ok(ctx_obj);
}

/*
 * @[extern "lean_multisig_setup_verifier_wrapper"]
 * opaque setupVerifier : IO VerifierContext
 */
LEAN_EXPORT lean_object *lean_multisig_setup_verifier_wrapper(lean_object *unused) {
    void *ctx = NULL;
    int32_t rc = lean_multisig_setup_verifier(&ctx);
    if (rc != LEAN_OK) {
        return mk_io_error("lean_multisig_setup_verifier failed");
    }
    lean_object *ctx_obj = lean_alloc_external(get_verifier_class(), ctx);
    return lean_io_result_mk_ok(ctx_obj);
}

/*
 * Flatten a Lean Array ByteArray into a contiguous length-prefixed buffer.
 *
 * Format: [u32_le_len][data][u32_le_len][data]...
 *
 * Returns a malloc'd buffer and sets *out_len. Caller must free.
 * Returns NULL on failure.
 */
static uint8_t *flatten_byte_array_array(lean_object *arr, size_t *out_len, size_t *out_count) {
    size_t count = lean_array_size(arr);
    *out_count = count;

    /* First pass: compute total size */
    size_t total = 0;
    for (size_t i = 0; i < count; i++) {
        lean_object *elem = lean_array_get_core(arr, i);
        size_t elem_size = lean_sarray_size(elem);
        total += 4 + elem_size; /* 4-byte length prefix + data */
    }

    uint8_t *buf = (uint8_t *)malloc(total);
    if (!buf) {
        *out_len = 0;
        return NULL;
    }

    /* Second pass: copy data */
    size_t offset = 0;
    for (size_t i = 0; i < count; i++) {
        lean_object *elem = lean_array_get_core(arr, i);
        size_t elem_size = lean_sarray_size(elem);
        uint32_t len32 = (uint32_t)elem_size;

        /* Write length prefix (LE) */
        buf[offset + 0] = (uint8_t)(len32 & 0xFF);
        buf[offset + 1] = (uint8_t)((len32 >> 8) & 0xFF);
        buf[offset + 2] = (uint8_t)((len32 >> 16) & 0xFF);
        buf[offset + 3] = (uint8_t)((len32 >> 24) & 0xFF);
        offset += 4;

        /* Write data */
        memcpy(buf + offset, lean_sarray_cptr(elem), elem_size);
        offset += elem_size;
    }

    *out_len = total;
    return buf;
}

/*
 * @[extern "lean_multisig_aggregate_wrapper"]
 * opaque aggregate (ctx : ProverContext) (pubkeys : Array ByteArray)
 *     (signatures : Array ByteArray) (message : ByteArray) : IO ByteArray
 */
LEAN_EXPORT lean_object *lean_multisig_aggregate_wrapper(
    lean_object *ctx_obj, lean_object *pubkeys_arr, lean_object *sigs_arr,
    lean_object *msg_obj, lean_object *unused
) {
    void *ctx = lean_get_external_data(ctx_obj);

    /* Flatten pubkeys */
    size_t pk_buf_len = 0, pk_count = 0;
    uint8_t *pk_buf = flatten_byte_array_array(pubkeys_arr, &pk_buf_len, &pk_count);
    if (!pk_buf && pk_count > 0) {
        return mk_io_error("lean_multisig_aggregate: pubkey allocation failed");
    }

    /* Flatten signatures */
    size_t sig_buf_len = 0, sig_count = 0;
    uint8_t *sig_buf = flatten_byte_array_array(sigs_arr, &sig_buf_len, &sig_count);
    if (!sig_buf && sig_count > 0) {
        free(pk_buf);
        return mk_io_error("lean_multisig_aggregate: signature allocation failed");
    }

    const uint8_t *msg = lean_sarray_cptr(msg_obj);
    size_t msg_len = lean_sarray_size(msg_obj);

    /* Allocate output buffer (proofs can be large) */
    size_t proof_cap = 16 * 1024 * 1024; /* 16 MiB */
    uint8_t *proof_buf = (uint8_t *)malloc(proof_cap);
    if (!proof_buf) {
        free(pk_buf);
        free(sig_buf);
        return mk_io_error("lean_multisig_aggregate: proof allocation failed");
    }

    size_t proof_len = 0;
    int32_t rc = lean_multisig_aggregate(
        ctx, pk_buf, pk_buf_len, pk_count,
        sig_buf, sig_buf_len, sig_count,
        msg, msg_len,
        proof_buf, proof_cap, &proof_len
    );

    free(pk_buf);
    free(sig_buf);

    if (rc != LEAN_OK) {
        free(proof_buf);
        return mk_io_error("lean_multisig_aggregate: aggregation failed");
    }

    lean_object *result = lean_alloc_sarray(1, proof_len, proof_len);
    memcpy(lean_sarray_cptr(result), proof_buf, proof_len);
    free(proof_buf);
    return lean_io_result_mk_ok(result);
}

/*
 * @[extern "lean_multisig_verify_wrapper"]
 * opaque verify (ctx : VerifierContext) (proof : ByteArray)
 *     (message : ByteArray) (pubkeys : Array ByteArray) : IO Bool
 */
LEAN_EXPORT lean_object *lean_multisig_verify_wrapper(
    lean_object *ctx_obj, lean_object *proof_obj, lean_object *msg_obj,
    lean_object *pubkeys_arr, lean_object *unused
) {
    void *ctx = lean_get_external_data(ctx_obj);

    const uint8_t *proof = lean_sarray_cptr(proof_obj);
    size_t proof_len = lean_sarray_size(proof_obj);
    const uint8_t *msg = lean_sarray_cptr(msg_obj);
    size_t msg_len = lean_sarray_size(msg_obj);

    /* Flatten pubkeys */
    size_t pk_buf_len = 0, pk_count = 0;
    uint8_t *pk_buf = flatten_byte_array_array(pubkeys_arr, &pk_buf_len, &pk_count);
    if (!pk_buf && pk_count > 0) {
        return mk_io_error("lean_multisig_verify: pubkey allocation failed");
    }

    int32_t rc = lean_multisig_verify(
        ctx, proof, proof_len, msg, msg_len,
        pk_buf, pk_buf_len, pk_count
    );

    free(pk_buf);

    if (rc < 0) {
        return mk_io_error("lean_multisig_verify: verification error");
    }

    return lean_io_result_mk_ok(lean_box(rc == LEAN_OK ? 1 : 0));
}
