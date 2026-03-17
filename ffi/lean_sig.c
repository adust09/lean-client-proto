/*
 * lean_sig.c — leanSig Lean4 FFI wrappers
 *
 * Each @[extern "lean_sig_xxx_wrapper"] in Lean4 maps to a function here
 * that converts between lean_object* and the raw C types in lean_sig.h.
 *
 * Pattern (from lean_sha256.c):
 *   - Extract ByteArray data via lean_sarray_cptr / lean_sarray_size
 *   - Allocate result ByteArray via lean_alloc_sarray
 *   - Return lean_io_result_mk_ok(result) or lean_io_result_mk_error(...)
 *   - Opaque handles use lean_alloc_external with a custom finalizer
 */

#include <lean/lean.h>
#include <string.h>
#include "lean_sig.h"

/* ── Opaque handle helpers ────────────────────────────────────── */

static void lean_sig_sk_finalizer(void *ptr) {
    lean_sig_free_private_key(ptr);
}

static void lean_sig_pk_finalizer(void *ptr) {
    lean_sig_free_public_key(ptr);
}

static lean_external_class *g_sk_class = NULL;
static lean_external_class *g_pk_class = NULL;

static lean_external_class *get_sk_class(void) {
    if (g_sk_class == NULL) {
        g_sk_class = lean_register_external_class(lean_sig_sk_finalizer, NULL);
    }
    return g_sk_class;
}

static lean_external_class *get_pk_class(void) {
    if (g_pk_class == NULL) {
        g_pk_class = lean_register_external_class(lean_sig_pk_finalizer, NULL);
    }
    return g_pk_class;
}

static inline void *sk_handle(lean_object *obj) {
    return lean_get_external_data(obj);
}

static inline void *pk_handle(lean_object *obj) {
    return lean_get_external_data(obj);
}

static lean_object *mk_io_error(const char *msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

/* ── Lean4 wrappers ──────────────────────────────────────────── */

/*
 * @[extern "lean_sig_keygen_wrapper"]
 * opaque leanSigKeygen (activationEpoch : UInt32) (numActiveEpochs : UInt32)
 *   : IO (LeanSigSecretKey × LeanSigPublicKey)
 */
LEAN_EXPORT lean_object *lean_sig_keygen_wrapper(
    uint32_t activation_epoch, uint32_t num_active_epochs, lean_object *unused
) {
    void *sk = NULL;
    void *pk = NULL;
    int32_t rc = lean_sig_keygen(activation_epoch, num_active_epochs, &sk, &pk);
    if (rc != LEAN_OK) {
        return mk_io_error("lean_sig_keygen failed");
    }
    lean_object *sk_obj = lean_alloc_external(get_sk_class(), sk);
    lean_object *pk_obj = lean_alloc_external(get_pk_class(), pk);
    lean_object *pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, sk_obj);
    lean_ctor_set(pair, 1, pk_obj);
    return lean_io_result_mk_ok(pair);
}

/*
 * @[extern "lean_sig_sign_wrapper"]
 * opaque leanSigSign (sk : LeanSigSecretKey) (epoch : UInt32)
 *   (message : ByteArray) : IO ByteArray
 */
LEAN_EXPORT lean_object *lean_sig_sign_wrapper(
    lean_object *sk_obj, uint32_t epoch, lean_object *msg_obj, lean_object *unused
) {
    void *sk_ptr = sk_handle(sk_obj);
    const uint8_t *msg = lean_sarray_cptr(msg_obj);
    size_t msg_len = lean_sarray_size(msg_obj);

    if (msg_len != 32) {
        return mk_io_error("lean_sig_sign: message must be exactly 32 bytes");
    }

    /* Allocate a generous buffer for the variable-length signature */
    size_t sig_cap = 1024 * 1024; /* 1 MiB — signatures are large but bounded */
    uint8_t *sig_buf = (uint8_t *)malloc(sig_cap);
    if (!sig_buf) {
        return mk_io_error("lean_sig_sign: allocation failed");
    }

    size_t sig_written = 0;
    int32_t rc = lean_sig_sign(sk_ptr, epoch, msg, msg_len, sig_buf, sig_cap, &sig_written);
    if (rc != LEAN_OK) {
        free(sig_buf);
        return mk_io_error("lean_sig_sign: signing failed");
    }

    lean_object *result = lean_alloc_sarray(1, sig_written, sig_written);
    memcpy(lean_sarray_cptr(result), sig_buf, sig_written);
    free(sig_buf);
    return lean_io_result_mk_ok(result);
}

/*
 * @[extern "lean_sig_verify_wrapper"]
 * opaque leanSigVerify (pk : ByteArray) (epoch : UInt32)
 *   (message : ByteArray) (signature : ByteArray) : IO Bool
 */
LEAN_EXPORT lean_object *lean_sig_verify_wrapper(
    lean_object *pk_obj, uint32_t epoch, lean_object *msg_obj,
    lean_object *sig_obj, lean_object *unused
) {
    const uint8_t *pk = lean_sarray_cptr(pk_obj);
    size_t pk_len = lean_sarray_size(pk_obj);
    const uint8_t *msg = lean_sarray_cptr(msg_obj);
    size_t msg_len = lean_sarray_size(msg_obj);
    const uint8_t *sig = lean_sarray_cptr(sig_obj);
    size_t sig_len = lean_sarray_size(sig_obj);

    int32_t rc = lean_sig_verify(pk, pk_len, epoch, msg, msg_len, sig, sig_len);
    if (rc < 0) {
        return mk_io_error("lean_sig_verify: verification error");
    }
    return lean_io_result_mk_ok(lean_box(rc == LEAN_OK ? 1 : 0));
}

/*
 * @[extern "lean_sig_export_public_key_wrapper"]
 * opaque leanSigExportPublicKey (pk : LeanSigPublicKey) : IO ByteArray
 */
LEAN_EXPORT lean_object *lean_sig_export_public_key_wrapper(
    lean_object *pk_obj, lean_object *unused
) {
    void *pk_ptr = pk_handle(pk_obj);

    /* First call to get the size */
    size_t buf_cap = 1024 * 1024;
    uint8_t *buf = (uint8_t *)malloc(buf_cap);
    if (!buf) {
        return mk_io_error("lean_sig_export_public_key: allocation failed");
    }

    size_t written = 0;
    int32_t rc = lean_sig_export_public_key(pk_ptr, buf, buf_cap, &written);
    if (rc != LEAN_OK) {
        free(buf);
        return mk_io_error("lean_sig_export_public_key: export failed");
    }

    lean_object *result = lean_alloc_sarray(1, written, written);
    memcpy(lean_sarray_cptr(result), buf, written);
    free(buf);
    return lean_io_result_mk_ok(result);
}

/*
 * @[extern "lean_sig_serialize_private_key_wrapper"]
 * opaque leanSigSerializePrivateKey (sk : LeanSigSecretKey) : IO ByteArray
 */
LEAN_EXPORT lean_object *lean_sig_serialize_private_key_wrapper(
    lean_object *sk_obj, lean_object *unused
) {
    void *sk_ptr = sk_handle(sk_obj);

    size_t buf_cap = 64 * 1024 * 1024; /* 64 MiB — secret keys can be very large */
    uint8_t *buf = (uint8_t *)malloc(buf_cap);
    if (!buf) {
        return mk_io_error("lean_sig_serialize_private_key: allocation failed");
    }

    size_t written = 0;
    int32_t rc = lean_sig_serialize_private_key(sk_ptr, buf, buf_cap, &written);
    if (rc != LEAN_OK) {
        free(buf);
        return mk_io_error("lean_sig_serialize_private_key: serialization failed");
    }

    lean_object *result = lean_alloc_sarray(1, written, written);
    memcpy(lean_sarray_cptr(result), buf, written);
    free(buf);
    return lean_io_result_mk_ok(result);
}

/*
 * @[extern "lean_sig_deserialize_private_key_wrapper"]
 * opaque leanSigDeserializePrivateKey (data : ByteArray) : IO LeanSigSecretKey
 */
LEAN_EXPORT lean_object *lean_sig_deserialize_private_key_wrapper(
    lean_object *data_obj, lean_object *unused
) {
    const uint8_t *buf = lean_sarray_cptr(data_obj);
    size_t buf_len = lean_sarray_size(data_obj);

    void *sk = NULL;
    int32_t rc = lean_sig_deserialize_private_key(buf, buf_len, &sk);
    if (rc != LEAN_OK) {
        return mk_io_error("lean_sig_deserialize_private_key: deserialization failed");
    }

    lean_object *sk_obj = lean_alloc_external(get_sk_class(), sk);
    return lean_io_result_mk_ok(sk_obj);
}

/*
 * @[extern "lean_sig_get_prepared_interval_wrapper"]
 * opaque leanSigGetPreparedInterval (sk : LeanSigSecretKey)
 *   : IO (UInt64 × UInt64)
 */
LEAN_EXPORT lean_object *lean_sig_get_prepared_interval_wrapper(
    lean_object *sk_obj, lean_object *unused
) {
    void *sk_ptr = sk_handle(sk_obj);
    uint64_t start = 0, end = 0;

    int32_t rc = lean_sig_get_prepared_interval(sk_ptr, &start, &end);
    if (rc != LEAN_OK) {
        return mk_io_error("lean_sig_get_prepared_interval: failed");
    }

    lean_object *pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, lean_box_uint64(start));
    lean_ctor_set(pair, 1, lean_box_uint64(end));
    return lean_io_result_mk_ok(pair);
}

/*
 * @[extern "lean_sig_advance_preparation_wrapper"]
 * opaque leanSigAdvancePreparation (sk : LeanSigSecretKey) : IO Unit
 */
LEAN_EXPORT lean_object *lean_sig_advance_preparation_wrapper(
    lean_object *sk_obj, lean_object *unused
) {
    void *sk_ptr = sk_handle(sk_obj);

    int32_t rc = lean_sig_advance_preparation(sk_ptr);
    if (rc != LEAN_OK) {
        return mk_io_error("lean_sig_advance_preparation: failed");
    }

    return lean_io_result_mk_ok(lean_box(0));
}
