/*
 * lean_sha256.c — SHA-256 FFI wrapper for Lean4
 *
 * Converts between Lean4's lean_object* (ByteArray) and raw C types,
 * then calls OpenSSL's SHA256 function.
 *
 * Build: compiled by Lake via moreLinkArgs, linked with -lssl -lcrypto
 *
 * Lean4 ByteArray layout:
 *   lean_sarray_object with m_data[] containing the bytes.
 *   lean_sarray_size(o)  → number of bytes
 *   lean_sarray_cptr(o)  → pointer to first byte
 */

#include <lean/lean.h>
#include <openssl/sha.h>
#include <string.h>

/*
 * lean_sha256 : @& ByteArray → ByteArray
 *
 * The @& annotation means `b` is borrowed (Lean manages its lifetime).
 * We must allocate and return a new ByteArray for the 32-byte result.
 */
LEAN_EXPORT lean_obj_res lean_sha256(b_lean_obj_arg b) {
    /* Extract input bytes */
    size_t len = lean_sarray_size(b);
    uint8_t *input = lean_sarray_cptr(b);

    /* Allocate output ByteArray (32 bytes for SHA-256) */
    lean_obj_res result = lean_alloc_sarray(1, 32, 32);
    uint8_t *output = lean_sarray_cptr(result);

    /* Compute SHA-256 */
    SHA256(input, len, output);

    return result;
}
