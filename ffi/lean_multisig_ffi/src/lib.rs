//! C FFI wrapper for leanMultisig ZK aggregation library.
//!
//! Exposes the aggregation/verification operations as `extern "C"` functions
//! using opaque pointer handles for prover/verifier contexts.
//!
//! The C ABI functions match `lean_multisig.h`.

use std::ptr;
use std::slice;

use backend::PrimeCharacteristicRing;
use lean_multisig::{
    AggregatedXMSS, F, MESSAGE_LEN_FE, XmssPublicKey, XmssSignature,
    setup_prover, setup_verifier, xmss_aggregate,
    xmss_verify_aggregation,
};

// Error codes matching lean_multisig.h
const LEAN_OK: i32 = 0;
const LEAN_ERR_INVALID_PARAM: i32 = -1;
const LEAN_ERR_BUFFER_TOO_SMALL: i32 = -2;
const LEAN_ERR_COUNT_MISMATCH: i32 = -6;
const _LEAN_ERR_AGGREGATION_FAILED: i32 = -7;
const _LEAN_ERR_SETUP_FAILED: i32 = -9;
const _LEAN_ERR_INTERNAL: i32 = -99;

/// ABI version
const ABI_MAJOR: u32 = 0;
const ABI_MINOR: u32 = 1;

/// Prover context: holds setup state for proof generation.
struct ProverCtx {
    _initialized: bool,
}

/// Verifier context: holds setup state for proof verification.
struct VerifierCtx {
    _initialized: bool,
}

/// Convert a 32-byte message into 9 KoalaBear field elements.
///
/// Splits the 32 bytes into 9 chunks (first 8 are 4 bytes each = 32 bytes,
/// but we only have 32 bytes for 9 elements). We use 3 bytes per element
/// for the first 8 elements (24 bytes) + 8 bytes for the 9th element,
/// OR more simply: interpret each 4-byte chunk as a u32 reduced mod p.
///
/// Actually, looking at the test code: `F::from_usize(i * 3)` — field elements
/// are just small integers. For the C ABI, we pass the message as 9 u32 values
/// packed as 36 bytes (little-endian), matching the field element representation.
fn bytes_to_message(msg: &[u8], msg_len: usize) -> Option<[F; MESSAGE_LEN_FE]> {
    // Expect 36 bytes = 9 × 4-byte LE u32 field elements
    if msg_len != MESSAGE_LEN_FE * 4 {
        return None;
    }
    let mut message = [F::ZERO; MESSAGE_LEN_FE];
    for i in 0..MESSAGE_LEN_FE {
        let offset = i * 4;
        let val = u32::from_le_bytes([
            msg[offset],
            msg[offset + 1],
            msg[offset + 2],
            msg[offset + 3],
        ]);
        message[i] = F::from_u32(val);
    }
    Some(message)
}

/// Deserialize a pubkey from postcard-encoded bytes.
fn deserialize_pubkey(data: &[u8]) -> Option<XmssPublicKey> {
    postcard::from_bytes(data).ok()
}

/// Deserialize a signature from postcard-encoded bytes.
fn deserialize_signature(data: &[u8]) -> Option<XmssSignature> {
    postcard::from_bytes(data).ok()
}

// ── Lean4 FFI exports ──────────────────────────────────────────

#[unsafe(no_mangle)]
pub extern "C" fn lean_multisig_setup_prover(ctx_out: *mut *mut std::ffi::c_void) -> i32 {
    if ctx_out.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }
    setup_prover();
    let ctx = Box::new(ProverCtx { _initialized: true });
    unsafe { *ctx_out = Box::into_raw(ctx).cast() };
    LEAN_OK
}

#[unsafe(no_mangle)]
pub extern "C" fn lean_multisig_setup_verifier(ctx_out: *mut *mut std::ffi::c_void) -> i32 {
    if ctx_out.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }
    setup_verifier();
    let ctx = Box::new(VerifierCtx { _initialized: true });
    unsafe { *ctx_out = Box::into_raw(ctx).cast() };
    LEAN_OK
}

#[unsafe(no_mangle)]
pub extern "C" fn lean_multisig_free_prover(ctx: *mut std::ffi::c_void) {
    if !ctx.is_null() {
        drop(unsafe { Box::from_raw(ctx.cast::<ProverCtx>()) });
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lean_multisig_free_verifier(ctx: *mut std::ffi::c_void) {
    if !ctx.is_null() {
        drop(unsafe { Box::from_raw(ctx.cast::<VerifierCtx>()) });
    }
}

/// Aggregate multiple XMSS signatures into a ZK proof.
///
/// # Safety
/// All pointer arguments must be valid and properly sized.
///
/// `pubkeys` is a flat buffer of concatenated postcard-serialized public keys.
/// `pubkeys_byte_len` is the total byte length.
/// `pubkey_count` is the number of public keys.
///
/// Similarly, `signatures` is concatenated postcard-serialized signatures.
///
/// Actually, for the flat buffer format: each pubkey/sig is length-prefixed
/// with a 4-byte LE u32 size prefix, followed by the serialized data.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_multisig_aggregate(
    _prover_ctx: *mut std::ffi::c_void,
    pubkeys: *const u8,
    pubkeys_byte_len: usize,
    pubkey_count: usize,
    signatures: *const u8,
    signatures_byte_len: usize,
    signature_count: usize,
    message: *const u8,
    message_len: usize,
    proof_out: *mut u8,
    proof_capacity: usize,
    proof_len_out: *mut usize,
) -> i32 {
    if pubkeys.is_null() || signatures.is_null() || message.is_null()
        || proof_out.is_null() || proof_len_out.is_null()
    {
        return LEAN_ERR_INVALID_PARAM;
    }
    if pubkey_count != signature_count {
        return LEAN_ERR_COUNT_MISMATCH;
    }

    let msg_slice = unsafe { slice::from_raw_parts(message, message_len) };
    let msg_fe = match bytes_to_message(msg_slice, message_len) {
        Some(m) => m,
        None => return LEAN_ERR_INVALID_PARAM,
    };

    // Parse length-prefixed pubkeys
    let pk_buf = unsafe { slice::from_raw_parts(pubkeys, pubkeys_byte_len) };
    let parsed_pks = match parse_length_prefixed_items(pk_buf, pubkey_count) {
        Some(items) => items,
        None => return LEAN_ERR_INVALID_PARAM,
    };
    let pks: Vec<XmssPublicKey> = match parsed_pks.iter().map(|data| deserialize_pubkey(data)).collect() {
        Some(v) => v,
        None => return LEAN_ERR_INVALID_PARAM,
    };

    // Parse length-prefixed signatures
    let sig_buf = unsafe { slice::from_raw_parts(signatures, signatures_byte_len) };
    let parsed_sigs = match parse_length_prefixed_items(sig_buf, signature_count) {
        Some(items) => items,
        None => return LEAN_ERR_INVALID_PARAM,
    };
    let sigs: Vec<XmssSignature> = match parsed_sigs.iter().map(|data| deserialize_signature(data)).collect() {
        Some(v) => v,
        None => return LEAN_ERR_INVALID_PARAM,
    };

    // Pair pubkeys and signatures
    let pairs: Vec<(XmssPublicKey, XmssSignature)> = pks.into_iter().zip(sigs).collect();

    // Use slot 0 for aggregation (the slot is encoded in signatures)
    let slot = 0u32;
    let log_inv_rate = 2usize;

    let aggregated = xmss_aggregate(&[], pairs, &msg_fe, slot, log_inv_rate);
    let proof_bytes = aggregated.serialize();

    if proof_bytes.len() > proof_capacity {
        return LEAN_ERR_BUFFER_TOO_SMALL;
    }

    unsafe {
        ptr::copy_nonoverlapping(proof_bytes.as_ptr(), proof_out, proof_bytes.len());
        *proof_len_out = proof_bytes.len();
    }
    LEAN_OK
}

/// Verify an aggregated ZK proof.
///
/// # Safety
/// All pointer arguments must be valid and properly sized.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_multisig_verify(
    _verifier_ctx: *mut std::ffi::c_void,
    proof: *const u8,
    proof_len: usize,
    message: *const u8,
    message_len: usize,
    pubkeys: *const u8,
    _pubkeys_byte_len: usize,
    _pubkey_count: usize,
) -> i32 {
    if proof.is_null() || message.is_null() || pubkeys.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }

    let msg_slice = unsafe { slice::from_raw_parts(message, message_len) };
    let msg_fe = match bytes_to_message(msg_slice, message_len) {
        Some(m) => m,
        None => return LEAN_ERR_INVALID_PARAM,
    };

    let proof_slice = unsafe { slice::from_raw_parts(proof, proof_len) };
    let aggregated = match AggregatedXMSS::deserialize(proof_slice) {
        Some(agg) => agg,
        None => return LEAN_ERR_INVALID_PARAM,
    };

    let slot = 0u32;

    match xmss_verify_aggregation(&aggregated, &msg_fe, slot) {
        Ok(_) => LEAN_OK,
        Err(_) => 1, // LEAN_NOT_VALID
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lean_ffi_abi_version(major: *mut u32, minor: *mut u32) -> i32 {
    if major.is_null() || minor.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }
    unsafe {
        *major = ABI_MAJOR;
        *minor = ABI_MINOR;
    }
    LEAN_OK
}

/// Parse a flat buffer of length-prefixed items.
/// Format: [u32_le_len][data][u32_le_len][data]...
fn parse_length_prefixed_items(buf: &[u8], expected_count: usize) -> Option<Vec<&[u8]>> {
    let mut items = Vec::with_capacity(expected_count);
    let mut offset = 0;
    for _ in 0..expected_count {
        if offset + 4 > buf.len() {
            return None;
        }
        let len = u32::from_le_bytes([
            buf[offset], buf[offset + 1], buf[offset + 2], buf[offset + 3],
        ]) as usize;
        offset += 4;
        if offset + len > buf.len() {
            return None;
        }
        items.push(&buf[offset..offset + len]);
        offset += len;
    }
    Some(items)
}
