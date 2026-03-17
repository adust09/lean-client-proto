//! C FFI wrapper for leanSig signature scheme.
//!
//! Exposes the `SignatureScheme` trait operations as `extern "C"` functions
//! using opaque pointer handles for secret and public keys.
//!
//! Default instantiation: `SIGTargetSumLifetime18W1Off10` (lifetime 2^18,
//! chunk size w=1, 10% target sum offset).

use std::ptr;
use std::slice;

use leansig::serialization::Serializable;
use leansig::signature::generalized_xmss::instantiations_poseidon::lifetime_2_to_the_18::target_sum::SIGTargetSumLifetime18W1Off10;
use leansig::signature::{SignatureScheme, SignatureSchemeSecretKey};

type Scheme = SIGTargetSumLifetime18W1Off10;
type SecretKey = <Scheme as SignatureScheme>::SecretKey;
type PublicKey = <Scheme as SignatureScheme>::PublicKey;
type Signature = <Scheme as SignatureScheme>::Signature;

// Error codes matching lean_sig.h
const LEAN_OK: i32 = 0;
const LEAN_NOT_VALID: i32 = 1;
const LEAN_ERR_INVALID_PARAM: i32 = -1;
const LEAN_ERR_BUFFER_TOO_SMALL: i32 = -2;
const LEAN_ERR_SIGNING_FAILED: i32 = -3;
const LEAN_ERR_INTERNAL: i32 = -99;

/// Returns the serialized public key size.
#[unsafe(no_mangle)]
pub extern "C" fn lean_sig_pubkey_size(out: *mut usize) -> i32 {
    if out.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }
    // Generate a dummy key to get the serialized size.
    // This is expensive but called rarely; cache the result on the caller side.
    let mut rng = rand::rng();
    let (pk, _sk) = Scheme::key_gen(&mut rng, 0, 1);
    let bytes = pk.to_bytes();
    unsafe { *out = bytes.len() };
    LEAN_OK
}

/// Returns the serialized signature size.
///
/// Note: leanSig signatures are variable-length (SSZ variable container).
/// This returns the size for a representative signature at epoch 0.
#[unsafe(no_mangle)]
pub extern "C" fn lean_sig_signature_size(out: *mut usize) -> i32 {
    if out.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }
    let mut rng = rand::rng();
    let (_, sk) = Scheme::key_gen(&mut rng, 0, 1);
    let msg = [0u8; 32];
    match Scheme::sign(&sk, 0, &msg) {
        Ok(sig) => {
            let bytes = sig.to_bytes();
            unsafe { *out = bytes.len() };
            LEAN_OK
        }
        Err(_) => LEAN_ERR_INTERNAL,
    }
}

/// Generates a new key pair.
///
/// # Safety
/// `sk_out` and `pk_out` must be valid, non-null pointers to `*mut c_void`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_keygen(
    activation_epoch: u32,
    num_active_epochs: u32,
    sk_out: *mut *mut std::ffi::c_void,
    pk_out: *mut *mut std::ffi::c_void,
) -> i32 {
    if sk_out.is_null() || pk_out.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }
    if num_active_epochs == 0 {
        return LEAN_ERR_INVALID_PARAM;
    }

    let mut rng = rand::rng();
    let (pk, sk) = Scheme::key_gen(
        &mut rng,
        activation_epoch as usize,
        num_active_epochs as usize,
    );

    let sk_box = Box::new(sk);
    let pk_box = Box::new(pk);

    unsafe {
        *sk_out = Box::into_raw(sk_box).cast();
        *pk_out = Box::into_raw(pk_box).cast();
    }
    LEAN_OK
}

/// Signs a message at a given epoch.
///
/// # Safety
/// - `private_key` must be a valid handle from `lean_sig_keygen`.
/// - `message` must point to exactly 32 bytes.
/// - `signature_out` must point to a buffer of at least `sig_cap` bytes.
/// - `sig_written` must be a valid, non-null pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_sign(
    private_key: *const std::ffi::c_void,
    epoch: u32,
    message: *const u8,
    message_len: usize,
    signature_out: *mut u8,
    sig_cap: usize,
    sig_written: *mut usize,
) -> i32 {
    if private_key.is_null() || message.is_null() || signature_out.is_null() || sig_written.is_null()
    {
        return LEAN_ERR_INVALID_PARAM;
    }
    if message_len != 32 {
        return LEAN_ERR_INVALID_PARAM;
    }

    let sk = unsafe { &*private_key.cast::<SecretKey>() };
    let msg: &[u8; 32] = unsafe { &*message.cast::<[u8; 32]>() };

    match Scheme::sign(sk, epoch, msg) {
        Ok(sig) => {
            let bytes = sig.to_bytes();
            if bytes.len() > sig_cap {
                return LEAN_ERR_BUFFER_TOO_SMALL;
            }
            unsafe {
                ptr::copy_nonoverlapping(bytes.as_ptr(), signature_out, bytes.len());
                *sig_written = bytes.len();
            }
            LEAN_OK
        }
        Err(_) => LEAN_ERR_SIGNING_FAILED,
    }
}

/// Verifies a signature against a serialized public key.
///
/// Returns: 0 = valid, 1 = invalid, < 0 = error.
///
/// # Safety
/// - `public_key` must point to `pk_len` bytes of a serialized public key.
/// - `message` must point to 32 bytes.
/// - `signature` must point to `sig_len` bytes of a serialized signature.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_verify(
    public_key: *const u8,
    pk_len: usize,
    epoch: u32,
    message: *const u8,
    message_len: usize,
    signature: *const u8,
    sig_len: usize,
) -> i32 {
    if public_key.is_null() || message.is_null() || signature.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }
    if message_len != 32 {
        return LEAN_ERR_INVALID_PARAM;
    }

    let pk_bytes = unsafe { slice::from_raw_parts(public_key, pk_len) };
    let sig_bytes = unsafe { slice::from_raw_parts(signature, sig_len) };
    let msg: &[u8; 32] = unsafe { &*message.cast::<[u8; 32]>() };

    let pk = match PublicKey::from_bytes(pk_bytes) {
        Ok(pk) => pk,
        Err(_) => return LEAN_ERR_INVALID_PARAM,
    };
    let sig = match Signature::from_bytes(sig_bytes) {
        Ok(sig) => sig,
        Err(_) => return LEAN_ERR_INVALID_PARAM,
    };

    if Scheme::verify(&pk, epoch, msg, &sig) {
        LEAN_OK
    } else {
        LEAN_NOT_VALID
    }
}

/// Serializes a secret key to bytes.
///
/// # Safety
/// - `private_key` must be a valid handle from `lean_sig_keygen`.
/// - `buf_out` must point to a buffer of at least `buf_cap` bytes.
/// - `bytes_written` must be a valid, non-null pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_serialize_private_key(
    private_key: *const std::ffi::c_void,
    buf_out: *mut u8,
    buf_cap: usize,
    bytes_written: *mut usize,
) -> i32 {
    if private_key.is_null() || buf_out.is_null() || bytes_written.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }

    let sk = unsafe { &*private_key.cast::<SecretKey>() };
    let bytes = sk.to_bytes();
    if bytes.len() > buf_cap {
        return LEAN_ERR_BUFFER_TOO_SMALL;
    }

    unsafe {
        ptr::copy_nonoverlapping(bytes.as_ptr(), buf_out, bytes.len());
        *bytes_written = bytes.len();
    }
    LEAN_OK
}

/// Deserializes a secret key from bytes.
///
/// # Safety
/// - `buf` must point to `buf_len` bytes of a serialized secret key.
/// - `sk_out` must be a valid, non-null pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_deserialize_private_key(
    buf: *const u8,
    buf_len: usize,
    sk_out: *mut *mut std::ffi::c_void,
) -> i32 {
    if buf.is_null() || sk_out.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }

    let bytes = unsafe { slice::from_raw_parts(buf, buf_len) };
    match SecretKey::from_bytes(bytes) {
        Ok(sk) => {
            let sk_box = Box::new(sk);
            unsafe { *sk_out = Box::into_raw(sk_box).cast() };
            LEAN_OK
        }
        Err(_) => LEAN_ERR_INVALID_PARAM,
    }
}

/// Serializes a public key to bytes.
///
/// # Safety
/// - `public_key` must be a valid handle from `lean_sig_keygen`.
/// - `buf_out` must point to a buffer of at least `buf_cap` bytes.
/// - `bytes_written` must be a valid, non-null pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_export_public_key(
    public_key: *const std::ffi::c_void,
    buf_out: *mut u8,
    buf_cap: usize,
    bytes_written: *mut usize,
) -> i32 {
    if public_key.is_null() || buf_out.is_null() || bytes_written.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }

    let pk = unsafe { &*public_key.cast::<PublicKey>() };
    let bytes = pk.to_bytes();
    if bytes.len() > buf_cap {
        return LEAN_ERR_BUFFER_TOO_SMALL;
    }

    unsafe {
        ptr::copy_nonoverlapping(bytes.as_ptr(), buf_out, bytes.len());
        *bytes_written = bytes.len();
    }
    LEAN_OK
}

/// Frees a secret key handle.
///
/// # Safety
/// `private_key` must be a valid handle from `lean_sig_keygen` or
/// `lean_sig_deserialize_private_key`, and must not be used after this call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_free_private_key(private_key: *mut std::ffi::c_void) {
    if !private_key.is_null() {
        drop(unsafe { Box::from_raw(private_key.cast::<SecretKey>()) });
    }
}

/// Frees a public key handle.
///
/// # Safety
/// `public_key` must be a valid handle from `lean_sig_keygen`,
/// and must not be used after this call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_free_public_key(public_key: *mut std::ffi::c_void) {
    if !public_key.is_null() {
        drop(unsafe { Box::from_raw(public_key.cast::<PublicKey>()) });
    }
}

/// Returns the prepared interval of a secret key.
///
/// # Safety
/// - `private_key` must be a valid handle from `lean_sig_keygen`.
/// - `start_out` and `end_out` must be valid, non-null pointers.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_get_prepared_interval(
    private_key: *const std::ffi::c_void,
    start_out: *mut u64,
    end_out: *mut u64,
) -> i32 {
    if private_key.is_null() || start_out.is_null() || end_out.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }

    let sk = unsafe { &*private_key.cast::<SecretKey>() };
    let interval = sk.get_prepared_interval();

    unsafe {
        *start_out = interval.start;
        *end_out = interval.end;
    }
    LEAN_OK
}

/// Advances the prepared interval of a secret key.
///
/// # Safety
/// `private_key` must be a valid handle from `lean_sig_keygen`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lean_sig_advance_preparation(
    private_key: *mut std::ffi::c_void,
) -> i32 {
    if private_key.is_null() {
        return LEAN_ERR_INVALID_PARAM;
    }

    let sk = unsafe { &mut *private_key.cast::<SecretKey>() };
    sk.advance_preparation();
    LEAN_OK
}
