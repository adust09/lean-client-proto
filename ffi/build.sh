#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

# Build SHA-256 FFI (C → OpenSSL)
LEAN_INCLUDE=$(lean --print-prefix)/include
gcc -c -fPIC -I"$LEAN_INCLUDE" lean_sha256.c -o lean_sha256.o
gcc -c -fPIC libc_compat.c -o libc_compat.o
ar rcs liblean_sha256.a lean_sha256.o libc_compat.o
echo "Built ffi/liblean_sha256.a"

# Build leanSig FFI (Rust → C static library)
if [ -d lean_sig_ffi ]; then
  (cd lean_sig_ffi && bash build.sh)
fi

# Build leanSig Lean4 wrapper (C → Lean4 runtime)
if [ -f liblean_sig_ffi.a ]; then
  gcc -c -fPIC -I"$LEAN_INCLUDE" lean_sig.c -o lean_sig.o
  ar rcs liblean_sig.a lean_sig.o
  echo "Built ffi/liblean_sig.a"
fi

# Build leanMultisig FFI (Rust → C static library)
if [ -d lean_multisig_ffi ]; then
  (cd lean_multisig_ffi && bash build.sh)
fi

# Build leanMultisig Lean4 wrapper (C → Lean4 runtime)
if [ -f liblean_multisig_ffi.a ]; then
  gcc -c -fPIC -I"$LEAN_INCLUDE" lean_multisig.c -o lean_multisig.o
  ar rcs liblean_multisig.a lean_multisig.o
  echo "Built ffi/liblean_multisig.a"
fi
