#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
LEAN_INCLUDE=$(lean --print-prefix)/include
gcc -c -fPIC -I"$LEAN_INCLUDE" lean_sha256.c -o lean_sha256.o
gcc -c -fPIC libc_compat.c -o libc_compat.o
ar rcs liblean_sha256.a lean_sha256.o libc_compat.o
echo "Built ffi/liblean_sha256.a"
