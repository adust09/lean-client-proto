#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
cargo build --release
cp target/release/liblean_multisig_ffi.a ../
echo "Built ffi/liblean_multisig_ffi.a"
