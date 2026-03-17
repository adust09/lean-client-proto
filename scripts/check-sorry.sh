#!/usr/bin/env bash
set -euo pipefail
# Check for sorry usage outside proofs/ directory
matches=$(grep -rn '\bsorry\b' --include='*.lean' . \
  --exclude-dir=proofs --exclude-dir=.lake --exclude-dir=lake-packages \
  | grep -v '^\./proofs/' || true)
if [ -n "$matches" ]; then
  echo "ERROR: sorry found outside proofs/:"
  echo "$matches"
  exit 1
fi
echo "OK: no sorry found outside proofs/"
