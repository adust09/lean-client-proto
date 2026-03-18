#!/usr/bin/env bash
# run-devnet.sh — Launch lean-consensus against pq-devnet-3 sidecar
set -euo pipefail

DATADIR="${DATADIR:-./lean-consensus-data}"
GENESIS="${GENESIS:-./genesis.json}"
BEACON_API_HOST="${BEACON_API_HOST:-127.0.0.1}"
BEACON_API_PORT="${BEACON_API_PORT:-5052}"
VALIDATOR_INDEX="${VALIDATOR_INDEX:-}"
KEY_FILE="${KEY_FILE:-}"
METRICS_PORT="${METRICS_PORT:-9090}"
LOG_LEVEL="${LOG_LEVEL:-info}"

# Build arguments
ARGS=(
  "--datadir" "$DATADIR"
  "--genesis" "$GENESIS"
  "--beacon-api-host" "$BEACON_API_HOST"
  "--beacon-api-port" "$BEACON_API_PORT"
  "--metrics-port" "$METRICS_PORT"
  "--log-level" "$LOG_LEVEL"
)

if [ -n "$VALIDATOR_INDEX" ]; then
  ARGS+=("--validator-index" "$VALIDATOR_INDEX")
fi

if [ -n "$KEY_FILE" ]; then
  ARGS+=("--key-file" "$KEY_FILE")
fi

echo "=== lean-consensus devnet launcher ==="
echo "Data dir:    $DATADIR"
echo "Genesis:     $GENESIS"
echo "Beacon API:  $BEACON_API_HOST:$BEACON_API_PORT"
echo "Metrics:     :$METRICS_PORT"
echo "======================================="

exec lake exe lean-consensus "${ARGS[@]}"
