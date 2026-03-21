---
title: Integration Guide
last_updated: 2026-03-21
tags:
  - integration
  - devnet
  - setup
---

# Integration Guide

How to run lean-consensus against the pq-devnet-3 testnet.

## Prerequisites

- Lean 4 (v4.28.0) with Lake
- OpenSSL development headers (`libssl-dev`)
- A pq-devnet-3 sidecar node (Lighthouse or Nimbus) running the Beacon API

## Building

```bash
# Build FFI libraries (required before first build)
cd ffi && bash build.sh && cd ..

# Build all targets
lake build
```

## Genesis File

Create a `genesis.json` matching the devnet genesis state:

```json
{
  "config": {
    "genesis_time": 1700000000,
    "fork_version": "0x10000091",
    "validator_count": 4
  },
  "validators": [
    { "attestation_pubkey": "0x...", "proposal_pubkey": "0x..." },
    { "attestation_pubkey": "0x...", "proposal_pubkey": "0x..." },
    { "attestation_pubkey": "0x...", "proposal_pubkey": "0x..." },
    { "attestation_pubkey": "0x...", "proposal_pubkey": "0x..." }
  ]
}
```

## Running

### Quick Start

```bash
# Start with defaults (connects to localhost:5052)
lake exe lean-consensus --genesis genesis.json

# Or use the launcher script
./scripts/run-devnet.sh
```

### Full Configuration

```bash
lake exe lean-consensus \
  --datadir ./data \
  --genesis genesis.json \
  --beacon-api-host 127.0.0.1 \
  --beacon-api-port 5052 \
  --validator-index 0 \
  --key-file ./validator.key \
  --metrics-port 9090 \
  --log-level info
```

### Environment Variables (for run-devnet.sh)

| Variable | Default | Description |
|----------|---------|-------------|
| `DATADIR` | `./lean-consensus-data` | Persistent storage directory |
| `GENESIS` | `./genesis.json` | Genesis file path |
| `BEACON_API_HOST` | `127.0.0.1` | Sidecar Beacon API host |
| `BEACON_API_PORT` | `5052` | Sidecar Beacon API port |
| `VALIDATOR_INDEX` | (none) | Validator index to operate |
| `KEY_FILE` | (none) | XMSS key file |
| `METRICS_PORT` | `9090` | Prometheus metrics port |
| `LOG_LEVEL` | `info` | Log verbosity |

## Verification Checklist

After starting the client against a running devnet:

- [ ] Sidecar connects (no connection errors in logs)
- [ ] Blocks imported (`blocks_imported_total` counter > 0)
- [ ] Attestations produced (`attestations_produced_total` counter > 0)
- [ ] Finalization progresses (`finalized_slot` gauge advances)
- [ ] SIGINT triggers graceful shutdown
- [ ] Restart resumes from store snapshot (no re-sync from genesis)

## Data Directory Layout

```
lean-consensus-data/
  blocks/
    <hex-root>.ssz     # SSZ-encoded Block files
  states/
    <hex-root>.ssz     # SSZ-encoded State files
  store-snapshot.ssz   # Fork choice metadata for restart
```

## Troubleshooting

### Connection refused

Ensure the sidecar Beacon API is running and accessible at the configured host:port.

### Genesis mismatch

The genesis file must match the devnet's actual genesis state. Mismatched validator counts or fork versions will cause block validation failures.

### Key file errors

If using `--key-file`, ensure the file exists or the client will generate a new key. Previous keys cannot be recovered if lost.
