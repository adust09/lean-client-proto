# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A post-quantum Ethereum consensus client prototype written in Lean 4 (v4.28.0), targeting the **pq-devnet-3** spec with **3-Slot Finality**. Currently in Phase 1 (SSZ serialization + consensus types), with Phase 3 stubs for fork choice, state transitions, actor framework, networking, and storage.

## Build Commands

```bash
# Build the FFI C library (required before first build)
cd ffi && bash build.sh && cd ..

# Build everything (library + executable + tests + proofs)
lake build

# Build only the main library
lake build LeanConsensus

# Build and run the executable
lake exe lean-consensus

# Run the test suite
lake exe test-runner

# Build proof library (checks theorem stubs compile)
lake build Proofs
```

The FFI layer (`ffi/`) links against OpenSSL (`-lssl -lcrypto`), so `libssl-dev` must be installed.

## Architecture

### SSZ Serialization (`LeanConsensus/SSZ/`)

The SSZ (Simple Serialize) layer is the most developed part. It uses a **typeclass hierarchy**:
- `SszType` — metadata: fixed (`some n`) vs variable (`none`) size
- `SszEncode` — serialization to `ByteArray`
- `SszDecode` — deserialization returning `Except SszError α`
- `SszHashTreeRoot` — Merkle `hash_tree_root` computation

Container encoding uses `SszEncoder`, a **two-pass encoder**: pass 1 computes the fixed region size, pass 2 writes fixed fields inline + 4-byte LE offsets for variable fields, then appends variable data.

Dependent-typed collections enforce invariants at the type level:
- `BytesN n` — fixed-length byte array with proof `data.size = n`
- `SszVector n α` — fixed-length array with proof `elems.size = n`
- `SszList maxCap α` — bounded list with proof `elems.size ≤ maxCap`
- `Bitvector n` / `Bitlist maxCap` — compact bit containers

### Consensus Types (`LeanConsensus/Consensus/`)

- `Constants.lean` — all protocol constants as `Nat` (usable as type-level parameters)
- `Types.lean` — full beacon chain type definitions (`State`, `Block`, `SignedBlock`, `BlockSignatures`, `Validator`, etc.) with auto-derived SSZ instances
- `ForkChoice.lean`, `StateTransition.lean` — consensus logic (slot processing, block validation, LMD-GHOST)

### Proofs (`proofs/`)

- `SSZ/Roundtrip.lean` — SSZ roundtrip correctness theorems (mostly `sorry`-stubbed). Three structural invariant theorems are proven.
- `Consensus/Safety.lean` — placeholder for finality safety theorems

### Tests (`test/`)

Custom test runner (not LSpec despite the comment). Each test module exports `runTests : IO (Nat × Nat)` returning `(total, failures)`. `TestMain.lean` aggregates them.

### FFI (`ffi/`)

C bindings for SHA-256 (via OpenSSL), XMSS signatures, XMSS multisig, and P2P. Only SHA-256 FFI is built and linked; others are header-declared stubs.

## Key Conventions

- `autoImplicit` is **disabled** globally — all variables must be explicitly bound
- Namespace pattern: `LeanConsensus.SSZ`, `LeanConsensus.Consensus`, `LeanConsensus.Actor`
- SSZ instances are written manually (not derived) using the `SszEncoder` pattern for containers
- Constants are `Nat`-valued so they work as type-level bounds for `SszVector`/`SszList`
- No external Lean package dependencies (empty `lake-manifest.json`)
