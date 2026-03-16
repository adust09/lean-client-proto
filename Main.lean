/-
  lean-consensus — Entry Point & CLI

  Phase 1: Minimal entry point that verifies the build works.
  Later phases will add CLI argument parsing, genesis loading,
  actor wiring, and the main event loop.
-/

import LeanConsensus

open LeanConsensus.SSZ
open LeanConsensus.Consensus

def main : IO Unit := do
  IO.println "lean-consensus v0.1.0 (Phase 1: SSZ + Types)"
  IO.println s!"  Slot size:           {SECONDS_PER_SLOT}s"
  IO.println s!"  Slots to finality:   {SLOTS_TO_FINALITY}"
  IO.println s!"  Max validators:      {VALIDATOR_REGISTRY_LIMIT}"
  IO.println s!"  Max attestations:    {MAX_ATTESTATIONS}"

  -- Verify basic SSZ roundtrip
  let checkpoint : Checkpoint := {
    slot := 42
    root := Bytes32.zero
  }
  let encoded := SszEncode.sszEncode checkpoint
  match SszDecode.sszDecode encoded with
  | .ok (decoded : Checkpoint) =>
    if decoded == checkpoint then
      IO.println "  SSZ roundtrip:       OK"
    else
      IO.println "  SSZ roundtrip:       MISMATCH"
  | .error e =>
    IO.println s!"  SSZ roundtrip:       ERROR ({e})"
