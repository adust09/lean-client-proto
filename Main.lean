/-
  lean-consensus — Main Entry Point

  Phase 5: Full client startup with CLI parsing, genesis loading,
  storage initialization, metrics, actor system, and signal handling.
-/

import LeanConsensus
import LeanConsensus.Config
import LeanConsensus.Genesis
import LeanConsensus.Storage
import LeanConsensus.Metrics
import LeanConsensus.Signal

open LeanConsensus.Config
open LeanConsensus.Genesis
open LeanConsensus.Storage
open LeanConsensus.Metrics
open LeanConsensus.Signal
open LeanConsensus.Actor.System
open LeanConsensus.Consensus
open LeanConsensus.Crypto.KeyState
open LeanConsensus.SSZ

def main (args : List String) : IO Unit := do
  -- 1. Parse CLI arguments
  let config ← match parseArgs args with
    | .ok c => pure c
    | .error "help" => printHelp; return
    | .error e => IO.eprintln s!"Error: {e}"; printHelp; IO.Process.exit 1

  IO.println "lean-consensus v0.5.0 (Phase 5: Infrastructure)"
  IO.println s!"  Data directory: {config.dataDir}"
  IO.println s!"  Genesis file:   {config.genesisFile}"
  IO.println s!"  Beacon API:     {config.beaconApiHost}:{config.beaconApiPort}"
  IO.println s!"  Log level:      {config.logLevel}"

  -- 2. Install signal handler
  installSignalHandler

  -- 3. Load genesis
  let genesisFileData ← loadGenesisFile config.genesisFile
  let genesisState ← match buildGenesisState genesisFileData with
    | .ok s => pure s
    | .error e => IO.eprintln s!"Genesis error: {e}"; IO.Process.exit 1
  let genesisBlock := buildGenesisBlock
  IO.println s!"  Validators:     {genesisState.validators.elems.size}"

  -- 4. Open storage
  let storageConfig : StorageConfig := { dataDir := config.dataDir }
  let storage ← StorageBackend.open storageConfig

  -- 5. Initialize metrics
  let metricsRegistry ← MetricsRegistry.new
  let beaconMetrics ← BeaconMetrics.register metricsRegistry

  -- 6. Load or create key state
  let keyState ← match config.keyFile with
    | some path =>
      let doesExist ← path.pathExists
      if doesExist then
        load path
      else
        let ks ← generate path
        IO.println s!"  Generated new key: {path}"
        pure ks
    | none =>
      -- Use an ephemeral key state for non-validator mode
      ephemeral

  -- 7. Build actor system config and start
  let actorConfig := toActorSystemConfig config genesisState genesisBlock keyState
  let actorConfigWithInfra := { actorConfig with
    storage := some storage
    metrics := some beaconMetrics
  }

  IO.println "Starting actor system..."
  let system ← startActorSystem actorConfigWithInfra
  IO.println "Actor system running. Press Ctrl+C to stop."

  -- 8. Poll loop: check for shutdown signal
  let mut running := true
  while running do
    IO.sleep 500
    let shouldStop ← isShutdownRequested
    if shouldStop then
      running := false

  -- 9. Graceful shutdown
  IO.println "\nShutting down..."
  shutdownActorSystem system

  -- Save store snapshot before exit
  -- (The store is internal to the blockchain actor; in a full impl we'd
  --  extract it. For now, save a minimal snapshot.)
  IO.println "Shutdown complete."
