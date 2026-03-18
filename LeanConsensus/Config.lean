/-
  Config — CLI Argument Parsing

  Manual argument parsing for the lean-consensus client.
  Avoids external dependencies; walks a List String recursively.
-/

import LeanConsensus.Actor.System
import LeanConsensus.Genesis
import LeanConsensus.Consensus.Types
import LeanConsensus.Crypto.KeyState
import LeanConsensus.Network.P2P

namespace LeanConsensus.Config

open LeanConsensus.Actor.System
open LeanConsensus.Actor.ValidatorActor
open LeanConsensus.Genesis
open LeanConsensus.Consensus
open LeanConsensus.Crypto.KeyState
open LeanConsensus.Network.P2P
open LeanConsensus.Network.BeaconAPI
open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- Client Configuration
-- ════════════════════════════════════════════════════════════════

/-- Full client configuration parsed from CLI arguments. -/
structure ClientConfig where
  dataDir        : System.FilePath := "./lean-consensus-data"
  genesisFile    : System.FilePath := "./genesis.json"
  beaconApiHost  : String := "127.0.0.1"
  beaconApiPort  : UInt16 := 5052
  validatorIndex : Option UInt64 := none
  keyFile        : Option System.FilePath := none
  metricsPort    : UInt16 := 9090
  logLevel       : String := "info"
  deriving Inhabited

-- ════════════════════════════════════════════════════════════════
-- CLI Parsing
-- ════════════════════════════════════════════════════════════════

/-- Print usage information. -/
def printHelp : IO Unit := do
  IO.println "Usage: lean-consensus [OPTIONS]"
  IO.println ""
  IO.println "Options:"
  IO.println "  --datadir DIR        Data directory (default: ./lean-consensus-data)"
  IO.println "  --genesis FILE       Genesis JSON file (default: ./genesis.json)"
  IO.println "  --beacon-api-host H  Beacon API host (default: 127.0.0.1)"
  IO.println "  --beacon-api-port P  Beacon API port (default: 5052)"
  IO.println "  --validator-index N  Validator index to run (optional)"
  IO.println "  --key-file FILE      XMSS key file path (optional)"
  IO.println "  --metrics-port P     Metrics port (default: 9090)"
  IO.println "  --log-level LEVEL    Log level: debug|info|warn|error (default: info)"
  IO.println "  --help               Show this help message"

/-- Parse CLI arguments into a ClientConfig. -/
def parseArgs (args : List String) : Except String ClientConfig := do
  let mut config : ClientConfig := {}
  let mut remaining := args
  while remaining.length > 0 do
    match remaining with
    | "--help" :: _ => .error "help"
    | "--datadir" :: val :: rest =>
      config := { config with dataDir := val }
      remaining := rest
    | "--genesis" :: val :: rest =>
      config := { config with genesisFile := val }
      remaining := rest
    | "--beacon-api-host" :: val :: rest =>
      config := { config with beaconApiHost := val }
      remaining := rest
    | "--beacon-api-port" :: val :: rest =>
      match val.toNat? with
      | some n => config := { config with beaconApiPort := n.toUInt16 }
      | none => .error s!"invalid port: {val}"
      remaining := rest
    | "--validator-index" :: val :: rest =>
      match val.toNat? with
      | some n => config := { config with validatorIndex := some n.toUInt64 }
      | none => .error s!"invalid validator index: {val}"
      remaining := rest
    | "--key-file" :: val :: rest =>
      config := { config with keyFile := some val }
      remaining := rest
    | "--metrics-port" :: val :: rest =>
      match val.toNat? with
      | some n => config := { config with metricsPort := n.toUInt16 }
      | none => .error s!"invalid metrics port: {val}"
      remaining := rest
    | "--log-level" :: val :: rest =>
      if val ∈ ["debug", "info", "warn", "error"] then
        config := { config with logLevel := val }
      else
        .error s!"invalid log level: {val} (expected debug|info|warn|error)"
      remaining := rest
    | unknown :: _ =>
      .error s!"unknown argument: {unknown}"
    | [] => pure ()
  return config

-- ════════════════════════════════════════════════════════════════
-- Config → ActorSystemConfig Conversion
-- ════════════════════════════════════════════════════════════════

/-- Convert a ClientConfig to an ActorSystemConfig for the actor system. -/
def toActorSystemConfig (config : ClientConfig) (genesisState : BeaconState)
    (genesisBlock : BeaconBlock) (keyState : KeyState) : ActorSystemConfig :=
  let beaconApiConfig : BeaconAPIConfig := {
    host := config.beaconApiHost
    port := config.beaconApiPort
  }
  let p2pConfig : P2PConfig := {
    beaconAPI := beaconApiConfig
  }
  let validatorConfig : ValidatorConfig := {
    keyState := keyState
    validatorIndex := config.validatorIndex.getD 0
    forkVersion := BytesN.zero 4
    genesisRoot := BytesN.zero 32
  }
  { p2pConfig
    validatorConfig
    genesisState
    genesisBlock
    startSlot := 0
    slotIntervalMs := (SECONDS_PER_SLOT * 1000).toUInt32 }

end LeanConsensus.Config
