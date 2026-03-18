/-
  Actor System — Orchestrated Actor Lifecycle

  Manages the full actor system lifecycle:
  - Spawn all actors in dependency order
  - Start the slot timer and SSE event listener
  - Orderly shutdown in reverse dependency order
-/

import LeanConsensus.Actor
import LeanConsensus.Actor.Messages
import LeanConsensus.Actor.P2PActor
import LeanConsensus.Actor.BlockchainActor
import LeanConsensus.Actor.ValidatorActor
import LeanConsensus.Network.P2P
import LeanConsensus.Consensus.Types
import LeanConsensus.Storage
import LeanConsensus.Metrics

namespace LeanConsensus.Actor.System

open LeanConsensus.Actor
open LeanConsensus.Actor.Messages
open LeanConsensus.Actor.P2PActor
open LeanConsensus.Actor.BlockchainActor
open LeanConsensus.Actor.ValidatorActor
open LeanConsensus.Network.P2P
open LeanConsensus.Consensus
open LeanConsensus.Storage
open LeanConsensus.Metrics
open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- Configuration
-- ════════════════════════════════════════════════════════════════

/-- Configuration for the full actor system. -/
structure ActorSystemConfig where
  p2pConfig       : P2PConfig
  validatorConfig : ValidatorConfig
  genesisState    : BeaconState
  genesisBlock    : BeaconBlock
  storage         : Option StorageBackend := none
  metrics         : Option BeaconMetrics := none
  startSlot       : UInt64 := 0
  slotIntervalMs  : UInt32 := 4000

-- ════════════════════════════════════════════════════════════════
-- Actor System
-- ════════════════════════════════════════════════════════════════

/-- Running actor system holding all actor handles. -/
structure ActorSystem where
  p2pNode         : P2PNode
  p2pActor        : ActorHandle P2PActorMsg
  blockchainActor : ActorHandle BlockchainActorMsg
  validatorActor  : ActorHandle ValidatorActorMsg
  slotAdapter     : ActorHandle SlotTick
  slotTimer       : Task (Except IO.Error Unit)
  eventListener   : Task (Except IO.Error Unit)

-- ════════════════════════════════════════════════════════════════
-- Startup
-- ════════════════════════════════════════════════════════════════

/-- Start the complete actor system.
    Spawn order: P2P node → validator → blockchain → P2P actor → slot timer → event listener.
    This ensures each actor has its dependencies available at spawn time. -/
def startActorSystem (config : ActorSystemConfig) : IO ActorSystem := do
  -- 1. Start P2P node (connects to sidecar)
  let p2pNode ← startNode config.p2pConfig

  -- 2. Spawn a temporary P2P actor handle (needed by blockchain and validator).
  --    We use a two-phase approach: create a placeholder, then wire up.
  --    Lean's actor model requires forward references, so we spawn in stages.

  -- Create a dummy P2P actor first (will be replaced)
  let dummyP2P ← spawnActor (msg := P2PActorMsg) fun _ => return true

  -- 3. Spawn validator actor (needs P2P handle for publishing)
  let validatorActor ← spawnValidatorActor config.validatorConfig dummyP2P config.metrics

  -- 4. Spawn blockchain actor (needs validator + P2P handles)
  let blockchainActor ← spawnBlockchainActor
    config.genesisState config.genesisBlock validatorActor dummyP2P
    config.storage config.metrics

  -- 5. Shut down dummy P2P and spawn real one (with blockchain handle)
  shutdown dummyP2P
  let p2pActor ← spawnP2PActor p2pNode blockchainActor

  -- 6. Start slot timer via an adapter actor that wraps SlotTick into BlockchainActorMsg
  let slotAdapter ← spawnActor (msg := SlotTick) fun tick => do
    send blockchainActor (.slotTick tick)
    return true
  let slotTimer ← spawnSlotTimer slotAdapter config.startSlot config.slotIntervalMs

  -- 7. Start SSE event listener
  let eventListener ← spawnEventListener p2pNode p2pActor

  return {
    p2pNode, p2pActor, blockchainActor, validatorActor,
    slotAdapter, slotTimer, eventListener
  }

-- ════════════════════════════════════════════════════════════════
-- Shutdown
-- ════════════════════════════════════════════════════════════════

/-- Shut down the actor system in reverse dependency order.
    P2P first (stops network input), then blockchain, then validator. -/
def shutdownActorSystem (system : ActorSystem) : IO Unit := do
  -- 1. Stop P2P actor (cuts off new network messages)
  send system.p2pActor .shutdown
  shutdown system.p2pActor

  -- 2. Stop the P2P node (closes SSE streams)
  stopNode system.p2pNode

  -- 3. Stop slot adapter
  shutdown system.slotAdapter

  -- 4. Stop blockchain actor
  send system.blockchainActor .shutdown
  shutdown system.blockchainActor

  -- 5. Stop validator actor
  send system.validatorActor .shutdown
  shutdown system.validatorActor

end LeanConsensus.Actor.System
