/-
  Blockchain Actor — Block and State Management

  Core consensus actor that:
  - Processes incoming blocks through fork choice + state transition
  - Processes attestations through fork choice
  - Advances slots on timer ticks
  - Notifies the validator actor of imported blocks and slot duties
-/

import LeanConsensus.Actor
import LeanConsensus.Actor.Messages
import LeanConsensus.Consensus.ForkChoice
import LeanConsensus.Consensus.StateTransition
import LeanConsensus.Storage
import LeanConsensus.Metrics

namespace LeanConsensus.Actor.BlockchainActor

open LeanConsensus.Actor
open LeanConsensus.Actor.Messages
open LeanConsensus.Consensus
open LeanConsensus.Consensus.ForkChoice
open LeanConsensus.Consensus.StateTransition
open LeanConsensus.SSZ
open LeanConsensus.Storage
open LeanConsensus.Metrics

-- ════════════════════════════════════════════════════════════════
-- State
-- ════════════════════════════════════════════════════════════════

/-- Internal state of the blockchain actor. -/
structure BlockchainActorState where
  store     : IO.Ref Store
  validator : ActorHandle ValidatorActorMsg
  p2p       : ActorHandle P2PActorMsg
  storage   : Option StorageBackend := none
  metrics   : Option BeaconMetrics := none

-- ════════════════════════════════════════════════════════════════
-- Block Handling
-- ════════════════════════════════════════════════════════════════

/-- Process a new signed block through fork choice and state transition. -/
private def handleNewBlock (state : BlockchainActorState)
    (signed : SignedBlock) : IO Unit := do
  let store ← state.store.get
  match onBlock store signed.message with
  | .ok newStore =>
    state.store.set newStore
    let root := blockRoot signed.message
    if let some sb := state.storage then
      sb.putBlock root signed.message
    if let some m := state.metrics then
      m.blocksImported.increment
      m.headSlot.set signed.message.slot.toNat
    send state.validator (.blockImported root signed.message.slot)
  | .error e =>
    IO.eprintln s!"[blockchain] block rejected: {e}"

-- ════════════════════════════════════════════════════════════════
-- Attestation Handling
-- ════════════════════════════════════════════════════════════════

/-- Process a new attestation through fork choice. -/
private def handleNewAttestation (state : BlockchainActorState)
    (att : SignedAttestation) : IO Unit := do
  let store ← state.store.get
  match onAttestation store att.validatorIndex att.data with
  | .ok newStore =>
    state.store.set newStore
    if let some m := state.metrics then
      m.attestationsImported.increment
  | .error e =>
    IO.eprintln s!"[blockchain] attestation rejected: {e}"

-- ════════════════════════════════════════════════════════════════
-- Slot Tick Handling
-- ════════════════════════════════════════════════════════════════

/-- Handle a slot tick: advance the store's time and determine duties. -/
private def handleSlotTick (state : BlockchainActorState)
    (tick : SlotTick) : IO Unit := do
  let store ← state.store.get
  let newStore := onTick store tick.slot
  state.store.set newStore
  send state.validator (.proposeBlock tick.slot)
  send state.validator (.attestSlot tick.slot)

-- ════════════════════════════════════════════════════════════════
-- Message Handler
-- ════════════════════════════════════════════════════════════════

/-- Handle a single blockchain actor message.
    Returns `true` to continue processing, `false` to terminate. -/
private def blockchainHandler (state : BlockchainActorState)
    (msg : BlockchainActorMsg) : IO Bool := do
  match msg with
  | .newBlock block =>
    handleNewBlock state block
    return true
  | .newAttestation att =>
    handleNewAttestation state att
    return true
  | .slotTick tick =>
    handleSlotTick state tick
    return true
  | .shutdown =>
    return false

-- ════════════════════════════════════════════════════════════════
-- Spawn
-- ════════════════════════════════════════════════════════════════

/-- Spawn the blockchain actor with the given genesis state and block.
    Initializes the fork choice store from genesis. -/
def spawnBlockchainActor (genesisState : State)
    (genesisBlock : Block)
    (validator : ActorHandle ValidatorActorMsg)
    (p2p : ActorHandle P2PActorMsg)
    (storage : Option StorageBackend := none)
    (metrics : Option BeaconMetrics := none) :
    IO (ActorHandle BlockchainActorMsg) := do
  let store ← IO.mkRef (fromAnchor genesisState genesisBlock)
  let state : BlockchainActorState := { store, validator, p2p, storage, metrics }
  spawnActor fun msg => blockchainHandler state msg

/-- Get the current head root from the blockchain actor's store. -/
def getHead (storeRef : IO.Ref Store) : IO Root := do
  let store ← storeRef.get
  return ForkChoice.getHead store

end LeanConsensus.Actor.BlockchainActor
