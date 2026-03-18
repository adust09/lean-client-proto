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

namespace LeanConsensus.Actor.BlockchainActor

open LeanConsensus.Actor
open LeanConsensus.Actor.Messages
open LeanConsensus.Consensus
open LeanConsensus.Consensus.ForkChoice
open LeanConsensus.Consensus.StateTransition
open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- State
-- ════════════════════════════════════════════════════════════════

/-- Internal state of the blockchain actor. -/
structure BlockchainActorState where
  store     : IO.Ref Store
  validator : ActorHandle ValidatorActorMsg
  p2p       : ActorHandle P2PActorMsg

-- ════════════════════════════════════════════════════════════════
-- Block Handling
-- ════════════════════════════════════════════════════════════════

/-- Process a new signed block through fork choice and state transition. -/
private def handleNewBlock (state : BlockchainActorState)
    (signed : SignedBeaconBlock) : IO Unit := do
  let store ← state.store.get
  match onBlock store signed.block with
  | .ok newStore =>
    state.store.set newStore
    let root := blockRoot signed.block
    send state.validator (.blockImported root signed.block.slot)
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
  | .error e =>
    IO.eprintln s!"[blockchain] attestation rejected: {e}"

-- ════════════════════════════════════════════════════════════════
-- Slot Tick Handling
-- ════════════════════════════════════════════════════════════════

/-- Handle a slot tick: advance the store's current slot and determine duties. -/
private def handleSlotTick (state : BlockchainActorState)
    (tick : SlotTick) : IO Unit := do
  let store ← state.store.get
  let newStore := { store with currentSlot := tick.slot }
  state.store.set newStore
  -- Notify validator of slot advancement for duty assignment
  -- In a full implementation, this would check the validator's duties
  -- (proposer/attester) for the new slot
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
def spawnBlockchainActor (genesisState : BeaconState)
    (genesisBlock : BeaconBlock)
    (validator : ActorHandle ValidatorActorMsg)
    (p2p : ActorHandle P2PActorMsg) :
    IO (ActorHandle BlockchainActorMsg) := do
  let store ← IO.mkRef (initStore genesisState genesisBlock)
  let state : BlockchainActorState := { store, validator, p2p }
  spawnActor fun msg => blockchainHandler state msg

/-- Get the current head root from the blockchain actor's store. -/
def getHead (storeRef : IO.Ref Store) : IO Root := do
  let store ← storeRef.get
  return ForkChoice.getHead store

end LeanConsensus.Actor.BlockchainActor
