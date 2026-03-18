/-
  P2P Actor — Network Interface Actor

  Bridges between the P2P network layer and the internal consensus actors.
  - Receives blocks/attestations from the network via SSE events
  - Forwards them to the BlockchainActor for processing
  - Publishes blocks/attestations produced locally to the network
-/

import LeanConsensus.Actor
import LeanConsensus.Actor.Messages
import LeanConsensus.Network.P2P
import LeanConsensus.Network.HttpClient

namespace LeanConsensus.Actor.P2PActor

open LeanConsensus.Actor
open LeanConsensus.Actor.Messages
open LeanConsensus.Network.P2P
open LeanConsensus.Network.HttpClient
open LeanConsensus.Consensus
open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- State
-- ════════════════════════════════════════════════════════════════

/-- Internal state of the P2P actor. -/
structure P2PActorState where
  node       : P2PNode
  blockchain : ActorHandle BlockchainActorMsg

-- ════════════════════════════════════════════════════════════════
-- Message Handler
-- ════════════════════════════════════════════════════════════════

/-- Handle a single P2P actor message.
    Returns `true` to continue processing, `false` to terminate. -/
private def p2pHandler (state : P2PActorState) (msg : P2PActorMsg) : IO Bool := do
  match msg with
  | .networkBlock block =>
    -- Forward received block to blockchain actor
    send state.blockchain (.newBlock block)
    return true
  | .networkAttestation att =>
    -- Forward received attestation to blockchain actor
    send state.blockchain (.newAttestation att)
    return true
  | .publishBlock block =>
    -- Publish a locally produced block to the network
    match ← publish state.node block with
    | .ok () => pure ()
    | .error e => IO.eprintln s!"[p2p] failed to publish block: {e}"
    return true
  | .publishAttestation att =>
    -- Publish a locally produced attestation to the network
    match ← publishAttestation state.node att with
    | .ok () => pure ()
    | .error e => IO.eprintln s!"[p2p] failed to publish attestation: {e}"
    return true
  | .shutdown =>
    return false

-- ════════════════════════════════════════════════════════════════
-- Spawn
-- ════════════════════════════════════════════════════════════════

/-- Spawn the P2P actor.
    Connects the P2P network node to the blockchain actor. -/
def spawnP2PActor (node : P2PNode) (blockchain : ActorHandle BlockchainActorMsg) :
    IO (ActorHandle P2PActorMsg) := do
  let state : P2PActorState := { node, blockchain }
  spawnActor fun msg => p2pHandler state msg

-- ════════════════════════════════════════════════════════════════
-- SSE Event Listener
-- ════════════════════════════════════════════════════════════════

/-- Spawn a background task that reads SSE events from the beacon node
    and forwards them as P2PActorMsg messages. -/
partial def spawnEventListener (node : P2PNode) (p2p : ActorHandle P2PActorMsg) :
    IO (Task (Except IO.Error Unit)) := do
  let stream ← subscribe node #["block", "attestation"]
  IO.asTask (prio := .default) do
    while true do
      let cancelled ← p2p.token.isCancelled
      if cancelled then break
      match ← stream.next with
      | none => break  -- stream closed
      | some event =>
        -- SSE events from the beacon node contain JSON data.
        -- For blocks, we need to fetch the full block via REST API.
        if event.event == "block" then
          -- The SSE block event contains the block root/slot as JSON.
          -- Fetch the actual block via the API.
          match ← requestBlock node "head" with
          | .ok block => send p2p (.networkBlock block)
          | .error _ => pure ()
        -- For attestations, the SSE event data would need JSON parsing.
        -- Attestation handling via SSE is a future enhancement.

end LeanConsensus.Actor.P2PActor
