/-
  P2P — Sidecar-Based P2P Networking Layer

  Wraps the Beacon Node REST API to provide a P2P-like interface for the
  consensus client. Instead of directly using libp2p, delegates all network
  operations to a companion Lighthouse/Nimbus sidecar:
  - Block/attestation publishing via REST API
  - Event subscription via SSE (Server-Sent Events)
  - Block range requests for sync
-/

import LeanConsensus.Network.BeaconAPI
import LeanConsensus.Network.HttpClient
import Std.Sync.CancellationToken

namespace LeanConsensus.Network.P2P

open LeanConsensus.Network.BeaconAPI
open LeanConsensus.Network.HttpClient
open LeanConsensus.Consensus
open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- Configuration
-- ════════════════════════════════════════════════════════════════

/-- Configuration for the P2P sidecar wrapper. -/
structure P2PConfig where
  beaconAPI : BeaconAPIConfig

instance : ToString P2PConfig where
  toString c := s!"P2PConfig(api={c.beaconAPI})"

-- ════════════════════════════════════════════════════════════════
-- P2P Node
-- ════════════════════════════════════════════════════════════════

/-- A running P2P node backed by a Beacon API sidecar. -/
structure P2PNode where
  client      : BeaconAPIClient
  eventStream : IO.Ref (Option SseStream)
  token       : Std.CancellationToken

-- ════════════════════════════════════════════════════════════════
-- Lifecycle
-- ════════════════════════════════════════════════════════════════

/-- Start a P2P node by connecting to the beacon node API.
    Verifies the sidecar is healthy before returning. -/
def startNode (config : P2PConfig) : IO P2PNode := do
  let client := mkClient config.beaconAPI
  let healthy ← checkHealth client
  if !healthy then
    throw (IO.userError "beacon node sidecar is not healthy")
  let eventStreamRef ← IO.mkRef (none : Option SseStream)
  let token ← Std.CancellationToken.new
  return { client, eventStream := eventStreamRef, token }

/-- Subscribe to SSE events from the beacon node.
    Connects to the event stream for the specified topics. -/
def subscribe (node : P2PNode) (topics : Array String) : IO SseStream := do
  let stream ← subscribeEvents node.client topics
  node.eventStream.set (some stream)
  return stream

-- ════════════════════════════════════════════════════════════════
-- Publishing
-- ════════════════════════════════════════════════════════════════

/-- Publish a signed beacon block to the network via the sidecar. -/
def publish (node : P2PNode) (block : SignedBlock) :
    IO (Except String Unit) :=
  submitBlock node.client block

/-- Publish an attestation to the network via the sidecar. -/
def publishAttestation (node : P2PNode) (att : SignedAggregatedAttestation) :
    IO (Except String Unit) :=
  submitAttestation node.client att

-- ════════════════════════════════════════════════════════════════
-- Data Retrieval
-- ════════════════════════════════════════════════════════════════

/-- Request a range of blocks from the beacon node for sync. -/
def requestBlocksByRange (node : P2PNode) (startSlot : UInt64) (count : Nat) :
    IO (Except String (Array SignedBlock)) :=
  getBlocksByRange node.client startSlot count

/-- Request a single block by its identifier. -/
def requestBlock (node : P2PNode) (blockId : String) :
    IO (Except String SignedBlock) :=
  getBlock node.client blockId

-- ════════════════════════════════════════════════════════════════
-- Shutdown
-- ════════════════════════════════════════════════════════════════

/-- Stop the P2P node and close any open event streams. -/
def stopNode (node : P2PNode) : IO Unit := do
  node.token.cancel
  match ← node.eventStream.get with
  | some stream => stream.close
  | none => pure ()

end LeanConsensus.Network.P2P
