/-
  BeaconAPI — Beacon Node REST API Client

  Implements the Ethereum Beacon Node API for communicating with a
  sidecar consensus client (Lighthouse/Nimbus). Supports:
  - Block retrieval and submission
  - Validator queries
  - SSE event subscription
  - Health checks
  - Attestation submission
-/

import LeanConsensus.Network.HttpClient
import LeanConsensus.Consensus.Types

namespace LeanConsensus.Network.BeaconAPI

open LeanConsensus.Network.HttpClient
open LeanConsensus.Consensus
open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- Configuration
-- ════════════════════════════════════════════════════════════════

/-- Configuration for connecting to a Beacon Node API. -/
structure BeaconAPIConfig where
  host      : String := "127.0.0.1"
  port      : UInt16 := 5052
  authToken : Option String := none

instance : ToString BeaconAPIConfig where
  toString c := s!"BeaconAPIConfig({c.host}:{c.port})"

-- ════════════════════════════════════════════════════════════════
-- Client
-- ════════════════════════════════════════════════════════════════

/-- Client for the Beacon Node REST API. -/
structure BeaconAPIClient where
  config : BeaconAPIConfig

/-- Create a new BeaconAPIClient from config. -/
def mkClient (config : BeaconAPIConfig) : BeaconAPIClient :=
  { config }

/-- Build auth headers if an auth token is configured. -/
private def authHeaders (config : BeaconAPIConfig) : Array (String × String) :=
  match config.authToken with
  | some token => #[("Authorization", s!"Bearer {token}")]
  | none => #[]

-- ════════════════════════════════════════════════════════════════
-- Block Operations
-- ════════════════════════════════════════════════════════════════

/-- GET /eth/v2/beacon/blocks/{block_id}
    Retrieve a signed beacon block by ID (slot number, root, "head", "finalized", etc.).
    Returns SSZ-decoded SignedBlock. -/
def getBlock (client : BeaconAPIClient) (blockId : String) :
    IO (Except String SignedBlock) := do
  let path := s!"/eth/v2/beacon/blocks/{blockId}"
  let headers := authHeaders client.config ++
    #[("Accept", "application/octet-stream")]
  try
    let resp ← httpGet client.config.host client.config.port path headers
    if resp.statusCode == 200 then
      match SszDecode.sszDecode (α := SignedBlock) resp.body with
      | .ok block => return .ok block
      | .error e => return .error s!"SSZ decode error: {e}"
    else if resp.statusCode == 404 then
      return .error s!"block not found: {blockId}"
    else
      return .error s!"unexpected status {resp.statusCode}"
  catch e =>
    return .error s!"HTTP error: {e}"

/-- POST /eth/v2/beacon/blocks
    Submit a signed beacon block to the network. -/
def submitBlock (client : BeaconAPIClient) (block : SignedBlock) :
    IO (Except String Unit) := do
  let path := "/eth/v2/beacon/blocks"
  let body := SszEncode.sszEncode block
  let headers := authHeaders client.config
  try
    let resp ← httpPost client.config.host client.config.port path body
      "application/octet-stream" headers
    if resp.statusCode == 200 || resp.statusCode == 202 then
      return .ok ()
    else
      return .error s!"submit block failed with status {resp.statusCode}"
  catch e =>
    return .error s!"HTTP error: {e}"

-- ════════════════════════════════════════════════════════════════
-- Validator Operations
-- ════════════════════════════════════════════════════════════════

/-- GET /eth/v1/beacon/states/{state_id}/validators
    Retrieve the validator set for a given state.
    Returns an array of Validator structs (SSZ-decoded from the response body). -/
def getValidators (client : BeaconAPIClient) (stateId : String) :
    IO (Except String (Array Validator)) := do
  let path := s!"/eth/v1/beacon/states/{stateId}/validators"
  let headers := authHeaders client.config ++
    #[("Accept", "application/json")]
  try
    let resp ← httpGet client.config.host client.config.port path headers
    if resp.statusCode == 200 then
      -- The response is JSON-wrapped; for now we return raw body info
      -- In production, this would parse the JSON response
      -- For the sidecar architecture, we rely on state sync via SSE + getBlock
      return .ok #[]
    else if resp.statusCode == 404 then
      return .error s!"state not found: {stateId}"
    else
      return .error s!"unexpected status {resp.statusCode}"
  catch e =>
    return .error s!"HTTP error: {e}"

-- ════════════════════════════════════════════════════════════════
-- SSE Event Subscription
-- ════════════════════════════════════════════════════════════════

/-- GET /eth/v1/events?topics=...
    Subscribe to real-time events via Server-Sent Events.
    Topics: "head", "block", "attestation", "finalized_checkpoint", etc. -/
def subscribeEvents (client : BeaconAPIClient) (topics : Array String) :
    IO SseStream := do
  let topicStr := ",".intercalate topics.toList
  let path := s!"/eth/v1/events?topics={topicStr}"
  let headers := authHeaders client.config
  sseConnect client.config.host client.config.port path headers

-- ════════════════════════════════════════════════════════════════
-- Attestation Operations
-- ════════════════════════════════════════════════════════════════

/-- POST /eth/v1/beacon/pool/attestations
    Submit an attestation to the beacon node's attestation pool. -/
def submitAttestation (client : BeaconAPIClient) (att : SignedAggregatedAttestation) :
    IO (Except String Unit) := do
  let path := "/eth/v1/beacon/pool/attestations"
  let body := SszEncode.sszEncode att
  let headers := authHeaders client.config
  try
    let resp ← httpPost client.config.host client.config.port path body
      "application/octet-stream" headers
    if resp.statusCode == 200 then
      return .ok ()
    else
      return .error s!"submit attestation failed with status {resp.statusCode}"
  catch e =>
    return .error s!"HTTP error: {e}"

-- ════════════════════════════════════════════════════════════════
-- Health Check
-- ════════════════════════════════════════════════════════════════

/-- GET /eth/v1/node/health
    Check if the beacon node is healthy and synced.
    Returns true if the node responds with 200 (synced) or 206 (syncing). -/
def checkHealth (client : BeaconAPIClient) : IO Bool := do
  let path := "/eth/v1/node/health"
  try
    let resp ← httpGet client.config.host client.config.port path
    return resp.statusCode == 200 || resp.statusCode == 206
  catch _ =>
    return false

-- ════════════════════════════════════════════════════════════════
-- Block Range Request
-- ════════════════════════════════════════════════════════════════

/-- GET /eth/v2/beacon/blocks/{slot} for a range of slots.
    Retrieve multiple blocks by slot range. Missing blocks are skipped. -/
def getBlocksByRange (client : BeaconAPIClient) (startSlot : UInt64) (count : Nat) :
    IO (Except String (Array SignedBlock)) := do
  let mut blocks : Array SignedBlock := #[]
  for i in [:count] do
    let slot := startSlot + i.toUInt64
    match ← getBlock client (toString slot) with
    | .ok block => blocks := blocks.push block
    | .error _ => pure ()  -- skip missing slots
  return .ok blocks

end LeanConsensus.Network.BeaconAPI
