/-
  Validator Actor — Block Proposal and Attestation

  Handles validator duties:
  - Block proposal: creates and signs blocks when assigned as proposer
  - Attestation: creates and signs attestations for assigned slots
  - Uses XMSS signing via KeyState with monotonic epoch tracking
  - Publishes produced blocks/attestations via the P2P actor
-/

import LeanConsensus.Actor
import LeanConsensus.Actor.Messages
import LeanConsensus.Consensus.Signing
import LeanConsensus.Crypto.KeyState
import LeanConsensus.SSZ.BytesN
import LeanConsensus.SSZ.List

namespace LeanConsensus.Actor.ValidatorActor

open LeanConsensus.Actor
open LeanConsensus.Actor.Messages
open LeanConsensus.Consensus
open LeanConsensus.Crypto.KeyState
open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- Configuration
-- ════════════════════════════════════════════════════════════════

/-- Configuration for the validator actor. -/
structure ValidatorConfig where
  keyState       : KeyState
  validatorIndex : ValidatorIndex
  forkVersion    : Bytes4
  genesisRoot    : Bytes32

-- ════════════════════════════════════════════════════════════════
-- State
-- ════════════════════════════════════════════════════════════════

/-- Internal state of the validator actor. -/
structure ValidatorActorState where
  config : ValidatorConfig
  p2p    : ActorHandle P2PActorMsg

-- ════════════════════════════════════════════════════════════════
-- Block Proposal
-- ════════════════════════════════════════════════════════════════

/-- Handle a block proposal request.
    Creates a minimal block and signs it with the validator's XMSS key. -/
private def handleProposeBlock (state : ValidatorActorState)
    (slot : UInt64) : IO Unit := do
  -- Compute the signing domain for block proposals
  let domain := computeDomain DOMAIN_BEACON_PROPOSER
    state.config.forkVersion state.config.genesisRoot
  -- Create a minimal block (in production, this would include
  -- attestations from the pool and the correct parent/state roots)
  let emptyAtts : SszList MAX_ATTESTATIONS SignedAggregatedAttestation :=
    ⟨#[], Nat.zero_le _⟩
  let block : BeaconBlock := {
    slot := slot
    proposerIndex := state.config.validatorIndex
    parentRoot := BytesN.zero 32  -- would be filled from fork choice head
    stateRoot := BytesN.zero 32   -- would be filled after state transition
    body := { attestations := emptyAtts }
  }
  -- Compute signing root and sign
  let signingRoot := computeSigningRoot block domain
  let epoch := (slot / SECONDS_PER_SLOT.toUInt64).toUInt32
  try
    let sigBytes ← signAndAdvance state.config.keyState epoch signingRoot.data
    let signature : XmssSignature :=
      if h : sigBytes.size = XMSS_SIGNATURE_SIZE then ⟨sigBytes, h⟩
      else BytesN.zero XMSS_SIGNATURE_SIZE
    let signedBlock : SignedBeaconBlock := { block, signature }
    send state.p2p (.publishBlock signedBlock)
  catch e =>
    IO.eprintln s!"[validator] block signing failed: {e}"

-- ════════════════════════════════════════════════════════════════
-- Attestation
-- ════════════════════════════════════════════════════════════════

/-- Handle an attestation duty.
    Creates attestation data for the slot and signs it. -/
private def handleAttestSlot (state : ValidatorActorState)
    (slot : UInt64) : IO Unit := do
  let domain := computeDomain DOMAIN_BEACON_ATTESTER
    state.config.forkVersion state.config.genesisRoot
  -- Create attestation data (in production, head/source/target would
  -- come from the fork choice store)
  let attData : AttestationData := {
    slot := slot
    headRoot := BytesN.zero 32  -- would be getHead from fork choice
    sourceCheckpoint := { slot := 0, root := BytesN.zero 32 }
    targetCheckpoint := { slot := slot, root := BytesN.zero 32 }
  }
  let signingRoot := computeSigningRoot attData domain
  let epoch := (slot / SECONDS_PER_SLOT.toUInt64).toUInt32
  try
    let sigBytes ← signAndAdvance state.config.keyState epoch signingRoot.data
    let signature : XmssSignature :=
      if h : sigBytes.size = XMSS_SIGNATURE_SIZE then ⟨sigBytes, h⟩
      else BytesN.zero XMSS_SIGNATURE_SIZE
    let att : SignedAttestation := {
      data := attData
      validatorIndex := state.config.validatorIndex
      signature
    }
    -- For publishing, we need an aggregated form. In the full pipeline,
    -- this would go through the aggregation pool. For now, wrap as
    -- a single-validator aggregated attestation.
    let bits := Bitlist.zeros (maxCap := MAX_VALIDATORS_PER_SUBNET) 1 (by decide)
    let aggAtt : SignedAggregatedAttestation := {
      data := attData
      aggregationBits := bits
      aggregationProof := { data := ByteArray.empty }
    }
    send state.p2p (.publishAttestation aggAtt)
  catch e =>
    IO.eprintln s!"[validator] attestation signing failed: {e}"

-- ════════════════════════════════════════════════════════════════
-- Block Import Notification
-- ════════════════════════════════════════════════════════════════

/-- Handle notification that a block was imported. -/
private def handleBlockImported (_state : ValidatorActorState)
    (_root : BytesN 32) (_slot : UInt64) : IO Unit := do
  -- In production, this would update the validator's view of the chain
  -- and potentially trigger re-attestation or fork choice updates
  pure ()

-- ════════════════════════════════════════════════════════════════
-- Message Handler
-- ════════════════════════════════════════════════════════════════

/-- Handle a single validator actor message.
    Returns `true` to continue processing, `false` to terminate. -/
private def validatorHandler (state : ValidatorActorState)
    (msg : ValidatorActorMsg) : IO Bool := do
  match msg with
  | .proposeBlock slot =>
    handleProposeBlock state slot
    return true
  | .attestSlot slot =>
    handleAttestSlot state slot
    return true
  | .blockImported root slot =>
    handleBlockImported state root slot
    return true
  | .shutdown =>
    return false

-- ════════════════════════════════════════════════════════════════
-- Spawn
-- ════════════════════════════════════════════════════════════════

/-- Spawn the validator actor with the given configuration.
    The validator will process duties and sign blocks/attestations. -/
def spawnValidatorActor (config : ValidatorConfig)
    (p2p : ActorHandle P2PActorMsg) :
    IO (ActorHandle ValidatorActorMsg) := do
  let state : ValidatorActorState := { config, p2p }
  spawnActor fun msg => validatorHandler state msg

end LeanConsensus.Actor.ValidatorActor
