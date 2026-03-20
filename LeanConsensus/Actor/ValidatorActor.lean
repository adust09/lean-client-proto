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
import LeanConsensus.Metrics

namespace LeanConsensus.Actor.ValidatorActor

open LeanConsensus.Actor
open LeanConsensus.Actor.Messages
open LeanConsensus.Consensus
open LeanConsensus.Crypto.KeyState
open LeanConsensus.SSZ
open LeanConsensus.Metrics

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
  config  : ValidatorConfig
  p2p     : ActorHandle P2PActorMsg
  metrics : Option BeaconMetrics := none

-- ════════════════════════════════════════════════════════════════
-- Block Proposal
-- ════════════════════════════════════════════════════════════════

/-- Handle a block proposal request.
    Creates a minimal block and signs it with the validator's XMSS key. -/
private def handleProposeBlock (state : ValidatorActorState)
    (slot : UInt64) : IO Unit := do
  let domain := computeDomain DOMAIN_BEACON_PROPOSER
    state.config.forkVersion state.config.genesisRoot
  let emptyAtts : SszList MAX_ATTESTATIONS AggregatedAttestation :=
    ⟨#[], Nat.zero_le _⟩
  let block : Block := {
    slot := slot
    proposerIndex := state.config.validatorIndex
    parentRoot := BytesN.zero 32
    stateRoot := BytesN.zero 32
    body := { attestations := emptyAtts }
  }
  let signingRoot := computeSigningRoot block domain
  let epoch := (slot / SECONDS_PER_SLOT.toUInt64).toUInt32
  try
    let sigBytes ← signAndAdvance state.config.keyState epoch signingRoot.data
    let proposerSig : XmssSignature :=
      if h : sigBytes.size = XMSS_SIGNATURE_SIZE then ⟨sigBytes, h⟩
      else BytesN.zero XMSS_SIGNATURE_SIZE
    let emptyProofs : SszList MAX_ATTESTATIONS AggregatedSignatureProof :=
      ⟨#[], Nat.zero_le _⟩
    let blockSigs : BlockSignatures := {
      attestationSignatures := emptyProofs
      proposerSignature := proposerSig
    }
    let signedBlock : SignedBlock := { message := block, signature := blockSigs }
    send state.p2p (.publishBlock signedBlock)
    if let some m := state.metrics then
      m.blocksProposed.increment
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
  let zeroCheckpoint : Checkpoint := { root := BytesN.zero 32, slot := 0 }
  let attData : AttestationData := {
    slot := slot
    head := zeroCheckpoint
    source := zeroCheckpoint
    target := { root := BytesN.zero 32, slot := slot }
  }
  let signingRoot := computeSigningRoot attData domain
  let epoch := (slot / SECONDS_PER_SLOT.toUInt64).toUInt32
  try
    let sigBytes ← signAndAdvance state.config.keyState epoch signingRoot.data
    let signature : XmssSignature :=
      if h : sigBytes.size = XMSS_SIGNATURE_SIZE then ⟨sigBytes, h⟩
      else BytesN.zero XMSS_SIGNATURE_SIZE
    let _att : SignedAttestation := {
      data := attData
      validatorIndex := state.config.validatorIndex
      signature
    }
    let emptyProofData : ByteListMiB := ⟨ByteArray.empty, by decide⟩
    let emptyBits : AggregationBits := Bitlist.empty VALIDATOR_REGISTRY_LIMIT
    let proof : AggregatedSignatureProof := {
      participants := emptyBits
      proofData := emptyProofData
    }
    let aggAtt : SignedAggregatedAttestation := {
      data := attData
      proof := proof
    }
    send state.p2p (.publishAttestation aggAtt)
    if let some m := state.metrics then
      m.attestationsProduced.increment
  catch e =>
    IO.eprintln s!"[validator] attestation signing failed: {e}"

-- ════════════════════════════════════════════════════════════════
-- Block Import Notification
-- ════════════════════════════════════════════════════════════════

/-- Handle notification that a block was imported. -/
private def handleBlockImported (_state : ValidatorActorState)
    (_root : BytesN 32) (_slot : UInt64) : IO Unit := do
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
    (p2p : ActorHandle P2PActorMsg)
    (metrics : Option BeaconMetrics := none) :
    IO (ActorHandle ValidatorActorMsg) := do
  let state : ValidatorActorState := { config, p2p, metrics }
  spawnActor fun msg => validatorHandler state msg

end LeanConsensus.Actor.ValidatorActor
