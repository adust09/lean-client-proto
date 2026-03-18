/-
  Actor Messages — Inter-Actor Message Types

  Defines the message types exchanged between actors in the consensus client:
  - P2PActorMsg: network ↔ consensus boundary
  - BlockchainActorMsg: block/attestation processing + slot ticks
  - ValidatorActorMsg: block proposal + attestation duties
-/

import LeanConsensus.Consensus.Types
import LeanConsensus.Actor

namespace LeanConsensus.Actor.Messages

open LeanConsensus.Consensus
open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- P2P Actor Messages
-- ════════════════════════════════════════════════════════════════

/-- Messages handled by the P2P actor.
    Bridges between network events and internal consensus logic. -/
inductive P2PActorMsg where
  | networkBlock (block : SignedBeaconBlock)
  | networkAttestation (att : SignedAttestation)
  | publishBlock (block : SignedBeaconBlock)
  | publishAttestation (att : SignedAggregatedAttestation)
  | shutdown

-- ════════════════════════════════════════════════════════════════
-- Blockchain Actor Messages
-- ════════════════════════════════════════════════════════════════

/-- Messages handled by the blockchain actor.
    Processes blocks, attestations, and slot advancement. -/
inductive BlockchainActorMsg where
  | newBlock (block : SignedBeaconBlock)
  | newAttestation (att : SignedAttestation)
  | slotTick (tick : SlotTick)
  | shutdown

-- ════════════════════════════════════════════════════════════════
-- Validator Actor Messages
-- ════════════════════════════════════════════════════════════════

/-- Messages handled by the validator actor.
    Triggers block proposal, attestation, and notifications. -/
inductive ValidatorActorMsg where
  | proposeBlock (slot : UInt64)
  | attestSlot (slot : UInt64)
  | blockImported (root : BytesN 32) (slot : UInt64)
  | shutdown

end LeanConsensus.Actor.Messages
