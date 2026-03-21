/-
  Consensus Types — pq-devnet-3 with 3-Slot Finality

  All beacon chain structures with their SSZ serialization instances.
  Types use dependent-typed collections (SszVector, SszList, BytesN)
  to enforce invariants at the type level.

  SSZ instances are auto-derived using the custom derive handlers in
  LeanConsensus.SSZ.Derive.
-/

import Std.Data.HashMap
import LeanConsensus.SSZ.Error
import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.BytesN
import LeanConsensus.SSZ.Encode
import LeanConsensus.SSZ.Decode
import LeanConsensus.SSZ.Bitvector
import LeanConsensus.SSZ.Bitlist
import LeanConsensus.SSZ.Vector
import LeanConsensus.SSZ.List
import LeanConsensus.SSZ.Merkleization
import LeanConsensus.SSZ.Derive
import LeanConsensus.Consensus.Constants

namespace LeanConsensus.Consensus

open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- Primitive Aliases
-- ════════════════════════════════════════════════════════════════

abbrev Slot           := UInt64
abbrev ValidatorIndex := UInt64
abbrev Gwei           := UInt64
abbrev Root           := Bytes32
abbrev Domain         := Bytes32
abbrev Version        := Bytes4

-- Cryptographic type aliases (using literals for derive macro compatibility)
abbrev XmssPubkey    := BytesN 52
abbrev XmssSignature := BytesN 424

-- ════════════════════════════════════════════════════════════════
-- Config
-- ════════════════════════════════════════════════════════════════

structure Config where
  genesisTime : UInt64
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- Checkpoint
-- ════════════════════════════════════════════════════════════════

structure Checkpoint where
  root : Root
  slot : Slot
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- AttestationData
-- ════════════════════════════════════════════════════════════════

structure AttestationData where
  slot   : Slot
  head   : Checkpoint
  target : Checkpoint
  source : Checkpoint
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- Validator
-- ════════════════════════════════════════════════════════════════

structure Validator where
  attestationPubkey : BytesN 52
  proposalPubkey    : BytesN 52
  index             : ValidatorIndex
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- AggregationBits
-- ════════════════════════════════════════════════════════════════

abbrev AggregationBits := Bitlist VALIDATOR_REGISTRY_LIMIT

-- ════════════════════════════════════════════════════════════════
-- ByteListMiB (manual SSZ — variable-size bounded byte list)
-- ════════════════════════════════════════════════════════════════

structure ByteListMiB where
  data : ByteArray
  hbound : data.size ≤ 1048576

instance : BEq ByteListMiB where
  beq a b := a.data == b.data

instance : SszType ByteListMiB where
  sszFixedSize := none

instance : SszEncode ByteListMiB where
  sszEncode b := b.data

instance : SszDecode ByteListMiB where
  sszDecode data :=
    if h : data.size ≤ 1048576 then
      .ok ⟨data, h⟩
    else
      .error (.other "ByteListMiB exceeds 1 MiB")

instance : SszHashTreeRoot ByteListMiB where
  hashTreeRoot b :=
    let chunks := pack b.data
    mixInLength (merkleize chunks (chunkCountVariable 1048576)) b.data.size

-- ════════════════════════════════════════════════════════════════
-- AggregatedSignatureProof
-- ════════════════════════════════════════════════════════════════

structure AggregatedSignatureProof where
  participants : AggregationBits
  proofData    : ByteListMiB
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- Attestation (single-validator, unsigned)
-- ════════════════════════════════════════════════════════════════

structure Attestation where
  data           : AttestationData
  validatorIndex : ValidatorIndex
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- SignedAttestation
-- ════════════════════════════════════════════════════════════════

structure SignedAttestation where
  data           : AttestationData
  validatorIndex : ValidatorIndex
  signature      : XmssSignature
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- AggregatedAttestation
-- ════════════════════════════════════════════════════════════════

structure AggregatedAttestation where
  aggregationBits : AggregationBits
  data            : AttestationData
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- SignedAggregatedAttestation
-- ════════════════════════════════════════════════════════════════

structure SignedAggregatedAttestation where
  data  : AttestationData
  proof : AggregatedSignatureProof
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- BlockBody (manual — single-field direct delegation, non-standard)
-- ════════════════════════════════════════════════════════════════

structure BlockBody where
  attestations : SszList MAX_ATTESTATIONS AggregatedAttestation
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- Block (renamed from BeaconBlock)
-- ════════════════════════════════════════════════════════════════

structure Block where
  slot          : Slot
  proposerIndex : ValidatorIndex
  parentRoot    : Root
  stateRoot     : Root
  body          : BlockBody
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- BlockSignatures
-- ════════════════════════════════════════════════════════════════

structure BlockSignatures where
  attestationSignatures : SszList MAX_ATTESTATIONS AggregatedSignatureProof
  proposerSignature     : XmssSignature
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- SignedBlock (renamed from SignedBeaconBlock)
-- ════════════════════════════════════════════════════════════════

structure SignedBlock where
  message   : Block
  signature : BlockSignatures
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- BeaconBlockHeader
-- ════════════════════════════════════════════════════════════════

structure BeaconBlockHeader where
  slot          : Slot
  proposerIndex : ValidatorIndex
  parentRoot    : Root
  stateRoot     : Root
  bodyRoot      : Root
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- State (renamed from BeaconState)
-- ════════════════════════════════════════════════════════════════

structure State where
  config                   : Config
  slot                     : Slot
  latestBlockHeader        : BeaconBlockHeader
  latestJustified          : Checkpoint
  latestFinalized          : Checkpoint
  historicalBlockHashes    : SszList HISTORICAL_ROOTS_LIMIT Root
  justifiedSlots           : Bitlist HISTORICAL_ROOTS_LIMIT
  validators               : SszList VALIDATOR_REGISTRY_LIMIT Validator
  justificationsRoots      : SszList HISTORICAL_ROOTS_LIMIT Root
  justificationsValidators : Bitlist (HISTORICAL_ROOTS_LIMIT * VALIDATOR_REGISTRY_LIMIT)
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- Non-SSZ Internal Types (for fork choice)
-- ════════════════════════════════════════════════════════════════

/-- Interval-based time: ticks since genesis (5 intervals per slot). -/
abbrev Interval := UInt64

structure Store where
  time                           : Interval
  config                         : Config
  head                           : Root
  safeTarget                     : Root
  latestJustified                : Checkpoint
  latestFinalized                : Checkpoint
  blocks                         : Std.HashMap Root Block
  states                         : Std.HashMap Root State
  validatorId                    : Option ValidatorIndex
  attestationSignatures          : Std.HashMap Root (Array SignedAttestation)
  latestNewAggregatedPayloads    : Std.HashMap Root SignedAggregatedAttestation
  latestKnownAggregatedPayloads  : Std.HashMap Root SignedAggregatedAttestation

end LeanConsensus.Consensus
