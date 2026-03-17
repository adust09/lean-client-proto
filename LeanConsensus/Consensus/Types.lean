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

-- Cryptographic type aliases
abbrev XmssPubkey    := BytesN XMSS_PUBKEY_SIZE
abbrev XmssSignature := BytesN XMSS_SIGNATURE_SIZE

-- ════════════════════════════════════════════════════════════════
-- Checkpoint
-- ════════════════════════════════════════════════════════════════

structure Checkpoint where
  slot : Slot
  root : Root
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- AttestationData
-- ════════════════════════════════════════════════════════════════

structure AttestationData where
  slot             : Slot
  headRoot         : Root
  sourceCheckpoint : Checkpoint
  targetCheckpoint : Checkpoint
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
-- LeanMultisigProof (manual — opaque ByteArray wrapper, non-standard)
-- ════════════════════════════════════════════════════════════════

structure LeanMultisigProof where
  data : ByteArray
  deriving BEq

instance : SszType LeanMultisigProof where
  sszFixedSize := none

instance : SszEncode LeanMultisigProof where
  sszEncode p := p.data

instance : SszDecode LeanMultisigProof where
  sszDecode data := .ok { data }

instance : SszHashTreeRoot LeanMultisigProof where
  hashTreeRoot p :=
    let chunks := pack p.data
    merkleize chunks chunks.size

-- ════════════════════════════════════════════════════════════════
-- SignedAggregatedAttestation
-- ════════════════════════════════════════════════════════════════

structure SignedAggregatedAttestation where
  data             : AttestationData
  aggregationBits  : Bitlist MAX_VALIDATORS_PER_SUBNET
  aggregationProof : LeanMultisigProof
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- BeaconBlockBody (manual — single-field direct delegation, non-standard)
-- ════════════════════════════════════════════════════════════════

structure BeaconBlockBody where
  attestations : SszList MAX_ATTESTATIONS SignedAggregatedAttestation
  deriving BEq

instance : SszType BeaconBlockBody where
  sszFixedSize := none

instance : SszEncode BeaconBlockBody where
  sszEncode body := SszEncode.sszEncode body.attestations

instance : SszDecode BeaconBlockBody where
  sszDecode data := do
    let attestations ← SszDecode.sszDecode data
    .ok { attestations }

instance : SszHashTreeRoot BeaconBlockBody where
  hashTreeRoot body :=
    let attHashes := body.attestations.elems.map SszHashTreeRoot.hashTreeRoot
    let attRoot := mixInLength
      (merkleize attHashes (chunkCountVariable MAX_ATTESTATIONS))
      body.attestations.elems.size
    merkleize #[attRoot] 1

-- ════════════════════════════════════════════════════════════════
-- BeaconBlock
-- ════════════════════════════════════════════════════════════════

structure BeaconBlock where
  slot          : Slot
  proposerIndex : ValidatorIndex
  parentRoot    : Root
  stateRoot     : Root
  body          : BeaconBlockBody
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- SignedBeaconBlock
-- ════════════════════════════════════════════════════════════════

structure SignedBeaconBlock where
  block     : BeaconBlock
  signature : XmssSignature
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
-- Validator
-- ════════════════════════════════════════════════════════════════

structure Validator where
  pubkey           : XmssPubkey
  effectiveBalance : Gwei
  slashed          : Bool
  activationSlot   : Slot
  exitSlot         : Slot
  withdrawableSlot : Slot
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- BeaconState
-- ════════════════════════════════════════════════════════════════

structure BeaconState where
  slot                : Slot
  latestBlockHeader   : BeaconBlockHeader
  blockRoots          : SszVector SLOTS_PER_HISTORICAL_ROOT Root
  stateRoots          : SszVector SLOTS_PER_HISTORICAL_ROOT Root
  validators          : SszList VALIDATOR_REGISTRY_LIMIT Validator
  balances            : SszList VALIDATOR_REGISTRY_LIMIT Gwei
  justifiedCheckpoint : Checkpoint
  finalizedCheckpoint : Checkpoint
  currentAttestations : SszList MAX_ATTESTATIONS_STATE SignedAggregatedAttestation
  deriving BEq, SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- Non-SSZ Internal Types (for fork choice)
-- ════════════════════════════════════════════════════════════════

structure LatestMessage where
  slot : Slot
  root : Root
  deriving BEq

structure Store where
  justifiedCheckpoint : Checkpoint
  finalizedCheckpoint : Checkpoint
  blocks              : Std.HashMap Root BeaconBlock
  blockStates         : Std.HashMap Root BeaconState
  latestMessages      : Std.HashMap ValidatorIndex LatestMessage
  currentSlot         : Slot

end LeanConsensus.Consensus
