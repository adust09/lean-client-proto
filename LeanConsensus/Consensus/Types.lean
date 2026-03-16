/-
  Consensus Types — pq-devnet-3 with 3-Slot Finality

  All beacon chain structures with their SSZ serialization instances.
  Types use dependent-typed collections (SszVector, SszList, BytesN)
  to enforce invariants at the type level.

  Manual SSZ instances use SszEncoder for containers (Phase 1a approach).
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
  deriving BEq

instance : SszType Checkpoint where
  sszFixedSize := some 40

instance : SszEncode Checkpoint where
  sszEncode c :=
    let enc := { : SszEncoder }
    let enc := SszEncoder.addField enc c.slot
    let enc := SszEncoder.addField enc c.root
    SszEncoder.finalize enc

instance : SszDecode Checkpoint where
  sszDecode data := do
    if data.size != 40 then
      .error (.invalidLength 40 data.size)
    let slot ← decodeUInt64At data 0
    let rootData := data.extract 8 40
    let root ← BytesN.mkChecked rootData
    .ok { slot, root }

instance : SszHashTreeRoot Checkpoint where
  hashTreeRoot c :=
    merkleize #[
      SszHashTreeRoot.hashTreeRoot c.slot,
      SszHashTreeRoot.hashTreeRoot c.root
    ] 2

-- ════════════════════════════════════════════════════════════════
-- AttestationData
-- ════════════════════════════════════════════════════════════════

structure AttestationData where
  slot             : Slot
  headRoot         : Root
  sourceCheckpoint : Checkpoint
  targetCheckpoint : Checkpoint
  deriving BEq

instance : SszType AttestationData where
  sszFixedSize := some 120

instance : SszEncode AttestationData where
  sszEncode a :=
    let enc := { : SszEncoder }
    let enc := SszEncoder.addField enc a.slot
    let enc := SszEncoder.addField enc a.headRoot
    let enc := SszEncoder.addField enc a.sourceCheckpoint
    let enc := SszEncoder.addField enc a.targetCheckpoint
    SszEncoder.finalize enc

instance : SszDecode AttestationData where
  sszDecode data := do
    if data.size != 120 then
      .error (.invalidLength 120 data.size)
    let slot ← decodeUInt64At data 0
    let headRoot ← BytesN.mkChecked (data.extract 8 40)
    let sourceCheckpoint ← SszDecode.sszDecode (data.extract 40 80)
    let targetCheckpoint ← SszDecode.sszDecode (data.extract 80 120)
    .ok { slot, headRoot, sourceCheckpoint, targetCheckpoint }

instance : SszHashTreeRoot AttestationData where
  hashTreeRoot a :=
    merkleize #[
      SszHashTreeRoot.hashTreeRoot a.slot,
      SszHashTreeRoot.hashTreeRoot a.headRoot,
      SszHashTreeRoot.hashTreeRoot a.sourceCheckpoint,
      SszHashTreeRoot.hashTreeRoot a.targetCheckpoint
    ] 4

-- ════════════════════════════════════════════════════════════════
-- SignedAttestation
-- ════════════════════════════════════════════════════════════════

structure SignedAttestation where
  data           : AttestationData
  validatorIndex : ValidatorIndex
  signature      : XmssSignature
  deriving BEq

instance : SszType SignedAttestation where
  sszFixedSize := some 3240  -- 120 + 8 + 3112

instance : SszEncode SignedAttestation where
  sszEncode sa :=
    let enc := { : SszEncoder }
    let enc := SszEncoder.addField enc sa.data
    let enc := SszEncoder.addField enc sa.validatorIndex
    let enc := SszEncoder.addField enc sa.signature
    SszEncoder.finalize enc

instance : SszDecode SignedAttestation where
  sszDecode data := do
    if data.size != 3240 then
      .error (.invalidLength 3240 data.size)
    let attData ← SszDecode.sszDecode (data.extract 0 120)
    let valIdx ← decodeUInt64At data 120
    let sig ← BytesN.mkChecked (data.extract 128 3240)
    .ok { data := attData, validatorIndex := valIdx, signature := sig }

instance : SszHashTreeRoot SignedAttestation where
  hashTreeRoot sa :=
    merkleize #[
      SszHashTreeRoot.hashTreeRoot sa.data,
      SszHashTreeRoot.hashTreeRoot sa.validatorIndex,
      SszHashTreeRoot.hashTreeRoot sa.signature
    ] 4

-- ════════════════════════════════════════════════════════════════
-- LeanMultisigProof
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
  deriving BEq

instance : SszType SignedAggregatedAttestation where
  sszFixedSize := none

instance : SszEncode SignedAggregatedAttestation where
  sszEncode saa :=
    let enc := { : SszEncoder }
    let enc := SszEncoder.addField enc saa.data
    let enc := SszEncoder.addField enc saa.aggregationBits
    let enc := SszEncoder.addField enc saa.aggregationProof
    SszEncoder.finalize enc

instance : SszDecode SignedAggregatedAttestation where
  sszDecode data := do
    if data.size < 128 then
      .error (.unexpectedEndOfInput 128 data.size)
    let attData ← SszDecode.sszDecode (data.extract 0 120)
    let offset1 ← decodeUInt32At data 120
    let offset2 ← decodeUInt32At data 124
    let o1 := offset1.toNat
    let o2 := offset2.toNat
    if o1 > o2 then
      .error (.offsetsNotMonotonic offset1 offset2)
    if o2 > data.size then
      .error (.invalidOffset offset2 data.size)
    let bits ← SszDecode.sszDecode (data.extract o1 o2)
    let proof ← SszDecode.sszDecode (data.extract o2 data.size)
    .ok { data := attData, aggregationBits := bits, aggregationProof := proof }

instance : SszHashTreeRoot SignedAggregatedAttestation where
  hashTreeRoot saa :=
    let bitChunks := packBits saa.aggregationBits.data
    let bitLimit := (MAX_VALIDATORS_PER_SUBNET + 255) / 256
    merkleize #[
      SszHashTreeRoot.hashTreeRoot saa.data,
      mixInLength (merkleize bitChunks bitLimit) saa.aggregationBits.length,
      SszHashTreeRoot.hashTreeRoot saa.aggregationProof
    ] 4

-- ════════════════════════════════════════════════════════════════
-- BeaconBlockBody
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
  deriving BEq

instance : SszType BeaconBlock where
  sszFixedSize := none

instance : SszEncode BeaconBlock where
  sszEncode blk :=
    let enc := { : SszEncoder }
    let enc := SszEncoder.addField enc blk.slot
    let enc := SszEncoder.addField enc blk.proposerIndex
    let enc := SszEncoder.addField enc blk.parentRoot
    let enc := SszEncoder.addField enc blk.stateRoot
    let enc := SszEncoder.addField enc blk.body
    SszEncoder.finalize enc

instance : SszDecode BeaconBlock where
  sszDecode data := do
    if data.size < 84 then
      .error (.unexpectedEndOfInput 84 data.size)
    let slot ← decodeUInt64At data 0
    let proposerIndex ← decodeUInt64At data 8
    let parentRoot ← BytesN.mkChecked (data.extract 16 48)
    let stateRoot ← BytesN.mkChecked (data.extract 48 80)
    let bodyOffset ← decodeUInt32At data 80
    let body ← SszDecode.sszDecode (data.extract bodyOffset.toNat data.size)
    .ok { slot, proposerIndex, parentRoot, stateRoot, body }

instance : SszHashTreeRoot BeaconBlock where
  hashTreeRoot blk :=
    merkleize #[
      SszHashTreeRoot.hashTreeRoot blk.slot,
      SszHashTreeRoot.hashTreeRoot blk.proposerIndex,
      SszHashTreeRoot.hashTreeRoot blk.parentRoot,
      SszHashTreeRoot.hashTreeRoot blk.stateRoot,
      SszHashTreeRoot.hashTreeRoot blk.body
    ] 8

-- ════════════════════════════════════════════════════════════════
-- SignedBeaconBlock
-- ════════════════════════════════════════════════════════════════

structure SignedBeaconBlock where
  block     : BeaconBlock
  signature : XmssSignature
  deriving BEq

instance : SszType SignedBeaconBlock where
  sszFixedSize := none

instance : SszEncode SignedBeaconBlock where
  sszEncode sbb :=
    let enc := { : SszEncoder }
    let enc := SszEncoder.addField enc sbb.block
    let enc := SszEncoder.addField enc sbb.signature
    SszEncoder.finalize enc

instance : SszDecode SignedBeaconBlock where
  sszDecode data := do
    if data.size < 3116 then
      .error (.unexpectedEndOfInput 3116 data.size)
    let blockOffset ← decodeUInt32At data 0
    let sig ← BytesN.mkChecked (data.extract 4 3116)
    let block ← SszDecode.sszDecode (data.extract blockOffset.toNat data.size)
    .ok { block, signature := sig }

instance : SszHashTreeRoot SignedBeaconBlock where
  hashTreeRoot sbb :=
    merkleize #[
      SszHashTreeRoot.hashTreeRoot sbb.block,
      SszHashTreeRoot.hashTreeRoot sbb.signature
    ] 2

-- ════════════════════════════════════════════════════════════════
-- BeaconBlockHeader
-- ════════════════════════════════════════════════════════════════

structure BeaconBlockHeader where
  slot          : Slot
  proposerIndex : ValidatorIndex
  parentRoot    : Root
  stateRoot     : Root
  bodyRoot      : Root
  deriving BEq

instance : SszType BeaconBlockHeader where
  sszFixedSize := some 112

instance : SszEncode BeaconBlockHeader where
  sszEncode h :=
    let enc := { : SszEncoder }
    let enc := SszEncoder.addField enc h.slot
    let enc := SszEncoder.addField enc h.proposerIndex
    let enc := SszEncoder.addField enc h.parentRoot
    let enc := SszEncoder.addField enc h.stateRoot
    let enc := SszEncoder.addField enc h.bodyRoot
    SszEncoder.finalize enc

instance : SszDecode BeaconBlockHeader where
  sszDecode data := do
    if data.size != 112 then
      .error (.invalidLength 112 data.size)
    let slot ← decodeUInt64At data 0
    let proposerIndex ← decodeUInt64At data 8
    let parentRoot ← BytesN.mkChecked (data.extract 16 48)
    let stateRoot ← BytesN.mkChecked (data.extract 48 80)
    let bodyRoot ← BytesN.mkChecked (data.extract 80 112)
    .ok { slot, proposerIndex, parentRoot, stateRoot, bodyRoot }

instance : SszHashTreeRoot BeaconBlockHeader where
  hashTreeRoot h :=
    merkleize #[
      SszHashTreeRoot.hashTreeRoot h.slot,
      SszHashTreeRoot.hashTreeRoot h.proposerIndex,
      SszHashTreeRoot.hashTreeRoot h.parentRoot,
      SszHashTreeRoot.hashTreeRoot h.stateRoot,
      SszHashTreeRoot.hashTreeRoot h.bodyRoot
    ] 8

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
  deriving BEq

instance : SszType Validator where
  sszFixedSize := some 65

instance : SszEncode Validator where
  sszEncode v :=
    let enc := { : SszEncoder }
    let enc := SszEncoder.addField enc v.pubkey
    let enc := SszEncoder.addField enc v.effectiveBalance
    let enc := SszEncoder.addField enc v.slashed
    let enc := SszEncoder.addField enc v.activationSlot
    let enc := SszEncoder.addField enc v.exitSlot
    let enc := SszEncoder.addField enc v.withdrawableSlot
    SszEncoder.finalize enc

instance : SszDecode Validator where
  sszDecode data := do
    if data.size != 65 then
      .error (.invalidLength 65 data.size)
    let pubkey ← BytesN.mkChecked (data.extract 0 32)
    let effectiveBalance ← decodeUInt64At data 32
    let slashed ← decodeBoolAt data 40
    let activationSlot ← decodeUInt64At data 41
    let exitSlot ← decodeUInt64At data 49
    let withdrawableSlot ← decodeUInt64At data 57
    .ok { pubkey, effectiveBalance, slashed, activationSlot, exitSlot, withdrawableSlot }

instance : SszHashTreeRoot Validator where
  hashTreeRoot v :=
    merkleize #[
      SszHashTreeRoot.hashTreeRoot v.pubkey,
      SszHashTreeRoot.hashTreeRoot v.effectiveBalance,
      SszHashTreeRoot.hashTreeRoot v.slashed,
      SszHashTreeRoot.hashTreeRoot v.activationSlot,
      SszHashTreeRoot.hashTreeRoot v.exitSlot,
      SszHashTreeRoot.hashTreeRoot v.withdrawableSlot
    ] 8

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
  deriving BEq

instance : SszType BeaconState where
  sszFixedSize := none

instance : SszEncode BeaconState where
  sszEncode s :=
    let enc := { : SszEncoder }
    let enc := SszEncoder.addField enc s.slot
    let enc := SszEncoder.addField enc s.latestBlockHeader
    let enc := SszEncoder.addField enc s.blockRoots
    let enc := SszEncoder.addField enc s.stateRoots
    let enc := SszEncoder.addField enc s.validators
    let enc := SszEncoder.addField enc s.balances
    let enc := SszEncoder.addField enc s.justifiedCheckpoint
    let enc := SszEncoder.addField enc s.finalizedCheckpoint
    let enc := SszEncoder.addField enc s.currentAttestations
    SszEncoder.finalize enc

instance : SszDecode BeaconState where
  sszDecode data := do
    -- Fixed region: 8 + 112 + 2048 + 2048 + 4 + 4 + 40 + 40 + 4 = 4308
    let fixedSize := 4308
    if data.size < fixedSize then
      .error (.unexpectedEndOfInput fixedSize data.size)
    let slot ← decodeUInt64At data 0
    let latestBlockHeader ← SszDecode.sszDecode (data.extract 8 120)
    let blockRoots ← SszDecode.sszDecode (data.extract 120 2168)
    let stateRoots ← SszDecode.sszDecode (data.extract 2168 4216)
    let validatorsOff ← decodeUInt32At data 4216
    let balancesOff ← decodeUInt32At data 4220
    let justifiedCheckpoint ← SszDecode.sszDecode (data.extract 4224 4264)
    let finalizedCheckpoint ← SszDecode.sszDecode (data.extract 4264 4304)
    let attestationsOff ← decodeUInt32At data 4304
    let vo := validatorsOff.toNat
    let bo := balancesOff.toNat
    let ao := attestationsOff.toNat
    let validators ← SszDecode.sszDecode (data.extract vo bo)
    let balances ← SszDecode.sszDecode (data.extract bo ao)
    let currentAttestations ← SszDecode.sszDecode (data.extract ao data.size)
    .ok {
      slot, latestBlockHeader, blockRoots, stateRoots,
      validators, balances, justifiedCheckpoint, finalizedCheckpoint,
      currentAttestations
    }

instance : SszHashTreeRoot BeaconState where
  hashTreeRoot s :=
    let validatorHashes := s.validators.elems.map SszHashTreeRoot.hashTreeRoot
    let balanceChunks := pack (encodeFixedList s.balances)
    merkleize #[
      SszHashTreeRoot.hashTreeRoot s.slot,
      SszHashTreeRoot.hashTreeRoot s.latestBlockHeader,
      merkleize (s.blockRoots.elems.map (·.data)) SLOTS_PER_HISTORICAL_ROOT,
      merkleize (s.stateRoots.elems.map (·.data)) SLOTS_PER_HISTORICAL_ROOT,
      mixInLength
        (merkleize validatorHashes (chunkCountVariable VALIDATOR_REGISTRY_LIMIT))
        s.validators.elems.size,
      mixInLength
        (merkleize balanceChunks (chunkCountFixed VALIDATOR_REGISTRY_LIMIT 8))
        s.balances.elems.size,
      SszHashTreeRoot.hashTreeRoot s.justifiedCheckpoint,
      SszHashTreeRoot.hashTreeRoot s.finalizedCheckpoint,
      let attHashes := s.currentAttestations.elems.map SszHashTreeRoot.hashTreeRoot
      mixInLength
        (merkleize attHashes (chunkCountVariable MAX_ATTESTATIONS_STATE))
        s.currentAttestations.elems.size
    ] 16

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
