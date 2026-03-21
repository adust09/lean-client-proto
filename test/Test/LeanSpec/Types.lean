/-
  LeanSpec Fixture Bridge Types — JSON deserialization for leanSpec fixtures

  leanSpec generates JSON fixtures with camelCase keys and structures that
  match our aligned domain types.

  These types parse the fixture JSON and provide conversion where possible.
-/

import Lean.Data.Json
import LeanConsensus.SSZ
import LeanConsensus.Consensus.Types
import LeanConsensus.Consensus.Constants

namespace Test.LeanSpec

open Lean (Json FromJson ToJson)
open LeanConsensus.SSZ
open LeanConsensus.Consensus

-- ════════════════════════════════════════════════════════════════
-- Fixture sub-types (matching leanSpec JSON output)
-- ════════════════════════════════════════════════════════════════

structure FixtureCheckpoint where
  root : String
  slot : Nat

instance : FromJson FixtureCheckpoint where
  fromJson? json := do
    let root ← json.getObjValAs? String "root"
    let slot ← json.getObjValAs? Nat "slot"
    return { root, slot }

structure FixtureBlockHeader where
  slot          : Nat
  proposerIndex : Nat
  parentRoot    : String
  stateRoot     : String
  bodyRoot      : String

instance : FromJson FixtureBlockHeader where
  fromJson? json := do
    let slot ← json.getObjValAs? Nat "slot"
    let proposerIndex ← json.getObjValAs? Nat "proposerIndex"
    let parentRoot ← json.getObjValAs? String "parentRoot"
    let stateRoot ← json.getObjValAs? String "stateRoot"
    let bodyRoot ← json.getObjValAs? String "bodyRoot"
    return { slot, proposerIndex, parentRoot, stateRoot, bodyRoot }

structure FixtureValidator where
  attestationPubkey : String
  proposalPubkey    : String
  index             : Nat

instance : FromJson FixtureValidator where
  fromJson? json := do
    let attestationPubkey ← json.getObjValAs? String "attestationPubkey"
    let proposalPubkey ← json.getObjValAs? String "proposalPubkey"
    let index ← json.getObjValAs? Nat "index"
    return { attestationPubkey, proposalPubkey, index }

structure FixtureBlock where
  slot          : Nat
  proposerIndex : Nat
  parentRoot    : String
  stateRoot     : String

instance : FromJson FixtureBlock where
  fromJson? json := do
    let slot ← json.getObjValAs? Nat "slot"
    let proposerIndex ← json.getObjValAs? Nat "proposerIndex"
    let parentRoot ← json.getObjValAs? String "parentRoot"
    let stateRoot ← json.getObjValAs? String "stateRoot"
    return { slot, proposerIndex, parentRoot, stateRoot }

structure FixtureConfig where
  genesisTime : Nat

instance : FromJson FixtureConfig where
  fromJson? json := do
    let genesisTime ← json.getObjValAs? Nat "genesisTime"
    return { genesisTime }

structure FixtureState where
  slot                     : Nat
  config                   : FixtureConfig
  latestBlockHeader        : FixtureBlockHeader
  latestJustified          : FixtureCheckpoint
  latestFinalized          : FixtureCheckpoint
  historicalBlockHashes    : Array String
  justifiedSlots           : String
  validators               : Array FixtureValidator
  justificationsRoots      : Array String
  justificationsValidators : String

instance : FromJson FixtureState where
  fromJson? json := do
    let slot ← json.getObjValAs? Nat "slot"
    let config ← json.getObjValAs? FixtureConfig "config"
    let latestBlockHeader ← json.getObjValAs? FixtureBlockHeader "latestBlockHeader"
    let latestJustified ← json.getObjValAs? FixtureCheckpoint "latestJustified"
    let latestFinalized ← json.getObjValAs? FixtureCheckpoint "latestFinalized"
    let historicalBlockHashes := (json.getObjValAs? (Array String) "historicalBlockHashes" |>.toOption).getD #[]
    let justifiedSlots := (json.getObjValAs? String "justifiedSlots" |>.toOption).getD "0x"
    let validators := (json.getObjValAs? (Array FixtureValidator) "validators" |>.toOption).getD #[]
    let justificationsRoots := (json.getObjValAs? (Array String) "justificationsRoots" |>.toOption).getD #[]
    let justificationsValidators := (json.getObjValAs? String "justificationsValidators" |>.toOption).getD "0x"
    return { slot, config, latestBlockHeader, latestJustified, latestFinalized,
             historicalBlockHashes, justifiedSlots, validators,
             justificationsRoots, justificationsValidators }

-- ════════════════════════════════════════════════════════════════
-- Hex Utilities
-- ════════════════════════════════════════════════════════════════

/-- Convert a single hex character to its numeric value. -/
private def hexDigit (c : Char) : Option UInt8 :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat).toUInt8
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10).toUInt8
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10).toUInt8
  else none

/-- Convert a hex string (with optional 0x prefix) to ByteArray. -/
def hexToBytes (hex : String) : Option ByteArray := do
  let s := if hex.startsWith "0x" then String.ofList (hex.toList.drop 2) else hex
  let chars := s.toList
  if chars.length % 2 != 0 then .none
  else
    let mut result := ByteArray.empty
    let mut i := 0
    while h : i + 1 < chars.length do
      let hi ← hexDigit (chars[i]'(by omega))
      let lo ← hexDigit (chars[i + 1]'(by omega))
      result := result.push (hi * 16 + lo)
      i := i + 2
    return result

/-- Convert a hex string to Bytes32, returning zero on failure. -/
def hexToBytes32 (hex : String) : Root :=
  match hexToBytes hex with
  | some ba => if h : ba.size = 32 then ⟨ba, h⟩ else BytesN.zero 32
  | none => BytesN.zero 32

/-- Convert a hex string to BytesN n, returning zero on failure. -/
def hexToBytesN (n : Nat) (hex : String) : BytesN n :=
  match hexToBytes hex with
  | some ba => if h : ba.size = n then ⟨ba, h⟩ else BytesN.zero n
  | none => BytesN.zero n

-- ════════════════════════════════════════════════════════════════
-- Fixture → Domain Conversions
-- ════════════════════════════════════════════════════════════════

def FixtureCheckpoint.toCheckpoint (fc : FixtureCheckpoint) : Checkpoint :=
  { root := hexToBytes32 fc.root, slot := fc.slot.toUInt64 }

def FixtureBlockHeader.toBlockHeader (fh : FixtureBlockHeader) : BeaconBlockHeader :=
  { slot := fh.slot.toUInt64
    proposerIndex := fh.proposerIndex.toUInt64
    parentRoot := hexToBytes32 fh.parentRoot
    stateRoot := hexToBytes32 fh.stateRoot
    bodyRoot := hexToBytes32 fh.bodyRoot }

def FixtureValidator.toValidator (fv : FixtureValidator) : Validator :=
  { attestationPubkey := hexToBytesN XMSS_PUBKEY_SIZE fv.attestationPubkey
    proposalPubkey := hexToBytesN XMSS_PUBKEY_SIZE fv.proposalPubkey
    index := fv.index.toUInt64 }

/-- Convert a FixtureState to a domain State.
    Variable-length fields are parsed from hex; on parse failure they default to empty. -/
def FixtureState.toState (fs : FixtureState) : Except String State := do
  let cfg : Config := { genesisTime := fs.config.genesisTime.toUInt64 }
  let hdr := fs.latestBlockHeader.toBlockHeader
  let hashes := fs.historicalBlockHashes.map hexToBytes32
  let vals := fs.validators.map FixtureValidator.toValidator
  let jRoots := fs.justificationsRoots.map hexToBytes32
  if hv : vals.size ≤ VALIDATOR_REGISTRY_LIMIT then
    if hh : hashes.size ≤ HISTORICAL_ROOTS_LIMIT then
      if hjr : jRoots.size ≤ HISTORICAL_ROOTS_LIMIT then
        .ok { config := cfg
              slot := fs.slot.toUInt64
              latestBlockHeader := hdr
              latestJustified := fs.latestJustified.toCheckpoint
              latestFinalized := fs.latestFinalized.toCheckpoint
              historicalBlockHashes := ⟨hashes, hh⟩
              justifiedSlots := Bitlist.empty HISTORICAL_ROOTS_LIMIT
              validators := ⟨vals, hv⟩
              justificationsRoots := ⟨jRoots, hjr⟩
              justificationsValidators := Bitlist.empty (HISTORICAL_ROOTS_LIMIT * VALIDATOR_REGISTRY_LIMIT) }
      else .error s!"justificationsRoots exceeds limit: {jRoots.size}"
    else .error s!"historicalBlockHashes exceeds limit: {hashes.size}"
  else .error s!"validators exceeds registry limit: {vals.size}"

-- ════════════════════════════════════════════════════════════════
-- SSZ fixture type
-- ════════════════════════════════════════════════════════════════

structure SSZFixture where
  typeName   : String
  serialized : String
  value      : Json

instance : FromJson SSZFixture where
  fromJson? json := do
    let typeName ← json.getObjValAs? String "typeName"
    let serialized ← json.getObjValAs? String "serialized"
    let value := json.getObjValD "value"
    return { typeName, serialized, value }

-- ════════════════════════════════════════════════════════════════
-- State transition fixture type
-- ════════════════════════════════════════════════════════════════

structure STFixture where
  pre    : FixtureState
  blocks : Array FixtureBlock

instance : FromJson STFixture where
  fromJson? json := do
    let pre ← json.getObjValAs? FixtureState "pre"
    let blocks ← json.getObjValAs? (Array FixtureBlock) "blocks"
    return { pre, blocks }

-- ════════════════════════════════════════════════════════════════
-- Fork choice fixture type
-- ════════════════════════════════════════════════════════════════

structure FCStepChecks where
  headSlot : Option Nat

instance : FromJson FCStepChecks where
  fromJson? json := do
    let headSlot := json.getObjValAs? Nat "headSlot" |>.toOption
    return { headSlot }

structure FCStep where
  valid    : Bool
  stepType : String
  block    : Option FixtureBlock
  checks   : Option FCStepChecks

instance : FromJson FCStep where
  fromJson? json := do
    let valid := (json.getObjValAs? Bool "valid" |>.toOption).getD true
    let stepType ← json.getObjValAs? String "stepType"
    let block := json.getObjValAs? FixtureBlock "block" |>.toOption
    let checks := json.getObjValAs? FCStepChecks "checks" |>.toOption
    return { valid, stepType, block, checks }

structure FCFixture where
  anchorState : FixtureState
  anchorBlock : FixtureBlock
  steps       : Array FCStep

instance : FromJson FCFixture where
  fromJson? json := do
    let anchorState ← json.getObjValAs? FixtureState "anchorState"
    let anchorBlock ← json.getObjValAs? FixtureBlock "anchorBlock"
    let steps ← json.getObjValAs? (Array FCStep) "steps"
    return { anchorState, anchorBlock, steps }

end Test.LeanSpec
