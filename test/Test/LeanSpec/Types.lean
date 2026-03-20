/-
  LeanSpec Fixture Bridge Types — JSON deserialization for leanSpec fixtures

  leanSpec generates JSON fixtures with camelCase keys and structures that
  diverge from our domain types (e.g., Validator has attestation_pubkey +
  proposal_pubkey + index vs our pubkey + effectiveBalance + slashed + slots).

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
  slot            : Nat
  config          : FixtureConfig
  latestBlockHeader : FixtureBlockHeader
  latestJustified : FixtureCheckpoint
  latestFinalized : FixtureCheckpoint

instance : FromJson FixtureState where
  fromJson? json := do
    let slot ← json.getObjValAs? Nat "slot"
    let config ← json.getObjValAs? FixtureConfig "config"
    let latestBlockHeader ← json.getObjValAs? FixtureBlockHeader "latestBlockHeader"
    let latestJustified ← json.getObjValAs? FixtureCheckpoint "latestJustified"
    let latestFinalized ← json.getObjValAs? FixtureCheckpoint "latestFinalized"
    return { slot, config, latestBlockHeader, latestJustified, latestFinalized }

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
