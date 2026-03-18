/-
  Genesis — Load and build genesis state from JSON configuration

  Parses a genesis JSON file containing validator deposits and chain
  parameters. Builds the initial BeaconState and BeaconBlock from
  the genesis configuration.
-/

import Lean.Data.Json
import LeanConsensus.SSZ
import LeanConsensus.Consensus.Types
import LeanConsensus.Consensus.Constants

namespace LeanConsensus.Genesis

open LeanConsensus.SSZ
open LeanConsensus.Consensus
open Lean (Json FromJson ToJson)

-- ════════════════════════════════════════════════════════════════
-- Genesis JSON Types
-- ════════════════════════════════════════════════════════════════

/-- Chain-level genesis parameters. -/
structure GenesisConfig where
  genesisTime         : Nat
  forkVersion         : String
  validatorCount      : Nat
  balancePerValidator : Nat
  deriving Inhabited

instance : FromJson GenesisConfig where
  fromJson? json := do
    let genesisTime ← json.getObjValAs? Nat "genesis_time"
    let forkVersion ← json.getObjValAs? String "fork_version"
    let validatorCount ← json.getObjValAs? Nat "validator_count"
    let balancePerValidator ← json.getObjValAs? Nat "balance_per_validator"
    return { genesisTime, forkVersion, validatorCount, balancePerValidator }

instance : ToJson GenesisConfig where
  toJson c := Json.mkObj [
    ("genesis_time", ToJson.toJson c.genesisTime),
    ("fork_version", ToJson.toJson c.forkVersion),
    ("validator_count", ToJson.toJson c.validatorCount),
    ("balance_per_validator", ToJson.toJson c.balancePerValidator)
  ]

/-- A single validator deposit entry. -/
structure ValidatorDeposit where
  pubkey  : String
  balance : Nat
  deriving Inhabited

instance : FromJson ValidatorDeposit where
  fromJson? json := do
    let pubkey ← json.getObjValAs? String "pubkey"
    let balance ← json.getObjValAs? Nat "balance"
    return { pubkey, balance }

instance : ToJson ValidatorDeposit where
  toJson d := Json.mkObj [
    ("pubkey", ToJson.toJson d.pubkey),
    ("balance", ToJson.toJson d.balance)
  ]

/-- Complete genesis file with config and validator list. -/
structure GenesisFile where
  config     : GenesisConfig
  validators : Array ValidatorDeposit
  deriving Inhabited

instance : FromJson GenesisFile where
  fromJson? json := do
    let config ← json.getObjValAs? GenesisConfig "config"
    let validators ← json.getObjValAs? (Array ValidatorDeposit) "validators"
    return { config, validators }

instance : ToJson GenesisFile where
  toJson g := Json.mkObj [
    ("config", ToJson.toJson g.config),
    ("validators", ToJson.toJson g.validators)
  ]

-- ════════════════════════════════════════════════════════════════
-- File Loading
-- ════════════════════════════════════════════════════════════════

/-- Load and parse a genesis JSON file from the given path. -/
def loadGenesisFile (path : System.FilePath) : IO GenesisFile := do
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error e => throw (IO.userError s!"failed to parse genesis JSON: {e}")
  | .ok json =>
    match FromJson.fromJson? json with
    | .error e => throw (IO.userError s!"invalid genesis file format: {e}")
    | .ok genesis => return genesis

-- ════════════════════════════════════════════════════════════════
-- Genesis State Construction
-- ════════════════════════════════════════════════════════════════

/-- Create a dummy XMSS pubkey from a hex string (for genesis initialization).
    In production, these would be real XMSS public keys from deposit data. -/
private def dummyPubkey : XmssPubkey := BytesN.zero XMSS_PUBKEY_SIZE

/-- Build a Validator from deposit data. -/
private def buildValidator (deposit : ValidatorDeposit) : Validator :=
  { pubkey := dummyPubkey
    effectiveBalance := deposit.balance.toUInt64
    slashed := false
    activationSlot := 0
    exitSlot := UInt64.ofNat FAR_FUTURE_SLOT
    withdrawableSlot := UInt64.ofNat FAR_FUTURE_SLOT }

/-- Build the genesis BeaconState from a GenesisFile.
    Returns an error if validation fails (e.g., too many validators). -/
def buildGenesisState (genesis : GenesisFile) : Except String BeaconState := do
  let validators := genesis.validators.map buildValidator
  let balances := genesis.validators.map fun d => d.balance.toUInt64
  -- Validate registry limit
  if validators.size > VALIDATOR_REGISTRY_LIMIT then
    .error s!"too many validators: {validators.size} > {VALIDATOR_REGISTRY_LIMIT}"
  -- Build genesis block header
  let genesisHeader : BeaconBlockHeader := {
    slot := 0, proposerIndex := 0,
    parentRoot := BytesN.zero 32, stateRoot := BytesN.zero 32,
    bodyRoot := BytesN.zero 32
  }
  -- Build empty historical roots
  let zeroRoots : Array Root := Array.replicate SLOTS_PER_HISTORICAL_ROOT (BytesN.zero 32)
  -- Construct the state with proof obligations
  if hv : validators.size ≤ VALIDATOR_REGISTRY_LIMIT then
    if hb : balances.size ≤ VALIDATOR_REGISTRY_LIMIT then
      if hr : zeroRoots.size = SLOTS_PER_HISTORICAL_ROOT then
        let emptyAtts : SszList MAX_ATTESTATIONS_STATE SignedAggregatedAttestation :=
          ⟨#[], Nat.zero_le _⟩
        .ok { slot := 0
              latestBlockHeader := genesisHeader
              blockRoots := ⟨zeroRoots, hr⟩
              stateRoots := ⟨zeroRoots, hr⟩
              validators := ⟨validators, hv⟩
              balances := ⟨balances, hb⟩
              justifiedCheckpoint := { slot := 0, root := BytesN.zero 32 }
              finalizedCheckpoint := { slot := 0, root := BytesN.zero 32 }
              currentAttestations := emptyAtts }
      else
        .error "internal error: failed to construct historical roots"
    else
      .error s!"balances array exceeds registry limit"
  else
    .error s!"validators array exceeds registry limit"

/-- Build the genesis block (slot 0, empty body). -/
def buildGenesisBlock : BeaconBlock :=
  let emptyAtts : SszList MAX_ATTESTATIONS SignedAggregatedAttestation :=
    ⟨#[], Nat.zero_le _⟩
  { slot := 0
    proposerIndex := 0
    parentRoot := BytesN.zero 32
    stateRoot := BytesN.zero 32
    body := { attestations := emptyAtts } }

end LeanConsensus.Genesis
