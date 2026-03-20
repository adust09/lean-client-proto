/-
  State Transition Tests
-/

import LeanConsensus.Consensus.StateTransition
import LeanConsensus.Consensus.Constants

namespace Test.Consensus.StateTransition

open LeanConsensus.SSZ
open LeanConsensus.Consensus
open LeanConsensus.Consensus.StateTransition

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

/-- Create a minimal genesis State with N validators. -/
def mkGenesisState (numValidators : Nat) : State :=
  let validators : Array Validator := Array.range numValidators |>.map fun i =>
    { attestationPubkey := BytesN.zero XMSS_PUBKEY_SIZE
      proposalPubkey := BytesN.zero XMSS_PUBKEY_SIZE
      index := i.toUInt64 }
  let zeroRoot := Bytes32.zero
  let genesisHeader : BeaconBlockHeader :=
    { slot := 0
      proposerIndex := 0
      parentRoot := zeroRoot
      stateRoot := zeroRoot
      bodyRoot := zeroRoot }
  let cfg : Config := { genesisTime := 0 }
  let zeroCheckpoint : Checkpoint := { root := zeroRoot, slot := 0 }
  match SszList.mkChecked (maxCap := VALIDATOR_REGISTRY_LIMIT) validators with
  | .ok vs =>
    { config := cfg
      slot := 0
      latestBlockHeader := genesisHeader
      latestJustified := zeroCheckpoint
      latestFinalized := zeroCheckpoint
      historicalBlockHashes := SszList.empty
      justifiedSlots := Bitlist.empty HISTORICAL_ROOTS_LIMIT
      validators := vs
      justificationsRoots := SszList.empty
      justificationsValidators := Bitlist.empty (HISTORICAL_ROOTS_LIMIT * VALIDATOR_REGISTRY_LIMIT) }
  | .error _ =>
    { config := cfg
      slot := 0
      latestBlockHeader := genesisHeader
      latestJustified := zeroCheckpoint
      latestFinalized := zeroCheckpoint
      historicalBlockHashes := SszList.empty
      justifiedSlots := Bitlist.empty HISTORICAL_ROOTS_LIMIT
      validators := SszList.empty
      justificationsRoots := SszList.empty
      justificationsValidators := Bitlist.empty (HISTORICAL_ROOTS_LIMIT * VALIDATOR_REGISTRY_LIMIT) }

def runTests : IO (Nat × Nat) := do
  IO.println "\n── State Transition ──"
  let mut total := 0
  let mut failures := 0

  let genesis := mkGenesisState 4

  -- Test 1: processSlot advances slot by 1
  do
    let s1 := processSlot genesis
    let (t, f) ← check "processSlot advances slot by 1" (s1.slot == 1)
    total := total + t; failures := failures + f

  -- Test 2: processSlots advances multiple slots
  do
    match processSlots genesis 3 with
    | .ok s3 =>
      let (t, f) ← check "processSlots to slot 3" (s3.slot == 3)
      total := total + t; failures := failures + f
    | .error e =>
      let (t, f) ← check s!"processSlots to slot 3 (error: {e})" false
      total := total + t; failures := failures + f

  -- Test 3: processSlots rejects targetSlot ≤ current
  do
    match processSlots genesis 0 with
    | .ok _ =>
      let (t, f) ← check "processSlots rejects slot 0" false
      total := total + t; failures := failures + f
    | .error _ =>
      let (t, f) ← check "processSlots rejects slot 0" true
      total := total + t; failures := failures + f

  -- Test 4: processBlock with valid block on genesis state
  do
    let s1 := processSlot genesis
    let parentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot s1.latestBlockHeader)
    let block : Block :=
      { slot := 1
        proposerIndex := 0
        parentRoot := parentRoot
        stateRoot := Bytes32.zero
        body := { attestations := SszList.empty } }
    match processBlock s1 block with
    | .ok s2 =>
      let (t, f) ← check "processBlock with valid block" (s2.slot == 1)
      total := total + t; failures := failures + f
    | .error e =>
      let (t, f) ← check s!"processBlock valid (error: {e})" false
      total := total + t; failures := failures + f

  -- Test 5: processBlock rejects wrong slot
  do
    let s1 := processSlot genesis
    let parentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot s1.latestBlockHeader)
    let block : Block :=
      { slot := 99
        proposerIndex := 0
        parentRoot := parentRoot
        stateRoot := Bytes32.zero
        body := { attestations := SszList.empty } }
    match processBlock s1 block with
    | .ok _ =>
      let (t, f) ← check "processBlock rejects wrong slot" false
      total := total + t; failures := failures + f
    | .error (.slotMismatch _ _) =>
      let (t, f) ← check "processBlock rejects wrong slot" true
      total := total + t; failures := failures + f
    | .error _ =>
      let (t, f) ← check "processBlock rejects wrong slot (wrong error)" false
      total := total + t; failures := failures + f

  -- Test 6: processBlock rejects invalid parent root
  do
    let s1 := processSlot genesis
    let block : Block :=
      { slot := 1
        proposerIndex := 0
        parentRoot := Bytes32.zero
        stateRoot := Bytes32.zero
        body := { attestations := SszList.empty } }
    match processBlock s1 block with
    | .ok _ =>
      let (t, f) ← check "processBlock rejects invalid parent root" false
      total := total + t; failures := failures + f
    | .error .parentRootMismatch =>
      let (t, f) ← check "processBlock rejects invalid parent root" true
      total := total + t; failures := failures + f
    | .error _ =>
      let (t, f) ← check "processBlock rejects invalid parent root (wrong error)" false
      total := total + t; failures := failures + f

  -- Test 7: processAttestation rejects future attestation
  do
    let s1 := processSlot genesis
    let futureData : AttestationData :=
      { slot := 99, head := genesis.latestJustified
        source := genesis.latestJustified
        target := genesis.latestJustified }
    let futureAtt : AggregatedAttestation :=
      { aggregationBits := Bitlist.empty VALIDATOR_REGISTRY_LIMIT
        data := futureData }
    match processAttestations s1 #[futureAtt] with
    | .error (.attestationSlotTooNew _ _) =>
      let (t, f) ← check "processAttestation rejects future attestation" true
      total := total + t; failures := failures + f
    | _ =>
      let (t, f) ← check "processAttestation rejects future attestation" false
      total := total + t; failures := failures + f

  -- Test 8: processAttestation rejects too-old attestation
  do
    match processSlots genesis 10 with
    | .ok s10 =>
      let oldData : AttestationData :=
        { slot := 1, head := s10.latestJustified
          source := s10.latestJustified
          target := s10.latestJustified }
      let oldAtt : AggregatedAttestation :=
        { aggregationBits := Bitlist.empty VALIDATOR_REGISTRY_LIMIT
          data := oldData }
      match processAttestations s10 #[oldAtt] with
      | .error (.attestationSlotTooOld _ _) =>
        let (t, f) ← check "processAttestation rejects too-old attestation" true
        total := total + t; failures := failures + f
      | _ =>
        let (t, f) ← check "processAttestation rejects too-old attestation" false
        total := total + t; failures := failures + f
    | .error _ =>
      let (t, f) ← check "processAttestation rejects too-old (setup failed)" false
      total := total + t; failures := failures + f

  return (total, failures)

end Test.Consensus.StateTransition
