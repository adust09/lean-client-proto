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

/-- Create a minimal genesis BeaconState with N validators. -/
def mkGenesisState (numValidators : Nat) : BeaconState :=
  let validators : Array Validator := Array.range numValidators |>.map fun _ =>
    { pubkey := BytesN.zero XMSS_PUBKEY_SIZE
      effectiveBalance := 32000000000
      slashed := false
      activationSlot := 0
      exitSlot := 0xFFFFFFFFFFFFFFFF
      withdrawableSlot := 0xFFFFFFFFFFFFFFFF }
  let balances : Array Gwei := Array.range numValidators |>.map fun _ => (32000000000 : UInt64)
  let zeroRoot := Bytes32.zero
  let genesisHeader : BeaconBlockHeader :=
    { slot := 0
      proposerIndex := 0
      parentRoot := zeroRoot
      stateRoot := zeroRoot
      bodyRoot := zeroRoot }
  let blockRoots := SszVector.mkChecked (n := SLOTS_PER_HISTORICAL_ROOT)
    (Array.replicate SLOTS_PER_HISTORICAL_ROOT zeroRoot)
  let stateRoots := SszVector.mkChecked (n := SLOTS_PER_HISTORICAL_ROOT)
    (Array.replicate SLOTS_PER_HISTORICAL_ROOT zeroRoot)
  let validatorsList := SszList.mkChecked (maxCap := VALIDATOR_REGISTRY_LIMIT) validators
  let balancesList := SszList.mkChecked (maxCap := VALIDATOR_REGISTRY_LIMIT) balances
  match blockRoots, stateRoots, validatorsList, balancesList with
  | .ok br, .ok sr, .ok vs, .ok bs =>
    { slot := 0
      latestBlockHeader := genesisHeader
      blockRoots := br
      stateRoots := sr
      validators := vs
      balances := bs
      justifiedCheckpoint := { slot := 0, root := zeroRoot }
      finalizedCheckpoint := { slot := 0, root := zeroRoot }
      currentAttestations := SszList.empty }
  | _, _, _, _ =>
    -- Unreachable with correct constants
    { slot := 0
      latestBlockHeader := genesisHeader
      blockRoots := ⟨Array.replicate SLOTS_PER_HISTORICAL_ROOT zeroRoot, by simp [Array.size_replicate]⟩
      stateRoots := ⟨Array.replicate SLOTS_PER_HISTORICAL_ROOT zeroRoot, by simp [Array.size_replicate]⟩
      validators := SszList.empty
      balances := SszList.empty
      justifiedCheckpoint := { slot := 0, root := zeroRoot }
      finalizedCheckpoint := { slot := 0, root := zeroRoot }
      currentAttestations := SszList.empty }

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
    -- Advance to slot 1 first
    let s1 := processSlot genesis
    -- Create a valid block for slot 1
    let parentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot s1.latestBlockHeader)
    let block : BeaconBlock :=
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
    let block : BeaconBlock :=
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
    let block : BeaconBlock :=
      { slot := 1
        proposerIndex := 0
        parentRoot := Bytes32.zero  -- wrong parent
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

  -- Test 7: processBlock rejects slashed proposer
  do
    -- Create state with slashed validator at index 0
    let slashedVal : Validator :=
      { pubkey := BytesN.zero XMSS_PUBKEY_SIZE
        effectiveBalance := 32000000000
        slashed := true
        activationSlot := 0
        exitSlot := 0xFFFFFFFFFFFFFFFF
        withdrawableSlot := 0xFFFFFFFFFFFFFFFF }
    let vals := #[slashedVal]
    match SszList.mkChecked (maxCap := VALIDATOR_REGISTRY_LIMIT) vals with
    | .ok vsList =>
      let slashedState := { genesis with validators := vsList }
      let s1 := processSlot slashedState
      let parentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot s1.latestBlockHeader)
      let block : BeaconBlock :=
        { slot := 1
          proposerIndex := 0
          parentRoot := parentRoot
          stateRoot := Bytes32.zero
          body := { attestations := SszList.empty } }
      match processBlock s1 block with
      | .error (.proposerSlashed _) =>
        let (t, f) ← check "processBlock rejects slashed proposer" true
        total := total + t; failures := failures + f
      | _ =>
        let (t, f) ← check "processBlock rejects slashed proposer" false
        total := total + t; failures := failures + f
    | .error _ =>
      let (t, f) ← check "processBlock rejects slashed proposer (setup failed)" false
      total := total + t; failures := failures + f

  -- Test 8: processAttestation rejects future attestation
  do
    let s1 := processSlot genesis
    let futureData : AttestationData :=
      { slot := 99, headRoot := Bytes32.zero
        sourceCheckpoint := genesis.justifiedCheckpoint
        targetCheckpoint := genesis.justifiedCheckpoint }
    let futureAtt : SignedAggregatedAttestation :=
      { data := futureData
        aggregationBits := Bitlist.empty MAX_VALIDATORS_PER_SUBNET
        aggregationProof := ⟨ByteArray.empty⟩ }
    match processAttestation s1 futureAtt with
    | .error (.attestationSlotTooNew _ _) =>
      let (t, f) ← check "processAttestation rejects future attestation" true
      total := total + t; failures := failures + f
    | _ =>
      let (t, f) ← check "processAttestation rejects future attestation" false
      total := total + t; failures := failures + f

  -- Test 9: processAttestation rejects too-old attestation
  do
    match processSlots genesis 10 with
    | .ok s10 =>
      let oldData : AttestationData :=
        { slot := 1, headRoot := Bytes32.zero
          sourceCheckpoint := s10.justifiedCheckpoint
          targetCheckpoint := s10.justifiedCheckpoint }
      let oldAtt : SignedAggregatedAttestation :=
        { data := oldData
          aggregationBits := Bitlist.empty MAX_VALIDATORS_PER_SUBNET
          aggregationProof := ⟨ByteArray.empty⟩ }
      match processAttestation s10 oldAtt with
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
