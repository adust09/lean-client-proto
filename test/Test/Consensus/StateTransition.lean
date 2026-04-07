/-
  State Transition Tests (leanSpec-aligned)
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
        proposerIndex := (1 % 4 : UInt64)
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
        proposerIndex := (1 % 4 : UInt64)
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

  -- Test 7: isJustifiableAfter — immediate window (0-5)
  do
    let ok := isJustifiableAfter 0 0 && isJustifiableAfter 5 0
        && isJustifiableAfter 3 0 && isJustifiableAfter 1 0
    let (t, f) ← check "isJustifiableAfter immediate window 0-5" ok
    total := total + t; failures := failures + f

  -- Test 8: isJustifiableAfter — perfect squares
  do
    let ok := isJustifiableAfter 9 0 && isJustifiableAfter 16 0
        && isJustifiableAfter 25 0
    let (t, f) ← check "isJustifiableAfter perfect squares (9,16,25)" ok
    total := total + t; failures := failures + f

  -- Test 9: isJustifiableAfter — pronic numbers
  do
    let ok := isJustifiableAfter 6 0 && isJustifiableAfter 12 0
        && isJustifiableAfter 20 0
    let (t, f) ← check "isJustifiableAfter pronic numbers (6,12,20)" ok
    total := total + t; failures := failures + f

  -- Test 10: isJustifiableAfter — non-justifiable slots
  do
    let ok := !isJustifiableAfter 7 0 && !isJustifiableAfter 8 0
        && !isJustifiableAfter 10 0 && !isJustifiableAfter 11 0
    let (t, f) ← check "isJustifiableAfter rejects 7,8,10,11" ok
    total := total + t; failures := failures + f

  -- Test 11: justifiedIndexAfter
  do
    let ok1 := justifiedIndexAfter 0 0 == none
    let ok2 := justifiedIndexAfter 5 5 == none
    let ok3 := justifiedIndexAfter 3 0 == some 2
    let ok4 := justifiedIndexAfter 1 0 == some 0
    let ok5 := justifiedIndexAfter 10 5 == some 4
    let (t, f) ← check "justifiedIndexAfter" (ok1 && ok2 && ok3 && ok4 && ok5)
    total := total + t; failures := failures + f

  -- Test 12: Genesis parent detection — first block sets justified/finalized roots
  do
    let s1 := processSlot genesis
    let parentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot s1.latestBlockHeader)
    let block : Block :=
      { slot := 1
        proposerIndex := (1 % 4 : UInt64)
        parentRoot := parentRoot
        stateRoot := Bytes32.zero
        body := { attestations := SszList.empty } }
    match processBlockHeader s1 block with
    | .ok s2 =>
      let justifiedRootSet := s2.latestJustified.root == parentRoot
      let finalizedRootSet := s2.latestFinalized.root == parentRoot
      let (t, f) ← check "genesis parent sets justified/finalized roots"
        (justifiedRootSet && finalizedRootSet)
      total := total + t; failures := failures + f
    | .error e =>
      let (t, f) ← check s!"genesis parent detection (error: {e})" false
      total := total + t; failures := failures + f

  -- Test 13: Gap filling — skipped slots produce zero hashes
  do
    match processSlots genesis 3 with
    | .ok s3 =>
      let parentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot s3.latestBlockHeader)
      let block : Block :=
        { slot := 3
          proposerIndex := (3 % 4 : UInt64)
          parentRoot := parentRoot
          stateRoot := Bytes32.zero
          body := { attestations := SszList.empty } }
      match processBlockHeader s3 block with
      | .ok s4 =>
        -- Genesis header slot=0, block slot=3: gap = 3-0-1 = 2 empty slots
        -- historicalBlockHashes should have: [parentRoot, zeroRoot, zeroRoot]
        let hasHistory := s4.historicalBlockHashes.elems.size == 3
        let firstIsParent := s4.historicalBlockHashes.elems.size > 0 &&
          s4.historicalBlockHashes.elems[0]! == parentRoot
        let secondIsZero := s4.historicalBlockHashes.elems.size > 1 &&
          isZeroRoot s4.historicalBlockHashes.elems[1]!
        let thirdIsZero := s4.historicalBlockHashes.elems.size > 2 &&
          isZeroRoot s4.historicalBlockHashes.elems[2]!
        let (t, f) ← check "gap filling with zero hashes"
          (hasHistory && firstIsParent && secondIsZero && thirdIsZero)
        total := total + t; failures := failures + f
      | .error e =>
        let (t, f) ← check s!"gap filling (error: {e})" false
        total := total + t; failures := failures + f
    | .error e =>
      let (t, f) ← check s!"gap filling setup (error: {e})" false
      total := total + t; failures := failures + f

  -- Test 14: Soft-skip attestations — invalid attestations leave state unchanged
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
    | .ok s2 =>
      let unchanged := s2.latestJustified == s1.latestJustified
        && s2.latestFinalized == s1.latestFinalized
      let (t, f) ← check "soft-skip invalid attestations" unchanged
      total := total + t; failures := failures + f
    | .error e =>
      let (t, f) ← check s!"soft-skip attestations (unexpected error: {e})" false
      total := total + t; failures := failures + f

  -- Test 15: Out-of-bounds attestation references are silently skipped
  do
    let s1 := processSlot genesis
    let oobCheckpoint : Checkpoint := { root := Bytes32.zero, slot := 9999 }
    let oobData : AttestationData :=
      { slot := 1, head := oobCheckpoint
        source := oobCheckpoint
        target := oobCheckpoint }
    let oobAtt : AggregatedAttestation :=
      { aggregationBits := Bitlist.empty VALIDATOR_REGISTRY_LIMIT
        data := oobData }
    match processAttestations s1 #[oobAtt] with
    | .ok _ =>
      let (t, f) ← check "out-of-bounds attestation silently skipped" true
      total := total + t; failures := failures + f
    | .error e =>
      let (t, f) ← check s!"out-of-bounds attestation (unexpected error: {e})" false
      total := total + t; failures := failures + f

  -- Test 16: processBlock rejects incorrect proposer
  do
    let s1 := processSlot genesis
    let parentRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot s1.latestBlockHeader)
    let block : Block :=
      { slot := 1
        proposerIndex := 3
        parentRoot := parentRoot
        stateRoot := Bytes32.zero
        body := { attestations := SszList.empty } }
    match processBlock s1 block with
    | .ok _ =>
      let (t, f) ← check "processBlock rejects incorrect proposer" false
      total := total + t; failures := failures + f
    | .error (.incorrectProposer _ _) =>
      let (t, f) ← check "processBlock rejects incorrect proposer" true
      total := total + t; failures := failures + f
    | .error _ =>
      let (t, f) ← check "processBlock rejects incorrect proposer (wrong error)" false
      total := total + t; failures := failures + f

  return (total, failures)

end Test.Consensus.StateTransition
