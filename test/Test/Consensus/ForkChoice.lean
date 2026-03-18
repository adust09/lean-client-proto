/-
  Fork Choice Tests
-/

import LeanConsensus.Consensus.ForkChoice
import LeanConsensus.Consensus.Constants

namespace Test.Consensus.ForkChoice

open LeanConsensus.SSZ
open LeanConsensus.Consensus
open LeanConsensus.Consensus.ForkChoice
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
    { slot := 0, proposerIndex := 0
      parentRoot := zeroRoot, stateRoot := zeroRoot, bodyRoot := zeroRoot }
  match SszVector.mkChecked (n := SLOTS_PER_HISTORICAL_ROOT)
          (Array.replicate SLOTS_PER_HISTORICAL_ROOT zeroRoot),
        SszVector.mkChecked (n := SLOTS_PER_HISTORICAL_ROOT)
          (Array.replicate SLOTS_PER_HISTORICAL_ROOT zeroRoot),
        SszList.mkChecked (maxCap := VALIDATOR_REGISTRY_LIMIT) validators,
        SszList.mkChecked (maxCap := VALIDATOR_REGISTRY_LIMIT) balances with
  | .ok br, .ok sr, .ok vs, .ok bs =>
    { slot := 0
      latestBlockHeader := genesisHeader
      blockRoots := br, stateRoots := sr
      validators := vs, balances := bs
      justifiedCheckpoint := { slot := 0, root := zeroRoot }
      finalizedCheckpoint := { slot := 0, root := zeroRoot }
      currentAttestations := SszList.empty }
  | _, _, _, _ =>
    { slot := 0
      latestBlockHeader := genesisHeader
      blockRoots := ⟨Array.replicate SLOTS_PER_HISTORICAL_ROOT zeroRoot, by simp [Array.size_replicate]⟩
      stateRoots := ⟨Array.replicate SLOTS_PER_HISTORICAL_ROOT zeroRoot, by simp [Array.size_replicate]⟩
      validators := SszList.empty, balances := SszList.empty
      justifiedCheckpoint := { slot := 0, root := zeroRoot }
      finalizedCheckpoint := { slot := 0, root := zeroRoot }
      currentAttestations := SszList.empty }

/-- Create genesis state and block with properly matched roots.
    Genesis state has latestBlockHeader.stateRoot = 0 (filled by processSlot).
    Genesis block has stateRoot = hashTreeRoot(genesis_state), so that after
    processSlot fills the header's stateRoot, hashTreeRoot(header) == hashTreeRoot(block). -/
def mkGenesis : BeaconState × BeaconBlock :=
  let genesisBody : BeaconBlockBody := { attestations := SszList.empty }
  let bodyRoot := StateTransition.toBytes32 (SszHashTreeRoot.hashTreeRoot genesisBody)
  let state := mkGenesisState 4
  -- Set header with bodyRoot matching the body, stateRoot = 0 (to be filled by processSlot)
  let state := { state with latestBlockHeader :=
    { slot := 0, proposerIndex := 0
      parentRoot := Bytes32.zero
      stateRoot := Bytes32.zero
      bodyRoot := bodyRoot } }
  -- Genesis block's stateRoot = hashTreeRoot(genesis_state)
  let genesisStateRoot := StateTransition.toBytes32 (SszHashTreeRoot.hashTreeRoot state)
  let genesisBlock : BeaconBlock :=
    { slot := 0, proposerIndex := 0
      parentRoot := Bytes32.zero
      stateRoot := genesisStateRoot
      body := genesisBody }
  (state, genesisBlock)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── Fork Choice ──"
  let mut total := 0
  let mut failures := 0

  let (genesis, genesisBlock) := mkGenesis

  -- Test 1: initStore creates valid store
  do
    let store := initStore genesis genesisBlock
    let root := blockRoot genesisBlock
    let hasBlock := store.blocks.contains root
    let hasState := store.blockStates.contains root
    let (t, f) ← check "initStore has genesis block" hasBlock
    total := total + t; failures := failures + f
    let (t, f) ← check "initStore has genesis state" hasState
    total := total + t; failures := failures + f

  -- Test 2: onBlock adds block and updates store
  do
    let store := initStore genesis genesisBlock
    let genesisRoot := blockRoot genesisBlock
    -- onBlock: looks up parentRoot in blockStates, runs processSlots + processBlock.
    -- parentRoot must be genesis root (a key in blockStates).
    -- processBlock checks parentRoot == hashTreeRoot(state.latestBlockHeader)
    -- after processSlots. For this to work, genesis state's latestBlockHeader
    -- must have bodyRoot = hashTreeRoot(genesis body), which mkGenesisStateForForkChoice ensures.
    let block1 : BeaconBlock :=
      { slot := 1, proposerIndex := 0
        parentRoot := genesisRoot
        stateRoot := Bytes32.zero
        body := { attestations := SszList.empty } }
    match onBlock store block1 with
    | .ok store2 =>
      let root1 := blockRoot block1
      let (t, f) ← check "onBlock adds block" (store2.blocks.contains root1)
      total := total + t; failures := failures + f
    | .error e =>
      let (t, f) ← check s!"onBlock adds block (error: {e})" false
      total := total + t; failures := failures + f

  -- Test 3: getHead returns genesis with no attestations
  do
    let store := initStore genesis genesisBlock
    let head := getHead store
    let genesisRoot := blockRoot genesisBlock
    let (t, f) ← check "getHead returns genesis root" (head == genesisRoot)
    total := total + t; failures := failures + f

  -- Test 4: onBlock rejects unknown parent
  do
    let store := initStore genesis genesisBlock
    let badBlock : BeaconBlock :=
      { slot := 1, proposerIndex := 0
        parentRoot := BytesN.zero 32  -- doesn't exist in store (unless it happens to match)
        stateRoot := Bytes32.zero
        body := { attestations := SszList.empty } }
    -- This might match genesis root if genesis root is also zero; check carefully
    let genesisRoot := blockRoot genesisBlock
    if BytesN.zero 32 == genesisRoot then
      -- Skip this test if zero root matches genesis
      let (t, f) ← check "onBlock rejects unknown parent (skip: zero = genesis)" true
      total := total + t; failures := failures + f
    else
      match onBlock store badBlock with
      | .error (.unknownParent _) =>
        let (t, f) ← check "onBlock rejects unknown parent" true
        total := total + t; failures := failures + f
      | _ =>
        let (t, f) ← check "onBlock rejects unknown parent" false
        total := total + t; failures := failures + f

  -- Test 5: onBlock rejects duplicate block
  do
    let store := initStore genesis genesisBlock
    match onBlock store genesisBlock with
    | .error (.blockAlreadyKnown _) =>
      let (t, f) ← check "onBlock rejects duplicate" true
      total := total + t; failures := failures + f
    | _ =>
      let (t, f) ← check "onBlock rejects duplicate" false
      total := total + t; failures := failures + f

  -- Test 6: onAttestation equivocation detection
  do
    let store := initStore genesis genesisBlock
    let genesisRoot := blockRoot genesisBlock
    let att1 : AttestationData :=
      { slot := 0, headRoot := genesisRoot
        sourceCheckpoint := store.justifiedCheckpoint
        targetCheckpoint := store.justifiedCheckpoint }
    match onAttestation store 0 att1 with
    | .ok store2 =>
      -- Now submit a different attestation for the same slot
      let att2 : AttestationData :=
        { slot := 0, headRoot := Bytes32.zero
          sourceCheckpoint := store.justifiedCheckpoint
          targetCheckpoint := store.justifiedCheckpoint }
      match onAttestation store2 0 att2 with
      | .error (.equivocation _) =>
        let (t, f) ← check "equivocation detection" true
        total := total + t; failures := failures + f
      | _ =>
        let (t, f) ← check "equivocation detection" false
        total := total + t; failures := failures + f
    | .error _ =>
      let (t, f) ← check "equivocation setup failed" false
      total := total + t; failures := failures + f

  return (total, failures)

end Test.Consensus.ForkChoice
