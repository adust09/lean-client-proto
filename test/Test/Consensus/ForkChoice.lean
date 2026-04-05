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

/-- Create a minimal genesis State with N validators. -/
def mkGenesisState (numValidators : Nat) : State :=
  let validators : Array Validator := Array.range numValidators |>.map fun i =>
    { attestationPubkey := BytesN.zero XMSS_PUBKEY_SIZE
      proposalPubkey := BytesN.zero XMSS_PUBKEY_SIZE
      index := i.toUInt64 }
  let zeroRoot := Bytes32.zero
  let cfg : Config := { genesisTime := 0 }
  let zeroCheckpoint : Checkpoint := { root := zeroRoot, slot := 0 }
  let genesisHeader : BeaconBlockHeader :=
    { slot := 0, proposerIndex := 0
      parentRoot := zeroRoot, stateRoot := zeroRoot, bodyRoot := zeroRoot }
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

/-- Create genesis state and block with properly matched roots. -/
def mkGenesis : State × Block :=
  let genesisBody : BlockBody := { attestations := SszList.empty }
  let bodyRoot := StateTransition.toBytes32 (SszHashTreeRoot.hashTreeRoot genesisBody)
  let state := mkGenesisState 4
  let state := { state with latestBlockHeader :=
    { slot := 0, proposerIndex := 0
      parentRoot := Bytes32.zero
      stateRoot := Bytes32.zero
      bodyRoot := bodyRoot } }
  let genesisStateRoot := StateTransition.toBytes32 (SszHashTreeRoot.hashTreeRoot state)
  let genesisBlock : Block :=
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

  -- Test 1: fromAnchor creates valid store
  do
    let store := fromAnchor genesis genesisBlock
    let root := blockRoot genesisBlock
    let hasBlock := store.blocks.contains root
    let hasState := store.states.contains root
    let (t, f) ← check "fromAnchor has genesis block" hasBlock
    total := total + t; failures := failures + f
    let (t, f) ← check "fromAnchor has genesis state" hasState
    total := total + t; failures := failures + f

  -- Test 2: onBlock adds block and updates store
  do
    let store := fromAnchor genesis genesisBlock
    let genesisRoot := blockRoot genesisBlock
    let block1 : Block :=
      { slot := 1, proposerIndex := 1
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
    let store := fromAnchor genesis genesisBlock
    let head := getHead store
    let genesisRoot := blockRoot genesisBlock
    let (t, f) ← check "getHead returns genesis root" (head == genesisRoot)
    total := total + t; failures := failures + f

  -- Test 4: onBlock rejects unknown parent
  do
    let store := fromAnchor genesis genesisBlock
    let badBlock : Block :=
      { slot := 1, proposerIndex := 0
        parentRoot := BytesN.zero 32
        stateRoot := Bytes32.zero
        body := { attestations := SszList.empty } }
    let genesisRoot := blockRoot genesisBlock
    if BytesN.zero 32 == genesisRoot then
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
    let store := fromAnchor genesis genesisBlock
    match onBlock store genesisBlock with
    | .error (.blockAlreadyKnown _) =>
      let (t, f) ← check "onBlock rejects duplicate" true
      total := total + t; failures := failures + f
    | _ =>
      let (t, f) ← check "onBlock rejects duplicate" false
      total := total + t; failures := failures + f

  -- Test 6: onGossipAttestation stores attestation
  do
    let store := fromAnchor genesis genesisBlock
    let genesisRoot := blockRoot genesisBlock
    let att : SignedAttestation :=
      { data := { slot := 0
                  head := { root := genesisRoot, slot := 0 }
                  source := store.latestJustified
                  target := store.latestJustified }
        validatorIndex := 0
        signature := BytesN.zero XMSS_SIGNATURE_SIZE }
    match onGossipAttestation store att with
    | .ok store2 =>
      let sigs := store2.attestationSignatures.getD genesisRoot #[]
      let (t, f) ← check "onGossipAttestation stores signature" (sigs.size == 1)
      total := total + t; failures := failures + f
    | .error e =>
      let (t, f) ← check s!"onGossipAttestation (error: {e})" false
      total := total + t; failures := failures + f

  -- Test 7: onTick at slot boundary promotes payloads
  do
    let store := fromAnchor genesis genesisBlock
    let store2 := onTick store (INTERVALS_PER_SLOT.toUInt64)
    let (t, f) ← check "onTick advances time" (store2.time == INTERVALS_PER_SLOT.toUInt64)
    total := total + t; failures := failures + f

  -- Test 8: intervalToSlot conversion
  do
    let slot := intervalToSlot (15 : UInt64)
    let (t, f) ← check "intervalToSlot(15) = 3" (slot == 3)
    total := total + t; failures := failures + f

  return (total, failures)

end Test.Consensus.ForkChoice
