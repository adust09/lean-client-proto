/-
  Wire Actor Tests — Message Passing Between Actors
-/

import LeanConsensus.Actor
import LeanConsensus.Actor.Messages

namespace Test.Actor.WireActors

open LeanConsensus.Actor
open LeanConsensus.Actor.Messages
open LeanConsensus.Consensus
open LeanConsensus.SSZ

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── Wire Actors ──"
  let mut total := 0
  let mut failures := 0

  -- Test 1: P2PActorMsg construction and pattern matching
  do
    let msg := P2PActorMsg.shutdown
    let isShutdown := match msg with
      | .shutdown => true
      | _ => false
    let (t, f) ← check "P2PActorMsg.shutdown pattern" isShutdown
    total := total + t; failures := failures + f

  -- Test 2: BlockchainActorMsg construction
  do
    let tick : SlotTick := { slot := 42 }
    let msg := BlockchainActorMsg.slotTick tick
    let isSlotTick := match msg with
      | .slotTick t => t.slot == 42
      | _ => false
    let (t, f) ← check "BlockchainActorMsg.slotTick" isSlotTick
    total := total + t; failures := failures + f

  -- Test 3: ValidatorActorMsg construction
  do
    let msg := ValidatorActorMsg.proposeBlock 10
    let isPropose := match msg with
      | .proposeBlock slot => slot == 10
      | _ => false
    let (t, f) ← check "ValidatorActorMsg.proposeBlock" isPropose
    total := total + t; failures := failures + f

  -- Test 4: BlockchainActorMsg.shutdown terminates actor
  do
    let counter ← IO.mkRef (0 : Nat)
    let actor ← spawnActor (msg := BlockchainActorMsg) fun msg => do
      counter.modify (· + 1)
      match msg with
      | .shutdown => return false
      | _ => return true
    send actor (.slotTick { slot := 1 })
    send actor (.slotTick { slot := 2 })
    send actor .shutdown
    IO.sleep 100
    let count ← counter.get
    let (t, f) ← check "blockchain shutdown after 3 messages" (count == 3)
    total := total + t; failures := failures + f
    shutdown actor

  -- Test 5: P2P → Blockchain message forwarding
  do
    let blockCount ← IO.mkRef (0 : Nat)
    let attCount ← IO.mkRef (0 : Nat)

    -- Mock blockchain actor that counts messages
    let blockchain ← spawnActor (msg := BlockchainActorMsg) fun msg => do
      match msg with
      | .newBlock _ => blockCount.modify (· + 1); return true
      | .newAttestation _ => attCount.modify (· + 1); return true
      | .shutdown => return false
      | _ => return true

    -- Mock P2P actor that forwards to blockchain
    let p2pActor ← spawnActor (msg := P2PActorMsg) fun msg => do
      match msg with
      | .networkBlock block =>
        send blockchain (.newBlock block)
        return true
      | .networkAttestation att =>
        send blockchain (.newAttestation att)
        return true
      | .shutdown => return false
      | _ => return true

    -- Create a mock signed block
    let emptyAtts : SszList MAX_ATTESTATIONS SignedAggregatedAttestation :=
      ⟨#[], Nat.zero_le _⟩
    let block : SignedBeaconBlock := {
      block := {
        slot := 1
        proposerIndex := 0
        parentRoot := BytesN.zero 32
        stateRoot := BytesN.zero 32
        body := { attestations := emptyAtts }
      }
      signature := BytesN.zero XMSS_SIGNATURE_SIZE
    }

    -- Create a mock attestation
    let att : SignedAttestation := {
      data := {
        slot := 1
        headRoot := BytesN.zero 32
        sourceCheckpoint := { slot := 0, root := BytesN.zero 32 }
        targetCheckpoint := { slot := 1, root := BytesN.zero 32 }
      }
      validatorIndex := 0
      signature := BytesN.zero XMSS_SIGNATURE_SIZE
    }

    send p2pActor (.networkBlock block)
    send p2pActor (.networkAttestation att)
    IO.sleep 200

    let blocks ← blockCount.get
    let atts ← attCount.get
    let (t, f) ← check "P2P forwards block to blockchain" (blocks == 1)
    total := total + t; failures := failures + f
    let (t, f) ← check "P2P forwards attestation to blockchain" (atts == 1)
    total := total + t; failures := failures + f

    send p2pActor .shutdown
    send blockchain .shutdown
    IO.sleep 100
    shutdown p2pActor
    shutdown blockchain

  -- Test 6: ValidatorActorMsg.blockImported carries root and slot
  do
    let rootRef ← IO.mkRef (ByteArray.empty)
    let slotRef ← IO.mkRef (0 : UInt64)

    let validator ← spawnActor (msg := ValidatorActorMsg) fun msg => do
      match msg with
      | .blockImported root slot =>
        rootRef.set root.data
        slotRef.set slot
        return true
      | .shutdown => return false
      | _ => return true

    let testRoot := BytesN.zero 32
    send validator (.blockImported testRoot 42)
    IO.sleep 100

    let slot ← slotRef.get
    let (t, f) ← check "blockImported carries slot" (slot == 42)
    total := total + t; failures := failures + f

    send validator .shutdown
    IO.sleep 50
    shutdown validator

  -- Test 7: Full pipeline: P2P → Blockchain → Validator
  do
    let validatorNotified ← IO.mkRef false

    let validator ← spawnActor (msg := ValidatorActorMsg) fun msg => do
      match msg with
      | .blockImported _ _ =>
        validatorNotified.set true
        return true
      | .shutdown => return false
      | _ => return true

    let blockchain ← spawnActor (msg := BlockchainActorMsg) fun msg => do
      match msg with
      | .newBlock block =>
        -- Simulate block import notification to validator
        send validator (.blockImported (BytesN.zero 32) block.block.slot)
        return true
      | .shutdown => return false
      | _ => return true

    let p2pActor ← spawnActor (msg := P2PActorMsg) fun msg => do
      match msg with
      | .networkBlock block =>
        send blockchain (.newBlock block)
        return true
      | .shutdown => return false
      | _ => return true

    let emptyAtts : SszList MAX_ATTESTATIONS SignedAggregatedAttestation :=
      ⟨#[], Nat.zero_le _⟩
    let block : SignedBeaconBlock := {
      block := {
        slot := 5
        proposerIndex := 0
        parentRoot := BytesN.zero 32
        stateRoot := BytesN.zero 32
        body := { attestations := emptyAtts }
      }
      signature := BytesN.zero XMSS_SIGNATURE_SIZE
    }

    send p2pActor (.networkBlock block)
    IO.sleep 300

    let notified ← validatorNotified.get
    let (t, f) ← check "full pipeline: P2P → Blockchain → Validator" notified
    total := total + t; failures := failures + f

    send p2pActor .shutdown
    send blockchain .shutdown
    send validator .shutdown
    IO.sleep 100
    shutdown p2pActor
    shutdown blockchain
    shutdown validator

  return (total, failures)

end Test.Actor.WireActors
