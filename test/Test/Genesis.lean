/-
  Tests for Genesis — JSON parsing and state construction
-/

import LeanConsensus.Genesis
import Lean.Data.Json

open LeanConsensus.Genesis
open LeanConsensus.Consensus
open Lean (Json ToJson FromJson)

namespace Test.Genesis

private def check (name : String) (cond : Bool) : IO (Nat × Nat) := do
  if cond then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "── Genesis tests ──"
  let mut total := 0
  let mut failures := 0

  -- GenesisConfig JSON roundtrip
  let config : GenesisConfig := {
    genesisTime := 1700000000
    forkVersion := "0x00000001"
    validatorCount := 4
    balancePerValidator := 32000000000
  }
  let json := ToJson.toJson config
  match @FromJson.fromJson? GenesisConfig _ json with
  | .ok config' =>
    let (t, f) ← check "GenesisConfig JSON roundtrip"
      (config'.genesisTime == 1700000000 && config'.validatorCount == 4)
    total := total + t; failures := failures + f
  | .error e =>
    let (t, f) ← check s!"GenesisConfig JSON roundtrip (error: {e})" false
    total := total + t; failures := failures + f

  -- ValidatorDeposit JSON roundtrip
  let deposit : ValidatorDeposit := { pubkey := "0xaabb", balance := 32000000000 }
  let json := ToJson.toJson deposit
  match @FromJson.fromJson? ValidatorDeposit _ json with
  | .ok d' =>
    let (t, f) ← check "ValidatorDeposit JSON roundtrip"
      (d'.pubkey == "0xaabb" && d'.balance == 32000000000)
    total := total + t; failures := failures + f
  | .error e =>
    let (t, f) ← check s!"ValidatorDeposit JSON roundtrip (error: {e})" false
    total := total + t; failures := failures + f

  -- Build genesis state with small validator set
  let genesis : GenesisFile := {
    config := { genesisTime := 0, forkVersion := "0x00", validatorCount := 4,
                balancePerValidator := 32000000000 }
    validators := #[
      { pubkey := "0x01", balance := 32000000000 },
      { pubkey := "0x02", balance := 32000000000 },
      { pubkey := "0x03", balance := 32000000000 },
      { pubkey := "0x04", balance := 32000000000 }
    ]
  }
  match buildGenesisState genesis with
  | .ok state =>
    let (t, f) ← check "buildGenesisState succeeds with 4 validators"
      (state.validators.elems.size == 4)
    total := total + t; failures := failures + f

    let (t, f) ← check "genesis state slot is 0" (state.slot == 0)
    total := total + t; failures := failures + f

    let (t, f) ← check "genesis state balances match"
      (state.balances.elems.size == 4)
    total := total + t; failures := failures + f
  | .error e =>
    let (t, f) ← check s!"buildGenesisState (error: {e})" false
    total := total + t; failures := failures + f
    let (t, f) ← check "skipped" false
    total := total + t; failures := failures + f
    let (t, f) ← check "skipped" false
    total := total + t; failures := failures + f

  -- Build genesis block
  let block := buildGenesisBlock
  let (t, f) ← check "buildGenesisBlock slot is 0" (block.slot == 0)
  total := total + t; failures := failures + f

  -- Genesis file loading from temp file
  let tmpPath := System.FilePath.mk "/tmp/lean-consensus-test-genesis.json"
  let genesisJson := ToJson.toJson genesis
  IO.FS.writeFile tmpPath (ToString.toString genesisJson)
  let loaded ← loadGenesisFile tmpPath
  let (t, f) ← check "loadGenesisFile parses correctly"
    (loaded.validators.size == 4 && loaded.config.validatorCount == 4)
  total := total + t; failures := failures + f

  try IO.FS.removeFile tmpPath catch | _ => pure ()

  return (total, failures)

end Test.Genesis
