/-
  Fixture discovery and loading for leanSpec test vectors.

  Uses `find` command to discover fixture files, avoiding
  direct filesystem API compatibility issues.
-/

import Lean.Data.Json
import Test.LeanSpec.Types

namespace Test.LeanSpec.Loader

open Lean (Json FromJson)

/-- Path to the leanSpec fixtures directory, relative to project root. -/
def fixturesDir : System.FilePath := "leanSpec/fixtures/consensus"

/-- Check whether fixtures have been generated. -/
def fixturesAvailable : IO Bool := do
  let doesExist ← fixturesDir.pathExists
  if !doesExist then return false
  fixturesDir.isDir

/-- Discover all .json files under a directory using the find command. -/
def discoverFixtures (dir : System.FilePath) : IO (Array System.FilePath) := do
  let doesExist ← dir.pathExists
  if !doesExist then return #[]
  let result ← IO.Process.output {
    cmd := "find"
    args := #[dir.toString, "-name", "*.json", "-type", "f"]
  }
  if result.exitCode != 0 then return #[]
  let lines := result.stdout.splitOn "\n"
  let paths := lines.filterMap fun (line : String) =>
    if line.isEmpty then none else some (System.FilePath.mk line)
  return paths.toArray

/-- Extract the first value from a JSON object (for envelope unwrapping). -/
private def firstObjValue (json : Json) : Option Json :=
  match json with
  | .obj kvs =>
    let keys := kvs.toList.map (·.1)
    match keys with
    | [] => none
    | k :: _ => match json.getObjVal? k with
      | .ok v => some v
      | .error _ => none
  | _ => none

/-- Load a JSON file and parse it. Returns the first fixture value from the envelope. -/
def loadFixture {α : Type} [FromJson α] (path : System.FilePath) : IO (Except String α) := do
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error e => return .error s!"JSON parse error in {path}: {e}"
  | .ok json =>
    match firstObjValue json with
    | none => return .error s!"empty fixture envelope in {path}"
    | some fixtureJson =>
      match @FromJson.fromJson? α _ fixtureJson with
      | .error e => return .error s!"fixture parse error in {path}: {e}"
      | .ok fixture => return .ok fixture

end Test.LeanSpec.Loader
