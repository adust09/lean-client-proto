import Lake
open Lake DSL

package «lean-consensus» where
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]
  moreLinkArgs := #[
    "-L./ffi",
    "-llean_sha256",
    "-llean_sig",
    "-llean_sig_ffi",
    "-L/usr/lib/x86_64-linux-gnu",
    "-lssl",
    "-lcrypto"
  ]

@[default_target]
lean_lib «LeanConsensus» where
  srcDir := "."

lean_exe «lean-consensus» where
  root := `Main
  supportInterpreter := true

-- Test library (depends on LeanConsensus)
lean_lib «LeanConsensusTest» where
  srcDir := "test"
  globs := #[.submodules `Test, .one `TestMain]

lean_exe «test-runner» where
  srcDir := "test"
  root := `TestMain

-- Proof library (compiles theorem stubs)
lean_lib «Proofs» where
  srcDir := "proofs"
  roots := #[`SSZ.Roundtrip, `Consensus.Safety]
