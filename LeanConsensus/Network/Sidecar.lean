/-
  Sidecar — Beacon Node Sidecar Process Manager

  Manages the lifecycle of a companion consensus client (Lighthouse/Nimbus)
  running as a subprocess. Provides:
  - Spawn with configurable args
  - Readiness polling via health endpoint
  - Graceful shutdown via stdin pipe close
  - Background health monitoring watchdog
-/

import LeanConsensus.Network.BeaconAPI

namespace LeanConsensus.Network.Sidecar

open LeanConsensus.Network.BeaconAPI
open LeanConsensus.Network.HttpClient

-- ════════════════════════════════════════════════════════════════
-- Configuration
-- ════════════════════════════════════════════════════════════════

/-- Configuration for the sidecar beacon node process. -/
structure SidecarConfig where
  binary  : String           -- Path to the beacon node binary (e.g., "lighthouse")
  args    : Array String     -- Command-line arguments
  dataDir : String           -- Data directory for the sidecar
  apiPort : UInt16 := 5052   -- REST API port
  network : String := "pq-devnet-3"

instance : ToString SidecarConfig where
  toString c := s!"SidecarConfig(binary={c.binary}, port={c.apiPort})"

-- ════════════════════════════════════════════════════════════════
-- Handle
-- ════════════════════════════════════════════════════════════════

/-- Handle to a running sidecar process. -/
structure SidecarHandle where
  child     : IO.Process.Child ⟨.piped, .piped, .piped⟩
  apiClient : BeaconAPIClient
  healthy   : IO.Ref Bool
  config    : SidecarConfig

-- ════════════════════════════════════════════════════════════════
-- Spawning
-- ════════════════════════════════════════════════════════════════

/-- Build the full argument list for the sidecar process. -/
private def buildArgs (config : SidecarConfig) : Array String :=
  config.args ++ #[
    "--datadir", config.dataDir,
    "--http",
    "--http-port", toString config.apiPort,
    "--http-allow-origin", "*"
  ]

/-- Spawn the sidecar beacon node as a child process.
    Does NOT wait for readiness — call `waitReady` after spawning. -/
def spawnSidecar (config : SidecarConfig) : IO SidecarHandle := do
  let args := buildArgs config
  let child ← IO.Process.spawn {
    cmd := config.binary
    args := args
    stdout := .piped
    stderr := .piped
    stdin := .piped
  }
  let apiConfig : BeaconAPIConfig := {
    host := "127.0.0.1"
    port := config.apiPort
  }
  let apiClient := mkClient apiConfig
  let healthy ← IO.mkRef false
  return { child, apiClient, healthy, config }

-- ════════════════════════════════════════════════════════════════
-- Readiness
-- ════════════════════════════════════════════════════════════════

/-- Poll the health endpoint until the sidecar is ready or timeout expires.
    Returns true if the sidecar became ready within the timeout.
    `timeoutMs` is in milliseconds, polled every 500ms. -/
partial def waitReady (handle : SidecarHandle) (timeoutMs : UInt32 := 30000) : IO Bool := do
  let iterations := timeoutMs / 500
  for _ in [:iterations.toNat] do
    let isHealthy ← checkHealth handle.apiClient
    if isHealthy then
      handle.healthy.set true
      return true
    IO.sleep 500
  return false

-- ════════════════════════════════════════════════════════════════
-- Shutdown
-- ════════════════════════════════════════════════════════════════

/-- Gracefully shut down the sidecar by closing its stdin pipe.
    This signals the child process to terminate.
    If the process doesn't exit cleanly, falls back to kill via subprocess. -/
def shutdownSidecar (handle : SidecarHandle) : IO Unit := do
  handle.healthy.set false
  -- Close stdin to signal the child to exit
  try
    handle.child.stdin.flush
    -- Note: IO.FS.Handle doesn't have an explicit close in Lean 4,
    -- but dropping the handle will close the fd.
    -- Send SIGTERM via kill command as fallback
    let _ ← IO.Process.output {
      cmd := "kill"
      args := #["-TERM", toString handle.child.pid]
    }
  catch _ => pure ()
  -- Wait briefly for cleanup
  IO.sleep 1000
  -- Force kill if still running
  try
    let _ ← IO.Process.output {
      cmd := "kill"
      args := #["-9", toString handle.child.pid]
    }
  catch _ => pure ()

-- ════════════════════════════════════════════════════════════════
-- Health Monitoring
-- ════════════════════════════════════════════════════════════════

/-- Spawn a background watchdog task that periodically checks sidecar health.
    Updates the `healthy` ref. Runs until the handle is shut down. -/
partial def monitorSidecar (handle : SidecarHandle)
    (intervalMs : UInt32 := 5000) : IO (Task (Except IO.Error Unit)) := do
  IO.asTask (prio := .default) do
    while true do
      IO.sleep intervalMs
      let isHealthy ← checkHealth handle.apiClient
      let wasHealthy ← handle.healthy.get
      handle.healthy.set isHealthy
      if wasHealthy && !isHealthy then
        IO.eprintln s!"[sidecar] health check failed for {handle.config.binary}"
      if !wasHealthy && isHealthy then
        IO.eprintln s!"[sidecar] {handle.config.binary} recovered"

/-- Check if the sidecar is currently healthy. -/
def isHealthy (handle : SidecarHandle) : IO Bool :=
  handle.healthy.get

end LeanConsensus.Network.Sidecar
