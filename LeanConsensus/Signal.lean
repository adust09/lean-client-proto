/-
  Signal — SIGINT/SIGTERM handler via C FFI

  Installs a signal handler that sets a global flag, which can be
  polled from the main loop to trigger graceful shutdown.
-/

namespace LeanConsensus.Signal

/-- Install SIGINT and SIGTERM handlers. Call once at startup. -/
@[extern "lean_signal_install"]
opaque installSignalHandler : IO Unit

/-- Check if a shutdown signal has been received. -/
@[extern "lean_signal_is_set"]
opaque isShutdownRequested : IO Bool

end LeanConsensus.Signal
