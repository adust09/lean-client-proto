/-
  Metrics — IO.Ref Counters/Gauges with Prometheus Text Exposition

  Provides a simple metrics system using IO.Ref for thread-safe counters
  and gauges. Renders metrics in Prometheus text exposition format for
  scraping by monitoring systems.
-/

namespace LeanConsensus.Metrics

-- ════════════════════════════════════════════════════════════════
-- Counter (monotonically increasing)
-- ════════════════════════════════════════════════════════════════

/-- A monotonically increasing counter backed by IO.Ref. -/
structure Counter where
  name : String
  help : String
  ref  : IO.Ref Nat

/-- Increment a counter by a given amount (default 1). -/
def Counter.increment (c : Counter) (n : Nat := 1) : IO Unit :=
  c.ref.modify (· + n)

/-- Get the current value of a counter. -/
def Counter.get (c : Counter) : IO Nat :=
  c.ref.get

-- ════════════════════════════════════════════════════════════════
-- Gauge (arbitrary value)
-- ════════════════════════════════════════════════════════════════

/-- A gauge that can be set to any integer value. -/
structure Gauge where
  name : String
  help : String
  ref  : IO.Ref Int

/-- Set the gauge to a specific value. -/
def Gauge.set (g : Gauge) (v : Int) : IO Unit :=
  g.ref.set v

/-- Get the current value of a gauge. -/
def Gauge.get (g : Gauge) : IO Int :=
  g.ref.get

/-- Increment a gauge by a given amount. -/
def Gauge.increment (g : Gauge) (n : Int := 1) : IO Unit :=
  g.ref.modify (· + n)

-- ════════════════════════════════════════════════════════════════
-- MetricsRegistry
-- ════════════════════════════════════════════════════════════════

/-- Registry holding all counters and gauges. -/
structure MetricsRegistry where
  counters : IO.Ref (Array Counter)
  gauges   : IO.Ref (Array Gauge)

/-- Create an empty metrics registry. -/
def MetricsRegistry.new : IO MetricsRegistry := do
  let counters ← IO.mkRef #[]
  let gauges ← IO.mkRef #[]
  return { counters, gauges }

/-- Register a new counter with the given name and help text. -/
def MetricsRegistry.registerCounter (reg : MetricsRegistry)
    (name : String) (help : String := "") : IO Counter := do
  let ref ← IO.mkRef 0
  let counter : Counter := { name, help, ref }
  reg.counters.modify (·.push counter)
  return counter

/-- Register a new gauge with the given name and help text. -/
def MetricsRegistry.registerGauge (reg : MetricsRegistry)
    (name : String) (help : String := "") : IO Gauge := do
  let ref ← IO.mkRef 0
  let gauge : Gauge := { name, help, ref }
  reg.gauges.modify (·.push gauge)
  return gauge

-- ════════════════════════════════════════════════════════════════
-- Prometheus Text Format Rendering
-- ════════════════════════════════════════════════════════════════

/-- Render all registered metrics in Prometheus text exposition format. -/
def MetricsRegistry.renderPrometheus (reg : MetricsRegistry) : IO String := do
  let mut output := ""
  let counters ← reg.counters.get
  for c in counters do
    let val ← c.get
    if c.help != "" then
      output := output ++ s!"# HELP {c.name} {c.help}\n"
    output := output ++ s!"# TYPE {c.name} counter\n"
    output := output ++ s!"{c.name} {val}\n"
  let gauges ← reg.gauges.get
  for g in gauges do
    let val ← g.get
    if g.help != "" then
      output := output ++ s!"# HELP {g.name} {g.help}\n"
    output := output ++ s!"# TYPE {g.name} gauge\n"
    output := output ++ s!"{g.name} {val}\n"
  return output

-- ════════════════════════════════════════════════════════════════
-- Standard Beacon Metrics (convenience)
-- ════════════════════════════════════════════════════════════════

/-- Standard set of metrics for a beacon consensus client. -/
structure BeaconMetrics where
  blocksImported       : Counter
  attestationsImported : Counter
  blocksProposed       : Counter
  attestationsProduced : Counter
  headSlot             : Gauge
  finalizedSlot        : Gauge

/-- Register all standard beacon metrics on the given registry. -/
def BeaconMetrics.register (reg : MetricsRegistry) : IO BeaconMetrics := do
  let blocksImported ← reg.registerCounter
    "blocks_imported_total" "Total number of blocks imported"
  let attestationsImported ← reg.registerCounter
    "attestations_imported_total" "Total number of attestations imported"
  let blocksProposed ← reg.registerCounter
    "blocks_proposed_total" "Total number of blocks proposed"
  let attestationsProduced ← reg.registerCounter
    "attestations_produced_total" "Total number of attestations produced"
  let headSlot ← reg.registerGauge
    "head_slot" "Current head slot"
  let finalizedSlot ← reg.registerGauge
    "finalized_slot" "Current finalized slot"
  return { blocksImported, attestationsImported, blocksProposed,
           attestationsProduced, headSlot, finalizedSlot }

end LeanConsensus.Metrics
