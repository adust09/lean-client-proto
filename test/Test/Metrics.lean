/-
  Tests for Metrics — counter, gauge, and Prometheus rendering
-/

import LeanConsensus.Metrics

open LeanConsensus.Metrics

namespace Test.Metrics

/-- Check if `needle` is a substring of `haystack`. -/
private def hasSubstr (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

private def check (name : String) (cond : Bool) : IO (Nat × Nat) := do
  if cond then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "── Metrics tests ──"
  let mut total := 0
  let mut failures := 0

  -- Counter tests
  let reg ← MetricsRegistry.new
  let counter ← reg.registerCounter "test_counter" "A test counter"
  let val ← counter.get
  let (t, f) ← check "counter starts at 0" (val == 0)
  total := total + t; failures := failures + f

  counter.increment
  let val ← counter.get
  let (t, f) ← check "counter increments to 1" (val == 1)
  total := total + t; failures := failures + f

  counter.increment 5
  let val ← counter.get
  let (t, f) ← check "counter increments by 5 to 6" (val == 6)
  total := total + t; failures := failures + f

  -- Gauge tests
  let gauge ← reg.registerGauge "test_gauge" "A test gauge"
  let val ← gauge.get
  let (t, f) ← check "gauge starts at 0" (val == 0)
  total := total + t; failures := failures + f

  gauge.set 42
  let val ← gauge.get
  let (t, f) ← check "gauge set to 42" (val == 42)
  total := total + t; failures := failures + f

  gauge.set (-10)
  let val ← gauge.get
  let (t, f) ← check "gauge set to -10" (val == -10)
  total := total + t; failures := failures + f

  gauge.increment 3
  let val ← gauge.get
  let (t, f) ← check "gauge increment by 3 to -7" (val == -7)
  total := total + t; failures := failures + f

  -- Prometheus rendering
  gauge.set 99
  let output ← reg.renderPrometheus
  let (t, f) ← check "prometheus output contains counter name"
    (hasSubstr output "test_counter")
  total := total + t; failures := failures + f

  let (t, f) ← check "prometheus output contains counter value 6"
    (hasSubstr output "test_counter 6")
  total := total + t; failures := failures + f

  let (t, f) ← check "prometheus output contains gauge value 99"
    (hasSubstr output "test_gauge 99")
  total := total + t; failures := failures + f

  let (t, f) ← check "prometheus output contains TYPE annotations"
    (hasSubstr output "# TYPE test_counter counter" &&
     hasSubstr output "# TYPE test_gauge gauge")
  total := total + t; failures := failures + f

  let (t, f) ← check "prometheus output contains HELP annotations"
    (hasSubstr output "# HELP test_counter A test counter" &&
     hasSubstr output "# HELP test_gauge A test gauge")
  total := total + t; failures := failures + f

  -- BeaconMetrics registration
  let reg2 ← MetricsRegistry.new
  let metrics ← BeaconMetrics.register reg2
  metrics.blocksImported.increment
  metrics.headSlot.set 100
  let output ← reg2.renderPrometheus
  let (t, f) ← check "beacon metrics registers standard counters"
    (hasSubstr output "blocks_imported_total 1" &&
     hasSubstr output "head_slot 100")
  total := total + t; failures := failures + f

  return (total, failures)

end Test.Metrics
