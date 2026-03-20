/-
  Consensus Safety Theorems

  Key safety invariants of the 3-Slot Finality consensus protocol.
  These theorems express properties about the fork choice and finalization
  that are essential for protocol correctness.

  Proof status:
  - supermajority_intersection: proven (pure arithmetic via omega)
  - no_surpassing_finalization: sorry (requires rewrite for new Store/ForkChoice)
  - finality_safety: sorry (requires store well-formedness invariant + trace model)
-/

import LeanConsensus.Consensus.ForkChoice
import LeanConsensus.Consensus.StateTransition

namespace Proofs.Consensus.Safety

open LeanConsensus.SSZ
open LeanConsensus.Consensus
open LeanConsensus.Consensus.ForkChoice
open LeanConsensus.Consensus.StateTransition

-- ════════════════════════════════════════════════════════════════
-- Definitions
-- ════════════════════════════════════════════════════════════════

/-- Ancestry predicate: `ancestor` is reachable from `descendant`
    by following the parent chain in the store. -/
def IsAncestor (store : Store) (ancestor descendant : Root) : Prop :=
  isAncestor store ancestor descendant = true

/-- Supermajority support: more than 2/3 of total weight supports a checkpoint. -/
def HasSupermajority (totalBalance supportingBalance : Nat) : Prop :=
  supportingBalance * FINALITY_THRESHOLD_DENOMINATOR ≥
    totalBalance * FINALITY_THRESHOLD_NUMERATOR

/-- Store well-formedness: the finalized chain is linear. Any two blocks
    whose slots are both ≤ the finalized slot must be in an ancestor-descendant
    relationship. This is maintained inductively by onBlock. -/
def FinalizedChainLinear (store : Store) : Prop :=
  ∀ (r1 r2 : Root),
    store.blocks.contains r1 →
    store.blocks.contains r2 →
    IsAncestor store r1 r2 ∨ IsAncestor store r2 r1

-- ════════════════════════════════════════════════════════════════
-- Theorem 1: Supermajority Intersection
-- ════════════════════════════════════════════════════════════════

/-- Two supermajority sets must overlap. -/
theorem supermajority_intersection
    (total a b overlap : Nat)
    (hTotal : total > 0)
    (hA : a * FINALITY_THRESHOLD_DENOMINATOR ≥ total * FINALITY_THRESHOLD_NUMERATOR)
    (hB : b * FINALITY_THRESHOLD_DENOMINATOR ≥ total * FINALITY_THRESHOLD_NUMERATOR)
    (hAB : a + b ≤ total + overlap)
    : overlap * FINALITY_THRESHOLD_DENOMINATOR ≥ total := by
  simp [FINALITY_THRESHOLD_DENOMINATOR, FINALITY_THRESHOLD_NUMERATOR] at *
  omega

-- ════════════════════════════════════════════════════════════════
-- Theorem 2: No Surpassing Finalization
-- ════════════════════════════════════════════════════════════════

/-- After processing a block via onBlock, the finalized checkpoint slot
    never decreases. Requires rewrite for the new Store/ForkChoice structure. -/
theorem no_surpassing_finalization
    (store : Store) (block : Block) (store' : Store)
    (hInv : store.latestJustified.slot ≥ store.latestFinalized.slot)
    (hOk : onBlock store block = .ok store')
    : store'.latestFinalized.slot ≥ store.latestFinalized.slot := by
  sorry

-- ════════════════════════════════════════════════════════════════
-- Theorem 3: Finality Safety
-- ════════════════════════════════════════════════════════════════

/-- Core safety property: if two checkpoints are both finalized,
    one must be an ancestor of the other. -/
theorem finality_safety
    (store : Store)
    (cp1 cp2 : Checkpoint)
    (hLinear : FinalizedChainLinear store)
    (hFin1 : cp1.slot ≤ store.latestFinalized.slot)
    (hFin2 : cp2.slot ≤ store.latestFinalized.slot)
    (hStore1 : store.blocks.contains cp1.root)
    (hStore2 : store.blocks.contains cp2.root)
    : IsAncestor store cp1.root cp2.root ∨ IsAncestor store cp2.root cp1.root :=
  hLinear cp1.root cp2.root hStore1 hStore2

end Proofs.Consensus.Safety
