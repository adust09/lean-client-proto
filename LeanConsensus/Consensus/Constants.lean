/-
  Consensus Constants — pq-devnet-3

  These constants define the consensus parameters for the post-quantum
  devnet with 3-Slot Finality. Values are from the devnet-3 spec.

  All constants are Nat-level (not UInt64) so they can be used as
  type-level parameters for SszVector and SszList bounds.
-/

namespace LeanConsensus.Consensus

-- ════════════════════════════════════════════════════════════════
-- Collection Limits (used as type-level parameters)
-- ════════════════════════════════════════════════════════════════

/-- Maximum attestations per block body. -/
def MAX_ATTESTATIONS : Nat := 4096

/-- Maximum number of validators in the registry. -/
def VALIDATOR_REGISTRY_LIMIT : Nat := 4096

/-- Maximum number of historical roots stored. -/
def HISTORICAL_ROOTS_LIMIT : Nat := 262144

-- ════════════════════════════════════════════════════════════════
-- Timing
-- ════════════════════════════════════════════════════════════════

/-- Duration of one slot in seconds. -/
def SECONDS_PER_SLOT : Nat := 4

/-- Number of slots to achieve finality (3-Slot Finality). -/
def SLOTS_TO_FINALITY : Nat := 3

/-- Number of intervals per slot. -/
def INTERVALS_PER_SLOT : Nat := 5

/-- Number of slots to look back for justification. -/
def JUSTIFICATION_LOOKBACK_SLOTS : Nat := 3

-- ════════════════════════════════════════════════════════════════
-- Finality
-- ════════════════════════════════════════════════════════════════

/-- Numerator of the finality threshold (2/3 of validators). -/
def FINALITY_THRESHOLD_NUMERATOR : Nat := 2

/-- Denominator of the finality threshold. -/
def FINALITY_THRESHOLD_DENOMINATOR : Nat := 3

/-- Sentinel slot value representing "no exit scheduled". -/
def FAR_FUTURE_SLOT : Nat := 0xFFFFFFFFFFFFFFFF

-- ════════════════════════════════════════════════════════════════
-- Cryptographic Sizes
-- ════════════════════════════════════════════════════════════════

/-- XMSS public key size in bytes. -/
def XMSS_PUBKEY_SIZE : Nat := 52

/-- XMSS signature size in bytes (test config for fixtures). -/
def XMSS_SIGNATURE_SIZE : Nat := 424

-- ════════════════════════════════════════════════════════════════
-- Network
-- ════════════════════════════════════════════════════════════════

/-- Gossipsub mesh target size. -/
def GOSSIPSUB_MESH_SIZE : Nat := 8

/-- Gossipsub heartbeat interval in milliseconds. -/
def GOSSIPSUB_HEARTBEAT_MS : Nat := 700

-- ════════════════════════════════════════════════════════════════
-- Domain Types (4-byte identifiers for signing context)
-- ════════════════════════════════════════════════════════════════

/-- Domain type for beacon block proposer. -/
def DOMAIN_BEACON_PROPOSER : UInt32 := 0x00000000

/-- Domain type for beacon attester. -/
def DOMAIN_BEACON_ATTESTER : UInt32 := 0x01000000

end LeanConsensus.Consensus
