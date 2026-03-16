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

/-- Maximum number of validators per attestation subnet. -/
def MAX_VALIDATORS_PER_SUBNET : Nat := 256

/-- Maximum attestations per block body. -/
def MAX_ATTESTATIONS : Nat := 128

/-- Maximum attestations stored in beacon state. -/
def MAX_ATTESTATIONS_STATE : Nat := 4096

/-- Number of historical block/state roots kept in circular buffer. -/
def SLOTS_PER_HISTORICAL_ROOT : Nat := 64

/-- Maximum number of validators in the registry. -/
def VALIDATOR_REGISTRY_LIMIT : Nat := 1024

-- ════════════════════════════════════════════════════════════════
-- Timing
-- ════════════════════════════════════════════════════════════════

/-- Duration of one slot in seconds. -/
def SECONDS_PER_SLOT : Nat := 4

/-- Number of slots to achieve finality (3-Slot Finality). -/
def SLOTS_TO_FINALITY : Nat := 3

-- ════════════════════════════════════════════════════════════════
-- Finality
-- ════════════════════════════════════════════════════════════════

/-- Numerator of the finality threshold (2/3 of validators). -/
def FINALITY_THRESHOLD_NUMERATOR : Nat := 2

/-- Denominator of the finality threshold. -/
def FINALITY_THRESHOLD_DENOMINATOR : Nat := 3

-- ════════════════════════════════════════════════════════════════
-- Cryptographic Sizes
-- ════════════════════════════════════════════════════════════════

/-- XMSS public key size in bytes. -/
def XMSS_PUBKEY_SIZE : Nat := 32

/-- XMSS signature size in bytes. -/
def XMSS_SIGNATURE_SIZE : Nat := 3112

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
