/-
  Signing — computeSigningRoot and computeDomain

  Per the Ethereum consensus spec:
  - ForkData and SigningData are SSZ containers used to derive signing domains
  - computeDomain creates a 32-byte domain from a domain type + fork version + genesis root
  - computeSigningRoot creates the message root that validators actually sign
-/

import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.BytesN
import LeanConsensus.SSZ.Encode
import LeanConsensus.SSZ.Merkleization
import LeanConsensus.SSZ.Derive
import LeanConsensus.Consensus.Constants

namespace LeanConsensus.Consensus

open LeanConsensus.SSZ

-- ════════════════════════════════════════════════════════════════
-- ForkData — SSZ container for domain computation
-- ════════════════════════════════════════════════════════════════

/-- SSZ container used to compute the fork data root for domain derivation.
    Fields: 4-byte fork version + 32-byte genesis validators root. -/
structure ForkData where
  currentVersion        : Bytes4
  genesisValidatorsRoot : Bytes32
  deriving SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- SigningData — SSZ container for signing root computation
-- ════════════════════════════════════════════════════════════════

/-- SSZ container that wraps an object root and domain for signing.
    The hash_tree_root of this container is the actual signing root. -/
structure SigningData where
  objectRoot : Bytes32
  domain     : Bytes32
  deriving SszType, SszEncode, SszDecode, SszHashTreeRoot

-- ════════════════════════════════════════════════════════════════
-- Helpers
-- ════════════════════════════════════════════════════════════════

/-- Wrap a ByteArray as Bytes32, falling back to zero if the size is wrong.
    In practice, SHA-256 output is always 32 bytes so the fallback is unreachable. -/
private def toBytes32 (data : ByteArray) : Bytes32 :=
  if h : data.size = 32 then ⟨data, h⟩ else BytesN.zero 32

-- ════════════════════════════════════════════════════════════════
-- computeDomain
-- ════════════════════════════════════════════════════════════════

/-- Compute a signing domain per the Ethereum consensus spec.

    domain = domainType (4 bytes LE) ++ forkDataRoot[0..28]

    where forkDataRoot = hash_tree_root(ForkData{forkVersion, genesisValidatorsRoot}). -/
def computeDomain (domainType : UInt32) (forkVersion : Bytes4) (genesisValidatorsRoot : Bytes32) : Bytes32 :=
  let forkData : ForkData := {
    currentVersion := forkVersion
    genesisValidatorsRoot := genesisValidatorsRoot
  }
  let forkDataRoot := SszHashTreeRoot.hashTreeRoot forkData
  let domainTypeBytes := encodeUInt32 domainType
  let domainBytes := domainTypeBytes ++ forkDataRoot.extract 0 28
  if h : domainBytes.size = 32 then ⟨domainBytes, h⟩
  else BytesN.zero 32  -- unreachable: 4 + 28 = 32

-- ════════════════════════════════════════════════════════════════
-- computeSigningRoot
-- ════════════════════════════════════════════════════════════════

/-- Compute the signing root for an SSZ object and domain.

    signingRoot = hash_tree_root(SigningData{hash_tree_root(object), domain})

    This is the 32-byte message that validators sign with their XMSS keys. -/
def computeSigningRoot {α : Type} [SszHashTreeRoot α] (sszObject : α) (domain : Bytes32) : Bytes32 :=
  let objectRoot := toBytes32 (SszHashTreeRoot.hashTreeRoot sszObject)
  let signingData : SigningData := {
    objectRoot := objectRoot
    domain := domain
  }
  toBytes32 (SszHashTreeRoot.hashTreeRoot signingData)

end LeanConsensus.Consensus
