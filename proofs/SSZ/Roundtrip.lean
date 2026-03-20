/-
  SSZ Roundtrip Theorems

  These theorems express the correctness property that SSZ encoding
  followed by decoding returns the original value.

  Proof status:
  - Structural invariants (3): proven
  - Bool roundtrip: proven by case analysis + rfl
  - UInt64 roundtrip: proven via BitVec LE byte decomposition identity
  - BytesN roundtrip: proven via size invariant + proof irrelevance
  - Container types (4): sorry (derive-generated instances produce terms
    too large for simp; requires custom reflection or derive-aware tactics)
-/

import LeanConsensus.SSZ
import LeanConsensus.Consensus.Types

namespace Proofs.SSZ.Roundtrip

open LeanConsensus.SSZ
open LeanConsensus.Consensus

-- ════════════════════════════════════════════════════════════════
-- Structural properties (trivially provable from structure definitions)
-- ════════════════════════════════════════════════════════════════

theorem bytesN_size_invariant {n : Nat} (b : BytesN n) : b.data.size = n := b.hsize

theorem sszVector_length_invariant {n : Nat} {α : Type} (v : SszVector n α) : v.elems.size = n := v.hlen

theorem sszList_bound_invariant {maxCap : Nat} {α : Type} (l : SszList maxCap α) : l.elems.size ≤ maxCap := l.hbound

-- ════════════════════════════════════════════════════════════════
-- Bool roundtrip
-- ════════════════════════════════════════════════════════════════

theorem ssz_roundtrip_bool (v : Bool) :
    SszDecode.sszDecode (SszEncode.sszEncode v) = .ok v := by
  cases v <;> rfl

-- ════════════════════════════════════════════════════════════════
-- UInt64 roundtrip — LE byte decomposition/recomposition
-- ════════════════════════════════════════════════════════════════

/-- Extracting byte k from a BitVec 64 via shift-right + mod 256, then masking
    with 0xFF and zero-extending back to 64 bits, equals setWidth 8 then setWidth 64. -/
private theorem ofNat64_shr_mod256_and255 (v : BitVec 64) (k : Nat) :
    BitVec.ofNat 64 (v.toNat >>> k % 256) &&& 255#64 =
    ((v >>> k).setWidth 8).setWidth 64 := by
  rw [(BitVec.toNat_ushiftRight v k).symm, show (256 : Nat) = 2 ^ 8 from by decide,
      ← BitVec.toNat_setWidth, BitVec.ofNat_toNat]
  bv_decide

/-- Special case of the above for k=0 (no shift). -/
private theorem ofNat64_mod256_and255 (v : BitVec 64) :
    BitVec.ofNat 64 (v.toNat % 256) &&& 255#64 =
    (v.setWidth 8).setWidth 64 := by
  have := ofNat64_shr_mod256_and255 v 0; simp at this; exact this

/-- Reassembling 8 LE bytes back into a 64-bit value is the identity. -/
private theorem le_byte_reassemble_bv (v : BitVec 64) :
    (v.setWidth 8).setWidth 64 |||
    ((v >>> 8).setWidth 8).setWidth 64 <<< (8#64 % 64#64) |||
    ((v >>> 16).setWidth 8).setWidth 64 <<< (16#64 % 64#64) |||
    ((v >>> 24).setWidth 8).setWidth 64 <<< (24#64 % 64#64) |||
    ((v >>> 32).setWidth 8).setWidth 64 <<< (32#64 % 64#64) |||
    ((v >>> 40).setWidth 8).setWidth 64 <<< (40#64 % 64#64) |||
    ((v >>> 48).setWidth 8).setWidth 64 <<< (48#64 % 64#64) |||
    ((v >>> 56).setWidth 8).setWidth 64 <<< (56#64 % 64#64) = v := by
  bv_decide

set_option maxHeartbeats 200000000 in
theorem ssz_roundtrip_uint64 (v : UInt64) :
    SszDecode.sszDecode (SszEncode.sszEncode v) = .ok v := by
  simp only [SszEncode.sszEncode, SszDecode.sszDecode, encodeUInt64, decodeUInt64At]
  simp only [ByteArray.size, Array.size, List.length]
  simp (config := { decide := true }) only []
  simp only [ByteArray.get!, ite_true, ite_false]
  simp
  change UInt64.ofBitVec _ = UInt64.ofBitVec _
  congr 1
  simp only [UInt64.toBitVec_or, UInt64.toBitVec_and, UInt64.toBitVec_shiftLeft,
             UInt64.ofNat, UInt64.toNat, UInt64.toBitVec_ofNat]
  rw [ofNat64_mod256_and255, ofNat64_shr_mod256_and255, ofNat64_shr_mod256_and255,
      ofNat64_shr_mod256_and255, ofNat64_shr_mod256_and255, ofNat64_shr_mod256_and255,
      ofNat64_shr_mod256_and255, ofNat64_shr_mod256_and255]
  exact le_byte_reassemble_bv v.toBitVec

-- ════════════════════════════════════════════════════════════════
-- BytesN roundtrip
-- ════════════════════════════════════════════════════════════════

theorem ssz_roundtrip_bytesN {n : Nat} (b : BytesN n) :
    SszDecode.sszDecode (SszEncode.sszEncode b) = .ok b := by
  simp [SszEncode.sszEncode, SszDecode.sszDecode, BytesN.mkChecked, b.hsize]

-- ════════════════════════════════════════════════════════════════
-- Container roundtrip proofs
-- ════════════════════════════════════════════════════════════════

theorem ssz_roundtrip_checkpoint (c : Checkpoint) :
    SszDecode.sszDecode (SszEncode.sszEncode c) = .ok c := by sorry

theorem ssz_roundtrip_attestation_data (a : AttestationData) :
    SszDecode.sszDecode (SszEncode.sszEncode a) = .ok a := by sorry

theorem ssz_roundtrip_block_header (h : BeaconBlockHeader) :
    SszDecode.sszDecode (SszEncode.sszEncode h) = .ok h := by sorry

theorem ssz_roundtrip_validator (v : Validator) :
    SszDecode.sszDecode (SszEncode.sszEncode v) = .ok v := by sorry

end Proofs.SSZ.Roundtrip
