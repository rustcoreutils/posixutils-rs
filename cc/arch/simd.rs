//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The lane-wise GNU vector operations each target computes with its packed
// instructions. The linearizer builds an `Opcode::Simd` for exactly what this
// table lists and computes everything else lane by lane; each backend emits
// exactly what it lists. It is the one statement of what is native.
//

use crate::float::FpFormat;
use crate::ir::SimdOp;
use crate::target::{Arch, Target};
use crate::types::{TypeId, TypeTable};

/// Whether `target` computes `op` on vectors of type `vec` with packed
/// instructions.
///
/// Only whole-register vectors qualify: sixteen bytes (an XMM or Q
/// register) or eight (the low half, or a D register). Integer lanes of one
/// to eight bytes and floating lanes of `float` or `double` format.
pub fn native(target: &Target, op: SimdOp, vec: TypeId, types: &TypeTable) -> bool {
    let Some((lane, _)) = types.vector_lanes(vec) else {
        return false;
    };
    let bytes = types.size_bytes(vec);
    if bytes != 16 && bytes != 8 {
        return false;
    }
    let float = match types.fp_format(lane) {
        Some(FpFormat::Binary32 | FpFormat::Binary64) => true,
        Some(_) => return false,
        None if types.is_integer(lane) && types.size_bytes(lane) <= 8 => false,
        None => return false,
    };
    if op.float_lanes().is_some_and(|f| f != float) {
        return false;
    }
    let lane_bytes = types.size_bytes(lane);
    match target.arch {
        Arch::X86_64 => x86_64(op, bytes, lane_bytes, float),
        Arch::Aarch64 => aarch64(op, lane_bytes),
    }
}

/// SSE2: integer lanes in either width -- an eight-byte vector rides in the
/// low half of an XMM register, and a lane-wise integer operation never
/// reads across lanes, so the upper half is harmless. Floating lanes only
/// at sixteen bytes: whatever the upper half holds, a packed operation on
/// it could raise a floating-point exception flag the program never asked
/// for.
///
/// SSE2 multiplies 16-bit lanes only (`pmullw`), and shifts every lane by
/// one count -- no bytes, and no 64-bit arithmetic right shift.
fn x86_64(op: SimdOp, bytes: usize, lane_bytes: usize, float: bool) -> bool {
    if float && bytes != 16 {
        return false;
    }
    match op {
        SimdOp::Mul => lane_bytes == 2,
        SimdOp::Shl | SimdOp::Lsr | SimdOp::Asr => false,
        SimdOp::ShlScalar | SimdOp::LsrScalar => lane_bytes >= 2,
        SimdOp::AsrScalar => matches!(lane_bytes, 2 | 4),
        // pcmpeq and pcmpgt reach dwords; 64-bit lanes are SSE4.1 and 4.2,
        // and an unsigned order has no SSE2 compare.
        SimdOp::CmpEq | SimdOp::CmpNe | SimdOp::CmpGt | SimdOp::CmpGe => lane_bytes <= 4,
        SimdOp::CmpGtU | SimdOp::CmpGeU => false,
        _ => true,
    }
}

/// NEON has the operations at both widths, in the Q register or its D
/// half. It multiplies lanes up to 32 bits, and shifts each lane by its own
/// count (`ushl`/`sshl`); a single count is spread to every lane first.
fn aarch64(op: SimdOp, lane_bytes: usize) -> bool {
    match op {
        SimdOp::Mul => lane_bytes <= 4,
        SimdOp::ShlScalar | SimdOp::LsrScalar | SimdOp::AsrScalar => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::Os;

    #[test]
    fn test_native_vector_operations() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let mut types = TypeTable::new(&target);
        let v4si = types.vector_of(types.int_id, 4, None);
        let v2si = types.vector_of(types.int_id, 2, None);
        let v16qi = types.vector_of(types.char_id, 16, None);
        let v4sf = types.vector_of(types.float_id, 4, None);
        let v2sf = types.vector_of(types.float_id, 2, None);
        let v2df = types.vector_of(types.double_id, 2, None);
        let v2hi = types.vector_of(types.short_id, 2, None);
        let v1ti = types.vector_of(types.int128_id, 1, None);
        let n = |op, v| native(&target, op, v, &types);
        for v in [v4si, v2si, v16qi] {
            for op in [
                SimdOp::Add,
                SimdOp::Sub,
                SimdOp::Xor,
                SimdOp::Not,
                SimdOp::Neg,
            ] {
                assert!(n(op, v), "{op:?}");
            }
            assert!(!n(SimdOp::FAdd, v));
        }
        for v in [v4sf, v2df] {
            assert!(n(SimdOp::FDiv, v) && n(SimdOp::FNeg, v));
            assert!(!n(SimdOp::Add, v));
        }
        // Eight bytes of floats, four bytes of anything, and a lane wider
        // than a register's lane stay lane by lane.
        assert!(!n(SimdOp::FAdd, v2sf));
        assert!(!n(SimdOp::Add, v2hi));
        assert!(!n(SimdOp::Add, v1ti));
        // NEON has them all, eight-byte floats included.
        let a64 = Target::new(Arch::Aarch64, Os::Linux);
        for (op, v) in [
            (SimdOp::Add, v4si),
            (SimdOp::FAdd, v2sf),
            (SimdOp::Neg, v2si),
        ] {
            assert!(native(&a64, op, v, &types), "{op:?}");
        }
        assert!(!native(&a64, SimdOp::Add, v2hi, &types));
    }

    #[test]
    fn test_native_splat_multiply_and_shift() {
        let x86 = Target::new(Arch::X86_64, Os::Linux);
        let a64 = Target::new(Arch::Aarch64, Os::Linux);
        let mut types = TypeTable::new(&x86);
        let v16qi = types.vector_of(types.char_id, 16, None);
        let v8hi = types.vector_of(types.short_id, 8, None);
        let v4si = types.vector_of(types.int_id, 4, None);
        let v2di = types.vector_of(types.long_id, 2, None);
        let v4sf = types.vector_of(types.float_id, 4, None);
        let n = |t: &Target, op, v| native(t, op, v, &types);
        // A splat of either lane kind.
        for v in [v16qi, v4si, v2di, v4sf] {
            assert!(n(&x86, SimdOp::Splat, v) && n(&a64, SimdOp::Splat, v));
        }
        // SSE2 multiplies words; NEON up to 32-bit lanes.
        assert!(n(&x86, SimdOp::Mul, v8hi) && !n(&x86, SimdOp::Mul, v4si));
        assert!(n(&a64, SimdOp::Mul, v4si) && !n(&a64, SimdOp::Mul, v2di));
        assert!(!n(&x86, SimdOp::Mul, v4sf));
        // SSE2 shifts by one count, not bytes, no 64-bit arithmetic shift;
        // NEON by per-lane counts.
        assert!(n(&x86, SimdOp::ShlScalar, v2di) && n(&x86, SimdOp::AsrScalar, v4si));
        assert!(!n(&x86, SimdOp::ShlScalar, v16qi) && !n(&x86, SimdOp::AsrScalar, v2di));
        assert!(!n(&x86, SimdOp::Shl, v4si));
        assert!(n(&a64, SimdOp::Asr, v16qi) && n(&a64, SimdOp::Lsr, v2di));
        assert!(!n(&a64, SimdOp::ShlScalar, v4si));
    }

    #[test]
    fn test_native_comparisons() {
        let x86 = Target::new(Arch::X86_64, Os::Linux);
        let a64 = Target::new(Arch::Aarch64, Os::Linux);
        let mut types = TypeTable::new(&x86);
        let v16qi = types.vector_of(types.char_id, 16, None);
        let v4si = types.vector_of(types.int_id, 4, None);
        let v2di = types.vector_of(types.long_id, 2, None);
        let v4sf = types.vector_of(types.float_id, 4, None);
        let v2sf = types.vector_of(types.float_id, 2, None);
        let n = |t: &Target, op, v| native(t, op, v, &types);
        for v in [v16qi, v4si] {
            assert!(n(&x86, SimdOp::CmpEq, v) && n(&x86, SimdOp::CmpGe, v));
            assert!(!n(&x86, SimdOp::CmpGtU, v));
        }
        assert!(!n(&x86, SimdOp::CmpEq, v2di) && !n(&x86, SimdOp::CmpGt, v2di));
        assert!(n(&x86, SimdOp::FCmpNe, v4sf) && !n(&x86, SimdOp::FCmpNe, v2sf));
        assert!(!n(&x86, SimdOp::CmpEq, v4sf) && !n(&x86, SimdOp::FCmpEq, v4si));
        for v in [v16qi, v4si, v2di] {
            assert!(n(&a64, SimdOp::CmpGtU, v) && n(&a64, SimdOp::CmpNe, v));
        }
        assert!(n(&a64, SimdOp::FCmpGe, v2sf));
    }
}
