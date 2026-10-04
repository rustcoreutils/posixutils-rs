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
    if op.is_float() != float {
        return false;
    }
    match target.arch {
        Arch::X86_64 => x86_64(bytes, float),
        // NEON has every listed operation at both widths: the Q register,
        // or its D half.
        Arch::Aarch64 => true,
    }
}

/// SSE2: integer lanes in either width -- an eight-byte vector rides in the
/// low half of an XMM register, and a lane-wise integer operation never
/// reads across lanes, so the upper half is harmless. Floating lanes only
/// at sixteen bytes: whatever the upper half holds, a packed operation on
/// it could raise a floating-point exception flag the program never asked
/// for.
fn x86_64(bytes: usize, float: bool) -> bool {
    !float || bytes == 16
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
}
