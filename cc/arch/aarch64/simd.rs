//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// aarch64 NEON lowering of `Opcode::Simd`: a lane-wise operation on a whole
// vector held in one V register -- the Q register for sixteen bytes, the D
// half for eight -- for what `arch::simd::native` lists.
//

use super::codegen::Aarch64CodeGen;
use super::lir::{Aarch64Inst, Arrangement, NeonOp};
use super::regalloc::{Loc, VReg};
use crate::ir::{Instruction, PseudoId, SimdOp};
use crate::types::{TypeId, TypeTable};

impl Aarch64CodeGen {
    /// `Opcode::Simd(op)`: one three-operand NEON instruction, reading each
    /// operand where it sits when that is a V register, and writing the
    /// target's register, or V16 for a target that lives in memory.
    pub(super) fn emit_simd(&mut self, insn: &Instruction, op: SimdOp, types: &TypeTable) {
        let Some(target) = insn.target else {
            return;
        };
        let vec = insn.typ.expect("a vector operation has its vector type");
        let (lane, _) = types
            .vector_lanes(vec)
            .expect("a vector operation's type is a vector");
        let total = insn.size as usize / 8;
        // The carrier the operands are held in, which picks the Q or D move.
        let carrier = if total == 16 {
            types.float128_id
        } else {
            types.double_id
        };
        let neon = Self::neon_op(op);
        let arr = if neon.is_bitwise() {
            Arrangement::of(1, total)
        } else {
            Arrangement::of(types.size_bytes(lane), total)
        };
        let src1 = self.simd_operand(insn.src[0], VReg::V17, carrier, insn.size, types);
        let src2 = insn
            .src
            .get(1)
            .map(|&s| self.simd_operand(s, VReg::V18, carrier, insn.size, types));
        let dst_loc = self.get_location(target);
        let dst = match dst_loc {
            Loc::VReg(v) => v,
            _ => VReg::V16,
        };
        self.push_lir(Aarch64Inst::Neon {
            op: neon,
            arr,
            src1,
            src2,
            dst,
        });
        if !matches!(dst_loc, Loc::VReg(v) if v == dst) {
            self.emit_fp_move_to_loc(dst, &dst_loc, Some(carrier), insn.size, types);
        }
    }

    /// The V register holding operand `src`: its own, or `scratch` loaded
    /// from wherever it lives.
    fn simd_operand(
        &mut self,
        src: PseudoId,
        scratch: VReg,
        carrier: TypeId,
        size: u32,
        types: &TypeTable,
    ) -> VReg {
        match self.get_location(src) {
            Loc::VReg(v) => v,
            _ => {
                self.emit_fp_move(src, scratch, Some(carrier), size, types);
                scratch
            }
        }
    }

    /// The NEON instruction computing `op`.
    fn neon_op(op: SimdOp) -> NeonOp {
        match op {
            SimdOp::Add => NeonOp::Add,
            SimdOp::Sub => NeonOp::Sub,
            SimdOp::And => NeonOp::And,
            SimdOp::Or => NeonOp::Orr,
            SimdOp::Xor => NeonOp::Eor,
            SimdOp::Not => NeonOp::Not,
            SimdOp::Neg => NeonOp::Neg,
            SimdOp::FAdd => NeonOp::Fadd,
            SimdOp::FSub => NeonOp::Fsub,
            SimdOp::FMul => NeonOp::Fmul,
            SimdOp::FDiv => NeonOp::Fdiv,
            SimdOp::FNeg => NeonOp::Fneg,
        }
    }
}
