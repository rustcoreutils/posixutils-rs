//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 packed SSE2 lowering of `Opcode::Simd`: a lane-wise operation on
// a whole vector held in one XMM register, for what `arch::simd::native`
// lists.
//
// Every packed operand is a register. The operands' homes may be stack
// slots that are not 16-byte aligned, where a legacy-SSE memory operand
// faults, so they are loaded with `movups` (the `Quad` move) first.
//

use super::codegen::X86_64CodeGen;
use super::lir::{FloatLane, IntLane, PackedOp, X86Inst};
use super::regalloc::{Loc, XmmReg};
use crate::arch::lir::FpSize;
use crate::ir::{Instruction, SimdOp};
use crate::types::TypeTable;

impl X86_64CodeGen {
    /// `Opcode::Simd(op)`: the operation on whole XMM registers, computed in
    /// the target's register, or in XMM15 for a target that lives in memory.
    pub(super) fn emit_simd(&mut self, insn: &Instruction, op: SimdOp, types: &TypeTable) {
        let Some(target) = insn.target else {
            return;
        };
        let vec = insn.typ.expect("a vector operation has its vector type");
        let (lane, _) = types
            .vector_lanes(vec)
            .expect("a vector operation's type is a vector");
        let lane_bytes = types.size_bytes(lane);
        // An eight-byte vector rides in the low half; integer lanes never
        // read across, and `arch::simd` keeps floating ones at sixteen.
        let size = if insn.size > 64 {
            FpSize::Quad
        } else {
            FpSize::Double
        };
        let dst_loc = self.get_location(target);
        let dst = match dst_loc {
            Loc::Xmm(x) => x,
            _ => XmmReg::Xmm15,
        };
        let scratch = if dst == XmmReg::Xmm15 {
            XmmReg::Xmm14
        } else {
            XmmReg::Xmm15
        };
        let packed = |op, src, dst| X86Inst::Packed { op, src, dst };
        match op {
            SimdOp::Not => {
                // ~x is x ^ all-ones; `pcmpeqd r, r` makes the ones.
                self.emit_fp_move(insn.src[0], dst, size);
                self.push_lir(packed(PackedOp::CmpEq(IntLane::D), scratch, scratch));
                self.push_lir(packed(PackedOp::Xor, scratch, dst));
            }
            SimdOp::Neg => {
                // 0 - x, with x read before the target is zeroed.
                self.emit_fp_move(insn.src[0], scratch, size);
                self.push_lir(packed(PackedOp::Xor, dst, dst));
                let lane = IntLane::of_bytes(lane_bytes);
                self.push_lir(packed(PackedOp::Sub(lane), scratch, dst));
            }
            SimdOp::FNeg => {
                // Flip each sign bit: all ones shifted up to the lane's top.
                self.emit_fp_move(insn.src[0], dst, size);
                self.push_lir(packed(PackedOp::CmpEq(IntLane::D), scratch, scratch));
                let lane = IntLane::of_bytes(lane_bytes);
                let count = (lane_bytes * 8 - 1) as u8;
                self.push_lir(X86Inst::PackedShiftLeftImm {
                    lane,
                    count,
                    dst: scratch,
                });
                let flane = FloatLane::of_bytes(lane_bytes);
                self.push_lir(packed(PackedOp::FXor(flane), scratch, dst));
            }
            _ => {
                let (a, b) = (insn.src[0], insn.src[1]);
                // The second operand in a register other than the target's,
                // secured before the first is moved into the target.
                let b_reg = match self.get_location(b) {
                    Loc::Xmm(x) if x != dst => x,
                    _ => {
                        self.emit_fp_move(b, scratch, size);
                        scratch
                    }
                };
                self.emit_fp_move(a, dst, size);
                self.push_lir(packed(Self::packed_op(op, lane_bytes), b_reg, dst));
            }
        }
        if !matches!(dst_loc, Loc::Xmm(x) if x == dst) {
            self.emit_fp_move_from_xmm(dst, &dst_loc, size);
        }
    }

    /// The SSE2 instruction for the two-operand `op` on lanes of
    /// `lane_bytes`.
    fn packed_op(op: SimdOp, lane_bytes: usize) -> PackedOp {
        let int = || IntLane::of_bytes(lane_bytes);
        let float = || FloatLane::of_bytes(lane_bytes);
        match op {
            SimdOp::Add => PackedOp::Add(int()),
            SimdOp::Sub => PackedOp::Sub(int()),
            SimdOp::And => PackedOp::And,
            SimdOp::Or => PackedOp::Or,
            SimdOp::Xor => PackedOp::Xor,
            SimdOp::FAdd => PackedOp::FAdd(float()),
            SimdOp::FSub => PackedOp::FSub(float()),
            SimdOp::FMul => PackedOp::FMul(float()),
            SimdOp::FDiv => PackedOp::FDiv(float()),
            SimdOp::Not | SimdOp::Neg | SimdOp::FNeg => {
                unreachable!("{op:?} has one operand")
            }
        }
    }
}
