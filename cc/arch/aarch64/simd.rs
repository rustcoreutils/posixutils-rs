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
use super::regalloc::{Loc, Reg, VReg};
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
        let lane_bytes = types.size_bytes(lane);
        let dst_loc = self.get_location(target);
        let dst = match dst_loc {
            Loc::VReg(v) => v,
            _ => VReg::V16,
        };
        if op == SimdOp::Splat {
            self.emit_neon_splat(insn.src[0], dst, lane_bytes, types.is_float(lane), types);
        } else if op == SimdOp::Shuffle {
            let idx = *insn.shuffle_indices();
            self.emit_neon_shuffle(insn, &idx, lane_bytes, dst, types);
        } else {
            let neon = Self::neon_op(op);
            let arr = if neon.is_bitwise() {
                Arrangement::of(1, total)
            } else {
                Arrangement::of(lane_bytes, total)
            };
            let src1 = self.simd_operand(insn.src[0], VReg::V17, carrier, insn.size, types);
            let mut src2 = insn
                .src
                .get(1)
                .map(|&s| self.simd_operand(s, VReg::V18, carrier, insn.size, types));
            if matches!(op, SimdOp::Lsr | SimdOp::Asr) {
                // A right shift is a left shift by the negated counts,
                // made in V18 so an operand's own register is untouched.
                let counts = src2.expect("a shift has its counts");
                self.push_lir(Aarch64Inst::Neon {
                    op: NeonOp::Neg,
                    arr,
                    src1: counts,
                    src2: None,
                    dst: VReg::V18,
                });
                src2 = Some(VReg::V18);
            }
            self.push_lir(Aarch64Inst::Neon {
                op: neon,
                arr,
                src1,
                src2,
                dst,
            });
            if matches!(op, SimdOp::CmpNe | SimdOp::FCmpNe) {
                // Not equal is the inverse of equal: a NaN is unequal.
                self.push_lir(Aarch64Inst::Neon {
                    op: NeonOp::Not,
                    arr: Arrangement::of(1, total),
                    src1: dst,
                    src2: None,
                    dst,
                });
            }
        }
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

    /// A constant shuffle as one `tbl`: the operands are the table -- one
    /// register, or two consecutive ones (V17 and V18) -- and the byte
    /// indices are built in `dst`, which the `tbl` then overwrites. Lane `i`
    /// of the second operand is at byte 16 + `i` * `lane_bytes` whatever the
    /// vector's width, the table registers being sixteen bytes. An
    /// unspecified lane takes an index past the table, which gives zero.
    fn emit_neon_shuffle(
        &mut self,
        insn: &Instruction,
        idx: &crate::ir::ShuffleIndices,
        lane_bytes: usize,
        dst: VReg,
        types: &TypeTable,
    ) {
        let total = insn.size as usize / 8;
        let carrier = if total == 16 {
            types.float128_id
        } else {
            types.double_id
        };
        let n = total / lane_bytes;
        let second = (insn.src.len() == 2).then_some(VReg::V18);
        let table = if second.is_some() {
            self.simd_pair(insn.src[0], insn.src[1], dst, carrier, insn.size, types);
            VReg::V17
        } else {
            match self.get_location(insn.src[0]) {
                // Not the target's register, which the index is built in.
                Loc::VReg(v) if v != dst => v,
                _ => {
                    self.emit_fp_move(insn.src[0], VReg::V17, Some(carrier), insn.size, types);
                    VReg::V17
                }
            }
        };
        let mut bytes = [0xffu8; 16];
        for k in 0..n {
            if let Some(i) = idx.lane(k) {
                let base = (i / n) * 16 + (i % n) * lane_bytes;
                for b in 0..lane_bytes {
                    bytes[k * lane_bytes + b] = (base + b) as u8;
                }
            }
        }
        let lo = u64::from_le_bytes(bytes[..8].try_into().expect("eight bytes"));
        let hi = u64::from_le_bytes(bytes[8..].try_into().expect("eight bytes"));
        self.emit_fp_bits(lo, hi, crate::arch::lir::FpSize::Quad, dst);
        self.push_lir(Aarch64Inst::NeonTbl {
            table,
            second,
            index: dst,
            dst,
            wide: total == 16,
        });
    }

    /// Operands `a` and `b` moved to V17 and V18, the consecutive pair a
    /// two-register `tbl` reads, whichever registers they start in: through
    /// `tmp` -- free until the index is built there -- when they sit in each
    /// other's.
    fn simd_pair(
        &mut self,
        a: PseudoId,
        b: PseudoId,
        tmp: VReg,
        carrier: TypeId,
        size: u32,
        types: &TypeTable,
    ) {
        let in_reg = |cg: &Self, p, r| matches!(cg.get_location(p), Loc::VReg(v) if v == r);
        if in_reg(self, b, VReg::V17) {
            if in_reg(self, a, VReg::V18) {
                self.emit_fp_move(a, tmp, Some(carrier), size, types);
                self.emit_fp_move(b, VReg::V18, Some(carrier), size, types);
                let quad = crate::arch::lir::FpSize::Quad;
                self.push_lir(Aarch64Inst::FmovReg {
                    size: quad,
                    src: tmp,
                    dst: VReg::V17,
                });
                return;
            }
            self.emit_fp_move(b, VReg::V18, Some(carrier), size, types);
            self.emit_fp_move(a, VReg::V17, Some(carrier), size, types);
        } else {
            self.emit_fp_move(a, VReg::V17, Some(carrier), size, types);
            self.emit_fp_move(b, VReg::V18, Some(carrier), size, types);
        }
    }

    /// Every lane of `dst` the scalar `src`, of a lane `lane_bytes` wide: a
    /// `dup` from the general register an integer is in, or from lane 0 of
    /// the V register a float is in. The arrangement fills all sixteen
    /// bytes, which an eight-byte vector's D-register forms then ignore.
    fn emit_neon_splat(
        &mut self,
        src: PseudoId,
        dst: VReg,
        lane_bytes: usize,
        float: bool,
        types: &TypeTable,
    ) {
        let arr = Arrangement::of(lane_bytes, 16);
        let bits = lane_bytes as u32 * 8;
        if float {
            let typ = if lane_bytes == 4 {
                types.float_id
            } else {
                types.double_id
            };
            self.emit_fp_move(src, VReg::V17, Some(typ), bits, types);
            self.push_lir(Aarch64Inst::NeonDupLane {
                arr,
                src: VReg::V17,
                dst,
            });
        } else {
            self.emit_move(src, Reg::X16, bits);
            self.push_lir(Aarch64Inst::NeonDupGp {
                arr,
                src: Reg::X16,
                dst,
            });
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
            SimdOp::Mul => NeonOp::Mul,
            SimdOp::Shl | SimdOp::Lsr => NeonOp::Ushl,
            SimdOp::Asr => NeonOp::Sshl,
            SimdOp::CmpEq | SimdOp::CmpNe => NeonOp::Cmeq,
            SimdOp::CmpGt => NeonOp::Cmgt,
            SimdOp::CmpGe => NeonOp::Cmge,
            SimdOp::CmpGtU => NeonOp::Cmhi,
            SimdOp::CmpGeU => NeonOp::Cmhs,
            SimdOp::FCmpEq | SimdOp::FCmpNe => NeonOp::Fcmeq,
            SimdOp::FCmpGt => NeonOp::Fcmgt,
            SimdOp::FCmpGe => NeonOp::Fcmge,
            SimdOp::CvtSF => NeonOp::Scvtf,
            SimdOp::CvtUF => NeonOp::Ucvtf,
            SimdOp::CvtFS => NeonOp::Fcvtzs,
            SimdOp::CvtFU => NeonOp::Fcvtzu,
            SimdOp::Splat
            | SimdOp::Shuffle
            | SimdOp::ShlScalar
            | SimdOp::LsrScalar
            | SimdOp::AsrScalar => {
                unreachable!("{op:?} is not one NEON instruction")
            }
        }
    }
}
