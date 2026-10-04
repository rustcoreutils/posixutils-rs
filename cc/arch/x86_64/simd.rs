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
use super::lir::{
    FloatCompare, FloatLane, GpOperand, IntLane, PackedOp, PackedShift, PackedShuffleOp, X86Inst,
};
use super::regalloc::{Loc, Reg, XmmReg};
use crate::arch::lir::{FpSize, OperandSize};
use crate::arch::simd::X86Shuffle;
use crate::ir::{Instruction, PseudoId, SimdOp};
use crate::types::TypeTable;

/// A two-operand operation as SSE computes it: `first` moved into the
/// target, `op` applied with `second`, then -- for an unsigned order --
/// compared for equality with `second` again, and inverted when the
/// instruction answers the opposite question.
struct PackedForm {
    first: PseudoId,
    second: PseudoId,
    op: PackedOp,
    then_equal: bool,
    invert: bool,
}

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
                self.push_lir(X86Inst::PackedShiftImm {
                    shift: PackedShift::Left,
                    lane,
                    count,
                    dst: scratch,
                });
                let flane = FloatLane::of_bytes(lane_bytes);
                self.push_lir(packed(PackedOp::FXor(flane), scratch, dst));
            }
            SimdOp::Splat => self.emit_splat(insn.src[0], dst, lane_bytes, types.is_float(lane)),
            SimdOp::Shuffle => {
                let idx = insn.shuffle_indices();
                match crate::arch::simd::x86_64_shuffle(idx, lane_bytes) {
                    Some(form) => self.emit_shuffle(insn, form, dst, scratch, size),
                    None => {
                        // SSSE3: the source's bytes picked by a constant
                        // control, which `arch::simd` listed.
                        let (src, control) =
                            crate::arch::simd::x86_64_byte_shuffle(idx, lane_bytes)
                                .expect("arch::simd lists only shuffles SSSE3 has");
                        self.emit_fp_move(insn.src[src], dst, size);
                        let lo = u64::from_le_bytes(control[..8].try_into().expect("eight"));
                        let hi = u64::from_le_bytes(control[8..].try_into().expect("eight"));
                        self.emit_bits128_to_xmm(lo, hi, scratch);
                        self.push_lir(packed(PackedOp::ShuffleBytes, scratch, dst));
                    }
                }
            }
            SimdOp::CvtSF | SimdOp::CvtFS => {
                // The conversion reads its source whole, so the target may
                // hold it.
                let src = match self.get_location(insn.src[0]) {
                    Loc::Xmm(x) => x,
                    _ => {
                        self.emit_fp_move(insn.src[0], dst, size);
                        dst
                    }
                };
                let cvt = if op == SimdOp::CvtSF {
                    PackedOp::CvtDwordsToFloats
                } else {
                    PackedOp::CvtFloatsToDwords
                };
                self.push_lir(packed(cvt, src, dst));
            }
            SimdOp::ShlScalar | SimdOp::LsrScalar | SimdOp::AsrScalar => {
                let shift = match op {
                    SimdOp::ShlScalar => PackedShift::Left,
                    SimdOp::LsrScalar => PackedShift::LogicalRight,
                    _ => PackedShift::ArithmeticRight,
                };
                let lane = IntLane::of_bytes(lane_bytes);
                let count = insn.src[1];
                if let Loc::Imm(n) = self.get_location(count) {
                    // A count past the lane clears it (or fills it with
                    // the sign); so does the largest immediate.
                    self.emit_fp_move(insn.src[0], dst, size);
                    self.push_lir(X86Inst::PackedShiftImm {
                        shift,
                        lane,
                        count: n.clamp(0, 255) as u8,
                        dst,
                    });
                } else {
                    // The count, from a general register into the low
                    // quadword of a scratch XMM register -- all of which the
                    // shift reads, so it is moved at its own width and
                    // zero-extended: the bits above an `int` argument are
                    // the caller's garbage, and a count over the lane width
                    // clears every lane.
                    let bits = lane_bytes as u32 * 8;
                    self.emit_move(count, Reg::R11, bits);
                    if bits < 32 {
                        self.push_lir(X86Inst::Movzx {
                            src_size: OperandSize::from_bits(bits),
                            dst_size: OperandSize::B32,
                            src: GpOperand::Reg(Reg::R11),
                            dst: Reg::R11,
                        });
                    }
                    self.push_lir(X86Inst::MovGpXmm {
                        size: OperandSize::B64,
                        src: Reg::R11,
                        dst: scratch,
                    });
                    self.emit_fp_move(insn.src[0], dst, size);
                    self.push_lir(packed(PackedOp::Shift(shift, lane), scratch, dst));
                }
            }
            _ => {
                let (a, b) = (insn.src[0], insn.src[1]);
                let form = Self::packed_form(op, a, b, lane_bytes);
                // The second operand in a register other than the target's,
                // secured before the first is moved into the target.
                let second_reg = match self.get_location(form.second) {
                    Loc::Xmm(x) if x != dst => x,
                    _ => {
                        self.emit_fp_move(form.second, scratch, size);
                        scratch
                    }
                };
                self.emit_fp_move(form.first, dst, size);
                self.push_lir(packed(form.op, second_reg, dst));
                if form.then_equal {
                    let lane = IntLane::of_bytes(lane_bytes);
                    self.push_lir(packed(PackedOp::CmpEq(lane), second_reg, dst));
                }
                if form.invert {
                    // The scratch register is free again: all ones, xored in.
                    self.push_lir(packed(PackedOp::CmpEq(IntLane::D), scratch, scratch));
                    self.push_lir(packed(PackedOp::Xor, scratch, dst));
                }
            }
        }
        if !matches!(dst_loc, Loc::Xmm(x) if x == dst) {
            self.emit_fp_move_from_xmm(dst, &dst_loc, size);
        }
    }

    /// A constant shuffle in its SSE2 `form`, into `dst`.
    fn emit_shuffle(
        &mut self,
        insn: &Instruction,
        form: X86Shuffle,
        dst: XmmReg,
        scratch: XmmReg,
        size: FpSize,
    ) {
        let (op, first, second, imm) = match form {
            X86Shuffle::Pshufd { src, imm } => {
                // One source, read whole: where it is, or loaded first.
                let src = match self.get_location(insn.src[src]) {
                    Loc::Xmm(x) => x,
                    _ => {
                        self.emit_fp_move(insn.src[src], scratch, size);
                        scratch
                    }
                };
                self.push_lir(X86Inst::PackedShuffle {
                    op: PackedShuffleOp::Pshufd,
                    imm,
                    src,
                    dst,
                });
                return;
            }
            X86Shuffle::Shufps { first, second, imm } => {
                (PackedShuffleOp::Shufps, first, second, imm)
            }
            X86Shuffle::Shufpd { first, second, imm } => {
                (PackedShuffleOp::Shufpd, first, second, imm)
            }
        };
        // Two sources: the second secured before the first is moved into
        // the target.
        let (first, second) = (insn.src[first], insn.src[second]);
        let second_reg = match self.get_location(second) {
            Loc::Xmm(x) if x != dst => x,
            _ => {
                self.emit_fp_move(second, scratch, size);
                scratch
            }
        };
        self.emit_fp_move(first, dst, size);
        self.push_lir(X86Inst::PackedShuffle {
            op,
            imm,
            src: second_reg,
            dst,
        });
    }

    /// Every lane of `dst` the scalar `src`, of a lane `lane_bytes` wide:
    /// moved into the low lane, then copied up by interleaving and
    /// shuffling.
    fn emit_splat(&mut self, src: PseudoId, dst: XmmReg, lane_bytes: usize, float: bool) {
        let packed = |op, r| X86Inst::Packed { op, src: r, dst: r };
        let shuffle = |op, r| X86Inst::PackedShuffle {
            op,
            imm: 0,
            src: r,
            dst: r,
        };
        if float {
            let single = lane_bytes == 4;
            let size = if single {
                FpSize::Single
            } else {
                FpSize::Double
            };
            self.emit_fp_move(src, dst, size);
            if single {
                self.push_lir(shuffle(PackedShuffleOp::Shufps, dst));
            } else {
                self.push_lir(packed(PackedOp::FUnpackLow(FloatLane::D), dst));
            }
            return;
        }
        let bits = lane_bytes as u32 * 8;
        self.emit_move(src, Reg::R11, bits);
        self.push_lir(X86Inst::MovGpXmm {
            size: if bits > 32 {
                OperandSize::B64
            } else {
                OperandSize::B32
            },
            src: Reg::R11,
            dst,
        });
        match lane_bytes {
            1 => {
                self.push_lir(packed(PackedOp::UnpackLow(IntLane::B), dst));
                self.push_lir(shuffle(PackedShuffleOp::Pshuflw, dst));
                self.push_lir(shuffle(PackedShuffleOp::Pshufd, dst));
            }
            2 => {
                self.push_lir(shuffle(PackedShuffleOp::Pshuflw, dst));
                self.push_lir(shuffle(PackedShuffleOp::Pshufd, dst));
            }
            4 => self.push_lir(shuffle(PackedShuffleOp::Pshufd, dst)),
            _ => self.push_lir(packed(PackedOp::UnpackLow(IntLane::Q), dst)),
        }
    }

    /// How SSE computes the two-operand `op` of `a` and `b` on lanes of
    /// `lane_bytes` ([`PackedForm`]). The integer compares are equality and
    /// signed greater-than only, so `!=` is the inverse of `==`, and
    /// `a >= b` the inverse of `b > a`; an unsigned `a >= b` is
    /// `min(a, b) == b`, which reads `b` twice and `a` once, and `a > b` the
    /// inverse of `b >= a`. The floating compares have every predicate C
    /// needs but greater-than, which is less-than of the operands swapped.
    fn packed_form(op: SimdOp, a: PseudoId, b: PseudoId, lane_bytes: usize) -> PackedForm {
        let int = || IntLane::of_bytes(lane_bytes);
        let float = || FloatLane::of_bytes(lane_bytes);
        let form = |first, second, op| PackedForm {
            first,
            second,
            op,
            then_equal: false,
            invert: false,
        };
        let inverse = |f: PackedForm| PackedForm { invert: true, ..f };
        let min_equal = |first, second| PackedForm {
            then_equal: true,
            ..form(first, second, PackedOp::MinU(int()))
        };
        match op {
            SimdOp::CmpEq => form(a, b, PackedOp::CmpEq(int())),
            SimdOp::CmpNe => inverse(form(a, b, PackedOp::CmpEq(int()))),
            SimdOp::CmpGt => form(a, b, PackedOp::CmpGt(int())),
            SimdOp::CmpGe => inverse(form(b, a, PackedOp::CmpGt(int()))),
            SimdOp::CmpGeU => min_equal(a, b),
            SimdOp::CmpGtU => inverse(min_equal(b, a)),
            SimdOp::FCmpEq => form(a, b, PackedOp::FCmp(FloatCompare::Eq, float())),
            SimdOp::FCmpNe => form(a, b, PackedOp::FCmp(FloatCompare::Neq, float())),
            SimdOp::FCmpGt => form(b, a, PackedOp::FCmp(FloatCompare::Lt, float())),
            SimdOp::FCmpGe => form(b, a, PackedOp::FCmp(FloatCompare::Le, float())),
            _ => form(a, b, Self::packed_op(op, lane_bytes)),
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
            SimdOp::Mul => PackedOp::MulLow(int()),
            SimdOp::Not
            | SimdOp::Neg
            | SimdOp::FNeg
            | SimdOp::Splat
            | SimdOp::Shl
            | SimdOp::Lsr
            | SimdOp::Asr
            | SimdOp::ShlScalar
            | SimdOp::LsrScalar
            | SimdOp::AsrScalar
            | SimdOp::CmpEq
            | SimdOp::CmpNe
            | SimdOp::CmpGt
            | SimdOp::CmpGe
            | SimdOp::CmpGtU
            | SimdOp::CmpGeU
            | SimdOp::FCmpEq
            | SimdOp::FCmpNe
            | SimdOp::FCmpGt
            | SimdOp::FCmpGe
            | SimdOp::Shuffle
            | SimdOp::CvtSF
            | SimdOp::CvtUF
            | SimdOp::CvtFS
            | SimdOp::CvtFU => unreachable!("{op:?} is not a plain two-register operation"),
        }
    }
}
