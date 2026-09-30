//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Floating-Point Code Generation (SSE)
//

use super::codegen::X86_64CodeGen;
use super::lir::{GpOperand, MemAddr, ShiftCount, X86Inst, XmmOperand};
use super::regalloc::{Loc, Reg, XmmReg};
use crate::arch::lir::{CondCode, Directive, FpSize, Label, OperandSize};
use crate::float::{FloatVal, IntegralRounding};
use crate::ir::{Instruction, Opcode, PseudoId};
use crate::types::{TypeId, TypeKind, TypeTable};

/// What `emit_fp_sign_bit_op` does to the sign bit.
#[derive(Clone, Copy)]
enum SignBitOp {
    /// Negate (`FNeg`).
    Flip,
    /// Take the magnitude (`Fabs`).
    Clear,
}

impl X86_64CodeGen {
    /// Get size in bits from type, with fallback to provided size.
    fn size_from_type(typ: Option<TypeId>, size: u32, types: &TypeTable) -> u32 {
        typ.map(|t| types.size_bits(t)).unwrap_or(size).max(32)
    }

    /// The register format for a floating-point operand, from its *type*.
    ///
    /// Width alone cannot answer this: x87 extended and IEEE binary128 are
    /// both 128 bits here, and only the type says which. Deriving the format
    /// from the width is what produced `movt`, an instruction that does not
    /// exist.
    /// A complex value is moved as one unit rather than as a scalar of its
    /// base type, so its *width* is what picks the instruction; everything
    /// else is decided by the type.
    pub(super) fn fp_format(&self, typ: Option<TypeId>, size: u32, types: &TypeTable) -> FpSize {
        let width = Self::size_from_type(typ, size, types);
        if typ.is_some_and(|t| types.is_complex_float(t)) {
            return FpSize::from_bits(width, &self.base.target);
        }
        FpSize::from_type_or_bits(typ, width, types, &self.base.target)
    }

    /// Emit a floating-point load operation
    pub(super) fn emit_fp_load(&mut self, insn: &Instruction, types: &TypeTable) {
        let addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };
        let addr_loc = self.get_location(addr);

        // Use type-aware FP size determination
        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types, &self.base.target);

        match addr_loc {
            Loc::Reg(r) => {
                self.push_fp_mem_load(
                    fp_size,
                    MemAddr::BaseOffset {
                        base: r,
                        offset: insn.displacement(),
                    },
                    dst_xmm,
                );
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self.pseudos.is_sym(addr);

                if is_symbol {
                    // Local variable - load directly from stack slot
                    self.push_fp_mem_load(
                        fp_size,
                        self.stack_mem(offset - insn.displacement()),
                        dst_xmm,
                    );
                } else {
                    // Spilled address - load address first, then load from that address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_fp_mem_load(
                        fp_size,
                        MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.displacement(),
                        },
                        dst_xmm,
                    );
                }
            }
            Loc::Global(name) => {
                let src = self.global_mem(&name, insn.displacement(), Reg::R11);
                self.push_fp_mem_load(fp_size, src, dst_xmm);
            }
            _ => {
                // Load address into R11, then load from that address
                self.emit_move(addr, Reg::R11, 64);
                self.push_fp_mem_load(
                    fp_size,
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.displacement(),
                    },
                    dst_xmm,
                );
            }
        }

        // If destination is not the XMM register we loaded to, move it
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(
                dst_xmm,
                &dst_loc,
                self.fp_format(insn.typ, insn.size, types),
            );
        }
    }

    /// Emit a floating-point store operation
    pub(super) fn emit_fp_store(&mut self, insn: &Instruction, types: &TypeTable) {
        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };
        // Use type-aware FP size determination
        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types, &self.base.target);

        // IMPORTANT: Check address location BEFORE emit_fp_move, because emit_fp_move
        // may clobber RAX when loading immediate values. If addr is in RAX, we need
        // to save it to R11 first.
        let addr_loc = self.get_location(addr);
        let addr_reg = match &addr_loc {
            Loc::Reg(Reg::Rax) => {
                // Address is in RAX - move it to R11 before emit_fp_move clobbers it
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Reg(Reg::R11),
                });
                Some(Reg::R11)
            }
            Loc::Reg(r) => Some(*r),
            _ => None,
        };

        // Move value to XMM15 (scratch register) - this may clobber RAX
        self.emit_fp_move(
            value,
            XmmReg::Xmm15,
            self.fp_format(insn.typ, insn.size, types),
        );

        match addr_loc {
            Loc::Reg(_) => {
                // Use the saved register (R11 if it was RAX, otherwise original)
                let r = addr_reg.unwrap_or(Reg::Rax);
                self.push_fp_mem_store(
                    fp_size,
                    XmmReg::Xmm15,
                    MemAddr::BaseOffset {
                        base: r,
                        offset: insn.displacement(),
                    },
                );
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self.pseudos.is_sym(addr);

                if is_symbol {
                    // Local variable - store directly to stack slot
                    self.push_fp_mem_store(
                        fp_size,
                        XmmReg::Xmm15,
                        self.stack_mem(offset - insn.displacement()),
                    );
                } else {
                    // Spilled address - load address first, then store through it
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_fp_mem_store(
                        fp_size,
                        XmmReg::Xmm15,
                        MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.displacement(),
                        },
                    );
                }
            }
            Loc::Global(name) => {
                let dst = self.global_mem(&name, insn.displacement(), Reg::R11);
                self.push_fp_mem_store(fp_size, XmmReg::Xmm15, dst);
            }
            _ => {
                // Load address into R11, then store
                self.emit_move(addr, Reg::R11, 64);
                self.push_fp_mem_store(
                    fp_size,
                    XmmReg::Xmm15,
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.displacement(),
                    },
                );
            }
        }
    }

    /// Load a `size` value from `src` into `dst`.
    ///
    /// binary16 has no SSE2 load of its own width: `movss` reads four bytes,
    /// two of them past the value, which can fault at the end of a page. It is
    /// read as a word into R10 (a codegen scratch register) and moved across,
    /// as gcc does, which also leaves the register's upper bits zero.
    fn push_fp_mem_load(&mut self, size: FpSize, src: MemAddr, dst: XmmReg) {
        if size != FpSize::Half {
            self.push_lir(X86Inst::MovFp {
                size,
                src: XmmOperand::Mem(src),
                dst: XmmOperand::Reg(dst),
            });
            return;
        }
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B16,
            dst_size: OperandSize::B32,
            src: GpOperand::Mem(src),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::MovGpXmm {
            size: OperandSize::B32,
            src: Reg::R10,
            dst,
        });
    }

    /// Store the `size` value in `src` to `dst`.
    ///
    /// binary16 has no SSE2 store of its own width, and `movss` writes four
    /// bytes: storing one half of a `_Float16 _Complex`, or one member of a
    /// struct of `_Float16`s, overwrote the next one. It goes through R10's
    /// low word instead, as gcc does (SSE4.1's `pextrw` to memory is not in
    /// the x86-64 baseline).
    fn push_fp_mem_store(&mut self, size: FpSize, src: XmmReg, dst: MemAddr) {
        if size != FpSize::Half {
            self.push_lir(X86Inst::MovFp {
                size,
                src: XmmOperand::Reg(src),
                dst: XmmOperand::Mem(dst),
            });
            return;
        }
        self.push_lir(X86Inst::MovXmmGp {
            size: OperandSize::B32,
            src,
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B16,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Mem(dst),
        });
    }

    /// Emit floating-point binary operation (addss/addsd, subss/subsd, etc.)
    pub(super) fn emit_fp_binop(&mut self, insn: &Instruction, types: &TypeTable) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        // When the target lives on the stack, we need an XMM scratch
        // register to perform the binop. Must be one outside the
        // allocator's palette (xmm0-xmm13) so we don't clobber a live
        // pseudo. Use xmm15 — xmm14 is used as the fallback scratch
        // elsewhere in this file when dst_xmm == xmm15.
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };
        // Use type-aware FP size determination
        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types, &self.base.target);

        // Helper to emit the FP binop LIR instruction
        let emit_fp_binop_lir = |cg: &mut Self, src: XmmOperand, dst: XmmReg| match insn.op {
            Opcode::FAdd => cg.push_lir(X86Inst::AddFp {
                size: fp_size,
                src,
                dst,
            }),
            Opcode::FSub => cg.push_lir(X86Inst::SubFp {
                size: fp_size,
                src,
                dst,
            }),
            Opcode::FMul => cg.push_lir(X86Inst::MulFp {
                size: fp_size,
                src,
                dst,
            }),
            Opcode::FDiv => cg.push_lir(X86Inst::DivFp {
                size: fp_size,
                src,
                dst,
            }),
            _ => {}
        };

        // Check if src2 is in dst_xmm — if so, moving src1 to dst_xmm would
        // clobber src2. Save src2 to a scratch register first.
        let src2_loc = self.get_location(src2);
        let src2_saved = if let Loc::Xmm(x) = src2_loc {
            if x == dst_xmm {
                let scratch = if dst_xmm == XmmReg::Xmm15 {
                    XmmReg::Xmm14
                } else {
                    XmmReg::Xmm15
                };
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(x),
                    dst: XmmOperand::Reg(scratch),
                });
                Some(scratch)
            } else {
                None
            }
        } else {
            None
        };

        // Move first operand to destination XMM register
        self.emit_fp_move(src1, dst_xmm, self.fp_format(insn.typ, insn.size, types));

        // Apply operation with second operand
        match src2_loc {
            Loc::Xmm(_) if src2_saved.is_some() => {
                emit_fp_binop_lir(self, XmmOperand::Reg(src2_saved.unwrap()), dst_xmm);
            }
            Loc::Xmm(x) => {
                emit_fp_binop_lir(self, XmmOperand::Reg(x), dst_xmm);
            }
            Loc::Stack(offset) => {
                emit_fp_binop_lir(self, XmmOperand::Mem(self.stack_mem(offset)), dst_xmm);
            }
            Loc::FImm(v, _) => {
                // Load float immediate to a scratch register, then operate
                // Use XMM14 if dst is XMM15, otherwise use XMM15
                let scratch = if dst_xmm == XmmReg::Xmm15 {
                    XmmReg::Xmm14
                } else {
                    XmmReg::Xmm15
                };
                self.emit_fp_imm_to_xmm(
                    v,
                    scratch,
                    Self::size_from_type(insn.typ, insn.size, types),
                );
                emit_fp_binop_lir(self, XmmOperand::Reg(scratch), dst_xmm);
            }
            _ => {
                // Move to a scratch register first
                // Use XMM14 if dst is XMM15, otherwise use XMM15
                let scratch = if dst_xmm == XmmReg::Xmm15 {
                    XmmReg::Xmm14
                } else {
                    XmmReg::Xmm15
                };
                self.emit_fp_move(src2, scratch, self.fp_format(insn.typ, insn.size, types));
                emit_fp_binop_lir(self, XmmOperand::Reg(scratch), dst_xmm);
            }
        }

        // Move result to destination if not already there
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(
                dst_xmm,
                &dst_loc,
                self.fp_format(insn.typ, insn.size, types),
            );
        }
    }

    /// Emit floating-point negation: flip the sign bit.
    pub(super) fn emit_fp_neg(&mut self, insn: &Instruction, types: &TypeTable) {
        self.emit_fp_sign_bit_op(insn, types, SignBitOp::Flip);
    }

    /// Emit `Fabs` of a `float` or `double`: clear the sign bit, in place. Only that bit
    /// changes, so `-0.0` becomes `+0.0` and a NaN keeps its payload.
    pub(super) fn emit_fp_abs(&mut self, insn: &Instruction, types: &TypeTable) {
        self.emit_fp_sign_bit_op(insn, types, SignBitOp::Clear);
    }

    /// Emit `Sqrt` of a `float` or `double`: `sqrtss`/`sqrtsd`, correctly
    /// rounded in the current rounding mode.
    pub(super) fn emit_fp_sqrt(&mut self, insn: &Instruction, types: &TypeTable) {
        let (Some(&src), Some(target)) = (insn.src.first(), insn.target) else {
            return;
        };
        let fp_size = self.fp_format(insn.typ, insn.size, types);
        let dst_loc = self.get_location(target);
        // A reserved scratch register when the result lives on the stack; see
        // emit_fp_binop.
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };
        self.emit_fp_move(src, dst_xmm, fp_size);
        self.push_lir(X86Inst::SqrtFp {
            size: fp_size,
            src: XmmOperand::Reg(dst_xmm),
            dst: dst_xmm,
        });
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, fp_size);
        }
    }

    /// Emit `RoundToIntegral` of a `float` or `double` -- `floor`, `ceil`,
    /// `trunc` or `rint` -- with SSE2 alone: gcc's sequences at the x86-64
    /// baseline, which has no `roundsd`.
    ///
    /// A value of magnitude 2^52 (2^23 for a `float`) or more has no
    /// fraction, and is its own answer, as are an infinity and a NaN, which
    /// is returned as it came -- a signalling one too, as gcc's sequence
    /// does. The test is on the biased exponent, in R10, so it raises
    /// nothing. Below that:
    /// - `floor`, `ceil`, `trunc`: the value converted to an integer with
    ///   truncation and back, which is exact, then one subtracted for a
    ///   `floor` that came out above the value or added for a `ceil` that
    ///   came out below it.
    /// - `rint`: 2^52 of the value's own sign added and subtracted, which
    ///   leaves the value rounded in the current direction.
    ///
    /// In both, the sign of the value is then **set** on the result rather
    /// than or-ed into it, as gcc's sequence does: the zero a `floor(0.5)`
    /// computes as `0 - 0` is `-0` when rounding downward, and the sign has
    /// to be the value's in every direction -- `rint(0.5)` is `+0` and
    /// `rint(-0.5)` is `-0`. gcc's sequences assume the default direction
    /// (`-fno-rounding-math`) and get these wrong in the others; so does its
    /// `rint`, which rounds the magnitude and not the value, and this one
    /// does not. `cvttsd2si` raises *inexact* for a fraction, as it does in
    /// gcc's `floor`.
    ///
    /// The value's bits stay in R11 throughout, which is what leaves the two
    /// reserved XMM registers enough: xmm15 holds the value and then the
    /// result, xmm14 the integer being built.
    pub(super) fn emit_fp_round_to_integral(
        &mut self,
        insn: &Instruction,
        how: IntegralRounding,
        types: &TypeTable,
    ) {
        let (Some(&src), Some(target)) = (insn.src.first(), insn.target) else {
            return;
        };
        let fp_size = self.fp_format(insn.typ, insn.size, types);
        let size = Self::fp_bits_size(fp_size);
        // The stored significand's width, and the biased exponent of the
        // format's first power of two with no fraction bits.
        let (mant_bits, bias) = if fp_size == FpSize::Single {
            (23u8, 127i64)
        } else {
            (52u8, 1023i64)
        };
        let top = ShiftCount::Imm((size.bits() - 1) as u8);
        let (x, int) = (XmmReg::Xmm15, XmmReg::Xmm14);

        let uid = self.unique_label_counter;
        self.unique_label_counter += 1;
        let done = Label::block(&self.base.current_fn, 10000 + uid * 2);

        self.emit_fp_move(src, x, fp_size);
        self.emit_xmm_bits_to_gp(x, fp_size, Reg::R11);
        // R10 = the biased exponent: the sign shifted out, then the
        // significand.
        self.push_lir(X86Inst::Mov {
            size,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Shl {
            size,
            count: ShiftCount::Imm(1),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Shr {
            size,
            count: ShiftCount::Imm(mant_bits + 1),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Cmp {
            size,
            src: GpOperand::Imm(bias + i64::from(mant_bits)),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: done.clone(),
        });

        match how {
            IntegralRounding::Rint => {
                // R10 = 2^52 with the value's sign: sign, then the biased
                // exponent of 2^52 below it, shifted up over the significand.
                self.push_lir(X86Inst::Mov {
                    size,
                    src: GpOperand::Reg(Reg::R11),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Shr {
                    size,
                    count: top,
                    dst: Reg::R10,
                });
                let exp_bits = size.bits() as u8 - 1 - mant_bits;
                self.push_lir(X86Inst::Shl {
                    size,
                    count: ShiftCount::Imm(exp_bits),
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Or {
                    size,
                    src: GpOperand::Imm(bias + i64::from(mant_bits)),
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Shl {
                    size,
                    count: ShiftCount::Imm(mant_bits),
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::MovGpXmm {
                    size,
                    src: Reg::R10,
                    dst: int,
                });
                self.push_lir(X86Inst::AddFp {
                    size: fp_size,
                    src: XmmOperand::Reg(int),
                    dst: x,
                });
                self.push_lir(X86Inst::SubFp {
                    size: fp_size,
                    src: XmmOperand::Reg(int),
                    dst: x,
                });
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(x),
                    dst: XmmOperand::Reg(int),
                });
            }
            IntegralRounding::Floor | IntegralRounding::Ceil | IntegralRounding::Trunc => {
                self.push_lir(X86Inst::CvtFpToInt {
                    fp_size,
                    int_size: size,
                    src: XmmOperand::Reg(x),
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::CvtIntToFp {
                    int_size: size,
                    fp_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: int,
                });
                self.emit_fp_integral_step(how, fp_size, x, int);
            }
            IntegralRounding::Round | IntegralRounding::NearbyInt => {
                unreachable!("{how:?} is a call on x86-64 (see computes_in_place)")
            }
        }

        // The result: the integer's magnitude (R10, its sign shifted out and
        // back as zero) with the value's sign (R11, shifted down and back).
        self.emit_xmm_bits_to_gp(int, fp_size, Reg::R10);
        let one = ShiftCount::Imm(1);
        self.push_lir(X86Inst::Shl {
            size,
            count: one,
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Shr {
            size,
            count: one,
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Shr {
            size,
            count: top,
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Shl {
            size,
            count: top,
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Or {
            size,
            src: GpOperand::Reg(Reg::R11),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::MovGpXmm {
            size,
            src: Reg::R10,
            dst: x,
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done)));
        let dst_loc = self.get_location(target);
        self.emit_fp_move_from_xmm(x, &dst_loc, fp_size);
    }

    /// The `floor` or `ceil` correction of the truncated integer in `int`,
    /// against the value in `x`: one less for a `floor` that came out above
    /// the value, one more for a `ceil` that came out below it; nothing for
    /// a `trunc`. The 0 or 1 is built in R10 from the comparison and
    /// converted, overwriting `x`, whose bits are kept in R11.
    fn emit_fp_integral_step(
        &mut self,
        how: IntegralRounding,
        fp_size: FpSize,
        x: XmmReg,
        int: XmmReg,
    ) {
        // `ucomis[sd] src, dst` sets "above" when dst > src.
        let (above, below) = match how {
            IntegralRounding::Floor => (int, x),
            IntegralRounding::Ceil => (x, int),
            _ => return,
        };
        self.push_lir(X86Inst::UComiFp {
            size: fp_size,
            src: XmmOperand::Reg(below),
            dst: above,
        });
        self.push_lir(X86Inst::SetCC {
            cc: CondCode::Ugt,
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(Reg::R10),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::CvtIntToFp {
            int_size: OperandSize::B32,
            fp_size,
            src: GpOperand::Reg(Reg::R10),
            dst: x,
        });
        self.push_lir(if how == IntegralRounding::Floor {
            X86Inst::SubFp {
                size: fp_size,
                src: XmmOperand::Reg(x),
                dst: int,
            }
        } else {
            X86Inst::AddFp {
                size: fp_size,
                src: XmmOperand::Reg(x),
                dst: int,
            }
        });
    }

    /// Flip or clear the sign bit of an SSE float or double: an
    /// `xorps`/`xorpd` or `andps`/`andpd` with a mask built through R10.
    fn emit_fp_sign_bit_op(&mut self, insn: &Instruction, types: &TypeTable, op: SignBitOp) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        // See emit_fp_binop above: fallback must be a reserved XMM (xmm14
        // or xmm15) so we don't clobber any chordal-allocated pseudo when
        // the target lives on the stack.
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };
        // Use type-aware FP size determination
        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types, &self.base.target);

        // Move source to destination
        self.emit_fp_move(src, dst_xmm, self.fp_format(insn.typ, insn.size, types));

        // The sign-bit mask goes in a scratch register that's not dst_xmm.
        let scratch_xmm = if dst_xmm == XmmReg::Xmm15 {
            XmmReg::Xmm14
        } else {
            XmmReg::Xmm15
        };
        // Use R10 (scratch) to avoid clobbering RAX which may hold
        // a live pseudo (the register allocator allocates RAX to pseudos
        // but doesn't know FP operations use it as scratch).
        let single = fp_size == FpSize::Single;
        let sign_bit: u64 = if single { 1 << 31 } else { 1 << 63 };
        let mask = match op {
            SignBitOp::Flip => sign_bit,
            // Every bit below the sign bit.
            SignBitOp::Clear => sign_bit - 1,
        };
        if single {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(mask as i64),
                dst: GpOperand::Reg(Reg::R10),
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B32,
                src: Reg::R10,
                dst: scratch_xmm,
            });
        } else {
            self.push_lir(X86Inst::MovAbs {
                imm: mask as i64,
                dst: Reg::R10,
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B64,
                src: Reg::R10,
                dst: scratch_xmm,
            });
        }
        self.push_lir(match op {
            SignBitOp::Flip => X86Inst::XorFp {
                size: fp_size,
                src: scratch_xmm,
                dst: dst_xmm,
            },
            SignBitOp::Clear => X86Inst::AndFp {
                size: fp_size,
                src: scratch_xmm,
                dst: dst_xmm,
            },
        });

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(
                dst_xmm,
                &dst_loc,
                self.fp_format(insn.typ, insn.size, types),
            );
        }
    }

    /// The bits of the SSE value in `src` into the general register `dst`.
    fn emit_xmm_bits_to_gp(&mut self, src: XmmReg, fp_size: FpSize, dst: Reg) {
        self.push_lir(X86Inst::MovXmmGp {
            size: Self::fp_bits_size(fp_size),
            src,
            dst,
        });
    }

    /// The integer operand size that holds an SSE `float` or `double`.
    fn fp_bits_size(fp_size: FpSize) -> OperandSize {
        if fp_size == FpSize::Single {
            OperandSize::B32
        } else {
            OperandSize::B64
        }
    }

    /// Emit `Signbit` of a `float` or `double`: its bits into R10, shifted
    /// down so the sign bit is the whole answer, 0 or 1.
    pub(super) fn emit_fp_signbit(&mut self, insn: &Instruction, types: &TypeTable) {
        let (Some(&src), Some(target)) = (insn.src.first(), insn.target) else {
            return;
        };
        let fp_size = self.fp_format(insn.src_typ, insn.src_size, types);
        let size = Self::fp_bits_size(fp_size);
        self.emit_fp_move(src, XmmReg::Xmm15, fp_size);
        self.emit_xmm_bits_to_gp(XmmReg::Xmm15, fp_size, Reg::R10);
        self.push_lir(X86Inst::Shr {
            size,
            count: ShiftCount::Imm((size.bits() - 1) as u8),
            dst: Reg::R10,
        });
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, u32::BITS);
    }

    /// Emit `CopySign` of a `float` or `double`: the magnitude bits of the
    /// first operand and the sign bit of the second, in R10 and R11.
    ///
    /// Integer operations, so nothing is computed that could raise, and a
    /// NaN in either operand is only ever moved. Both operands are staged in
    /// the two reserved XMM scratch registers before either general one is
    /// written, because loading an operand may itself go through R10 (an
    /// immediate) or R11 (a global); and both are read before the
    /// destination, which may be either one's register, is written.
    pub(super) fn emit_fp_copysign(&mut self, insn: &Instruction, types: &TypeTable) {
        let (Some(&x), Some(&y), Some(target)) = (insn.src.first(), insn.src.get(1), insn.target)
        else {
            return;
        };
        let fp_size = self.fp_format(insn.typ, insn.size, types);
        let size = Self::fp_bits_size(fp_size);
        let top = ShiftCount::Imm((size.bits() - 1) as u8);
        let one = ShiftCount::Imm(1);

        self.emit_fp_move(x, XmmReg::Xmm15, fp_size);
        self.emit_fp_move(y, XmmReg::Xmm14, fp_size);
        self.emit_xmm_bits_to_gp(XmmReg::Xmm15, fp_size, Reg::R10);
        self.emit_xmm_bits_to_gp(XmmReg::Xmm14, fp_size, Reg::R11);
        // R11 = the sign bit of y, alone.
        self.push_lir(X86Inst::Shr {
            size,
            count: top,
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Shl {
            size,
            count: top,
            dst: Reg::R11,
        });
        // R10 = x with its sign bit cleared.
        self.push_lir(X86Inst::Shl {
            size,
            count: one,
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Shr {
            size,
            count: one,
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Or {
            size,
            src: GpOperand::Reg(Reg::R11),
            dst: Reg::R10,
        });

        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };
        self.push_lir(X86Inst::MovGpXmm {
            size,
            src: Reg::R10,
            dst: dst_xmm,
        });
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, fp_size);
        }
    }

    /// Emit floating-point comparison
    pub(super) fn emit_fp_compare(&mut self, insn: &Instruction, types: &TypeTable) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        // The operands' format; the result is an `int`.
        let (operand, width) = (insn.operand_type(), insn.operand_width());
        let fp_size = FpSize::from_type_or_bits(operand, width, types, &self.base.target);
        let move_size = self.fp_format(operand, width, types);

        // Use Xmm15 as the work register for src1 (Xmm15/Xmm14 are
        // reserved scratch — not in the allocator palette). src2 cannot
        // be in Xmm15 (allocator never picks it), so no aliasing check
        // is needed.

        // Load first operand to XMM15
        self.emit_fp_move(src1, XmmReg::Xmm15, move_size);

        // Compare with second operand using ucomiss/ucomisd
        let src2_loc = self.get_location(src2);
        match src2_loc {
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(x),
                    dst: XmmReg::Xmm15,
                });
            }
            Loc::Stack(offset) => {
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Mem(self.stack_mem(offset)),
                    dst: XmmReg::Xmm15,
                });
            }
            Loc::FImm(v, _) => {
                if move_size == FpSize::Quad {
                    self.emit_quad_const_to_xmm(v, XmmReg::Xmm14);
                } else {
                    self.emit_fp_imm_to_xmm(v, XmmReg::Xmm14, move_size.bits());
                }
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm14),
                    dst: XmmReg::Xmm15,
                });
            }
            _ => {
                self.emit_fp_move(src2, XmmReg::Xmm14, move_size);
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm14),
                    dst: XmmReg::Xmm15,
                });
            }
        }

        // Set result based on comparison type.
        // IEEE 754: ucomisd sets PF=1 for unordered (NaN). Ordered comparisons
        // must check PF to return false when either operand is NaN.
        // - seta/setae already exclude NaN (CF=1 for NaN → seta/setae = 0)
        // - sete/setb/setbe incorrectly return true for NaN (ZF=1 or CF=1)
        // - setne incorrectly returns false for NaN (ZF=1)
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10,
        };

        // IEEE 754: ucomisd sets PF=1 for NaN. Ordered comparisons must
        // exclude NaN by checking PF. seta/setae are already NaN-safe
        // (CF=1 for NaN makes them return 0). sete/setb/setbe/setne need
        // a parity check to handle NaN correctly.
        let scratch = if dst_reg == Reg::R11 {
            Reg::R10
        } else {
            Reg::R11
        };
        match insn.op {
            Opcode::FCmpOEq | Opcode::FCmpOLt | Opcode::FCmpOLe => {
                // result = setcc(dst) AND setnp(scratch)
                let cc = match insn.op {
                    Opcode::FCmpOEq => CondCode::Eq,
                    Opcode::FCmpOLt => CondCode::Ult,
                    Opcode::FCmpOLe => CondCode::Ule,
                    _ => unreachable!(),
                };
                self.push_lir(X86Inst::SetCC { cc, dst: dst_reg });
                self.push_lir(X86Inst::SetCC {
                    cc: CondCode::Np,
                    dst: scratch,
                });
                self.push_lir(X86Inst::And {
                    size: OperandSize::B8,
                    src: GpOperand::Reg(scratch),
                    dst: dst_reg,
                });
            }
            Opcode::FCmpONe => {
                // result = setne(dst) OR setp(scratch)
                self.push_lir(X86Inst::SetCC {
                    cc: CondCode::Ne,
                    dst: dst_reg,
                });
                self.push_lir(X86Inst::SetCC {
                    cc: CondCode::P,
                    dst: scratch,
                });
                self.push_lir(X86Inst::Or {
                    size: OperandSize::B8,
                    src: GpOperand::Reg(scratch),
                    dst: dst_reg,
                });
            }
            Opcode::FCmpOGt => {
                self.push_lir(X86Inst::SetCC {
                    cc: CondCode::Ugt,
                    dst: dst_reg,
                });
            }
            Opcode::FCmpOGe => {
                self.push_lir(X86Inst::SetCC {
                    cc: CondCode::Uge,
                    dst: dst_reg,
                });
            }
            _ => return,
        }

        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(dst_reg),
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, u32::BITS);
        }
    }

    /// Emit integer to float conversion
    pub(super) fn emit_int_to_float(&mut self, insn: &Instruction, types: &TypeTable) {
        // Use type-aware sizing: src_typ is the integer type, typ is the float type
        let src_size = insn
            .src_typ
            .map(|t| types.size_bits(t))
            .unwrap_or(insn.src_size)
            .max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        // Reserved scratch when target lives on the stack (see emit_fp_binop).
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };

        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types, &self.base.target);
        let is_unsigned_64 = insn.op == Opcode::UCvtF && src_size == 64;

        // Move integer to R10 first (scratch register)
        self.emit_move(src, Reg::R10, src_size);

        if is_unsigned_64 {
            // Unsigned 64-bit to float/double: cvtsi2sd treats input as signed,
            // so values >= 2^63 produce wrong results. Use split conversion:
            //   test r10, r10
            //   js .unsigned_path
            //   cvtsi2sd r10, xmm_dst    ; signed path (value < 2^63)
            //   jmp .done
            // .unsigned_path:
            //   mov r10, r11
            //   shr 1, r11               ; val/2
            //   and 1, r10               ; save low bit
            //   or r10, r11              ; (val/2) | (val&1) to avoid rounding loss
            //   cvtsi2sd r11, xmm_dst    ; convert half-value (positive)
            //   addsd xmm_dst, xmm_dst   ; multiply by 2
            // .done:
            let uid = self.unique_label_counter;
            self.unique_label_counter += 1;
            // Use high block_id values (10000+) to avoid colliding with basic block IDs
            let unsigned_label = Label::block(&self.base.current_fn, 10000 + uid * 2);
            let done_label = Label::block(&self.base.current_fn, 10000 + uid * 2 + 1);

            // test r10, r10 — check sign bit
            self.push_lir(X86Inst::Test {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R10),
                dst: GpOperand::Reg(Reg::R10),
            });
            // js .unsigned_path (SF=1 after test means bit 63 set)
            self.push_lir(X86Inst::Jcc {
                cc: CondCode::Slt,
                target: unsigned_label.clone(),
            });
            // Signed path: value < 2^63, cvtsi2sd works directly
            self.push_lir(X86Inst::CvtIntToFp {
                int_size: OperandSize::B64,
                fp_size,
                src: GpOperand::Reg(Reg::R10),
                dst: dst_xmm,
            });
            self.push_lir(X86Inst::Jmp {
                target: done_label.clone(),
            });
            // Unsigned path: value >= 2^63
            self.push_lir(X86Inst::Directive(Directive::BlockLabel(unsigned_label)));
            // mov r10, r11
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R10),
                dst: GpOperand::Reg(Reg::R11),
            });
            // shr $1, r11
            self.push_lir(X86Inst::Shr {
                size: OperandSize::B64,
                count: ShiftCount::Imm(1),
                dst: Reg::R11,
            });
            // and $1, r10
            self.push_lir(X86Inst::And {
                size: OperandSize::B64,
                src: GpOperand::Imm(1),
                dst: Reg::R10,
            });
            // or r10, r11
            self.push_lir(X86Inst::Or {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R10),
                dst: Reg::R11,
            });
            // cvtsi2sd r11, xmm_dst
            self.push_lir(X86Inst::CvtIntToFp {
                int_size: OperandSize::B64,
                fp_size,
                src: GpOperand::Reg(Reg::R11),
                dst: dst_xmm,
            });
            // addsd xmm_dst, xmm_dst (double the value)
            // addsd/addss xmm_dst, xmm_dst (double the value)
            self.push_lir(X86Inst::AddFp {
                size: fp_size,
                src: XmmOperand::Reg(dst_xmm),
                dst: dst_xmm,
            });
            self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
        } else {
            // Signed conversion, or unsigned 32-bit (zero-extended to 64-bit fits signed)
            let int_size = if insn.op == Opcode::UCvtF && src_size == 32 {
                // Zero-extend 32-bit unsigned to 64-bit signed for correct conversion
                OperandSize::B64
            } else {
                OperandSize::from_bits(src_size)
            };
            self.push_lir(X86Inst::CvtIntToFp {
                int_size,
                fp_size,
                src: GpOperand::Reg(Reg::R10),
                dst: dst_xmm,
            });
        }

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(
                dst_xmm,
                &dst_loc,
                self.fp_format(insn.typ, insn.size, types),
            );
        }
    }

    /// Emit float to integer conversion
    pub(super) fn emit_float_to_int(&mut self, insn: &Instruction, types: &TypeTable) {
        // Use type-aware sizing: src_typ is the float type, typ is the integer type
        let dst_typ = insn.typ.expect("float-to-int conversion must have typ");
        let dst_size = types.size_bits(dst_typ).max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Move float to XMM15 (reserved scratch — see emit_fp_binop).
        let fp_size =
            FpSize::from_type_or_bits(insn.src_typ, insn.src_size, types, &self.base.target);
        self.emit_fp_move(
            src,
            XmmReg::Xmm15,
            self.fp_format(insn.src_typ, insn.src_size, types),
        );

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };

        // Signedness is the opcode's. `linearize_cast` and `emit_convert` both
        // choose `FCvtU` from `is_unsigned(dst_typ)`, so asking the opcode asks
        // the same question one step closer to the answer, and it is the
        // question `emit_int_to_float` already asks for the other direction.
        let is_unsigned = insn.op == Opcode::FCvtU;

        if is_unsigned && dst_size == 64 && matches!(fp_size, FpSize::Single | FpSize::Double) {
            // cvttss2si/cvttsd2si are *signed*: every value at or above 2^63
            // overflows and yields the "integer indefinite" 0x8000000000000000,
            // so the whole upper half of the unsigned range came back wrong.
            // Split the range, mirroring emit_int_to_float's unsigned path.
            self.emit_float_to_u64(fp_size, dst_reg);
        } else {
            // Convert using cvttss2si/cvttsd2si (truncate toward zero).
            // For unsigned 32-bit targets, use 64-bit conversion to avoid
            // overflow for values >= 2^31 that fit in uint32_t but not int32_t.
            // A Half source cannot reach 2^63 at all, so it needs no split.
            let int_size = if is_unsigned && dst_size == 32 {
                OperandSize::B64 // cvttsd2siq then truncate
            } else {
                OperandSize::from_bits(dst_size)
            };
            self.push_lir(X86Inst::CvtFpToInt {
                fp_size,
                int_size,
                src: XmmOperand::Reg(XmmReg::Xmm15),
                dst: dst_reg,
            });
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, dst_size);
        }
    }

    /// Convert the float in XMM15 to a 64-bit *unsigned* integer in `dst`.
    ///
    /// x86-64 has no unsigned float-to-integer instruction: `cvttsd2si` reads
    /// its result as signed, so anything at or above 2^63 overflows and gives
    /// the "integer indefinite" 0x8000000000000000. The range has to be split:
    ///
    /// ```text
    ///     movsd  $2^63, %xmm14
    ///     ucomisd %xmm14, %xmm15
    ///     jae    .big
    ///     cvttsd2si %xmm15, dst        # value < 2^63, or negative, or NaN
    ///     jmp    .done
    ///   .big:
    ///     subsd  %xmm14, %xmm15        # bring it into the signed range
    ///     cvttsd2si %xmm15, dst
    ///     movabs $1<<63, %r11
    ///     xor    %r11, dst             # put the bit back
    ///   .done:
    /// ```
    ///
    /// The subtraction is exact: 2^63 is a power of two, and every value in
    /// [2^63, 2^64) has an exponent at least that of 2^63, so no bit of the
    /// significand is lost. A NaN compares unordered, which sets CF and so
    /// takes the `jae`-not-taken path — the conversion is undefined there and
    /// yields the same indefinite gcc produces.
    ///
    /// This mirrors `emit_int_to_float`'s unsigned path, which is the same
    /// problem in the other direction.
    fn emit_float_to_u64(&mut self, fp_size: FpSize, dst: Reg) {
        debug_assert_ne!(
            dst,
            Reg::R11,
            "R11 is codegen scratch and is clobbered here"
        );

        let uid = self.unique_label_counter;
        self.unique_label_counter += 1;
        // High block_id values, as emit_int_to_float does, so these cannot
        // collide with a basic block's own label.
        let big_label = Label::block(&self.base.current_fn, 10000 + uid * 2);
        let done_label = Label::block(&self.base.current_fn, 10000 + uid * 2 + 1);

        // 2^63 is exactly representable in both float and double.
        const TWO_POW_63: f64 = 9223372036854775808.0;
        self.emit_fp_imm_to_xmm(
            FloatVal::from_f64(TWO_POW_63),
            XmmReg::Xmm14,
            fp_size.bits(),
        );

        self.push_lir(X86Inst::UComiFp {
            size: fp_size,
            src: XmmOperand::Reg(XmmReg::Xmm14),
            dst: XmmReg::Xmm15,
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: big_label.clone(),
        });

        // Below 2^63: the signed conversion is already the right answer.
        self.push_lir(X86Inst::CvtFpToInt {
            fp_size,
            int_size: OperandSize::B64,
            src: XmmOperand::Reg(XmmReg::Xmm15),
            dst,
        });
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(big_label)));
        self.push_lir(X86Inst::SubFp {
            size: fp_size,
            src: XmmOperand::Reg(XmmReg::Xmm14),
            dst: XmmReg::Xmm15,
        });
        self.push_lir(X86Inst::CvtFpToInt {
            fp_size,
            int_size: OperandSize::B64,
            src: XmmOperand::Reg(XmmReg::Xmm15),
            dst,
        });
        // `xor` rather than `add`: the ALU immediate is 32 bits, and the bit
        // is known to be clear after subtracting 2^63.
        self.push_lir(X86Inst::MovAbs {
            imm: i64::MIN,
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Xor {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst,
        });
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }

    /// Emit float to float conversion (e.g., float to double)
    pub(super) fn emit_float_to_float(&mut self, insn: &Instruction, types: &TypeTable) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        // Reserved scratch when target lives on the stack (see emit_fp_binop).
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };

        // Scratch for holding the source value before the conversion.
        // Must not collide with dst_xmm; use Xmm14 when dst is Xmm15.
        let src_xmm = if dst_xmm == XmmReg::Xmm15 {
            XmmReg::Xmm14
        } else {
            XmmReg::Xmm15
        };

        // Move source to scratch
        self.emit_fp_move(
            src,
            src_xmm,
            self.fp_format(insn.src_typ, insn.src_size, types),
        );

        // Check types directly to determine conversion needed
        let src_kind = insn.src_typ.map(|t| types.kind(t));
        let dst_kind = insn.typ.map(|t| types.kind(t));

        match (src_kind, dst_kind) {
            (Some(TypeKind::Float), Some(TypeKind::Double | TypeKind::LongDouble)) => {
                // float to double: cvtss2sd
                self.push_lir(X86Inst::CvtFpFp {
                    src_size: FpSize::Single,
                    dst_size: FpSize::Double,
                    src: src_xmm,
                    dst: dst_xmm,
                });
            }
            (Some(TypeKind::Double | TypeKind::LongDouble), Some(TypeKind::Float)) => {
                // double to float: cvtsd2ss
                self.push_lir(X86Inst::CvtFpFp {
                    src_size: FpSize::Double,
                    dst_size: FpSize::Single,
                    src: src_xmm,
                    dst: dst_xmm,
                });
            }
            _ => {
                // Same type or types unknown, just move if needed
                if dst_xmm != src_xmm {
                    let dst_fp_size =
                        FpSize::from_type_or_bits(insn.typ, insn.size, types, &self.base.target);
                    self.push_lir(X86Inst::MovFp {
                        size: dst_fp_size,
                        src: XmmOperand::Reg(src_xmm),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                }
            }
        }

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(
                dst_xmm,
                &dst_loc,
                self.fp_format(insn.typ, insn.size, types),
            );
        }
    }

    /// Load a floating-point constant into an XMM register
    pub(super) fn emit_fp_const_load(&mut self, target: PseudoId, value: FloatVal, size: FpSize) {
        let dst_loc = self.get_location(target);
        // Reserved scratch when target lives on the stack (see emit_fp_binop).
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };

        if size == FpSize::Quad {
            self.emit_quad_const_to_xmm(value, dst_xmm);
        } else {
            self.emit_fp_imm_to_xmm(value, dst_xmm, size.bits());
        }

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Load a binary128 constant from the `.rodata` pool.
    ///
    /// There is no immediate form and no general register wide enough, so the
    /// value is interned and loaded. Narrowing it through `f64` first — which
    /// is what the scalar immediate path does — lost 60 of its significand
    /// bits.
    fn emit_quad_const_to_xmm(&mut self, value: FloatVal, xmm: XmmReg) {
        let (lo, hi) = value.to_f128_bits();
        let mut bytes = [0u8; 16];
        bytes[..8].copy_from_slice(&lo.to_le_bytes());
        bytes[8..].copy_from_slice(&hi.to_le_bytes());
        // Keyed on the binary128 image, which is what is being pooled.
        // `pool_key` is the *x87* encoding, and everything below x87's
        // smallest subnormal collapses to zero there -- so `0x1p-16494q` and
        // `0.0q` shared one entry, and whichever was interned last won.
        let key = ((hi as u128) << 64) | lo as u128;
        self.quad_constants.insert(key, bytes);
        let label = crate::arch::lir::internal_label("quad_const", key);
        self.push_lir(X86Inst::MovFp {
            size: FpSize::Quad,
            src: XmmOperand::Mem(MemAddr::RipRelative(crate::arch::lir::Symbol {
                name: label,
                is_local: true,
                is_extern: false,
            })),
            dst: XmmOperand::Reg(xmm),
        });
    }

    /// Load a float immediate value into an XMM register
    ///
    /// XMM holds only `_Float16`, `float` and `double`; an x87 80-bit or a
    /// binary128 constant never reaches here. The bits are the value's own
    /// encoding at `size`, taken from the exact value.
    pub(super) fn emit_fp_imm_to_xmm(&mut self, value: FloatVal, xmm: XmmReg, size: u32) {
        // `is_positive_zero`, not `is_zero`: the shortcut below produces
        // `+0.0`, and `-0.0` is a different value with the same magnitude.
        // C equates the two under `==` but not under `signbit`, and
        // `copysign(1.0, -0.0)` is `-1.0`.
        if value.is_positive_zero() {
            // Use xorps/xorpd to zero the register (faster)
            let fp_size = FpSize::from_bits(size, &self.base.target);
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: xmm,
                dst: xmm,
            });
            return;
        }
        // Loaded through an integer register (R10 scratch).
        let bits = value.to_bits_at_width(size);
        let width = if size <= 32 {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(bits),
                dst: GpOperand::Reg(Reg::R10),
            });
            OperandSize::B32
        } else {
            self.push_lir(X86Inst::MovAbs {
                imm: bits,
                dst: Reg::R10,
            });
            OperandSize::B64
        };
        self.push_lir(X86Inst::MovGpXmm {
            size: width,
            src: Reg::R10,
            dst: xmm,
        });
    }

    /// Move a value to an XMM register
    pub(super) fn emit_fp_move(&mut self, src: PseudoId, dst: XmmReg, fp_size: FpSize) {
        let src_loc = self.get_location(src);
        // The format is decided by the caller, from the *type*. Width alone
        // cannot tell x87 extended from binary128 here: this compiler sizes
        // both at 128 bits, and asking `from_bits` produced `movt`.
        let size = fp_size.bits();

        match src_loc {
            Loc::Xmm(x) if x == dst => {
                // Already in destination
            }
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(x),
                    dst: XmmOperand::Reg(dst),
                });
            }
            Loc::Stack(offset) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(self.stack_mem(offset)),
                    dst: XmmOperand::Reg(dst),
                });
            }
            Loc::IncomingArg(offset) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset,
                    }),
                    dst: XmmOperand::Reg(dst),
                });
            }
            Loc::FImm(v, imm_size) => {
                // A binary128 constant has no immediate form; it comes from
                // the pool. The caller's format decides, since the FImm's own
                // width cannot tell binary128 from x87 extended.
                if fp_size == FpSize::Quad {
                    self.emit_quad_const_to_xmm(v, dst);
                } else {
                    // Use the size from the FImm, not the passed-in size
                    // This ensures float constants are loaded as float, not double
                    self.emit_fp_imm_to_xmm(v, dst, imm_size);
                }
            }
            Loc::Reg(r) => {
                // Move from GP register to XMM (unusual but possible)
                self.push_lir(X86Inst::MovGpXmm {
                    size: OperandSize::B64,
                    src: r,
                    dst,
                });
            }
            Loc::Imm(v) => {
                let v = v as i64;
                // Integer immediate to float — use R10 (scratch) to avoid clobbering
                // RAX which is allocatable and may hold a live pseudo
                if size <= 32 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(v),
                        dst: GpOperand::Reg(Reg::R10),
                    });
                    self.push_lir(X86Inst::MovGpXmm {
                        size: OperandSize::B32,
                        src: Reg::R10,
                        dst,
                    });
                } else {
                    self.push_lir(X86Inst::MovAbs {
                        imm: v,
                        dst: Reg::R10,
                    });
                    self.push_lir(X86Inst::MovGpXmm {
                        size: OperandSize::B64,
                        src: Reg::R10,
                        dst,
                    });
                }
            }
            Loc::Global(name) => {
                let src = self.global_mem(&name, 0, Reg::R11);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(src),
                    dst: XmmOperand::Reg(dst),
                });
            }
        }
    }

    /// Move from XMM register to a location
    pub(super) fn emit_fp_move_from_xmm(&mut self, src: XmmReg, dst: &Loc, fp_size: FpSize) {
        match dst {
            Loc::Xmm(x) if *x == src => {
                // Already in destination
            }
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(src),
                    dst: XmmOperand::Reg(*x),
                });
            }
            Loc::Stack(offset) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(src),
                    dst: XmmOperand::Mem(self.stack_mem(*offset)),
                });
            }
            Loc::Reg(r) => {
                // Move from XMM to GP register
                self.push_lir(X86Inst::MovXmmGp {
                    size: OperandSize::B64,
                    src,
                    dst: *r,
                });
            }
            _ => {}
        }
    }

    /// Helper for emit_va_arg: emit float path for va_arg
    /// ap_base: base register for va_list access
    /// ap_base_offset: added to all va_list field offsets (0 for Reg, ap_offset for Stack)
    pub(super) fn emit_va_arg_float(
        &mut self,
        ap_base: Reg,
        ap_base_offset: i32,
        dst_loc: &Loc,
        arg_type: TypeId,
        label_suffix: u32,
        types: &TypeTable,
    ) {
        let overflow_label = Label::internal("va_fp_overflow", label_suffix);
        let done_label = Label::internal("va_fp_done", label_suffix);

        let fp_size = types.size_bits(arg_type);
        // `__float128` occupies a whole XMM register and a sixteen-byte slot.
        // Moving it as a `Double` copies its low half and leaves the exponent
        // and the top of the mantissa behind.
        let lir_fp_size = if fp_size <= 32 {
            FpSize::Single
        } else if fp_size <= 64 {
            FpSize::Double
        } else {
            FpSize::Quad
        };
        // How far the overflow area advances, and how far the register save
        // area's cursor does. Both are the argument's slot, which is eight
        // bytes for a float or double and sixteen for a binary128.
        let slot_bytes: i64 = if fp_size > 64 { 16 } else { 8 };

        // Load fp_offset from va_list (at offset 4)
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 4,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });
        // Compare with 176 (end of XMM save area)
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B32,
            src: GpOperand::Imm(176),
            dst: GpOperand::Reg(Reg::Rax),
        });
        // Jump if above or equal (fp_offset >= 176 means use overflow)
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: overflow_label.clone(),
        });

        // Register save area path: load from reg_save_area + fp_offset
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 16,
            }),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Movsx {
            src_size: OperandSize::B32,
            dst_size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rax),
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: Reg::R10,
        });

        // Load float value from save area
        self.push_lir(X86Inst::MovFp {
            size: lir_fp_size,
            src: XmmOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R10,
                offset: 0,
            }),
            dst: XmmOperand::Reg(XmmReg::Xmm15),
        });

        // Store XMM15 to destination
        match dst_loc {
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovFp {
                    size: lir_fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Reg(*x),
                });
            }
            Loc::Stack(dst_offset) => {
                self.push_lir(X86Inst::MovFp {
                    size: lir_fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(self.stack_mem(*dst_offset)),
                });
            }
            _ => {}
        }

        // Increment fp_offset by 16 (XMM slot size)
        self.push_lir(X86Inst::Add {
            size: OperandSize::B32,
            src: GpOperand::Imm(16),
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 4,
            }),
        });
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        // Overflow path: use overflow_arg_area
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });

        self.push_lir(X86Inst::MovFp {
            size: lir_fp_size,
            src: XmmOperand::Mem(MemAddr::BaseOffset {
                base: Reg::Rax,
                offset: 0,
            }),
            dst: XmmOperand::Reg(XmmReg::Xmm15),
        });

        // Store XMM15 to destination
        match dst_loc {
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovFp {
                    size: lir_fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Reg(*x),
                });
            }
            Loc::Stack(dst_offset) => {
                self.push_lir(X86Inst::MovFp {
                    size: lir_fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(self.stack_mem(*dst_offset)),
                });
            }
            _ => {}
        }

        // Advance overflow_arg_area by 8
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(slot_bytes),
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }
}
