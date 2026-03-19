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
use crate::arch::lir::{CondCode, Directive, FpSize, Label, OperandSize, Symbol};
use crate::ir::{Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeKind, TypeTable};

impl X86_64CodeGen {
    /// Get size in bits from type, with fallback to provided size.
    fn size_from_type(typ: Option<TypeId>, size: u32, types: &TypeTable) -> u32 {
        typ.map(|t| types.size_bits(t)).unwrap_or(size).max(32)
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
        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types);

        match addr_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    }),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - load directly from stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -total_offset,
                        }),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                } else {
                    // Spilled address - load address first, then load from that address
                    let adjusted = offset + self.callee_saved_offset;
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                }
            }
            Loc::Global(name) => {
                if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then load FP value
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                } else {
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::RipRelative(Symbol {
                            name: name.to_string(),
                            is_local: false,
                            is_extern: false,
                        })),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                }
            }
            _ => {
                // Load address into R11, then load from that address
                self.emit_move(addr, Reg::R11, 64);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
        }

        // If destination is not the XMM register we loaded to, move it
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(
                dst_xmm,
                &dst_loc,
                Self::size_from_type(insn.typ, insn.size, types),
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
        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types);

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
            Self::size_from_type(insn.typ, insn.size, types),
        );

        match addr_loc {
            Loc::Reg(_) => {
                // Use the saved register (R11 if it was RAX, otherwise original)
                let r = addr_reg.unwrap_or(Reg::Rax);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    }),
                });
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - store directly to stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm15),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -total_offset,
                        }),
                    });
                } else {
                    // Spilled address - load address first, then store through it
                    let adjusted = offset + self.callee_saved_offset;
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm15),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                    });
                }
            }
            Loc::Global(name) => {
                if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then store FP value
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm15),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                    });
                } else {
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm15),
                        dst: XmmOperand::Mem(MemAddr::RipRelative(Symbol {
                            name: name.to_string(),
                            is_local: false,
                            is_extern: false,
                        })),
                    });
                }
            }
            _ => {
                // Load address into R11, then store
                self.emit_move(addr, Reg::R11, 64);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }),
                });
            }
        }
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
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0, // Use XMM0 as work register
        };
        // Use type-aware FP size determination
        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types);

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
        self.emit_fp_move(
            src1,
            dst_xmm,
            Self::size_from_type(insn.typ, insn.size, types),
        );

        // Apply operation with second operand
        match src2_loc {
            Loc::Xmm(_) if src2_saved.is_some() => {
                emit_fp_binop_lir(self, XmmOperand::Reg(src2_saved.unwrap()), dst_xmm);
            }
            Loc::Xmm(x) => {
                emit_fp_binop_lir(self, XmmOperand::Reg(x), dst_xmm);
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                emit_fp_binop_lir(
                    self,
                    XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst_xmm,
                );
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
                self.emit_fp_move(
                    src2,
                    scratch,
                    Self::size_from_type(insn.typ, insn.size, types),
                );
                emit_fp_binop_lir(self, XmmOperand::Reg(scratch), dst_xmm);
            }
        }

        // Move result to destination if not already there
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(
                dst_xmm,
                &dst_loc,
                Self::size_from_type(insn.typ, insn.size, types),
            );
        }
    }

    /// Emit floating-point negation
    pub(super) fn emit_fp_neg(&mut self, insn: &Instruction, types: &TypeTable) {
        let src = match insn.src.first() {
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
            _ => XmmReg::Xmm0,
        };
        // Use type-aware FP size determination
        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types);

        // Move source to destination
        self.emit_fp_move(
            src,
            dst_xmm,
            Self::size_from_type(insn.typ, insn.size, types),
        );

        // XOR with sign bit mask to negate
        // For float: 0x80000000, for double: 0x8000000000000000
        // Use a scratch register that's not dst_xmm to hold the sign mask
        let scratch_xmm = if dst_xmm == XmmReg::Xmm15 {
            XmmReg::Xmm14
        } else {
            XmmReg::Xmm15
        };
        // Use R10 (scratch) to avoid clobbering RAX which may hold
        // a live pseudo (the register allocator allocates RAX to pseudos
        // but doesn't know FP operations use it as scratch).
        if fp_size == FpSize::Single {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(0x80000000),
                dst: GpOperand::Reg(Reg::R10),
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B32,
                src: Reg::R10,
                dst: scratch_xmm,
            });
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: scratch_xmm,
                dst: dst_xmm,
            });
        } else {
            self.push_lir(X86Inst::MovAbs {
                imm: 0x8000000000000000u64 as i64,
                dst: Reg::R10,
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B64,
                src: Reg::R10,
                dst: scratch_xmm,
            });
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: scratch_xmm,
                dst: dst_xmm,
            });
        }

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(
                dst_xmm,
                &dst_loc,
                Self::size_from_type(insn.typ, insn.size, types),
            );
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
        // Use type-aware FP size determination
        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types);
        let move_size = Self::size_from_type(insn.typ, insn.size, types);

        // Check if src2 is in Xmm0 before we clobber it with src1.
        // If so, move src2 to Xmm15 first to avoid losing its value.
        let src2_loc = self.get_location(src2);
        if matches!(src2_loc, Loc::Xmm(XmmReg::Xmm0)) {
            self.push_lir(X86Inst::MovFp {
                size: fp_size,
                src: XmmOperand::Reg(XmmReg::Xmm0),
                dst: XmmOperand::Reg(XmmReg::Xmm15),
            });
        }

        // Load first operand to XMM0
        self.emit_fp_move(src1, XmmReg::Xmm0, move_size);

        // Compare with second operand using ucomiss/ucomisd
        // Re-read src2 location; if it was Xmm0, it's now in Xmm15.
        let src2_loc = self.get_location(src2);
        match src2_loc {
            Loc::Xmm(x) => {
                let actual_reg = if x == XmmReg::Xmm0 { XmmReg::Xmm15 } else { x };
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(actual_reg),
                    dst: XmmReg::Xmm0,
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst: XmmReg::Xmm0,
                });
            }
            Loc::FImm(v, _) => {
                self.emit_fp_imm_to_xmm(v, XmmReg::Xmm15, move_size);
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmReg::Xmm0,
                });
            }
            _ => {
                self.emit_fp_move(src2, XmmReg::Xmm15, move_size);
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmReg::Xmm0,
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
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };

        let fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types);
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
            let unsigned_label = Label::new(&self.base.current_fn, 10000 + uid * 2);
            let done_label = Label::new(&self.base.current_fn, 10000 + uid * 2 + 1);

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
                Self::size_from_type(insn.typ, insn.size, types),
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

        // Move float to XMM0 using type-aware source FP size
        let fp_size = FpSize::from_type_or_bits(insn.src_typ, insn.src_size, types);
        self.emit_fp_move(
            src,
            XmmReg::Xmm0,
            Self::size_from_type(insn.src_typ, insn.src_size, types),
        );

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };

        // Convert using cvttss2si/cvttsd2si (truncate toward zero)
        let int_size = OperandSize::from_bits(dst_size);
        self.push_lir(X86Inst::CvtFpToInt {
            fp_size,
            int_size,
            src: XmmOperand::Reg(XmmReg::Xmm0),
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, dst_size);
        }
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
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };

        // Move source to XMM0
        self.emit_fp_move(
            src,
            XmmReg::Xmm0,
            Self::size_from_type(insn.src_typ, insn.src_size, types),
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
                    src: XmmReg::Xmm0,
                    dst: dst_xmm,
                });
            }
            (Some(TypeKind::Double | TypeKind::LongDouble), Some(TypeKind::Float)) => {
                // double to float: cvtsd2ss
                self.push_lir(X86Inst::CvtFpFp {
                    src_size: FpSize::Double,
                    dst_size: FpSize::Single,
                    src: XmmReg::Xmm0,
                    dst: dst_xmm,
                });
            }
            _ => {
                // Same type or types unknown, just move if needed
                if dst_xmm != XmmReg::Xmm0 {
                    let dst_fp_size = FpSize::from_type_or_bits(insn.typ, insn.size, types);
                    self.push_lir(X86Inst::MovFp {
                        size: dst_fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm0),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                }
            }
        }

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(
                dst_xmm,
                &dst_loc,
                Self::size_from_type(insn.typ, insn.size, types),
            );
        }
    }

    /// Load a floating-point constant into an XMM register
    pub(super) fn emit_fp_const_load(&mut self, target: PseudoId, value: f64, size: u32) {
        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };

        self.emit_fp_imm_to_xmm(value, dst_xmm, size);

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Load a float immediate value into an XMM register
    pub(super) fn emit_fp_imm_to_xmm(&mut self, value: f64, xmm: XmmReg, size: u32) {
        if value == 0.0 {
            // Use xorps/xorpd to zero the register (faster)
            let fp_size = FpSize::from_bits(size);
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: xmm,
                dst: xmm,
            });
        } else if size == 16 {
            // Float16: load via integer register (use R10 scratch)
            let bits = f64_to_f16_bits(value);
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(bits as i64),
                dst: GpOperand::Reg(Reg::R10),
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B32,
                src: Reg::R10,
                dst: xmm,
            });
        } else if size == 32 {
            // Float: load via integer register (use R10 scratch)
            let bits = (value as f32).to_bits();
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(bits as i64),
                dst: GpOperand::Reg(Reg::R10),
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B32,
                src: Reg::R10,
                dst: xmm,
            });
        } else {
            // Double: load via integer register (use R10 scratch)
            let bits = value.to_bits();
            self.push_lir(X86Inst::MovAbs {
                imm: bits as i64,
                dst: Reg::R10,
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B64,
                src: Reg::R10,
                dst: xmm,
            });
        }
    }

    /// Move a value to an XMM register
    pub(super) fn emit_fp_move(&mut self, src: PseudoId, dst: XmmReg, size: u32) {
        let src_loc = self.get_location(src);
        let fp_size = FpSize::from_bits(size);

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
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
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
                // Use the size from the FImm, not the passed-in size
                // This ensures float constants are loaded as float, not double
                self.emit_fp_imm_to_xmm(v, dst, imm_size);
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
                if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then load FP value
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: 0,
                        }),
                        dst: XmmOperand::Reg(dst),
                    });
                } else {
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                        dst: XmmOperand::Reg(dst),
                    });
                }
            }
        }
    }

    /// Move from XMM register to a location
    pub(super) fn emit_fp_move_from_xmm(&mut self, src: XmmReg, dst: &Loc, size: u32) {
        let fp_size = FpSize::from_bits(size);

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
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(src),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
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
        let overflow_label = Label::new("va_fp_overflow", label_suffix);
        let done_label = Label::new("va_fp_done", label_suffix);

        let fp_size = types.size_bits(arg_type);
        let lir_fp_size = if fp_size <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        };

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
                let adjusted_offset = -(*dst_offset + self.callee_saved_offset);
                self.push_lir(X86Inst::MovFp {
                    size: lir_fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: adjusted_offset,
                    }),
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
                let adjusted_offset = -(*dst_offset + self.callee_saved_offset);
                self.push_lir(X86Inst::MovFp {
                    size: lir_fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: adjusted_offset,
                    }),
                });
            }
            _ => {}
        }

        // Advance overflow_arg_area by 8
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(8),
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

/// Convert f64 to IEEE 754 half-precision (binary16) bits.
/// This handles the conversion from 64-bit double to 16-bit half precision.
pub fn f64_to_f16_bits(val: f64) -> u16 {
    let bits = val.to_bits();
    let sign = ((bits >> 63) & 1) as u16;
    let exp = ((bits >> 52) & 0x7FF) as i32;
    let frac = bits & 0xFFFFFFFFFFFFF;

    // Handle special cases
    if exp == 0x7FF {
        // NaN or Infinity
        if frac != 0 {
            // NaN: preserve some mantissa bits
            return (sign << 15) | 0x7E00 | ((frac >> 42) as u16 & 0x1FF);
        } else {
            // Infinity
            return (sign << 15) | 0x7C00;
        }
    }

    // Rebias exponent: f64 bias is 1023, f16 bias is 15
    let new_exp = exp - 1023 + 15;

    if new_exp >= 31 {
        // Overflow to infinity
        return (sign << 15) | 0x7C00;
    }

    if new_exp <= 0 {
        // Denormal or zero
        if new_exp < -10 {
            // Too small, flush to zero
            return sign << 15;
        }
        // Denormal: shift mantissa right
        let shift = 1 - new_exp;
        let frac_with_hidden = frac | 0x10000000000000; // Add hidden bit
        let shifted = frac_with_hidden >> (42 + shift);
        return (sign << 15) | (shifted as u16 & 0x3FF);
    }

    // Normal number: truncate mantissa from 52 bits to 10 bits
    let new_frac = (frac >> 42) as u16;
    (sign << 15) | ((new_exp as u16) << 10) | (new_frac & 0x3FF)
}
