//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Expression Code Generation (Binary and Unary Operations)
//

use super::codegen::X86_64CodeGen;
use super::lir::{GpOperand, MemAddr, ShiftCount, X86Inst};
use super::regalloc::{Loc, Reg};
use crate::arch::codegen::UnaryOp;
use crate::arch::lir::{CondCode, Directive, Label, OperandSize};
use crate::ir::{Instruction, Opcode, PseudoId};
use crate::types::{TypeKind, TypeTable};

impl X86_64CodeGen {
    pub(super) fn emit_binop(&mut self, insn: &Instruction, types: &TypeTable) {
        let size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));
        if insn.typ.is_some_and(|t| types.kind(t) == TypeKind::Int128) {
            self.emit_int128_binop(insn);
            return;
        }
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        // Use R10 (reserved scratch register, never allocated) when dst is on stack
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10,
        };

        if matches!(insn.op, Opcode::Shl | Opcode::Lsr | Opcode::Asr) {
            let src2_loc = self.get_location(src2);
            match src2_loc {
                Loc::Imm(v) => {
                    // Immediate shift - can use work_reg directly
                    self.emit_move(src1, work_reg, size);
                    // LIR: shift by immediate
                    let shift_count = ShiftCount::Imm(v as u8);
                    match insn.op {
                        Opcode::Shl => self.push_lir(X86Inst::Shl {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        Opcode::Lsr => self.push_lir(X86Inst::Shr {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        Opcode::Asr => self.push_lir(X86Inst::Sar {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        _ => {}
                    }
                }
                _ => {
                    // Variable shift - shift count goes in Rcx/Cl
                    // IMPORTANT: If work_reg is Rcx, we can't use it for the value
                    // because emit_move(src2, Rcx) will clobber it. Use R10 instead.
                    let shift_reg = if work_reg == Reg::Rcx {
                        Reg::R10
                    } else {
                        work_reg
                    };
                    self.emit_move(src1, shift_reg, size);
                    self.emit_move(src2, Reg::Rcx, 8);
                    // LIR: shift by %cl
                    let shift_count = ShiftCount::Cl;
                    match insn.op {
                        Opcode::Shl => self.push_lir(X86Inst::Shl {
                            size: op_size,
                            count: shift_count,
                            dst: shift_reg,
                        }),
                        Opcode::Lsr => self.push_lir(X86Inst::Shr {
                            size: op_size,
                            count: shift_count,
                            dst: shift_reg,
                        }),
                        Opcode::Asr => self.push_lir(X86Inst::Sar {
                            size: op_size,
                            count: shift_count,
                            dst: shift_reg,
                        }),
                        _ => {}
                    }
                    // If we used R10, move result back to work_reg (Rcx)
                    if shift_reg != work_reg {
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Reg(shift_reg),
                            dst: GpOperand::Reg(work_reg),
                        });
                    }
                }
            }
        } else {
            // Non-shift binary ops: move src1 into work_reg first
            self.emit_move(src1, work_reg, size);
            let src2_loc = self.get_location(src2);
            // For 64-bit operations, check if immediate fits in 32-bit signed range.
            // x86-64 binary ops (add, sub, and, or, xor) only support 32-bit sign-extended immediates.
            // If the immediate doesn't fit, load it into R11 first.
            let src2_gp = match &src2_loc {
                Loc::Imm(v)
                    if size == 64
                        && (*v as i64 > i32::MAX as i64 || (*v as i64) < i32::MIN as i64) =>
                {
                    // Large 64-bit immediate - load into R11 first
                    self.push_lir(X86Inst::MovAbs {
                        imm: *v as i64,
                        dst: Reg::R11,
                    });
                    GpOperand::Reg(Reg::R11)
                }
                _ => self.loc_to_gp_operand(&src2_loc),
            };
            // LIR: binary operation (add, sub, and, or, xor)
            match insn.op {
                Opcode::Add => self.push_lir(X86Inst::Add {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::Sub => self.push_lir(X86Inst::Sub {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::And => self.push_lir(X86Inst::And {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::Or => self.push_lir(X86Inst::Or {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::Xor => self.push_lir(X86Inst::Xor {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                _ => {}
            }
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size);
        }
    }

    pub(super) fn emit_unary_op(&mut self, insn: &Instruction, op: UnaryOp, types: &TypeTable) {
        let size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));
        if insn.typ.is_some_and(|t| types.kind(t) == TypeKind::Int128) {
            self.emit_int128_unary(insn, op);
            return;
        }
        let op_size = OperandSize::from_bits(size);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };
        self.emit_move(src, work_reg, size);
        self.push_lir(match op {
            UnaryOp::Neg => X86Inst::Neg {
                size: op_size,
                dst: work_reg,
            },
            UnaryOp::Not => X86Inst::Not {
                size: op_size,
                dst: work_reg,
            },
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size);
        }
    }

    pub(super) fn emit_mul(&mut self, insn: &Instruction, types: &TypeTable) {
        let size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));
        if insn.typ.is_some_and(|t| types.kind(t) == TypeKind::Int128) {
            self.emit_int128_mul(insn);
            return;
        }
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };
        self.emit_move(src1, dst_reg, size);
        let src2_loc = self.get_location(src2);

        // Check if src2 is a large immediate that doesn't fit in 32-bit signed
        // x86-64 imul with immediate only accepts 32-bit immediates
        let src2_gp = match &src2_loc {
            Loc::Imm(v) if (*v as i64 > i32::MAX as i64 || (*v as i64) < i32::MIN as i64) => {
                // Large immediate - must load into register first
                self.emit_move(src2, Reg::R11, size);
                GpOperand::Reg(Reg::R11)
            }
            _ => self.loc_to_gp_operand(&src2_loc),
        };

        // LIR: 2-operand imul instruction
        self.push_lir(X86Inst::IMul2 {
            size: op_size,
            src: src2_gp,
            dst: dst_reg,
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size);
        }
    }

    pub(super) fn emit_div(&mut self, insn: &Instruction, types: &TypeTable) {
        let size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));
        if insn.typ.is_some_and(|t| types.kind(t) == TypeKind::Int128) {
            self.emit_int128_div(insn);
            return;
        }
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // IMPORTANT: Check divisor location BEFORE cltd/cqto clobbers RDX!
        // If divisor is in Rax or Rdx, we must save it to R10 first.
        let src2_loc = self.get_location(src2);
        let divisor_reg = match &src2_loc {
            Loc::Reg(r) if *r != Reg::Rax && *r != Reg::Rdx => *r,
            _ => {
                // Divisor is in Rax, Rdx, stack, or immediate - move to R10 first
                // This MUST happen before emit_move(src1, Rax) in case src2 is in Rax
                self.emit_move(src2, Reg::R10, size);
                Reg::R10
            }
        };

        // Now safe to move dividend to Rax
        self.emit_move(src1, Reg::Rax, size);

        // Sign-extend or zero-extend for division (this clobbers RDX)
        match insn.op {
            Opcode::DivS | Opcode::ModS => {
                if size == 64 {
                    // LIR: sign-extend RAX to RDX:RAX
                    self.push_lir(X86Inst::Cqto);
                } else {
                    // LIR: sign-extend EAX to EDX:EAX
                    self.push_lir(X86Inst::Cltd);
                }
            }
            Opcode::DivU | Opcode::ModU => {
                // LIR: zero RDX for unsigned division
                self.push_lir(X86Inst::Xor {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rdx),
                    dst: Reg::Rdx,
                });
            }
            _ => {}
        }
        // LIR: division instruction
        match insn.op {
            Opcode::DivS | Opcode::ModS => {
                self.push_lir(X86Inst::IDiv {
                    size: op_size,
                    divisor: GpOperand::Reg(divisor_reg),
                });
            }
            _ => {
                self.push_lir(X86Inst::Div {
                    size: op_size,
                    divisor: GpOperand::Reg(divisor_reg),
                });
            }
        }
        let dst_loc = self.get_location(target);
        let result_reg = match insn.op {
            Opcode::DivS | Opcode::DivU => Reg::Rax,
            Opcode::ModS | Opcode::ModU => Reg::Rdx,
            _ => Reg::Rax,
        };
        self.emit_move_to_loc(result_reg, &dst_loc, size);
    }

    pub(super) fn emit_compare(&mut self, insn: &Instruction, types: &TypeTable) {
        let size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));
        if insn.typ.is_some_and(|t| types.kind(t) == TypeKind::Int128) {
            self.emit_int128_compare(insn);
            return;
        }
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Get target location to determine work register
        let dst_loc = self.get_location(target);
        // Use target's register if available, otherwise use R11 as scratch
        // This avoids clobbering other live registers (like Rax)
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R11,
        };

        // Get src1 location to see if we need to move it
        let src1_loc = self.get_location(src1);
        let cmp_reg = match &src1_loc {
            Loc::Reg(r) => *r, // Compare directly in src1's register
            _ => {
                // src1 is not in a register, move to work_reg
                self.emit_move(src1, work_reg, size);
                work_reg
            }
        };

        let src2_loc = self.get_location(src2);
        // x86-64 cmp instruction only supports 32-bit signed immediates
        // For larger values, we need to load into a register first
        let src2_gp = match &src2_loc {
            Loc::Imm(v) if (*v as i64 > i32::MAX as i64 || (*v as i64) < i32::MIN as i64) => {
                // Large immediate - load into a different scratch register
                let scratch = if work_reg == Reg::R10 {
                    Reg::R11
                } else {
                    Reg::R10
                };
                self.push_lir(X86Inst::MovAbs {
                    imm: *v as i64,
                    dst: scratch,
                });
                GpOperand::Reg(scratch)
            }
            _ => self.loc_to_gp_operand(&src2_loc),
        };

        // LIR: compare instruction
        self.push_lir(X86Inst::Cmp {
            size: op_size,
            src: src2_gp,
            dst: GpOperand::Reg(cmp_reg),
        });

        // Map opcode to condition code
        let cc = match insn.op {
            Opcode::SetEq => CondCode::Eq,
            Opcode::SetNe => CondCode::Ne,
            Opcode::SetLt => CondCode::Slt,
            Opcode::SetLe => CondCode::Sle,
            Opcode::SetGt => CondCode::Sgt,
            Opcode::SetGe => CondCode::Sge,
            Opcode::SetB => CondCode::Ult,
            Opcode::SetBe => CondCode::Ule,
            Opcode::SetA => CondCode::Ugt,
            Opcode::SetAe => CondCode::Uge,
            _ => return,
        };

        // LIR: setCC instruction - use work_reg for the result
        self.push_lir(X86Inst::SetCC { cc, dst: work_reg });
        // LIR: zero extend low byte to 32-bit
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(work_reg),
            dst: work_reg,
        });

        // Move to final destination if not already there
        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, u32::BITS);
        }
    }

    pub(super) fn emit_extend(&mut self, insn: &Instruction) {
        // Handle conversions to/from 128-bit integers
        // Check if target or source is an int128 pseudo
        let target_is_int128 = insn
            .target
            .is_some_and(|t| self.int128_pseudos.contains(&t));
        let src_is_int128 = insn
            .src
            .first()
            .is_some_and(|s| self.int128_pseudos.contains(s));
        if target_is_int128 || src_is_int128 {
            self.emit_int128_extend(insn);
            return;
        }
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };
        match insn.op {
            Opcode::Zext => {
                // Load the source value, then ensure zero extension.
                // The source may have been sign-extended by a prior load
                // (e.g., load.8 of a 'char' produces movsbl), so we must
                // mask to the actual source width to get the unsigned value.
                self.emit_move(src, dst_reg, insn.src_size.max(32));
                if insn.src_size < 32 {
                    // Mask to source width: AND with (1 << src_size) - 1
                    let mask = (1i64 << insn.src_size) - 1;
                    self.push_lir(X86Inst::And {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(mask),
                        dst: dst_reg,
                    });
                }
            }
            Opcode::Sext => {
                // Move source at its original size, then sign-extend
                self.emit_move(src, dst_reg, insn.src_size.max(32));
                match insn.src_size {
                    8 => {
                        // LIR: sign-extend byte to qword
                        self.push_lir(X86Inst::Movsx {
                            src_size: OperandSize::B8,
                            dst_size: OperandSize::B64,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        // LIR: sign-extend word to qword
                        self.push_lir(X86Inst::Movsx {
                            src_size: OperandSize::B16,
                            dst_size: OperandSize::B64,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // LIR: sign-extend dword to qword
                        self.push_lir(X86Inst::Movsx {
                            src_size: OperandSize::B32,
                            dst_size: OperandSize::B64,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    _ => {}
                }
            }
            Opcode::Trunc => {
                // Move source value to register, then mask to target size
                self.emit_move(src, dst_reg, 64);
                // Truncate by masking to the target size
                match insn.size {
                    8 => {
                        // LIR: zero-extend byte to dword (masks to 8 bits)
                        self.push_lir(X86Inst::Movzx {
                            src_size: OperandSize::B8,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        // LIR: zero-extend word to dword (masks to 16 bits)
                        self.push_lir(X86Inst::Movzx {
                            src_size: OperandSize::B16,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // LIR: mov dword to self (32-bit ops zero upper 32 bits)
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, insn.size);
        }
    }

    // ========================================================================
    // 128-bit Integer (__int128) Operations
    // ========================================================================
    //
    // All __int128 values live on the stack (16 bytes, never in GP registers).
    // Layout: lo 64 bits at offset, hi 64 bits at offset+8 (little-endian).
    // We use R10 and R11 as scratch registers (reserved, never allocated).

    /// Get the stack memory address for the lo half of a 128-bit operand.
    fn int128_lo_mem(&self, loc: &Loc) -> MemAddr {
        match loc {
            Loc::Stack(offset) => self.stack_mem(*offset),
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: *offset,
            },
            _ => panic!("int128 operand must be on stack, got {:?}", loc),
        }
    }

    /// Get the stack memory address for the hi half of a 128-bit operand.
    fn int128_hi_mem(&self, loc: &Loc) -> MemAddr {
        match loc {
            Loc::Stack(offset) => self.stack_mem(*offset - 8),
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: *offset + 8,
            },
            _ => panic!("int128 operand must be on stack, got {:?}", loc),
        }
    }

    /// Load the lo half of a 128-bit operand into a register.
    fn int128_load_lo(&mut self, pseudo: PseudoId, dst: Reg) {
        let loc = self.get_location(pseudo);
        match &loc {
            Loc::Imm(v) => {
                let lo = *v as i64;
                if lo > i32::MAX as i64 || lo < i32::MIN as i64 {
                    self.push_lir(X86Inst::MovAbs { imm: lo, dst });
                } else {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(lo),
                        dst: GpOperand::Reg(dst),
                    });
                }
            }
            Loc::Stack(_) | Loc::IncomingArg(_) => {
                let mem = self.int128_lo_mem(&loc);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(mem),
                    dst: GpOperand::Reg(dst),
                });
            }
            _ => panic!("int128_load_lo: unexpected loc {:?}", loc),
        }
    }

    /// Load the hi half of a 128-bit operand into a register.
    fn int128_load_hi(&mut self, pseudo: PseudoId, dst: Reg) {
        let loc = self.get_location(pseudo);
        match &loc {
            Loc::Imm(v) => {
                let hi = (*v >> 64) as i64;
                if hi > i32::MAX as i64 || hi < i32::MIN as i64 {
                    self.push_lir(X86Inst::MovAbs { imm: hi, dst });
                } else {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(hi),
                        dst: GpOperand::Reg(dst),
                    });
                }
            }
            Loc::Stack(_) | Loc::IncomingArg(_) => {
                let mem = self.int128_hi_mem(&loc);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(mem),
                    dst: GpOperand::Reg(dst),
                });
            }
            _ => panic!("int128_load_hi: unexpected loc {:?}", loc),
        }
    }

    /// Store a register to the lo half of a 128-bit destination.
    fn int128_store_lo(&mut self, src: Reg, dst_loc: &Loc) {
        let mem = self.int128_lo_mem(dst_loc);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(src),
            dst: GpOperand::Mem(mem),
        });
    }

    /// Store a register to the hi half of a 128-bit destination.
    fn int128_store_hi(&mut self, src: Reg, dst_loc: &Loc) {
        let mem = self.int128_hi_mem(dst_loc);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(src),
            dst: GpOperand::Mem(mem),
        });
    }

    /// Store an immediate to the lo half of a 128-bit destination.
    fn int128_store_lo_imm(&mut self, imm: i64, dst_loc: &Loc) {
        let mem = self.int128_lo_mem(dst_loc);
        if imm > i32::MAX as i64 || imm < i32::MIN as i64 {
            self.push_lir(X86Inst::MovAbs { imm, dst: Reg::R10 });
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R10),
                dst: GpOperand::Mem(mem),
            });
        } else {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Imm(imm),
                dst: GpOperand::Mem(mem),
            });
        }
    }

    /// Store an immediate to the hi half of a 128-bit destination.
    fn int128_store_hi_imm(&mut self, imm: i64, dst_loc: &Loc) {
        let mem = self.int128_hi_mem(dst_loc);
        if imm > i32::MAX as i64 || imm < i32::MIN as i64 {
            self.push_lir(X86Inst::MovAbs { imm, dst: Reg::R10 });
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R10),
                dst: GpOperand::Mem(mem),
            });
        } else {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Imm(imm),
                dst: GpOperand::Mem(mem),
            });
        }
    }

    /// Generate a unique label name for int128 internal branch targets.
    fn int128_label(&mut self, prefix: &str) -> Label {
        let suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        Label::new(prefix, suffix)
    }

    /// Get the GpOperand for the lo half of src2 (for use in add/sub/etc).
    /// If the operand is an immediate, returns GpOperand::Imm or loads into R11.
    fn int128_src2_lo_operand(&mut self, src2: PseudoId) -> GpOperand {
        let loc = self.get_location(src2);
        match &loc {
            Loc::Imm(v) => {
                let lo = *v as i64;
                if lo > i32::MAX as i64 || lo < i32::MIN as i64 {
                    self.push_lir(X86Inst::MovAbs {
                        imm: lo,
                        dst: Reg::R11,
                    });
                    GpOperand::Reg(Reg::R11)
                } else {
                    GpOperand::Imm(lo)
                }
            }
            Loc::Stack(_) | Loc::IncomingArg(_) => GpOperand::Mem(self.int128_lo_mem(&loc)),
            _ => panic!("int128_src2_lo_operand: unexpected loc {:?}", loc),
        }
    }

    /// Get the GpOperand for the hi half of src2.
    fn int128_src2_hi_operand(&mut self, src2: PseudoId) -> GpOperand {
        let loc = self.get_location(src2);
        match &loc {
            Loc::Imm(v) => {
                let hi = (*v >> 64) as i64;
                if hi > i32::MAX as i64 || hi < i32::MIN as i64 {
                    self.push_lir(X86Inst::MovAbs {
                        imm: hi,
                        dst: Reg::R11,
                    });
                    GpOperand::Reg(Reg::R11)
                } else {
                    GpOperand::Imm(hi)
                }
            }
            Loc::Stack(_) | Loc::IncomingArg(_) => GpOperand::Mem(self.int128_hi_mem(&loc)),
            _ => panic!("int128_src2_hi_operand: unexpected loc {:?}", loc),
        }
    }

    /// Emit 128-bit binary operation (Add, Sub, And, Or, Xor, Shl, Lsr, Asr).
    fn emit_int128_binop(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);

        match insn.op {
            Opcode::Add => {
                // lo: addq src2_lo, src1_lo → dst_lo (sets CF)
                // hi: adcq src2_hi, src1_hi → dst_hi (uses CF)
                self.int128_load_lo(src1, Reg::R10);
                let src2_lo = self.int128_src2_lo_operand(src2);
                self.push_lir(X86Inst::Add {
                    size: OperandSize::B64,
                    src: src2_lo,
                    dst: Reg::R10,
                });
                self.int128_store_lo(Reg::R10, &dst_loc);
                self.int128_load_hi(src1, Reg::R10);
                let src2_hi = self.int128_src2_hi_operand(src2);
                self.push_lir(X86Inst::Adc {
                    size: OperandSize::B64,
                    src: src2_hi,
                    dst: Reg::R10,
                });
                self.int128_store_hi(Reg::R10, &dst_loc);
            }
            Opcode::Sub => {
                // lo: subq src2_lo, src1_lo → dst_lo (sets CF)
                // hi: sbbq src2_hi, src1_hi → dst_hi (uses CF)
                self.int128_load_lo(src1, Reg::R10);
                let src2_lo = self.int128_src2_lo_operand(src2);
                self.push_lir(X86Inst::Sub {
                    size: OperandSize::B64,
                    src: src2_lo,
                    dst: Reg::R10,
                });
                self.int128_store_lo(Reg::R10, &dst_loc);
                self.int128_load_hi(src1, Reg::R10);
                let src2_hi = self.int128_src2_hi_operand(src2);
                self.push_lir(X86Inst::Sbb {
                    size: OperandSize::B64,
                    src: src2_hi,
                    dst: Reg::R10,
                });
                self.int128_store_hi(Reg::R10, &dst_loc);
            }
            Opcode::And | Opcode::Or | Opcode::Xor => {
                // Independent 64-bit ops on lo and hi halves (no carry)
                self.int128_load_lo(src1, Reg::R10);
                let src2_lo = self.int128_src2_lo_operand(src2);
                match insn.op {
                    Opcode::And => self.push_lir(X86Inst::And {
                        size: OperandSize::B64,
                        src: src2_lo,
                        dst: Reg::R10,
                    }),
                    Opcode::Or => self.push_lir(X86Inst::Or {
                        size: OperandSize::B64,
                        src: src2_lo,
                        dst: Reg::R10,
                    }),
                    Opcode::Xor => self.push_lir(X86Inst::Xor {
                        size: OperandSize::B64,
                        src: src2_lo,
                        dst: Reg::R10,
                    }),
                    _ => unreachable!(),
                }
                self.int128_store_lo(Reg::R10, &dst_loc);
                self.int128_load_hi(src1, Reg::R10);
                let src2_hi = self.int128_src2_hi_operand(src2);
                match insn.op {
                    Opcode::And => self.push_lir(X86Inst::And {
                        size: OperandSize::B64,
                        src: src2_hi,
                        dst: Reg::R10,
                    }),
                    Opcode::Or => self.push_lir(X86Inst::Or {
                        size: OperandSize::B64,
                        src: src2_hi,
                        dst: Reg::R10,
                    }),
                    Opcode::Xor => self.push_lir(X86Inst::Xor {
                        size: OperandSize::B64,
                        src: src2_hi,
                        dst: Reg::R10,
                    }),
                    _ => unreachable!(),
                }
                self.int128_store_hi(Reg::R10, &dst_loc);
            }
            Opcode::Shl => {
                self.emit_int128_shl(src1, src2, &dst_loc);
            }
            Opcode::Lsr => {
                self.emit_int128_lsr(src1, src2, &dst_loc);
            }
            Opcode::Asr => {
                self.emit_int128_asr(src1, src2, &dst_loc);
            }
            _ => {}
        }
    }

    /// Emit 128-bit shift left.
    /// If shift < 64: shldq %cl, lo, hi; shlq %cl, lo
    /// If shift >= 64: shlq %cl-64, lo → hi; lo = 0
    fn emit_int128_shl(&mut self, src1: PseudoId, src2: PseudoId, dst_loc: &Loc) {
        // Load shift amount into Rcx
        let src2_loc = self.get_location(src2);
        if let Loc::Imm(v) = &src2_loc {
            let n = *v as u32;
            // Constant shift amount - generate specialized code
            if n == 0 {
                // No-op: just copy
                self.int128_load_lo(src1, Reg::R10);
                self.int128_store_lo(Reg::R10, dst_loc);
                self.int128_load_hi(src1, Reg::R10);
                self.int128_store_hi(Reg::R10, dst_loc);
            } else if n < 64 {
                // Load both halves
                self.int128_load_lo(src1, Reg::R10); // lo
                self.int128_load_hi(src1, Reg::R11); // hi
                                                     // shldq shifts hi left, filling from lo
                self.push_lir(X86Inst::Shld {
                    size: OperandSize::B64,
                    count: ShiftCount::Imm(n as u8),
                    src: Reg::R10,
                    dst: Reg::R11,
                });
                self.push_lir(X86Inst::Shl {
                    size: OperandSize::B64,
                    count: ShiftCount::Imm(n as u8),
                    dst: Reg::R10,
                });
                self.int128_store_lo(Reg::R10, dst_loc);
                self.int128_store_hi(Reg::R11, dst_loc);
            } else if n < 128 {
                // hi = lo << (n-64), lo = 0
                self.int128_load_lo(src1, Reg::R10);
                if n > 64 {
                    self.push_lir(X86Inst::Shl {
                        size: OperandSize::B64,
                        count: ShiftCount::Imm((n - 64) as u8),
                        dst: Reg::R10,
                    });
                }
                self.int128_store_hi(Reg::R10, dst_loc);
                self.int128_store_lo_imm(0, dst_loc);
            } else {
                // Shift >= 128: result is 0
                self.int128_store_lo_imm(0, dst_loc);
                self.int128_store_hi_imm(0, dst_loc);
            }
            return;
        }

        // Variable shift amount
        self.emit_move(src2, Reg::Rcx, 64);
        self.int128_load_lo(src1, Reg::R10);
        self.int128_load_hi(src1, Reg::R11);

        let ge64_label = self.int128_label("i128shl_ge64");
        let done_label = self.int128_label("i128shl_done");

        // cmpq $64, %rcx
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B64,
            src: GpOperand::Imm(64),
            dst: GpOperand::Reg(Reg::Rcx),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: ge64_label.clone(),
        });

        // shift < 64
        self.push_lir(X86Inst::Shld {
            size: OperandSize::B64,
            count: ShiftCount::Cl,
            src: Reg::R10,
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Shl {
            size: OperandSize::B64,
            count: ShiftCount::Cl,
            dst: Reg::R10,
        });
        self.int128_store_lo(Reg::R10, dst_loc);
        self.int128_store_hi(Reg::R11, dst_loc);
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        // shift >= 64
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(ge64_label)));
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Imm(64),
            dst: Reg::Rcx,
        });
        self.push_lir(X86Inst::Shl {
            size: OperandSize::B64,
            count: ShiftCount::Cl,
            dst: Reg::R10,
        });
        self.int128_store_hi(Reg::R10, dst_loc);
        self.int128_store_lo_imm(0, dst_loc);

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }

    /// Emit 128-bit logical shift right.
    fn emit_int128_lsr(&mut self, src1: PseudoId, src2: PseudoId, dst_loc: &Loc) {
        let src2_loc = self.get_location(src2);
        if let Loc::Imm(v) = &src2_loc {
            let n = *v as u32;
            if n == 0 {
                self.int128_load_lo(src1, Reg::R10);
                self.int128_store_lo(Reg::R10, dst_loc);
                self.int128_load_hi(src1, Reg::R10);
                self.int128_store_hi(Reg::R10, dst_loc);
            } else if n < 64 {
                self.int128_load_lo(src1, Reg::R10);
                self.int128_load_hi(src1, Reg::R11);
                self.push_lir(X86Inst::Shrd {
                    size: OperandSize::B64,
                    count: ShiftCount::Imm(n as u8),
                    src: Reg::R11,
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Shr {
                    size: OperandSize::B64,
                    count: ShiftCount::Imm(n as u8),
                    dst: Reg::R11,
                });
                self.int128_store_lo(Reg::R10, dst_loc);
                self.int128_store_hi(Reg::R11, dst_loc);
            } else if n < 128 {
                // lo = hi >> (n-64), hi = 0
                self.int128_load_hi(src1, Reg::R10);
                if n > 64 {
                    self.push_lir(X86Inst::Shr {
                        size: OperandSize::B64,
                        count: ShiftCount::Imm((n - 64) as u8),
                        dst: Reg::R10,
                    });
                }
                self.int128_store_lo(Reg::R10, dst_loc);
                self.int128_store_hi_imm(0, dst_loc);
            } else {
                self.int128_store_lo_imm(0, dst_loc);
                self.int128_store_hi_imm(0, dst_loc);
            }
            return;
        }

        // Variable shift
        self.emit_move(src2, Reg::Rcx, 64);
        self.int128_load_lo(src1, Reg::R10);
        self.int128_load_hi(src1, Reg::R11);

        let ge64_label = self.int128_label("i128lsr_ge64");
        let done_label = self.int128_label("i128lsr_done");

        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B64,
            src: GpOperand::Imm(64),
            dst: GpOperand::Reg(Reg::Rcx),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: ge64_label.clone(),
        });

        // shift < 64
        self.push_lir(X86Inst::Shrd {
            size: OperandSize::B64,
            count: ShiftCount::Cl,
            src: Reg::R11,
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Shr {
            size: OperandSize::B64,
            count: ShiftCount::Cl,
            dst: Reg::R11,
        });
        self.int128_store_lo(Reg::R10, dst_loc);
        self.int128_store_hi(Reg::R11, dst_loc);
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        // shift >= 64
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(ge64_label)));
        // lo = hi >> (shift - 64)
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Imm(64),
            dst: Reg::Rcx,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Shr {
            size: OperandSize::B64,
            count: ShiftCount::Cl,
            dst: Reg::R10,
        });
        self.int128_store_lo(Reg::R10, dst_loc);
        self.int128_store_hi_imm(0, dst_loc);

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }

    /// Emit 128-bit arithmetic shift right.
    fn emit_int128_asr(&mut self, src1: PseudoId, src2: PseudoId, dst_loc: &Loc) {
        let src2_loc = self.get_location(src2);
        if let Loc::Imm(v) = &src2_loc {
            let n = *v as u32;
            if n == 0 {
                self.int128_load_lo(src1, Reg::R10);
                self.int128_store_lo(Reg::R10, dst_loc);
                self.int128_load_hi(src1, Reg::R10);
                self.int128_store_hi(Reg::R10, dst_loc);
            } else if n < 64 {
                self.int128_load_lo(src1, Reg::R10);
                self.int128_load_hi(src1, Reg::R11);
                self.push_lir(X86Inst::Shrd {
                    size: OperandSize::B64,
                    count: ShiftCount::Imm(n as u8),
                    src: Reg::R11,
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Sar {
                    size: OperandSize::B64,
                    count: ShiftCount::Imm(n as u8),
                    dst: Reg::R11,
                });
                self.int128_store_lo(Reg::R10, dst_loc);
                self.int128_store_hi(Reg::R11, dst_loc);
            } else if n < 128 {
                // lo = hi >> (n-64) arithmetic, hi = hi >> 63 (all sign bits)
                self.int128_load_hi(src1, Reg::R10);
                self.int128_load_hi(src1, Reg::R11);
                if n > 64 {
                    self.push_lir(X86Inst::Sar {
                        size: OperandSize::B64,
                        count: ShiftCount::Imm((n - 64) as u8),
                        dst: Reg::R10,
                    });
                }
                self.push_lir(X86Inst::Sar {
                    size: OperandSize::B64,
                    count: ShiftCount::Imm(63),
                    dst: Reg::R11,
                });
                self.int128_store_lo(Reg::R10, dst_loc);
                self.int128_store_hi(Reg::R11, dst_loc);
            } else {
                // All sign bits
                self.int128_load_hi(src1, Reg::R10);
                self.push_lir(X86Inst::Sar {
                    size: OperandSize::B64,
                    count: ShiftCount::Imm(63),
                    dst: Reg::R10,
                });
                self.int128_store_lo(Reg::R10, dst_loc);
                self.int128_store_hi(Reg::R10, dst_loc);
            }
            return;
        }

        // Variable shift
        self.emit_move(src2, Reg::Rcx, 64);
        self.int128_load_lo(src1, Reg::R10);
        self.int128_load_hi(src1, Reg::R11);

        let ge64_label = self.int128_label("i128asr_ge64");
        let done_label = self.int128_label("i128asr_done");

        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B64,
            src: GpOperand::Imm(64),
            dst: GpOperand::Reg(Reg::Rcx),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: ge64_label.clone(),
        });

        // shift < 64
        self.push_lir(X86Inst::Shrd {
            size: OperandSize::B64,
            count: ShiftCount::Cl,
            src: Reg::R11,
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Sar {
            size: OperandSize::B64,
            count: ShiftCount::Cl,
            dst: Reg::R11,
        });
        self.int128_store_lo(Reg::R10, dst_loc);
        self.int128_store_hi(Reg::R11, dst_loc);
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        // shift >= 64: lo = hi >> (shift-64), hi = hi >> 63
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(ge64_label)));
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Imm(64),
            dst: Reg::Rcx,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Sar {
            size: OperandSize::B64,
            count: ShiftCount::Cl,
            dst: Reg::R10,
        });
        self.int128_store_lo(Reg::R10, dst_loc);
        // hi = sign extend of original hi
        self.push_lir(X86Inst::Sar {
            size: OperandSize::B64,
            count: ShiftCount::Imm(63),
            dst: Reg::R11,
        });
        self.int128_store_hi(Reg::R11, dst_loc);

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }

    /// Emit 128-bit multiply.
    /// result_lo = lo(src1_lo * src2_lo)
    /// result_hi = hi(src1_lo * src2_lo) + src1_lo * src2_hi + src1_hi * src2_lo
    ///
    /// Uses RAX, RDX (for mul), R10, R11 as scratch. RAX/RDX are allocatable
    /// but the regalloc ensures they are not live across this instruction.
    fn emit_int128_mul(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);

        // Step 1: RAX = src1_lo, mulq src2_lo → RDX:RAX = src1_lo * src2_lo
        self.int128_load_lo(src1, Reg::Rax);
        let src2_lo_loc = self.get_location(src2);
        let src2_lo_op = match &src2_lo_loc {
            Loc::Stack(_) | Loc::IncomingArg(_) => GpOperand::Mem(self.int128_lo_mem(&src2_lo_loc)),
            Loc::Imm(v) => {
                let lo = *v as i64;
                if lo > i32::MAX as i64 || lo < i32::MIN as i64 {
                    self.push_lir(X86Inst::MovAbs {
                        imm: lo,
                        dst: Reg::R10,
                    });
                } else {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(lo),
                        dst: GpOperand::Reg(Reg::R10),
                    });
                }
                GpOperand::Reg(Reg::R10)
            }
            _ => panic!("int128_mul: unexpected src2 loc {:?}", src2_lo_loc),
        };
        self.push_lir(X86Inst::Mul1 {
            size: OperandSize::B64,
            src: src2_lo_op,
        });
        // RAX = result_lo, RDX = partial_hi
        self.int128_store_lo(Reg::Rax, &dst_loc);
        // Save partial_hi in R11
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rdx),
            dst: GpOperand::Reg(Reg::R11),
        });

        // Step 2: R10 = src1_hi * src2_lo (only lo 64 bits matter)
        self.int128_load_hi(src1, Reg::R10);
        // We need src2_lo in a register for imulq
        let src2_lo_loc2 = self.get_location(src2);
        let src2_lo_gp = match &src2_lo_loc2 {
            Loc::Stack(_) | Loc::IncomingArg(_) => {
                GpOperand::Mem(self.int128_lo_mem(&src2_lo_loc2))
            }
            Loc::Imm(v) => {
                let lo = *v as i64;
                if lo > i32::MAX as i64 || lo < i32::MIN as i64 {
                    self.push_lir(X86Inst::MovAbs {
                        imm: lo,
                        dst: Reg::Rax,
                    });
                    GpOperand::Reg(Reg::Rax)
                } else {
                    GpOperand::Imm(lo)
                }
            }
            _ => panic!("int128_mul: unexpected src2 loc {:?}", src2_lo_loc2),
        };
        self.push_lir(X86Inst::IMul2 {
            size: OperandSize::B64,
            src: src2_lo_gp,
            dst: Reg::R10,
        });
        // R11 += R10
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: Reg::R11,
        });

        // Step 3: R10 = src1_lo * src2_hi (only lo 64 bits matter)
        self.int128_load_lo(src1, Reg::R10);
        let src2_hi_loc = self.get_location(src2);
        let src2_hi_gp = match &src2_hi_loc {
            Loc::Stack(_) | Loc::IncomingArg(_) => GpOperand::Mem(self.int128_hi_mem(&src2_hi_loc)),
            Loc::Imm(v) => {
                let hi = (*v >> 64) as i64;
                if hi > i32::MAX as i64 || hi < i32::MIN as i64 {
                    self.push_lir(X86Inst::MovAbs {
                        imm: hi,
                        dst: Reg::Rax,
                    });
                    GpOperand::Reg(Reg::Rax)
                } else {
                    GpOperand::Imm(hi)
                }
            }
            _ => panic!("int128_mul: unexpected src2 loc {:?}", src2_hi_loc),
        };
        self.push_lir(X86Inst::IMul2 {
            size: OperandSize::B64,
            src: src2_hi_gp,
            dst: Reg::R10,
        });
        // R11 += R10
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: Reg::R11,
        });

        // Store result_hi
        self.int128_store_hi(Reg::R11, &dst_loc);
    }

    /// Emit 128-bit division.
    /// For __int128 division, we call the compiler runtime functions
    /// __divti3 (signed) or __udivti3 (unsigned).
    /// Args: (lo1, hi1, lo2, hi2) in RDI, RSI, RDX, RCX
    /// Returns: (lo, hi) in RAX, RDX
    fn emit_int128_div(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);

        let func_name = match insn.op {
            Opcode::DivS => "__divti3",
            Opcode::DivU => "__udivti3",
            Opcode::ModS => "__modti3",
            Opcode::ModU => "__umodti3",
            _ => return,
        };

        // SysV ABI: __int128 args passed as (lo, hi) pairs in GP registers
        // arg1 = (RDI=lo1, RSI=hi1), arg2 = (RDX=lo2, RCX=hi2)
        // BUT: we must be careful about order because loading src2_lo into RDX
        // could clobber a register we need. Load src2 first into RCX/R10,
        // then src1, then move src2_lo to RDX.

        // Load src2_hi into RCX first
        self.int128_load_hi(src2, Reg::Rcx);
        // Load src2_lo into R10 (temporary, will move to RDX later)
        self.int128_load_lo(src2, Reg::R10);
        // Load src1
        self.int128_load_lo(src1, Reg::Rdi);
        self.int128_load_hi(src1, Reg::Rsi);
        // Now move src2_lo to RDX
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Reg(Reg::Rdx),
        });

        // Call the runtime function
        let sym = crate::arch::lir::Symbol::global(func_name.to_string());
        self.push_lir(X86Inst::Call {
            target: crate::arch::lir::CallTarget::Direct(sym),
        });

        // Result in RAX (lo), RDX (hi)
        self.int128_store_lo(Reg::Rax, &dst_loc);
        self.int128_store_hi(Reg::Rdx, &dst_loc);
    }

    /// Emit 128-bit comparison.
    fn emit_int128_compare(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10,
        };

        match insn.op {
            Opcode::SetEq | Opcode::SetNe => {
                // XOR both halves and OR them. Result is zero iff equal.
                self.int128_load_lo(src1, Reg::R10);
                let src2_lo = self.int128_src2_lo_operand(src2);
                self.push_lir(X86Inst::Xor {
                    size: OperandSize::B64,
                    src: src2_lo,
                    dst: Reg::R10,
                });
                self.int128_load_hi(src1, Reg::R11);
                let src2_hi = self.int128_src2_hi_operand(src2);
                self.push_lir(X86Inst::Xor {
                    size: OperandSize::B64,
                    src: src2_hi,
                    dst: Reg::R11,
                });
                self.push_lir(X86Inst::Or {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R11),
                    dst: Reg::R10,
                });
                let cc = if insn.op == Opcode::SetEq {
                    CondCode::Eq
                } else {
                    CondCode::Ne
                };
                self.push_lir(X86Inst::SetCC { cc, dst: work_reg });
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B8,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Reg(work_reg),
                    dst: work_reg,
                });
            }
            _ => {
                // Ordered comparisons: compare hi first, then lo if hi equal.
                // For signed: hi compared signed, lo compared unsigned.
                // For unsigned: both compared unsigned.
                let is_signed = matches!(
                    insn.op,
                    Opcode::SetLt | Opcode::SetLe | Opcode::SetGt | Opcode::SetGe
                );

                let hi_decides_label = self.int128_label("i128cmp_hi");
                let done_label = self.int128_label("i128cmp_done");

                // Compare hi halves
                self.int128_load_hi(src1, Reg::R10);
                let src2_hi = self.int128_src2_hi_operand(src2);
                self.push_lir(X86Inst::Cmp {
                    size: OperandSize::B64,
                    src: src2_hi,
                    dst: GpOperand::Reg(Reg::R10),
                });
                // If hi halves are not equal, the hi comparison decides
                self.push_lir(X86Inst::Jcc {
                    cc: CondCode::Ne,
                    target: hi_decides_label.clone(),
                });

                // Hi halves are equal: compare lo halves (always unsigned)
                self.int128_load_lo(src1, Reg::R10);
                let src2_lo = self.int128_src2_lo_operand(src2);
                self.push_lir(X86Inst::Cmp {
                    size: OperandSize::B64,
                    src: src2_lo,
                    dst: GpOperand::Reg(Reg::R10),
                });
                // Use unsigned comparison for lo half
                let lo_cc = match insn.op {
                    Opcode::SetLt | Opcode::SetB => CondCode::Ult,
                    Opcode::SetLe | Opcode::SetBe => CondCode::Ule,
                    Opcode::SetGt | Opcode::SetA => CondCode::Ugt,
                    Opcode::SetGe | Opcode::SetAe => CondCode::Uge,
                    _ => CondCode::Ult,
                };
                self.push_lir(X86Inst::SetCC {
                    cc: lo_cc,
                    dst: work_reg,
                });
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B8,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Reg(work_reg),
                    dst: work_reg,
                });
                self.push_lir(X86Inst::Jmp {
                    target: done_label.clone(),
                });

                // Hi decides the comparison
                self.push_lir(X86Inst::Directive(Directive::BlockLabel(hi_decides_label)));
                let hi_cc = if is_signed {
                    match insn.op {
                        Opcode::SetLt => CondCode::Slt,
                        Opcode::SetLe => CondCode::Sle,
                        Opcode::SetGt => CondCode::Sgt,
                        Opcode::SetGe => CondCode::Sge,
                        _ => CondCode::Slt,
                    }
                } else {
                    match insn.op {
                        Opcode::SetB => CondCode::Ult,
                        Opcode::SetBe => CondCode::Ule,
                        Opcode::SetA => CondCode::Ugt,
                        Opcode::SetAe => CondCode::Uge,
                        _ => CondCode::Ult,
                    }
                };
                self.push_lir(X86Inst::SetCC {
                    cc: hi_cc,
                    dst: work_reg,
                });
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B8,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Reg(work_reg),
                    dst: work_reg,
                });

                self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
            }
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, u32::BITS);
        }
    }

    /// Emit 128-bit unary operation (Neg, Not).
    fn emit_int128_unary(&mut self, insn: &Instruction, op: UnaryOp) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);

        match op {
            UnaryOp::Not => {
                // Bitwise NOT: not lo; not hi
                self.int128_load_lo(src, Reg::R10);
                self.push_lir(X86Inst::Not {
                    size: OperandSize::B64,
                    dst: Reg::R10,
                });
                self.int128_store_lo(Reg::R10, &dst_loc);
                self.int128_load_hi(src, Reg::R10);
                self.push_lir(X86Inst::Not {
                    size: OperandSize::B64,
                    dst: Reg::R10,
                });
                self.int128_store_hi(Reg::R10, &dst_loc);
            }
            UnaryOp::Neg => {
                // Two's complement negate: not lo; not hi; add $1, lo; adc $0, hi
                self.int128_load_lo(src, Reg::R10);
                self.push_lir(X86Inst::Not {
                    size: OperandSize::B64,
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Add {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(1),
                    dst: Reg::R10,
                });
                self.int128_store_lo(Reg::R10, &dst_loc);
                self.int128_load_hi(src, Reg::R10);
                self.push_lir(X86Inst::Not {
                    size: OperandSize::B64,
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Adc {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(0),
                    dst: Reg::R10,
                });
                self.int128_store_hi(Reg::R10, &dst_loc);
            }
        }
    }

    /// Emit 128-bit extend/truncate operations.
    fn emit_int128_extend(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        if insn.size == 128 {
            // Extending TO 128-bit
            let dst_loc = self.get_location(target);
            match insn.op {
                Opcode::Zext => {
                    // Zero-extend: lo = src, hi = 0
                    self.emit_move(src, Reg::R10, insn.src_size.max(32));
                    // If src_size < 64, ensure upper bits are zeroed
                    if insn.src_size < 32 {
                        let mask = (1i64 << insn.src_size) - 1;
                        self.push_lir(X86Inst::And {
                            size: OperandSize::B32,
                            src: GpOperand::Imm(mask),
                            dst: Reg::R10,
                        });
                    }
                    self.int128_store_lo(Reg::R10, &dst_loc);
                    self.int128_store_hi_imm(0, &dst_loc);
                }
                Opcode::Sext => {
                    // Sign-extend: lo = src, hi = src >> 63 (sign extension)
                    self.emit_move(src, Reg::R10, insn.src_size.max(32));
                    // Sign-extend src to 64 bits first if needed
                    match insn.src_size {
                        8 => {
                            self.push_lir(X86Inst::Movsx {
                                src_size: OperandSize::B8,
                                dst_size: OperandSize::B64,
                                src: GpOperand::Reg(Reg::R10),
                                dst: Reg::R10,
                            });
                        }
                        16 => {
                            self.push_lir(X86Inst::Movsx {
                                src_size: OperandSize::B16,
                                dst_size: OperandSize::B64,
                                src: GpOperand::Reg(Reg::R10),
                                dst: Reg::R10,
                            });
                        }
                        32 => {
                            self.push_lir(X86Inst::Movsx {
                                src_size: OperandSize::B32,
                                dst_size: OperandSize::B64,
                                src: GpOperand::Reg(Reg::R10),
                                dst: Reg::R10,
                            });
                        }
                        _ => {} // 64-bit: already correct
                    }
                    self.int128_store_lo(Reg::R10, &dst_loc);
                    // hi = sign extension of lo
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::R10),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::Sar {
                        size: OperandSize::B64,
                        count: ShiftCount::Imm(63),
                        dst: Reg::R11,
                    });
                    self.int128_store_hi(Reg::R11, &dst_loc);
                }
                _ => {}
            }
        } else {
            // Truncating FROM 128-bit (insn.src_size == 128)
            // Just load the lo half and truncate
            let dst_loc = self.get_location(target);
            let dst_reg = match &dst_loc {
                Loc::Reg(r) => *r,
                _ => Reg::R10,
            };
            self.int128_load_lo(src, dst_reg);
            // Truncate to target size
            match insn.size {
                8 => {
                    self.push_lir(X86Inst::Movzx {
                        src_size: OperandSize::B8,
                        dst_size: OperandSize::B32,
                        src: GpOperand::Reg(dst_reg),
                        dst: dst_reg,
                    });
                }
                16 => {
                    self.push_lir(X86Inst::Movzx {
                        src_size: OperandSize::B16,
                        dst_size: OperandSize::B32,
                        src: GpOperand::Reg(dst_reg),
                        dst: dst_reg,
                    });
                }
                32 => {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(dst_reg),
                        dst: GpOperand::Reg(dst_reg),
                    });
                }
                _ => {} // 64-bit: lo half is the result
            }
            if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                self.emit_move_to_loc(dst_reg, &dst_loc, insn.size);
            }
        }
    }
}
