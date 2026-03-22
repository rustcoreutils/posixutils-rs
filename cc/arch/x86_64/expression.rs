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
            Loc::Reg(r) => {
                // Register: treat as containing the low 64 bits
                if *r != dst {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(*r),
                        dst: GpOperand::Reg(dst),
                    });
                }
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
            Loc::Reg(_) => {
                // Register holds a scalar; hi half is 0
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(0),
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

    /// Emit 128-bit shift operations (Shl, Lsr, Asr).
    /// Other int128 ops (Add, Sub, And, Or, Xor, Mul, Neg, Not, comparisons)
    /// are expanded by the hwmap pass into 64-bit sequences.
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

    // ========================================================================
    // Int128 decomposition ops (Lo64, Hi64, Pair64)
    // ========================================================================

    /// Lo64: extract low 64 bits from 128-bit pseudo.
    /// target(64) = lo64(src(128))
    pub(super) fn emit_lo64(&mut self, insn: &Instruction) {
        let src = insn.src[0];
        let target = insn.target.expect("Lo64 must have target");
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10,
        };
        self.int128_load_lo(src, dst_reg);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 64);
        }
    }

    /// Hi64: extract high 64 bits from 128-bit pseudo.
    /// target(64) = hi64(src(128))
    pub(super) fn emit_hi64(&mut self, insn: &Instruction) {
        let src = insn.src[0];
        let target = insn.target.expect("Hi64 must have target");
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10,
        };
        self.int128_load_hi(src, dst_reg);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 64);
        }
    }

    /// Pair64: combine two 64-bit pseudos into 128-bit.
    /// target(128) = pair64(lo(64), hi(64))
    pub(super) fn emit_pair64(&mut self, insn: &Instruction) {
        let src_lo = insn.src[0];
        let src_hi = insn.src[1];
        let target = insn.target.expect("Pair64 must have target");
        let dst_loc = self.get_location(target);

        // Store lo half
        self.emit_move(src_lo, Reg::R10, 64);
        self.int128_store_lo(Reg::R10, &dst_loc);

        // Store hi half
        self.emit_move(src_hi, Reg::R10, 64);
        self.int128_store_hi(Reg::R10, &dst_loc);
    }

    /// AddC/AdcC: 64-bit add with carry.
    /// AddC (with_carry=false): add, sets CF
    /// AdcC (with_carry=true): adc (add with carry in), sets CF
    pub(super) fn emit_addc(&mut self, insn: &Instruction, with_carry: bool) {
        let target = insn.target.expect("AddC/AdcC must have target");
        let src1 = insn.src[0];
        let src2 = insn.src[1];
        let dst_loc = self.get_location(target);
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10,
        };

        self.emit_move(src1, work_reg, 64);
        let src2_loc = self.get_location(src2);
        let src2_op = match &src2_loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Imm(v) if *v >= i32::MIN as i128 && *v <= i32::MAX as i128 => {
                GpOperand::Imm(*v as i64)
            }
            _ => {
                self.emit_move(src2, Reg::R11, 64);
                GpOperand::Reg(Reg::R11)
            }
        };

        if with_carry {
            self.push_lir(X86Inst::Adc {
                size: OperandSize::B64,
                src: src2_op,
                dst: work_reg,
            });
        } else {
            self.push_lir(X86Inst::Add {
                size: OperandSize::B64,
                src: src2_op,
                dst: work_reg,
            });
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, 64);
        }
    }

    /// SubC/SbcC: 64-bit sub with borrow.
    /// SubC (with_borrow=false): sub, sets CF
    /// SbcC (with_borrow=true): sbb (sub with borrow in), sets CF
    pub(super) fn emit_subc(&mut self, insn: &Instruction, with_borrow: bool) {
        let target = insn.target.expect("SubC/SbcC must have target");
        let src1 = insn.src[0];
        let src2 = insn.src[1];
        let dst_loc = self.get_location(target);
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10,
        };

        self.emit_move(src1, work_reg, 64);
        let src2_loc = self.get_location(src2);
        let src2_op = match &src2_loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Imm(v) if *v >= i32::MIN as i128 && *v <= i32::MAX as i128 => {
                GpOperand::Imm(*v as i64)
            }
            _ => {
                self.emit_move(src2, Reg::R11, 64);
                GpOperand::Reg(Reg::R11)
            }
        };

        if with_borrow {
            self.push_lir(X86Inst::Sbb {
                size: OperandSize::B64,
                src: src2_op,
                dst: work_reg,
            });
        } else {
            self.push_lir(X86Inst::Sub {
                size: OperandSize::B64,
                src: src2_op,
                dst: work_reg,
            });
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, 64);
        }
    }

    /// UMulHi: upper 64 bits of 64×64 unsigned multiply.
    /// Uses mul instruction which puts result in RDX:RAX.
    pub(super) fn emit_umulhi(&mut self, insn: &Instruction) {
        let target = insn.target.expect("UMulHi must have target");
        let src1 = insn.src[0];
        let src2 = insn.src[1];
        let dst_loc = self.get_location(target);

        // mul uses RAX as implicit first operand, result in RDX:RAX
        self.emit_move(src1, Reg::Rax, 64);
        let src2_loc = self.get_location(src2);
        let src2_op = match &src2_loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            _ => {
                self.emit_move(src2, Reg::R11, 64);
                GpOperand::Reg(Reg::R11)
            }
        };

        self.push_lir(X86Inst::Mul1 {
            size: OperandSize::B64,
            src: src2_op,
        });

        // High result is in RDX
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::Rdx) {
            self.emit_move_to_loc(Reg::Rdx, &dst_loc, 64);
        }
    }
}
