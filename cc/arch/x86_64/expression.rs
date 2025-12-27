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
use super::lir::{GpOperand, ShiftCount, X86Inst};
use super::regalloc::{Loc, Reg};
use crate::arch::codegen::UnaryOp;
use crate::arch::lir::{CondCode, OperandSize};
use crate::ir::{Instruction, Opcode};

impl X86_64CodeGen {
    pub(super) fn emit_binop(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
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
                Loc::Imm(v) if size == 64 && (*v > i32::MAX as i64 || *v < i32::MIN as i64) => {
                    // Large 64-bit immediate - load into R11 first
                    self.push_lir(X86Inst::MovAbs {
                        imm: *v,
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

    pub(super) fn emit_unary_op(&mut self, insn: &Instruction, op: UnaryOp) {
        let size = insn.size.max(32);
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

    pub(super) fn emit_mul(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
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
        // LIR: 2-operand imul instruction
        let src2_gp = self.loc_to_gp_operand(&src2_loc);
        self.push_lir(X86Inst::IMul2 {
            size: op_size,
            src: src2_gp,
            dst: dst_reg,
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size);
        }
    }

    pub(super) fn emit_div(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
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

    pub(super) fn emit_compare(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
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
            Loc::Imm(v) if *v > i32::MAX as i64 || *v < i32::MIN as i64 => {
                // Large immediate - load into a different scratch register
                let scratch = if work_reg == Reg::R10 {
                    Reg::R11
                } else {
                    Reg::R10
                };
                self.push_lir(X86Inst::MovAbs {
                    imm: *v,
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
            self.emit_move_to_loc(work_reg, &dst_loc, 32);
        }
    }

    pub(super) fn emit_extend(&mut self, insn: &Instruction) {
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
                // Move source at its original size - this ensures we only load
                // the valid bits from stack/memory. On x86-64, 32-bit register
                // writes automatically zero the upper 32 bits.
                // For 8/16-bit sources, emit_move uses movzbl/movzwl which
                // zero-extends to 32 bits (and thus to 64 bits).
                self.emit_move(src, dst_reg, insn.src_size.max(32));
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
}
