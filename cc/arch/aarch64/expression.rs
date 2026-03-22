//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Expression Code Generation (Binary and Unary Operations)
//

use super::codegen::Aarch64CodeGen;
use super::lir::{Aarch64Inst, GpOperand};
use super::regalloc::{Loc, Reg};
use crate::arch::codegen::UnaryOp;
use crate::arch::lir::{CondCode, Directive, OperandSize};
use crate::ir::{Instruction, Opcode, PseudoId};
use crate::types::TypeTable;

impl Aarch64CodeGen {
    pub(super) fn emit_binop(&mut self, insn: &Instruction, types: &TypeTable) {
        let size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));

        // 128-bit shifts are still handled by the backend (hwmap doesn't expand them)
        if size == 128 {
            if matches!(insn.op, Opcode::Shl | Opcode::Lsr | Opcode::Asr) {
                self.emit_int128_binop(insn);
            }
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
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        // Load first operand
        self.emit_move(src1, work_reg, size);

        // Get second operand as GpOperand
        let src2_loc = self.get_location(src2);
        let src2_operand = match &src2_loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Imm(v) if *v >= 0 && *v <= 4095 => GpOperand::Imm(*v as i64),
            _ => {
                self.emit_move(src2, Reg::X10, size);
                GpOperand::Reg(Reg::X10)
            }
        };

        // Emit the appropriate LIR instruction
        match insn.op {
            Opcode::Add => self.push_lir(Aarch64Inst::Add {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Sub => self.push_lir(Aarch64Inst::Sub {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::And => self.push_lir(Aarch64Inst::And {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Or => self.push_lir(Aarch64Inst::Orr {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Xor => self.push_lir(Aarch64Inst::Eor {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Shl => self.push_lir(Aarch64Inst::Lsl {
                size: op_size,
                src: work_reg,
                amount: src2_operand,
                dst: work_reg,
            }),
            Opcode::Lsr => self.push_lir(Aarch64Inst::Lsr {
                size: op_size,
                src: work_reg,
                amount: src2_operand,
                dst: work_reg,
            }),
            Opcode::Asr => self.push_lir(Aarch64Inst::Asr {
                size: op_size,
                src: work_reg,
                amount: src2_operand,
                dst: work_reg,
            }),
            _ => return,
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
            _ => Reg::X16,
        };

        self.emit_move(src, work_reg, size);
        self.push_lir(match op {
            UnaryOp::Neg => Aarch64Inst::Neg {
                size: op_size,
                src: work_reg,
                dst: work_reg,
            },
            UnaryOp::Not => Aarch64Inst::Mvn {
                size: op_size,
                src: work_reg,
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
            _ => Reg::X16,
        };

        self.emit_move(src1, Reg::X10, size);
        self.emit_move(src2, Reg::X11, size);

        self.push_lir(Aarch64Inst::Mul {
            size: op_size,
            src1: Reg::X10,
            src2: Reg::X11,
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

        self.emit_move(src1, Reg::X10, size);
        self.emit_move(src2, Reg::X11, size);

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        // Division instruction
        match insn.op {
            Opcode::DivS | Opcode::ModS => self.push_lir(Aarch64Inst::Sdiv {
                size: op_size,
                src1: Reg::X10,
                src2: Reg::X11,
                dst: dst_reg,
            }),
            Opcode::DivU | Opcode::ModU => self.push_lir(Aarch64Inst::Udiv {
                size: op_size,
                src1: Reg::X10,
                src2: Reg::X11,
                dst: dst_reg,
            }),
            _ => return,
        }

        // For modulo, compute remainder: r = n - (n / d) * d
        // Using msub: msub Rd, Rm, Rn, Ra -> Rd = Ra - Rm * Rn
        if matches!(insn.op, Opcode::ModS | Opcode::ModU) {
            // dst_reg now has quotient, compute: src1 - quotient * src2
            self.push_lir(Aarch64Inst::Msub {
                size: op_size,
                mul1: dst_reg,
                mul2: Reg::X11,
                sub: Reg::X10,
                dst: dst_reg,
            });
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size);
        }
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

        self.emit_move(src1, Reg::X10, size);

        // Try to use immediate for comparison if possible
        let src2_loc = self.get_location(src2);
        let src2_operand = match &src2_loc {
            Loc::Imm(v) if *v >= 0 && *v <= 4095 => GpOperand::Imm(*v as i64),
            _ => {
                self.emit_move(src2, Reg::X11, size);
                GpOperand::Reg(Reg::X11)
            }
        };

        self.push_lir(Aarch64Inst::Cmp {
            size: op_size,
            src1: Reg::X10,
            src2: src2_operand,
        });

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        // Use cset to set register based on condition
        let cond = match insn.op {
            Opcode::SetEq => CondCode::Eq,
            Opcode::SetNe => CondCode::Ne,
            Opcode::SetLt => CondCode::Slt,
            Opcode::SetLe => CondCode::Sle,
            Opcode::SetGt => CondCode::Sgt,
            Opcode::SetGe => CondCode::Sge,
            Opcode::SetB => CondCode::Ult,  // unsigned less than (lo)
            Opcode::SetBe => CondCode::Ule, // unsigned less than or equal
            Opcode::SetA => CondCode::Ugt,  // unsigned greater than
            Opcode::SetAe => CondCode::Uge, // unsigned greater than or equal (hs)
            _ => return,
        };

        self.push_lir(Aarch64Inst::Cset { cond, dst: dst_reg });

        // Store as 64-bit so that CBR's 64-bit load doesn't read stack garbage
        // in the upper 32 bits. cset produces 0 or 1 in a 64-bit register
        // (upper 32 bits are already zero), so str x__ is correct.
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 64);
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

        // Handle truncation FROM 128-bit (Zext/Sext TO 128 handled by hwmap)
        if insn.src_size == 128 && insn.op == Opcode::Trunc {
            self.emit_int128_trunc(insn);
            return;
        }

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        match insn.op {
            Opcode::Zext => {
                // Zero extend: use uxtb, uxth, or just mov for 32->64
                self.emit_move(src, dst_reg, 64);
                match insn.src_size {
                    8 => {
                        self.push_lir(Aarch64Inst::Uxtb {
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        self.push_lir(Aarch64Inst::Uxth {
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // 32-bit ops automatically zero-extend on AArch64
                    }
                    _ => {}
                }
            }
            Opcode::Sext => {
                // Sign extend: use sxtb, sxth, sxtw based on source size
                self.emit_move(src, dst_reg, 64);
                match insn.src_size {
                    8 => {
                        self.push_lir(Aarch64Inst::Sxtb {
                            dst_size: OperandSize::B64,
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        self.push_lir(Aarch64Inst::Sxth {
                            dst_size: OperandSize::B64,
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        self.push_lir(Aarch64Inst::Sxtw {
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    _ => {}
                }
            }
            Opcode::Trunc => {
                // Truncate: move value then mask to target size
                self.emit_move(src, dst_reg, 64);
                // Mask to target size using AND
                match insn.size {
                    8 => {
                        self.push_lir(Aarch64Inst::And {
                            size: OperandSize::B32,
                            src1: dst_reg,
                            src2: GpOperand::Imm(0xff),
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        self.push_lir(Aarch64Inst::And {
                            size: OperandSize::B32,
                            src1: dst_reg,
                            src2: GpOperand::Imm(0xffff),
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // 32-bit already handled - writing to w register zeros upper 32 bits
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
    // 128-bit Integer (Int128) Operations
    // ========================================================================
    //
    // All __int128 values live on the stack (16 bytes, never in registers).
    // Operations: load lo/hi 64-bit halves into scratch registers, operate, store back.
    //
    // Scratch register allocation for Int128:
    //   X9, X10  = first operand (lo, hi)
    //   X11, X16 = second operand (lo, hi)
    //   X17      = extra scratch for shifts/mul
    //
    // STP/LDP pair format: stp/ldp reg_lo, reg_hi, [base, #offset]
    // stores reg_lo at [base+offset] and reg_hi at [base+offset+8]

    /// Load a 128-bit value from a pseudo into (lo_reg, hi_reg).
    fn load_int128(&mut self, src: PseudoId, lo_reg: Reg, hi_reg: Reg) {
        let loc = self.get_location(src);
        match loc {
            Loc::Stack(offset) => {
                let mem = self.stack_mem(offset);
                self.push_lir(Aarch64Inst::Ldp {
                    size: OperandSize::B64,
                    addr: mem,
                    dst1: lo_reg,
                    dst2: hi_reg,
                });
            }
            Loc::Imm(v) => {
                let lo = v as u64 as i64;
                let hi = (v >> 64) as u64 as i64;
                self.emit_mov_imm(lo_reg, lo, 64);
                self.emit_mov_imm(hi_reg, hi, 64);
            }
            _ => {
                // Fallback: load as 64-bit, zero hi
                self.emit_move(src, lo_reg, 64);
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Xzr),
                    dst: hi_reg,
                });
            }
        }
    }

    /// Store a 128-bit result from (lo_reg, hi_reg) to the target pseudo.
    fn store_int128(&mut self, lo_reg: Reg, hi_reg: Reg, target: PseudoId) {
        let dst_loc = self.get_location(target);
        if let Loc::Stack(offset) = dst_loc {
            let mem = self.stack_mem(offset);
            self.push_lir(Aarch64Inst::Stp {
                size: OperandSize::B64,
                src1: lo_reg,
                src2: hi_reg,
                addr: mem,
            });
        }
    }

    /// Emit 128-bit shift operation (shl, lsr, asr)
    fn emit_int128_binop(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load src1 as 128-bit: X9=lo1, X10=hi1
        self.load_int128(src1, Reg::X9, Reg::X10);

        // Shift amount is a regular small integer, not int128.
        self.emit_move(src2, Reg::X11, 64);

        match insn.op {
            Opcode::Shl => self.emit_int128_shl(),
            Opcode::Lsr => self.emit_int128_lsr(),
            Opcode::Asr => self.emit_int128_asr(),
            _ => return,
        }

        self.store_int128(Reg::X9, Reg::X10, target);
    }

    /// Emit 128-bit left shift. Inputs: X9=lo, X10=hi, X11=shift amount.
    /// Uses X16, X17 as scratch. Results in X9=lo, X10=hi.
    fn emit_int128_shl(&mut self) {
        let label_zero = self.next_unique_label("i128");
        let label_ge64 = self.next_unique_label("i128");
        let label_done = self.next_unique_label("i128");

        // If shift amount is 0, skip entirely (result = input unchanged)
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X11,
            src2: GpOperand::Imm(0),
        });
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Eq,
            target: label_zero.clone(),
        });

        // Compare shift amount with 64
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X11,
            src2: GpOperand::Imm(64),
        });
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Uge,
            target: label_ge64.clone(),
        });

        // shift < 64 path:
        // X17 = hi << shift
        self.push_lir(Aarch64Inst::Lsl {
            size: OperandSize::B64,
            src: Reg::X10,
            amount: GpOperand::Reg(Reg::X11),
            dst: Reg::X17,
        });
        // X16 = 64 - shift
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Imm(64),
            dst: Reg::X16,
        });
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::X16,
            src2: GpOperand::Reg(Reg::X11),
            dst: Reg::X16,
        });
        // X16 = lo >> (64 - shift)
        self.push_lir(Aarch64Inst::Lsr {
            size: OperandSize::B64,
            src: Reg::X9,
            amount: GpOperand::Reg(Reg::X16),
            dst: Reg::X16,
        });
        // X10 = X17 | X16 (hi result)
        self.push_lir(Aarch64Inst::Orr {
            size: OperandSize::B64,
            src1: Reg::X17,
            src2: GpOperand::Reg(Reg::X16),
            dst: Reg::X10,
        });
        // X9 = lo << shift
        self.push_lir(Aarch64Inst::Lsl {
            size: OperandSize::B64,
            src: Reg::X9,
            amount: GpOperand::Reg(Reg::X11),
            dst: Reg::X9,
        });
        self.push_lir(Aarch64Inst::B {
            target: label_done.clone(),
        });

        // shift >= 64 path:
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_ge64)));
        // X16 = shift - 64
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::X11,
            src2: GpOperand::Imm(64),
            dst: Reg::X16,
        });
        // X10 = lo << (shift - 64)
        self.push_lir(Aarch64Inst::Lsl {
            size: OperandSize::B64,
            src: Reg::X9,
            amount: GpOperand::Reg(Reg::X16),
            dst: Reg::X10,
        });
        // X9 = 0
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Xzr),
            dst: Reg::X9,
        });
        self.push_lir(Aarch64Inst::B {
            target: label_done.clone(),
        });

        // shift == 0: result is input unchanged (already in X9:X10)
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_zero)));

        // done:
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_done)));
    }

    /// Emit 128-bit logical right shift. Inputs: X9=lo, X10=hi, X11=shift amount.
    fn emit_int128_lsr(&mut self) {
        let label_zero = self.next_unique_label("i128");
        let label_ge64 = self.next_unique_label("i128");
        let label_done = self.next_unique_label("i128");

        // If shift amount is 0, skip entirely (result = input unchanged)
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X11,
            src2: GpOperand::Imm(0),
        });
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Eq,
            target: label_zero.clone(),
        });

        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X11,
            src2: GpOperand::Imm(64),
        });
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Uge,
            target: label_ge64.clone(),
        });

        // shift < 64:
        // X17 = lo >> shift
        self.push_lir(Aarch64Inst::Lsr {
            size: OperandSize::B64,
            src: Reg::X9,
            amount: GpOperand::Reg(Reg::X11),
            dst: Reg::X17,
        });
        // X16 = 64 - shift
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Imm(64),
            dst: Reg::X16,
        });
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::X16,
            src2: GpOperand::Reg(Reg::X11),
            dst: Reg::X16,
        });
        // X16 = hi << (64 - shift)
        self.push_lir(Aarch64Inst::Lsl {
            size: OperandSize::B64,
            src: Reg::X10,
            amount: GpOperand::Reg(Reg::X16),
            dst: Reg::X16,
        });
        // X9 = X17 | X16 (lo result)
        self.push_lir(Aarch64Inst::Orr {
            size: OperandSize::B64,
            src1: Reg::X17,
            src2: GpOperand::Reg(Reg::X16),
            dst: Reg::X9,
        });
        // X10 = hi >> shift
        self.push_lir(Aarch64Inst::Lsr {
            size: OperandSize::B64,
            src: Reg::X10,
            amount: GpOperand::Reg(Reg::X11),
            dst: Reg::X10,
        });
        self.push_lir(Aarch64Inst::B {
            target: label_done.clone(),
        });

        // shift >= 64:
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_ge64)));
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::X11,
            src2: GpOperand::Imm(64),
            dst: Reg::X16,
        });
        // X9 = hi >> (shift - 64)
        self.push_lir(Aarch64Inst::Lsr {
            size: OperandSize::B64,
            src: Reg::X10,
            amount: GpOperand::Reg(Reg::X16),
            dst: Reg::X9,
        });
        // X10 = 0
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Xzr),
            dst: Reg::X10,
        });
        self.push_lir(Aarch64Inst::B {
            target: label_done.clone(),
        });

        // shift == 0: result is input unchanged (already in X9:X10)
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_zero)));

        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_done)));
    }

    /// Emit 128-bit arithmetic right shift. Inputs: X9=lo, X10=hi, X11=shift amount.
    fn emit_int128_asr(&mut self) {
        let label_zero = self.next_unique_label("i128");
        let label_ge64 = self.next_unique_label("i128");
        let label_done = self.next_unique_label("i128");

        // If shift amount is 0, skip entirely (result = input unchanged)
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X11,
            src2: GpOperand::Imm(0),
        });
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Eq,
            target: label_zero.clone(),
        });

        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X11,
            src2: GpOperand::Imm(64),
        });
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Uge,
            target: label_ge64.clone(),
        });

        // shift < 64:
        // X17 = lo >> shift (logical)
        self.push_lir(Aarch64Inst::Lsr {
            size: OperandSize::B64,
            src: Reg::X9,
            amount: GpOperand::Reg(Reg::X11),
            dst: Reg::X17,
        });
        // X16 = 64 - shift
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Imm(64),
            dst: Reg::X16,
        });
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::X16,
            src2: GpOperand::Reg(Reg::X11),
            dst: Reg::X16,
        });
        // X16 = hi << (64 - shift)
        self.push_lir(Aarch64Inst::Lsl {
            size: OperandSize::B64,
            src: Reg::X10,
            amount: GpOperand::Reg(Reg::X16),
            dst: Reg::X16,
        });
        // X9 = X17 | X16 (lo result)
        self.push_lir(Aarch64Inst::Orr {
            size: OperandSize::B64,
            src1: Reg::X17,
            src2: GpOperand::Reg(Reg::X16),
            dst: Reg::X9,
        });
        // X10 = hi >> shift (arithmetic)
        self.push_lir(Aarch64Inst::Asr {
            size: OperandSize::B64,
            src: Reg::X10,
            amount: GpOperand::Reg(Reg::X11),
            dst: Reg::X10,
        });
        self.push_lir(Aarch64Inst::B {
            target: label_done.clone(),
        });

        // shift >= 64:
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_ge64)));
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::X11,
            src2: GpOperand::Imm(64),
            dst: Reg::X16,
        });
        // X9 = hi >> (shift - 64) (arithmetic)
        self.push_lir(Aarch64Inst::Asr {
            size: OperandSize::B64,
            src: Reg::X10,
            amount: GpOperand::Reg(Reg::X16),
            dst: Reg::X9,
        });
        // X10 = hi >> 63 (sign extension)
        self.push_lir(Aarch64Inst::Asr {
            size: OperandSize::B64,
            src: Reg::X10,
            amount: GpOperand::Imm(63),
            dst: Reg::X10,
        });
        self.push_lir(Aarch64Inst::B {
            target: label_done.clone(),
        });

        // shift == 0: result is input unchanged (already in X9:X10)
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_zero)));

        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_done)));
    }

    /// Emit truncation from 128-bit to a smaller type
    fn emit_int128_trunc(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Truncate from 128: just take lo half (or part of it)
        self.load_int128(src, Reg::X9, Reg::X10);
        // X9 has lo half, which is what we want
        let target_size = insn.size;
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9, // already in X9
        };
        if dst_reg != Reg::X9 {
            self.push_lir(Aarch64Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::X9),
                dst: dst_reg,
            });
        }
        // Mask to target size if needed
        match target_size {
            8 => {
                self.push_lir(Aarch64Inst::And {
                    size: OperandSize::B32,
                    src1: dst_reg,
                    src2: GpOperand::Imm(0xff),
                    dst: dst_reg,
                });
            }
            16 => {
                self.push_lir(Aarch64Inst::And {
                    size: OperandSize::B32,
                    src1: dst_reg,
                    src2: GpOperand::Imm(0xffff),
                    dst: dst_reg,
                });
            }
            32 | 64 => {
                // Already correct width
            }
            _ => {}
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, target_size);
        }
    }

    // ========================================================================
    // Int128 decomposition ops (Lo64, Hi64, Pair64)
    // ========================================================================

    /// Lo64: extract low 64 bits from 128-bit pseudo.
    pub(super) fn emit_lo64(&mut self, insn: &Instruction) {
        let src = insn.src[0];
        let target = insn.target.expect("Lo64 must have target");
        // Load both halves, use lo
        self.load_int128(src, Reg::X9, Reg::X10);
        let dst_loc = self.get_location(target);
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::X9 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::X9),
                        dst: r,
                    });
                }
            }
            _ => self.emit_move_to_loc(Reg::X9, &dst_loc, 64),
        }
    }

    /// Hi64: extract high 64 bits from 128-bit pseudo.
    pub(super) fn emit_hi64(&mut self, insn: &Instruction) {
        let src = insn.src[0];
        let target = insn.target.expect("Hi64 must have target");
        // Load both halves, use hi
        self.load_int128(src, Reg::X9, Reg::X10);
        let dst_loc = self.get_location(target);
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::X10 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::X10),
                        dst: r,
                    });
                }
            }
            _ => self.emit_move_to_loc(Reg::X10, &dst_loc, 64),
        }
    }

    /// Pair64: combine two 64-bit pseudos into 128-bit.
    pub(super) fn emit_pair64(&mut self, insn: &Instruction) {
        let src_lo = insn.src[0];
        let src_hi = insn.src[1];
        let target = insn.target.expect("Pair64 must have target");

        self.emit_move(src_lo, Reg::X9, 64);
        self.emit_move(src_hi, Reg::X10, 64);
        self.store_int128(Reg::X9, Reg::X10, target);
    }

    /// AddC/AdcC: 64-bit add with carry.
    /// AddC (with_carry=false): adds (sets flags)
    /// AdcC (with_carry=true): adc (add with carry in)
    pub(super) fn emit_addc(&mut self, insn: &Instruction, with_carry: bool) {
        let target = insn.target.expect("AddC/AdcC must have target");
        let src1 = insn.src[0];
        let src2 = insn.src[1];
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        self.emit_move(src1, dst_reg, 64);
        self.emit_move(src2, Reg::X10, 64);

        if with_carry {
            self.push_lir(Aarch64Inst::Adc {
                size: OperandSize::B64,
                src1: dst_reg,
                src2: Reg::X10,
                dst: dst_reg,
            });
        } else {
            self.push_lir(Aarch64Inst::Adds {
                size: OperandSize::B64,
                src1: dst_reg,
                src2: GpOperand::Reg(Reg::X10),
                dst: dst_reg,
            });
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 64);
        }
    }

    /// SubC/SbcC: 64-bit sub with borrow.
    /// SubC (with_borrow=false): subs (sets flags)
    /// SbcC (with_borrow=true): sbc (sub with borrow in)
    pub(super) fn emit_subc(&mut self, insn: &Instruction, with_borrow: bool) {
        let target = insn.target.expect("SubC/SbcC must have target");
        let src1 = insn.src[0];
        let src2 = insn.src[1];
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        self.emit_move(src1, dst_reg, 64);
        self.emit_move(src2, Reg::X10, 64);

        if with_borrow {
            self.push_lir(Aarch64Inst::Sbc {
                size: OperandSize::B64,
                src1: dst_reg,
                src2: Reg::X10,
                dst: dst_reg,
            });
        } else {
            self.push_lir(Aarch64Inst::Subs {
                size: OperandSize::B64,
                src1: dst_reg,
                src2: GpOperand::Reg(Reg::X10),
                dst: dst_reg,
            });
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 64);
        }
    }

    /// UMulHi: upper 64 bits of 64×64 unsigned multiply.
    pub(super) fn emit_umulhi(&mut self, insn: &Instruction) {
        let target = insn.target.expect("UMulHi must have target");
        let src1 = insn.src[0];
        let src2 = insn.src[1];
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        self.emit_move(src1, Reg::X9, 64);
        self.emit_move(src2, Reg::X10, 64);

        self.push_lir(Aarch64Inst::Umulh {
            src1: Reg::X9,
            src2: Reg::X10,
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 64);
        }
    }
}
