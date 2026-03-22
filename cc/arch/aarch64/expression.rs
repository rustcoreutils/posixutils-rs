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

        if size == 128 {
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

        if size == 128 {
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

        if size == 128 {
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

        if size == 128 {
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

        if size == 128 {
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

        // Handle 128-bit extensions and truncations
        if insn.size == 128 || insn.src_size == 128 {
            self.emit_int128_extend(insn);
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

    /// Emit 128-bit binary operation (add, sub, and, or, xor, shl, lsr, asr, mul)
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

        // For shift ops, src2 is the shift amount (a regular small integer, not int128).
        // Load it as a 64-bit value into X11 only. For all other ops, load as 128-bit.
        let is_shift = matches!(insn.op, Opcode::Shl | Opcode::Lsr | Opcode::Asr);
        if is_shift {
            self.emit_move(src2, Reg::X11, 64);
        } else {
            self.load_int128(src2, Reg::X11, Reg::X16);
        }

        match insn.op {
            Opcode::Add => {
                // adds x9, x9, x11  (lo + lo, set carry)
                // adc  x10, x10, x16 (hi + hi + carry)
                self.push_lir(Aarch64Inst::Adds {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: GpOperand::Reg(Reg::X11),
                    dst: Reg::X9,
                });
                self.push_lir(Aarch64Inst::Adc {
                    size: OperandSize::B64,
                    src1: Reg::X10,
                    src2: Reg::X16,
                    dst: Reg::X10,
                });
            }
            Opcode::Sub => {
                // subs x9, x9, x11  (lo - lo, set borrow)
                // sbc  x10, x10, x16 (hi - hi - borrow)
                self.push_lir(Aarch64Inst::Subs {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: GpOperand::Reg(Reg::X11),
                    dst: Reg::X9,
                });
                self.push_lir(Aarch64Inst::Sbc {
                    size: OperandSize::B64,
                    src1: Reg::X10,
                    src2: Reg::X16,
                    dst: Reg::X10,
                });
            }
            Opcode::And => {
                self.push_lir(Aarch64Inst::And {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: GpOperand::Reg(Reg::X11),
                    dst: Reg::X9,
                });
                self.push_lir(Aarch64Inst::And {
                    size: OperandSize::B64,
                    src1: Reg::X10,
                    src2: GpOperand::Reg(Reg::X16),
                    dst: Reg::X10,
                });
            }
            Opcode::Or => {
                self.push_lir(Aarch64Inst::Orr {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: GpOperand::Reg(Reg::X11),
                    dst: Reg::X9,
                });
                self.push_lir(Aarch64Inst::Orr {
                    size: OperandSize::B64,
                    src1: Reg::X10,
                    src2: GpOperand::Reg(Reg::X16),
                    dst: Reg::X10,
                });
            }
            Opcode::Xor => {
                self.push_lir(Aarch64Inst::Eor {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: GpOperand::Reg(Reg::X11),
                    dst: Reg::X9,
                });
                self.push_lir(Aarch64Inst::Eor {
                    size: OperandSize::B64,
                    src1: Reg::X10,
                    src2: GpOperand::Reg(Reg::X16),
                    dst: Reg::X10,
                });
            }
            Opcode::Mul => {
                // 128-bit multiply: (lo1, hi1) * (lo2, hi2)
                // result_lo = lo1 * lo2 (lower 64 bits)
                // result_hi = umulh(lo1, lo2) + hi1*lo2 + lo1*hi2
                //
                // X9=lo1, X10=hi1, X11=lo2, X16=hi2
                // X17 = umulh(lo1, lo2)
                self.push_lir(Aarch64Inst::Umulh {
                    src1: Reg::X9,
                    src2: Reg::X11,
                    dst: Reg::X17,
                });
                // X17 = X17 + hi1*lo2 = madd(X10, X11, X17)
                self.push_lir(Aarch64Inst::MAdd {
                    size: OperandSize::B64,
                    src1: Reg::X10,
                    src2: Reg::X11,
                    acc: Reg::X17,
                    dst: Reg::X17,
                });
                // X17 = X17 + lo1*hi2 = madd(X9, X16, X17)
                self.push_lir(Aarch64Inst::MAdd {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: Reg::X16,
                    acc: Reg::X17,
                    dst: Reg::X17,
                });
                // X9 = lo1 * lo2 (lower 64 bits)
                self.push_lir(Aarch64Inst::Mul {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: Reg::X11,
                    dst: Reg::X9,
                });
                // hi result in X10
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::X17),
                    dst: Reg::X10,
                });
            }
            Opcode::Shl => {
                // 128-bit left shift: shift amount in X11 (lo half of src2)
                self.emit_int128_shl();
            }
            Opcode::Lsr => {
                // 128-bit logical right shift
                self.emit_int128_lsr();
            }
            Opcode::Asr => {
                // 128-bit arithmetic right shift
                self.emit_int128_asr();
            }
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

    /// Emit 128-bit division (calls __udivti3/__divti3/__umodti3/__modti3 runtime helpers)
    fn emit_int128_div(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // AAPCS64: 128-bit args passed in X0:X1 (first) and X2:X3 (second)
        // Return value in X0:X1
        self.load_int128(src1, Reg::X0, Reg::X1);
        self.load_int128(src2, Reg::X2, Reg::X3);

        let func_name = match insn.op {
            Opcode::DivS => "__divti3",
            Opcode::DivU => "__udivti3",
            Opcode::ModS => "__modti3",
            Opcode::ModU => "__umodti3",
            _ => return,
        };

        use crate::arch::lir::CallTarget;
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(crate::arch::lir::Symbol::extern_sym(func_name)),
        });

        // Result in X0:X1 -> store to target
        // Move to X9:X10 first to avoid clobbering if target overlaps arg regs
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::X0),
            dst: Reg::X9,
        });
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::X1),
            dst: Reg::X10,
        });
        self.store_int128(Reg::X9, Reg::X10, target);
    }

    /// Emit 128-bit comparison
    fn emit_int128_compare(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        self.load_int128(src1, Reg::X9, Reg::X10);
        self.load_int128(src2, Reg::X11, Reg::X16);

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X17,
        };

        match insn.op {
            Opcode::SetEq | Opcode::SetNe => {
                // Eq/Ne: eor both halves, orr results, compare with zero
                // X9 = lo1 ^ lo2
                self.push_lir(Aarch64Inst::Eor {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: GpOperand::Reg(Reg::X11),
                    dst: Reg::X9,
                });
                // X10 = hi1 ^ hi2
                self.push_lir(Aarch64Inst::Eor {
                    size: OperandSize::B64,
                    src1: Reg::X10,
                    src2: GpOperand::Reg(Reg::X16),
                    dst: Reg::X10,
                });
                // X9 = X9 | X10
                self.push_lir(Aarch64Inst::Orr {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: GpOperand::Reg(Reg::X10),
                    dst: Reg::X9,
                });
                // cmp X9, #0
                self.push_lir(Aarch64Inst::Cmp {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: GpOperand::Imm(0),
                });
                let cond = if insn.op == Opcode::SetEq {
                    CondCode::Eq
                } else {
                    CondCode::Ne
                };
                self.push_lir(Aarch64Inst::Cset { cond, dst: dst_reg });
            }
            _ => {
                // Ordered comparisons: branch-based approach to avoid ccmp nzcv=0 bug.
                // Compare hi halves first; if not equal, hi comparison determines result.
                // If hi halves are equal, compare lo halves (always unsigned tiebreaker).
                let label_hi_gt = self.next_unique_label("i128cmp");
                let label_hi_lt = self.next_unique_label("i128cmp");
                let label_done = self.next_unique_label("i128cmp");

                // Determine signedness and what result to produce in each case.
                // For signed: compare hi with signed conditions.
                // For unsigned: compare hi with unsigned conditions.
                let (hi_gt_cond, hi_lt_cond, lo_cond, hi_gt_val, hi_lt_val) = match insn.op {
                    // SetLt: result=1 when src1 < src2
                    Opcode::SetLt => (CondCode::Sgt, CondCode::Slt, CondCode::Ult, 0i64, 1i64),
                    // SetLe: result=1 when src1 <= src2
                    Opcode::SetLe => (CondCode::Sgt, CondCode::Slt, CondCode::Ule, 0, 1),
                    // SetGt: result=1 when src1 > src2
                    Opcode::SetGt => (CondCode::Sgt, CondCode::Slt, CondCode::Ugt, 1, 0),
                    // SetGe: result=1 when src1 >= src2
                    Opcode::SetGe => (CondCode::Sgt, CondCode::Slt, CondCode::Uge, 1, 0),
                    // SetB (unsigned <): result=1 when src1 < src2
                    Opcode::SetB => (CondCode::Ugt, CondCode::Ult, CondCode::Ult, 0, 1),
                    // SetBe (unsigned <=)
                    Opcode::SetBe => (CondCode::Ugt, CondCode::Ult, CondCode::Ule, 0, 1),
                    // SetA (unsigned >)
                    Opcode::SetA => (CondCode::Ugt, CondCode::Ult, CondCode::Ugt, 1, 0),
                    // SetAe (unsigned >=)
                    Opcode::SetAe => (CondCode::Ugt, CondCode::Ult, CondCode::Uge, 1, 0),
                    _ => return,
                };

                // Compare hi halves (X10=hi1, X16=hi2)
                self.push_lir(Aarch64Inst::Cmp {
                    size: OperandSize::B64,
                    src1: Reg::X10,
                    src2: GpOperand::Reg(Reg::X16),
                });
                self.push_lir(Aarch64Inst::BCond {
                    cond: hi_gt_cond,
                    target: label_hi_gt.clone(),
                });
                self.push_lir(Aarch64Inst::BCond {
                    cond: hi_lt_cond,
                    target: label_hi_lt.clone(),
                });

                // Hi halves equal: compare lo halves (unsigned tiebreaker)
                self.push_lir(Aarch64Inst::Cmp {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: GpOperand::Reg(Reg::X11),
                });
                self.push_lir(Aarch64Inst::Cset {
                    cond: lo_cond,
                    dst: dst_reg,
                });
                self.push_lir(Aarch64Inst::B {
                    target: label_done.clone(),
                });

                // Hi1 > Hi2 (signed or unsigned depending on comparison)
                self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_hi_gt)));
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(hi_gt_val),
                    dst: dst_reg,
                });
                self.push_lir(Aarch64Inst::B {
                    target: label_done.clone(),
                });

                // Hi1 < Hi2 (signed or unsigned depending on comparison)
                self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_hi_lt)));
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(hi_lt_val),
                    dst: dst_reg,
                });

                // Done
                self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(label_done)));
            }
        }

        // Store as 64-bit so CBR's 64-bit load doesn't read stack garbage
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 64);
        }
    }

    /// Emit 128-bit unary operation (neg, not)
    fn emit_int128_unary(&mut self, insn: &Instruction, op: UnaryOp) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        self.load_int128(src, Reg::X9, Reg::X10);

        match op {
            UnaryOp::Neg => {
                // negs x9, x9  (negate lo, set flags)
                // ngc  x10, x10 (negate hi with borrow)
                self.push_lir(Aarch64Inst::Negs {
                    size: OperandSize::B64,
                    src: Reg::X9,
                    dst: Reg::X9,
                });
                self.push_lir(Aarch64Inst::Ngc {
                    size: OperandSize::B64,
                    src: Reg::X10,
                    dst: Reg::X10,
                });
            }
            UnaryOp::Not => {
                // mvn x9, x9
                // mvn x10, x10
                self.push_lir(Aarch64Inst::Mvn {
                    size: OperandSize::B64,
                    src: Reg::X9,
                    dst: Reg::X9,
                });
                self.push_lir(Aarch64Inst::Mvn {
                    size: OperandSize::B64,
                    src: Reg::X10,
                    dst: Reg::X10,
                });
            }
        }

        self.store_int128(Reg::X9, Reg::X10, target);
    }

    /// Emit 128-bit extend/truncate operations
    fn emit_int128_extend(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        match insn.op {
            Opcode::Zext => {
                // Zero extend to 128: lo = src, hi = 0
                let dst_loc = self.get_location(target);
                if let Loc::Stack(dst_offset) = dst_loc {
                    self.emit_move(src, Reg::X9, 64);
                    // Zero-extend from smaller source if needed
                    match insn.src_size {
                        8 => {
                            self.push_lir(Aarch64Inst::Uxtb {
                                src: Reg::X9,
                                dst: Reg::X9,
                            });
                        }
                        16 => {
                            self.push_lir(Aarch64Inst::Uxth {
                                src: Reg::X9,
                                dst: Reg::X9,
                            });
                        }
                        32 => {
                            // Writing to w9 zeroes upper 32 bits
                            self.push_lir(Aarch64Inst::Mov {
                                size: OperandSize::B32,
                                src: GpOperand::Reg(Reg::X9),
                                dst: Reg::X9,
                            });
                        }
                        _ => {} // 64-bit: nothing extra needed
                    }
                    let mem = self.stack_mem(dst_offset);
                    self.push_lir(Aarch64Inst::Stp {
                        size: OperandSize::B64,
                        src1: Reg::X9,
                        src2: Reg::Xzr,
                        addr: mem,
                    });
                }
            }
            Opcode::Sext => {
                // Sign extend to 128: lo = src, hi = src >> 63 (sign bit)
                let dst_loc = self.get_location(target);
                if let Loc::Stack(dst_offset) = dst_loc {
                    self.emit_move(src, Reg::X9, 64);
                    // Sign-extend from smaller source if needed
                    match insn.src_size {
                        8 => {
                            self.push_lir(Aarch64Inst::Sxtb {
                                dst_size: OperandSize::B64,
                                src: Reg::X9,
                                dst: Reg::X9,
                            });
                        }
                        16 => {
                            self.push_lir(Aarch64Inst::Sxth {
                                dst_size: OperandSize::B64,
                                src: Reg::X9,
                                dst: Reg::X9,
                            });
                        }
                        32 => {
                            self.push_lir(Aarch64Inst::Sxtw {
                                src: Reg::X9,
                                dst: Reg::X9,
                            });
                        }
                        _ => {} // 64-bit: nothing extra needed
                    }
                    // hi = lo >> 63 (arithmetic)
                    self.push_lir(Aarch64Inst::Asr {
                        size: OperandSize::B64,
                        src: Reg::X9,
                        amount: GpOperand::Imm(63),
                        dst: Reg::X10,
                    });
                    let mem = self.stack_mem(dst_offset);
                    self.push_lir(Aarch64Inst::Stp {
                        size: OperandSize::B64,
                        src1: Reg::X9,
                        src2: Reg::X10,
                        addr: mem,
                    });
                }
            }
            Opcode::Trunc => {
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
            _ => {}
        }
    }
}
