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

        // 128-bit shifts are still handled by the backend (mapping pass doesn't expand them)
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

        // Get second operand as GpOperand.
        //
        // Which immediates an instruction accepts depends on the instruction:
        // `add`/`sub` take a 12-bit unsigned value, the logical operations take
        // a *bitmask* immediate that cannot even represent 0, and a shift takes
        // an amount below the operand width. One 0..=4095 test served all
        // three, so `x & 0`, `x | 1000` and `x << 32` each emitted an operand
        // the assembler rejects and the build failed.
        //
        // Anything not encodable goes to a register, which every form accepts.
        let src2_loc = self.get_location(src2);
        let src2_operand = match &src2_loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Imm(v) if immediate_fits(insn.op, *v, size) => GpOperand::Imm(*v as i64),
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

        // Handle truncation FROM 128-bit (Zext/Sext TO 128 handled by mapping pass)
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
                        // Writing a W register zeros the upper half, but the
                        // move above is 64 bits wide, so the truncation has to
                        // be materialized here: a later widening would
                        // otherwise read bits it is supposed to drop. x86-64
                        // emits the same self-move for the same reason.
                        self.push_lir(Aarch64Inst::Mov {
                            size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
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
            ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // Either frame: a `__int128` parameter that overflowed the
                // register file arrives in the caller's. Without its arm this
                // fell to the fallback below, which loads 64 bits and zeroes
                // the top half -- a silent wrong answer, not a crash.
                let mem = self.loc_mem(l).unwrap();
                self.emit_ldp_legalized(OperandSize::B64, mem, lo_reg, hi_reg);
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
            self.emit_stp_legalized(OperandSize::B64, lo_reg, hi_reg, mem);
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
            32 => {
                // The low half arrived in a whole X register, so "already the
                // right width" was only true of the *store* that usually
                // follows. Anything that read the pseudo directly -- widening
                // it back, in `__builtin_add_overflow` -- saw the bits the
                // truncation was supposed to drop, and the builtin compared
                // its result against an untruncated copy of itself.
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(dst_reg),
                    dst: dst_reg,
                });
            }
            64 => {
                // Already the whole register.
            }
            _ => {}
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, target_size);
        }
    }

    // Int128 decomposition ops (Lo64, Hi64, Pair64)

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

/// Whether `value` can be written as an immediate operand of `op` at `size`
/// bits, rather than having to be moved into a register first.
///
/// Each family has its own encoding and they are not interchangeable:
///
/// - `add` / `sub` take a 12-bit unsigned value (a shifted-by-12 form also
///   exists; not used here).
/// - `and` / `orr` / `eor` take a *bitmask immediate*, which encodes a rotated
///   run of ones replicated across the register. It cannot represent 0, cannot
///   represent all-ones, and cannot represent most ordinary numbers -- 1000
///   and 3000 are both unencodable.
/// - `lsl` / `lsr` / `asr` take a shift amount below the operand width.
fn immediate_fits(op: Opcode, value: i128, size: u32) -> bool {
    match op {
        Opcode::Add | Opcode::Sub => (0..=4095).contains(&value),
        Opcode::And | Opcode::Or | Opcode::Xor => {
            value >= 0 && is_logical_immediate(value as u64, size)
        }
        Opcode::Shl | Opcode::Lsr | Opcode::Asr => {
            let width = if size > 32 { 64 } else { 32 } as i128;
            (0..width).contains(&value)
        }
        // Anything else reaching this operand builder takes a register.
        _ => false,
    }
}

/// Whether `value` is a valid AArch64 logical (bitmask) immediate.
///
/// The encoding describes an element of 2, 4, 8, 16, 32 or 64 bits holding a
/// run of consecutive ones, rotated within the element, replicated to fill the
/// register. All-zeros and all-ones have no encoding, which is why `x & 0`
/// assembled as `and w1, w1, #0` and was refused.
///
/// A wrong "no" here only costs a register move; a wrong "yes" emits assembly
/// the assembler rejects, so the test is exact rather than approximate.
fn is_logical_immediate(value: u64, size: u32) -> bool {
    let width: u32 = if size > 32 { 64 } else { 32 };
    let mask = if width == 64 {
        u64::MAX
    } else {
        u32::MAX as u64
    };
    let value = value & mask;
    if value == 0 || value == mask {
        return false;
    }

    // Find the smallest element the value repeats at.
    let mut elem = width;
    loop {
        let half = elem / 2;
        let half_mask = (1u64 << half) - 1;
        if (value & half_mask) != ((value >> half) & half_mask) {
            break;
        }
        elem = half;
        if elem <= 2 {
            break;
        }
    }

    // Within one element the ones must be consecutive, allowing for rotation:
    // rotating the element until the ones are contiguous at the bottom leaves a
    // value one less than a power of two.
    let elem_mask = if elem == 64 {
        u64::MAX
    } else {
        (1u64 << elem) - 1
    };
    let e = value & elem_mask;
    if e == 0 || e == elem_mask {
        return false;
    }
    // Rotate within the element. `r == 0` is the unrotated value, and spelling
    // it as `e << (elem - r)` shifts by `elem`, which is undefined at 64 --
    // masked to a no-op in release, an "attempt to shift left with overflow"
    // panic in a debug build, and so an ICE on `x & 0xffffffff00000000`.
    let rotate = |r: u32| -> u64 {
        if r == 0 {
            e
        } else {
            ((e >> r) | (e << (elem - r))) & elem_mask
        }
    };
    (0..elem).map(rotate).any(|r| (r + 1) & r == 0)
}

#[cfg(test)]
mod immediate_tests {
    use super::*;

    /// The values the AArch64 assembler accepts for `and w0, w0, #N`.
    ///
    /// Checked against `aarch64-linux-gnu-as` over all 65 536 values of a
    /// 16-bit immediate: it accepts 136 of them, and this predicate answers
    /// yes for exactly those 136 -- no wrong yes, which would emit assembly
    /// the assembler rejects, and no wrong no, which would cost a register
    /// move. The cases below are a readable sample of that run.
    #[test]
    fn logical_immediates_match_the_encoding() {
        // Encodable: a rotated run of ones, replicated.
        for v in [1u64, 3, 7, 255, 4095, 0xFFFF, 0x5555_5555, 0xF0F0_F0F0] {
            assert!(is_logical_immediate(v, 32), "{v:#x} should encode");
        }
        // Not encodable, and the two that mattered: zero has no encoding at
        // all, and ordinary numbers mostly do not.
        for v in [0u64, 1000, 3000, 0xFFFF_FFFF] {
            assert!(!is_logical_immediate(v, 32), "{v:#x} should not encode");
        }
        // 64-bit forms.
        assert!(is_logical_immediate(0xFFFF_FFFF_0000_0000, 64));
        assert!(is_logical_immediate(0x5555_5555_5555_5555, 64));
        assert!(!is_logical_immediate(0, 64));
        assert!(!is_logical_immediate(u64::MAX, 64));
        assert!(!is_logical_immediate(0x0123_4567_89AB_CDEF, 64));
    }

    /// A 64-bit element must survive the unrotated case.
    ///
    /// Every value whose halves differ leaves the repeat width at 64, so the
    /// rotation loop reaches `r == 0` with `elem == 64`. Written as
    /// `e << (elem - r)` that is a shift by 64: masked to a no-op in release,
    /// but an "attempt to shift left with overflow" panic in a debug build --
    /// which made `cargo test` fail outright and a debug-built c17 ICE on
    /// `x & 0xffffffff00000000`. Release and debug must agree.
    #[test]
    fn logical_immediates_handle_an_unrotated_64_bit_element() {
        // Encodable only at rotation 0 or by wrapping: a single run of ones
        // that does not repeat below 64 bits.
        assert!(is_logical_immediate(0x0000_0000_FFFF_FFFF, 64));
        assert!(is_logical_immediate(0xFFFF_FFFF_0000_0000, 64));
        assert!(is_logical_immediate(0x0000_FFFF_FFFF_0000, 64));
        // Not encodable, and still must not panic.
        for v in [
            0x0123_4567_89AB_CDEFu64,
            0xDEAD_BEEF_CAFE_BABE,
            0x8000_0000_0000_0001,
        ] {
            let _ = is_logical_immediate(v, 64);
        }
        // The whole 64-bit space of single-bit values, every one of which
        // leaves `elem` at 64 and so walks the full rotation loop.
        for bit in 0..64 {
            assert!(
                is_logical_immediate(1u64 << bit, 64),
                "a single set bit at {bit} is always encodable"
            );
        }
    }

    /// Each family has its own encoding, so one 0..=4095 test cannot serve
    /// all three: `x & 0`, `x | 1000` and `x << 32` all name operands the
    /// assembler rejects.
    #[test]
    fn each_opcode_family_has_its_own_immediate_range() {
        // add/sub: 12-bit unsigned.
        assert!(immediate_fits(Opcode::Add, 4095, 32));
        assert!(!immediate_fits(Opcode::Add, 4096, 32));
        assert!(!immediate_fits(Opcode::Sub, -1, 32));

        // logical: bitmask immediate, so 4095 fits and 1000 does not.
        assert!(immediate_fits(Opcode::And, 4095, 32));
        assert!(!immediate_fits(Opcode::And, 1000, 32));
        assert!(!immediate_fits(Opcode::Or, 0, 32));
        assert!(!immediate_fits(Opcode::Xor, 0, 32));

        // shifts: below the operand width.
        assert!(immediate_fits(Opcode::Shl, 31, 32));
        assert!(!immediate_fits(Opcode::Shl, 32, 32));
        assert!(immediate_fits(Opcode::Lsr, 63, 64));
        assert!(!immediate_fits(Opcode::Asr, 64, 64));
        assert!(!immediate_fits(Opcode::Shl, -1, 32));
    }
}
