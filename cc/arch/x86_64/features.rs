//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Feature Code Generation (Variadic Functions, Byte Swapping, Bit Counting)
//

use super::codegen::X86_64CodeGen;
use super::lir::{GpOperand, MemAddr, ShiftCount, X86Inst};
use super::regalloc::{Loc, Reg, XmmReg};
use crate::arch::codegen::BswapSize;
use crate::arch::lir::{CallTarget, CondCode, Directive, Label, OperandSize, Symbol};
use crate::ir::Instruction;
use crate::types::TypeTable;

impl X86_64CodeGen {
    // ========================================================================
    // Variadic function support (va_* builtins)
    // ========================================================================
    //
    // On x86-64 System V ABI, va_list is a 24-byte struct:
    //   struct {
    //       unsigned int gp_offset;     // offset to next GP reg in save area
    //       unsigned int fp_offset;     // offset to next FP reg in save area
    //       void *overflow_arg_area;    // pointer to stack arguments
    //       void *reg_save_area;        // pointer to register save area
    //   };
    //
    // This implementation provides a simplified version that works with
    // stack-based arguments. Full register save area support would require
    // function prologue changes.

    /// Emit va_start: Initialize va_list
    pub(super) fn emit_va_start(&mut self, insn: &Instruction) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };

        let ap_loc = self.get_location(ap_addr);

        // For x86-64 System V ABI:
        // va_list is a 24-byte struct. We initialize:
        // - gp_offset = fixed_gp_params * 8 (offset to first variadic GP arg in save area)
        // - fp_offset = 48 + fixed_fp_params * 16 (offset to first variadic FP arg)
        // - overflow_arg_area = rbp + 16 (where stack args start, for overflow)
        // - reg_save_area = pointer to where we saved the argument registers

        let gp_offset = (self.num_fixed_gp_params * 8) as i32;
        let fp_offset = 48 + (self.num_fixed_fp_params * 16) as i32;
        let reg_save_base = self.reg_save_area_offset;

        match ap_loc {
            Loc::Stack(offset) => {
                // gp_offset = offset to next variadic GP arg
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(gp_offset as i64),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset,
                    }),
                });
                // fp_offset = offset to next variadic FP arg (48 + fixed_fp_params * 16)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(fp_offset as i64),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: offset + 4,
                    }),
                });
                // overflow_arg_area = rbp + 16
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: 16,
                    },
                    dst: Reg::Rax,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: offset + 8,
                    }),
                });
                // reg_save_area = pointer to saved registers
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -reg_save_base,
                    },
                    dst: Reg::Rax,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: offset + 16,
                    }),
                });
            }
            Loc::Reg(r) => {
                // Register contains the address of the va_list struct
                // Use R10 as scratch to avoid clobbering the va_list address if r == Rax
                // gp_offset = offset to next variadic GP arg
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(gp_offset as i64),
                    dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                });
                // fp_offset = offset to next variadic FP arg
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(fp_offset as i64),
                    dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 4 }),
                });
                // overflow_arg_area = rbp + 16
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: 16,
                    },
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 8 }),
                });
                // reg_save_area = pointer to saved registers
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -reg_save_base,
                    },
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: 16,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Helper for emit_va_arg: emit integer path for va_arg
    pub(super) fn emit_va_arg_int(
        &mut self,
        ap_base: Reg,
        ap_base_offset: i32,
        dst_loc: &Loc,
        arg_size: u32,
        arg_bytes: i32,
        label_suffix: u32,
    ) {
        let overflow_label = Label::new("va_overflow", label_suffix);
        let done_label = Label::new("va_done", label_suffix);
        let lir_arg_size = OperandSize::from_bits(arg_size);

        // This function uses Rax for reg_save_area/overflow pointer, Rcx for sign-extended offset.
        // The constraint-aware register allocator ensures ap_base is never in Rax or Rcx
        // by declaring VaArg as clobbering those registers in opcode_constraints().

        // Load gp_offset into R10d (using R10 as scratch for gp_offset)
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset,
            }),
            dst: GpOperand::Reg(Reg::R10),
        });
        // Compare with 48
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B32,
            src: GpOperand::Imm(48),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: overflow_label.clone(),
        });

        // Register save area path
        // Load reg_save_area into Rax
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 16,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });
        // Sign-extend gp_offset (in R10) to 64-bit and add to reg_save_area
        self.push_lir(X86Inst::Movsx {
            src_size: OperandSize::B32,
            dst_size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: Reg::Rcx,
        });
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rcx),
            dst: Reg::Rax,
        });

        // Store value from [Rax] to destination
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Mov {
                    size: lir_arg_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rax,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Stack(dst_offset) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rax,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(Reg::R11),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::R11),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_offset,
                    }),
                });
            }
            _ => {}
        }

        // Increment gp_offset by 8 and store back
        self.push_lir(X86Inst::Add {
            size: OperandSize::B32,
            src: GpOperand::Imm(8),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset,
            }),
        });
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        // Overflow path
        // Bug fix: Load overflow_arg_area pointer into R11 FIRST, then load value into Rax.
        // This prevents the pointer from being clobbered when storing to a register destination.
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
            dst: GpOperand::Reg(Reg::R11),
        });

        // Load value from [R11] into Rax, then store to destination
        self.push_lir(X86Inst::Mov {
            size: lir_arg_size,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R11,
                offset: 0,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });

        // Store value from Rax to destination
        match dst_loc {
            Loc::Reg(r) => {
                if *r != Reg::Rax {
                    self.push_lir(X86Inst::Mov {
                        size: lir_arg_size,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Reg(*r),
                    });
                }
            }
            Loc::Stack(dst_offset) => {
                self.push_lir(X86Inst::Mov {
                    size: lir_arg_size,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_offset,
                    }),
                });
            }
            _ => {}
        }

        // Advance overflow_arg_area (using R11 which still has the original pointer)
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(arg_bytes as i64),
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }

    pub(super) fn emit_va_arg(&mut self, insn: &Instruction, types: &TypeTable) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let arg_type = insn.typ.unwrap_or(types.int_id);
        let arg_size = types.size_bits(arg_type).max(32);
        let arg_bytes = (arg_size / 8).max(8) as i32;

        let ap_loc = self.get_location(ap_addr);
        let dst_loc = self.get_location(target);

        let label_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;

        match &ap_loc {
            Loc::Stack(ap_offset) => {
                if types.is_float(arg_type) {
                    self.emit_va_arg_float(
                        Reg::Rbp,
                        *ap_offset,
                        &dst_loc,
                        arg_type,
                        label_suffix,
                        types,
                    );
                } else {
                    self.emit_va_arg_int(
                        Reg::Rbp,
                        *ap_offset,
                        &dst_loc,
                        arg_size,
                        arg_bytes,
                        label_suffix,
                    );
                }
            }
            Loc::Reg(ap_reg) => {
                if types.is_float(arg_type) {
                    self.emit_va_arg_float(*ap_reg, 0, &dst_loc, arg_type, label_suffix, types);
                } else {
                    self.emit_va_arg_int(*ap_reg, 0, &dst_loc, arg_size, arg_bytes, label_suffix);
                }
            }
            _ => {}
        }
    }

    /// Emit va_copy: Copy a va_list (24 bytes)
    pub(super) fn emit_va_copy(&mut self, insn: &Instruction) {
        let dest_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let src_addr = match insn.src.get(1) {
            Some(&s) => s,
            None => return,
        };

        let dest_loc = self.get_location(dest_addr);
        let src_loc = self.get_location(src_addr);

        // Copy 24 bytes from src to dest
        // Both src_loc and dest_loc contain addresses of va_list structs
        match (&src_loc, &dest_loc) {
            (Loc::Stack(src_off), Loc::Stack(dst_off)) => {
                // Copy gp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *src_off,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_off,
                    }),
                });
                // Copy fp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 4,
                    }),
                });
                // Copy overflow_arg_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 8,
                    }),
                });
                // Copy reg_save_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 16,
                    }),
                });
            }
            (Loc::Reg(src_reg), Loc::Reg(dst_reg)) => {
                // Both src and dest are in registers (containing addresses)
                // Copy gp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 0,
                    }),
                });
                // Copy fp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 4,
                    }),
                });
                // Copy overflow_arg_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 8,
                    }),
                });
                // Copy reg_save_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 16,
                    }),
                });
            }
            (Loc::Reg(src_reg), Loc::Stack(dst_off)) => {
                // Src in register, dest on stack
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_off,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 4,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 8,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 16,
                    }),
                });
            }
            (Loc::Stack(src_off), Loc::Reg(dst_reg)) => {
                // Src on stack, dest in register
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *src_off,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 0,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 4,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 8,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 16,
                    }),
                });
            }
            _ => {}
        }
    }

    // =========================================================================
    // Byte-swapping builtins
    // =========================================================================

    /// Emit byte-swap instruction for 16/32/64-bit values
    pub(super) fn emit_bswap(&mut self, insn: &Instruction, swap_size: BswapSize) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let dst = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(dst);
        let op_size = match swap_size {
            BswapSize::B16 => OperandSize::B16,
            BswapSize::B32 => OperandSize::B32,
            BswapSize::B64 => OperandSize::B64,
        };

        // Load source into R10 (scratch register)
        match (&src_loc, &swap_size) {
            // 16-bit: use zero-extending moves
            (Loc::Reg(r), BswapSize::B16) if *r != Reg::R10 => {
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Reg(*r),
                    dst: Reg::R10,
                });
            }
            (Loc::Stack(off), BswapSize::B16) => {
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *off,
                    }),
                    dst: Reg::R10,
                });
            }
            // 32/64-bit: use regular moves
            (Loc::Reg(r), _) if *r != Reg::R10 => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(*r),
                    dst: GpOperand::Reg(Reg::R10),
                });
            }
            (Loc::Stack(off), _) => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *off,
                    }),
                    dst: GpOperand::Reg(Reg::R10),
                });
            }
            (Loc::Imm(v), _) => {
                self.push_lir(X86Inst::Mov {
                    size: if matches!(swap_size, BswapSize::B16) {
                        OperandSize::B32
                    } else {
                        op_size
                    },
                    src: GpOperand::Imm(*v),
                    dst: GpOperand::Reg(Reg::R10),
                });
            }
            (Loc::Reg(_), _) => {} // Already in R10
            _ => return,
        }

        // Perform byte-swap: 16-bit uses ROR, 32/64-bit uses BSWAP
        match swap_size {
            BswapSize::B16 => {
                self.push_lir(X86Inst::Ror {
                    size: OperandSize::B16,
                    count: ShiftCount::Imm(8),
                    dst: Reg::R10,
                });
            }
            BswapSize::B32 | BswapSize::B64 => {
                self.push_lir(X86Inst::Bswap {
                    size: op_size,
                    reg: Reg::R10,
                });
            }
        }

        // Store result
        match (&dst_loc, &swap_size) {
            // 16-bit: use zero-extending move for register destination
            (Loc::Reg(r), BswapSize::B16) if *r != Reg::R10 => {
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::R10),
                    dst: *r,
                });
            }
            (Loc::Stack(off), BswapSize::B16) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B16,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *off,
                    }),
                });
            }
            // 32/64-bit: use regular moves
            (Loc::Reg(r), _) if *r != Reg::R10 => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Reg(*r),
                });
            }
            (Loc::Stack(off), _) => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit count trailing zeros
    pub(super) fn emit_ctz(&mut self, insn: &Instruction, src_size: OperandSize) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let dst = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(dst);

        // BSF (bit scan forward) finds index of least significant set bit
        // which is equivalent to count of trailing zeros
        // Use R10 as scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Bsf {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: Reg::R10,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Bsf {
                    size: src_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                    dst: Reg::R10,
                });
            }
            Loc::Imm(v) => {
                // Load immediate first, then BSF
                self.push_lir(X86Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Bsf {
                    size: src_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::R10,
                });
            }
            _ => return,
        }

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::R10 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(Reg::R10),
                        dst: GpOperand::Reg(r),
                    });
                }
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit count leading zeros: CLZ(x) = operand_bits - 1 - BSR(x)
    pub(super) fn emit_clz(&mut self, insn: &Instruction, src_size: OperandSize) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let dst = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(dst);

        // BSR (bit scan reverse) finds index of most significant set bit
        // CLZ = (operand_size - 1) - BSR_result
        // Use R10 as scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Bsr {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: Reg::R10,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Bsr {
                    size: src_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                    dst: Reg::R10,
                });
            }
            Loc::Imm(v) => {
                // Load immediate first, then BSR
                self.push_lir(X86Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Bsr {
                    size: src_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::R10,
                });
            }
            _ => return,
        }

        // XOR R10 with (size - 1) to convert BSR result to CLZ
        // Since BSR gives index from LSB, we need (size_bits - 1) - result
        // XOR with (size_bits - 1) achieves this for valid inputs (non-zero)
        let xor_value = (src_size.bits() - 1) as i64;
        self.push_lir(X86Inst::Xor {
            size: OperandSize::B32, // Result is always 32-bit int
            src: GpOperand::Imm(xor_value),
            dst: Reg::R10,
        });

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::R10 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(Reg::R10),
                        dst: GpOperand::Reg(r),
                    });
                }
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit population count
    pub(super) fn emit_popcount(&mut self, insn: &Instruction, src_size: OperandSize) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let dst = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(dst);

        // POPCNT instruction directly counts set bits
        // Use R10 as scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Popcnt {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: Reg::R10,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Popcnt {
                    size: src_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                    dst: Reg::R10,
                });
            }
            Loc::Imm(v) => {
                // Load immediate first, then POPCNT
                self.push_lir(X86Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Popcnt {
                    size: src_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::R10,
                });
            }
            _ => return,
        }

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::R10 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(Reg::R10),
                        dst: GpOperand::Reg(r),
                    });
                }
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                });
            }
            _ => {}
        }
    }

    // ========================================================================
    // setjmp/longjmp/alloca support
    // ========================================================================

    /// Emit setjmp(env) - saves execution context
    /// System V AMD64 ABI: env in RDI, returns int in EAX
    pub(super) fn emit_setjmp(&mut self, insn: &Instruction) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Put env argument in RDI (first argument register)
        self.emit_move(env, Reg::Rdi, 64);

        // Call setjmp
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("setjmp".to_string())),
        });

        // Store result from EAX to target
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::Rax, &dst_loc, 32);
    }

    /// Emit longjmp(env, val) - restores execution context (noreturn)
    /// System V AMD64 ABI: env in RDI, val in RSI
    pub(super) fn emit_longjmp(&mut self, insn: &Instruction) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let val = match insn.src.get(1) {
            Some(&v) => v,
            None => return,
        };

        // IMPORTANT: Load val first into RSI, THEN env into RDI.
        // If we loaded env into RDI first and val was passed as the first
        // function argument (in RDI), it would get overwritten.
        // Put val argument in RSI (second argument register) FIRST
        self.emit_move(val, Reg::Rsi, 32);

        // Put env argument in RDI (first argument register)
        self.emit_move(env, Reg::Rdi, 64);

        // Call longjmp (noreturn - control never comes back)
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("longjmp".to_string())),
        });

        // Emit ud2 after longjmp since it never returns
        // This helps catch any bugs where longjmp somehow returns
        self.push_lir(X86Inst::Ud2);
    }

    /// Emit __builtin_alloca - dynamic stack allocation
    pub(super) fn emit_alloca(&mut self, insn: &Instruction) {
        let size = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load size into R10 (scratch register)
        self.emit_move(size, Reg::R10, 64);

        // Round up to 16-byte alignment: (size + 15) & ~15
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(15),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::And {
            size: OperandSize::B64,
            src: GpOperand::Imm(-16),
            dst: Reg::R10,
        });

        // Subtract from stack pointer
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: Reg::Rsp,
        });

        // Return new stack pointer
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rsp),
            dst: GpOperand::Reg(Reg::R10),
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
    }

    /// Emit __builtin_memset(dest, c, n) - calls memset
    /// System V AMD64 ABI: dest in RDI, c in RSI, n in RDX, returns dest in RAX
    pub(super) fn emit_memset(&mut self, insn: &Instruction) {
        let dest = match insn.src.first() {
            Some(&d) => d,
            None => return,
        };
        let c = match insn.src.get(1) {
            Some(&c) => c,
            None => return,
        };
        let n = match insn.src.get(2) {
            Some(&n) => n,
            None => return,
        };
        let target = insn.target;

        // Load arguments in reverse order to avoid clobbering
        self.emit_move(n, Reg::Rdx, 64);
        self.emit_move(c, Reg::Rsi, 32); // c is int (32-bit)
        self.emit_move(dest, Reg::Rdi, 64);

        // Call memset
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("memset".to_string())),
        });

        // Store result from RAX to target (returns dest)
        if let Some(target) = target {
            let dst_loc = self.get_location(target);
            self.emit_move_to_loc(Reg::Rax, &dst_loc, 64);
        }
    }

    /// Emit __builtin_memcpy(dest, src, n) - calls memcpy
    /// System V AMD64 ABI: dest in RDI, src in RSI, n in RDX, returns dest in RAX
    pub(super) fn emit_memcpy(&mut self, insn: &Instruction) {
        let dest = match insn.src.first() {
            Some(&d) => d,
            None => return,
        };
        let src = match insn.src.get(1) {
            Some(&s) => s,
            None => return,
        };
        let n = match insn.src.get(2) {
            Some(&n) => n,
            None => return,
        };
        let target = insn.target;

        // Load arguments in reverse order to avoid clobbering
        self.emit_move(n, Reg::Rdx, 64);
        self.emit_move(src, Reg::Rsi, 64);
        self.emit_move(dest, Reg::Rdi, 64);

        // Call memcpy
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("memcpy".to_string())),
        });

        // Store result from RAX to target (returns dest)
        if let Some(target) = target {
            let dst_loc = self.get_location(target);
            self.emit_move_to_loc(Reg::Rax, &dst_loc, 64);
        }
    }

    /// Emit __builtin_memmove(dest, src, n) - calls memmove
    /// System V AMD64 ABI: dest in RDI, src in RSI, n in RDX, returns dest in RAX
    pub(super) fn emit_memmove(&mut self, insn: &Instruction) {
        let dest = match insn.src.first() {
            Some(&d) => d,
            None => return,
        };
        let src = match insn.src.get(1) {
            Some(&s) => s,
            None => return,
        };
        let n = match insn.src.get(2) {
            Some(&n) => n,
            None => return,
        };
        let target = insn.target;

        // Load arguments in reverse order to avoid clobbering
        self.emit_move(n, Reg::Rdx, 64);
        self.emit_move(src, Reg::Rsi, 64);
        self.emit_move(dest, Reg::Rdi, 64);

        // Call memmove
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("memmove".to_string())),
        });

        // Store result from RAX to target (returns dest)
        if let Some(target) = target {
            let dst_loc = self.get_location(target);
            self.emit_move_to_loc(Reg::Rax, &dst_loc, 64);
        }
    }

    /// Emit __builtin_frame_address(level) - return frame pointer at given level
    pub(super) fn emit_frame_address(&mut self, insn: &Instruction) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // For level 0, return the current frame pointer (rbp)
        // For other levels, we'd need to walk the frame chain, but we simplify
        // by always returning the current frame pointer
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rbp),
            dst: GpOperand::Reg(Reg::R10),
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
    }

    /// Emit __builtin_return_address(level) - return address at given level
    pub(super) fn emit_return_address(&mut self, insn: &Instruction) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // For level 0, return [rbp+8] (the saved return address)
        // For other levels, we'd need to walk the frame chain
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: 8,
            }),
            dst: GpOperand::Reg(Reg::R10),
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
    }

    /// Emit __builtin_fabsf - absolute value of float
    pub(super) fn emit_fabs32(&mut self, insn: &Instruction) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into XMM0 (first FP argument register)
        self.emit_fp_move(arg, XmmReg::Xmm0, 32);

        // Call fabsf from libc
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("fabsf".to_string())),
        });

        // Result is in XMM0, store to target
        let dst_loc = self.get_location(target);
        self.emit_fp_move_from_xmm(XmmReg::Xmm0, &dst_loc, 32);
    }

    /// Emit __builtin_fabs - absolute value of double
    pub(super) fn emit_fabs64(&mut self, insn: &Instruction) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into XMM0 (first FP argument register)
        self.emit_fp_move(arg, XmmReg::Xmm0, 64);

        // Call fabs from libc
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("fabs".to_string())),
        });

        // Result is in XMM0, store to target
        let dst_loc = self.get_location(target);
        self.emit_fp_move_from_xmm(XmmReg::Xmm0, &dst_loc, 64);
    }
}
