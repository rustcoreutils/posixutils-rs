//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Feature Code Generation (Variadic Functions, Byte Swapping, Bit Counting)
//

use super::codegen::Aarch64CodeGen;
use super::lir::{Aarch64Inst, GpOperand, MemAddr};
use super::regalloc::{Loc, Reg, VReg};
use crate::arch::codegen::BswapSize;
use crate::arch::lir::{CallTarget, FpSize, OperandSize, Symbol};
use crate::ir::Instruction;
use crate::types::TypeTable;

impl Aarch64CodeGen {
    // ========================================================================
    // Variadic function support (va_* builtins)
    // ========================================================================
    //
    // Platform-specific va_list handling:
    //
    // Linux/FreeBSD (AAPCS64):
    //   - va_list is a char* pointing to register save area
    //   - In prologue, we save x0-x7 to the register save area
    //   - va_start computes: reg_save_area + (num_fixed_gp_params * 8)
    //
    // Darwin (macOS/iOS):
    //   - Variadic args are passed on the stack by the caller
    //   - va_list is a char* pointing to the caller's stack
    //   - va_start computes: FP + frame_size (where caller placed variadic args)

    /// Emit va_start: Initialize va_list to point to first variadic arg
    /// Note: ap_addr is the ADDRESS of the va_list variable (from symaddr), not the va_list itself
    pub(super) fn emit_va_start(&mut self, insn: &Instruction, frame_size: i32) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };

        let ap_loc = self.get_location(ap_addr);
        let (scratch0, scratch1, _) = Reg::scratch_regs();

        // Compute address of first variadic argument
        let is_darwin = self.base.target.os == crate::target::Os::MacOS;
        let vararg_offset = if is_darwin {
            // Darwin: Variadic args are on the stack, placed by the caller
            // They start at FP + frame_size (the original SP before prologue)
            frame_size
        } else {
            // Linux/FreeBSD: Variadic args are in the register save area
            // First variadic arg is at: reg_save_area_offset + (num_fixed_gp_params * 8)
            self.reg_save_area_offset + (self.num_fixed_gp_params as i32 * 8)
        };

        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: Reg::X29,
            src2: GpOperand::Imm(vararg_offset as i64),
            dst: scratch0,
        });

        // ap_loc contains the ADDRESS of the va_list variable
        // We need to store scratch0 TO that address (indirect store)
        match ap_loc {
            Loc::Stack(offset) => {
                // The stack location contains the address of ap, load it first
                let actual_offset = self.stack_offset(frame_size, offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                // Store the computed va_list pointer to *scratch1
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(scratch1),
                });
            }
            Loc::Reg(r) => {
                // r contains the address of ap, store scratch0 to [r]
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(r),
                });
            }
            _ => {}
        }
    }

    /// Emit va_arg: Get the next variadic argument of the specified type
    /// Note: ap_addr is the ADDRESS of the va_list variable (from symaddr), not the va_list itself
    pub(super) fn emit_va_arg(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
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
        let arg_bytes = (arg_size / 8).max(8) as i64; // Minimum 8 bytes per slot on ARM64

        let ap_loc = self.get_location(ap_addr);
        let dst_loc = self.get_location(target);
        let (scratch0, scratch1, _) = Reg::scratch_regs();

        // ap_loc contains the ADDRESS of the va_list variable (from symaddr)
        // First, get the address of ap into scratch1, then load ap value from there
        let ap_ptr_reg = match &ap_loc {
            Loc::Stack(offset) => {
                // Stack location contains the address of ap
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                scratch1
            }
            Loc::Reg(r) => {
                // Register contains the address of ap
                if *r != scratch1 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(*r),
                        dst: scratch1,
                    });
                }
                scratch1
            }
            _ => return,
        };

        // Load the current ap pointer value from [ap_ptr_reg]
        self.push_lir(Aarch64Inst::Ldr {
            size: OperandSize::B64,
            addr: MemAddr::Base(ap_ptr_reg),
            dst: scratch0,
        });

        // Load the argument from *ap
        if types.is_float(arg_type) {
            // Load floating point value
            let fp_size = types.size_bits(arg_type);
            let fp_size_enum = FpSize::from_bits(fp_size);
            self.push_lir(Aarch64Inst::LdrFp {
                size: fp_size_enum,
                addr: MemAddr::Base(scratch0),
                dst: VReg::V16,
            });

            // Store to destination
            match &dst_loc {
                Loc::VReg(v) => {
                    self.push_lir(Aarch64Inst::FmovReg {
                        size: fp_size_enum,
                        src: VReg::V16,
                        dst: *v,
                    });
                }
                Loc::Stack(offset) => {
                    // FP-relative for alloca safety
                    let actual_offset = self.stack_offset(frame_size, *offset);
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_size_enum,
                        src: VReg::V16,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: actual_offset,
                        },
                    });
                }
                _ => {}
            }
        } else {
            // Load integer value
            let op_size = OperandSize::from_bits(arg_size);
            self.push_lir(Aarch64Inst::Ldr {
                size: op_size,
                addr: MemAddr::Base(scratch0),
                dst: scratch1,
            });

            // Store to destination
            match &dst_loc {
                Loc::Reg(r) => {
                    if *r != scratch1 {
                        self.push_lir(Aarch64Inst::Mov {
                            size: op_size,
                            src: GpOperand::Reg(scratch1),
                            dst: *r,
                        });
                    }
                }
                Loc::Stack(offset) => {
                    // FP-relative for alloca safety
                    let actual_offset = self.stack_offset(frame_size, *offset);
                    self.push_lir(Aarch64Inst::Str {
                        size: OperandSize::B64,
                        src: scratch1,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: actual_offset,
                        },
                    });
                }
                _ => {}
            }
        }

        // Advance ap by the argument size
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: scratch0,
            src2: GpOperand::Imm(arg_bytes),
            dst: scratch0,
        });

        // Store updated ap back to [&ap] (ap_loc contains the address of ap)
        // Need to reload the address since scratch1 may have been clobbered
        match &ap_loc {
            Loc::Stack(offset) => {
                // Reload the address of ap into scratch1
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                // Store updated ap to [scratch1]
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(scratch1),
                });
            }
            Loc::Reg(r) => {
                // r contains the address of ap, store scratch0 to [r]
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(*r),
                });
            }
            _ => {}
        }
    }

    /// Emit va_copy: Copy a va_list
    /// Note: Both addresses are pointers to va_list variables (from symaddr)
    pub(super) fn emit_va_copy(&mut self, insn: &Instruction, frame_size: i32) {
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
        let (scratch0, scratch1, _) = Reg::scratch_regs();

        // Both src_loc and dest_loc contain ADDRESSES of va_list variables
        // Get the address of src va_list into scratch1
        let src_ptr_reg = match &src_loc {
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                scratch1
            }
            Loc::Reg(r) => *r,
            _ => return,
        };

        // Load the src va_list pointer value from [src_ptr_reg]
        self.push_lir(Aarch64Inst::Ldr {
            size: OperandSize::B64,
            addr: MemAddr::Base(src_ptr_reg),
            dst: scratch0,
        });

        // Get the address of dest va_list and store scratch0 there
        match &dest_loc {
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(scratch1),
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(*r),
                });
            }
            _ => {}
        }
    }

    // =========================================================================
    // Byte-swapping builtins
    // =========================================================================

    /// Emit byte-swap instruction for 16/32/64-bit values
    pub(super) fn emit_bswap(&mut self, insn: &Instruction, frame_size: i32, swap_size: BswapSize) {
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
        let scratch = Reg::X9;
        let op_size = match swap_size {
            BswapSize::B16 => OperandSize::B16,
            BswapSize::B32 => OperandSize::B32,
            BswapSize::B64 => OperandSize::B64,
        };
        // For moves, 16-bit uses 32-bit register operations
        let mov_size = if matches!(swap_size, BswapSize::B16) {
            OperandSize::B32
        } else {
            op_size
        };

        // Load source into scratch register
        match &src_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: mov_size,
                    src: GpOperand::Reg(*r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: op_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: mov_size,
                    src: GpOperand::Imm(*v),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // Perform byte-swap: 16-bit uses Rev16+mask, 32/64-bit uses Rev
        match swap_size {
            BswapSize::B16 => {
                self.push_lir(Aarch64Inst::Rev16 {
                    size: OperandSize::B32,
                    src: scratch,
                    dst: scratch,
                });
                self.push_lir(Aarch64Inst::And {
                    size: OperandSize::B32,
                    src1: scratch,
                    src2: GpOperand::Imm(0xFFFF),
                    dst: scratch,
                });
            }
            BswapSize::B32 | BswapSize::B64 => {
                self.push_lir(Aarch64Inst::Rev {
                    size: op_size,
                    src: scratch,
                    dst: scratch,
                });
            }
        }

        // Store result
        match &dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: mov_size,
                    src: GpOperand::Reg(scratch),
                    dst: *r,
                });
            }
            Loc::Stack(off) => {
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: op_size,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit count trailing zeros: on AArch64, CTZ = CLZ(RBIT(x))
    pub(super) fn emit_ctz(&mut self, insn: &Instruction, frame_size: i32, src_size: OperandSize) {
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
        let scratch = Reg::X9;

        // Load source into scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: src_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // Reverse bits: RBIT
        self.push_lir(Aarch64Inst::Rbit {
            size: src_size,
            src: scratch,
            dst: scratch,
        });

        // Count leading zeros: CLZ - this gives us the count of trailing zeros
        self.push_lir(Aarch64Inst::Clz {
            size: src_size,
            src: scratch,
            dst: scratch,
        });

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit count leading zeros (AArch64 has a direct CLZ instruction)
    pub(super) fn emit_clz(&mut self, insn: &Instruction, frame_size: i32, src_size: OperandSize) {
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
        let scratch = Reg::X9;

        // Load source into scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: src_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                // Use emit_mov_imm which handles large immediates with movz/movk
                self.emit_mov_imm(scratch, v, src_size.bits());
            }
            _ => return,
        }

        // Count leading zeros using CLZ instruction
        self.push_lir(Aarch64Inst::Clz {
            size: src_size,
            src: scratch,
            dst: scratch,
        });

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit population count. On AArch64:
    /// fmov d0, x0; cnt v0.8b, v0.8b; addv b0, v0.8b; fmov w0, s0
    pub(super) fn emit_popcount(
        &mut self,
        insn: &Instruction,
        frame_size: i32,
        src_size: OperandSize,
    ) {
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
        let scratch = Reg::X9;
        let fp_scratch = VReg::V16; // Use reserved scratch FP register

        // Load source into scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: src_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                // Use emit_mov_imm which handles large immediates with movz/movk
                self.emit_mov_imm(scratch, v, src_size.bits());
            }
            _ => return,
        }

        // Move to SIMD register (always use 64-bit for the fmov)
        self.push_lir(Aarch64Inst::FmovFromGp {
            size: FpSize::Double,
            src: scratch,
            dst: fp_scratch,
        });

        // Count bits per byte
        self.push_lir(Aarch64Inst::Cnt {
            src: fp_scratch,
            dst: fp_scratch,
        });

        // Sum all bytes
        self.push_lir(Aarch64Inst::Addv {
            src: fp_scratch,
            dst: fp_scratch,
        });

        // Move result back to GP register (use Single since result is in b0/s0)
        self.push_lir(Aarch64Inst::FmovToGp {
            size: FpSize::Single,
            src: fp_scratch,
            dst: scratch,
        });

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    // ========================================================================
    // setjmp/longjmp/alloca support
    // ========================================================================

    /// Emit setjmp(env) - saves execution context
    /// AAPCS64: env in X0, returns int in W0
    pub(super) fn emit_setjmp(&mut self, insn: &Instruction, frame_size: i32) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Put env argument in X0 (first argument register)
        self.emit_move(env, Reg::X0, 64, frame_size);

        // Call setjmp
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("setjmp")),
        });

        // Store result from W0 to target
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::X0, &dst_loc, 32, frame_size);
    }

    /// Emit longjmp(env, val) - restores execution context (noreturn)
    /// AAPCS64: env in X0, val in X1
    pub(super) fn emit_longjmp(&mut self, insn: &Instruction, frame_size: i32) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let val = match insn.src.get(1) {
            Some(&v) => v,
            None => return,
        };

        // CONSTRAINT: Load val into X1 BEFORE loading env into X0.
        // If env is loaded into X0 first and val happens to be in X0 (first
        // function argument), it would be overwritten. This is a manual constraint
        // that will be expressible through the constraint system when inline asm
        // support is added.
        self.emit_move(val, Reg::X1, 32, frame_size);

        // Put env argument in X0 (first argument register)
        self.emit_move(env, Reg::X0, 64, frame_size);

        // Call longjmp (noreturn - control never comes back)
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("longjmp")),
        });

        // Emit brk after longjmp since it never returns
        // This helps catch any bugs where longjmp somehow returns
        self.push_lir(Aarch64Inst::Brk { imm: 1 });
    }

    /// Emit __builtin_alloca - dynamic stack allocation
    pub(super) fn emit_alloca(&mut self, insn: &Instruction, frame_size: i32) {
        let size = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load size into X9 (scratch register)
        self.emit_move(size, Reg::X9, 64, frame_size);

        // Round up to 16-byte alignment: (size + 15) & ~15
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: Reg::X9,
            src2: GpOperand::Imm(15),
            dst: Reg::X9,
        });
        self.push_lir(Aarch64Inst::And {
            size: OperandSize::B64,
            src1: Reg::X9,
            src2: GpOperand::Imm(-16),
            dst: Reg::X9,
        });

        // Subtract from stack pointer
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::SP,
            src2: GpOperand::Reg(Reg::X9),
            dst: Reg::SP,
        });

        // Return new stack pointer
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::SP),
            dst: Reg::X9,
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::X9, &dst_loc, 64, frame_size);
    }

    /// Emit __builtin_fabsf - absolute value of float
    pub(super) fn emit_fabs32(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into V0 (first FP argument register, single precision)
        self.emit_fp_move(arg, VReg::V0, None, 32, frame_size, types);

        // Call fabsf from libc
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("fabsf")),
        });

        // Result is in V0, store to target
        let dst_loc = self.get_location(target);
        self.emit_fp_move_to_loc(VReg::V0, &dst_loc, None, 32, frame_size, types);
    }

    /// Emit __builtin_fabs - absolute value of double
    pub(super) fn emit_fabs64(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into V0 (first FP argument register, double precision)
        self.emit_fp_move(arg, VReg::V0, None, 64, frame_size, types);

        // Call fabs from libc
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("fabs")),
        });

        // Result is in V0, store to target
        let dst_loc = self.get_location(target);
        self.emit_fp_move_to_loc(VReg::V0, &dst_loc, None, 64, frame_size, types);
    }
}
