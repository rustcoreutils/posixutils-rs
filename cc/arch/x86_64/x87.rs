//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 x87 FPU Code Generation (Long Double / 80-bit Extended Precision)
//
// The x87 FPU uses a stack-based architecture:
// - ST(0) is the top of the stack
// - ST(1) through ST(7) are below
// - Operations implicitly use ST(0) and may push/pop the stack
//
// Since System V AMD64 ABI classifies long double as Indirect (memory-based),
// we use simple load/op/store sequences without x87 register allocation.
// All long double values reside in memory, so each operation:
// 1. Loads operand(s) from memory to x87 stack
// 2. Performs the operation
// 3. Stores result to memory and pops the stack
//

use super::codegen::X86_64CodeGen;
use super::lir::{GpOperand, MemAddr, X86Inst, X87BinOp};
use super::regalloc::{Loc, Reg};
use crate::arch::lir::{CondCode, OperandSize};
use crate::ir::{Instruction, Opcode, PseudoId};
use crate::types::{TypeKind, TypeTable};

impl X86_64CodeGen {
    /// Check if this instruction operates on long double (80-bit x87)
    pub fn is_longdouble_op(&self, insn: &Instruction, types: &TypeTable) -> bool {
        insn.size >= 80
            && insn
                .typ
                .is_some_and(|t| types.kind(t) == TypeKind::LongDouble)
    }

    /// Emit x87 load operation (Load instruction for long double)
    ///
    /// Pattern:
    ///   mov    addr, %r11      ; get address
    ///   fldt   (%r11)          ; load to ST(0)
    ///   fstpt  dst(%rbp)       ; store to destination
    pub(super) fn emit_x87_load(&mut self, insn: &Instruction) {
        let addr = match insn.src.first() {
            Some(&a) => a,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Get the address to load from
        // For Load instruction, addr is a pseudo representing a memory location
        // (either a stack local, or a pointer in a register/memory)
        let addr_loc = self.get_location(addr);
        let src_addr = match addr_loc {
            Loc::Reg(r) => {
                // addr is a pointer in a register
                MemAddr::BaseOffset {
                    base: r,
                    offset: insn.offset as i32,
                }
            }
            Loc::Stack(offset) => {
                // addr is a local variable on stack - load directly from it
                let adjusted = offset + self.callee_saved_offset;
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -(adjusted) + insn.offset as i32,
                }
            }
            Loc::Global(name) => {
                // For globals, use RIP-relative addressing
                if self.needs_got_access(&name) {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(
                            crate::arch::lir::Symbol::extern_sym(name.clone()),
                        )),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }
                } else {
                    MemAddr::RipRelative(crate::arch::lir::Symbol {
                        name,
                        is_local: false,
                        is_extern: false,
                    })
                }
            }
            _ => {
                // Load address into R11
                self.emit_move(addr, Reg::R11, 64);
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: insn.offset as i32,
                }
            }
        };

        // Load from source to ST(0)
        self.push_lir(X86Inst::X87Load { addr: src_addr });

        // Store to destination
        let dst_addr = self.get_x87_mem_addr(target);
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit x87 store operation (Store instruction for long double)
    ///
    /// Pattern:
    ///   fldt   src(%rbp)       ; load value to ST(0)
    ///   fstpt  dst(%rbp)       ; store to destination
    pub(super) fn emit_x87_store(&mut self, insn: &Instruction) {
        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };

        // Load value to ST(0)
        let src_addr = self.get_x87_mem_addr(value);
        self.push_lir(X86Inst::X87Load { addr: src_addr });

        // Get destination address
        // For Store instruction, addr is a pseudo representing a memory location
        // (either a stack local, or a pointer in a register/memory)
        let addr_loc = self.get_location(addr);
        let dst_addr = match addr_loc {
            Loc::Reg(r) => {
                // addr is a pointer in a register
                MemAddr::BaseOffset {
                    base: r,
                    offset: insn.offset as i32,
                }
            }
            Loc::Stack(offset) => {
                // addr is a local variable on stack - store directly to it
                let adjusted = offset + self.callee_saved_offset;
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -(adjusted) + insn.offset as i32,
                }
            }
            Loc::Global(name) => {
                if self.needs_got_access(&name) {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(
                            crate::arch::lir::Symbol::extern_sym(name.clone()),
                        )),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }
                } else {
                    MemAddr::RipRelative(crate::arch::lir::Symbol {
                        name,
                        is_local: false,
                        is_extern: false,
                    })
                }
            }
            _ => {
                self.emit_move(addr, Reg::R11, 64);
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: insn.offset as i32,
                }
            }
        };

        // Store from ST(0)
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit x87 binary operation (add, sub, mul, div)
    ///
    /// Pattern for a + b:
    ///   fldt   a(%rbp)       ; load a to ST(0)
    ///   fldt   b(%rbp)       ; load b to ST(0), a moves to ST(1)
    ///   faddp  %st, %st(1)   ; ST(0) = ST(1) + ST(0), pop
    ///   fstpt  result(%rbp)  ; store result, pop
    pub(super) fn emit_x87_binop(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load first operand to ST(0)
        let src1_addr = self.get_x87_mem_addr(src1);
        self.push_lir(X86Inst::X87Load { addr: src1_addr });

        // Load second operand to ST(0), first moves to ST(1)
        let src2_addr = self.get_x87_mem_addr(src2);
        self.push_lir(X86Inst::X87Load { addr: src2_addr });

        // Perform operation: ST(0) = ST(1) op ST(0), pop
        let op = match insn.op {
            Opcode::FAdd => X87BinOp::Add,
            Opcode::FSub => X87BinOp::Sub,
            Opcode::FMul => X87BinOp::Mul,
            Opcode::FDiv => X87BinOp::Div,
            _ => return,
        };
        self.push_lir(X86Inst::X87BinOp { op });

        // Store result to destination and pop
        let dst_addr = self.get_x87_mem_addr(target);
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit x87 negation
    ///
    /// Pattern:
    ///   fldt   a(%rbp)       ; load to ST(0)
    ///   fchs                 ; negate ST(0)
    ///   fstpt  result(%rbp)  ; store and pop
    pub(super) fn emit_x87_neg(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_addr = self.get_x87_mem_addr(src);
        self.push_lir(X86Inst::X87Load { addr: src_addr });
        self.push_lir(X86Inst::X87Neg);

        let dst_addr = self.get_x87_mem_addr(target);
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit x87 comparison
    ///
    /// Pattern:
    ///   fldt   b(%rbp)       ; load b to ST(0)
    ///   fldt   a(%rbp)       ; load a to ST(0), b moves to ST(1)
    ///   fcomip %st(1), %st   ; compare ST(0) with ST(1), set EFLAGS, pop
    ///   fstp   %st(0)        ; discard remaining value
    ///   setcc  %al           ; set result based on condition
    pub(super) fn emit_x87_compare(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load in reverse order so comparison is a op b
        // Load b first (goes to ST(0), then ST(1))
        let src2_addr = self.get_x87_mem_addr(src2);
        self.push_lir(X86Inst::X87Load { addr: src2_addr });

        // Load a (goes to ST(0), b is now in ST(1))
        let src1_addr = self.get_x87_mem_addr(src1);
        self.push_lir(X86Inst::X87Load { addr: src1_addr });

        // Compare ST(0) with ST(1), set EFLAGS, pop ST(0)
        self.push_lir(X86Inst::X87CmpPop);

        // Discard remaining ST(0)
        self.push_lir(X86Inst::X87Pop);

        // Set result based on condition code
        // FCOMIP sets: CF=1 if a<b, ZF=1 if a==b, PF=1 if unordered
        // x87 comparison uses same flags as unsigned integer comparison
        let cc = match insn.op {
            Opcode::FCmpOEq => CondCode::Eq,
            Opcode::FCmpONe => CondCode::Ne,
            Opcode::FCmpOLt => CondCode::Ult, // CF=1
            Opcode::FCmpOLe => CondCode::Ule, // CF=1 or ZF=1
            Opcode::FCmpOGt => CondCode::Ugt, // CF=0 and ZF=0
            Opcode::FCmpOGe => CondCode::Uge, // CF=0
            _ => return,
        };

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10,
        };

        // Set byte based on condition
        self.push_lir(X86Inst::SetCC { cc, dst: dst_reg });

        // Zero-extend to 32-bit
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(dst_reg),
            dst: dst_reg,
        });

        // Move to final destination if needed
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 32);
        }
    }

    /// Get memory address for x87 operand.
    /// Long doubles are always in memory per System V AMD64 ABI.
    pub(super) fn get_x87_mem_addr(&mut self, pseudo: PseudoId) -> MemAddr {
        let loc = self.get_location(pseudo);
        match loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                }
            }
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset,
            },
            Loc::Global(name) => {
                // For global long doubles, use RIP-relative addressing
                MemAddr::RipRelative(crate::arch::lir::Symbol {
                    name,
                    is_local: false,
                    is_extern: false,
                })
            }
            Loc::Reg(r) => {
                // Pointer to long double in a register
                MemAddr::BaseOffset { base: r, offset: 0 }
            }
            Loc::FImm(v, _) => {
                // Long double immediate - convert f64 to 80-bit extended precision
                // and register for later emission in the data section
                let label_bits = v.to_bits();
                let temp_label = format!(".Lld_const_{}", label_bits);

                // Convert f64 to 80-bit extended precision (stored in 16 bytes)
                let ld_bytes = f64_to_x87_extended(v);
                self.ld_constants.insert(label_bits, ld_bytes);

                MemAddr::RipRelative(crate::arch::lir::Symbol::local(temp_label))
            }
            _ => {
                // Fallback - should not happen for proper long double handling
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: 0,
                }
            }
        }
    }

    /// Emit float-to-float conversion involving long double.
    /// Handles conversions between float/double and long double using x87 FPU.
    pub(super) fn emit_x87_fp_cvt(&mut self, insn: &Instruction, types: &TypeTable) {
        // Type info must be present for FP conversions
        let src_kind = insn
            .src_typ
            .map(|t| types.kind(t))
            .expect("FP conversion must have src_typ");
        let dst_kind = insn
            .typ
            .map(|t| types.kind(t))
            .expect("FP conversion must have dst typ");
        let src_is_longdouble = src_kind == TypeKind::LongDouble;
        let dst_is_longdouble = dst_kind == TypeKind::LongDouble;
        let src_is_float = src_kind == TypeKind::Float;
        let dst_is_float = dst_kind == TypeKind::Float;
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Get source memory address
        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(target);

        // Helper to get memory address for non-long-double operand
        let get_mem_addr = |loc: &Loc, this: &Self| -> MemAddr {
            match loc {
                Loc::Stack(offset) => {
                    let adjusted = offset + this.callee_saved_offset;
                    MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }
                }
                Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: *offset,
                },
                Loc::Global(name) => MemAddr::RipRelative(crate::arch::lir::Symbol {
                    name: name.clone(),
                    is_local: false,
                    is_extern: false,
                }),
                Loc::Reg(r) => MemAddr::BaseOffset {
                    base: *r,
                    offset: 0,
                },
                _ => MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: 0,
                },
            }
        };

        if src_is_longdouble && !dst_is_longdouble {
            // Long double -> float/double
            // Load long double to ST(0), store as float or double
            let src_addr = self.get_x87_mem_addr(src);
            self.push_lir(X86Inst::X87Load { addr: src_addr });

            // Check if destination is XMM register - need scratch memory
            let needs_xmm_load = matches!(&dst_loc, Loc::Xmm(_));
            let dst_addr = if needs_xmm_load {
                // Use scratch location for x87 -> XMM transfer
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -8,
                }
            } else {
                get_mem_addr(&dst_loc, self)
            };

            if dst_is_float {
                // Store as float (32-bit)
                self.push_lir(X86Inst::X87StoreFloat {
                    addr: dst_addr.clone(),
                });
            } else {
                // Store as double (64-bit)
                self.push_lir(X86Inst::X87StoreDouble {
                    addr: dst_addr.clone(),
                });
            }

            // If destination is XMM, load from scratch location
            if let Loc::Xmm(xmm_reg) = &dst_loc {
                use super::lir::XmmOperand;
                use crate::arch::lir::FpSize;
                let fp_size = if dst_is_float {
                    FpSize::Single
                } else {
                    FpSize::Double
                };
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(dst_addr),
                    dst: XmmOperand::Reg(*xmm_reg),
                });
            }
        } else if !src_is_longdouble && dst_is_longdouble {
            // Float/double -> long double
            // Handle different source locations
            let src_addr = match &src_loc {
                Loc::Xmm(xmm_reg) => {
                    // XMM register - store to scratch memory first
                    use super::lir::XmmOperand;
                    use crate::arch::lir::FpSize;
                    let scratch = MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -8,
                    };
                    let fp_size = if src_is_float {
                        FpSize::Single
                    } else {
                        FpSize::Double
                    };
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(*xmm_reg),
                        dst: XmmOperand::Mem(scratch.clone()),
                    });
                    scratch
                }
                Loc::FImm(val, _) => {
                    // Float/double immediate - create constant in rodata
                    let bits = val.to_bits();
                    let label = format!(".Ldbl_const_{}", bits);
                    self.double_constants.insert(bits, *val);
                    MemAddr::RipRelative(crate::arch::lir::Symbol::local(label))
                }
                _ => get_mem_addr(&src_loc, self),
            };

            // Load as float/double to x87
            if src_is_float {
                // Load float (32-bit)
                self.push_lir(X86Inst::X87LoadFloat { addr: src_addr });
            } else {
                // Load double (64-bit)
                self.push_lir(X86Inst::X87LoadDouble { addr: src_addr });
            }

            let dst_addr = self.get_x87_mem_addr(target);
            self.push_lir(X86Inst::X87Store { addr: dst_addr });
        } else {
            // Long double -> long double (just copy)
            let src_addr = self.get_x87_mem_addr(src);
            self.push_lir(X86Inst::X87Load { addr: src_addr });
            let dst_addr = self.get_x87_mem_addr(target);
            self.push_lir(X86Inst::X87Store { addr: dst_addr });
        }
    }

    /// Emit integer to long double conversion.
    ///
    /// Pattern:
    ///   movl   %src, temp(%rbp)    ; store integer to memory (if in register)
    ///   fildl  temp(%rbp)          ; load as int, convert to x87 extended
    ///   fstpt  dst(%rbp)           ; store as long double
    pub(super) fn emit_x87_int_to_float(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = insn.src_size.max(32);
        let src_loc = self.get_location(src);

        // We need the integer in memory for fild. If it's in a register, store it first.
        let src_addr = match &src_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                }
            }
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: *offset,
            },
            Loc::Reg(r) => {
                // Integer is in a register - store to a temp location first
                // Use a scratch stack location (reuse destination if it's on stack)
                let dst_loc = self.get_location(target);
                let temp_addr = if let Loc::Stack(offset) = &dst_loc {
                    // Use the bottom part of the destination (which is 16 bytes)
                    let adjusted = offset + self.callee_saved_offset;
                    MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }
                } else {
                    // Use a fixed scratch location
                    MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -8,
                    }
                };

                let op_size = if src_size <= 32 {
                    OperandSize::B32
                } else {
                    OperandSize::B64
                };
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(*r),
                    dst: GpOperand::Mem(temp_addr.clone()),
                });
                temp_addr
            }
            Loc::Imm(val) => {
                // Immediate - store to temp location
                let temp_addr = MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -8,
                };
                let op_size = if src_size <= 32 {
                    OperandSize::B32
                } else {
                    OperandSize::B64
                };
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(*val),
                    dst: GpOperand::Mem(temp_addr.clone()),
                });
                temp_addr
            }
            Loc::Global(name) => MemAddr::RipRelative(crate::arch::lir::Symbol {
                name: name.clone(),
                is_local: false,
                is_extern: false,
            }),
            _ => {
                // Fallback - should not happen
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: 0,
                }
            }
        };

        // Load integer with fildl/fildq - converts to x87 extended precision
        if src_size <= 32 {
            self.push_lir(X86Inst::X87LoadInt32 { addr: src_addr });
        } else {
            self.push_lir(X86Inst::X87LoadInt64 { addr: src_addr });
        }

        // Store as long double
        let dst_addr = self.get_x87_mem_addr(target);
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit long double to integer conversion.
    ///
    /// Pattern:
    ///   fldt   src(%rbp)          ; load long double to ST(0)
    ///   fisttpl dst(%rbp)         ; convert with truncation and store
    pub(super) fn emit_x87_float_to_int(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_size = insn.size.max(32);
        let dst_loc = self.get_location(target);

        // Load long double to x87 stack
        let src_addr = self.get_x87_mem_addr(src);
        self.push_lir(X86Inst::X87Load { addr: src_addr });

        // Determine where to store the result
        let needs_load_to_reg = matches!(&dst_loc, Loc::Reg(_) | Loc::Xmm(_));

        let store_addr = match &dst_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                }
            }
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: *offset,
            },
            _ => {
                // Store to scratch location, will load to register afterwards
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -8,
                }
            }
        };

        // Convert and store with fisttp (truncation toward zero)
        if dst_size <= 32 {
            self.push_lir(X86Inst::X87StoreInt32 {
                addr: store_addr.clone(),
            });
        } else {
            self.push_lir(X86Inst::X87StoreInt64 {
                addr: store_addr.clone(),
            });
        }

        // If destination is a register, load the result
        if needs_load_to_reg {
            if let Loc::Reg(dst_reg) = &dst_loc {
                let op_size = if dst_size <= 32 {
                    OperandSize::B32
                } else {
                    OperandSize::B64
                };
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(store_addr),
                    dst: GpOperand::Reg(*dst_reg),
                });
            }
        }
    }
}

/// Convert a 64-bit double to x87 80-bit extended precision format.
/// Returns 16 bytes (80 bits padded to 128 bits for alignment).
///
/// x87 80-bit extended format:
/// - Bit 79: Sign
/// - Bits 78-64: 15-bit exponent (bias 16383)
/// - Bit 63: Integer bit (explicit, always 1 for normalized numbers)
/// - Bits 62-0: 63-bit fraction
pub fn f64_to_x87_extended(value: f64) -> [u8; 16] {
    let mut result = [0u8; 16];

    // Handle special cases
    if value == 0.0 {
        // Check for negative zero
        if value.to_bits() >> 63 == 1 {
            result[9] = 0x80; // Set sign bit
        }
        return result;
    }

    if value.is_nan() {
        // NaN: all exponent bits set, non-zero fraction
        result[7] = 0xC0; // Set quiet NaN bit in fraction
        result[8] = 0xFF;
        result[9] = 0x7F;
        return result;
    }

    if value.is_infinite() {
        // Infinity: all exponent bits set, zero fraction, integer bit set
        result[7] = 0x80; // Integer bit
        result[8] = 0xFF;
        result[9] = if value > 0.0 { 0x7F } else { 0xFF }; // Sign in bit 79
        return result;
    }

    // Normal number conversion
    let bits = value.to_bits();
    let sign = (bits >> 63) as u8;
    let exp64 = ((bits >> 52) & 0x7FF) as i32;
    let frac64 = bits & 0x000F_FFFF_FFFF_FFFF;

    // Convert exponent: rebias from 1023 (double) to 16383 (extended)
    // For denormals, exp64 == 0, actual exponent is -1022
    let exp80 = if exp64 == 0 {
        // Denormalized double -> need to normalize for x87
        // This is complex; for simplicity, use 0 exponent (denormal x87)
        0u16
    } else {
        // Normal: rebias
        ((exp64 - 1023) + 16383) as u16
    };

    // Convert fraction: double has 52 bits, x87 extended has 63 bits
    // Shift left by 11 (63 - 52 = 11)
    let frac80 = frac64 << 11;

    // Set the integer bit (bit 63) for normalized numbers
    let int_bit = if exp64 != 0 { 1u64 << 63 } else { 0 };
    let mantissa = int_bit | frac80;

    // Pack into bytes (little-endian)
    // Bytes 0-7: 64-bit mantissa (including integer bit)
    result[0..8].copy_from_slice(&mantissa.to_le_bytes());

    // Bytes 8-9: 15-bit exponent + sign bit
    let exp_sign = exp80 | ((sign as u16) << 15);
    result[8..10].copy_from_slice(&exp_sign.to_le_bytes());

    // Bytes 10-15: padding (zeros)

    result
}
