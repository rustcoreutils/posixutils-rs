//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 memory access lowering: address computation, loads, stores,
// moves, struct copies and 128-bit integer transfers
//

use crate::arch::aarch64::codegen::Aarch64CodeGen;
use crate::arch::aarch64::lir::{Aarch64Inst, GpOperand, MemAddr};
use crate::arch::aarch64::regalloc::{Loc, LocalSlot, Reg, VReg};
use crate::arch::lir::{FpSize, OperandSize, Symbol};
use crate::ir::{Instruction, PseudoId, PseudoKind};
use crate::target::Os;
use crate::types::{TypeId, TypeTable};

use super::f64_to_f16_bits;

/// Result of computing a memory address for load/store operations
enum ComputedAddr {
    /// Direct memory address - no setup instructions needed
    Direct(MemAddr),
    /// Memory address after emitting setup instructions (e.g., loading spilled address)
    WithSetup(MemAddr),
    /// Global symbol - needs special handling (different for load vs store)
    Global(String),
}

impl Aarch64CodeGen {
    /// Load address of a global symbol into a register
    pub(super) fn emit_load_addr(&mut self, name: &str, dst: Reg) {
        // Thread-local storage: compute TLS address directly (Linux ELF only)
        if self.tls_symbols.contains(name) && self.base.target.os == Os::Linux {
            self.emit_tls_addr(name, dst);
            return;
        }

        // Local labels (starting with '.') don't get the _ prefix on macOS
        let sym = if name.starts_with('.') {
            Symbol::local(name)
        } else {
            Symbol::global(name)
        };

        if self.needs_got_access(name) {
            // External symbols on macOS: load address from GOT
            // ADRP + LDR from GOT
            let extern_sym = Symbol::extern_sym(name);
            self.push_lir(Aarch64Inst::AdrpGotPage {
                sym: extern_sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::LdrSymGotPageOff {
                sym: extern_sym,
                base: dst,
                dst,
            });
        } else {
            // ADRP + ADD sequence for PIC address loading
            self.push_lir(Aarch64Inst::Adrp {
                sym: sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::AddSymOffset {
                sym,
                base: dst,
                dst,
            });
        }
    }

    /// Load value of a global symbol into a register with specified size
    pub(super) fn emit_load_global(&mut self, name: &str, dst: Reg, size: OperandSize) {
        // Thread-local storage: compute TLS address, then load value (Linux ELF only)
        if self.tls_symbols.contains(name) && self.base.target.os == Os::Linux {
            self.emit_tls_addr(name, dst);
            // dst now holds the address of the TLS variable; load from it
            self.push_lir(Aarch64Inst::Ldr {
                size,
                addr: MemAddr::Base(dst),
                dst,
            });
            return;
        }

        // Local labels (starting with '.') don't get the _ prefix on macOS
        let sym = if name.starts_with('.') {
            Symbol::local(name)
        } else {
            Symbol::global(name)
        };

        if self.needs_got_access(name) {
            // External symbols on macOS: load address from GOT, then load value
            // ADRP + LDR from GOT gets address, then LDR value
            let extern_sym = Symbol::extern_sym(name);
            self.push_lir(Aarch64Inst::AdrpGotPage {
                sym: extern_sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::LdrSymGotPageOff {
                sym: extern_sym,
                base: dst,
                dst,
            });
            // Now dst contains the address, load the actual value
            self.push_lir(Aarch64Inst::Ldr {
                size,
                addr: MemAddr::Base(dst),
                dst,
            });
        } else {
            // ADRP + LDR sequence for PIC value loading
            self.push_lir(Aarch64Inst::Adrp {
                sym: sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::LdrSymOffset {
                size,
                sym,
                base: dst,
                dst,
            });
        }
    }

    /// Move immediate value to register
    pub(super) fn emit_mov_imm(&mut self, dst: Reg, val: i64, size: u32) {
        let op_size = OperandSize::from_bits(size.max(32));

        // AArch64 can only move 16-bit immediates directly
        // For larger values, we need movz + movk sequence
        if (0..=0xFFFF).contains(&val) {
            // LIR: simple mov immediate
            self.push_lir(Aarch64Inst::Mov {
                size: op_size,
                src: GpOperand::Imm(val),
                dst,
            });
        } else if (-0x8000..0).contains(&val) {
            // Small negative number - use mov (assembler handles movn)
            self.push_lir(Aarch64Inst::Mov {
                size: op_size,
                src: GpOperand::Imm(val),
                dst,
            });
        } else {
            // Use movz + movk for larger values
            let uval = val as u64;
            // LIR: movz base
            self.push_lir(Aarch64Inst::Movz {
                size: OperandSize::B64,
                imm: (uval & 0xFFFF) as u16,
                shift: 0,
                dst,
            });
            if (uval >> 16) & 0xFFFF != 0 {
                // LIR: movk shift 16
                self.push_lir(Aarch64Inst::Movk {
                    size: OperandSize::B64,
                    imm: ((uval >> 16) & 0xFFFF) as u16,
                    shift: 16,
                    dst,
                });
            }
            if (uval >> 32) & 0xFFFF != 0 {
                // LIR: movk shift 32
                self.push_lir(Aarch64Inst::Movk {
                    size: OperandSize::B64,
                    imm: ((uval >> 32) & 0xFFFF) as u16,
                    shift: 32,
                    dst,
                });
            }
            if (uval >> 48) & 0xFFFF != 0 {
                // LIR: movk shift 48
                self.push_lir(Aarch64Inst::Movk {
                    size: OperandSize::B64,
                    imm: ((uval >> 48) & 0xFFFF) as u16,
                    shift: 48,
                    dst,
                });
            }
        }
    }

    pub(super) fn emit_move(&mut self, src: PseudoId, dst: Reg, size: u32) {
        let actual_size = size; // Keep original size for sub-32-bit stack loads
        let size = size.max(32);
        let loc = self.get_location(src);
        let op_size = OperandSize::from_bits(size);

        match loc {
            Loc::Reg(r) if r == dst => {}
            Loc::Reg(r) => {
                // LIR: mov register to register
                self.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(r),
                    dst,
                });
            }
            ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // For sub-32-bit values, use sized load (ldrb/ldrh) which zero-extends.
                // This avoids reading garbage from adjacent stack bytes.
                let load_size = OperandSize::from_bits(actual_size.max(8));
                // LIR: load from stack (FP-relative for alloca safety)
                self.push_lir(Aarch64Inst::Ldr {
                    size: load_size,
                    addr: self.loc_mem(l).unwrap(),
                    dst,
                });
            }
            Loc::Imm(v) => {
                self.emit_mov_imm(dst, v as i64, size);
            }
            Loc::Global(name) => {
                let load_size = OperandSize::from_bits(size.max(32));
                self.emit_load_global(&name, dst, load_size);
            }
            Loc::VReg(v) => {
                // LIR: fmov from FP to GP register
                let fp_size = if size <= 32 {
                    FpSize::Single
                } else {
                    FpSize::Double
                };
                self.push_lir(Aarch64Inst::FmovToGp {
                    size: fp_size,
                    src: v,
                    dst,
                });
            }
            Loc::FImm(f, imm_size) => {
                // Use the size from the FImm, not the passed-in size
                // This ensures float constants are loaded correctly for their type
                let bits = if imm_size == 16 {
                    f64_to_f16_bits(f.to_f64()) as i64
                } else if imm_size == 32 {
                    (f.to_f64() as f32).to_bits() as i64
                } else {
                    f.to_f64().to_bits() as i64
                };
                self.emit_mov_imm(dst, bits, imm_size);
            }
        }
    }

    /// Move a 128-bit value from a source pseudo to a stack destination.
    /// Both halves (lo/hi) are loaded into scratch registers and stored via STP.
    fn emit_int128_move_to_stack(&mut self, src: PseudoId, dst_offset: LocalSlot) {
        let loc = self.get_location(src);
        match loc {
            ref loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // Stack-to-stack copy: load lo/hi via LDP, store via STP.
                // A 128-bit argument that arrived on the stack is copied the
                // same way; without its arm it fell to the `_` case below and
                // was truncated to 64 bits.
                let src_mem = self.loc_mem(loc).unwrap();
                let dst_mem = self.stack_mem(dst_offset);
                self.emit_ldp_legalized(OperandSize::B64, src_mem, Reg::X9, Reg::X10);
                self.emit_stp_legalized(OperandSize::B64, Reg::X9, Reg::X10, dst_mem);
            }
            Loc::Imm(v) => {
                let lo = v as u64 as i64;
                let hi = (v >> 64) as u64 as i64;
                self.emit_mov_imm(Reg::X9, lo, 64);
                self.emit_mov_imm(Reg::X10, hi, 64);
                let dst_mem = self.stack_mem(dst_offset);
                self.emit_stp_legalized(OperandSize::B64, Reg::X9, Reg::X10, dst_mem);
            }
            _ => {
                // For other locations, load as 64-bit and zero-extend
                self.emit_move(src, Reg::X9, 64);
                let dst_mem = self.stack_mem(dst_offset);
                self.emit_stp_legalized(OperandSize::B64, Reg::X9, Reg::Xzr, dst_mem);
            }
        }
    }

    pub(super) fn emit_move_to_loc(&mut self, src: Reg, dst: &Loc, size: u32) {
        // For stack stores, use actual size to properly handle char/short
        // For register-to-register, use minimum 32-bit
        match dst {
            Loc::Reg(r) if *r == src => {}
            Loc::Reg(r) => {
                let reg_size = size.max(32);
                // LIR: mov register to register
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::from_bits(reg_size),
                    src: GpOperand::Reg(src),
                    dst: *r,
                });
            }
            Loc::Stack(offset) => {
                // LIR: store to stack (FP-relative for alloca safety)
                let op_size = OperandSize::from_bits(size);
                self.push_lir(Aarch64Inst::Str {
                    size: op_size,
                    src,
                    addr: self.stack_mem(*offset),
                });
            }
            _ => {}
        }
    }

    /// Compute memory address from a pseudo, handling all location types.
    /// For Loc::Stack with spilled addresses and other non-register/non-symbol locations,
    /// emits setup instructions to load the address into temp_reg.
    fn compute_mem_addr(
        &mut self,
        addr: PseudoId,
        insn_offset: i64,
        temp_reg: Reg,
    ) -> ComputedAddr {
        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => ComputedAddr::Direct(MemAddr::BaseOffset {
                base: r,
                offset: insn_offset as i32,
            }),
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - access directly from stack slot (FP-relative for alloca safety)
                    ComputedAddr::Direct(self.stack_mem_plus(offset, insn_offset as i32))
                } else {
                    // Spilled address - load address first (FP-relative for alloca safety)
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: self.stack_mem(offset),
                        dst: temp_reg,
                    });
                    ComputedAddr::WithSetup(MemAddr::BaseOffset {
                        base: temp_reg,
                        offset: insn_offset as i32,
                    })
                }
            }
            Loc::Global(name) => ComputedAddr::Global(name.clone()),
            _ => {
                // Other location types - emit move to temp register
                self.emit_move(addr, temp_reg, 64);
                ComputedAddr::WithSetup(MemAddr::BaseOffset {
                    base: temp_reg,
                    offset: insn_offset as i32,
                })
            }
        }
    }

    pub(super) fn emit_load(&mut self, insn: &Instruction, types: &TypeTable) {
        let mem_size = insn.size;
        let reg_size = insn.size.max(32);
        let addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);

        // Check if this is an FP load
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t)) || matches!(dst_loc, Loc::VReg(_));

        if is_fp {
            self.emit_fp_load(insn, types);
            return;
        }

        // 128-bit integer load: load both halves to destination stack slot
        if mem_size == 128 {
            if let Loc::Stack(dst_offset) = dst_loc {
                match self.compute_mem_addr(addr, insn.offset, Reg::X16) {
                    ComputedAddr::Direct(mem_addr) | ComputedAddr::WithSetup(mem_addr) => {
                        self.emit_ldp_legalized(OperandSize::B64, mem_addr, Reg::X9, Reg::X10);
                        self.emit_stp_legalized(
                            OperandSize::B64,
                            Reg::X9,
                            Reg::X10,
                            self.stack_mem(dst_offset),
                        );
                    }
                    ComputedAddr::Global(name) => {
                        self.emit_load_addr(&name, Reg::X16);
                        self.push_lir(Aarch64Inst::Ldp {
                            size: OperandSize::B64,
                            addr: MemAddr::Base(Reg::X16),
                            dst1: Reg::X9,
                            dst2: Reg::X10,
                        });
                        self.emit_stp_legalized(
                            OperandSize::B64,
                            Reg::X9,
                            Reg::X10,
                            self.stack_mem(dst_offset),
                        );
                    }
                }
            }
            return;
        }

        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Sign- or zero-extend by the type's signedness. `is_unsigned`
        // answers for plain `char` per the target, so there is nothing to
        // special-case here.
        let is_unsigned = insn.typ.is_some_and(|t| types.is_unsigned(t));

        // Helper to emit the appropriate load instruction
        let emit_load_lir = |this: &mut Self, mem_addr: MemAddr| match mem_size {
            8 if is_unsigned => {
                this.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B8,
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            8 => {
                this.push_lir(Aarch64Inst::Ldrs {
                    src_size: OperandSize::B8,
                    dst_size: OperandSize::from_bits(reg_size),
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            16 if is_unsigned => {
                this.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B16,
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            16 => {
                this.push_lir(Aarch64Inst::Ldrs {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::from_bits(reg_size),
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            _ => {
                this.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::from_bits(mem_size),
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
        };

        match self.compute_mem_addr(addr, insn.offset, Reg::X16) {
            ComputedAddr::Direct(mem_addr) | ComputedAddr::WithSetup(mem_addr) => {
                emit_load_lir(self, mem_addr);
            }
            ComputedAddr::Global(name) => {
                let load_size = OperandSize::from_bits(mem_size);
                self.emit_load_global(&name, dst_reg, load_size);
            }
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, reg_size);
        }
    }

    pub(super) fn emit_store(&mut self, insn: &Instruction, types: &TypeTable) {
        // Use actual size for memory stores (8, 16, 32, 64 bits)
        let mem_size = insn.size;

        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };

        // Floating-point stores need the FP path, as they do on x86_64.
        // emit_struct_store below ends its operand match in `_ => return`, so
        // a 128-bit `long double` reaching it in a V register would be
        // silently dropped.
        let value_loc = self.get_location(value);
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t))
            || matches!(value_loc, Loc::VReg(_) | Loc::FImm(..));
        if is_fp {
            self.emit_fp_store(insn, types);
            return;
        }

        // For struct stores (size > 64), we need to copy multiple words
        // The value is a symbol containing the struct data
        if mem_size > 64 {
            // Check for Int128 immediate stores
            if mem_size == 128 {
                let value_loc = self.get_location(value);
                if let Loc::Imm(v) = value_loc {
                    self.emit_int128_imm_store(insn, addr, v);
                    return;
                }
            }
            self.emit_struct_store(insn, addr, value);
            return;
        }

        // Widen 32-bit stores at offset 0 to 64-bit to prevent stale
        // upper bits when a 32-bit result is stored into a 64-bit
        // local (e.g., int-to-long, int-to-pointer assignments).
        // Only widen for known local variables (in sym_type_sizes).
        // Do NOT widen stores to globals/statics (not in sym_type_sizes)
        // or stores through pointers — widening could clobber adjacent data.
        // Exception: struct/union fields at offset 0 must use exact
        // size to avoid clobbering the adjacent field at offset 4.
        let store_size = if mem_size == 32 && insn.offset == 0 {
            if let Some(&sym_bits) = self.sym_type_sizes.get(&addr) {
                // Known local variable — safe to widen if scalar and > 32 bits
                if sym_bits > 64 {
                    OperandSize::from_bits(mem_size) // struct field: exact size
                } else if sym_bits > 32 {
                    OperandSize::B64 // scalar/pointer local: safe to widen
                } else {
                    OperandSize::from_bits(mem_size)
                }
            } else {
                OperandSize::from_bits(mem_size) // global/static/pointer: exact size
            }
        } else {
            OperandSize::from_bits(mem_size)
        };

        // Use widened size for register load when store is widened
        let reg_size = if store_size == OperandSize::B64 {
            64
        } else {
            insn.size.max(32)
        };

        self.emit_move(value, Reg::X9, reg_size);

        // Helper to emit store instruction
        let emit_store_lir = |this: &mut Self, mem_addr: MemAddr| {
            this.push_lir(Aarch64Inst::Str {
                size: store_size,
                src: Reg::X9,
                addr: mem_addr,
            });
        };

        match self.compute_mem_addr(addr, insn.offset, Reg::X16) {
            ComputedAddr::Direct(mem_addr) | ComputedAddr::WithSetup(mem_addr) => {
                emit_store_lir(self, mem_addr);
            }
            ComputedAddr::Global(name) => {
                self.emit_load_addr(&name, Reg::X16);
                emit_store_lir(self, MemAddr::Base(Reg::X16));
            }
        }
    }

    /// Emit a struct copy (store of size > 64 bits)
    /// The value is a symbol containing the source struct data
    fn emit_struct_store(&mut self, insn: &Instruction, addr: PseudoId, value: PseudoId) {
        let struct_size = insn.size; // Size in bits
        let num_qwords = struct_size.div_ceil(64);

        // Get source address (where the struct data is)
        let value_loc = self.get_location(value);

        // Special case: if value is immediate 0, zero the struct instead of copying
        if let Loc::Imm(0) = value_loc {
            self.emit_struct_zero(insn, addr, num_qwords);
            return;
        }

        // Get destination address
        let addr_loc = self.get_location(addr);

        // Load source address into X16 (FP-relative for alloca safety)
        match value_loc {
            ref loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                let (base, total_offset) = self.loc_addr_parts(loc).unwrap();
                self.push_lir(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: base,
                    src2: GpOperand::Imm(total_offset as i64),
                    dst: Reg::X16,
                });
            }
            Loc::Reg(r) => {
                if r != Reg::X16 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: Reg::X16,
                    });
                }
            }
            Loc::Global(ref name) => {
                self.emit_load_addr(name, Reg::X16);
            }
            _ => return,
        }

        // Load destination address into X17 (FP-relative for alloca safety)
        match addr_loc {
            Loc::Stack(offset) => {
                // Distinguish symbol (local variable — use direct address) vs
                // temp/spilled pointer (load the pointer value from the slot).
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    let (base, base_off) = self.loc_addr_parts(&addr_loc).unwrap();
                    let total_offset = base_off + insn.offset as i32;
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: base,
                        src2: GpOperand::Imm(total_offset as i64),
                        dst: Reg::X17,
                    });
                } else {
                    // Spilled pointer — load the pointer value, then add offset
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: self.stack_mem(offset),
                        dst: Reg::X17,
                    });
                    if insn.offset != 0 {
                        self.push_lir(Aarch64Inst::Add {
                            size: OperandSize::B64,
                            src1: Reg::X17,
                            src2: GpOperand::Imm(insn.offset),
                            dst: Reg::X17,
                        });
                    }
                }
            }
            Loc::Reg(r) => {
                if insn.offset != 0 {
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: r,
                        src2: GpOperand::Imm(insn.offset),
                        dst: Reg::X17,
                    });
                } else if r != Reg::X17 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: Reg::X17,
                    });
                }
            }
            Loc::Global(ref name) => {
                self.emit_load_addr(name, Reg::X17);
            }
            _ => return,
        }

        // Copy qword by qword using X9 as temp
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            self.push_lir(Aarch64Inst::Ldr {
                size: OperandSize::B64,
                addr: MemAddr::BaseOffset {
                    base: Reg::X16,
                    offset: byte_offset,
                },
                dst: Reg::X9,
            });
            self.push_lir(Aarch64Inst::Str {
                size: OperandSize::B64,
                src: Reg::X9,
                addr: MemAddr::BaseOffset {
                    base: Reg::X17,
                    offset: byte_offset,
                },
            });
        }
    }

    /// Emit code to zero a struct (for struct = {0} initialization)
    fn emit_struct_zero(&mut self, insn: &Instruction, addr: PseudoId, num_qwords: u32) {
        let addr_loc = self.get_location(addr);

        // Load destination address into X17
        match addr_loc {
            ref loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                let (base, base_off) = self.loc_addr_parts(loc).unwrap();
                let total_offset = base_off + insn.offset as i32;
                self.push_lir(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: base,
                    src2: GpOperand::Imm(total_offset as i64),
                    dst: Reg::X17,
                });
            }
            Loc::Reg(r) => {
                if insn.offset != 0 {
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: r,
                        src2: GpOperand::Imm(insn.offset),
                        dst: Reg::X17,
                    });
                } else if r != Reg::X17 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: Reg::X17,
                    });
                }
            }
            Loc::Global(ref name) => {
                self.emit_load_addr(name, Reg::X17);
                if insn.offset != 0 {
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: Reg::X17,
                        src2: GpOperand::Imm(insn.offset),
                        dst: Reg::X17,
                    });
                }
            }
            _ => return,
        }

        // Store zeros using XZR (zero register) - aarch64 has hardware zero reg!
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            self.push_lir(Aarch64Inst::Str {
                size: OperandSize::B64,
                src: Reg::Xzr,
                addr: MemAddr::BaseOffset {
                    base: Reg::X17,
                    offset: byte_offset,
                },
            });
        }
    }

    /// Store a 128-bit immediate value to a memory destination
    fn emit_int128_imm_store(&mut self, insn: &Instruction, addr: PseudoId, v: i128) {
        let lo = v as u64 as i64;
        let hi = (v >> 64) as u64 as i64;
        self.emit_mov_imm(Reg::X9, lo, 64);
        self.emit_mov_imm(Reg::X10, hi, 64);

        match self.compute_mem_addr(addr, insn.offset, Reg::X16) {
            ComputedAddr::Direct(mem_addr) | ComputedAddr::WithSetup(mem_addr) => {
                self.emit_stp_legalized(OperandSize::B64, Reg::X9, Reg::X10, mem_addr);
            }
            ComputedAddr::Global(name) => {
                self.emit_load_addr(&name, Reg::X16);
                self.push_lir(Aarch64Inst::Stp {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: Reg::X10,
                    addr: MemAddr::Base(Reg::X16),
                });
            }
        }
    }

    pub(super) fn emit_copy_with_type(
        &mut self,
        src: PseudoId,
        dst: PseudoId,
        size: u32,
        typ: Option<TypeId>,
        types: &TypeTable,
    ) {
        // Keep actual size for handling narrow types
        let actual_size = size;
        let reg_size = size.max(32);
        let dst_loc = self.get_location(dst);
        let src_loc = self.get_location(src);

        // Check if this is a FP copy (source or dest is in VReg or is FImm)
        let is_fp_copy =
            matches!(&src_loc, Loc::VReg(_) | Loc::FImm(..)) || matches!(&dst_loc, Loc::VReg(_));

        // M9a — identity-Copy elision (aarch64 mirror of x86_64
        // path). Same gating: same location, no narrow-type
        // extension, not FP. See x86_64::emit_copy_with_type for
        // the full rationale.
        if src_loc == dst_loc && actual_size >= 32 && !is_fp_copy {
            return;
        }

        // Sign- or zero-extend by the type's signedness. `is_unsigned`
        // answers for plain `char` per the target, so there is nothing to
        // special-case here.
        let is_unsigned = typ.is_some_and(|t| types.is_unsigned(t));

        // Handle 128-bit integer copy.
        //
        // Only integers: a 128-bit `long double` is binary128, and this path
        // moves the low 64 bits through a general-purpose register. That
        // truncated an `FImm` to its *f64* encoding (and a `VReg` to its low
        // lane), so `long double a = 3.14159...L;` landed a denormal on the
        // stack. FP goes through `emit_fp_move`, which assembles both halves.
        if actual_size == 128 && !is_fp_copy {
            if let Loc::Stack(dst_offset) = dst_loc {
                self.emit_int128_move_to_stack(src, dst_offset);
            }
            return;
        }

        if is_fp_copy {
            // Handle FP copy
            let dst_vreg = match &dst_loc {
                Loc::VReg(v) => *v,
                _ => VReg::V16, // Use scratch register
            };

            self.emit_fp_move(src, dst_vreg, typ, reg_size, types);

            if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
                self.emit_fp_move_to_loc(dst_vreg, &dst_loc, typ, reg_size, types);
            }
        } else {
            // Integer copy
            match &dst_loc {
                Loc::Reg(r) => {
                    self.emit_move(src, *r, reg_size);
                    // For narrow types (8 or 16 bits), extend to correct width
                    // AARCH64: UXTB/UXTH for unsigned, SXTB/SXTH for signed
                    if actual_size == 8 {
                        if is_unsigned {
                            self.push_lir(Aarch64Inst::Uxtb { src: *r, dst: *r });
                        } else {
                            self.push_lir(Aarch64Inst::Sxtb {
                                dst_size: OperandSize::B32,
                                src: *r,
                                dst: *r,
                            });
                        }
                    } else if actual_size == 16 {
                        if is_unsigned {
                            self.push_lir(Aarch64Inst::Uxth { src: *r, dst: *r });
                        } else {
                            self.push_lir(Aarch64Inst::Sxth {
                                dst_size: OperandSize::B32,
                                src: *r,
                                dst: *r,
                            });
                        }
                    }
                }
                Loc::Stack(_) => {
                    self.emit_move(src, Reg::X9, reg_size);
                    // For narrow types stored to stack, use the actual size
                    if actual_size <= 16 {
                        self.emit_move_to_loc(Reg::X9, &dst_loc, actual_size);
                    } else {
                        self.emit_move_to_loc(Reg::X9, &dst_loc, reg_size);
                    }
                }
                _ => {}
            }
        }
    }
}
