//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 memory access lowering: loads, stores, moves, struct copies and
// 128-bit integer transfers
//

use crate::arch::lir::{FpSize, OperandSize, Symbol};
use crate::arch::x86_64::codegen::X86_64CodeGen;
use crate::arch::x86_64::lir::{GpOperand, MemAddr, ShiftCount, X86Inst, XmmOperand};
use crate::arch::x86_64::regalloc::{Loc, Reg, XmmReg};
use crate::ir::{Instruction, PseudoId, PseudoKind};
use crate::target::Os;
use crate::types::{TypeId, TypeKind, TypeTable};

impl X86_64CodeGen {
    /// The source width of a zero-extending load, when the hardware has one.
    ///
    /// `movz` extends from a byte or a word and from nothing else, so a width
    /// that is neither -- an aggregate of three bytes travelling in a
    /// register, say -- has no extending form and must be moved at the next
    /// size up instead. Asking `OperandSize::from_bits` and using the answer
    /// regardless produced a `Movzx` from 32 bits to 32 bits, which the
    /// assembly printer then had to guess at.
    fn extending_load_size(actual_size: u32) -> Option<OperandSize> {
        match actual_size {
            1..=8 => Some(OperandSize::B8),
            9..=16 => Some(OperandSize::B16),
            _ => None,
        }
    }

    pub(super) fn emit_move(&mut self, src: PseudoId, dst: Reg, size: u32) {
        let actual_size = size; // Keep original size for sub-32-bit handling
        let narrow = Self::extending_load_size(actual_size).filter(|_| actual_size < 32);
        let size = size.max(32);
        let op_size = OperandSize::from_bits(size);
        let loc = self.get_location(src);
        match loc {
            Loc::Reg(r) if r == dst => {
                // No-op: same register
            }
            Loc::Reg(r) => {
                // LIR: register-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(r),
                    dst: GpOperand::Reg(dst),
                });
            }
            Loc::Stack(offset) => {
                // For sub-32-bit values, use zero-extending load to avoid garbage in upper bits
                if let Some(src_size) = narrow {
                    // LIR: zero-extending memory-to-register move
                    self.push_lir(X86Inst::Movzx {
                        src_size,
                        dst_size: OperandSize::B32,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst,
                    });
                } else {
                    // LIR: memory-to-register move
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst: GpOperand::Reg(dst),
                    });
                }
            }
            Loc::IncomingArg(offset) => {
                // For sub-32-bit values, use zero-extending load
                if let Some(src_size) = narrow {
                    // LIR: zero-extending memory-to-register move from incoming stack arg
                    self.push_lir(X86Inst::Movzx {
                        src_size,
                        dst_size: OperandSize::B32,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset,
                        }),
                        dst,
                    });
                } else {
                    // LIR: memory-to-register move from incoming stack arg
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset,
                        }),
                        dst: GpOperand::Reg(dst),
                    });
                }
            }
            Loc::Imm(v) => {
                let v = v as i64;
                // x86-64: movl sign-extends to 64-bit, movq only works with 32-bit signed immediates
                // For values outside 32-bit signed range, use movabsq
                if size == 64 && (v > i32::MAX as i64 || v < i32::MIN as i64) {
                    // LIR: 64-bit immediate move
                    self.push_lir(X86Inst::MovAbs { imm: v, dst });
                } else {
                    // LIR: immediate-to-register move
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Imm(v),
                        dst: GpOperand::Reg(dst),
                    });
                }
            }
            Loc::Global(name) => {
                // LIR: memory-to-register move
                // Use local symbol for labels starting with '.' (e.g., .LC0 for string constants)
                let symbol = if name.starts_with('.') {
                    Symbol::local(name.clone())
                } else {
                    Symbol::global(name.clone())
                };

                if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then load value
                    // Use R11 as temp if dst is R11, otherwise use dst
                    let temp_reg = if dst == Reg::R11 { Reg::R10 } else { Reg::R11 };
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(temp_reg),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: temp_reg,
                            offset: 0,
                        }),
                        dst: GpOperand::Reg(dst),
                    });
                } else if self.tls_symbols.contains(&name) && self.base.target.os == Os::Linux {
                    // Thread-local storage: use FS segment
                    // Use Initial Exec model for external TLS or when building shared libraries.
                    // PIE executables can use Local Exec for their own TLS variables.
                    let use_ie_model = self.use_tls_ie(&name);

                    if use_ie_model {
                        // Initial Exec: load offset from GOT, then load via FS segment
                        let temp_reg = if dst == Reg::R11 { Reg::R10 } else { Reg::R11 };
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(MemAddr::TlsGottpoff(symbol)),
                            dst: GpOperand::Reg(temp_reg),
                        });
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(MemAddr::FsBase(temp_reg)),
                            dst: GpOperand::Reg(dst),
                        });
                    } else {
                        // Local Exec: direct access via %fs:symbol@TPOFF
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(MemAddr::TlsLocalExec(symbol)),
                            dst: GpOperand::Reg(dst),
                        });
                    }
                } else {
                    // Regular RIP-relative access
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::RipRelative(symbol)),
                        dst: GpOperand::Reg(dst),
                    });
                }
            }
            Loc::Xmm(x) => {
                // Move from XMM to general-purpose register.
                // Use movd for ≤32-bit (float/float16) or movq for 64-bit (double).
                self.push_lir(X86Inst::MovXmmGp {
                    size: op_size,
                    src: x,
                    dst,
                });
                // For sub-32-bit values (e.g., Float16), zero-extend to clear garbage
                if actual_size < 32 {
                    self.push_lir(X86Inst::Movzx {
                        src_size: OperandSize::from_bits(actual_size),
                        dst_size: OperandSize::B32,
                        src: GpOperand::Reg(dst),
                        dst,
                    });
                }
            }
            Loc::FImm(v, fp_size) => {
                // A floating constant in a general register is its bit
                // pattern -- used for the Float16 rtlib calls, and for an
                // inline-asm operand under a general-register constraint.
                let bits = v.to_bits_at_width(fp_size);
                if fp_size <= 32 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(bits),
                        dst: GpOperand::Reg(dst),
                    });
                } else {
                    // A double's pattern does not fit a 32-bit immediate.
                    self.push_lir(X86Inst::MovAbs { imm: bits, dst });
                }
            }
        }
    }

    /// Store a GP register to a regalloc stack slot. All regalloc slots are
    /// 8 bytes on x86-64. For integer values >= 32 bits, we always write the
    /// full 64-bit slot to prevent stale upper bytes from being read by
    /// subsequent wider loads. On x86-64, any 32-bit register operation
    /// zero-extends to 64 bits, so `movq %reg, mem` is correct.
    /// For 8/16-bit values, store the actual size to avoid clobbering adjacent
    /// struct fields when the "slot" is really a struct member.
    fn store_to_stack_slot(&mut self, src: Reg, stack_offset: i32) {
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(src),
            dst: GpOperand::Mem(self.stack_mem(stack_offset)),
        });
    }

    pub(super) fn emit_move_to_loc(&mut self, src: Reg, dst: &Loc, size: u32) {
        // For stack stores, use actual size to properly handle char/short
        // For register-to-register, use minimum 32-bit
        match dst {
            Loc::Reg(r) if *r == src => {
                // No-op: same register
            }
            Loc::Reg(r) => {
                let reg_size = size.max(32);
                let op_size = OperandSize::from_bits(reg_size);
                // LIR: register-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(src),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Stack(offset) => {
                if size < 32 {
                    // Sub-32-bit values: zero-extend to 32 bits, then store
                    // full slot via store_to_stack_slot (which writes 64-bit)
                    let scratch = if src == Reg::R10 { Reg::R11 } else { Reg::R10 };
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(src),
                        dst: GpOperand::Reg(scratch),
                    });
                    let mask = (1i64 << size) - 1;
                    self.push_lir(X86Inst::And {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(mask),
                        dst: scratch,
                    });
                    self.store_to_stack_slot(scratch, *offset);
                } else {
                    self.store_to_stack_slot(src, *offset);
                }
            }
            Loc::Xmm(xmm) => {
                // Move from GP register to XMM register (used for Float16 rtlib returns)
                let op_size = if size <= 32 {
                    OperandSize::B32
                } else {
                    OperandSize::B64
                };
                self.push_lir(X86Inst::MovGpXmm {
                    size: op_size,
                    src,
                    dst: *xmm,
                });
            }
            _ => {}
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

        // Check if this is a 128-bit integer load (target is an int128 pseudo).
        // For int128 local variables, the IR Load uses a Sym pseudo (addr) which
        // resolves to the variable's stack slot. This is effectively a stack-to-stack
        // copy via the emit_int128_copy path. For pointer dereferences (*ptr where
        // ptr is to an int128), addr is a runtime pointer that must be dereferenced.
        if self.int128_pseudos.contains(&target) {
            let addr_loc = self.get_location(addr);
            match &addr_loc {
                Loc::Stack(_) | Loc::IncomingArg(_) | Loc::Imm(_) => {
                    // addr is a local/sym — do stack-to-stack copy
                    self.emit_int128_copy(addr, target);
                }
                Loc::Global(name) => {
                    // Global int128: load address into R10, then load both halves.
                    // emit_move would load the VALUE (double deref), so load
                    // the address directly instead.
                    let dst_lo = self.int128_lo_mem_loc(&dst_loc);
                    let dst_hi = self.int128_hi_mem_loc(&dst_loc);
                    let symbol = if name.starts_with('.') {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    if self.needs_got_access(name) {
                        // GOT: movq loads the address
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(
                                name.clone(),
                            ))),
                            dst: GpOperand::Reg(Reg::R10),
                        });
                    } else {
                        // RIP-relative: LEA to get address
                        self.push_lir(X86Inst::Lea {
                            addr: MemAddr::RipRelative(symbol),
                            dst: Reg::R10,
                        });
                    }
                    if insn.offset != 0 {
                        self.push_lir(X86Inst::Add {
                            size: OperandSize::B64,
                            src: GpOperand::Imm(insn.offset),
                            dst: Reg::R10,
                        });
                    }
                    // Load both halves from [R10]
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R10,
                            offset: 0,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::R11),
                        dst: GpOperand::Mem(dst_lo),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R10,
                            offset: 8,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::R11),
                        dst: GpOperand::Mem(dst_hi),
                    });
                }
                _ => {
                    // addr is a runtime pointer — dereference it
                    let dst_lo = self.int128_lo_mem_loc(&dst_loc);
                    let dst_hi = self.int128_hi_mem_loc(&dst_loc);
                    self.emit_move(addr, Reg::R10, 64);
                    if insn.offset != 0 {
                        self.push_lir(X86Inst::Add {
                            size: OperandSize::B64,
                            src: GpOperand::Imm(insn.offset),
                            dst: Reg::R10,
                        });
                    }
                    // Copy 16 bytes from [R10] to dst
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R10,
                            offset: 0,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::R11),
                        dst: GpOperand::Mem(dst_lo),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R10,
                            offset: 8,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::R11),
                        dst: GpOperand::Mem(dst_hi),
                    });
                }
            }
            return;
        }

        // Check if this is an FP load
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t)) || matches!(dst_loc, Loc::Xmm(_));

        if is_fp {
            // Long double uses x87, not SSE
            if self.is_longdouble_op(insn, types) {
                self.emit_x87_load(insn);
            } else {
                self.emit_fp_load(insn, types);
            }
            return;
        }

        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };

        // Sign- or zero-extend by the type's signedness. `is_unsigned`
        // answers for plain `char` per the target, so there is nothing to
        // special-case here.
        let is_unsigned = insn.typ.is_some_and(|t| types.is_unsigned(t));

        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                if mem_size <= 16 {
                    // Use sign/zero extending load
                    // LIR: use Movzx or Movsx
                    let src_size = OperandSize::from_bits(mem_size);
                    if is_unsigned {
                        self.push_lir(X86Inst::Movzx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: r,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    } else {
                        self.push_lir(X86Inst::Movsx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: r,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    }
                } else {
                    // 32/64-bit load
                    let op_size = OperandSize::from_bits(reg_size);
                    // LIR: simple Mov
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: r,
                            offset: insn.offset as i32,
                        }),
                        dst: GpOperand::Reg(dst_reg),
                    });
                }
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - load directly from stack slot
                    let stack_addr = self.stack_mem(offset - insn.offset as i32);
                    if mem_size <= 16 {
                        // LIR: sign/zero extending load from stack
                        let src_size = OperandSize::from_bits(mem_size);
                        if is_unsigned {
                            self.push_lir(X86Inst::Movzx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(stack_addr),
                                dst: dst_reg,
                            });
                        } else {
                            self.push_lir(X86Inst::Movsx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(stack_addr),
                                dst: dst_reg,
                            });
                        }
                    } else {
                        // LIR: regular load from stack
                        let op_size = OperandSize::from_bits(reg_size);
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(stack_addr),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                } else {
                    // Spilled address - load address first, then load from that address
                    // LIR: load spilled address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    if mem_size <= 16 {
                        // LIR: sign/zero extending load through R11
                        let src_size = OperandSize::from_bits(mem_size);
                        if is_unsigned {
                            self.push_lir(X86Inst::Movzx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R11,
                                    offset: insn.offset as i32,
                                }),
                                dst: dst_reg,
                            });
                        } else {
                            self.push_lir(X86Inst::Movsx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R11,
                                    offset: insn.offset as i32,
                                }),
                                dst: dst_reg,
                            });
                        }
                    } else {
                        // LIR: regular load through R11
                        let op_size = OperandSize::from_bits(reg_size);
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                }
            }
            Loc::Global(name) => {
                // Use local symbol for labels starting with '.' (e.g., .LC0 for string constants)
                let is_local_label = name.starts_with('.');
                let symbol = if is_local_label {
                    Symbol::local(name.clone())
                } else {
                    Symbol::global(name.clone())
                };

                // Check TLS first - TLS symbols need special access pattern even for external symbols
                if self.tls_symbols.contains(&name) && self.base.target.os == Os::Linux {
                    // Check if this is an external TLS variable (needs Initial Exec model)
                    // or if we're building a shared library (also needs IE model).
                    // PIE executables can use Local Exec for their own TLS variables.
                    let use_ie_model = self.use_tls_ie(&name);

                    if use_ie_model {
                        // Initial Exec TLS model for external symbols:
                        // movq symbol@GOTTPOFF(%rip), %r11  ; load TLS offset from GOT
                        // movl %fs:(%r11), %dst             ; load from thread-local storage
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(MemAddr::TlsGottpoff(symbol.clone())),
                            dst: GpOperand::Reg(Reg::R11),
                        });
                        // Now load from %fs:(%r11) with appropriate sign/zero extension
                        if mem_size <= 16 {
                            let src_size = OperandSize::from_bits(mem_size);
                            if is_unsigned {
                                self.push_lir(X86Inst::Movzx {
                                    src_size,
                                    dst_size: OperandSize::B32,
                                    src: GpOperand::Mem(MemAddr::FsBase(Reg::R11)),
                                    dst: dst_reg,
                                });
                            } else {
                                self.push_lir(X86Inst::Movsx {
                                    src_size,
                                    dst_size: OperandSize::B32,
                                    src: GpOperand::Mem(MemAddr::FsBase(Reg::R11)),
                                    dst: dst_reg,
                                });
                            }
                        } else {
                            let op_size = OperandSize::from_bits(reg_size);
                            self.push_lir(X86Inst::Mov {
                                size: op_size,
                                src: GpOperand::Mem(MemAddr::FsBase(Reg::R11)),
                                dst: GpOperand::Reg(dst_reg),
                            });
                        }
                    } else {
                        // Local Exec TLS model for local symbols: %fs:symbol@TPOFF
                        let mem_addr = MemAddr::TlsLocalExec(symbol);
                        if mem_size <= 16 {
                            let src_size = OperandSize::from_bits(mem_size);
                            if is_unsigned {
                                self.push_lir(X86Inst::Movzx {
                                    src_size,
                                    dst_size: OperandSize::B32,
                                    src: GpOperand::Mem(mem_addr.clone()),
                                    dst: dst_reg,
                                });
                            } else {
                                self.push_lir(X86Inst::Movsx {
                                    src_size,
                                    dst_size: OperandSize::B32,
                                    src: GpOperand::Mem(mem_addr.clone()),
                                    dst: dst_reg,
                                });
                            }
                        } else {
                            let op_size = OperandSize::from_bits(reg_size);
                            self.push_lir(X86Inst::Mov {
                                size: op_size,
                                src: GpOperand::Mem(mem_addr),
                                dst: GpOperand::Reg(dst_reg),
                            });
                        }
                    }
                } else if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then load value
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    if mem_size <= 16 {
                        let src_size = OperandSize::from_bits(mem_size);
                        if is_unsigned {
                            self.push_lir(X86Inst::Movzx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R11,
                                    offset: insn.offset as i32,
                                }),
                                dst: dst_reg,
                            });
                        } else {
                            self.push_lir(X86Inst::Movsx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R11,
                                    offset: insn.offset as i32,
                                }),
                                dst: dst_reg,
                            });
                        }
                    } else {
                        let op_size = OperandSize::from_bits(reg_size);
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                } else {
                    // Regular global: RIP-relative addressing
                    let mem_addr = MemAddr::RipRelative(symbol);
                    if mem_size <= 16 {
                        // LIR: sign/zero extending load from global
                        let src_size = OperandSize::from_bits(mem_size);
                        if is_unsigned {
                            self.push_lir(X86Inst::Movzx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(mem_addr.clone()),
                                dst: dst_reg,
                            });
                        } else {
                            self.push_lir(X86Inst::Movsx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(mem_addr.clone()),
                                dst: dst_reg,
                            });
                        }
                    } else {
                        // LIR: regular load from global
                        let op_size = OperandSize::from_bits(reg_size);
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(mem_addr),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                }
            }
            _ => {
                self.emit_move(addr, Reg::R11, 64);
                if mem_size <= 16 {
                    // LIR: sign/zero extending load through R11
                    let src_size = OperandSize::from_bits(mem_size);
                    if is_unsigned {
                        self.push_lir(X86Inst::Movzx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    } else {
                        self.push_lir(X86Inst::Movsx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    }
                } else {
                    // LIR: regular load through R11
                    let op_size = OperandSize::from_bits(reg_size);
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                        dst: GpOperand::Reg(dst_reg),
                    });
                }
            }
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, reg_size);
        }
    }

    pub(super) fn emit_store(&mut self, insn: &Instruction, types: &TypeTable) {
        // Use actual size for memory stores (8, 16, 32, 64 bits)
        // This is critical for char/short types that need byte/word stores
        let mem_size = insn.size;
        // Register operations use minimum 32-bit
        let reg_size = insn.size.max(32);

        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };

        // Check if this is an FP store
        let value_loc = self.get_location(value);
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t))
            || matches!(value_loc, Loc::Xmm(_) | Loc::FImm(..));

        if is_fp {
            // Long double uses x87, not SSE
            if self.is_longdouble_op(insn, types) {
                self.emit_x87_store(insn);
            } else {
                self.emit_fp_store(insn, types);
            }
            return;
        }

        // For struct stores (size > 64), we need to copy multiple words
        // The value is a symbol containing the struct data
        if mem_size > 64 {
            self.emit_struct_store(insn, addr, value);
            return;
        }

        // Get the value to a register - use its existing register if available,
        // otherwise use R10 as scratch (avoiding Rax which may hold other live values)
        let value_reg = match &value_loc {
            Loc::Reg(r) => *r,
            _ => {
                self.emit_move(value, Reg::R10, reg_size);
                Reg::R10
            }
        };

        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                let op_size = OperandSize::from_bits(mem_size);
                // LIR: store through register
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(value_reg),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    }),
                });
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                let op_size = OperandSize::from_bits(mem_size);
                if is_symbol {
                    // Local variable - store directly to stack slot.
                    // Widen 32-bit stores at offset 0 to 64-bit to prevent stale
                    // upper bits when a 32-bit result is stored into a 64-bit
                    // local (e.g., int-to-long, int-to-pointer assignments).
                    // Exception: struct/union fields at offset 0 must use exact
                    // size to avoid clobbering the adjacent field at offset 4.
                    let store_size = if mem_size == 32 && insn.offset == 0 {
                        let sym_bits = self.sym_type_sizes.get(&addr).copied().unwrap_or(64);
                        if sym_bits > 32 {
                            // Check if this is a struct/union (don't widen field stores)
                            let is_struct =
                                self.sym_type_sizes.contains_key(&addr) && sym_bits > 64;
                            if is_struct {
                                op_size // struct field: exact size
                            } else {
                                OperandSize::B64 // scalar/pointer: safe to widen
                            }
                        } else {
                            OperandSize::B64 // small scalar: safe to widen
                        }
                    } else {
                        op_size
                    };
                    // LIR: store to stack slot
                    self.push_lir(X86Inst::Mov {
                        size: store_size,
                        src: GpOperand::Reg(value_reg),
                        dst: GpOperand::Mem(self.stack_mem(offset - insn.offset as i32)),
                    });
                } else {
                    // Spilled address - load address first, then store through it
                    // LIR: load spilled address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    // LIR: store through loaded address
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(value_reg),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                    });
                }
            }
            Loc::Global(name) => {
                // Use local symbol for labels starting with '.' (e.g., .LC0 for string constants)
                let is_local_label = name.starts_with('.');
                let op_size = OperandSize::from_bits(mem_size);
                let symbol = if is_local_label {
                    Symbol::local(name.clone())
                } else {
                    Symbol::global(name.clone())
                };

                // Check TLS FIRST before GOT - TLS symbols need special access pattern
                // and should not go through the GOT path even in PIC mode
                if self.tls_symbols.contains(&name) && self.base.target.os == Os::Linux {
                    // Thread-local storage: use FS segment
                    // Use Initial Exec model for external TLS or when building shared libraries.
                    // PIE executables can use Local Exec for their own TLS variables.
                    let use_ie_model = self.use_tls_ie(&name);

                    if use_ie_model {
                        // Initial Exec: load offset from GOT, then store via FS segment
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(MemAddr::TlsGottpoff(symbol)),
                            dst: GpOperand::Reg(Reg::R11),
                        });
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Reg(value_reg),
                            dst: GpOperand::Mem(MemAddr::FsBase(Reg::R11)),
                        });
                    } else {
                        // Local Exec: direct access via %fs:symbol@TPOFF
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Reg(value_reg),
                            dst: GpOperand::Mem(MemAddr::TlsLocalExec(symbol)),
                        });
                    }
                } else if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then store
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(value_reg),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                    });
                } else {
                    // LIR: store to global via RIP-relative
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(value_reg),
                        dst: GpOperand::Mem(MemAddr::RipRelative(symbol)),
                    });
                }
            }
            _ => {
                self.emit_move(addr, Reg::R11, 64);
                let op_size = OperandSize::from_bits(mem_size);
                // LIR: store through R11
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(value_reg),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }),
                });
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
        // Get destination address
        let addr_loc = self.get_location(addr);

        // Special case: if value is immediate 0, zero the struct instead of copying
        if let Loc::Imm(0) = value_loc {
            self.emit_struct_zero(insn, addr, num_qwords);
            return;
        }

        // Load source address into R10
        match value_loc {
            Loc::Stack(offset) => {
                // LIR: lea for source address
                self.push_lir(X86Inst::Lea {
                    addr: self.stack_mem(offset),
                    dst: Reg::R10,
                });
            }
            Loc::Reg(r) => {
                if r != Reg::R10 {
                    // LIR: mov for source address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R10),
                    });
                }
            }
            Loc::Global(ref name) => {
                // LIR: lea for global source address
                if self.needs_got_access(name) {
                    // External symbols on macOS: load address from GOT
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R10),
                    });
                } else {
                    let symbol = if name.starts_with('.') {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::RipRelative(symbol),
                        dst: Reg::R10,
                    });
                }
            }
            _ => return,
        }

        // Load destination address into R11
        match addr_loc {
            Loc::Stack(offset) => {
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable — LEA to get direct stack address
                    self.push_lir(X86Inst::Lea {
                        addr: self.stack_mem(offset - insn.offset as i32),
                        dst: Reg::R11,
                    });
                } else {
                    // Spilled pointer — load the pointer value from the slot
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    if insn.offset != 0 {
                        self.push_lir(X86Inst::Add {
                            size: OperandSize::B64,
                            src: GpOperand::Imm(insn.offset),
                            dst: Reg::R11,
                        });
                    }
                }
            }
            Loc::Reg(r) => {
                if insn.offset != 0 {
                    // LIR: lea with offset
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::BaseOffset {
                            base: r,
                            offset: insn.offset as i32,
                        },
                        dst: Reg::R11,
                    });
                } else if r != Reg::R11 {
                    // LIR: mov for destination address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
            }
            Loc::Global(ref name) => {
                // LIR: lea for global destination address
                if self.needs_got_access(name) {
                    // External symbols on macOS: load address from GOT
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                } else {
                    let symbol = if name.starts_with('.') {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::RipRelative(symbol),
                        dst: Reg::R11,
                    });
                }
            }
            _ => return,
        }

        // Copy qword by qword using XMM15 as shuttle (R10=src, R11=dst).
        // XMM15 is reserved scratch — avoids clobbering RAX which is
        // allocatable and may hold a live pseudo.
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            // LIR: load from source via XMM15
            self.push_lir(X86Inst::MovFp {
                size: FpSize::Double,
                src: XmmOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::R10,
                    offset: byte_offset,
                }),
                dst: XmmOperand::Reg(XmmReg::Xmm15),
            });
            // LIR: store to destination via XMM15
            self.push_lir(X86Inst::MovFp {
                size: FpSize::Double,
                src: XmmOperand::Reg(XmmReg::Xmm15),
                dst: XmmOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: byte_offset,
                }),
            });
        }
    }

    /// Write a 128-bit constant into the sixteen bytes at `dst_loc`.
    ///
    /// Each half goes as an immediate where it fits in the `mov` displacement
    /// and through `movabs` where it does not. R10 is reserved scratch.
    pub(super) fn store_int128_imm(&mut self, v: i128, dst_loc: &Loc) {
        let lo = v as i64;
        let hi = (v >> 64) as u64 as i64;
        for (half, addr) in [
            (lo, self.int128_lo_mem_loc(dst_loc)),
            (hi, self.int128_hi_mem_loc(dst_loc)),
        ] {
            if half > i32::MAX as i64 || half < i32::MIN as i64 {
                self.push_lir(X86Inst::MovAbs {
                    imm: half,
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(addr),
                });
            } else {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(half),
                    dst: GpOperand::Mem(addr),
                });
            }
        }
    }

    fn emit_int128_copy(&mut self, src: PseudoId, dst: PseudoId) {
        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(dst);

        match &src_loc {
            Loc::Imm(v) => self.store_int128_imm(*v, &dst_loc),
            Loc::Stack(_) | Loc::IncomingArg(_) => {
                // Stack-to-stack copy: two qword moves via R10
                let src_lo = self.int128_lo_mem_loc(&src_loc);
                let dst_lo = self.int128_lo_mem_loc(&dst_loc);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(src_lo),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(dst_lo),
                });
                let src_hi = self.int128_hi_mem_loc(&src_loc);
                let dst_hi = self.int128_hi_mem_loc(&dst_loc);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(src_hi),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(dst_hi),
                });
            }
            _ => {
                // Fallback: load lo half into R10 and store it
                self.emit_move(src, Reg::R10, 64);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(self.int128_lo_mem_loc(&dst_loc)),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(0),
                    dst: GpOperand::Mem(self.int128_hi_mem_loc(&dst_loc)),
                });
            }
        }
    }

    /// Helper: get lo-half MemAddr for a 128-bit Loc.
    pub(super) fn int128_lo_mem_loc(&self, loc: &Loc) -> MemAddr {
        match loc {
            Loc::Stack(offset) => self.stack_mem(*offset),
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: *offset,
            },
            _ => panic!("int128_lo_mem_loc: expected stack loc, got {:?}", loc),
        }
    }

    /// Helper: get hi-half MemAddr for a 128-bit Loc.
    pub(super) fn int128_hi_mem_loc(&self, loc: &Loc) -> MemAddr {
        match loc {
            Loc::Stack(offset) => self.stack_mem(*offset - 8),
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: *offset + 8,
            },
            _ => panic!("int128_hi_mem_loc: expected stack loc, got {:?}", loc),
        }
    }

    /// Emit code to zero a struct (for struct = {0} initialization)
    fn emit_struct_zero(&mut self, insn: &Instruction, addr: PseudoId, num_qwords: u32) {
        let addr_loc = self.get_location(addr);

        // Load destination address into R11
        match addr_loc {
            Loc::Stack(offset) => {
                self.push_lir(X86Inst::Lea {
                    addr: self.stack_mem(offset - insn.offset as i32),
                    dst: Reg::R11,
                });
            }
            Loc::Reg(r) => {
                if insn.offset != 0 {
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::BaseOffset {
                            base: r,
                            offset: insn.offset as i32,
                        },
                        dst: Reg::R11,
                    });
                } else if r != Reg::R11 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
            }
            Loc::Global(ref name) => {
                if self.needs_got_access(name) {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                } else {
                    let symbol = if name.starts_with('.') {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::RipRelative(symbol),
                        dst: Reg::R11,
                    });
                }
                if insn.offset != 0 {
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(insn.offset),
                        dst: Reg::R11,
                    });
                }
            }
            _ => return,
        }

        // Load 0 into R10 once
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Imm(0),
            dst: GpOperand::Reg(Reg::R10),
        });

        // Store zeros to each qword
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R10),
                dst: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: byte_offset,
                }),
            });
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
        // Handle 128-bit integer copies (stack-to-stack).
        // Only intercept when src/dst are known int128 pseudos.
        // Do NOT check size==128 here — 16-byte structs also have size 128 but
        // use the struct copy path instead.
        if self.int128_pseudos.contains(&src) || self.int128_pseudos.contains(&dst) {
            self.emit_int128_copy(src, dst);
            return;
        }

        // Keep actual size for handling narrow types
        let actual_size = size;
        let reg_size = size.max(32);
        let dst_loc = self.get_location(dst);
        let src_loc = self.get_location(src);

        // Check if this is a FP copy (source or dest is in XMM, is FImm, or type is float)
        let is_fp_copy = matches!(&src_loc, Loc::Xmm(_) | Loc::FImm(..))
            || matches!(&dst_loc, Loc::Xmm(_))
            || typ.is_some_and(|t| types.is_float(t));

        // M9a — identity-Copy elision. When the allocator placed the
        // source and destination pseudos at the same location AND the
        // copy doesn't carry narrow-type truncation (8/16 bit needs
        // explicit AND/SHL+SAR) AND it's not a FP copy (FP path has
        // size-specific quirks), the Copy is a true no-op and can be
        if src_loc == dst_loc && actual_size >= 32 && !is_fp_copy {
            return;
        }

        // Check if this is long double (uses x87, not XMM)
        let is_longdouble = typ.is_some_and(|t| types.kind(t) == TypeKind::LongDouble);

        // Sign- or zero-extend by the type's signedness. `is_unsigned`
        // answers for plain `char` per the target, so there is nothing to
        // special-case here.
        let is_unsigned = typ.is_some_and(|t| types.is_unsigned(t));

        if is_fp_copy {
            // Long double uses x87, not XMM
            if is_longdouble {
                let src_addr = self.get_x87_mem_addr(src);
                self.push_lir(X86Inst::X87Load { addr: src_addr });
                let dst_addr = self.get_x87_mem_addr(dst);
                self.push_lir(X86Inst::X87Store { addr: dst_addr });
            } else {
                // Handle regular FP copy (float/double).
                // Reserved scratch when target lives on the stack (see
                // emit_fp_binop in float.rs for the full rationale).
                let dst_xmm = match &dst_loc {
                    Loc::Xmm(x) => *x,
                    _ => XmmReg::Xmm15,
                };

                // Use type-aware size for FP operations
                let fp_size = typ.map(|t| types.size_bits(t)).unwrap_or(reg_size).max(32);
                let fp_fmt = self.fp_format(typ, fp_size, types);
                self.emit_fp_move(src, dst_xmm, fp_fmt);

                if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
                    self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, fp_fmt);
                }
            }
        } else {
            // Integer copy
            match &dst_loc {
                Loc::Reg(r) => {
                    self.emit_move(src, *r, reg_size);
                    // For narrow types (8 or 16 bits), truncate to correct width
                    // Unsigned: zero-extend (AND mask)
                    // Signed: sign-extend (shift left then arithmetic shift right)
                    if actual_size == 8 {
                        if is_unsigned {
                            // LIR: zero-extend with AND mask
                            self.push_lir(X86Inst::And {
                                size: OperandSize::B32,
                                src: GpOperand::Imm(0xFF),
                                dst: *r,
                            });
                        } else {
                            // Sign-extend: shift left 24 bits then arithmetic shift right 24 bits
                            // LIR: shift left
                            self.push_lir(X86Inst::Shl {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(24),
                                dst: *r,
                            });
                            // LIR: arithmetic shift right
                            self.push_lir(X86Inst::Sar {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(24),
                                dst: *r,
                            });
                        }
                    } else if actual_size == 16 {
                        if is_unsigned {
                            // LIR: zero-extend with AND mask
                            self.push_lir(X86Inst::And {
                                size: OperandSize::B32,
                                src: GpOperand::Imm(0xFFFF),
                                dst: *r,
                            });
                        } else {
                            // Sign-extend: shift left 16 bits then arithmetic shift right 16 bits
                            // LIR: shift left
                            self.push_lir(X86Inst::Shl {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(16),
                                dst: *r,
                            });
                            // LIR: arithmetic shift right
                            self.push_lir(X86Inst::Sar {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(16),
                                dst: *r,
                            });
                        }
                    }
                }
                Loc::Stack(_) => {
                    // All regalloc stack slots are 8 bytes on x86-64.
                    // Always store full 64-bit to prevent stale upper bytes
                    // from being read by subsequent 64-bit loads of the same slot.
                    // movl to a 32-bit register zero-extends to 64-bit on x86-64,
                    // so loading with reg_size then storing with 64 is correct.
                    self.emit_move(src, Reg::R10, reg_size);
                    if actual_size <= 16 {
                        // 8/16-bit values: store actual size (these are char/short
                        // fields stored to struct offsets, not full stack slots)
                        self.emit_move_to_loc(Reg::R10, &dst_loc, actual_size);
                    } else {
                        // 32-bit values: store as 64-bit to zero-fill upper 4 bytes
                        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
                    }
                }
                _ => {}
            }
        }
    }
}
