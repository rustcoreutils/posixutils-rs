//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 C11 atomic operations and fences
//

use crate::arch::lir::{CondCode, Directive, Label, OperandSize, Symbol};
use crate::arch::x86_64::codegen::X86_64CodeGen;
use crate::arch::x86_64::lir::{GpOperand, MemAddr, X86Inst};
use crate::arch::x86_64::regalloc::{Loc, Reg};
use crate::ir::Instruction;
use crate::types::TypeTable;

/// Helper enum for atomic bitwise operations
#[derive(Clone, Copy)]
enum AtomicBitOp {
    And,
    Or,
    Xor,
}

impl X86_64CodeGen {
    /// Emit atomic load
    /// On x86-64, aligned loads are already atomic - just use regular mov
    pub(super) fn emit_atomic_load(&mut self, insn: &Instruction, types: &TypeTable) {
        // Atomic load is identical to regular load on x86-64 for aligned data
        // Memory ordering is handled by x86's strong memory model
        self.emit_load(insn, types);
    }

    /// Emit atomic store
    /// On x86-64, aligned stores are atomic. For SeqCst, use XCHG for full barrier.
    pub(super) fn emit_atomic_store(&mut self, insn: &Instruction, types: &TypeTable) {
        use crate::ir::MemoryOrder;

        let target = insn.target.expect("atomic store needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        // The memory operand must be exactly as wide as the object; widening
        // it to 32 bits made an 8- or 16-bit atomic read-modify-write touch
        // its neighbours. Register moves still use at least 32 bits.
        let mem_size = insn.size;
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(mem_size);

        // For SeqCst, use XCHG which provides full barrier
        // For weaker orderings, regular store + optional SFENCE is sufficient
        if insn.memory_order == MemoryOrder::SeqCst {
            // Load value into a register
            let value_loc = self.get_location(value);
            let addr_loc = self.get_location(addr);

            // Move value to R10
            self.emit_mov_to_reg(value_loc, Reg::R10, size);

            // Get address into R11
            let mem_addr = match addr_loc {
                Loc::Reg(r) => MemAddr::BaseOffset { base: r, offset: 0 },
                Loc::Stack(offset) => {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: 0,
                    }
                }
                Loc::Global(name) => {
                    let symbol = Symbol::global(self.format_symbol_name(&name));
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::RipRelative(symbol),
                        dst: Reg::R11,
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: 0,
                    }
                }
                _ => {
                    // Handle other cases
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(0),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: 0,
                    }
                }
            };

            // XCHG provides atomic store with full barrier
            self.push_lir(X86Inst::Xchg {
                size: op_size,
                reg: Reg::R10,
                mem: mem_addr,
            });
        } else {
            // For release/relaxed, regular store is sufficient on x86
            self.emit_store(insn, types);
        }

        // Target is void, but we need to assign something
        self.locations.set(target, Loc::Imm(0));
    }

    pub(super) fn emit_atomic_swap(&mut self, insn: &Instruction, types: &TypeTable) {
        let target = insn.target.expect("atomic swap needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        // The memory operand must be exactly as wide as the object; widening
        // it to 32 bits made an 8- or 16-bit atomic read-modify-write touch
        // its neighbours. Register moves still use at least 32 bits.
        let mem_size = insn.size;
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(mem_size);

        let value_loc = self.get_location(value);
        let addr_loc = self.get_location(addr);

        // Get address FIRST (before loading value, in case addr is in RAX)
        let mem_addr = self.get_mem_addr_for_atomic(addr_loc);

        // Move new value to RAX (will hold old value after XCHG)
        self.emit_mov_to_reg(value_loc, Reg::Rax, size);

        // XCHG atomically swaps RAX with memory
        self.push_lir(X86Inst::Xchg {
            size: op_size,
            reg: Reg::Rax,
            mem: mem_addr,
        });

        // Result (old value) is in RAX
        self.extend_narrow_atomic_result(insn, types);
        // The result is in RAX because the instruction requires it, but the
        // allocator assigned this pseudo its own location. Overwriting that
        // assignment made every atomic result alias RAX, so two atomic results
        // live at once collapsed into one: `f(&a) + f(&b)` became `add %rax,
        // %rax`. Move it to where the allocator expects instead.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::Rax) {
            self.emit_move_to_loc(Reg::Rax, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic compare-and-swap
    pub(super) fn emit_atomic_cas(&mut self, insn: &Instruction, _types: &TypeTable) {
        let target = insn.target.expect("atomic cas needs target");
        let addr = insn.src[0];
        let expected_ptr = insn.src[1];
        let desired = insn.src[2];
        // As above: the compare-and-exchange must be exactly as wide as the
        // object, or it reads and writes adjacent bytes.
        let mem_size = insn.size;
        let size = insn.size.max(32);

        let op_size = OperandSize::from_bits(mem_size);

        let addr_loc = self.get_location(addr);
        let expected_loc = self.get_location(expected_ptr);
        let desired_loc = self.get_location(desired);

        // IMPORTANT: Regalloc may have assigned operands to any register including
        // R9, R10, R11, or RAX. Loading one operand into a scratch register can
        // clobber another operand.
        //
        // Strategy: We need to load three values into R9, R10, R11 (plus RAX for *expected).
        // Any source could be in any of these registers. We use a dependency-aware load order:
        //
        // 1. Collect which sources are in which registers
        // 2. Load into target registers in an order that doesn't clobber unread sources
        //
        // For simplicity, we use the red zone (128 bytes below RSP) as scratch space.
        // We spill all three operands first, then load from there.

        // Helper lambda to check if a location is a specific register
        let is_reg = |loc: &Loc, r: Reg| -> bool { matches!(loc, Loc::Reg(x) if *x == r) };

        // Use red zone for temporary storage at RSP-8, RSP-16, RSP-24
        let addr_temp = MemAddr::BaseOffset {
            base: Reg::Rsp,
            offset: -8,
        };
        let expected_temp = MemAddr::BaseOffset {
            base: Reg::Rsp,
            offset: -16,
        };
        let desired_temp = MemAddr::BaseOffset {
            base: Reg::Rsp,
            offset: -24,
        };

        // Step 1: Spill all three operands to red zone.
        // We need a temporary register that is NOT one of our targets (R9, R10, R11).
        // Use RCX as temp since it's caller-saved and not involved here.

        // Spill addr
        if let Loc::Reg(r) = addr_loc {
            // Already in a register, just store it
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(r),
                dst: GpOperand::Mem(addr_temp.clone()),
            });
        } else {
            self.emit_mov_to_reg(addr_loc, Reg::Rcx, 64);
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::Rcx),
                dst: GpOperand::Mem(addr_temp.clone()),
            });
        }

        // Spill expected_ptr
        if let Loc::Reg(r) = expected_loc {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(r),
                dst: GpOperand::Mem(expected_temp.clone()),
            });
        } else {
            self.emit_mov_to_reg(expected_loc, Reg::Rcx, 64);
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::Rcx),
                dst: GpOperand::Mem(expected_temp.clone()),
            });
        }

        // Spill desired
        if let Loc::Reg(r) = desired_loc {
            self.push_lir(X86Inst::Mov {
                size: op_size,
                src: GpOperand::Reg(r),
                dst: GpOperand::Mem(desired_temp.clone()),
            });
        } else {
            self.emit_mov_to_reg(desired_loc, Reg::Rcx, size);
            self.push_lir(X86Inst::Mov {
                size: op_size,
                src: GpOperand::Reg(Reg::Rcx),
                dst: GpOperand::Mem(desired_temp.clone()),
            });
        }

        // Step 2: Load from red zone into target registers.
        // Now all values are safely on stack, order doesn't matter.

        // Load addr into R11
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(addr_temp),
            dst: GpOperand::Reg(Reg::R11),
        });

        // Load expected_ptr into R9
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(expected_temp),
            dst: GpOperand::Reg(Reg::R9),
        });

        // Load desired into R10
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Mem(desired_temp),
            dst: GpOperand::Reg(Reg::R10),
        });

        // Suppress unused variable warnings
        let _ = is_reg;

        // Load expected value from *expected_ptr (R9) into RAX
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R9,
                offset: 0,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });

        // LOCK CMPXCHG: if *addr == RAX, set *addr = R10 and ZF=1
        //               else RAX = *addr and ZF=0
        self.push_lir(X86Inst::LockCmpxchg {
            size: op_size,
            src: Reg::R10,
            mem: MemAddr::BaseOffset {
                base: Reg::R11,
                offset: 0,
            },
        });

        // SETE stores 1 if ZF=1 (success), 0 otherwise (use R8 to avoid clobbering)
        self.push_lir(X86Inst::SetCC {
            cc: CondCode::Eq,
            dst: Reg::R8,
        });

        // On failure, store RAX (actual value) to *expected (R9 has expected_ptr)
        let label_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        let skip_label = Label::new("cas_done", label_suffix);
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Eq, // Jump if equal (success)
            target: skip_label.clone(),
        });
        // Failed: store actual value to *expected
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R9,
                offset: 0,
            }),
        });
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(skip_label)));

        // Zero-extend result (0 or 1) to full register
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(Reg::R8),
            dst: Reg::Rax,
        });

        // Result (success flag) is in RAX
        // The result is in RAX because the instruction requires it, but the
        // allocator assigned this pseudo its own location. Overwriting that
        // assignment made every atomic result alias RAX, so two atomic results
        // live at once collapsed into one: `f(&a) + f(&b)` became `add %rax,
        // %rax`. Move it to where the allocator expects instead.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::Rax) {
            self.emit_move_to_loc(Reg::Rax, &dst_loc, size.max(32));
        }
    }

    /// Widen a narrow atomic result in RAX to a full 32-bit register value.
    ///
    /// An 8- or 16-bit atomic operation leaves only the low bits of RAX
    /// meaningful; every consumer expects at least 32. Extend with the same
    /// signedness rule `emit_load` uses, so `_Atomic signed char` reads back
    /// negative rather than as a large positive.
    fn extend_narrow_atomic_result(&mut self, insn: &Instruction, types: &TypeTable) {
        let mem_size = insn.size;
        if mem_size >= 32 {
            return;
        }

        let is_unsigned = insn.typ.is_some_and(|t| types.is_unsigned(t));

        let src_size = OperandSize::from_bits(mem_size);
        if is_unsigned {
            self.push_lir(X86Inst::Movzx {
                src_size,
                dst_size: OperandSize::B32,
                src: GpOperand::Reg(Reg::Rax),
                dst: Reg::Rax,
            });
        } else {
            self.push_lir(X86Inst::Movsx {
                src_size,
                dst_size: OperandSize::B32,
                src: GpOperand::Reg(Reg::Rax),
                dst: Reg::Rax,
            });
        }
    }

    pub(super) fn emit_atomic_fetch_add(&mut self, insn: &Instruction, types: &TypeTable) {
        let target = insn.target.expect("atomic fetch_add needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        // The memory operand must be exactly as wide as the object; widening
        // it to 32 bits made an 8- or 16-bit atomic read-modify-write touch
        // its neighbours. Register moves still use at least 32 bits.
        let mem_size = insn.size;
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(mem_size);

        let value_loc = self.get_location(value);
        let addr_loc = self.get_location(addr);

        // Get address FIRST (before loading value, in case addr is in RAX)
        let mem_addr = self.get_mem_addr_for_atomic(addr_loc);

        // Move value to RAX
        self.emit_mov_to_reg(value_loc, Reg::Rax, size);

        // LOCK XADD: atomically adds RAX to *addr, returns old value in RAX
        self.push_lir(X86Inst::LockXadd {
            size: op_size,
            reg: Reg::Rax,
            mem: mem_addr,
        });

        // Result (old value) is in RAX
        self.extend_narrow_atomic_result(insn, types);
        // The result is in RAX because the instruction requires it, but the
        // allocator assigned this pseudo its own location. Overwriting that
        // assignment made every atomic result alias RAX, so two atomic results
        // live at once collapsed into one: `f(&a) + f(&b)` became `add %rax,
        // %rax`. Move it to where the allocator expects instead.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::Rax) {
            self.emit_move_to_loc(Reg::Rax, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic fetch-and-subtract
    pub(super) fn emit_atomic_fetch_sub(&mut self, insn: &Instruction, types: &TypeTable) {
        let target = insn.target.expect("atomic fetch_sub needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        // The memory operand must be exactly as wide as the object; widening
        // it to 32 bits made an 8- or 16-bit atomic read-modify-write touch
        // its neighbours. Register moves still use at least 32 bits.
        let mem_size = insn.size;
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(mem_size);

        let value_loc = self.get_location(value);
        let addr_loc = self.get_location(addr);

        // Get address FIRST (before loading value, in case addr is in RAX)
        let mem_addr = self.get_mem_addr_for_atomic(addr_loc);

        // Negate value: sub is add of negative
        self.emit_mov_to_reg(value_loc, Reg::Rax, size);
        self.push_lir(X86Inst::Neg {
            size: op_size,
            dst: Reg::Rax,
        });

        // LOCK XADD with negated value
        self.push_lir(X86Inst::LockXadd {
            size: op_size,
            reg: Reg::Rax,
            mem: mem_addr,
        });

        // Result (old value) is in RAX
        self.extend_narrow_atomic_result(insn, types);
        // The result is in RAX because the instruction requires it, but the
        // allocator assigned this pseudo its own location. Overwriting that
        // assignment made every atomic result alias RAX, so two atomic results
        // live at once collapsed into one: `f(&a) + f(&b)` became `add %rax,
        // %rax`. Move it to where the allocator expects instead.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::Rax) {
            self.emit_move_to_loc(Reg::Rax, &dst_loc, size.max(32));
        }
    }

    pub(super) fn emit_atomic_fetch_and(&mut self, insn: &Instruction, types: &TypeTable) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::And, types);
    }

    pub(super) fn emit_atomic_fetch_or(&mut self, insn: &Instruction, types: &TypeTable) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::Or, types);
    }

    pub(super) fn emit_atomic_fetch_xor(&mut self, insn: &Instruction, types: &TypeTable) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::Xor, types);
    }

    /// Helper for atomic fetch bitwise operations (AND, OR, XOR)
    /// Uses CMPXCHG loop since x86 doesn't have LOCK AND/OR/XOR that return old value
    fn emit_atomic_fetch_bitop(&mut self, insn: &Instruction, op: AtomicBitOp, types: &TypeTable) {
        let target = insn.target.expect("atomic fetch needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        // The memory operand must be exactly as wide as the object; widening
        // it to 32 bits made an 8- or 16-bit atomic read-modify-write touch
        // its neighbours. Register moves still use at least 32 bits.
        let mem_size = insn.size;
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(mem_size);

        let value_loc = self.get_location(value);
        let addr_loc = self.get_location(addr);

        // Get address into R11
        let mem_addr = self.get_mem_addr_for_atomic(addr_loc);

        // Move operand value to R10
        self.emit_mov_to_reg(value_loc, Reg::R10, size);

        // Load current value into RAX
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Mem(mem_addr.clone()),
            dst: GpOperand::Reg(Reg::Rax),
        });

        // Loop label
        let label_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        let loop_label = Label::new("atomic_bitop", label_suffix);
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // Copy old value to RCX for computing new value
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Reg(Reg::Rcx),
        });

        // Apply bitwise operation: RCX = RCX op R10
        match op {
            AtomicBitOp::And => {
                self.push_lir(X86Inst::And {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::Rcx,
                });
            }
            AtomicBitOp::Or => {
                self.push_lir(X86Inst::Or {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::Rcx,
                });
            }
            AtomicBitOp::Xor => {
                self.push_lir(X86Inst::Xor {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::Rcx,
                });
            }
        }

        // LOCK CMPXCHG: if *addr == RAX, set *addr = RCX
        self.push_lir(X86Inst::LockCmpxchg {
            size: op_size,
            src: Reg::Rcx,
            mem: mem_addr,
        });

        // If failed (ZF=0), retry - RAX now has actual value
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Ne,
            target: loop_label,
        });

        // Result (old value) is in RAX
        self.extend_narrow_atomic_result(insn, types);
        // The result is in RAX because the instruction requires it, but the
        // allocator assigned this pseudo its own location. Overwriting that
        // assignment made every atomic result alias RAX, so two atomic results
        // live at once collapsed into one: `f(&a) + f(&b)` became `add %rax,
        // %rax`. Move it to where the allocator expects instead.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::Rax) {
            self.emit_move_to_loc(Reg::Rax, &dst_loc, size.max(32));
        }
    }

    pub(super) fn emit_fence(&mut self, insn: &Instruction) {
        use crate::ir::MemoryOrder;

        let target = insn.target.expect("fence needs target");

        // Emit appropriate fence based on memory ordering
        match insn.memory_order {
            MemoryOrder::SeqCst | MemoryOrder::AcqRel => {
                self.push_lir(X86Inst::Mfence);
            }
            MemoryOrder::Acquire | MemoryOrder::Consume => {
                // LFENCE - but x86 loads have acquire semantics anyway
                self.push_lir(X86Inst::Lfence);
            }
            MemoryOrder::Release => {
                // SFENCE - but x86 stores have release semantics anyway
                self.push_lir(X86Inst::Sfence);
            }
            MemoryOrder::Relaxed => {
                // No fence needed for relaxed
            }
        }

        self.locations.set(target, Loc::Imm(0));
    }

    /// Helper to move a value to a register
    fn emit_mov_to_reg(&mut self, loc: Loc, reg: Reg, size: u32) {
        let op_size = OperandSize::from_bits(size);
        match loc {
            Loc::Reg(src) => {
                if src != reg {
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(src),
                        dst: GpOperand::Reg(reg),
                    });
                }
            }
            Loc::Imm(v) => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(v as i64),
                    dst: GpOperand::Reg(reg),
                });
            }
            Loc::Stack(offset) => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(self.stack_mem(offset)),
                    dst: GpOperand::Reg(reg),
                });
            }
            Loc::Global(name) => {
                let symbol = Symbol::global(self.format_symbol_name(&name));
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(MemAddr::RipRelative(symbol)),
                    dst: GpOperand::Reg(reg),
                });
            }
            // A caller-passed stack argument. Reachable whenever an atomic
            // operand is the seventh or later parameter.
            Loc::IncomingArg(offset) => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset,
                    }),
                    dst: GpOperand::Reg(reg),
                });
            }
            // The atomic operations move values through general-purpose
            // registers, so a floating-point operand has to come across as its
            // bit pattern. Silently loading 0 here is what made every
            // `_Atomic float`/`_Atomic double` operation produce zero.
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovXmmGp {
                    size: op_size,
                    src: x,
                    dst: reg,
                });
            }
            Loc::FImm(v, bits) => {
                let pattern: i64 = if bits <= 32 {
                    (v.to_f64() as f32).to_bits() as i64
                } else {
                    v.to_f64().to_bits() as i64
                };
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(pattern),
                    dst: GpOperand::Reg(reg),
                });
            }
        }
    }

    /// Helper to get memory address for atomic operations
    /// Always copies the address to R11 to avoid conflicts with RAX used for values
    fn get_mem_addr_for_atomic(&mut self, loc: Loc) -> MemAddr {
        match loc {
            Loc::Reg(r) => {
                // Always copy to R11 to avoid conflicts when RAX is used for values
                if r != Reg::R11 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: 0,
                }
            }
            Loc::Stack(offset) => {
                // Load address into R11
                self.push_lir(X86Inst::Lea {
                    addr: self.stack_mem(offset),
                    dst: Reg::R11,
                });
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: 0,
                }
            }
            Loc::Global(name) => {
                let symbol = Symbol::global(self.format_symbol_name(&name));
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::RipRelative(symbol),
                    dst: Reg::R11,
                });
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: 0,
                }
            }
            _ => {
                // Load zero address into R11 (this shouldn't happen)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(0),
                    dst: GpOperand::Reg(Reg::R11),
                });
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: 0,
                }
            }
        }
    }
}
