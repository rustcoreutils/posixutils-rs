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
use crate::ir::{Instruction, MemoryOrder, PseudoId};
use crate::types::TypeTable;

/// Helper enum for atomic bitwise operations
#[derive(Clone, Copy)]
enum AtomicBitOp {
    And,
    Or,
    Xor,
}

/// The one mapping from a memory order to instructions on x86-64, which is
/// gcc's.
///
/// x86-64 is TSO: the only reordering it allows is a later load passing an
/// earlier store, and only seq-cst forbids that. So seq-cst is the one order
/// that costs anything -- `xchg` for a store, `mfence` for a fence -- while
/// every load is a plain `mov` and every read-modify-write a `lock`ed
/// instruction, which is a full barrier at any order.
fn orders_store_then_load(order: MemoryOrder) -> bool {
    order == MemoryOrder::SeqCst
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
        let target = insn.target.expect("atomic store needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        // The memory operand must be exactly as wide as the object; widening
        // it to 32 bits made an 8- or 16-bit atomic read-modify-write touch
        // its neighbours. Register moves still use at least 32 bits.
        let mem_size = insn.size;
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(mem_size);

        // XCHG is a store with a full barrier; anything weaker is a plain
        // store, which already has release semantics on x86.
        if orders_store_then_load(insn.extra().memory_order) {
            // The value first: resolving the address writes only R11.
            let value_loc = self.get_location(value);
            self.emit_mov_to_reg(value_loc, Reg::R10, size);
            let mem_addr = self.atomic_mem_operand(addr, insn.displacement(), Reg::R11);

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

        // The address first: the value goes into a register that may hold it.
        let mem_addr = self.atomic_mem_operand(addr, insn.displacement(), Reg::R11);

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

        // Every operand is read before any register that can hold another
        // is written. The allocator never places a pseudo in R10 or R11, so
        // the address and the desired value go there first; R9 may hold
        // either of them, so the expected object's address goes there last.
        let mem_addr = self.atomic_mem_operand(addr, insn.displacement(), Reg::R11);
        let desired_loc = self.get_location(desired);
        self.emit_mov_to_reg(desired_loc, Reg::R10, size);
        let expected_mem = self.atomic_mem_operand(expected_ptr, 0, Reg::R9);

        // Load the expected value into RAX
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Mem(expected_mem.clone()),
            dst: GpOperand::Reg(Reg::Rax),
        });

        // LOCK CMPXCHG: if *addr == RAX, set *addr = R10 and ZF=1
        //               else RAX = *addr and ZF=0
        self.push_lir(X86Inst::LockCmpxchg {
            size: op_size,
            src: Reg::R10,
            mem: mem_addr,
        });

        // SETE stores 1 if ZF=1 (success), 0 otherwise (use R8 to avoid clobbering)
        self.push_lir(X86Inst::SetCC {
            cc: CondCode::Eq,
            dst: Reg::R8,
        });

        // On failure, store RAX (the value found) to the expected object
        let label_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        let skip_label = Label::internal("cas_done", label_suffix);
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Eq, // Jump if equal (success)
            target: skip_label.clone(),
        });
        // Failed: store actual value to *expected
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Mem(expected_mem),
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

        // The address first: the value goes into a register that may hold it.
        let mem_addr = self.atomic_mem_operand(addr, insn.displacement(), Reg::R11);

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

        // The address first: the value goes into a register that may hold it.
        let mem_addr = self.atomic_mem_operand(addr, insn.displacement(), Reg::R11);

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

        // The address first: the value goes into a register that may hold it.
        let mem_addr = self.atomic_mem_operand(addr, insn.displacement(), Reg::R11);

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
        let loop_label = Label::internal("atomic_bitop", label_suffix);
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
        let target = insn.target.expect("fence needs target");

        // An acquire, release or acq-rel fence needs no instruction: x86
        // already keeps loads and stores in every order but store-then-load.
        // Nor does a signal fence, at any order.
        if insn
            .hardware_fence_order()
            .is_some_and(orders_store_then_load)
        {
            self.push_lir(X86Inst::Mfence);
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
                let pattern = v.to_bits_at_width(bits);
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(pattern),
                    dst: GpOperand::Reg(reg),
                });
            }
        }
    }

    /// The memory operand of an atomic access through `addr`, found by the
    /// rule every ordinary load and store uses ([`Self::scalar_mem_operand`]).
    ///
    /// An allocated base register is copied to `scratch`: the atomic
    /// sequences overwrite RAX, RCX, R8 and R9 after resolving the address,
    /// and any of them can be where the allocator put the pointer.
    fn atomic_mem_operand(&mut self, addr: PseudoId, disp: i32, scratch: Reg) -> MemAddr {
        match self.scalar_mem_operand(addr, disp, scratch) {
            MemAddr::BaseOffset { base, offset } if Reg::allocatable().contains(&base) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(base),
                    dst: GpOperand::Reg(scratch),
                });
                MemAddr::BaseOffset {
                    base: scratch,
                    offset,
                }
            }
            mem => mem,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arch::codegen::PseudoTable;
    use crate::ir::Pseudo;
    use crate::target::{Arch, Os, Target};

    fn codegen(ptr: PseudoId, loc: Loc) -> X86_64CodeGen {
        let mut cg = X86_64CodeGen::new(Target::new(Arch::X86_64, Os::Linux));
        cg.pseudos = PseudoTable::new(&[Pseudo::reg(ptr, 1)]);
        cg.locations.set(ptr, loc);
        cg
    }

    /// A pointer the allocator left in RAX moves to the scratch register,
    /// since the atomic sequence loads its value operand into RAX next.
    #[test]
    fn an_allocated_base_moves_to_the_scratch_register() {
        let ptr = PseudoId(1);
        let mut cg = codegen(ptr, Loc::Reg(Reg::Rax));
        assert_eq!(
            cg.atomic_mem_operand(ptr, 0, Reg::R11),
            MemAddr::BaseOffset {
                base: Reg::R11,
                offset: 0
            }
        );
        assert!(matches!(
            cg.base.lir_buffer.as_slice(),
            [X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::Rax),
                dst: GpOperand::Reg(Reg::R11),
            }]
        ));
    }

    /// A spilled pointer is loaded from its slot -- never its slot's address,
    /// which made an atomic add through it add to the pointer itself.
    #[test]
    fn a_spilled_pointer_is_loaded_not_addressed() {
        let ptr = PseudoId(1);
        let mut cg = codegen(ptr, Loc::Stack(56));
        let slot = cg.stack_mem(56);
        cg.atomic_mem_operand(ptr, 0, Reg::R11);
        assert!(matches!(
            cg.base.lir_buffer.as_slice(),
            [X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(m),
                dst: GpOperand::Reg(Reg::R11),
            }] if *m == slot
        ));
    }
}
