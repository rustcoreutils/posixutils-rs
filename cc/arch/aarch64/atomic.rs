//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 C11 atomic operations and fences
//

use crate::arch::aarch64::codegen::Aarch64CodeGen;
use crate::arch::aarch64::lir::{Aarch64Inst, DmbOption, GpOperand, MemAddr};
use crate::arch::aarch64::regalloc::{Loc, Reg};
use crate::arch::lir::{CondCode, Directive, FpSize, OperandSize};
use crate::ir::Instruction;

/// Helper enum for atomic bitwise operations
#[derive(Clone, Copy)]
enum AtomicBitOp {
    And,
    Or,
    Xor,
}

// Register discipline for this file
//
// Every temporary here must be a scratch register -- X9, X10, X11, and the
// linker-scratch pair X16/X17 -- plus X8 for the store-exclusive status.
// None of those is ever handed to a pseudo by the register allocator, which
// is the whole reason they exist (see `regalloc.rs`).
//
// These emitters used X0, X1 and X2, which *are* allocatable. Nothing caught
// it while every local still went through memory: no value was live in an
// argument register across an atomic. Once locals were promoted, the six
// parameters of a function bracketing a `fetch_add` sat in W0-W5, and the
// LL/SC expansion overwrote three of them -- so the addend, the loaded old
// value and the computed new value each destroyed a live argument.

impl Aarch64CodeGen {
    pub(super) fn emit_atomic_load(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic load needs target");
        let addr = insn.src[0];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);

        // Load the pointer (64-bit) into a scratch register
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // LDAR provides acquire semantics (sufficient for SeqCst on AArch64)
        self.push_lir(Aarch64Inst::Ldar {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X9,
        });

        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X9) {
            self.emit_move_to_loc(Reg::X9, &dst_loc, size.max(32));
        }
    }

    pub(super) fn emit_atomic_store(&mut self, insn: &Instruction) {
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load the pointer first, so a value in the same register is
        // still readable when it is loaded next.
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load the value
        self.emit_mov_to_reg(value_loc, Reg::X9, size);

        // STLR provides release semantics (sufficient for SeqCst on AArch64)
        self.push_lir(Aarch64Inst::Stlr {
            size: op_size,
            src: Reg::X9,
            addr: MemAddr::Base(Reg::X10),
        });

        // Atomic store has no result value
        if let Some(target) = insn.target {
            self.locations.set(target, Loc::Imm(0));
        }
    }

    /// Emit atomic swap using LL/SC (LDAXR/STLXR loop)
    pub(super) fn emit_atomic_swap(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic swap needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load the pointer first, so a value in the same register is
        // still readable when it is loaded next.
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load the new value
        self.emit_mov_to_reg(value_loc, Reg::X9, size);

        // LL/SC loop for atomic swap
        let loop_label = self.next_unique_label("swap_loop");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: load-acquire exclusive old value
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X11,
        });

        // STLXR: try to store the new value; status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X9,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry if store failed (status != 0)
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32, // Status is always 32-bit
            src: Reg::X8,
            target: loop_label,
        });

        // Result: the old value
        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X11) {
            self.emit_move_to_loc(Reg::X11, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic compare-and-swap using LL/SC
    pub(super) fn emit_atomic_cas(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic CAS needs target");
        let addr = insn.src[0];
        let expected_ptr = insn.src[1];
        let desired = insn.src[2];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let expected_loc = self.get_location(expected_ptr);
        let desired_loc = self.get_location(desired);

        // Load pointer to atomic variable into X10 FIRST
        // (before the other loads, so none of them can clobber it)
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load expected_ptr (pointer to expected value) into X11
        // Then load the expected value from that address into X9
        self.emit_mov_to_reg(expected_loc, Reg::X11, 64);
        self.push_lir(Aarch64Inst::Ldr {
            size: op_size,
            addr: MemAddr::Base(Reg::X11),
            dst: Reg::X9,
        });

        // Load the desired value
        self.emit_mov_to_reg(desired_loc, Reg::X17, size);

        // LL/SC loop for CAS
        let loop_label = self.next_unique_label("cas_loop");
        let fail_label = self.next_unique_label("cas_fail");
        let done_label = self.next_unique_label("cas_done");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: load-acquire exclusive current value
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X16,
        });

        // Compare the current value with the expected one
        self.push_lir(Aarch64Inst::Cmp {
            size: op_size,
            src1: Reg::X16,
            src2: GpOperand::Reg(Reg::X9),
        });

        // If not equal, branch to fail
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Ne,
            target: fail_label.clone(),
        });

        // STLXR: try to store the desired value; status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X17,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry loop if store failed
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32,
            src: Reg::X8,
            target: loop_label,
        });

        // Success: set result to 1
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Imm(1),
            dst: Reg::X16,
        });
        self.push_lir(Aarch64Inst::B {
            target: done_label.clone(),
        });

        // Fail label: CAS failed (value != expected)
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(fail_label)));

        // Store the actual value to *expected_ptr. This is the current
        // value's last use, so the result may reuse its register below.
        self.push_lir(Aarch64Inst::Str {
            size: op_size,
            src: Reg::X16,
            addr: MemAddr::Base(Reg::X11),
        });

        // Set result to 0 (failure)
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Imm(0),
            dst: Reg::X16,
        });

        // Done label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(done_label)));

        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X16) {
            self.emit_move_to_loc(Reg::X16, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic fetch-and-add using LL/SC
    pub(super) fn emit_atomic_fetch_add(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic fetch_add needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load the pointer first, so a value in the same register is
        // still readable when it is loaded next.
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load the addend
        self.emit_mov_to_reg(value_loc, Reg::X9, size);

        // LL/SC loop for fetch_add
        let loop_label = self.next_unique_label("fadd_loop");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: load-acquire exclusive old value
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X11,
        });

        // new = old + addend
        self.push_lir(Aarch64Inst::Add {
            size: op_size,
            src1: Reg::X11,
            src2: GpOperand::Reg(Reg::X9),
            dst: Reg::X16,
        });

        // STLXR: try to store the new value; status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X16,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry if store failed
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32,
            src: Reg::X8,
            target: loop_label,
        });

        // Result: the old value
        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X11) {
            self.emit_move_to_loc(Reg::X11, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic fetch-and-subtract using LL/SC
    pub(super) fn emit_atomic_fetch_sub(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic fetch_sub needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load the pointer first, so a value in the same register is
        // still readable when it is loaded next.
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load subtrahend value into X0
        self.emit_mov_to_reg(value_loc, Reg::X9, size);

        // LL/SC loop for fetch_sub
        let loop_label = self.next_unique_label("fsub_loop");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: load-acquire exclusive old value
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X11,
        });

        // SUB: X2 = X1 (old) - X0 (subtrahend)
        self.push_lir(Aarch64Inst::Sub {
            size: op_size,
            src1: Reg::X11,
            src2: GpOperand::Reg(Reg::X9),
            dst: Reg::X16,
        });

        // STLXR: try to store the new value; status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X16,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry if store failed
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32,
            src: Reg::X8,
            target: loop_label,
        });

        // Result: the old value
        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X11) {
            self.emit_move_to_loc(Reg::X11, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic fetch-and-AND using LL/SC
    pub(super) fn emit_atomic_fetch_and(&mut self, insn: &Instruction) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::And);
    }

    /// Emit atomic fetch-and-OR using LL/SC
    pub(super) fn emit_atomic_fetch_or(&mut self, insn: &Instruction) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::Or);
    }

    /// Emit atomic fetch-and-XOR using LL/SC
    pub(super) fn emit_atomic_fetch_xor(&mut self, insn: &Instruction) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::Xor);
    }

    /// Helper for atomic fetch bitwise operations (AND, OR, XOR)
    /// Uses LL/SC loop with LDAXR/STLXR
    fn emit_atomic_fetch_bitop(&mut self, insn: &Instruction, op: AtomicBitOp) {
        let target = insn.target.expect("atomic fetch bitop needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load the pointer first, so a value in the same register is
        // still readable when it is loaded next.
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load operand value into X0
        self.emit_mov_to_reg(value_loc, Reg::X9, size);

        // LL/SC loop
        let loop_label = self.next_unique_label("atomic_bitop");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: load-acquire exclusive old value
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X11,
        });

        // Apply bitwise operation: X2 = X1 (old) op X0 (operand)
        match op {
            AtomicBitOp::And => {
                self.push_lir(Aarch64Inst::And {
                    size: op_size,
                    src1: Reg::X11,
                    src2: GpOperand::Reg(Reg::X9),
                    dst: Reg::X16,
                });
            }
            AtomicBitOp::Or => {
                self.push_lir(Aarch64Inst::Orr {
                    size: op_size,
                    src1: Reg::X11,
                    src2: GpOperand::Reg(Reg::X9),
                    dst: Reg::X16,
                });
            }
            AtomicBitOp::Xor => {
                self.push_lir(Aarch64Inst::Eor {
                    size: op_size,
                    src1: Reg::X11,
                    src2: GpOperand::Reg(Reg::X9),
                    dst: Reg::X16,
                });
            }
        }

        // STLXR: try to store the new value; status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X16,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry if store failed
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32,
            src: Reg::X8,
            target: loop_label,
        });

        // Result: the old value
        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X11) {
            self.emit_move_to_loc(Reg::X11, &dst_loc, size.max(32));
        }
    }

    pub(super) fn emit_fence(&mut self, insn: &Instruction) {
        use crate::ir::MemoryOrder;

        // Emit appropriate fence based on memory ordering
        match insn.memory_order {
            MemoryOrder::SeqCst | MemoryOrder::AcqRel => {
                // Full barrier
                self.push_lir(Aarch64Inst::Dmb {
                    option: DmbOption::Ish,
                });
            }
            MemoryOrder::Acquire | MemoryOrder::Consume => {
                // Load barrier
                self.push_lir(Aarch64Inst::Dmb {
                    option: DmbOption::Ishld,
                });
            }
            MemoryOrder::Release => {
                // Store barrier
                self.push_lir(Aarch64Inst::Dmb {
                    option: DmbOption::Ishst,
                });
            }
            MemoryOrder::Relaxed => {
                // No fence needed for relaxed
            }
        }

        // Fence has no result value, but set target to 0 if present
        if let Some(target) = insn.target {
            self.locations.set(target, Loc::Imm(0));
        }
    }

    /// Helper to move a value into a register
    fn emit_mov_to_reg(&mut self, loc: Loc, reg: Reg, size: u32) {
        let op_size = OperandSize::from_bits(size);
        match loc {
            Loc::Reg(src) => {
                if src != reg {
                    self.push_lir(Aarch64Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(src),
                        dst: reg,
                    });
                }
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(v as i64),
                    dst: reg,
                });
            }
            ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // Address through the frame pointer, as every other path in
                // this backend does ("FP-relative for alloca safety"). This
                // used its own SP-relative arithmetic, which is wrong the
                // moment anything moves SP: the atomic CAS loop allocates its
                // expected-value slot with alloc_local_temp, whose Alloca does
                // exactly that, and the pointer was then read back 16 bytes
                // off -- from the saved LR slot.
                self.push_lir(Aarch64Inst::Ldr {
                    size: op_size,
                    addr: self.loc_mem(l).unwrap(),
                    dst: reg,
                });
            }
            Loc::Global(name) => {
                self.emit_load_global(&name, reg, op_size);
            }
            // A floating-point value has to cross to the general-purpose file
            // as its bit pattern. Falling through to a zero immediate here is
            // what made every _Atomic float/double operation store 0 -- the
            // x86_64 twin of this function was fixed for exactly that and this
            // one was missed.
            Loc::VReg(v) => {
                self.push_lir(Aarch64Inst::FmovToGp {
                    size: if size <= 32 {
                        FpSize::Single
                    } else {
                        FpSize::Double
                    },
                    src: v,
                    dst: reg,
                });
            }
            Loc::FImm(f, imm_size) => {
                let bits = if imm_size == 16 {
                    super::f64_to_f16_bits(f.to_f64()) as i64
                } else if imm_size == 32 {
                    (f.to_f64() as f32).to_bits() as i64
                } else {
                    f.to_f64().to_bits() as i64
                };
                self.emit_mov_imm(reg, bits, 64);
            }
        }
    }
}
