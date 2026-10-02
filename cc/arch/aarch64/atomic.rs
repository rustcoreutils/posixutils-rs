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
use crate::ir::{Instruction, MemoryOrder, Opcode};

/// How an atomic access is ordered on AArch64 without LSE: which of its
/// halves carry the acquire and the release.
///
/// This is the one mapping from a memory order to instructions, and it is
/// gcc's (`-mno-outline-atomics`): an acquiring order makes the load an
/// acquire (`ldar`, `ldaxr`) and a releasing one makes the store a release
/// (`stlr`, `stlxr`); relaxed is the plain `ldr`, `str`, `ldxr`, `stxr`.
/// Seq-cst needs nothing beyond acq-rel, because ARMv8 never lets a
/// load-acquire pass an earlier store-release.
#[derive(Clone, Copy)]
struct Ordering {
    acquire: bool,
    release: bool,
}

impl Ordering {
    fn of(order: MemoryOrder) -> Self {
        Self {
            acquire: order.acquires(),
            release: order.releases(),
        }
    }
}

/// The barrier a thread fence at `order` needs, as gcc picks it: `dmb
/// ishld` orders only earlier loads, which is all an acquire fence needs.
/// A release fence must order earlier loads *and* stores before later
/// stores, which `dmb ishst` (stores only) does not do, so it is a full
/// `dmb ish`.
fn fence_barrier(order: MemoryOrder) -> Option<DmbOption> {
    match order {
        MemoryOrder::Relaxed => None,
        MemoryOrder::Consume | MemoryOrder::Acquire => Some(DmbOption::Ishld),
        MemoryOrder::Release | MemoryOrder::AcqRel | MemoryOrder::SeqCst => Some(DmbOption::Ish),
    }
}

/// What a read-modify-write stores, computed from the old value in X11 and
/// the operand in X9.
#[derive(Clone, Copy)]
enum RmwOp {
    Swap,
    Add,
    Sub,
    And,
    Or,
    Xor,
}

impl RmwOp {
    fn of(op: Opcode) -> Self {
        match op {
            Opcode::AtomicSwap => Self::Swap,
            Opcode::AtomicFetchAdd => Self::Add,
            Opcode::AtomicFetchSub => Self::Sub,
            Opcode::AtomicFetchAnd => Self::And,
            Opcode::AtomicFetchOr => Self::Or,
            Opcode::AtomicFetchXor => Self::Xor,
            _ => unreachable!("{op:?} is not an atomic read-modify-write"),
        }
    }
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

        self.emit_addr_into(addr, insn.displacement(), Reg::X10);

        let addr = MemAddr::Base(Reg::X10);
        let (size, dst) = (op_size, Reg::X9);
        self.push_lir(if Ordering::of(insn.extra().memory_order).acquire {
            Aarch64Inst::Ldar { size, addr, dst }
        } else {
            Aarch64Inst::Ldr { size, addr, dst }
        });

        self.move_atomic_result(Reg::X9, target, insn.size);
    }

    pub(super) fn emit_atomic_store(&mut self, insn: &Instruction) {
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let value_loc = self.get_location(value);

        // Load the pointer first, so a value in the same register is
        // still readable when it is loaded next.
        self.emit_addr_into(addr, insn.displacement(), Reg::X10);

        // Load the value
        self.emit_mov_to_reg(value_loc, Reg::X9, size);

        let addr = MemAddr::Base(Reg::X10);
        let (size, src) = (op_size, Reg::X9);
        self.push_lir(if Ordering::of(insn.extra().memory_order).release {
            Aarch64Inst::Stlr { size, src, addr }
        } else {
            Aarch64Inst::Str { size, src, addr }
        });

        // Atomic store has no result value
        if let Some(target) = insn.target {
            self.locations.set(target, Loc::Imm(0));
        }
    }

    /// Emit an exchange or a fetch-and-op as an LL/SC loop: load-exclusive
    /// the old value, compute the new one, store-exclusive it, and retry
    /// until the store succeeds. The result is the old value.
    pub(super) fn emit_atomic_rmw(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic read-modify-write needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);
        let ordering = Ordering::of(insn.extra().memory_order);

        let value_loc = self.get_location(value);

        // Load the pointer first, so a value in the same register is
        // still readable when it is loaded next.
        self.emit_addr_into(addr, insn.displacement(), Reg::X10);

        // Load the operand
        self.emit_mov_to_reg(value_loc, Reg::X9, size);

        let loop_label = self.next_unique_label("atomic_rmw");
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        self.push_lir(Aarch64Inst::Ldxr {
            acquire: ordering.acquire,
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X11,
        });

        let new = self.emit_rmw_compute(RmwOp::of(insn.op), op_size);

        // Try to store the new value; status in W8
        self.push_lir(Aarch64Inst::Stxr {
            release: ordering.release,
            size: op_size,
            src: new,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // Retry if the store failed (status != 0)
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32, // Status is always 32-bit
            src: Reg::X8,
            target: loop_label,
        });

        self.move_atomic_result(Reg::X11, target, size);
    }

    /// Compute the value a read-modify-write stores from the old value in
    /// X11 and the operand in X9, returning the register holding it.
    fn emit_rmw_compute(&mut self, op: RmwOp, size: OperandSize) -> Reg {
        let (src1, src2, dst) = (Reg::X11, GpOperand::Reg(Reg::X9), Reg::X16);
        self.push_lir(match op {
            RmwOp::Swap => return Reg::X9,
            RmwOp::Add => Aarch64Inst::Add {
                size,
                src1,
                src2,
                dst,
            },
            RmwOp::Sub => Aarch64Inst::Sub {
                size,
                src1,
                src2,
                dst,
            },
            RmwOp::And => Aarch64Inst::And {
                size,
                src1,
                src2,
                dst,
            },
            RmwOp::Or => Aarch64Inst::Orr {
                size,
                src1,
                src2,
                dst,
            },
            RmwOp::Xor => Aarch64Inst::Eor {
                size,
                src1,
                src2,
                dst,
            },
        });
        dst
    }

    /// Move an atomic's result from the fixed scratch register `reg` to
    /// where the allocator placed `target`.
    ///
    /// Overwriting the pseudo's location with the scratch register made
    /// every atomic result alias the same register, so two atomic reads in
    /// one expression collapsed into one -- `x + y` on two _Atomic ints
    /// returned `y + y`.
    fn move_atomic_result(&mut self, reg: Reg, target: crate::ir::PseudoId, size: u32) {
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == reg) {
            self.emit_move_to_loc(reg, &dst_loc, size.max(32));
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
        // The success and failure orders were combined into this one by the
        // linearizer (`cas_order`); a failed attempt runs the same load.
        let ordering = Ordering::of(insn.extra().memory_order);

        let desired_loc = self.get_location(desired);

        // Load pointer to atomic variable into X10 FIRST
        // (before the other loads, so none of them can clobber it)
        self.emit_addr_into(addr, insn.displacement(), Reg::X10);

        // Load expected_ptr (pointer to expected value) into X11
        // Then load the expected value from that address into X9
        self.emit_addr_into(expected_ptr, 0, Reg::X11);
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

        // Load-exclusive the current value
        self.push_lir(Aarch64Inst::Ldxr {
            acquire: ordering.acquire,
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

        // Try to store the desired value; status in W8
        self.push_lir(Aarch64Inst::Stxr {
            release: ordering.release,
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

        self.move_atomic_result(Reg::X16, target, size);
    }

    pub(super) fn emit_fence(&mut self, insn: &Instruction) {
        if let Some(option) = insn.hardware_fence_order().and_then(fence_barrier) {
            self.push_lir(Aarch64Inst::Dmb { option });
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
                // moment anything moves SP -- a VLA or `alloca` in the same
                // function -- and a pointer was then read back 16 bytes off,
                // from the saved LR slot.
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
                let bits = f.to_bits_at_width(imm_size);
                self.emit_mov_imm(reg, bits, 64);
            }
        }
    }
}
