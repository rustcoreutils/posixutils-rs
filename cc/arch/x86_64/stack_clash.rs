//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 `-fstack-clash-protection`: the stack pointer never moves past
// the guard page without a store touching it. gcc's scheme, laid out at
// `target::StackClash`.
//
// The call's return address is the probe a function starts from. Its frame
// is allocated an interval at a time with a probe at each new stack pointer
// (`ix86_adjust_stack_and_probe`), leaving the last part, under an
// interval, unprobed. Every later move probes at the top of each interval
// it uncovers -- eight bytes under where the stack pointer stood -- which
// is safe however much the frame left unprobed, as gcc's `alloca` loop
// does, and ends with a probe at the new stack pointer. The CFA is `%rbp`
// based from the second instruction of every function on, so none of the
// moves need a CFI rule.
//

use crate::arch::lir::{CondCode, Directive, Label, OperandSize};
use crate::arch::x86_64::codegen::X86_64CodeGen;
use crate::arch::x86_64::lir::{GpOperand, MemAddr, X86Inst};
use crate::arch::x86_64::regalloc::Reg;
use crate::target::ProbePlan;

impl X86_64CodeGen {
    /// The protection's probe interval, which must only be asked for while
    /// it is on.
    fn probe_interval(&self) -> i64 {
        let stack_clash = self.base.stack_clash.expect("stack clash protection");
        stack_clash.interval(self.base.target.arch)
    }

    /// A probe of the stack at `offset` from `%rsp`.
    fn probe_at(&mut self, offset: i64) {
        self.push_lir(X86Inst::StackProbe {
            addr: MemAddr::BaseOffset {
                base: Reg::Rsp,
                offset,
            },
        });
    }

    fn sub_rsp(&mut self, bytes: i64) {
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Imm(bytes),
            dst: Reg::Rsp,
        });
    }

    /// Allocate the `bytes` of the frame below the pushed registers, with
    /// `%rsp` probed up to here: one `subq`, or, once the frame reaches the
    /// guard, gcc's prologue probes -- an interval at a time, each probed at
    /// the new `%rsp`, four written out and a loop down to `%r11` past
    /// that, and the residual left unprobed.
    pub(super) fn emit_frame_allocation(&mut self, bytes: i64) {
        let Some(plan) = self.base.frame_probe_plan(bytes) else {
            if bytes > 0 {
                self.sub_rsp(bytes);
            }
            self.unprobed_frame = bytes;
            return;
        };
        let interval = self.probe_interval();
        match plan {
            ProbePlan::Unrolled { steps, .. } => {
                for _ in 0..steps {
                    self.sub_rsp(interval);
                    self.probe_at(0);
                }
            }
            ProbePlan::Loop { rounded, .. } => {
                // R11 is free in a prologue: no argument arrives in it.
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rsp,
                        offset: -rounded,
                    },
                    dst: Reg::R11,
                });
                let top = self.probe_label("probe_frame");
                self.push_lir(X86Inst::Directive(Directive::BlockLabel(top.clone())));
                self.sub_rsp(interval);
                self.probe_at(0);
                self.push_lir(X86Inst::Cmp {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R11),
                    dst: GpOperand::Reg(Reg::Rsp),
                });
                self.push_lir(X86Inst::Jcc {
                    cc: CondCode::Ne,
                    target: top,
                });
            }
        }
        let residual = plan.residual();
        if residual > 0 {
            self.sub_rsp(residual);
        }
        self.unprobed_frame = residual;
    }

    /// Round `%rsp` down to `align`, probed, where the prologue has
    /// probed `%rsp` itself: the `andq` an over-aligned frame makes, which
    /// moves the stack pointer by up to the alignment -- past the guard, for
    /// an alignment past it -- made as a probed move instead.
    pub(super) fn emit_probed_realignment(&mut self, align: i32) {
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rsp),
            dst: GpOperand::Reg(Reg::R11),
        });
        self.push_lir(X86Inst::And {
            size: OperandSize::B64,
            src: GpOperand::Imm(-i64::from(align)),
            dst: Reg::R11,
        });
        self.emit_probed_descent_to_r11();
    }

    /// Move `%rsp` down to `%r11`, probing as it goes: an interval at a
    /// time, each probed at its top, then what is left, probed at its top,
    /// and the new `%rsp`. Clobbers R10 and the flags. The probes at the
    /// tops are what make it safe wherever the last probe above `%rsp` was,
    /// up to the guard; the one at the bottom leaves nothing unprobed.
    pub(super) fn emit_probed_descent_to_r11(&mut self) {
        let interval = self.probe_interval();
        let top = self.probe_label("probe_loop");
        let done = self.probe_label("probe_done");
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(top.clone())));
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rsp),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B64,
            src: GpOperand::Imm(interval),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Ult,
            target: done.clone(),
        });
        self.sub_rsp(interval);
        self.probe_at(interval - 8);
        self.push_lir(X86Inst::Jmp { target: top });
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done)));
        // The top of what is left: under `%rsp`, which a probe that changes
        // nothing may touch even when nothing is left.
        self.probe_at(-8);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Reg(Reg::Rsp),
        });
        self.probe_at(0);
    }

    /// Move `%rsp` down by the `bytes` in R10, for `alloca`: probed under
    /// the protection, every dynamic allocation being one that might jump
    /// the guard.
    pub(super) fn emit_dynamic_allocation(&mut self) {
        if self.base.stack_clash.is_none() {
            self.push_lir(X86Inst::Sub {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R10),
                dst: Reg::Rsp,
            });
            return;
        }
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rsp),
            dst: GpOperand::Reg(Reg::R11),
        });
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: Reg::R11,
        });
        self.emit_probed_descent_to_r11();
    }

    /// Reserve a call's outgoing argument area of `bytes`. It needs probes
    /// once the stores into it, or the call's own return address below it,
    /// could land past the guard: when it and what the prologue left
    /// unprobed above it reach the guard. (gcc pushes its arguments, each
    /// push a probe of its own, and reserves a large aggregate's area
    /// without one.)
    pub(super) fn emit_call_area(&mut self, bytes: i64) {
        let probed = self.base.stack_clash.is_some_and(|stack_clash| {
            let guard = stack_clash.guard_bytes(self.base.target.arch);
            self.unprobed_frame + bytes + 8 > guard
        });
        if !probed {
            self.sub_rsp(bytes);
            return;
        }
        self.push_lir(X86Inst::Lea {
            addr: MemAddr::BaseOffset {
                base: Reg::Rsp,
                offset: -bytes,
            },
            dst: Reg::R11,
        });
        self.emit_probed_descent_to_r11();
    }

    fn probe_label(&mut self, prefix: &str) -> Label {
        let id = self.unique_label_counter;
        self.unique_label_counter += 1;
        Label::internal(prefix, id)
    }
}
