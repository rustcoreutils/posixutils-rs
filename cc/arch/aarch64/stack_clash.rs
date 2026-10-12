//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// aarch64 `-fstack-clash-protection`: the stack pointer never moves past
// the guard page without a store touching it. gcc's scheme
// (`aarch64_allocate_and_probe_stack_space`), laid out at
// `target::StackClash`.
//
// `bl` stores nothing, so a function may be entered with up to 1 KiB
// below its caller's last probe unprobed (`AARCH64_CALLER_GUARD`). The
// frame record a prologue stores at the bottom of the frame probes any
// frame under the guard less that; a larger one is allocated an interval
// at a time, each probed 1 KiB above the new `sp` -- a store that only
// reaches memory the step uncovered -- and its residual is probed the same
// way when it alone could jump the guard. In the body, which addresses its
// frame from x29, every move that could take `sp` more than 1 KiB below
// the last probe ends with a probe at the new `sp`: `alloca`, and a call's
// stacked arguments past 1 KiB.
//

use crate::arch::aarch64::codegen::Aarch64CodeGen;
use crate::arch::aarch64::lir::{Aarch64Inst, GpOperand, MemAddr};
use crate::arch::aarch64::regalloc::Reg;
use crate::arch::lir::{CondCode, Directive, OperandSize};
use crate::target::{ProbePlan, StackClash};

/// Where each interval's probe goes: the caller's allowance above the new
/// `sp`.
const STEP_PROBE: i32 = StackClash::AARCH64_CALLER_GUARD as i32;

impl Aarch64CodeGen {
    /// The protection in force, which must only be asked for while it is on.
    fn stack_clash(&self) -> StackClash {
        self.base.stack_clash.expect("stack clash protection")
    }

    /// `str xzr, [sp, #offset]`: a probe.
    fn probe_at(&mut self, offset: i32) {
        self.push_lir(Aarch64Inst::Str {
            size: OperandSize::B64,
            src: Reg::Xzr,
            addr: if offset == 0 {
                MemAddr::Base(Reg::SP)
            } else {
                MemAddr::BaseOffset {
                    base: Reg::SP,
                    offset,
                }
            },
        });
    }

    fn sub_sp(&mut self, bytes: i64) {
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::SP,
            src2: GpOperand::Imm(bytes),
            dst: Reg::SP,
        });
    }

    /// Move `sp` down `plan`'s whole intervals, each probed 1 KiB up, the
    /// CFA being `cfa` bytes above `sp` beforehand when the move is a
    /// prologue's and has to be described; then its residual, probed 1 KiB
    /// up when it is too large for a probe at the new `sp` to follow
    /// within the guard. Returns the CFA's offset after. The loop counts
    /// down to X17, which is free at a prologue and at a call's argument
    /// setup, and describes the CFA from it while `sp` moves.
    fn emit_probe_steps(&mut self, plan: ProbePlan, cfa: Option<i32>) -> Option<i32> {
        let stack_clash = self.stack_clash();
        let arch = self.base.target.arch;
        let interval = stack_clash.interval(arch);
        let mut cfa = cfa;
        match plan {
            ProbePlan::Unrolled { steps, .. } => {
                for _ in 0..steps {
                    cfa = self.lower_sp(interval, cfa);
                    self.probe_at(STEP_PROBE);
                }
            }
            ProbePlan::Loop { rounded, .. } => {
                self.push_lir(Aarch64Inst::Sub {
                    size: OperandSize::B64,
                    src1: Reg::SP,
                    src2: GpOperand::Imm(rounded),
                    dst: Reg::X17,
                });
                if let Some(c) = cfa {
                    self.push_cfi(Directive::cfi_def_cfa("x17", c + rounded as i32));
                }
                let top = self.next_unique_label("probe_frame");
                self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(top.clone())));
                self.sub_sp(interval);
                self.probe_at(STEP_PROBE);
                self.push_lir(Aarch64Inst::Cmp {
                    size: OperandSize::B64,
                    src1: Reg::SP,
                    src2: GpOperand::Reg(Reg::X17),
                });
                self.push_lir(Aarch64Inst::BCond {
                    cond: CondCode::Ne,
                    target: top,
                });
                if let Some(c) = cfa {
                    let after = c + rounded as i32;
                    self.push_cfi(Directive::cfi_def_cfa("sp", after));
                    cfa = Some(after);
                }
            }
        }
        let residual = plan.residual();
        if residual > 0 {
            cfa = self.lower_sp(residual, cfa);
            if residual >= stack_clash.probed_from(arch) {
                self.probe_at(STEP_PROBE);
            }
        }
        cfa
    }

    /// `sp` down by `bytes`, described when `cfa` is known.
    fn lower_sp(&mut self, bytes: i64, cfa: Option<i32>) -> Option<i32> {
        match cfa {
            Some(c) => {
                self.emit_sp_adjust(-(bytes as i32), c);
                Some(c + bytes as i32)
            }
            None => {
                self.sub_sp(bytes);
                None
            }
        }
    }

    /// Allocate a frame of `total_frame` bytes in the prologue, whose CFA
    /// is `sp` on entry: one move, or under the protection, once the frame
    /// reaches the guard less the caller's 1 KiB, gcc's probes. The frame
    /// record the prologue stores next, at the new `sp`, is the last probe.
    pub(super) fn emit_frame_allocation(&mut self, total_frame: i32) {
        match self.base.frame_probe_plan(i64::from(total_frame)) {
            None => self.emit_sp_adjust(-total_frame, 0),
            Some(plan) => {
                self.emit_probe_steps(plan, Some(0));
            }
        }
    }

    /// Move `sp` down the `bytes` of a frame below a record the prologue has
    /// already stored at its top, x29 pointing at it: probed as gcc probes
    /// a frame, the CFA staying x29-based so no move needs describing. The
    /// bottom record stored next is the last probe.
    pub(super) fn emit_frame_body_allocation(&mut self, bytes: i64) {
        match self.base.frame_probe_plan(bytes) {
            None => self.sub_sp(bytes),
            Some(plan) => {
                self.emit_probe_steps(plan, None);
            }
        }
    }

    /// Move `sp` down a call's outgoing argument area of `bytes`. Up to
    /// 1 KiB is the allowance a callee assumes and needs no probe; past it,
    /// under the protection, the area is probed as an `alloca` of its size
    /// is, ending with a probe at the new `sp`.
    pub(super) fn emit_call_area(&mut self, bytes: i64) {
        if self.base.stack_clash.is_none() || bytes <= StackClash::AARCH64_CALLER_GUARD {
            self.sub_sp(bytes);
            return;
        }
        self.emit_probed_lowering(bytes);
    }

    /// Probe the `bytes` below `sp` without moving it, ahead of a move by up
    /// to that much whose amount only the run knows: the realignment of a
    /// call's argument area.
    pub(super) fn emit_probe_ahead(&mut self, bytes: i64) {
        if self.base.stack_clash.is_none() || bytes <= StackClash::AARCH64_CALLER_GUARD {
            return;
        }
        self.emit_probed_lowering(bytes);
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: Reg::SP,
            src2: GpOperand::Imm(bytes),
            dst: Reg::SP,
        });
    }

    /// `sp` down by `bytes`, probed, the last probe at the new `sp`.
    fn emit_probed_lowering(&mut self, bytes: i64) {
        let plan = self.stack_clash().steps(self.base.target.arch, bytes);
        self.emit_probe_steps(plan, None);
        self.probe_at(0);
    }

    /// Move `sp` down by the bytes in X9, a multiple of sixteen, for
    /// `alloca`: probed under the protection, every dynamic allocation
    /// being one that might jump the guard. Clobbers X9, X10, X15 and the
    /// flags.
    pub(super) fn emit_dynamic_allocation(&mut self) {
        if self.base.stack_clash.is_none() {
            self.push_lir(Aarch64Inst::Sub {
                size: OperandSize::B64,
                src1: Reg::SP,
                src2: GpOperand::Reg(Reg::X9),
                dst: Reg::SP,
            });
            return;
        }
        let stack_clash = self.stack_clash();
        let arch = self.base.target.arch;
        let interval = stack_clash.interval(arch);
        let done = self.next_unique_label("probe_done");
        let top = self.next_unique_label("probe_loop");
        let rest = self.next_unique_label("probe_rest");
        let bottom = self.next_unique_label("probe_bottom");
        // Nothing to allocate: nothing to probe, and `[sp]` is not ours.
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X9,
            src2: GpOperand::Imm(0),
        });
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Eq,
            target: done.clone(),
        });
        // X10: where the whole intervals end. X9: what is left under one.
        self.push_lir(Aarch64Inst::And {
            size: OperandSize::B64,
            src1: Reg::X9,
            src2: GpOperand::Imm(-interval),
            dst: Reg::X10,
        });
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::SP,
            src2: GpOperand::Reg(Reg::X10),
            dst: Reg::X10,
        });
        self.push_lir(Aarch64Inst::And {
            size: OperandSize::B64,
            src1: Reg::X9,
            src2: GpOperand::Imm(interval - 1),
            dst: Reg::X9,
        });
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(top.clone())));
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::SP,
            src2: GpOperand::Reg(Reg::X10),
        });
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Eq,
            target: rest.clone(),
        });
        self.sub_sp(interval);
        self.probe_at(STEP_PROBE);
        self.push_lir(Aarch64Inst::B { target: top });
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(rest)));
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::SP,
            src2: GpOperand::Reg(Reg::X9),
            dst: Reg::SP,
        });
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X9,
            src2: GpOperand::Imm(stack_clash.probed_from(arch)),
        });
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Ult,
            target: bottom.clone(),
        });
        self.probe_at(STEP_PROBE);
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(bottom)));
        self.probe_at(0);
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(done)));
    }
}
