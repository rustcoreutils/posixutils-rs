//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((cleanup(fn)))`: the call `fn(&var)` on every path out of
// `var`'s scope
//

//! Cleanup lowering.
//!
//! A variable declared with `cleanup(fn)` gets `fn(&var)` called when it goes
//! out of scope, as gcc does it:
//!
//! - innermost scope first, and within one scope in reverse declaration
//!   order;
//! - falling out of a block or a statement expression, after the statement
//!   expression's value is computed;
//! - on `return`, after the returned value is computed with all of its side
//!   effects;
//! - on `break` and `continue`, each time round;
//! - on a `goto` out of the scope, forward or backward.
//!
//! A computed `goto`, an `asm goto`, `longjmp` and `exit` run none, as in
//! gcc.
//!
//! Under `-fexceptions` they also run when the frame is unwound through a
//! call in their scope -- thread cancellation, `pthread_exit`, a C++
//! exception -- as gcc arranges it. Each such call gets a *landing pad*
//! ([`crate::ir::InsnExtra::landing_pad`]), a CFG edge out of its block,
//! which the unwinder takes with the exception object in a register:
//!
//! ```text
//!   call g()  unwind .Lpad          .Lpad:   %e = landingpad
//!   br .Lcont                                  store %e, __eh_exc
//!                                              br .Lchain(b)
//!   .Lchain(b): call cleanup_b(&b)  unwind .Lpad'   (the pad of `a`)
//!               br .Lchain(a)
//!   .Lchain(a): call cleanup_a(&a)
//!               br .Lresume
//!   .Lresume:   _Unwind_Resume(load __eh_exc)
//! ```
//!
//! One chain per variable, shared by every pad in its scope: it runs that
//! variable's cleanup and falls into the chain of the variable declared
//! before it, the last into the one block that resumes the unwind. A call
//! in a cleanup unwinds to the pads of the variables still in force below
//! it, as gcc's do. Every call gets a pad of its own, so that a pad has one
//! predecessor and SSA construction never puts a phi in front of its
//! `landingpad`. A `nothrow` callee needs none. The code generator turns
//! the pads into gcc's LSDA (`arch::codegen::EhTables`).
//!
//! The parser has already built and checked each call
//! ([`crate::parse::ast::InitDeclarator::cleanup`]). The linearizer keeps the
//! calls of the variables in scope on a stack, [`Linearizer::cleanups`], and
//! every exit asks one question -- [`Linearizer::cleanups_kept_by`]: how much
//! of that stack is still in scope where the exit lands? -- and runs the rest
//! through [`Linearizer::run_cleanups_above`].

use super::linearize::{Linearizer, Scope};
use super::{BasicBlockId, Instruction, Opcode, PseudoId};
use crate::parse::ast::{Expr, InitDeclarator, LabelId};
use crate::symbol::SymbolId;
use crate::types::TypeId;

/// The cleanup of one variable in scope.
pub(crate) struct PendingCleanup {
    /// The variable, which a `goto` matches against its label's scopes.
    var: SymbolId,
    /// The call `fn(&var)`.
    call: Expr,
    /// The loop-or-switch and loop nesting it was declared at. A `break` or
    /// `continue` leaves every variable declared at or inside the depth it
    /// jumps out of -- the rule `VlaMark` follows for VLAs.
    break_depth: usize,
    continue_depth: usize,
    /// Under `-fexceptions`, the block that runs this cleanup and then the
    /// ones in force below it during an unwind, once a call has needed it.
    chain: Option<BasicBlockId>,
}

/// The landing-pad state of the function being linearized.
#[derive(Default)]
pub(crate) struct UnwindState {
    /// The hidden local that carries the exception object from the pad
    /// that received it to `_Unwind_Resume`.
    exception: Option<PseudoId>,
    /// The block that resumes the unwind.
    resume: Option<BasicBlockId>,
    /// While a cleanup is being emitted: how many of the cleanups in force,
    /// from the outermost, a call in it unwinds through -- those below it.
    /// `None` everywhere else, where a call unwinds through all of them.
    depth: Option<usize>,
}

/// A way out of one or more scopes.
#[derive(Clone, Copy)]
pub(crate) enum ScopeExit {
    /// `return`: every scope in the function.
    Return,
    /// `break`: the innermost loop or switch.
    Break,
    /// `continue`: the innermost loop's body.
    Continue,
    /// `goto` this label: every scope the jump is in and its label is not.
    Goto(LabelId),
}

impl Linearizer<'_> {
    /// Put the cleanup of `declarator`, if it has one, in force from here to
    /// the end of its scope. Called once the variable is initialized: a jump
    /// out of its initializer leaves nothing to clean up.
    pub(crate) fn register_cleanup(&mut self, declarator: &InitDeclarator) {
        let Some(call) = &declarator.cleanup else {
            return;
        };
        self.cleanups.push(PendingCleanup {
            var: declarator.symbol,
            call: call.clone(),
            break_depth: self.break_targets.len(),
            continue_depth: self.continue_targets.len(),
            chain: None,
        });
    }

    /// Emit the call `insn`, of a callee that does not return when
    /// `noreturn`: given a landing pad when it may unwind through cleanups
    /// in force, and then ending its block, so that nothing the block
    /// computes after the call appears to the optimizer to be available on
    /// the way into the pad.
    pub(crate) fn emit_call(&mut self, mut insn: Instruction, noreturn: bool) {
        let from = self.current_bb.filter(|_| !self.is_terminated());
        let pad = match from {
            Some(from) if insn.extra().may_throw => self.landing_pad(from),
            _ => None,
        };
        insn.extra_mut().landing_pad = pad;
        self.emit(insn);
        if let (Some(from), Some(pad)) = (from, pad) {
            self.link_bb(from, pad);
        }
        if noreturn {
            self.emit_no_return(
                Instruction::new(Opcode::Unreachable).with_type(self.types.void_id),
            );
        } else if let (Some(from), Some(_)) = (from, pad) {
            let cont = self.alloc_bb();
            self.emit(Instruction::br(cont));
            self.link_bb(from, cont);
            self.switch_bb(cont);
        }
    }

    /// A landing pad for a call in block `from` that may unwind through the
    /// cleanups in force, or `None` when there are none to run: a block that
    /// takes the exception object and enters the chain of the innermost.
    fn landing_pad(&mut self, from: BasicBlockId) -> Option<BasicBlockId> {
        let depth = self.unwind.depth.unwrap_or(self.cleanups.len());
        let innermost = depth.checked_sub(1)?;
        let chain = self.cleanup_chain(innermost);
        let slot = self.exception_slot();
        let void_ptr = self.types.void_ptr_id;
        let bits = self.ptr_bits();
        let pad = self.alloc_bb();
        self.switch_bb(pad);
        let exc = self.alloc_reg_pseudo();
        self.emit(
            Instruction::new(Opcode::LandingPad)
                .with_target(exc)
                .with_type(void_ptr)
                .with_size(bits),
        );
        self.emit(Instruction::store(exc, slot, 0, void_ptr, bits));
        self.emit(Instruction::br(chain));
        self.link_bb(pad, chain);
        self.current_bb = Some(from);
        Some(pad)
    }

    /// The block that runs cleanup `i` during an unwind and goes on to the
    /// ones below it, made the first time a pad needs it.
    fn cleanup_chain(&mut self, i: usize) -> BasicBlockId {
        if let Some(chain) = self.cleanups[i].chain {
            return chain;
        }
        let chain = self.alloc_bb();
        self.cleanups[i].chain = Some(chain);
        let saved = (self.current_bb, self.unwind.depth);
        self.switch_bb(chain);
        self.unwind.depth = Some(i);
        let call = self.cleanups[i].call.clone();
        self.linearize_expr(&call);
        let next = match i.checked_sub(1) {
            Some(below) => self.cleanup_chain(below),
            None => self.resume_block(),
        };
        if let Some(end) = self.current_bb.filter(|_| !self.is_terminated()) {
            self.emit(Instruction::br(next));
            self.link_bb(end, next);
        }
        (self.current_bb, self.unwind.depth) = saved;
        chain
    }

    /// The block that hands the exception object back to the unwinder,
    /// `_Unwind_Resume(exception)`, made the first time a chain needs it.
    ///
    /// The call may itself unwind through the cleanups of a caller this
    /// function is inlined into, so it is a call that may throw.
    fn resume_block(&mut self) -> BasicBlockId {
        if let Some(resume) = self.unwind.resume {
            return resume;
        }
        let slot = self.exception_slot();
        let void_ptr = self.types.void_ptr_id;
        let bits = self.ptr_bits();
        let resume = self.alloc_bb();
        let saved = self.current_bb;
        self.switch_bb(resume);
        let exc = self.alloc_reg_pseudo();
        self.emit(Instruction::load(exc, slot, 0, void_ptr, bits));
        let mut call = Instruction::call_with_abi(
            None,
            "_Unwind_Resume",
            vec![exc],
            vec![void_ptr],
            self.types.void_id,
            crate::abi::CallingConv::C,
            self.types,
            self.target,
        );
        let extra = call.extra_mut();
        extra.is_noreturn_call = true;
        extra.may_throw = true;
        extra.callee_binding = crate::parse::ast::CalleeBinding::Library;
        self.emit(call);
        self.emit(Instruction::new(Opcode::Unreachable).with_type(self.types.void_id));
        self.current_bb = saved;
        self.unwind.resume = Some(resume);
        resume
    }

    /// The hidden local the exception object travels in, from the pad that
    /// received it to `_Unwind_Resume`; SSA promotes it.
    fn exception_slot(&mut self) -> PseudoId {
        if let Some(slot) = self.unwind.exception {
            return slot;
        }
        let slot = self.alloc_pseudo();
        let name = format!("__eh_exception.{}", slot.0);
        self.named_local(slot, name, self.types.void_ptr_id, None, None);
        self.unwind.exception = Some(slot);
        slot
    }

    /// Emit the `Ret` `insn`, once every cleanup in force has run. The value
    /// it returns is already computed, side effects and all.
    pub(crate) fn emit_return(&mut self, insn: Instruction) {
        self.leave_scopes(ScopeExit::Return);
        self.emit(insn);
    }

    /// `value`, a value of type `typ`, made safe from the cleanups above the
    /// first `kept`: a value the IR hands around as the address of its
    /// storage is copied out first when one of those could change it
    /// ([`Linearizer::detach_value`]), as gcc copies a returned variable
    /// before its own cleanup runs.
    pub(crate) fn outlive_cleanups(
        &mut self,
        value: PseudoId,
        typ: TypeId,
        kept: usize,
    ) -> PseudoId {
        if self.cleanups.len() <= kept {
            return value;
        }
        self.detach_value(value, typ)
    }

    /// Run the cleanup of every variable `exit` leaves the scope of.
    pub(crate) fn leave_scopes(&mut self, exit: ScopeExit) {
        let kept = self.cleanups_kept_by(exit);
        self.run_cleanups_above(kept);
    }

    /// How many of the cleanups in force, from the outermost, are still in
    /// scope where `exit` lands. The one place that decides which cleanups an
    /// exit runs.
    fn cleanups_kept_by(&self, exit: ScopeExit) -> usize {
        let first_left = |depth_of: fn(&PendingCleanup) -> usize, depth: usize| {
            self.cleanups
                .iter()
                .position(|c| depth_of(c) >= depth)
                .unwrap_or(self.cleanups.len())
        };
        match exit {
            ScopeExit::Return => 0,
            ScopeExit::Break => first_left(|c| c.break_depth, self.break_targets.len()),
            ScopeExit::Continue => first_left(|c| c.continue_depth, self.continue_targets.len()),
            // Scopes nest, so the label's variables are a prefix of the
            // jump's: everything past the shared prefix is left.
            ScopeExit::Goto(label) => {
                let at_label = self.label_cleanups.get(&label).map_or(&[][..], |v| v);
                self.cleanups
                    .iter()
                    .zip(at_label)
                    .take_while(|(c, var)| c.var == **var)
                    .count()
            }
        }
    }

    /// Emit the cleanups above the first `kept`, innermost first. They stay
    /// in force: the scope that declared each one still runs it on its own
    /// way out.
    ///
    /// A call in cleanup `i` unwinds through the cleanups below it alone.
    fn run_cleanups_above(&mut self, kept: usize) {
        let depth = self.unwind.depth;
        for i in (kept..self.cleanups.len()).rev() {
            if self.current_bb.is_none() || self.is_terminated() {
                break;
            }
            let call = self.cleanups[i].call.clone();
            self.unwind.depth = Some(i);
            self.linearize_expr(&call);
        }
        self.unwind.depth = depth;
    }

    /// Run, on the path falling out of `scope`, the cleanups of the variables
    /// it declared, and take them out of force.
    ///
    /// Called only from [`Linearizer::pop_scope`], so that leaving a scope
    /// and running its cleanups are the same act.
    pub(crate) fn close_cleanup_scope(&mut self, scope: &Scope) {
        let entry = scope.cleanup_entry;
        if self.cleanups.len() <= entry {
            return;
        }
        self.run_cleanups_above(entry);
        self.cleanups.truncate(entry);
    }
}
