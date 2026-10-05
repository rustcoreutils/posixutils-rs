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
//! The parser has already built and checked each call
//! ([`crate::parse::ast::InitDeclarator::cleanup`]). The linearizer keeps the
//! calls of the variables in scope on a stack, [`Linearizer::cleanups`], and
//! every exit asks one question -- [`Linearizer::cleanups_kept_by`]: how much
//! of that stack is still in scope where the exit lands? -- and runs the rest
//! through [`Linearizer::run_cleanups_above`].

use super::linearize::{Linearizer, Scope};
use super::{Instruction, PseudoId};
use crate::parse::ast::{Expr, InitDeclarator};
use crate::strings::StringId;
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
    /// `goto` the label of this name: every scope it is in and its label is
    /// not.
    Goto(StringId),
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
        });
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
    fn run_cleanups_above(&mut self, kept: usize) {
        for i in (kept..self.cleanups.len()).rev() {
            if self.current_bb.is_none() || self.is_terminated() {
                return;
            }
            let call = self.cleanups[i].call.clone();
            self.linearize_expr(&call);
        }
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
