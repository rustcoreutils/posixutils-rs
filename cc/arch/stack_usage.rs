//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fstack-usage` and `-Wstack-usage=`: the stack each function takes,
// counted as gcc counts it, so that a c17 build and a gcc build can be
// compared function by function.
//
// gcc's measure is every byte the stack pointer moves below the caller's
// before anything is allocated at run time. On x86-64 that starts with the
// return address the call pushed; on AArch64 the return address is in a
// register and the count is the frame the prologue allocates. Each backend
// says what its prologue took (`emit_function`); the moves its body makes
// below that -- outgoing argument areas, and pushes it pops again -- are
// tallied by `CodeGenBase::sp_lowered` and `sp_raised` as they are emitted.
//

use crate::diag::Position;
use crate::ir::{Function, Opcode};

/// How far a function's use of the stack is known, in gcc's words.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bound {
    /// The prologue's frame is all there is.
    Static,
    /// The body also moves the stack pointer, by amounts known when it is
    /// compiled, and the largest is counted.
    Bounded,
    /// The function allocates at run time: `alloca`, or a variable-length
    /// array. Its count is a floor.
    Dynamic,
}

impl Bound {
    /// The qualifier a `.su` record ends with.
    pub fn qualifier(self) -> &'static str {
        match self {
            Bound::Static => "static",
            Bound::Bounded => "dynamic,bounded",
            Bound::Dynamic => "dynamic",
        }
    }
}

/// One function's stack use.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackUsage {
    /// The function's C name.
    pub name: String,
    /// Where its declarator names it.
    pub pos: Position,
    /// The bytes it takes, by gcc's measure.
    pub bytes: u64,
    pub bound: Bound,
}

impl StackUsage {
    /// The function's line of a `.su` file, as gcc writes it:
    /// `file:line:column:name<TAB>bytes<TAB>qualifier`. The file is the
    /// one a `#line` names, spelled as the command line or `#include` gave
    /// it, and the column is counted in bytes, a tab being one.
    pub fn record(&self) -> String {
        let (file, line, _) = crate::diag::effective_position(self.pos);
        let column = u32::from(self.pos.line_offset) + 1;
        format!(
            "{file}:{line}:{column}:{}\t{}\t{}",
            self.name,
            self.bytes,
            self.bound.qualifier()
        )
    }

    /// `-Wstack-usage=`: warn, in gcc's words and at the function's name,
    /// when the use is over the limit or has none.
    pub fn check_limit(&self) {
        let Some(limit) = crate::diag::stack_usage_warns_above() else {
            return;
        };
        let bytes = self.bytes.to_string();
        let warn = |template: &str| {
            crate::diag::group_warning_args(
                crate::diag::STACK_USAGE_WARNING,
                self.pos,
                template,
                &[&bytes],
            )
        };
        match self.bound {
            Bound::Dynamic => warn("stack usage might be unbounded"),
            _ if self.bytes <= limit => {}
            Bound::Bounded => warn("stack usage might be {0} bytes"),
            Bound::Static => warn("stack usage is {0} bytes"),
        }
    }
}

/// Whether `func` allocates stack at run time: `alloca` and variable-length
/// arrays, which are all `Opcode::Alloca` by now.
pub fn allocates_at_run_time(func: &Function) -> bool {
    func.blocks
        .iter()
        .flat_map(|b| &b.insns)
        .any(|i| i.op == Opcode::Alloca)
}

/// How far below the prologue's frame the body of the function being
/// emitted has moved the stack pointer: by how much now, and at most.
#[derive(Debug, Clone, Copy, Default)]
pub struct Excursion {
    now: i64,
    deepest: i64,
}

impl Excursion {
    /// The stack pointer moves `bytes` further down.
    pub fn lowered(&mut self, bytes: i64) {
        self.now += bytes;
        self.deepest = self.deepest.max(self.now);
    }

    /// The stack pointer moves `bytes` back up.
    pub fn raised(&mut self, bytes: i64) {
        self.now -= bytes;
    }

    /// The stack pointer is back at the bottom of the frame -- restored
    /// from a saved copy after a call whose argument area was realigned.
    pub fn restored(&mut self) {
        self.now = 0;
    }

    /// The function's stack use, its prologue having taken `frame_bytes`,
    /// with the function named as the source declared it; `None` for one it
    /// did not declare, which gcc's records cannot name. The excursion
    /// starts over for the next function.
    pub fn finish(&mut self, func: &Function, frame_bytes: i64) -> Option<StackUsage> {
        let Excursion {
            now,
            deepest: below,
        } = std::mem::take(self);
        // Every move the body reported, it also reported undone.
        debug_assert_eq!(now, 0, "unbalanced stack pointer moves in {}", func.name);
        let bound = if allocates_at_run_time(func) {
            Bound::Dynamic
        } else if below > 0 {
            Bound::Bounded
        } else {
            Bound::Static
        };
        let declared = func.declared.as_ref()?;
        Some(StackUsage {
            name: declared.name.clone(),
            pos: declared.pos,
            bytes: u64::try_from(frame_bytes + below).unwrap_or(0),
            bound,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn declared(name: &str) -> Function {
        let mut func = Function::default();
        func.declared = Some(crate::ir::FunctionDecl {
            name: name.to_string(),
            pos: Position::default(),
        });
        func
    }

    /// The deepest point counts, not the last, and a balanced push inside
    /// an argument area adds to it; the next function starts from nothing.
    #[test]
    fn excursion_keeps_the_deepest_point() {
        let func = declared("f");
        let mut e = Excursion::default();
        e.lowered(32);
        e.lowered(24);
        e.raised(24);
        e.raised(32);
        e.lowered(16);
        e.restored();
        let usage = e.finish(&func, 48).expect("declared");
        assert_eq!((usage.bytes, usage.bound), (104, Bound::Bounded));
        let usage = e.finish(&func, 48).expect("declared");
        assert_eq!((usage.bytes, usage.bound), (48, Bound::Static));
    }

    #[test]
    fn undeclared_functions_have_no_record() {
        assert_eq!(Excursion::default().finish(&Function::default(), 16), None);
    }

    #[test]
    fn qualifiers_are_gccs() {
        assert_eq!(Bound::Static.qualifier(), "static");
        assert_eq!(Bound::Bounded.qualifier(), "dynamic,bounded");
        assert_eq!(Bound::Dynamic.qualifier(), "dynamic");
    }
}
