//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for `__attribute__((cleanup(fn)))`: which exits call
// `fn(&var)`, in what order, and after what.
//

use super::test_linearize::linearize_source;
use crate::ir::{BasicBlock, Function, Module, Opcode};
use crate::target::{Arch, Os, Target};

fn targets() -> [Target; 2] {
    [
        Target::new(Arch::X86_64, Os::Linux),
        Target::new(Arch::Aarch64, Os::Linux),
    ]
}

fn func<'a>(module: &'a Module, name: &str) -> &'a Function {
    module
        .functions
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("no function {name}"))
}

/// The functions `bb` calls, in order.
fn callees(bb: &BasicBlock) -> Vec<&str> {
    bb.insns
        .iter()
        .filter(|i| i.op == Opcode::Call)
        .filter_map(|i| i.extra().func_name.as_deref())
        .collect()
}

/// For every block that calls something, the callees in order, sorted so
/// the answer does not depend on block layout.
fn call_runs(f: &Function) -> Vec<Vec<&str>> {
    let mut runs: Vec<Vec<&str>> = f
        .blocks
        .iter()
        .map(callees)
        .filter(|c| !c.is_empty())
        .collect();
    runs.sort();
    runs
}

/// Each way out of a scope runs the cleanups of the variables it leaves,
/// innermost first, and only those: `continue`, `break` and the `goto` leave
/// the loop body's `b`; the `return` leaves `b` and then `a`; falling out of
/// the body runs `b`, and the final `return` runs `a`.
#[test]
fn every_exit_runs_the_cleanups_it_leaves_innermost_first() {
    let src = "void c(int *); void d(int *);\n\
               int f(int x) {\n\
                 int a __attribute__((cleanup(c))) = 1;\n\
                 for (int i = 0; i < x; i++) {\n\
                   int b __attribute__((cleanup(d))) = i;\n\
                   if (i == 1) continue;\n\
                   if (i == 2) break;\n\
                   if (i == 3) goto out;\n\
                   if (i == 4) return b++;\n\
                 }\n\
               out:\n\
                 return 0;\n\
               }\n";
    for target in targets() {
        let module = linearize_source(src, &target);
        let f = func(&module, "f");
        assert_eq!(
            call_runs(f),
            [
                vec!["c"],
                vec!["d"],
                vec!["d"],
                vec!["d"],
                vec!["d"],
                vec!["d", "c"],
            ],
            "{:?}",
            target.arch
        );
        // The cleanups are the last thing before the jump or return that
        // leaves: nothing follows them but the terminator, and on the path
        // falling out of a block the end of the variables' lifetimes.
        for bb in f.blocks.iter().filter(|bb| !callees(bb).is_empty()) {
            let last_call = bb.insns.iter().rposition(|i| i.op == Opcode::Call);
            let after: Vec<Opcode> = bb.insns[last_call.unwrap() + 1..]
                .iter()
                .map(|i| i.op)
                .filter(|&op| op != Opcode::LifetimeEnd)
                .collect();
            assert!(
                matches!(after[..], [op] if op.is_terminator()),
                "{:?}: {after:?} after the cleanups",
                target.arch
            );
        }
    }
}

/// `return b++` returns the value before the increment, and the cleanup sees
/// the value after it: the store of the increment precedes the call.
#[test]
fn a_return_runs_its_cleanups_after_the_value_is_computed() {
    let src = "void d(int *);\n\
               int f(void) {\n\
                 int b __attribute__((cleanup(d))) = 1;\n\
                 return b++;\n\
               }\n";
    for target in targets() {
        let module = linearize_source(src, &target);
        let f = func(&module, "f");
        let bb = f.blocks.iter().find(|bb| !callees(bb).is_empty()).unwrap();
        let call = bb.insns.iter().position(|i| i.op == Opcode::Call).unwrap();
        let last_store = bb.insns.iter().rposition(|i| i.op == Opcode::Store);
        let ret = bb.insns.iter().position(|i| i.op == Opcode::Ret).unwrap();
        assert!(last_store.is_some_and(|s| s < call), "{:?}", target.arch);
        assert!(call < ret, "{:?}", target.arch);
    }
}

/// A returned aggregate is copied into the caller's buffer -- the last store
/// -- before its own cleanup can scrub it.
#[test]
fn a_returned_aggregate_is_copied_before_its_cleanup() {
    let src = "struct S { long v[8]; };\n\
               void s(struct S *);\n\
               struct S f(void) {\n\
                 struct S r __attribute__((cleanup(s))) = {{1}};\n\
                 return r;\n\
               }\n";
    for target in targets() {
        let module = linearize_source(src, &target);
        let f = func(&module, "f");
        let insns: Vec<_> = f.blocks.iter().flat_map(|bb| bb.insns.iter()).collect();
        let cleanup = insns
            .iter()
            .position(|i| i.op == Opcode::Call && i.extra().func_name.as_deref() == Some("s"))
            .unwrap();
        let copy = insns.iter().rposition(|i| i.op == Opcode::Store);
        assert!(copy.is_some_and(|c| c < cleanup), "{:?}", target.arch);
    }
}

/// A statement expression's value is computed before its cleanups run.
#[test]
fn a_statement_expression_runs_its_cleanups_after_its_value() {
    let src = "void d(int *);\n\
               int f(void) {\n\
                 return ({ int v __attribute__((cleanup(d))) = 1; v + 1; });\n\
               }\n";
    for target in targets() {
        let module = linearize_source(src, &target);
        let f = func(&module, "f");
        let insns: Vec<_> = f.blocks.iter().flat_map(|bb| bb.insns.iter()).collect();
        let add = insns.iter().position(|i| i.op == Opcode::Add).unwrap();
        let call = insns.iter().position(|i| i.op == Opcode::Call).unwrap();
        assert!(add < call, "{:?}", target.arch);
    }
}

/// gcc runs no cleanup on a computed `goto`, or on an `asm goto`'s jump:
/// only the `asm goto`'s fall-through leaves the scope normally.
#[test]
fn no_cleanup_runs_on_a_computed_or_asm_goto() {
    let src = "void c(int *);\n\
               void computed(void) {\n\
                 static void *t[] = {&&out};\n\
                 { int v __attribute__((cleanup(c))) = 1; goto *t[0]; }\n\
               out:;\n\
               }\n\
               void asm_goto(void) {\n\
                 { int v __attribute__((cleanup(c))) = 1; asm goto (\"\" :::: out); }\n\
               out:;\n\
               }\n";
    for target in targets() {
        let module = linearize_source(src, &target);
        assert!(
            call_runs(func(&module, "computed")).is_empty(),
            "{:?}",
            target.arch
        );
        assert_eq!(
            call_runs(func(&module, "asm_goto")),
            [vec!["c"]],
            "{:?}",
            target.arch
        );
    }
}

/// A `goto` runs the cleanups of the scopes it leaves -- backward to a label
/// before the declaration, and forward out of two blocks -- and none for a
/// label still inside them.
#[test]
fn a_goto_runs_the_cleanups_of_the_scopes_it_leaves() {
    let src = "void c(int *); void d(int *);\n\
               void f(int n) {\n\
                 int a __attribute__((cleanup(c))) = 0;\n\
               again:\n\
                 {\n\
                   int b __attribute__((cleanup(d))) = n;\n\
                   if (n--) goto again;\n\
                   if (n == 5) goto inside;\n\
                 inside:\n\
                   if (n == 7) goto out;\n\
                 }\n\
               out:;\n\
               }\n";
    for target in targets() {
        let module = linearize_source(src, &target);
        // `goto again` and `goto out` leave `b`; `goto inside` leaves
        // nothing; falling out of the block leaves `b`, and of the function
        // `a`.
        assert_eq!(
            call_runs(func(&module, "f")),
            [vec!["c"], vec!["d"], vec!["d"], vec!["d"]],
            "{:?}",
            target.arch
        );
    }
}
