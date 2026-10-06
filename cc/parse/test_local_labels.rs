//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for GNU local labels (`__label__`): every label definition
// and reference resolves to the declaration in force where it is written.
//

use super::ast::{BlockItem, Expr, ExprKind, ExternalDecl, Label, LabelId, LabelScope, Stmt};
use super::test_parser::parse_tu_for;
use crate::strings::StringTable;
use crate::target::{Arch, Os, Target};

/// One label definition or reference, in source order.
#[derive(Debug, PartialEq)]
enum Use {
    /// `name:`
    Def(String, LabelScope),
    /// `goto name;`, `&&name` or an `asm goto` label.
    Ref(String, LabelScope),
}

/// Every label definition and reference in the single function of `src`.
fn label_uses(src: &str) -> Vec<Use> {
    let target = Target::new(Arch::X86_64, Os::Linux);
    let (tu, _, strings, _) = parse_tu_for(src, &target).expect("parse");
    let Some(ExternalDecl::FunctionDef(func)) = tu.items.first() else {
        panic!("expected one function definition");
    };
    let mut out = Vec::new();
    walk_stmt(&func.body, &strings, &mut out);
    out
}

fn spell(strings: &StringTable, label: &LabelId) -> String {
    strings.get(label.name).to_string()
}

fn walk_stmt(stmt: &Stmt, strings: &StringTable, out: &mut Vec<Use>) {
    match stmt {
        Stmt::Block(items) => walk_items(items, strings, out),
        Stmt::Labeled { labels, stmt } => {
            for label in labels {
                if let Label::Named { label, .. } = label {
                    out.push(Use::Def(spell(strings, label), label.scope));
                }
            }
            walk_stmt(stmt, strings, out);
        }
        Stmt::Goto { label, .. } => out.push(Use::Ref(spell(strings, label), label.scope)),
        Stmt::Asm { goto_labels, .. } => {
            for label in goto_labels {
                out.push(Use::Ref(spell(strings, label), label.scope));
            }
        }
        Stmt::Expr(e) => walk_expr(e, strings, out),
        _ => {}
    }
}

fn walk_items(items: &[BlockItem], strings: &StringTable, out: &mut Vec<Use>) {
    for item in items {
        if let BlockItem::Statement(s) = item {
            walk_stmt(s, strings, out);
        }
    }
}

fn walk_expr(expr: &Expr, strings: &StringTable, out: &mut Vec<Use>) {
    match &expr.kind {
        ExprKind::LabelAddr(label) => out.push(Use::Ref(spell(strings, label), label.scope)),
        ExprKind::Assign { value, .. } => walk_expr(value, strings, out),
        ExprKind::StmtExpr { stmts, result } => {
            walk_items(stmts, strings, out);
            walk_expr(result, strings, out);
        }
        _ => {}
    }
}

fn def(name: &str, scope: LabelScope) -> Use {
    Use::Def(name.to_string(), scope)
}

fn refer(name: &str, scope: LabelScope) -> Use {
    Use::Ref(name.to_string(), scope)
}

/// A reference resolves to the innermost `__label__` declaring its name,
/// and an undeclared name to the function's label.
#[test]
fn local_label_resolves_to_the_innermost_declaration() {
    use LabelScope::{Function, Local};
    let uses = label_uses(
        "void f(void) {
            __label__ a;
            {
                __label__ a, b;
                goto a; goto b; goto c;
                a: ; b: ;
            }
            goto a;
            a: ;
            c: ;
        }",
    );
    // The outer `a` is declared first, so it is local label 0; the inner
    // declaration's `a` and `b` are 1 and 2.
    assert_eq!(
        uses,
        [
            refer("a", Local(1)),
            refer("b", Local(2)),
            refer("c", Function),
            def("a", Local(1)),
            def("b", Local(2)),
            refer("a", Local(0)),
            def("a", Local(0)),
            def("c", Function),
        ]
    );
}

/// A local label shadows the function's label of its name only inside its
/// block; outside, the name is the function's again.
#[test]
fn local_label_shadows_a_function_label_only_in_its_block() {
    use LabelScope::{Function, Local};
    let uses = label_uses(
        "void f(void) {
            goto out;
            {
                __label__ out;
                goto out;
                out: ;
            }
            out: ;
            goto out;
        }",
    );
    assert_eq!(
        uses,
        [
            refer("out", Function),
            refer("out", Local(0)),
            def("out", Local(0)),
            def("out", Function),
            refer("out", Function),
        ]
    );
}

/// The same name declared in two sibling blocks is two labels, and so is
/// one macro-like statement expression written twice.
#[test]
fn local_label_in_sibling_blocks_is_two_labels() {
    use LabelScope::Local;
    let uses = label_uses(
        "void f(void) {
            { __label__ l; goto l; l: ; }
            { __label__ l; goto l; l: ; }
            ({ __label__ m; goto m; m: 0; });
            ({ __label__ m; goto m; m: 0; });
        }",
    );
    assert_eq!(
        uses,
        [
            refer("l", Local(0)),
            def("l", Local(0)),
            refer("l", Local(1)),
            def("l", Local(1)),
            refer("m", Local(2)),
            def("m", Local(2)),
            refer("m", Local(3)),
            def("m", Local(3)),
        ]
    );
}

/// `&&label` and `asm goto` labels resolve like `goto`, and a block nested
/// inside the declaring one sees the declaration too.
#[test]
fn local_label_address_and_asm_goto_resolve_like_goto() {
    use LabelScope::{Function, Local};
    let uses = label_uses(
        "void f(void) {
            void *p;
            {
                __label__ x;
                { p = &&x; asm goto(\"\" :::: x, y); }
                x: ;
            }
            y: ;
            p = &&x;
            x: ;
        }",
    );
    assert_eq!(
        uses,
        [
            refer("x", Local(0)),
            refer("x", Local(0)),
            refer("y", Function),
            def("x", Local(0)),
            def("y", Function),
            refer("x", Function),
            def("x", Function),
        ]
    );
}

/// `__label__` is a declaration only at the head of a block.
#[test]
fn local_label_declaration_only_at_the_head_of_a_block() {
    let target = Target::new(Arch::X86_64, Os::Linux);
    for (src, want) in [
        (
            "void f(void) { int a; __label__ x; x: ; }",
            "expected expression before '__label__'",
        ),
        (
            "void f(void) { for (__label__ x;;) ; }",
            "expected expression before '__label__'",
        ),
        (
            "__label__ x;",
            "expected identifier or '(' before '__label__'",
        ),
        (
            "void f(void) { __label__ x; }",
            "expected declaration or statement before '}' token",
        ),
        ("void f(void) { __label__: ; }", "expected identifier"),
    ] {
        let Err(err) = parse_tu_for(src, &target) else {
            panic!("{src:?} should not parse");
        };
        assert!(err.to_string().contains(want), "{src:?}: {err}");
    }
}
