//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A label in front of a declaration in a block: C23 allows it, and gcc and
// clang take it in C17 too, as `L: ; decl`.
//

use super::ast::{BlockItem, ExprKind, ExternalDecl, Label, Stmt};
use super::test_parser::{parse_expr, parse_tu};

/// The block items of the body of the only function in `src`.
fn body_items(src: &str) -> Vec<BlockItem> {
    let (tu, ..) = parse_tu(src).expect("parse");
    let Some(ExternalDecl::FunctionDef(func)) = tu.items.into_iter().next() else {
        panic!("expected one function definition");
    };
    let Stmt::Block(items) = func.body else {
        panic!("expected a block body");
    };
    items
}

/// Whether `item` is the labels `want` on an empty statement.
fn is_labels_on_empty(item: &BlockItem, want: fn(&[Label]) -> bool) -> bool {
    matches!(item, BlockItem::Statement(stmt)
        if matches!(&**stmt, Stmt::Labeled { labels, stmt }
            if want(labels) && matches!(**stmt, Stmt::Empty)))
}

/// `L: decl` is the labels on an empty statement, then the declaration as a
/// block item of its own, in scope for what follows.
#[test]
fn a_named_label_before_a_declaration_labels_an_empty_statement() {
    let items = body_items("int f(void) { goto l; l: char const *p; p = \"x\"; return *p; }");
    let [_, label, BlockItem::Declaration(_), _, _] = items.as_slice() else {
        panic!("{items:#?}");
    };
    assert!(is_labels_on_empty(label, |l| matches!(
        l,
        [Label::Named { .. }]
    )));
}

/// `case` and `default` labels, several in a row, the form grep's dfa.c
/// takes in a `switch`.
#[test]
fn case_labels_before_a_declaration_label_an_empty_statement() {
    let items = body_items(
        "int f(int c) { switch (c) { case 1: int a = 1; return a; \
         default: s: int b = 2; return b; } return 0; }",
    );
    let [BlockItem::Statement(sw), _] = items.as_slice() else {
        panic!("{items:#?}");
    };
    let Stmt::Switch { body, .. } = &**sw else {
        panic!("{sw:#?}");
    };
    let Stmt::Block(items) = &**body else {
        panic!("{body:#?}");
    };
    let [case, BlockItem::Declaration(_), _, default, BlockItem::Declaration(_), _] =
        items.as_slice()
    else {
        panic!("{items:#?}");
    };
    assert!(is_labels_on_empty(case, |l| matches!(l, [Label::Case(..)])));
    assert!(is_labels_on_empty(default, |l| matches!(
        l,
        [Label::Default(_), Label::Named { .. }]
    )));
}

/// In a statement expression the declaration stays a declaration, and the
/// last expression statement still gives the value.
#[test]
fn a_labeled_declaration_in_a_statement_expression() {
    let (expr, ..) = parse_expr("({ l: int x = 3; x; })").unwrap();
    let ExprKind::StmtExpr { stmts, result } = &expr.kind else {
        panic!("expected StmtExpr");
    };
    assert!(matches!(result.kind, ExprKind::Ident(_)));
    let [label, BlockItem::Declaration(_)] = stmts.as_slice() else {
        panic!("{stmts:#?}");
    };
    assert!(is_labels_on_empty(label, |l| matches!(
        l,
        [Label::Named { .. }]
    )));
}

/// Only a block item may be a declaration: the body of an `if` or a loop is
/// a statement, labeled or not, in C23 as in C17.
#[test]
fn a_labeled_declaration_is_not_a_secondary_block() {
    for src in [
        "void f(int c) { if (c) l: int a; }",
        "void f(int c) { while (c) case 1: int a; }",
    ] {
        let before = crate::diag::error_count();
        let failed = parse_tu(src).is_err();
        assert!(
            failed || crate::diag::error_count() > before,
            "{src}: accepted"
        );
    }
}
