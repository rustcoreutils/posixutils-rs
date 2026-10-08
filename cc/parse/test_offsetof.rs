//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `offsetof` with an array index that is not a constant: a GNU extension
// (C17 7.19p3 wants `&(t.member-designator)` to be an address constant).
// util-linux's `list_entry(p, struct lsns_process, ns_siblings[ns->type])`
// and libblkid's `offsetof(struct atari_rootsector, part[i])` need it.
//

use super::ast::{BinaryOp, ExprKind, ExternalDecl, OffsetOfPath, Stmt};
use super::test_parser::parse_tu;

/// The expression the only function in `src` returns.
fn returned(src: &str) -> super::ast::Expr {
    let (tu, ..) = parse_tu(src).expect("parse");
    let Some(ExternalDecl::FunctionDef(func)) = tu.items.into_iter().last() else {
        panic!("expected a function definition last");
    };
    let Stmt::Block(items) = func.body else {
        panic!("expected a block body");
    };
    for item in items {
        if let super::ast::BlockItem::Statement(stmt) = item {
            if let Stmt::Return(Some(e)) = *stmt {
                return e;
            }
        }
    }
    panic!("no return statement");
}

const S: &str = "struct S { char c; struct { int x; short y[3]; } a[4]; long z; };";

/// A variable index makes the result the constant offset of the element
/// with index 0 plus the index times the element size, computed at run time.
#[test]
fn a_variable_index_adds_its_scaled_value_to_the_constant_offset() {
    let e = returned(&format!(
        "{S} unsigned long f(int i) {{ return __builtin_offsetof(struct S, a[i].x); }}"
    ));
    let ExprKind::Binary {
        op: BinaryOp::Add,
        left,
        right,
    } = &e.kind
    else {
        panic!("expected a sum, got {:?}", e.kind);
    };
    let ExprKind::OffsetOf { path, .. } = &left.kind else {
        panic!("expected the constant part first, got {:?}", left.kind);
    };
    assert!(matches!(path[1], OffsetOfPath::Index(0)), "{path:?}");
    assert!(
        matches!(
            right.kind,
            ExprKind::Binary {
                op: BinaryOp::Mul,
                ..
            }
        ),
        "{:?}",
        right.kind
    );
}

/// Two variable indices each add their own term.
#[test]
fn each_variable_index_adds_a_term() {
    let e = returned(&format!(
        "{S} unsigned long f(int i, int j) {{ return __builtin_offsetof(struct S, a[i].y[j]); }}"
    ));
    let ExprKind::Binary {
        op: BinaryOp::Add,
        left,
        ..
    } = &e.kind
    else {
        panic!("expected a sum, got {:?}", e.kind);
    };
    assert!(
        matches!(
            left.kind,
            ExprKind::Binary {
                op: BinaryOp::Add,
                ..
            }
        ),
        "{:?}",
        left.kind
    );
}

/// A constant index is still an integer constant expression.
#[test]
fn a_constant_index_stays_a_constant_expression() {
    let src = format!(
        "{S} _Static_assert(__builtin_offsetof(struct S, a[2].y[1]) == 4 + 2 * 12 + 4 + 2, \"c\");
         int arr[__builtin_offsetof(struct S, a[1])];
         unsigned long f(void) {{ return __builtin_offsetof(struct S, a[3]); }}"
    );
    let before = crate::diag::error_count();
    let (tu, ..) = parse_tu(&src).expect("parse");
    assert_eq!(
        crate::diag::error_count(),
        before,
        "a constant index was refused"
    );
    let Some(ExternalDecl::FunctionDef(func)) = tu.items.into_iter().last() else {
        panic!("expected a function definition last");
    };
    let Stmt::Block(items) = func.body else {
        panic!("expected a block body");
    };
    let Some(super::ast::BlockItem::Statement(stmt)) = items.into_iter().next() else {
        panic!("expected a statement");
    };
    let Stmt::Return(Some(e)) = *stmt else {
        panic!("expected a return");
    };
    assert!(matches!(e.kind, ExprKind::OffsetOf { .. }), "{:?}", e.kind);
}

/// A variable index is not a constant: `_Static_assert` refuses it. (A
/// static initializer is refused when it is lowered; see the e2e test.)
#[test]
fn a_variable_index_is_not_a_constant_expression() {
    for body in [
        "_Static_assert(__builtin_offsetof(struct S, a[n]) == 4, \"v\");",
        "enum { E = __builtin_offsetof(struct S, a[n]) };",
    ] {
        let src = format!("{S} void f(int n) {{ {body} }}");
        let before = crate::diag::error_count();
        let refused = parse_tu(&src).is_err();
        assert!(
            refused || crate::diag::error_count() > before,
            "{body}: accepted"
        );
    }
}

/// The index must have integer type, as an array subscript must.
#[test]
fn a_non_integer_index_is_refused() {
    let src =
        format!("{S} unsigned long f(double d) {{ return __builtin_offsetof(struct S, a[d]); }}");
    let before = crate::diag::error_count();
    let refused = parse_tu(&src).is_err();
    assert!(
        refused || crate::diag::error_count() > before,
        "a double index was accepted"
    );
}
