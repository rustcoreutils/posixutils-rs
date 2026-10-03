//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for valid code once rejected: member-list shapes from the
// Linux UAPI headers, `_Static_assert` messages, and short-circuit constants.
//

use super::test_parser::{parse_tu, with_statement_expr};
use crate::symbol::Namespace;

/// The members `struct <tag>` in `src` was defined with, as (named, width).
fn members(src: &str, tag: &str) -> Vec<(bool, Option<u32>)> {
    let (_, types, strings, symbols) = parse_tu(src).unwrap();
    let tag = strings.lookup(tag).expect("tag interned");
    let typ = symbols
        .lookup(tag, Namespace::Tag)
        .expect("tag declared")
        .typ;
    let composite = types.get(typ).composite.as_ref().expect("a composite");
    composite
        .members
        .iter()
        .map(|m| (m.name != crate::strings::StringId::EMPTY, m.bit_width))
        .collect()
}

#[test]
fn test_unnamed_bitfields_may_begin_a_declarator_list() {
    assert_eq!(
        members("struct s { unsigned char :1, :1, x:1; };", "s"),
        [(false, Some(1)), (false, Some(1)), (true, Some(1))]
    );
}

#[test]
fn test_stray_semicolons_in_a_member_list() {
    assert_eq!(
        members("struct s { ; int a;; int b; ; };", "s"),
        [(true, None), (true, None)]
    );
}

#[test]
fn test_fam_after_an_anonymous_union_is_accepted() {
    assert_eq!(
        members("struct s { union { int a; }; char d[]; };", "s"),
        [(false, None), (true, None)]
    );
}

#[test]
fn test_static_assert_message_concatenates() {
    let Err(err) = parse_tu("_Static_assert(0, \"first \" \"second\");") else {
        panic!("a failed assertion was accepted");
    };
    assert!(
        err.to_string()
            .contains("static assertion failed: first second"),
        "{err}"
    );
    parse_tu("_Static_assert(1, L\"a\" L\"b\");").unwrap();
}

/// The right operand of `&&` / `||` is not evaluated once the left decides,
/// so it need not be constant (C17 6.5.13p4, 6.5.14p4, 6.6p3).
#[test]
fn test_logical_operators_short_circuit_when_folded() {
    for (expr, want) in [
        ("1 || 1/0", Some(1)),
        ("0 && 1/0", Some(0)),
        ("2 || x", Some(1)),
        ("0 && x", Some(0)),
        ("0 || 2.5", Some(1)),
        ("1 && 0", Some(0)),
        ("0 || 1/0", None),
        ("1 && x", None),
    ] {
        with_statement_expr("int x;", expr, |p, e| {
            assert_eq!(p.eval_const_expr(e), want, "{expr}");
        });
    }
}
