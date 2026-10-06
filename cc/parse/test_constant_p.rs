//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// What the parser answers for `__builtin_constant_p`, and what it defers:
// gcc's `fold_builtin_constant_p` rules.
//

use super::ast::ExprKind;
use super::test_parser::parse_expr_on;
use super::LibraryCallPolicy;
use crate::target::{Arch, Os, Target};

/// What the parser made of `__builtin_constant_p(operand)`, with the `int`
/// variable `x` in scope: `Some(answer)`, or `None` where it deferred.
fn answer(operand: &str, optimizing: bool, target: &Target) -> Option<i64> {
    let policy = LibraryCallPolicy {
        optimizing,
        math_errno: true,
    };
    let src = format!("__builtin_constant_p({operand})");
    let (expr, ..) = parse_expr_on(&src, &["x"], policy, target).unwrap();
    match expr.kind {
        ExprKind::IntLit(v) => Some(v),
        ExprKind::ConstantP(_) => None,
        other => panic!("{operand}: {other:?}"),
    }
}

fn targets() -> [Target; 2] {
    [
        Target::new(Arch::X86_64, Os::Linux),
        Target::new(Arch::Aarch64, Os::Linux),
    ]
}

/// The address of a string literal's first character is 1 at every level,
/// however it is spelled; any other address into one is not.
#[test]
fn a_string_literals_start_is_constant() {
    for target in targets() {
        for optimizing in [false, true] {
            for operand in [
                "\"hi\"",
                "L\"wide\"",
                "u8\"hi\"",
                "&\"hi\"",
                "&\"hi\"[0]",
                "&*\"hi\"",
                "\"hi\" + 0",
                "(const void *)\"hi\"",
                "(long)\"hi\"",
                "(unsigned long)(char *)\"hi\"",
            ] {
                assert_eq!(answer(operand, optimizing, &target), Some(1), "{operand}");
            }
            for operand in ["\"hi\" + 1", "&\"hi\"[1]"] {
                assert_eq!(answer(operand, optimizing, &target), Some(0), "{operand}");
            }
        }
    }
}

/// `(int)"hi"` drops half the address on LP64: no longer the literal's
/// address, it is an `int` like any other, deferred when optimizing.
#[test]
fn a_truncated_string_address_is_not_the_literal() {
    for target in targets() {
        assert_eq!(answer("(int)\"hi\"", true, &target), None);
        assert_eq!(answer("(int)\"hi\"", false, &target), Some(0));
    }
}

/// Any other pointer or aggregate is 0 at once, as in gcc: even optimized,
/// `&x` is not a constant `__builtin_constant_p` reports.
#[test]
fn a_pointer_or_aggregate_is_answered_at_once() {
    for target in targets() {
        for operand in [
            "&x",
            "(char *)x",
            "(struct { int a; }){ 1 }",
            "(int[2]){ 1, 2 }",
        ] {
            assert_eq!(answer(operand, true, &target), Some(0), "{operand}");
        }
        // A null pointer constant is a constant first.
        assert_eq!(answer("(void *)0", true, &target), Some(1));
    }
}

/// A GNU vector is a value, not an aggregate: like a scalar it waits for the
/// optimizer, which answers a vector of constants 1 as gcc does, and is 0 at
/// once at `-O0`.
#[test]
fn a_vector_is_deferred_like_a_scalar() {
    for target in targets() {
        let operand = "(int __attribute__((vector_size(8))))(long)x";
        assert_eq!(answer(operand, true, &target), None);
        assert_eq!(answer(operand, false, &target), Some(0));
    }
}

/// An arithmetic operand the parser cannot fold waits for the optimizer --
/// the trapping ones too, which the linearizer decides about -- and is 0 at
/// once at `-O0`.
#[test]
fn an_unknown_scalar_is_deferred_only_when_optimizing() {
    for target in targets() {
        for operand in [
            "x",
            "x + 1",
            "x / 2",
            "\"hi\"[0]",
            "*\"hi\"",
            "x++",
            "(double)x",
        ] {
            assert_eq!(answer(operand, true, &target), None, "{operand}");
            assert_eq!(answer(operand, false, &target), Some(0), "{operand}");
        }
    }
}
