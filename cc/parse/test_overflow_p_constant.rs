//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_{add,sub,mul}_overflow_p` with constant operands is an integer
// constant expression, as in gcc: gnulib's intprops.h builds INT_ADD_OVERFLOW
// and its siblings on it, and test-intprops.c `verify`s them.
//

use super::test_parser::parse_tu;

/// `_Static_assert(expr)` is accepted, with no diagnostic.
fn assert_holds(expr: &str) {
    let src = format!("_Static_assert({expr}, \"\");");
    let before = crate::diag::error_count();
    let parsed = parse_tu(&src);
    assert!(
        parsed.is_ok() && crate::diag::error_count() == before,
        "{expr}: {:?}",
        parsed.err()
    );
}

/// Every verdict here is gcc 13's.
#[test]
fn overflow_p_with_constant_operands_folds() {
    for expr in [
        "__builtin_add_overflow_p(0x7fffffff, 1, (__typeof__(0x7fffffff + 1))0)",
        "!__builtin_add_overflow_p(0x7fffffff, -1, (int)0)",
        "__builtin_sub_overflow_p(-0x7fffffff - 1, 1, (int)0)",
        "__builtin_mul_overflow_p(0x7fffffff, 2, (int)0)",
        "!__builtin_mul_overflow_p(0x7fffffff, 2, (long long)0)",
        "__builtin_add_overflow_p(0xffffffffu, 1u, (unsigned)0)",
        "__builtin_add_overflow_p(200, 100, (unsigned char)0)",
        "!__builtin_add_overflow_p(200, 55, (unsigned char)0)",
        "__builtin_sub_overflow_p(0, 1, (unsigned)0)",
        "__builtin_mul_overflow_p(-1, 1, (unsigned long)0)",
        "!__builtin_mul_overflow_p(-1, -1, (unsigned long)0)",
        "__builtin_add_overflow_p(127, 1, (signed char)0)",
        "!__builtin_sub_overflow_p(-127, 1, (signed char)0)",
        // The exact value, not one computed in the operands' type.
        "!__builtin_add_overflow_p(0xffffffffu, 1u, (long long)0)",
        "__builtin_mul_overflow_p(1 << 20, 1 << 12, (int)0)",
        // 128-bit operands and results: the exact value needs 129 bits.
        "__builtin_add_overflow_p((unsigned __int128)-1, 1, (unsigned __int128)0)",
        "!__builtin_sub_overflow_p((unsigned __int128)-1, (unsigned __int128)-1, (__int128)0)",
        "__builtin_mul_overflow_p((unsigned __int128)-1, 2, (unsigned __int128)0)",
        "!__builtin_mul_overflow_p((__int128)-1, (unsigned __int128)-1 >> 1, (__int128)0)",
        "__builtin_sub_overflow_p(-1, (unsigned __int128)-1, (__int128)0)",
        "!__builtin_add_overflow_p(-1, (unsigned __int128)-1, (unsigned __int128)0)",
    ] {
        assert_holds(expr);
    }
}

/// gcc reads the third argument for its type alone, but one with a side
/// effect makes the call no constant.
#[test]
fn overflow_p_with_a_side_effect_is_not_constant() {
    let src = "int i; void f(void) { _Static_assert(!__builtin_add_overflow_p(1, 1, i++), \"\"); }";
    let before = crate::diag::error_count();
    let failed = parse_tu(src).is_err();
    assert!(failed || crate::diag::error_count() > before, "accepted");
}
