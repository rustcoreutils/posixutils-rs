//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_*_overflow_p` in integer constant expressions.
//

use crate::common::{compile_and_run_everywhere, compile_expect_error};

/// gnulib's intprops.h, as test-intprops.c uses it (sed, libunistring and
/// every other gnulib package run it): with `__builtin_add_overflow_p`
/// available, `INT_ADD_OVERFLOW (a, b)` is
/// `__builtin_add_overflow_p (a, b, (__typeof__ ((a) + (b))) 0)`, and
/// `verify` puts it in a `_Static_assert`. gcc folds it there, and in an
/// enumerator and an array bound; the run-time answer must agree.
#[test]
fn builtins_overflow_p_is_an_integer_constant_expression() {
    compile_and_run_everywhere(
        "overflow_p_constant",
        r#"
#include <limits.h>

#define INT_ADD_OVERFLOW(a, b) \
    __builtin_add_overflow_p (a, b, (__typeof__ ((a) + (b))) 0)
#define INT_SUBTRACT_OVERFLOW(a, b) \
    __builtin_sub_overflow_p (a, b, (__typeof__ ((a) - (b))) 0)
#define INT_MULTIPLY_OVERFLOW(a, b) \
    __builtin_mul_overflow_p (a, b, (__typeof__ ((a) * (b))) 0)
#define verify(R) _Static_assert (R, "verify (" #R ")")

verify (INT_ADD_OVERFLOW (INT_MAX, 1));
verify (!INT_ADD_OVERFLOW (INT_MAX, -1));
verify (INT_SUBTRACT_OVERFLOW (INT_MIN, 1));
verify (INT_MULTIPLY_OVERFLOW (INT_MAX, 2));
verify (INT_ADD_OVERFLOW (UINT_MAX, 1u));
verify (INT_SUBTRACT_OVERFLOW (0u, 1u));
verify (!INT_MULTIPLY_OVERFLOW (LONG_MAX / 2, 2L));
verify (__builtin_add_overflow_p (200, 100, (unsigned char) 0));
verify (!__builtin_mul_overflow_p (INT_MAX, 2, (long long) 0));
verify (__builtin_mul_overflow_p (-1, 1, (unsigned long) 0));
verify (__builtin_add_overflow_p ((unsigned __int128) -1, 1,
                                  (unsigned __int128) 0));

enum { WRAPS = __builtin_mul_overflow_p (1 << 20, 1 << 12, (int) 0) };
static char fits[__builtin_add_overflow_p (100, 27, (signed char) 0) ? 1 : 2];

int main (void)
{
    volatile int max = INT_MAX, one = 1;
    if (WRAPS != 1) return 1;
    if (sizeof fits != 2) return 2;
    if (!INT_ADD_OVERFLOW (max, one)) return 3;
    if (INT_ADD_OVERFLOW (max, -one)) return 4;
    switch (one) {
    case __builtin_sub_overflow_p (0, 1, (unsigned) 0):
        break;
    default:
        return 5;
    }
    return 0;
}
"#,
    );
}

/// A third argument with a side effect is evaluated, so the call is not a
/// constant -- gcc's verdict too.
#[test]
fn builtins_overflow_p_with_a_side_effect_is_not_constant() {
    compile_expect_error(
        "overflow_p_side_effect",
        "int i;\nvoid f(void) { _Static_assert(!__builtin_add_overflow_p(1, 1, i++), \"\"); }\n",
        "expression in static assertion is not constant",
    );
}
