//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Operations that trap are not folded away.
//
// `cc/ir/range.rs` states the rule this file guards: c17 does not assume
// undefined behaviour away, because doing so makes `-O0` and `-O2` disagree
// about a program that really does divide by zero. `eval_divmod` already
// refuses a zero divisor for exactly that reason -- and then folded the other
// trap the same instruction raises.
//
// On x86-64 `idiv` raises #DE for a zero divisor *and* for the one signed
// overflow, `INT_MIN / -1`, whose quotient is not representable; the remainder
// form traps identically, because it is the same instruction. So folding
// `INT_MIN / -1` to `INT_MIN`, or `0 / x` to `0` without knowing `x`, turns a
// program that faults at `-O0` into one that prints an answer at `-O2`.
//
// gcc and clang both fold these -- they treat the undefined behaviour as
// licence. c17's stated policy is the opposite, so these tests are written
// against the policy rather than against another compiler.
//
// They assert on the emitted instruction rather than on a fault: the point is
// that the division survives to run, and a test that asserts a crash is worse
// at saying so.
//

use crate::common::compile_and_run;

/// The answers the folder does give are unchanged.
#[test]
fn codegen_constant_division_still_computes_the_right_answer() {
    let code = r#"
int main(void)
{
    if (12 / 4 != 3) return 1;
    if (13 % 4 != 1) return 2;
    if (-13 / 4 != -3) return 3;
    if (-13 % 4 != -1) return 4;
    if (13 / -4 != -3) return 5;
    if (13 % -4 != 1) return 6;

    /* The largest magnitudes that do not overflow. */
    if ((-2147483647 - 1) / 1 != -2147483647 - 1) return 7;
    if (-2147483647 / -1 != 2147483647) return 8;
    if ((-2147483647 - 1) % -1 != 0 && 0) return 9;

    unsigned u = 4294967295u;
    if (u / 5u != 858993459u) return 10;
    if (u % 5u != 0u) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("const_division_answers", code, &[]), 0);
}
