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

use crate::codegen::asm_probe::{asm_for_with, body_of, AARCH64_LINUX, X86_64_LINUX};
use crate::common::compile_and_run;

/// How many divide instructions the body of `func` contains.
fn divisions(asm: &str, func: &str) -> usize {
    body_of(asm, func)
        .lines()
        .filter(|l| {
            let t = l.trim();
            t.starts_with("idiv")
                || t.starts_with("div")
                || t.starts_with("sdiv")
                || t.starts_with("udiv")
        })
        .count()
}

/// The two traps `idiv` raises are both left alone.
#[test]
fn codegen_a_trapping_division_is_not_folded() {
    let src = "\
int ovf_div(void) { int a = -2147483647 - 1, b = -1; return a / b; }
int ovf_mod(void) { int a = -2147483647 - 1, b = -1; return a % b; }
long ovf_div_long(void) { long a = -9223372036854775807L - 1, b = -1; return a / b; }
int zero_div(int x) { return 0 / x; }
int zero_mod(int x) { return 0 % x; }
";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("trap_fold", triple, src, &["-O2"]);
        for func in ["ovf_div", "ovf_mod", "ovf_div_long", "zero_div", "zero_mod"] {
            assert!(
                divisions(&asm, func) >= 1,
                "{func} traps at -O0 and must still divide at -O2 on {triple}, \
                 not be folded to an answer:\n{}",
                body_of(&asm, func)
            );
        }
    }
}

/// The control: a division that cannot trap is still folded.
///
/// Without this the test above is satisfied by never folding a division at
/// all, which would cost every program that divides by a constant.
///
/// `x / 8` is deliberately not here: with `x` unknown there is nothing to fold,
/// and c17 has no divide-by-constant strength reduction, so it emits a division
/// for reasons that have nothing to do with trapping.
#[test]
fn codegen_a_safe_division_is_still_folded() {
    let src = "\
int by_one(int x) { return x / 1; }
int mod_one(int x) { return x % 1; }
int both_known(void) { return 12 / 4; }
int both_known_mod(void) { return 13 % 4; }
int neg_but_safe(void) { int a = -2147483647, b = -1; return a / b; }
";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("safe_fold", triple, src, &["-O2"]);
        for func in [
            "by_one",
            "mod_one",
            "both_known",
            "both_known_mod",
            "neg_but_safe",
        ] {
            assert_eq!(
                divisions(&asm, func),
                0,
                "{func} cannot trap, so it is still folded on {triple}:\n{}",
                body_of(&asm, func)
            );
        }
    }
}

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
