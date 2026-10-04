//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/trapping_folds.rs, in process: operations
// that trap are not folded away.
//
// On x86-64 `idiv` raises #DE for a zero divisor *and* for the one signed
// overflow, `INT_MIN / -1`; c17 does not assume undefined behaviour away, so
// both divisions must survive to run, while a division that cannot trap is
// still folded.
//

use crate::test_asm::asm_probe::{asm_for_with, body_of, AARCH64_LINUX, X86_64_LINUX};

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
