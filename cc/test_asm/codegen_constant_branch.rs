//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/constant_branch.rs, in process: a branch
// whose condition is a constant expression, at every level.
//
// gcc emits neither the arm a constant condition makes unreachable nor any
// other block no path reaches, even at -O0, and a program may depend on it:
// gcc.c-torture's `link_error` tests call an undefined function from such an
// arm and expect the link to succeed.
//

use crate::test_asm::asm_probe::asm_for_at;

/// Every call to a `D*` function sits where no path reaches; every `K*` one
/// is reachable and must stay.
const SHAPES: &str = r#"
extern void D1(void), D2(void), D3(void), D4(void), D5(void), D6(void),
    D7(void), D8(void), D9(void), D10(void), D11(void), D12(void), D13(void),
    D14(void), D15(void), D16(void), D17(void);
extern void K1(void), K2(void), K3(void), K4(void), K5(void), K6(void);
extern int g(void);
void f_if(void) { if (0) D1(); }
void f_else(void) { if (1) K1(); else D2(); }
void f_while(void) { while (0) D3(); }
void f_for(void) { for (; 0;) D4(); }
void f_do(void) { do K2(); while (0); }
int f_and(void) { return 0 && (D5(), 1); }
int f_or(void) { return 1 || (D6(), 1); }
void f_float(void) { if (1.0 > 2.0) D7(); }
void f_nan(void) { if (__builtin_nan("") == __builtin_nan("")) D8(); }
void f_zero(void) { if (-0.0 == 0.0) K3(); else D9(); }
void f_cond(void) { if (0 && g()) D10(); }
void f_constant_p(int n) { if (__builtin_constant_p(n)) D11(); }
void f_return(void) { return; D12(); }
void f_goto(void) { goto out; D13(); out:; }
void f_forever(void) { for (;;) g(); D14(); }
void f_ternary(void) { 0.0 ? D15() : K4(); }
void f_const_object(void) { const int k = 0; if (k) K5(); }
void f_switch(void) { switch (1) { case 0: D16(); break; case 1: K6(); } }
void f_switch_none(void) { switch (7) { case 0: D17(); } }
"#;

/// Whether `asm` names the symbol `name` anywhere, as a whole word and with
/// or without Mach-O's leading underscore.
fn mentions(asm: &str, name: &str) -> bool {
    asm.split(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .any(|w| w.strip_prefix('_').unwrap_or(w) == name)
}

/// The reproducer: c17 kept every `D*` call at -O0, where gcc keeps none.
/// A `const int` is not a constant expression in C, and gcc keeps `K5`.
#[test]
fn constant_branch_drops_the_unreachable_arm_at_o0() {
    for level in ["-O0", "-O2"] {
        let asm = asm_for_at("cbr_shapes", SHAPES, &[level]);
        for n in 1..=17 {
            let call = format!("D{n}");
            assert!(
                !mentions(&asm, &call),
                "{level}: {call} is unreachable and must not be emitted\n{asm}"
            );
        }
        if level == "-O0" {
            for n in 1..=6 {
                let call = format!("K{n}");
                assert!(
                    mentions(&asm, &call),
                    "{level}: {call} is reachable and must be emitted\n{asm}"
                );
            }
        }
    }
}

/// The dead arm's line has nowhere to land under `-g`, but the rest of the
/// function still has line information and assembles.
#[test]
fn constant_branch_debug_info_stays_coherent() {
    let src = "extern void link_error(void);\n\
               int f(int x) {\n\
                   if (0) {\n\
                       link_error();\n\
                   }\n\
                   return x + 1;\n\
               }\n";
    let asm = asm_for_at("cbr_debug", src, &["-O0", "-g"]);
    assert!(!asm.contains("link_error"), "{asm}");
    assert!(
        asm.lines()
            .any(|l| l.trim_start().starts_with(".loc") && l.contains(" 6 ")),
        "the return statement keeps its line\n{asm}"
    );
    assert!(
        !asm.lines()
            .any(|l| l.trim_start().starts_with(".loc") && l.contains(" 4 ")),
        "the removed call leaves no line entry\n{asm}"
    );
}
