//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A branch whose condition is a constant expression, at every level.
//
// gcc emits neither the arm a constant condition makes unreachable nor any
// other block no path reaches, even at -O0, and a program may depend on it:
// gcc.c-torture's `link_error` tests call an undefined function from such an
// arm and expect the link to succeed. What reaches a block by a label, a
// `case` or a `goto` keeps it.
//

use crate::common::{asm_for_at, compile_and_run, compile_and_run_aarch64};

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

/// The labels that keep a block: a `case` inside `if (0)` (gcc.c-torture
/// medce-1), a `goto` into a dead arm, a `default` inside a dead loop, and
/// Duff's device. The code in front of each label is still unreachable, so
/// `link_error` never has to exist.
const LABELS: &str = r#"
extern void link_error(void);
static int ok;
static void bar(void) { ok++; }

static void medce(int x) {
    switch (x) {
    case 0:
        if (0) {
            link_error();
    case 1:
            bar();
        }
    }
}

static int into_dead_arm(int c) {
    int r = 0;
    if (c) goto inside;
    if (0) {
        link_error();
    inside:
        r = 7;
    }
    return r;
}

static int dead_loop_default(int x) {
    int r = 0;
    switch (x) {
    case 0:
        while (0) {
            link_error();
    default:
            r = 5;
            break;
        }
    }
    return r;
}

static int duff(int n) {
    int count = 0;
    int k = (n + 3) / 4;
    switch (n % 4) {
    case 0: do { count++;
    case 3:      count++;
    case 2:      count++;
    case 1:      count++;
            } while (--k > 0);
    }
    return count;
}

static int loops(void) {
    int i = 0, j = 0;
    while (1) { if (++i == 5) break; }
    do { j++; if (j < 3) continue; } while (0);
    for (;;) { if (i++ > 9) break; if (i & 1) continue; j++; }
    if (0 || 0) link_error();
    return i * 100 + j;
}

static int constant_switch(void) {
    int r = 0;
    switch (2) { case 1: link_error(); case 2: r++; case 3: r++; }
    switch ((unsigned char)-1) { case -1: link_error(); break; case 255: r += 10; }
    switch (5) { case 1 ... 3: link_error(); break; default: r += 100; }
    switch (9) { case 0: link_error(); }
    return r;
}

int main(void) {
    medce(1);
    if (ok != 1) return 1;
    medce(0);
    if (ok != 1) return 2;
    if (into_dead_arm(1) != 7 || into_dead_arm(0) != 0) return 3;
    if (dead_loop_default(4) != 5 || dead_loop_default(0) != 0) return 4;
    if (duff(7) != 7 || duff(8) != 8 || duff(1) != 1) return 5;
    if (loops() != 1104) return 6;
    if (constant_switch() != 112) return 7;
    return 0;
}
"#;

#[test]
fn constant_branch_keeps_a_block_a_label_reaches() {
    for level in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("cbr_labels", LABELS, &[level.to_string()]),
            0,
            "{level}"
        );
    }
}

#[test]
fn constant_branch_keeps_a_block_a_label_reaches_aarch64() {
    for level in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64("cbr_labels_a64", LABELS, level) {
            assert_eq!(rc, 0, "aarch64 {level}");
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
