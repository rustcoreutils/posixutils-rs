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

/// Whether `asm` names the symbol `name` anywhere, as a whole word and with
/// or without Mach-O's leading underscore.
fn mentions(asm: &str, name: &str) -> bool {
    asm.split(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .any(|w| w.strip_prefix('_').unwrap_or(w) == name)
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

/// gcc.c-torture ieee/fp-cmp-7's shape: nothing is greater than `+Inf`, so
/// under `-fno-trapping-math` gcc drops the call at `-O0`, and the program
/// links although `link_error` exists nowhere.
const INF_COMPARE: &str = r#"
extern void link_error(void);
static int calls;
static double next(double x) { calls++; return x; }
static void foo(double x, float y, long double z) {
    if (x > __builtin_inf()) link_error();
    if (-__builtin_inf() > x) link_error();
    if (y < -__builtin_inff()) link_error();
    if (z > __builtin_infl()) link_error();
    if (next(x) > 1e308 * 10) link_error();
    if (__builtin_isgreater(x, __builtin_inf())) link_error();
}
int main(void) {
    foo(1.0, 2.0f, 3.0L);
    foo(__builtin_nan(""), __builtin_nanf(""), __builtin_nanl(""));
    return calls == 2 ? 0 : 1;
}
"#;

#[test]
fn trapping_math_off_folds_a_comparison_no_value_changes() {
    for level in ["-O0", "-O2"] {
        let opts = [level.to_string(), "-fno-trapping-math".to_string()];
        assert_eq!(compile_and_run("cbr_inf", INF_COMPARE, &opts), 0, "{level}");
        if let Some(rc) = crate::common::compile_and_run_aarch64_with(
            "cbr_inf_a64",
            INF_COMPARE,
            &[level, "-fno-trapping-math"],
            &[],
        ) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// With trapping math -- the default, or `-ftrapping-math` after
/// `-fno-trapping-math` -- `-O0` keeps the comparison: a NaN operand raises
/// `FE_INVALID`, which a folded one would not.
#[test]
fn trapping_math_keeps_the_comparison_at_o0() {
    let src = "extern void link_error(void);\n\
               void foo(double x) { if (x > __builtin_inf()) link_error(); }\n";
    for opts in [
        &["-O0"][..],
        &["-O0", "-ftrapping-math"][..],
        &["-O0", "-fno-trapping-math", "-ftrapping-math"][..],
    ] {
        let asm = asm_for_at("cbr_trap", src, opts);
        assert!(mentions(&asm, "link_error"), "{opts:?}\n{asm}");
    }
    let asm = asm_for_at(
        "cbr_notrap",
        src,
        &["-ftrapping-math", "-fno-trapping-math"],
    );
    assert!(!mentions(&asm, "link_error"), "the last flag wins\n{asm}");
}
