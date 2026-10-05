//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly and compile-only cases of tests/builtins/libm.rs, in process:
// libm functions computed by the machine's own instructions.
//

use super::asm_probe::calls_any;
use crate::test_compile::{asm_for, compile_expect_error};

/// `asm` for the host and for aarch64 Linux, with `opts`.
fn asm_both(name: &str, src: &str, opts: &[&str]) -> [(&'static str, String); 2] {
    let mut a64 = opts.to_vec();
    a64.extend_from_slice(&["--target=aarch64-unknown-linux-gnu"]);
    [
        ("host", asm_for(name, src, opts)),
        ("aarch64", asm_for(&format!("{name}_a64"), src, &a64)),
    ]
}

/// Whether `asm` contains the instruction `mnemonic` (with its operands).
fn has_insn(asm: &str, mnemonic: &str) -> bool {
    asm.lines()
        .any(|l| l.split_whitespace().next() == Some(mnemonic))
}

const SQRT_SRC: &str = "double sqrt(double); float sqrtf(float);\n\
                        double d(double x) { return sqrt(x); }\n\
                        float f(float x) { return sqrtf(x); }\n\
                        double bd(double x) { return __builtin_sqrt(x); }\n\
                        float bf(float x) { return __builtin_sqrtf(x); }\n";

/// At `-O2` each root is the instruction, with a call to the library only
/// for an argument below zero, so that `errno` is set; `-fno-math-errno`
/// drops the call. As in gcc.
#[test]
fn libm_sqrt_is_the_instruction() {
    for (target, asm) in asm_both("sqrt_o2", SQRT_SRC, &["-O2"]) {
        let (d, s) = if target == "host" && cfg!(target_arch = "x86_64") {
            ("sqrtsd", "sqrtss")
        } else {
            ("fsqrt", "fsqrt")
        };
        assert!(has_insn(&asm, d) && has_insn(&asm, s), "{target}:\n{asm}");
        assert!(
            calls_any(&asm, &["sqrt", "sqrtf"]),
            "{target}: no errno path:\n{asm}"
        );
    }
    for (target, asm) in asm_both("sqrt_no_errno", SQRT_SRC, &["-O2", "-fno-math-errno"]) {
        assert!(!calls_any(&asm, &["sqrt", "sqrtf"]), "{target}:\n{asm}");
    }
    // The last of the pair wins.
    let opts = ["-O2", "-fno-math-errno", "-fmath-errno"];
    for (target, asm) in asm_both("sqrt_errno_again", SQRT_SRC, &opts) {
        assert!(calls_any(&asm, &["sqrt", "sqrtf"]), "{target}:\n{asm}");
    }
}

/// At `-O0` gcc calls the library for a bare `sqrt`, and for
/// `__builtin_sqrt` too while `errno` is to be set, since only the optimizer
/// can split the call; under `-fno-math-errno` the builtin spelling is the
/// instruction.
#[test]
fn libm_sqrt_at_o0_follows_gcc() {
    for (target, asm) in asm_both("sqrt_o0", SQRT_SRC, &["-O0"]) {
        assert!(
            !has_insn(&asm, "sqrtsd") && !has_insn(&asm, "fsqrt"),
            "{target}:\n{asm}"
        );
        assert!(calls_any(&asm, &["sqrt"]), "{target}:\n{asm}");
    }
    let builtin_only = "double bd(double x) { return __builtin_sqrt(x); }\n\
                        float bf(float x) { return __builtin_sqrtf(x); }\n";
    for (target, asm) in asm_both("sqrt_o0_ne", builtin_only, &["-O0", "-fno-math-errno"]) {
        assert!(!calls_any(&asm, &["sqrt", "sqrtf"]), "{target}:\n{asm}");
    }
    let bare_only = "double sqrt(double);\ndouble d(double x) { return sqrt(x); }\n";
    for (target, asm) in asm_both("sqrt_o0_ne_bare", bare_only, &["-O0", "-fno-math-errno"]) {
        assert!(calls_any(&asm, &["sqrt"]), "{target}:\n{asm}");
    }
}

/// `sqrtl`: the x87 `fsqrt` on x86-64, guarded like the others; binary128 on
/// aarch64 has no instruction, so it is only the call -- with no comparison
/// in front of it, which would be a libgcc call of its own.
#[test]
fn libm_sqrtl_by_target() {
    let src = "long double sqrtl(long double);\n\
               long double l(long double x) { return sqrtl(x); }\n";
    let [(_, host), (_, a64)] = asm_both("sqrtl", src, &["-O2"]);
    if cfg!(target_arch = "x86_64") {
        assert!(has_insn(&host, "fsqrt"), "{host}");
    }
    assert!(calls_any(&a64, &["sqrtl"]), "{a64}");
    assert!(!calls_any(&a64, &["__lttf2"]), "{a64}");
}

/// `-fno-builtin-sqrt` keeps the call to `sqrt`, and only that one.
#[test]
fn libm_sqrt_fno_builtin_keeps_the_call() {
    let opts = ["-O2", "-fno-builtin-sqrt"];
    let src = "double sqrt(double);\ndouble d(double x) { return sqrt(x); }\n";
    for (target, asm) in asm_both("sqrt_nb", src, &opts) {
        assert!(
            calls_any(&asm, &["sqrt"]) && !has_insn(&asm, "sqrtsd") && !has_insn(&asm, "fsqrt"),
            "{target}: -fno-builtin-sqrt computed sqrt in place:\n{asm}"
        );
    }
    let src = "float sqrtf(float);\nfloat f(float x) { return sqrtf(x); }\n";
    for (target, asm) in asm_both("sqrtf_nb", src, &opts) {
        assert!(
            has_insn(&asm, "sqrtss") || has_insn(&asm, "fsqrt"),
            "{target}: -fno-builtin-sqrt displaced sqrtf:\n{asm}"
        );
    }
}

/// A constant root folds, in code and in a static initializer, to what the
/// instruction computes; a domain error does not, in either.
#[test]
fn libm_sqrt_of_constants() {
    let src = "double sqrt(double);\n\
               double f(void) { return sqrt(2.0) + __builtin_sqrtf(16.0f); }\n";
    for (target, asm) in asm_both("sqrt_fold", src, &["-O2"]) {
        assert!(
            !calls_any(&asm, &["sqrt", "sqrtf"]) && !has_insn(&asm, "sqrtsd"),
            "{target}: not folded:\n{asm}"
        );
        assert!(!has_insn(&asm, "fsqrt"), "{target}: not folded:\n{asm}");
    }
    let src = "double sqrt(double);\ndouble f(void) { return sqrt(-2.0); }\n";
    for (target, asm) in asm_both("sqrt_fold_neg", src, &["-O2"]) {
        assert!(calls_any(&asm, &["sqrt"]), "{target}: errno lost:\n{asm}");
    }

    // The static-initializer program runs in libm_constants_mega
    // (tests/builtins/libm.rs).
    compile_expect_error(
        "sqrt_static_domain",
        "double sqrt(double);\nstatic double z = sqrt(-1.0);\ndouble *p = &z;\n",
        "cannot initialize an object with static storage duration",
    );
}

const ROUND_NAMES: [&str; 12] = [
    "floor",
    "ceil",
    "trunc",
    "round",
    "rint",
    "nearbyint",
    "floorf",
    "ceilf",
    "truncf",
    "roundf",
    "rintf",
    "nearbyintf",
];

/// A function computing each of the twelve, spelled `prefix` + name.
fn round_source(prefix: &str) -> String {
    let mut src = String::from(
        "double floor(double); double ceil(double); double trunc(double);\n\
         double round(double); double rint(double); double nearbyint(double);\n\
         float floorf(float); float ceilf(float); float truncf(float);\n\
         float roundf(float); float rintf(float); float nearbyintf(float);\n",
    );
    for name in ROUND_NAMES {
        let t = if name.ends_with('f') {
            "float"
        } else {
            "double"
        };
        src.push_str(&format!(
            "{t} t_{name}({t} x) {{ return {prefix}{name}(x); }}\n"
        ));
    }
    src
}

/// The instructions: `frintm`, `frintp`, `frintz`, `frinta`, `frintx` and
/// `frinti` on aarch64; on x86-64 gcc's SSE2 sequences through `cvttsd2si`
/// for `floor`, `ceil` and `trunc` and the 2^52 addition for `rint`, and
/// calls to `round` and `nearbyint`, which gcc keeps too.
#[test]
fn libm_rounding_is_computed_in_place() {
    for prefix in ["", "__builtin_"] {
        let [(_, host), (_, a64)] = asm_both("round_o2", &round_source(prefix), &["-O2"]);
        for insn in ["frintm", "frintp", "frintz", "frinta", "frintx", "frinti"] {
            assert!(has_insn(&a64, insn), "{prefix}: no {insn}:\n{a64}");
        }
        assert!(
            !calls_any(&a64, &ROUND_NAMES),
            "{prefix}: a call remains:\n{a64}"
        );
        if cfg!(target_arch = "x86_64") {
            assert!(
                has_insn(&host, "cvttsd2si") || has_insn(&host, "cvttsd2siq"),
                "{host}"
            );
            assert!(
                has_insn(&host, "cvttss2si") || has_insn(&host, "cvttss2sil"),
                "{host}"
            );
            let called = [
                "floor", "ceil", "trunc", "rint", "floorf", "ceilf", "truncf", "rintf",
            ];
            assert!(
                !calls_any(&host, &called),
                "{prefix}: a call remains:\n{host}"
            );
            for kept in ["round", "nearbyint", "roundf", "nearbyintf"] {
                assert!(
                    calls_any(&host, &[kept]),
                    "{prefix}: {kept} not called:\n{host}"
                );
            }
        }
    }
}

/// At `-O0` the bare spellings are calls, and the `__builtin_` ones are
/// computed in place where the target can, as in gcc.
#[test]
fn libm_rounding_at_o0_follows_gcc() {
    for (target, asm) in asm_both("round_o0", &round_source(""), &["-O0"]) {
        for name in ROUND_NAMES {
            assert!(
                calls_any(&asm, &[name]),
                "{target}: {name} not called:\n{asm}"
            );
        }
    }
    let [(_, host), (_, a64)] = asm_both("round_o0_b", &round_source("__builtin_"), &["-O0"]);
    assert!(!calls_any(&a64, &ROUND_NAMES), "a call remains:\n{a64}");
    if cfg!(target_arch = "x86_64") {
        assert!(
            !calls_any(&host, &["floor", "ceil", "trunc", "rint"]),
            "{host}"
        );
    }
}

/// `-fno-builtin-floor` keeps the call to `floor`, and only that one.
#[test]
fn libm_rounding_fno_builtin_keeps_the_call() {
    let opts = ["-O2", "-fno-builtin-floor"];
    let src = "double floor(double);\ndouble d(double x) { return floor(x); }\n";
    for (target, asm) in asm_both("floor_nb", src, &opts) {
        assert!(calls_any(&asm, &["floor"]), "{target}:\n{asm}");
    }
    let src = "double ceil(double);\ndouble d(double x) { return ceil(x); }\n";
    for (target, asm) in asm_both("ceil_nb", src, &opts) {
        assert!(!calls_any(&asm, &["ceil"]), "{target}:\n{asm}");
    }
}

/// Constants fold, in code and in static initializers, except a `rint` or
/// `nearbyint` of a value that is not already an integer: its answer is the
/// current rounding direction's, and gcc leaves it too.
#[test]
fn libm_rounding_of_constants() {
    let src = "double floor(double); double ceil(double); double round(double);\n\
               double f(void) { return floor(-0.5) + ceil(2.5) + round(0.5)\n\
               + __builtin_trunc(-1.5) + __builtin_rint(3.0) + __builtin_nearbyintf(-4.0f); }\n";
    for (target, asm) in asm_both("round_fold", src, &["-O2"]) {
        assert!(!calls_any(&asm, &ROUND_NAMES), "{target}:\n{asm}");
        assert!(
            !asm.contains("frint") && !asm.contains("cvtt"),
            "{target}:\n{asm}"
        );
    }
    let src = "double rint(double);\ndouble f(void) { return rint(2.5); }\n";
    let [(_, _), (_, a64)] = asm_both("rint_nofold", src, &["-O2"]);
    assert!(has_insn(&a64, "frintx"), "rint(2.5) folded:\n{a64}");

    // The static-initializer program runs in libm_constants_mega
    // (tests/builtins/libm.rs).
    compile_expect_error(
        "rint_static_inexact",
        "double rint(double);\nstatic double z = rint(2.5);\ndouble *p = &z;\n",
        "cannot initialize an object with static storage duration",
    );
}

/// A call whose answer is a constant is that constant at every level, as
/// under gcc: at `-O0`, where a bare libm spelling is otherwise a call, it
/// still initializes a static object, and a rounding of a constant calls
/// nothing. (A root keeps its call for `errno` until the optimizer folds it.)
/// One whose answer is not a constant -- a root's domain error, a `rint` of
/// a value that is not an integer -- stays a call, and cannot.
#[test]
fn libm_constant_call_is_a_constant_at_every_level() {
    // The program runs in libm_values_mega (tests/builtins/libm.rs).
    let src = "double floor(double);\ndouble g(void) { return floor(2.5); }\n";
    for target in [None, Some("--target=aarch64-unknown-linux-gnu")] {
        let mut args = vec!["-O0"];
        if let Some(t) = target {
            args.push(t);
        }
        let asm = asm_for("libm_const_o0", src, &args);
        assert!(
            !calls_any(&asm, &["floor"]),
            "{target:?}: a call remains:\n{asm}"
        );
    }
    compile_expect_error(
        "sqrt_static_domain_error",
        "double sqrt(double);\nstatic double z = sqrt(-1.0);\ndouble *p = &z;\n",
        "cannot initialize an object with static storage duration",
    );
}

const MIN_MAX_FMA_NAMES: [&str; 6] = ["fmin", "fmax", "fma", "fminf", "fmaxf", "fmaf"];

/// A function computing each of the six, spelled `prefix` + name.
fn min_max_fma_source(prefix: &str) -> String {
    let mut src = String::from(
        "double fmin(double, double); double fmax(double, double);\n\
         double fma(double, double, double); float fminf(float, float);\n\
         float fmaxf(float, float); float fmaf(float, float, float);\n",
    );
    for name in MIN_MAX_FMA_NAMES {
        let t = if name.ends_with('f') {
            "float"
        } else {
            "double"
        };
        let (params, args) = if name == "fma" || name == "fmaf" {
            (format!("{t} x, {t} y, {t} z"), "x, y, z")
        } else {
            (format!("{t} x, {t} y"), "x, y")
        };
        src.push_str(&format!(
            "{t} t_{name}({params}) {{ return {prefix}{name}({args}); }}\n"
        ));
    }
    src
}

/// `fminnm`, `fmaxnm` and `fmadd` on aarch64, as gcc emits them; calls on
/// x86-64, whose baseline has no FMA and whose `minsd` is not `fmin`, as in
/// gcc. At `-O0` the bare spellings are calls everywhere.
#[test]
fn libm_min_max_fma_instructions() {
    for prefix in ["", "__builtin_"] {
        let [(_, host), (_, a64)] = asm_both("mmf_o2", &min_max_fma_source(prefix), &["-O2"]);
        for insn in ["fminnm", "fmaxnm", "fmadd"] {
            assert!(has_insn(&a64, insn), "{prefix}: no {insn}:\n{a64}");
        }
        assert!(!calls_any(&a64, &MIN_MAX_FMA_NAMES), "{prefix}:\n{a64}");
        if cfg!(target_arch = "x86_64") {
            for name in MIN_MAX_FMA_NAMES {
                assert!(
                    calls_any(&host, &[name]),
                    "{prefix}: {name} not called:\n{host}"
                );
            }
        }
    }
    for (target, asm) in asm_both("mmf_o0", &min_max_fma_source(""), &["-O0"]) {
        for name in MIN_MAX_FMA_NAMES {
            assert!(
                calls_any(&asm, &[name]),
                "{target}: {name} not called:\n{asm}"
            );
        }
    }
    let [_, (_, a64)] = asm_both("mmf_o0_b", &min_max_fma_source("__builtin_"), &["-O0"]);
    assert!(!calls_any(&a64, &MIN_MAX_FMA_NAMES), "{a64}");
}

/// `-fno-builtin-fma` keeps the call to `fma`, and only that one.
#[test]
fn libm_fma_fno_builtin_keeps_the_call() {
    let src = "double fma(double, double, double); double fmin(double, double);\n\
               double f(double x, double y) { return fma(x, y, x) + fmin(x, y); }\n";
    let asm = asm_for(
        "fma_nb",
        src,
        &[
            "-O2",
            "-fno-builtin-fma",
            "--target=aarch64-unknown-linux-gnu",
        ],
    );
    assert!(
        calls_any(&asm, &["fma"]) && !has_insn(&asm, "fmadd"),
        "{asm}"
    );
    assert!(
        has_insn(&asm, "fminnm") && !calls_any(&asm, &["fmin"]),
        "{asm}"
    );
}

/// Constants fold on both targets -- on x86-64 too, where the function is
/// otherwise a call -- exactly: `fma` rounds once, and the zeros of `fmin`
/// and `fmax` fold to gcc's -0 and +0. In a static initializer gcc refuses a
/// NaN argument, and so does this.
#[test]
fn libm_min_max_fma_of_constants() {
    let src = "double fmin(double, double); double fmax(double, double);\n\
               double fma(double, double, double);\n\
               double f(void) { return fmin(1.0, 2.0) + fmax(-1.0, __builtin_nan(\"\"))\n\
               + fma(0x1.0000000000001p0, 0x1.fffffffffffffp-1, -1.0)\n\
               + __builtin_fmaf(2.0f, 3.0f, 4.0f); }\n";
    for (target, asm) in asm_both("mmf_fold", src, &["-O2"]) {
        assert!(!calls_any(&asm, &MIN_MAX_FMA_NAMES), "{target}:\n{asm}");
        assert!(
            !asm.contains("fminnm") && !asm.contains("fmadd"),
            "{target}:\n{asm}"
        );
    }

    // The static-initializer program runs in libm_constants_mega
    // (tests/builtins/libm.rs).
    compile_expect_error(
        "fmin_static_nan",
        "double fmin(double, double);\nstatic double z = fmin(__builtin_nan(\"\"), 3.0);\n\
         double *p = &z;\n",
        "cannot initialize an object with static storage duration",
    );
}
