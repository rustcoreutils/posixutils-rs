//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly and compile-only cases of tests/builtins/math.rs, in process.
//

use super::asm_probe::{asm_prefix, asm_symbol, calls_any};
use crate::test_compile::{asm_for, compile_expect_error};

/// Whether `asm` calls exactly `sym` (an assembler-spelled name).
fn calls_exactly(asm: &str, sym: &str) -> bool {
    asm.lines().any(|l| {
        let mut words = l.split_whitespace();
        matches!(words.next(), Some("call" | "bl" | "jmp" | "b"))
            && words.next().map(|t| t.trim_end_matches("@PLT")) == Some(sym)
    })
}

/// A narrowed call is computed in place, at `float` -- `cvttss2si` on
/// x86-64, `frintm` of an `s` register on aarch64 -- and nothing is called.
/// At `-O0` the bare `floor` is not computed but called, as under gcc, and
/// the call is the one the program wrote: to `floor`, never `floorf`, so
/// that a definition of `floor` anywhere in the unit is what it reaches.
#[test]
fn builtins_math_narrowing_computes_in_place() {
    let src = "double floor(double);\nfloat q(float a) { return floor(a); }\n";
    // The host's own format, plus both Darwin triples so the Mach-O spelling
    // is exercised wherever this runs. The prefix is read off each output
    // rather than assumed -- Mach-O calls `_floorf`, and "floorf" is a
    // substring of that, so a check spelled for ELF keeps passing there
    // while its negative half matches nothing at all. The last element is
    // the instruction that computes a `float` floor in place.
    let host_insn = if cfg!(target_arch = "aarch64") {
        "frintm s"
    } else {
        "cvttss2si"
    };
    let targets: [(&[&str], &str); 3] = [
        (&[], host_insn),
        (&["--target=aarch64-apple-darwin"], "frintm s"),
        (&["--target=x86_64-apple-darwin"], "cvttss2si"),
    ];
    for (target, in_place) in targets {
        for opt in ["-O0", "-O1", "-O2"] {
            let mut args = vec![opt];
            args.extend_from_slice(target);
            let asm = asm_for("math_narrow_level", src, &args);
            // `q` is the function this source defines, so it calibrates.
            let p = asm_prefix(&asm, "q");
            // Matched exactly, across all four spellings a call takes here:
            // `call f@PLT` and `bl f` on ELF, `call _f` and `bl _f` on
            // Mach-O, which has no PLT syntax. Exactness is also what stops
            // `floor` matching the `floorf` the narrowing produces.
            let calls = |name: &str| {
                let want = format!("{p}{name}");
                asm.lines().any(|line| {
                    let t = line.trim_start();
                    t.strip_prefix("call ")
                        .or_else(|| t.strip_prefix("bl "))
                        .is_some_and(|dst| {
                            dst == want
                                || dst.strip_prefix(&want).is_some_and(|s| s.starts_with('@'))
                        })
                })
            };
            assert!(
                !calls("floorf"),
                "{target:?} at {opt}: the float form must not be called:\n{asm}"
            );
            if opt == "-O0" {
                assert!(
                    calls("floor"),
                    "{target:?} at {opt}: the call should be to {p}floor:\n{asm}"
                );
            } else {
                assert!(
                    !calls("floor") && asm.contains(in_place),
                    "{target:?} at {opt}: not computed in place at float:\n{asm}"
                );
            }
        }
    }
}

/// `creal`, `cimag` and `conj` are computed in place under their bare names,
/// as the `__builtin_` spellings always were; `-fno-builtin-NAME` keeps the
/// library call. (The run halves are in tests/builtins/math.rs.)
#[test]
fn builtins_bare_complex_accessors() {
    let src = "double creal(double _Complex);\n\
               double cimag(double _Complex);\n\
               double _Complex conj(double _Complex);\n\
               double f(double _Complex z) { return creal(z) + cimag(conj(z)); }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for("complex_accessors_asm", src, &[opt]);
        for name in ["creal", "cimag", "conj"] {
            assert!(
                !calls_exactly(&asm, &asm_symbol(name)),
                "{opt}: {name} was called:\n{asm}"
            );
        }
    }
    let asm = asm_for("complex_accessors_nb", src, &["-fno-builtin-creal"]);
    assert!(
        calls_exactly(&asm, &asm_symbol("creal")),
        "-fno-builtin-creal kept creal inline:\n{asm}"
    );
    assert!(
        !calls_exactly(&asm, &asm_symbol("conj")),
        "-fno-builtin-creal displaced conj:\n{asm}"
    );
}

/// `fabs` and `fabsf` are computed in place: no call at any level. (The run
/// halves, linked without -lm, are in tests/builtins/math.rs.)
#[test]
fn builtins_fabs_needs_no_libm() {
    let src = "double f(double x) { return __builtin_fabs(x); }\n\
               float g(float x) { return __builtin_fabsf(x); }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for("fabs_inline", src, &[opt]);
        for name in ["fabs", "fabsf"] {
            assert!(
                !calls_exactly(&asm, &asm_symbol(name)),
                "{opt}: {name} was called:\n{asm}"
            );
        }
    }
}

/// `fabsl` is a sign-bit operation on every target, the x87 `fabs` on x86-64
/// and a clear of bit 127 of the binary128 on aarch64: never a call. (The run
/// halves are in tests/builtins/math.rs.)
#[test]
fn builtins_fabsl_needs_no_libm() {
    let called = |asm: &str| {
        asm.lines().any(|l| {
            let mut w = l.split_whitespace();
            matches!(w.next(), Some("call" | "bl" | "jmp" | "b"))
                && w.next()
                    .is_some_and(|t| t.trim_end_matches("@PLT").ends_with("fabsl"))
        })
    };
    let src = "long double f(long double x) { return __builtin_fabsl(x); }\n\
               long double g(long double x) { return fabsl(x); }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for("fabsl_inline", src, &[opt]);
        assert!(!called(&asm), "{opt}: fabsl was called:\n{asm}");
        let asm = asm_for(
            "fabsl_inline_a64",
            src,
            &[opt, "--target=aarch64-unknown-linux-gnu"],
        );
        assert!(!called(&asm), "{opt} aarch64: fabsl was called:\n{asm}");
    }
}

/// `fabs` of a constant folds at -O1 and above: no sign-clearing instruction
/// is left. Checked for `long double` only where its negation folds too; a
/// binary128 `-3.5L` is a libgcc call before the optimizer sees it.
#[test]
fn builtins_fabs_of_a_constant_folds() {
    let mut src = String::from(
        "double f(void) { return __builtin_fabs(-3.5); }\n\
         float g(void) { return __builtin_fabsf(-2.0f); }\n",
    );
    if cfg!(target_arch = "x86_64") {
        src.push_str("long double h(void) { return __builtin_fabsl(-3.5L); }\n");
    }
    for opt in ["-O1", "-O2"] {
        let asm = asm_for("fabs_const", &src, &[opt]);
        assert!(
            !asm.lines().any(|l| matches!(
                l.split_whitespace().next(),
                Some("andpd" | "andps" | "fabs")
            )),
            "{opt}: fabs of a constant was not folded:\n{asm}"
        );
    }
}

/// A string gcc does not fold is not folded here either. `__builtin_nans`
/// has no library function, so it is an error -- gcc's is a link failure
/// against `__builtin_nans`. (The `__builtin_nan` library-call half runs in
/// tests/builtins/math.rs.)
#[test]
fn builtins_nan_of_a_string_that_does_not_fold() {
    compile_expect_error(
        "nans_malformed",
        "double f(void) { return __builtin_nans(\"zz\"); }\n",
        "is not a string literal naming a NaN payload",
    );
}

/// Neither `signbit` nor `copysign` is a call, at any width, on either
/// target: not to the libm function, and not to glibc's `__signbit*`.
#[test]
fn builtins_signbit_and_copysign_are_not_calls() {
    const CALLEES: &[&str] = &[
        "signbit",
        "signbitf",
        "signbitl",
        "signbitd",
        "copysign",
        "copysignf",
        "copysignl",
    ];
    let src = "double copysign(double, double); float copysignf(float, float);\n\
               long double copysignl(long double, long double);\n\
               int sf(float x) { return __builtin_signbit(x) + __builtin_signbitf(x); }\n\
               int sd(double x) { return __builtin_signbit(x); }\n\
               int sl(long double x) { return __builtin_signbit(x) + __builtin_signbitl(x); }\n\
               double cd(double x, double y) { return copysign(x, y) + __builtin_copysign(y, x); }\n\
               float cf(float x, float y) { return copysignf(x, y) + __builtin_copysignf(y, x); }\n\
               long double cl(long double x, long double y)\n\
               { return copysignl(x, y) + __builtin_copysignl(y, x); }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for("sign_ops_inline", src, &[opt]);
        assert!(!calls_any(&asm, CALLEES), "{opt}: a call remains:\n{asm}");
        let asm = asm_for(
            "sign_ops_inline_a64",
            src,
            &[opt, "--target=aarch64-unknown-linux-gnu"],
        );
        assert!(
            !calls_any(&asm, CALLEES),
            "{opt} aarch64: a call remains:\n{asm}"
        );
    }
}

/// glibc's `signbit` macro and the `copysign` family `<math.h>` declares
/// are the builtins, so a program using them needs no -lm either. (The same
/// program runs in tests/builtins/math.rs.)
#[test]
fn builtins_math_h_signbit_and_copysign() {
    let code = r#"
#include <math.h>
int main(void) {
    volatile double x = -2.0, z = 0.0;
    volatile float f = 1.0f;
    volatile long double l = -0.0L;
    if (!signbit(x) || signbit(z) || signbit(f) || !signbit(l)) return 1;
    if (copysign(3.0, x) != -3.0 || copysignf(f, -1.0f) != -1.0f) return 2;
    if (copysignl(5.0L, l) != -5.0L || !signbit(copysign(z, -1.0))) return 3;
    return 0;
}
"#;
    let asm = asm_for("sign_ops_math_h_asm", code, &["-O2"]);
    assert!(
        !calls_any(
            &asm,
            &[
                "signbit",
                "signbitf",
                "signbitl",
                "copysign",
                "copysignf",
                "copysignl"
            ]
        ),
        "a call remains:\n{asm}"
    );
}

/// `-fno-builtin-copysign` keeps the call to `copysign`, and only that one.
#[test]
fn builtins_copysign_fno_builtin_keeps_the_call() {
    let src = "double copysign(double, double); float copysignf(float, float);\n\
               double f(double x, double y) { return copysign(x, y); }\n\
               float g(float x, float y) { return copysignf(x, y); }\n";
    let asm = asm_for("copysign_nb", src, &["-fno-builtin-copysign"]);
    assert!(
        calls_any(&asm, &["copysign"]),
        "-fno-builtin-copysign kept copysign inline:\n{asm}"
    );
    assert!(
        !calls_any(&asm, &["copysignf"]),
        "-fno-builtin-copysign displaced copysignf:\n{asm}"
    );
}

/// As in gcc: `copysign`, `fabs` and `abs` of constants fold in a static
/// initializer -- but, being calls, are not integer constant expressions, so
/// an array bound of one at file scope is rejected. (The accepted half runs
/// in tests/builtins/math.rs.)
#[test]
fn builtins_sign_ops_in_constant_expressions() {
    compile_expect_error(
        "abs_not_ice",
        "int abs(int);\nint a[abs(-2)];\n",
        "variable length arrays cannot have file scope",
    );
}

/// `copysign` and `signbit` of constants fold at -O1 and above: nothing is
/// left to compute them.
#[test]
fn builtins_sign_ops_of_constants_fold() {
    let src = "double f(void) { return __builtin_copysign(3.5, -0.0); }\n\
               float g(void) { return __builtin_copysignf(2.0f, -1.0f); }\n\
               int h(void) { return __builtin_signbit(-2.0) + __builtin_signbit(-1.0f); }\n";
    for opt in ["-O1", "-O2"] {
        for target in [None, Some("--target=aarch64-unknown-linux-gnu")] {
            let mut args = vec![opt];
            if let Some(t) = target {
                args.push(t);
            }
            let asm = asm_for("sign_ops_const", src, &args);
            assert!(
                !calls_any(&asm, &["signbit", "signbitf", "copysign", "copysignf"]),
                "{opt} {target:?}: a call remains:\n{asm}"
            );
            assert!(
                !asm.lines().any(|l| matches!(
                    l.split_whitespace().next(),
                    Some("shrq" | "shrl" | "shlq" | "shll" | "orq" | "orl" | "lsr" | "lsl" | "orr")
                )),
                "{opt} {target:?}: a sign operation on a constant was not folded:\n{asm}"
            );
        }
    }
}
