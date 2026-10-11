//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_tgmath`, gcc 8's builtin behind glibc's <tgmath.h>: which of
// its functions a call picks, by C2x's rules, and what gcc refuses. Every
// expected pick is gcc 13's.
//

use crate::common::compile_expect_error;

#[cfg(target_os = "linux")]
const FUNCTIONS: &str = r#"
static int F(float x) { return 1; }
static int D(double x) { return 2; }
static int L(long double x) { return 3; }
static int F32(_Float32 x) { return 4; }
static int F64(_Float64 x) { return 5; }
static int F128(_Float128 x) { return 6; }
static int F32X(_Float32x x) { return 7; }
static int F64X(_Float64x x) { return 8; }
static int CF(_Complex float x) { return 9; }
static int CD(_Complex double x) { return 10; }
static int CL(_Complex long double x) { return 11; }
static int F2(float x, float y) { return 1; }
static int D2(double x, double y) { return 2; }
static int L2(long double x, long double y) { return 3; }
static int F322(_Float32 x, _Float32 y) { return 4; }
static int F642(_Float64 x, _Float64 y) { return 5; }
static int F1282(_Float128 x, _Float128 y) { return 6; }
static int F32X2(_Float32x x, _Float32x y) { return 7; }
static int F64X2(_Float64x x, _Float64x y) { return 8; }
static int CF2(_Complex float x, _Complex float y) { return 9; }
static int CD2(_Complex double x, _Complex double y) { return 10; }
static int LDX(double x, int e) { return 2; }
static int LDXF(float x, int e) { return 1; }
#define U(x) __builtin_tgmath(F, D, L, F32, F64, F128, F32X, F64X, CF, CD, CL, x)
#define UR(x) __builtin_tgmath(F, D, L, x)
#define UC(x) __builtin_tgmath(CF, CD, CL, x)
#define B(x, y) __builtin_tgmath(F2, D2, L2, F322, F642, F1282, F32X2, F64X2, CF2, CD2, x, y)
"#;

/// The function picked: the argument's own type for one generic parameter
/// (an integer is `double`), the usual arithmetic conversions of C2x for
/// more -- `_FloatN` over the standard type of its format, the standard
/// type over `_FloatNx` -- complex where any argument is, and a complex
/// function for a real argument where there is no real one.
#[test]
#[cfg(target_os = "linux")]
fn tgmath_picks_gccs_function() {
    let cases: &[(&str, i32)] = &[
        ("U(1.0f)", 1),
        ("U(1)", 2),
        ("U(1.0)", 2),
        ("U(1.0L)", 3),
        ("U(1.0f32)", 4),
        ("U(1.0f64)", 5),
        ("U(1.0f128)", 6),
        ("U((_Float32x)1)", 7),
        ("U((_Float64x)1)", 8),
        ("U((_Complex float)1)", 9),
        ("U((_Complex double)1)", 10),
        ("U((char)1)", 2),
        ("U(1ULL)", 2),
        ("UR(1.0f)", 1),
        ("UR(1)", 2),
        ("UC(1.0f)", 9),
        ("UC(1)", 10),
        ("UC(1.0L)", 11),
        ("B(1.0f, 1.0f)", 1),
        ("B(1.0f, 1.0)", 2),
        ("B(1.0f, 1)", 2),
        ("B(1, 1)", 2),
        ("B(1.0f32, 1.0f)", 4),
        ("B(1.0f, 1.0f32)", 4),
        ("B(1.0f64, 1.0)", 5),
        ("B(1.0, 1.0f64)", 5),
        ("B((_Float32x)1, 1.0f)", 7),
        ("B((_Float32x)1, 1.0)", 2),
        ("B(1.0f128, 1.0L)", 6),
        ("B((_Float64x)1, 1.0L)", 3),
        ("B(1.0L, (_Float64x)1)", 3),
        ("B(1.0f, (_Complex float)1)", 9),
        ("B(1.0, (_Complex float)1)", 10),
        ("B(1, (_Complex float)1)", 10),
        ("__builtin_tgmath(LDXF, LDX, 1.0f, 3)", 1),
        ("__builtin_tgmath(LDXF, LDX, 1, 3)", 2),
    ];
    let mut body = String::new();
    for (i, (call, want)) in cases.iter().enumerate() {
        body += &format!("    if ({call} != {want}) return {};\n", i + 1);
    }
    // The arguments are evaluated once, and the call's value is the
    // function's.
    body += "    int n = 0;\n    if (U((n++, 1.0f)) != 1 || n != 1) return 100;\n";
    let src = format!("{FUNCTIONS}int main(void) {{\n{body}    return 0;\n}}\n");
    crate::common::compile_and_run_everywhere("tgmath_picks", &src);
}

/// What gcc refuses, in its words.
#[test]
fn tgmath_refusals_match_gcc() {
    let decls = "double sq(double); float sqf(float); long double sql(long double);\n\
                 double ld(double, int);\n";
    for (call, message) in [
        (
            "__builtin_tgmath(sq, 1.0)",
            "too few arguments to '__builtin_tgmath'",
        ),
        (
            "__builtin_tgmath(sqf)",
            "too few arguments to '__builtin_tgmath'",
        ),
        (
            "__builtin_tgmath(sqf, sq, 1.0L)",
            "no matching function for type-generic call",
        ),
        (
            "__builtin_tgmath(sqf, sq, 1.0i)",
            "no matching function for type-generic call",
        ),
        (
            "__builtin_tgmath(sqf, sq, 1.0, 2.0)",
            "argument 3 of '__builtin_tgmath' is not a function pointer",
        ),
        (
            "__builtin_tgmath(1, sq, 1.0)",
            "argument 1 of '__builtin_tgmath' is not a function pointer",
        ),
        (
            "__builtin_tgmath(sqf, sq, (void *)0)",
            "invalid type of argument 1 of type-generic function",
        ),
        (
            "__builtin_tgmath(sqf, ld, 1.0)",
            "argument 2 of '__builtin_tgmath' has wrong number of arguments",
        ),
        (
            "__builtin_tgmath(sqf, sqf, 1.0)",
            "function arguments of '__builtin_tgmath' all have the same type",
        ),
    ] {
        let src = format!("{decls}void f(void) {{ (void){call}; }}\n");
        compile_expect_error("tgmath_refused", &src, message);
    }
}
