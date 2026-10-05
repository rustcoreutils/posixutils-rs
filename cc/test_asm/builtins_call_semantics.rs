//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A builtin that stands for a library function is still a call: the
// compile-only cases of tests/builtins/call_semantics.rs, in process.
//
// C17 7.1.4p1 lets an implementation compute any library function in place,
// but the program is still calling a function: its arguments are checked and
// converted against the prototype (6.5.2.2p2, p7), and its result is a value,
// not an object (6.5.2.2p5). c17 computes `abs`, `fabs`, `creal`, `conj` and
// their siblings in place, and reaches the libm entry points and the
// `__builtin_` library aliases through synthesized calls; none of that may
// change what the program means.
//

use crate::test_compile::compile;

/// The diagnostic text after `error: ` / `warning: `, one per line, with the
/// file and position stripped: what a reader is told, not where.
fn messages(stderr: &str) -> Vec<String> {
    stderr
        .lines()
        .filter_map(|l| {
            ["error: ", "warning: "]
                .iter()
                .find_map(|k| l.find(k).map(|i| l[i..].to_string()))
        })
        .collect()
}

// ============================================================================
// The result is a value, not an object
// ============================================================================

/// `creal(z) = 5.0` is an assignment to a function's return value. It became
/// `__real__ z` -- which gcc makes an lvalue when `z` is one -- and so
/// compiled, and wrote through into `z`.
#[test]
fn builtin_library_call_result_is_not_an_lvalue() {
    let decls = "double creal(double _Complex); double cimag(double _Complex);\n\
                 float crealf(float _Complex); long double cimagl(long double _Complex);\n\
                 double _Complex conj(double _Complex);\n\
                 int abs(int); double fabs(double); double copysign(double, double);\n";
    let cases = [
        "creal(z) = 5.0;",
        "cimag(z) += 1.0;",
        "++creal(z);",
        "cimag(z)--;",
        "double *p = &creal(z); (void)p;",
        "crealf(fz) = 1.0f;",
        "cimagl(lz) = 1.0L;",
        "__builtin_creal(z) = 5.0;",
        "__builtin_cimag(z) *= 2.0;",
        "double *q = &__builtin_cimag(z); (void)q;",
        "conj(z) = z;",
        "abs(i) = 1;",
        "fabs(d) = 1.0;",
        "copysign(d, d) = 1.0;",
        "__builtin_copysign(d, d) += 1.0;",
    ];
    for stmt in cases {
        let src = format!(
            "{decls}void f(void) {{ double _Complex z = 1.0; float _Complex fz = 1.0f;\n\
             long double _Complex lz = 1.0L; int i = 1; double d = 1.0;\n\
             {stmt}\n}}\n"
        );
        let c = compile("call_semantics", &src, &[]);
        let (ok, stderr) = (c.success, c.stderr);
        assert!(!ok, "accepted `{stmt}`:\n{stderr}");
        assert!(
            stderr.contains("lvalue required"),
            "`{stmt}` was rejected for the wrong reason:\n{stderr}"
        );
    }

    // The GNU operators the accessors are built from are still lvalues: that
    // half runs in tests/builtins/call_semantics.rs.
}

// ============================================================================
// The arguments are checked against the prototype
// ============================================================================

/// Each builtin call, next to an ordinary function with the builtin's own
/// prototype and the same arguments. Whatever the ordinary call is told --
/// an error, a warning, or nothing -- the builtin call must be told too.
const PARITY: &[(&str, &str, &str)] = &[
    // (prototype, builtin call, preamble)
    ("int F(int)", "abs(s)", "struct S { int a; } s = { -3 };"),
    (
        "int F(int)",
        "__builtin_abs(s)",
        "struct S { int a; } s = { -3 };",
    ),
    ("long F(long)", "labs(p)", "int *p = 0;"),
    (
        "long long F(long long)",
        "llabs(s)",
        "struct S { int a; } s;",
    ),
    ("long F(long)", "imaxabs(s)", "struct S { int a; } s;"),
    ("double F(double)", "fabs(s)", "struct S { double a; } s;"),
    ("float F(float)", "__builtin_fabsf(p)", "int *p = 0;"),
    (
        "long double F(long double)",
        "fabsl(s)",
        "struct S { double a; } s;",
    ),
    ("double F(double)", "floor(s)", "struct S { double a; } s;"),
    ("double F(double)", "__builtin_ceil(p)", "int *p = 0;"),
    ("float F(float)", "__builtin_floorf(p)", "int *p = 0;"),
    (
        "double F(double, double)",
        "fmin(s, 1.0)",
        "struct S { double a; } s;",
    ),
    (
        "float F(float, float)",
        "__builtin_fmaxf(1.0f, p)",
        "int *p = 0;",
    ),
    (
        "double F(double, double, double)",
        "fma(1.0, s, 2.0)",
        "struct S { double a; } s;",
    ),
    ("float F(float, float, float)", "fmaf(1.0f, 2.0f)", ""),
    ("double F(double, double)", "__builtin_fmin(1.0)", ""),
    ("float F(float)", "rintf(s)", "struct S { float a; } s;"),
    ("double F(double)", "nearbyint(1.0, 2.0)", ""),
    ("float F(float)", "roundf()", ""),
    (
        "double F(double)",
        "__builtin_trunc(s)",
        "struct S { double a; } s;",
    ),
    (
        "double F(double _Complex)",
        "creal(s)",
        "struct S { double a, b; } s;",
    ),
    ("float F(float _Complex)", "cimagf(p)", "int *p = 0;"),
    (
        "double F(double _Complex)",
        "__builtin_creal(s)",
        "struct S { double a, b; } s;",
    ),
    (
        "double _Complex F(double _Complex)",
        "conj(s)",
        "struct S { double a, b; } s;",
    ),
    (
        "double F(double, double)",
        "copysign(s, 1.0)",
        "struct S { double a; } s;",
    ),
    (
        "float F(float, float)",
        "__builtin_copysignf(1.0f, p)",
        "int *p = 0;",
    ),
    (
        "long double F(long double, long double)",
        "copysignl(1.0L, s)",
        "struct S { double a; } s;",
    ),
    (
        "double F(double, double)",
        "__builtin_copysign(p, s)",
        "int *p = 0; struct S { double a; } s;",
    ),
    // Arity.
    ("int F(int)", "abs(1, 2)", ""),
    ("double F(double, double)", "copysign(1.0)", ""),
    (
        "double F(double, double)",
        "__builtin_copysign(1.0, 2.0, 3.0)",
        "",
    ),
    ("float F(float, float)", "copysignf()", ""),
    ("int F(int)", "abs()", ""),
    ("double F(double)", "fabs(1.0, 2.0)", ""),
    ("double F(double _Complex)", "creal()", ""),
    ("double F(double)", "floor(1.0, 2.0)", ""),
    ("double F(double)", "sqrt(s)", "struct S { double a; } s;"),
    ("float F(float)", "__builtin_sqrtf(p)", "int *p = 0;"),
    (
        "long double F(long double)",
        "sqrtl(s)",
        "struct S { double a; } s;",
    ),
    ("double F(double)", "__builtin_sqrt(p)", "int *p = 0;"),
    ("double F(double)", "sqrt(1.0, 2.0)", ""),
    ("float F(float)", "sqrtf()", ""),
    // A `__builtin_` library alias, checked against the declaration in scope.
    (
        "unsigned long F(const char *)",
        "__builtin_strlen(s)",
        "struct S { int a; } s;",
    ),
    ("unsigned long F(const char *)", "__builtin_strlen(1.5)", ""),
];

#[test]
fn builtin_library_call_arguments_are_checked_like_a_call() {
    let decls = "int abs(int); long labs(long); long long llabs(long long);\n\
                 long imaxabs(long); double fabs(double); float fabsf(float);\n\
                 long double fabsl(long double); double floor(double); double ceil(double);\n\
                 double creal(double _Complex); float cimagf(float _Complex);\n\
                 double _Complex conj(double _Complex);\n\
                 double copysign(double, double); float copysignf(float, float);\n\
                 long double copysignl(long double, long double);\n\
                 double sqrt(double); float sqrtf(float); long double sqrtl(long double);\n\
                 double nearbyint(double); float floorf(float); float rintf(float);\n\
                 float roundf(float); double trunc(double);\n\
                 double fmin(double, double); float fmaxf(float, float);\n\
                 double fma(double, double, double); float fmaf(float, float, float);\n\
                 unsigned long strlen(const char *);\n";
    for (proto, call, pre) in PARITY {
        // The ordinary call: the builtin's name replaced by `F`, declared with
        // the builtin's prototype.
        let open = call.find('(').unwrap();
        let twin_call = format!("F{}", &call[open..]);
        let twin = format!("{decls}{proto};\nvoid f(void) {{ {pre} (void){twin_call}; }}\n");
        let real = format!("{decls}void f(void) {{ {pre} (void){call}; }}\n");
        let twin_c = compile("call_semantics", &twin, &[]);
        let real_c = compile("call_semantics", &real, &[]);
        let (twin_ok, twin_err) = (twin_c.success, twin_c.stderr);
        let (real_ok, real_err) = (real_c.success, real_c.stderr);
        assert!(
            !twin_err.is_empty(),
            "the twin of `{call}` draws no diagnostic, so this case proves nothing"
        );
        assert_eq!(
            real_ok, twin_ok,
            "`{call}` accepted={real_ok}, ordinary call accepted={twin_ok}\n\
             builtin:\n{real_err}\nordinary:\n{twin_err}"
        );
        // An argument diagnostic names the callee as it was spelled.
        let named = |m: String| m.replace("'F'", &format!("'{}'", &call[..open]));
        assert_eq!(
            messages(&real_err),
            messages(&twin_err)
                .into_iter()
                .map(named)
                .collect::<Vec<_>>(),
            "`{call}` is diagnosed differently from an ordinary call"
        );
    }
}
