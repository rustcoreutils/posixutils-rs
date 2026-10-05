//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A builtin that stands for a library function is still a call
//
// C17 7.1.4p1 lets an implementation compute any library function in place,
// but the program is still calling a function: its arguments are checked and
// converted against the prototype (6.5.2.2p2, p7), and its result is a value,
// not an object (6.5.2.2p5). c17 computes `abs`, `fabs`, `creal`, `conj` and
// their siblings in place, and reaches the libm entry points and the
// `__builtin_` library aliases through synthesized calls; none of that may
// change what the program means.
//

use crate::common::compile_and_run;

// ============================================================================
// The result is a value, not an object
// ============================================================================

/// `creal(z) = 5.0` is an assignment to a function's return value. It became
/// `__real__ z` -- which gcc makes an lvalue when `z` is one -- and so
/// compiled, and wrote through into `z`.
#[test]
fn builtin_library_call_result_is_not_an_lvalue() {
    // The diagnostic half is a unit test in cc/test_asm/builtins_call_semantics.rs.

    // The GNU operators the accessors are built from are still lvalues.
    let code = r#"
int main(void) {
    double _Complex z = 1.0;
    __real__ z = 5.0;
    __imag__ z += 2.0;
    double *p = &__real__ z;
    *p += 1.0;
    return (__real__ z == 6.0 && __imag__ z == 2.0) ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("gnu_real_imag_lvalue", code, &[]), 0);
}

/// Legal calls stay legal and keep their values: the conversions the
/// prototype asks for still happen.
#[test]
fn builtin_library_call_valid_arguments_convert() {
    let code = r#"
int abs(int); long labs(long); double fabs(double);
double creal(double _Complex); double cimag(double _Complex);
double _Complex conj(double _Complex);
int main(void) {
    signed char c = -3;                      /* plain char is unsigned on aarch64 Linux */
    if (abs(c) != 3) return 1;               /* signed char promotes to int */
    if (labs(-4) != 4L) return 2;            /* int converts to long */
    if (abs(-5.9) != 5) return 3;            /* double converts to int */
    if (fabs(-2) != 2.0) return 4;           /* int converts to double */
    if (creal(7) != 7.0 || cimag(7) != 0.0) return 5;
    float _Complex w = 1.0f + 2.0fi;
    if (conj(w) != 1.0 - 2.0i) return 6;     /* float _Complex widens */
    _Bool b = 1;
    if (abs(b) != 1) return 7;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("builtin_call_convert", code, &["-lm".to_string()]),
        0
    );
}
