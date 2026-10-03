//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// <tgmath.h> dispatch (C17 7.25p3) and the type of `I` (7.3.1p4).
//
// An integer argument counts as `double`: `pow(i, f)` for an `int i` and a
// `float f` is `pow`, where the usual arithmetic conversions alone give
// `float` and called `powf`, rounding `i`. A real argument to a complex-only
// macro is the complex type of its own precision. `I` is `float _Complex`,
// and a `double _Complex` one widened every float complex expression.
//

use crate::common::compile_and_run;

#[test]
fn tgmath_integer_arguments_count_as_double() {
    let src = r#"
#include <tgmath.h>
#define T(x) _Generic((x), float: 2, double: 3, long double: 4, \
    float _Complex: 5, double _Complex: 6, long double _Complex: 7, default: 99)
int i = 16777217; float fl = 2.0f; long double ld = 2.0L;
float _Complex fc = 1.0f;
int main(void) {
    int got[] = {
        T(pow(i, fl)), T(pow(fl, i)), T(atan2(i, fl)), T(fmax(fl, i)),
        T(fma(i, fl, fl)), T(nextafter(fl, i)), T(pow(i, fc)), T(pow(fl, fl)),
        T(cimag(fl)), T(creal(I)), T(conj(fl)), T(pow(ld, i)),
        T(sqrt(i)), T(fabs(fc)), T(carg(i)), T(remquo(fl, i, &i)),
    };
    int want[] = { 3, 3, 3, 3, 3, 3, 6, 2, 2, 2, 5, 4, 3, 2, 3, 3 };
    for (unsigned k = 0; k < sizeof want / sizeof want[0]; k++)
        if (got[k] != want[k]) return 1 + k;
    /* The integer reached the function unrounded. */
    if (fmax(fl, 16777217) != 16777217.0) return 50;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("tgmath_int_args", src, &["-lm".to_string()]),
        0
    );
}

#[test]
fn complex_i_is_float_complex() {
    let src = r#"
#include <complex.h>
#define T(x) _Generic((x), float _Complex: 5, double _Complex: 6, default: 99)
int main(void) {
    if (sizeof(I) != sizeof(float _Complex)) return 1;
    if (T(I) != 5 || T(1.0f * I) != 5 || T(1 * I) != 5 || T(_Complex_I) != 5) return 2;
    if (T(1.0 * I) != 6) return 3;
    float _Complex z = 1.0f + 2.0f * I;
    if (crealf(z) != 1.0f || cimagf(z) != 2.0f) return 4;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("complex_i_float", src, &["-lm".to_string()]),
        0
    );
}
