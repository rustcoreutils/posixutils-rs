//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C23's _FloatN and _FloatNx types (TS 18661-3), which c17 has as gcc does:
// distinct types in the formats of the standard ones. glibc relies on that
// once the compiler claims gcc 7 -- its <math.h> lists `float:` and
// `_Float32:` in one `_Generic`.
//

use crate::common::compile_and_run;

/// The glibc headers take their gcc 7 paths, and the type-generic macros of
/// both <math.h> and <tgmath.h> reach the function of each argument's type --
/// `sqrtf32` where the C library has it, else `sqrtf`, of the same format.
#[test]
fn float_n_through_the_c_library() {
    let src = r#"
#include <math.h>
#include <tgmath.h>
#include <complex.h>
int main(void) {
#if __GNUC__ != 7
    return 1;
#endif
    _Float32 a = 2.0f32;
    _Float64 b = 4.0f64;
    _Float32x c = 9.0f32x;
    if (sqrt(b) != 2 || sqrt(c) != 3) return 2;
    if (_Generic(sqrt(a), _Float32: 0, float: 0, default: 1)) return 3;
    if (_Generic(sqrt(c), _Float32x: 0, double: 0, default: 1)) return 4;
    if (!isnan(__builtin_nanf32x("")) || !isinf(__builtin_inff32x())) return 5;
    if (!signbit(-a) || isnan(a) || !isfinite(c)) return 6;
    _Complex _Float32 z = 1 + 2 * I;
    if (cimag(z) != 2) return 7;
#ifdef __FLT64X_MANT_DIG__
    _Float64x d = 16.0f64x;
    if (sqrt(d) != 4 || fabs(-d) != d) return 8;
    if (_Generic(sqrt(d), _Float64x: 0, long double: 0, default: 1)) return 9;
    if (!isinf(__builtin_huge_valf64x()) || __builtin_copysignf64x(1, -d) != -1) return 10;
#endif
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("float_n_libc", src, &["-lm".to_string()]),
        0
    );
}

/// The types themselves: distinct in `_Generic`, interchange over standard
/// over extended in the usual arithmetic conversions, `_Float32` passed
/// through `...` as itself, and the <float.h> families on request.
#[test]
fn float_n_types_and_conversions() {
    let src = r#"
#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>
#include <stdarg.h>
#define T(x) _Generic((x), float: 1, double: 2, _Float32: 3, _Float64: 4, _Float32x: 5, default: 0)
static double second(int n, ...) {
    va_list ap;
    va_start(ap, n);
    _Float32 first = va_arg(ap, _Float32);
    double second = va_arg(ap, double);
    va_end(ap);
    return first + second;
}
int main(void) {
    _Float32 a = 1; _Float64 b = 1; _Float32x c = 1; float f = 1; double d = 1;
    if (T(a + f) != 3 || T(f + a) != 3) return 1;
    if (T(b + d) != 4 || T(c + d) != 2 || T(c + b) != 4) return 2;
    if (T(a + 1) != 3 || T(a + d) != 2 || T(c * c) != 5) return 3;
    if (sizeof(_Float32) != 4 || sizeof(_Float32x) != 8) return 4;
    if (second(2, a + 0.5f32, 2.25) != 3.75) return 5;
    if (FLT32_MANT_DIG != 24 || FLT64_MAX != DBL_MAX || FLT32X_EPSILON != DBL_EPSILON) return 6;
    if (FLT32_MAX != FLT_MAX || FLT32_TRUE_MIN != FLT_TRUE_MIN) return 7;
#ifdef __FLT64X_MANT_DIG__
    if (FLT64X_MANT_DIG != LDBL_MANT_DIG || FLT64X_MAX != LDBL_MAX) return 8;
#endif
    return 0;
}
"#;
    assert_eq!(compile_and_run("float_n_types", src, &[]), 0);
}
