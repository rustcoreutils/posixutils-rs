//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Math Builtins Mega-Test
//
// Consolidates: nan, nans, flt_rounds tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: Math builtins
// ============================================================================

#[test]
fn builtins_math_mega() {
    let code = r#"
int main(void) {
    // ========== __BUILTIN_NAN (returns 1-9) ==========
    {
        // Quiet NaN for double
        double d = __builtin_nan("");
        volatile double vd = d;

        // Quiet NaN for float
        float f = __builtin_nanf("");
        volatile float vf = f;

        // Quiet NaN for long double
        long double ld = __builtin_nanl("");
        volatile long double vld = ld;

        // The values exist and don't crash
    }

    // ========== __BUILTIN_NANS (signaling NaN) (returns 10-19) ==========
    {
        // Signaling NaN variants
        double d = __builtin_nans("");
        volatile double vd = d;

        float f = __builtin_nansf("");
        volatile float vf = f;

        long double ld = __builtin_nansl("");
        volatile long double vld = ld;
    }

    // ========== __BUILTIN_FLT_ROUNDS (returns 20-29) ==========
    {
        // Returns current rounding mode
        // 1 = round to nearest (IEEE 754 default)
        int mode = __builtin_flt_rounds();
        if (mode != 1) return 20;
    }

    // ========== __BUILTIN_HUGE_VAL (returns 30-39) ==========
    {
        // Positive infinity
        double inf = __builtin_huge_val();
        volatile double vinf = inf;

        float finf = __builtin_huge_valf();
        volatile float vfinf = finf;

        long double ldinf = __builtin_huge_vall();
        volatile long double vldinf = ldinf;

        // inf > any finite number
        if (!(inf > 1000000.0)) return 30;
        if (!(finf > 1000000.0f)) return 31;
    }

    // ========== __BUILTIN_INF (returns 40-49) ==========
    {
        // Same as huge_val on most systems
        double inf = __builtin_inf();
        volatile double vinf = inf;

        float finf = __builtin_inff();
        volatile float vfinf = finf;

        long double ldinf = __builtin_infl();
        volatile long double vldinf = ldinf;

        if (!(inf > 1000000.0)) return 40;
    }

    // ========== FLOATING POINT BUILTINS (returns 60-79) ==========
    {
        // __builtin_fabs
        double fa = __builtin_fabs(-3.14);
        if (fa < 3.0 || fa > 3.2) return 62;

        fa = __builtin_fabs(3.14);
        if (fa < 3.0 || fa > 3.2) return 63;

        float ff = __builtin_fabsf(-2.5f);
        if (ff < 2.4f || ff > 2.6f) return 64;
    }

    // ========== __BUILTIN_SIGNBIT (returns 80-99) ==========
    {
        // signbit returns non-zero for negative, zero for non-negative
        // Test double (signbit)
        double neg_d = -1.0;
        double pos_d = 1.0;
        double zero_d = 0.0;
        double neg_zero_d = -0.0;

        if (!__builtin_signbit(neg_d)) return 80;
        if (__builtin_signbit(pos_d)) return 81;
        if (__builtin_signbit(zero_d)) return 82;
        if (!__builtin_signbit(neg_zero_d)) return 83;  // -0.0 has sign bit set

        // Test float (signbitf)
        float neg_f = -2.5f;
        float pos_f = 2.5f;
        float zero_f = 0.0f;
        float neg_zero_f = -0.0f;

        if (!__builtin_signbitf(neg_f)) return 84;
        if (__builtin_signbitf(pos_f)) return 85;
        if (__builtin_signbitf(zero_f)) return 86;
        if (!__builtin_signbitf(neg_zero_f)) return 87;

        // Test with infinity
        double neg_inf = -__builtin_inf();
        double pos_inf = __builtin_inf();
        if (!__builtin_signbit(neg_inf)) return 88;
        if (__builtin_signbit(pos_inf)) return 89;

        // Test result is usable as int
        int sb = __builtin_signbit(-5.0);
        if (sb == 0) return 90;
        sb = __builtin_signbit(5.0);
        if (sb != 0) return 91;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("builtins_math_mega", code, &["-lm".to_string()]),
        0
    );
}

/// The floating classification builtins agree with gcc at every width.
///
/// `__builtin_isnan` and friends are lowered as comparisons rather than bit
/// tests, which keeps them exact for `long double` and needs no backend work.
/// They were also deliberately not expressed with `fabs`, because
/// `__builtin_fabsl` used to narrow a `long double` to a double -- that is
/// #C121 and is fixed, but a comparison is still the cheaper lowering.
///
/// Checked against gcc on the same source at -O0 and -O2.
#[test]
fn builtins_float_classification_matches_gcc() {
    let code = r#"
#include <float.h>

static volatile double dz = 0.0;
static volatile float  fz = 0.0f;

/* nan, inf, finite, normal -- packed into one integer per case */
#define BITS(x) ( (__builtin_isnan(x)    ? 8 : 0) \
                | (__builtin_isinf(x)    ? 4 : 0) \
                | (__builtin_isfinite(x) ? 2 : 0) \
                | (__builtin_isnormal(x) ? 1 : 0) )

int main(void)
{
    double dn = dz / dz, di = 1.0 / dz, ds = 2.2250738585072014e-308 / 4.0;
    float  fn = fz / fz, fi = 1.0f / fz, fs = 1.17549435e-38f / 4.0f;
    long double ln = (long double)dn, li = (long double)di;
    /* Spelled from <float.h> rather than as a fixed exponent: the smallest
       normal is 2^-16382 for x87 and binary128 but 2^-1022 where long double
       is double, and a fixed exponent underflows to zero there -- turning a
       normal into a zero and testing nothing. */
    long double ls = LDBL_TRUE_MIN, lmin = LDBL_MIN;

    if (BITS(dn)  != 8) return 1;
    if (BITS(di)  != 4) return 2;
    if (BITS(-di) != 4) return 3;
    if (BITS(0.0) != 2) return 4;
    if (BITS(ds)  != 2) return 5;
    if (BITS(1.5) != 3) return 6;

    if (BITS(fn)   != 8) return 7;
    if (BITS(fi)   != 4) return 8;
    if (BITS(0.0f) != 2) return 9;
    if (BITS(fs)   != 2) return 10;
    if (BITS(1.5f) != 3) return 11;

    /* long double is the width that used to be unreachable: on x87 and
       binary128 its smallest normal is 2^-16382, which an f64 cannot
       represent at all. */
    if (BITS(ln)   != 8) return 12;
    if (BITS(li)   != 4) return 13;
    if (BITS(-li)  != 4) return 14;
    if (BITS(0.0L) != 2) return 15;
    if (BITS(ls)   != 2) return 16;
    if (BITS(lmin) != 3) return 17;
    if (BITS(1.5L) != 3) return 18;
    if (BITS(-1.5L) != 3) return 19;

    /* isnan must be exactly 1, not merely non-zero: that is the whole
       difference from glibc's fallback, which answers 65535. */
    if (__builtin_isnan(ln) != 1) return 20;

    /* fpclassify picks the same class. */
    if (__builtin_fpclassify(0,1,2,3,4, dn)   != 0) return 30;
    if (__builtin_fpclassify(0,1,2,3,4, di)   != 1) return 31;
    if (__builtin_fpclassify(0,1,2,3,4, 1.5)  != 2) return 32;
    if (__builtin_fpclassify(0,1,2,3,4, ds)   != 3) return 33;
    if (__builtin_fpclassify(0,1,2,3,4, 0.0)  != 4) return 34;
    if (__builtin_fpclassify(0,1,2,3,4, ln)   != 0) return 35;
    if (__builtin_fpclassify(0,1,2,3,4, li)   != 1) return 36;
    if (__builtin_fpclassify(0,1,2,3,4, 1.5L) != 2) return 37;
    if (__builtin_fpclassify(0,1,2,3,4, ls)   != 3) return 38;
    if (__builtin_fpclassify(0,1,2,3,4, 0.0L) != 4) return 39;

    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_fp_classify", code, &[]), 0);
}

/// `isnan(x)` answers 1, not 65535.
///
/// glibc's `<math.h>` only uses `__builtin_isnan` once the compiler claims
/// GCC 4.4; below that it takes a `sizeof` ternary that calls `__isnanl`,
/// which returns raw class bits. Both conform — C99 7.12.3.4 asks only for "a
/// nonzero value" — but `isnan(x) == 1` is what real code writes, and it is
/// what gcc gives. Reaching that path also required `__float128`, since
/// `bits/floatn.h` turns on `__HAVE_FLOAT128` one threshold *below* it.
///
/// This is audit finding #C7.
#[test]
fn builtins_isnan_answers_one_at_every_width() {
    let code = r#"
#include <math.h>

static volatile double dz = 0.0;

int main(void)
{
    double dn = dz / dz;
    float fn = (float)dn;
    long double ln = (long double)dn;

    /* Exactly 1, not merely nonzero. */
    if (isnan(dn) != 1) return 1;
    if (isnan(fn) != 1) return 2;
    if (isnan(ln) != 1) return 3;

    /* And still 0 for a number. */
    if (isnan(1.0) != 0) return 4;
    if (isnan(1.0f) != 0) return 5;
    if (isnan(1.0L) != 0) return 6;

    /* `__builtin_isinf_sign`, which glibc's `isinf` uses once __float128 is
       on: the sign of the infinity, or zero. */
    double inf = 1.0 / dz;
    if (__builtin_isinf_sign(inf) != 1) return 7;
    if (__builtin_isinf_sign(-inf) != -1) return 8;
    if (__builtin_isinf_sign(1.0) != 0) return 9;
    if (__builtin_isinf_sign(dn) != 0) return 10;

    /* The classification macros still agree with themselves. */
    if (!isinf(inf) || isinf(1.0)) return 11;
    if (!isfinite(1.0) || isfinite(inf)) return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_isnan_is_one", code, &[]), 0);
}

/// `__builtin_fabsl` and `__builtin_signbitl` operate on a `long double`, not
/// on its low eight bytes (#C121).
///
/// Both lowered to the *`double`* opcode, whose emitter moves the argument as a
/// `double` and calls `fabs` / `__signbit`. On x86-64 a `long double` is the
/// 80-bit x87 format, so that read its mantissa:
/// `__builtin_fabsl(-3.5L)` returned **2.5e-4932**. They are ordinary calls to
/// `fabsl` and `__signbitl` now, which gets the long-double ABI from the call
/// path that already carries one for `__mulxc3`.
///
/// The `signbit` family is also normalised to 0/1. C17 7.12.3.6 permits any
/// nonzero value and the library entry points return the sign bit in place --
/// 8, 128 and 512 for the three widths, which did not even agree with each
/// other. gcc is no more consistent: on x86-64 it answers 512 for a runtime
/// `long double` and 1 for a constant one, and on aarch64 it answers 1 for
/// both. Everything here is conforming; 0/1 is merely predictable.
#[test]
fn builtins_long_double_magnitude_and_sign() {
    let code = r#"
int main(void) {
    /* Through an array so the value is not folded at compile time -- the
       constant form was always right, which is what hid this. */
    long double v[] = { -3.5L, 3.5L, -0.0L, 0.0L };

    if (__builtin_fabsl(v[0]) != 3.5L) return 1;
    if (__builtin_fabsl(v[1]) != 3.5L) return 2;
    if (__builtin_fabsl(v[2]) != 0.0L) return 3;

    if (__builtin_signbitl(v[0]) != 1) return 4;
    if (__builtin_signbitl(v[1]) != 0) return 5;
    if (__builtin_signbitl(v[2]) != 1) return 6;      /* -0.0 is negative */
    if (__builtin_signbitl(v[3]) != 0) return 7;

    /* fabsl clears the sign, so the result is never negative. */
    if (__builtin_signbitl(__builtin_fabsl(v[0])) != 0) return 8;
    if (__builtin_signbitl(__builtin_fabsl(v[2])) != 0) return 9;

    /* The narrower widths, which were already right, and their 0/1 answers. */
    double d[] = { -2.5, 2.5 };
    float  f[] = { -1.5f, 1.5f };
    if (__builtin_fabs(d[0]) != 2.5) return 10;
    if (__builtin_fabsf(f[0]) != 1.5f) return 11;
    if (__builtin_signbit(d[0]) != 1) return 12;
    if (__builtin_signbit(d[1]) != 0) return 13;
    if (__builtin_signbitf(f[0]) != 1) return 14;
    if (__builtin_signbitf(f[1]) != 0) return 15;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("long_double_magnitude", code, &["-lm".to_string()]),
        0
    );
}
