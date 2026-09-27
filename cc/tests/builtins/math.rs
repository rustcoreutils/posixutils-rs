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

use crate::common::{
    asm_for_at, asm_symbol, compile_and_run, compile_and_run_aarch64, compile_expect_error,
};

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
/// on its low eight bytes: on x86-64 that is the 80-bit x87 format, and
/// reading it as a `double` reads its mantissa. Both are computed in place at
/// the `long double` width.
///
/// The `signbit` family answers 0/1. C17 7.12.3.6 permits any nonzero value,
/// and gcc is not consistent: it answers 1 for a constant, and at run time
/// the bit in place -- `INT_MIN` for a `float`, and 512 for a `long double`
/// on x86-64. Everything here is conforming; 0/1 is merely predictable.
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
    assert_eq!(compile_and_run("long_double_magnitude", code, &[]), 0);
    if let Some(rc) = compile_and_run_aarch64("long_double_magnitude_a64", code, "-O2") {
        assert_eq!(rc, 0);
    }
}

/// A library builtin taking `double` must be declared taking `double`.
///
/// When the header that would declare `sqrt` or `copysign` has not been
/// included, these builtins synthesize a declaration. Every synthesized
/// parameter was an `unsigned long` — right for the `_chk` family, whose
/// arguments are pointers, sizes and flags, and wrong for a `double`, which
/// the ABI passes in an SSE register instead. The argument went to the wrong
/// register file entirely, so `__builtin_sqrt(4.0)` read whatever was in xmm0
/// and answered 0.0, and `__builtin_copysign(1.0, -1.0)` answered 1.0 because
/// the sign argument never arrived.
///
/// Silent: the call links and runs. The library functions of the same name
/// were always correct, which is what narrowed it to this path.
///
/// No `<math.h>` here on purpose — including it declares them properly and
/// the synthesized path is never taken.
#[test]
fn builtins_library_math_builtins_take_doubles() {
    let code = r#"
int main(void) {
    if (__builtin_sqrt(4.0) != 2.0) return 1;
    if (__builtin_sqrt(9.0) != 3.0) return 2;
    if (__builtin_sqrt(0.0) != 0.0) return 3;

    if (__builtin_copysign(1.0, -1.0) != -1.0) return 4;
    if (__builtin_copysign(-1.0, 1.0) != 1.0) return 5;
    if (__builtin_copysign(-5.0, 2.0) != 5.0) return 6;
    /* The magnitude has to survive too: this answered 0.0, not -3.0. */
    if (__builtin_copysign(3.0, -0.0) != -3.0) return 7;

    /* A non-constant argument takes the same path. */
    volatile double v = 16.0;
    if (__builtin_sqrt(v) != 4.0) return 8;
    volatile double sign = -2.0;
    if (__builtin_copysign(7.0, sign) != -7.0) return 9;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("builtins_lib_math", code, &["-lm".to_string()]),
        0
    );
}

/// `creal`, `cimag` and `conj`, and the suffixed spellings of `isnan` and
/// `isinf`.
///
/// `creal`/`cimag` lower to the `__real__` and `__imag__` c17 already has, and
/// `conj` to `__builtin_complex(__real__ z, -__imag__ z)` -- every piece
/// existed, so none of the three needs a libm call or `-lm`. The `isnan`/
/// `isinf` suffixes carry no information the node needs: `FpTest` dispatches
/// on the operand's own type.
///
/// gcc has **no** `__builtin_isfinitef` or `__builtin_isnormall`, despite
/// having the unsuffixed pair -- they compile and then fail to link, which is
/// how the first version of this got it wrong. Claiming a builtin gcc does not
/// have would make `__has_builtin` a worse answer than none, so the test pins
/// their absence alongside the others' presence.
#[test]
fn builtins_complex_parts_and_suffixed_fp_tests() {
    let code = r#"
int main(void) {
    _Complex double z = __builtin_complex(3.0, 4.0);
    _Complex float  w = __builtin_complex(1.0f, 2.0f);
    _Complex long double q = __builtin_complex(5.0L, 6.0L);

    /* creal/cimag name the halves __real__ and __imag__ already reach. */
    if (__builtin_creal(z) != 3.0 || __builtin_cimag(z) != 4.0) return 1;
    if (__builtin_crealf(w) != 1.0f || __builtin_cimagf(w) != 2.0f) return 2;
    if (__builtin_creall(q) != 5.0L || __builtin_cimagl(q) != 6.0L) return 3;

    /* conj flips the sign of the imaginary half, at every precision. */
    { _Complex double c = __builtin_conj(z);
      if (__builtin_creal(c) != 3.0 || __builtin_cimag(c) != -4.0) return 4; }
    { _Complex float c = __builtin_conjf(w);
      if (__builtin_crealf(c) != 1.0f || __builtin_cimagf(c) != -2.0f) return 5; }
    { _Complex long double c = __builtin_conjl(q);
      if (__builtin_creall(c) != 5.0L || __builtin_cimagl(c) != -6.0L) return 6; }

    /* An involution: conj of conj is the original. */
    { _Complex double c = __builtin_conj(__builtin_conj(z));
      if (__builtin_creal(c) != 3.0 || __builtin_cimag(c) != 4.0) return 7; }

    /* A zero imaginary part conjugates to negative zero, which is the whole
       reason conj is not "subtract the imaginary part from zero". */
    { _Complex double c = __builtin_conj(__builtin_complex(1.0, 0.0));
      if (!__builtin_signbit(__builtin_cimag(c))) return 8; }

    /* The suffixed isnan/isinf spellings ask the same question as the
       unsuffixed one, of the operand's own type. */
    if (!__builtin_isinff(1.0f / 0.0f)) return 9;
    if (!__builtin_isinfl(1.0L / 0.0L)) return 10;
    if (!__builtin_isnanf(0.0f / 0.0f)) return 11;
    if (!__builtin_isnanl(0.0L / 0.0L)) return 12;
    if (__builtin_isinff(1.0f) || __builtin_isnanf(1.0f)) return 13;

    /* gcc has no suffixed isfinite or isnormal, so neither do we, and
       __has_builtin must say so rather than over-promise. */
    if (__has_builtin(__builtin_isfinitef)) return 14;
    if (__has_builtin(__builtin_isnormall)) return 15;
    if (!__has_builtin(__builtin_isinff)) return 16;
    if (!__has_builtin(__builtin_conjf)) return 17;
    if (!__has_builtin(__builtin_creal)) return 18;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_complex_parts", code, &[]), 0);
}

// ============================================================================
// The plain spellings
// ============================================================================

/// gcc recognizes `fabs` and friends as builtins whether or not `<math.h>`
/// was included, and a program that only writes `extern double fabs(double);`
/// still gets the intrinsic. Recognizing the plain spelling is what lets
/// `fabs(x) < 0.0` fold.
///
/// The three negative halves are the point. The bare name is an object as
/// well as a call -- `double (*p)(double) = fabs;` names the library function
/// and must not be parsed as a builtin invocation. A declaration of something
/// *other* than a function displaces it. And the argument has to be converted
/// to the prototype's type before it reaches an opcode that masks a sign bit:
/// `fabs(-3)` passing an `int` straight through read the integer as a double
/// bit pattern.
#[test]
fn builtins_plain_fabs_spellings_are_recognized() {
    let code = r#"
extern double fabs(double);
extern float fabsf(float);
extern long double fabsl(long double);
extern void abort(void);

static double (*as_value)(double) = fabs;

int main(void)
{
    int i = -3;

    if (fabs(-3.5) != 3.5) abort();
    if (fabsf(-2.25f) != 2.25f) abort();
    if (fabsl(-1.5L) != 1.5L) abort();

    /* The name used as a value, not a call. */
    if (as_value(-7.0) != 7.0) abort();

    /* The argument converts to the prototype's type first. */
    if (fabs(-3) != 3.0) abort();
    if (fabs(i) != 3.0) abort();
    if (fabsf(-2) != 2.0f) abort();
    if (fabs(-1.5L) != 1.5) abort();

    /* fabs of a NaN is a NaN with the sign cleared. */
    {
        double n = -__builtin_nan("");
        double a = fabs(n);
        if (a == a) abort();
        if (__builtin_signbit(a)) abort();
    }

    return 0;
}
"#;
    for extra in [
        vec!["-lm".to_string()],
        vec!["-lm".to_string(), "-fno-builtin".to_string()],
    ] {
        assert_eq!(
            compile_and_run("builtins_plain_fabs", code, &extra),
            0,
            "with {extra:?}"
        );
    }
}

/// A declaration of something that is not a function takes the name back.
#[test]
fn builtins_a_plain_fabs_object_displaces_the_builtin() {
    let code = r#"
extern void abort(void);
static double fabs = 2.5;

int main(void)
{
    if (fabs != 2.5) abort();
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_fabs_object", code, &[]), 0);
}

/// `(float)floor((double)x)` is `floorf(x)`, and c17 narrows it.
///
/// Exact because the result is an integer no greater in magnitude than `x`,
/// so a value representable as a `float` stays representable. The condition
/// is on the **argument** type and not the result: a function returning
/// `double` narrows too, because the narrowing happens before the widening.
///
/// The negative half is the point, and the c-torture test that covers this
/// weaponises it -- only the *exactly-rounding* functions qualify. `sinf(x)`
/// and `(float)sin((double)x)` differ in the last bit for some `x`, so
/// narrowing `sin` would be a wrong answer rather than a faster one.
///
/// c17 narrows in the parser and so does it at every level, where gcc does
/// it only with the optimizer on. Both are correct, since the rewrite is
/// exact; [`builtins_math_narrowing_happens_at_every_level`] pins the
/// difference down on the assembly, because a run-time test of it would
/// disagree with gcc at `-O0` for a reason that is not a defect.
#[test]
fn builtins_exactly_rounding_math_narrows_to_its_float_form() {
    let code = r#"
extern void abort(void);
double floor(double); double ceil(double); double trunc(double);
double round(double); double rint(double); double nearbyint(double);
double sin(double); double log(double);

/* Every weak definition here is the identity except the ones that must not
   be reached, which abort. The arguments below are all integral, so the
   true mathematical answer is the identity too -- which is what makes this
   robust to the compiler expanding the narrowed call as a machine
   instruction instead of calling it at all. What it catches is the *wide*
   form being reached with a `float` argument. */
__attribute__((weak)) double floor(double a) { abort(); }
__attribute__((weak)) float floorf(float a) { return a; }
__attribute__((weak)) double ceil(double a) { abort(); }
__attribute__((weak)) float ceilf(float a) { return a; }
__attribute__((weak)) double trunc(double a) { abort(); }
__attribute__((weak)) float truncf(float a) { return a; }
__attribute__((weak)) double round(double a) { abort(); }
__attribute__((weak)) float roundf(float a) { return a; }
__attribute__((weak)) double rint(double a) { abort(); }
__attribute__((weak)) float rintf(float a) { return a; }
__attribute__((weak)) double nearbyint(double a) { abort(); }
__attribute__((weak)) float nearbyintf(float a) { return a; }

/* `sin` and `log` must NOT narrow, so the arrangement is reversed: the wide
   one is the identity and the narrow one aborts. */
__attribute__((weak)) double sin(double a) { return a; }
__attribute__((weak)) float sinf(float a) { abort(); }
__attribute__((weak)) double log(double a) { return a; }
__attribute__((weak)) float logf(float a) { abort(); }

__attribute__((noinline)) static float narrow(float x)
{
    return floor(x) + ceil(x) + trunc(x) + round(x) + rint(x) + nearbyint(x);
}

/* A `double` result from a `float` argument narrows just the same: the
   narrowing happens before the widening. */
__attribute__((noinline)) static double wide_result(float x) { return floor(x); }

__attribute__((noinline)) static double transcendental(float x)
{
    return sin(x) + log(x);
}

int main(void)
{
    /* Guarded because gcc narrows only with the optimizer on, so at `-O0` it
       reaches the aborting wide form and this program is not a statement
       about it. c17 narrows at every level, which
       `builtins_math_narrowing_happens_at_every_level` checks on the
       assembly instead. */
#ifdef __OPTIMIZE__
    /* Six identities at an integral argument. */
    if (narrow(0.0f) != 0.0f) abort();
    if (narrow(2.0f) != 12.0f) abort();
    if (narrow(-3.0f) != -18.0f) abort();
    if (wide_result(0.0f) != 0.0) abort();
    if (wide_result(-4.0f) != -4.0) abort();
    if (transcendental(0.0f) != 0.0) abort();
    if (transcendental(5.0f) != 10.0) abort();
#endif
    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2", "-Os"] {
        assert_eq!(
            compile_and_run("builtins_math_narrowing", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// The narrowing happens in the parser, so it does not wait for `-O`.
///
/// gcc performs it as an optimization and leaves the wide call at `-O0`.
/// Doing it always is a deliberate difference and a safe one -- the rewrite
/// is exact at every level -- but it is a difference, so it is stated here
/// rather than left for someone to discover from a disassembly. At `-O0`
/// the bare `floor` is a call, to `floorf`; once optimizing it is computed in
/// place, still at `float` -- `cvttss2si` on x86-64, `frintm` of an `s`
/// register on aarch64 -- and nothing is called.
#[test]
fn builtins_math_narrowing_happens_at_every_level() {
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
            let asm = asm_for_at("math_narrow_level", src, &args);
            // `q` is the function this source defines, so it calibrates.
            let p = crate::common::asm_prefix(&asm, "q");
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
                !calls("floor"),
                "{target:?} at {opt}: the wide form must not be called:\n{asm}"
            );
            if opt == "-O0" {
                assert!(
                    calls("floorf"),
                    "{target:?} at {opt}: the call should be narrowed to {p}floorf:\n{asm}"
                );
            } else {
                assert!(
                    !calls("floorf") && asm.contains(in_place),
                    "{target:?} at {opt}: not computed in place at float:\n{asm}"
                );
            }
        }
    }
}

// ============================================================================
// C99 7.12.14 — the unordered-safe relations
// ============================================================================

/// glibc's `<math.h>` *defines* `isgreater`, `isless`, `isunordered` and their
/// siblings as these builtins, so a translation unit that includes the header
/// and uses one did not compile at all before they existed.
///
/// Every relation is checked against an ordered pair in both directions and
/// against a NaN on each side, because the NaN answer is the whole reason C99
/// has this family: the ordinary relational operators may raise `FE_INVALID`
/// on an unordered pair and these may not. `islessgreater` is the one that
/// cannot be spelled as `!=` -- `!=` is *true* for an unordered pair and this
/// must be false for one.
const FP_RELATIONS: &str = r#"
extern void abort(void);
#define CHECK(n, e) do { if (!(e)) return n; } while (0)

int main(void) {
    volatile double one = 1.0, two = 2.0, nan = 0.0, zero = 0.0;
    nan = nan / zero;                    /* a NaN the optimizer cannot fold */

    CHECK(1, __builtin_isgreater(two, one) == 1);
    CHECK(2, __builtin_isgreater(one, two) == 0);
    CHECK(3, __builtin_isgreater(one, one) == 0);
    CHECK(4, __builtin_isgreaterequal(one, one) == 1);
    CHECK(5, __builtin_isgreaterequal(one, two) == 0);
    CHECK(6, __builtin_isless(one, two) == 1);
    CHECK(7, __builtin_isless(two, one) == 0);
    CHECK(8, __builtin_islessequal(one, one) == 1);
    CHECK(9, __builtin_islessequal(two, one) == 0);
    CHECK(10, __builtin_islessgreater(one, two) == 1);
    CHECK(11, __builtin_islessgreater(one, one) == 0);
    CHECK(12, __builtin_isunordered(one, two) == 0);

    /* Every relation is false for an unordered pair, on either side... */
    CHECK(13, __builtin_isgreater(nan, one) == 0);
    CHECK(14, __builtin_isgreater(one, nan) == 0);
    CHECK(15, __builtin_isgreaterequal(nan, one) == 0);
    CHECK(16, __builtin_isless(nan, one) == 0);
    CHECK(17, __builtin_islessequal(nan, one) == 0);
    CHECK(18, __builtin_islessgreater(nan, one) == 0);
    CHECK(19, __builtin_islessgreater(nan, nan) == 0);
    /* ...and `isunordered` is the one that is true for it. */
    CHECK(20, __builtin_isunordered(nan, one) == 1);
    CHECK(21, __builtin_isunordered(one, nan) == 1);
    CHECK(22, __builtin_isunordered(nan, nan) == 1);

    /* Infinities are ordered, so the relations answer normally. */
    {
        volatile double inf = __builtin_inf();
        CHECK(23, __builtin_isgreater(inf, one) == 1);
        CHECK(24, __builtin_isless(-inf, one) == 1);
        CHECK(25, __builtin_isunordered(inf, one) == 0);
    }

    /* float and long double reach the same lowering through a conversion. */
    {
        volatile float f1 = 1.0f, f2 = 2.0f;
        volatile long double l1 = 1.0L, l2 = 2.0L;
        CHECK(26, __builtin_isless(f1, f2) == 1);
        CHECK(27, __builtin_isgreater(l2, l1) == 1);
        CHECK(28, __builtin_isunordered(f1, f2) == 0);
    }

    /* The usual arithmetic conversions run first, as they do for `<`. */
    CHECK(29, __builtin_isless(1, 2.0) == 1);
    CHECK(30, __builtin_isgreater(3.0f, 2) == 1);
    return 0;
}
"#;

#[test]
fn builtins_fp_relations() {
    assert_eq!(
        compile_and_run("fp_relations", FP_RELATIONS, &["-lm".into()]),
        0
    );
}

/// Each operand is evaluated exactly once. The relation is desugared in the
/// linearizer rather than written out as `a < b` in the parser precisely so
/// that `isunordered(f(), g())` does not call either function twice.
#[test]
fn builtins_fp_relations_evaluate_each_operand_once() {
    let code = r#"
int lhs, rhs;
double f(void) { lhs++; return 1.0; }
double g(void) { rhs++; return 2.0; }

int main(void) {
    if (__builtin_isless(f(), g()) != 1) return 1;
    if (lhs != 1 || rhs != 1) return 2;
    /* `isunordered` and `islessgreater` each read both operands twice in the
       lowering, which is where a duplicated *expression* would show up. */
    lhs = rhs = 0;
    if (__builtin_isunordered(f(), g()) != 0) return 3;
    if (lhs != 1 || rhs != 1) return 4;
    lhs = rhs = 0;
    if (__builtin_islessgreater(f(), g()) != 1) return 5;
    if (lhs != 1 || rhs != 1) return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_relations_once", code, &[]), 0);
}

/// `<math.h>`'s own macros, which are what real code writes. This is the
/// case that failed to compile: the header expands `isgreater(x, y)` straight
/// to `__builtin_isgreater(x, y)`.
#[test]
fn builtins_math_h_relation_macros() {
    let code = r#"
#include <math.h>
int main(void) {
    volatile double one = 1.0, two = 2.0, zero = 0.0;
    volatile double nan = zero / zero;
    if (!isgreater(two, one)) return 1;
    if (!isgreaterequal(one, one)) return 2;
    if (!isless(one, two)) return 3;
    if (!islessequal(one, one)) return 4;
    if (!islessgreater(one, two)) return 5;
    if (!isunordered(nan, one)) return 6;
    if (isunordered(one, two)) return 7;
    if (isgreater(nan, one)) return 8;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("math_h_relations", code, &["-lm".into()]),
        0
    );
}

/// The suffixed spellings of the libm aliases, and the memory ones gcc has
/// under a `__builtin_` name. `bits/floatn.h` reaches for
/// `__builtin_copysignf`, so its absence broke any file including `<math.h>`
/// on a target with `_Float128` support advertised.
#[test]
fn builtins_libm_and_memory_aliases() {
    let code = r#"
int main(void) {
    if (__builtin_copysign(2.0, -1.0) != -2.0) return 1;
    if (__builtin_copysignf(2.0f, -1.0f) != -2.0f) return 2;
    if (__builtin_copysignl(2.0L, -1.0L) != -2.0L) return 3;
    if (__builtin_sqrtf(16.0f) != 4.0f) return 4;
    if (__builtin_sqrtl(16.0L) != 4.0L) return 5;
    if (__builtin_fmax(3.0, 4.0) != 4.0) return 6;
    if (__builtin_fmaxf(3.0f, 4.0f) != 4.0f) return 7;
    if (__builtin_fmaxl(3.0L, 4.0L) != 4.0L) return 8;
    if (__builtin_fmin(3.0, 4.0) != 3.0) return 9;
    if (__builtin_fminf(3.0f, 4.0f) != 3.0f) return 10;
    if (__builtin_fminl(3.0L, 4.0L) != 3.0L) return 11;
    if (__builtin_pow(2.0, 10.0) != 1024.0) return 12;
    if (__builtin_powf(2.0f, 10.0f) != 1024.0f) return 13;
    if (__builtin_fma(2.0, 3.0, 4.0) != 10.0) return 14;

    {
        char d[8] = "abcdefg";
        __builtin_bzero(d, 8);
        if (d[0] != 0 || d[7] != 0) return 15;
    }
    if (__builtin_bcmp("ab", "ab", 2) != 0) return 16;
    if (__builtin_bcmp("ab", "ac", 2) == 0) return 17;
    {
        char dst[8];
        if (__builtin_stpncpy(dst, "ab", 3) != dst + 2) return 18;
        if (dst[0] != 'a' || dst[1] != 'b' || dst[2] != 0) return 19;
    }
    if (__builtin_strdup("hi") == 0) return 20;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("libm_memory_aliases", code, &["-lm".into()]),
        0
    );
}

/// `__builtin_extract_return_addr` is the identity on both targets c17 has,
/// and `__builtin___clear_cache` has to reach libgcc on AArch64, where the
/// caches are not coherent and a JIT is wrong without it.
#[test]
fn builtins_return_address_and_cache() {
    let code = r#"
int calls;
void *bump(void) { calls++; return (void *)0x1234; }

int main(void) {
    char code[16];
    __builtin___clear_cache(code, code + 16);
    if (__builtin_extract_return_addr(bump()) != (void *)0x1234) return 1;
    if (calls != 1) return 2;
    if (__builtin_extract_return_addr(__builtin_return_address(0)) == 0) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("return_addr_clear_cache", code, &[]), 0);
}

/// The libm entry points under their `__builtin_` spellings, at each of the
/// three real widths.
///
/// `__builtin_ceilf` and `__builtin_modf` are ordinary functions any program
/// may name; c17 recognised the bare `ceil`/`floor` family and not these.
/// Signatures come from one table, because a `float` entry point that is
/// declared as taking a `double` does not fail to link -- it sends the
/// argument at the wrong width and answers with whatever was in the register.
#[test]
fn builtins_libm_entry_points() {
    let code = r#"
int main(void) {
    if (__builtin_ceilf(1.2f) != 2.0f) return 1;
    if (__builtin_ceil(1.2) != 2.0) return 2;
    if (__builtin_ceill(1.2L) != 2.0L) return 3;
    if (__builtin_floor(1.8) != 1.0) return 4;
    if (__builtin_floorf(1.8f) != 1.0f) return 5;
    if (__builtin_trunc(-1.8) != -1.0) return 6;
    if (__builtin_fmod(7.0, 4.0) != 3.0) return 7;
    if (__builtin_atan2(0.0, 1.0) != 0.0) return 8;
    if (__builtin_hypot(3.0, 4.0) != 5.0) return 9;
    if (__builtin_exp(0.0) != 1.0) return 10;
    if (__builtin_log(1.0) != 0.0) return 11;

    /* These three do not take a list of one type: the second parameter is a
       pointer or an `int`, and declaring them uniformly sends it to the wrong
       register file. */
    {
        double ip;
        if (__builtin_modf(3.25, &ip) != 0.25 || ip != 3.0) return 12;
    }
    {
        int e;
        if (__builtin_frexp(8.0, &e) != 0.5 || e != 4) return 13;
    }
    if (__builtin_ldexp(0.5, 4) != 8.0) return 14;

    /* The POSIX case-insensitive comparisons. */
    if (__builtin_strncasecmp("AbC", "abc", 3) != 0) return 15;
    if (__builtin_strcasecmp("AbC", "abd") == 0) return 16;
    if (__builtin_strndup("abcd", 2) == 0) return 17;
    if (__builtin_memcmp_eq("ab", "ab", 2) != 0) return 18;
    if (__builtin_memcmp_eq("ab", "ac", 2) == 0) return 19;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("libm_entry_points", code, &["-lm".into()]),
        0
    );
}

// ============================================================================
// Bare complex accessors: creal, cimag, conj
// ============================================================================

// Self-contained, since the aarch64 run has no target headers to include.
const COMPLEX_ACCESSOR_PROGRAM: &str = r#"
double creal(double _Complex); float crealf(float _Complex);
long double creall(long double _Complex);
double cimag(double _Complex); float cimagf(float _Complex);
long double cimagl(long double _Complex);
double _Complex conj(double _Complex); float _Complex conjf(float _Complex);
long double _Complex conjl(long double _Complex);
static int n;
static double _Complex g(void) { n++; return 1.0 + 2.0i; }
int main(void) {
    volatile double _Complex d = 1.5 + 2.5i;
    volatile float _Complex f = 3.0f - 4.0if;
    volatile long double _Complex l = 5.0L + 6.0iL;

    /* The bare spellings, every precision. */
    if (creal(d) != 1.5 || cimag(d) != 2.5) return 1;
    if (crealf(f) != 3.0f || cimagf(f) != -4.0f) return 2;
    if (creall(l) != 5.0L || cimagl(l) != 6.0L) return 3;
    if (conj(d) != 1.5 - 2.5i || conjf(f) != 3.0f + 4.0if) return 4;
    if (conjl(l) != 5.0L - 6.0iL) return 5;

    /* The argument is evaluated once, bare or reserved. */
    if (conj(g()) != 1.0 - 2.0i || n != 1) return 6;
    if (__builtin_conj(g()) != 1.0 - 2.0i || n != 2) return 7;
    if (creal(g()) != 1.0 || n != 3) return 8;

    /* The argument converts to the suffix's type, as the prototype says:
       a real is a complex with a zero imaginary part, and a double half
       narrows to float. 1 + 2^-25 rounds to 1.0f. */
    if (creal(3) != 3.0 || cimag(3) != 0.0) return 9;
    volatile double _Complex p = 1.0 + 1.0000000298023223876953125i;
    if (cimagf(p) != 1.0f || __builtin_cimagf(p) != 1.0f) return 10;
    if (sizeof(crealf(d)) != sizeof(float)) return 11;
    if (sizeof(conjf(d)) != sizeof(float _Complex)) return 12;
    if (sizeof(__builtin_creall(d)) != sizeof(long double)) return 13;
    return 0;
}
"#;

/// `creal`, `cimag` and `conj` are computed in place under their bare names,
/// as the `__builtin_` spellings always were; `-fno-builtin-NAME` keeps the
/// library call.
#[test]
fn builtins_bare_complex_accessors() {
    assert_eq!(
        compile_and_run(
            "complex_accessors",
            COMPLEX_ACCESSOR_PROGRAM,
            &["-lm".to_string()]
        ),
        0
    );
    if let Some(rc) =
        compile_and_run_aarch64("complex_accessors_a64", COMPLEX_ACCESSOR_PROGRAM, "-O2")
    {
        assert_eq!(rc, 0);
    }

    // Named without a call, the identifier is the library function. Host
    // only: the aarch64 helper links no libm.
    let code = r#"
double creal(double _Complex);
int main(void) {
    double (*fp)(double _Complex) = creal;
    return fp(1.5 + 2.5i) == 1.5 ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("complex_accessor_fnptr", code, &["-lm".to_string()]),
        0
    );

    let calls = |asm: &str, name: &str| {
        let sym = asm_symbol(name);
        asm.lines().any(|l| {
            let mut words = l.split_whitespace();
            matches!(words.next(), Some("call" | "bl" | "jmp" | "b"))
                && words.next().map(|t| t.trim_end_matches("@PLT")) == Some(sym.as_str())
        })
    };
    let src = "double creal(double _Complex);\n\
               double cimag(double _Complex);\n\
               double _Complex conj(double _Complex);\n\
               double f(double _Complex z) { return creal(z) + cimag(conj(z)); }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_at("complex_accessors_asm", src, &[opt]);
        for name in ["creal", "cimag", "conj"] {
            assert!(!calls(&asm, name), "{opt}: {name} was called:\n{asm}");
        }
    }
    let asm = asm_for_at("complex_accessors_nb", src, &["-fno-builtin-creal"]);
    assert!(
        calls(&asm, "creal"),
        "-fno-builtin-creal kept creal inline:\n{asm}"
    );
    assert!(
        !calls(&asm, "conj"),
        "-fno-builtin-creal displaced conj:\n{asm}"
    );
}

// ============================================================================
// fabs and fabsf are computed in place
// ============================================================================

// Linked without -lm on purpose: gcc never needs libm for these, at any
// level, and c17 used to lower the opcode to a call to `fabs`, so this
// failed to link. Self-contained for the header-less aarch64 run.
const FABS_PROGRAM: &str = r#"
double fabs(double); float fabsf(float);
typedef unsigned long long u64; typedef unsigned int u32;
static u64 bits(double d) { u64 u; __builtin_memcpy(&u, &d, 8); return u; }
static u32 fbits(float f) { u32 u; __builtin_memcpy(&u, &f, 4); return u; }
int main(void) {
    volatile double m = -1.5, nz = -0.0, ninf = -__builtin_inf();
    volatile float fm = -2.5f, fnz = -0.0f;
    volatile double nnan = -__builtin_nan("");
    if (fabs(m) != 1.5 || __builtin_fabs(m) != 1.5) return 1;
    if (fabsf(fm) != 2.5f || __builtin_fabsf(fm) != 2.5f) return 2;
    /* Only the sign bit changes: -0 becomes +0, -inf +inf, and a negative
       NaN keeps its payload with the sign cleared. */
    if (bits(fabs(nz)) != 0) return 3;
    if (fbits(fabsf(fnz)) != 0) return 4;
    if (fabs(ninf) != __builtin_inf()) return 5;
    if (bits(fabs(nnan)) != (bits(nnan) & 0x7fffffffffffffffULL)) return 6;
    if (fabs(1.25) != 1.25 || fabsf(3.0f) != 3.0f) return 7;
    return 0;
}
"#;

#[test]
fn builtins_fabs_needs_no_libm() {
    assert_eq!(compile_and_run("fabs_no_libm", FABS_PROGRAM, &[]), 0);
    if let Some(rc) = compile_and_run_aarch64("fabs_no_libm_a64", FABS_PROGRAM, "-O0") {
        assert_eq!(rc, 0);
    }
    if let Some(rc) = compile_and_run_aarch64("fabs_no_libm_a64_o2", FABS_PROGRAM, "-O2") {
        assert_eq!(rc, 0);
    }
    let src = "double f(double x) { return __builtin_fabs(x); }\n\
               float g(float x) { return __builtin_fabsf(x); }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_at("fabs_inline", src, &[opt]);
        for name in ["fabs", "fabsf"] {
            let sym = asm_symbol(name);
            assert!(
                !asm.lines().any(|l| {
                    let mut w = l.split_whitespace();
                    matches!(w.next(), Some("call" | "bl" | "jmp" | "b"))
                        && w.next().map(|t| t.trim_end_matches("@PLT")) == Some(sym.as_str())
                }),
                "{opt}: {name} was called:\n{asm}"
            );
        }
    }
}

// Linked without -lm on purpose, like the test above: `fabsl` is a sign-bit
// operation on every target, the x87 `fabs` on x86-64 and a clear of bit 127
// of the binary128 on aarch64. The NaN inputs are built from bytes at run
// time, so no constant fold stands in for the instruction; the payload and
// the quiet bit must come through unchanged, for a signalling NaN too.
const FABSL_PROGRAM: &str = r#"
long double fabsl(long double);
typedef unsigned char u8;
#if __LDBL_MANT_DIG__ == 64
#define LD_BYTES 10 /* x87: the rest of the object is padding */
#else
#define LD_BYTES ((int)sizeof(long double))
#endif
static void to_bytes(u8 *out, long double v) { __builtin_memcpy(out, &v, sizeof v); }
/* The byte holding the sign bit, as its top bit. */
static int sign_index(void) {
    u8 a[sizeof(long double)], b[sizeof(long double)];
    to_bytes(a, 1.0L);
    to_bytes(b, -1.0L);
    for (int i = 0; i < LD_BYTES; i++)
        if (a[i] != b[i]) return i;
    return -1;
}
/* Whether fabsl of the value in `in` is `in` with only the sign cleared. */
static int only_sign_cleared(const u8 *in, int si) {
    volatile long double v;
    __builtin_memcpy((void *)&v, in, sizeof v);
    u8 out[sizeof(long double)];
    to_bytes(out, fabsl(v));
    for (int i = 0; i < LD_BYTES; i++) {
        u8 want = i == si ? in[i] & 0x7f : in[i];
        if (out[i] != want) return 0;
    }
    return 1;
}
int main(void) {
    volatile long double m = -1.5L, nz = -0.0L, ninf = -__builtin_infl();
    int si = sign_index();
    if (si < 0) return 1;
    int lo = si == 0 ? LD_BYTES - 1 : 0; /* the least significant byte */
    if (fabsl(m) != 1.5L || __builtin_fabsl(m) != 1.5L) return 2;
    u8 b[sizeof(long double)];
    to_bytes(b, fabsl(nz));
    for (int i = 0; i < LD_BYTES; i++)
        if (b[i] != 0) return 3;
    if (fabsl(ninf) != __builtin_infl()) return 4;
    to_bytes(b, m);
    if (!only_sign_cleared(b, si)) return 5;
    /* A negative quiet NaN with a payload. */
    to_bytes(b, __builtin_nanl(""));
    b[lo] |= 0x5a;
    b[si] |= 0x80;
    if (!only_sign_cleared(b, si)) return 6;
    /* A negative signalling NaN: an infinity with payload bits, quiet bit
       clear. */
    to_bytes(b, __builtin_infl());
    b[lo] |= 0x5a;
    b[si] |= 0x80;
    if (!only_sign_cleared(b, si)) return 7;
    if (fabsl(2.25L) != 2.25L) return 8;
    return 0;
}
"#;

#[test]
fn builtins_fabsl_needs_no_libm() {
    assert_eq!(compile_and_run("fabsl_no_libm", FABSL_PROGRAM, &[]), 0);
    if let Some(rc) = compile_and_run_aarch64("fabsl_no_libm_a64", FABSL_PROGRAM, "-O0") {
        assert_eq!(rc, 0);
    }
    if let Some(rc) = compile_and_run_aarch64("fabsl_no_libm_a64_o2", FABSL_PROGRAM, "-O2") {
        assert_eq!(rc, 0);
    }
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
        let asm = asm_for_at("fabsl_inline", src, &[opt]);
        assert!(!called(&asm), "{opt}: fabsl was called:\n{asm}");
        let asm = asm_for_at(
            "fabsl_inline_a64",
            src,
            &[opt, "--target", "aarch64-unknown-linux-gnu"],
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
        let asm = asm_for_at("fabs_const", &src, &[opt]);
        assert!(
            !asm.lines().any(|l| matches!(
                l.split_whitespace().next(),
                Some("andpd" | "andps" | "fabs")
            )),
            "{opt}: fabs of a constant was not folded:\n{asm}"
        );
    }
}

// ============================================================================
// NaN payloads
// ============================================================================

// Every value here was read off gcc on the same source, x86-64 and aarch64,
// at -O0 and -O2. A NaN's payload and sign are part of its value: the
// payload a `__builtin_nan` string names, the quiet bit `__builtin_nans`
// leaves clear, the sign a negation flips, and the payload bits a conversion
// keeps. Self-contained for the header-less aarch64 run.
const NAN_PAYLOAD_PROGRAM: &str = r#"
typedef unsigned long long u64; typedef unsigned u32;
static u64 b64(double d) { u64 u; __builtin_memcpy(&u, &d, 8); return u; }
static u32 b32(float f) { u32 u; __builtin_memcpy(&u, &f, 4); return u; }
static const double sd = __builtin_nan("0x1234");
static const float sf = __builtin_nanf("0x123");
static double sneg = -__builtin_nan("0x1234");
int main(void) {
    double d = __builtin_nan("0x1234");
    volatile double vd = __builtin_nan("0x1234");
    if (b64(sd) != 0x7ff8000000001234ULL) return 1;
    if (b64(d) != 0x7ff8000000001234ULL) return 2;
    if (b64(vd) != 0x7ff8000000001234ULL) return 3;
    if (b64(-__builtin_nan("0x1234")) != 0xfff8000000001234ULL) return 4;
    if (b64(sneg) != 0xfff8000000001234ULL) return 5;
    if (b64(__builtin_nans("0x1234")) != 0x7ff0000000001234ULL) return 6;
    if (b64(__builtin_nan("")) != 0x7ff8000000000000ULL) return 7;
    if (b64(__builtin_nan("4660")) != 0x7ff8000000001234ULL) return 8;
    if (b32(__builtin_nanf("0x123")) != 0x7fc00123u) return 9;
    if (b32(sf) != 0x7fc00123u) return 10;
    if (b32(__builtin_nansf("0x123")) != 0x7f800123u) return 11;
    /* A conversion keeps the payload's high bits, as the hardware does. */
    if (b32((float)__builtin_nan("0x40000000")) != 0x7fc00002u) return 12;
    if (b64((double)__builtin_nanf("0x123")) != 0x7ff8002460000000ULL) return 13;
    /* long double: x87 extended on x86-64, binary128 on aarch64. */
    long double l = __builtin_nanl("0x1234");
    unsigned char c[16] = {0};
    __builtin_memcpy(c, &l, sizeof(long double) == 16 && __LDBL_MANT_DIG__ == 64 ? 10 : sizeof(long double));
    u64 lo, hi;
    __builtin_memcpy(&lo, c, 8);
    __builtin_memcpy(&hi, c + 8, 8);
#if __LDBL_MANT_DIG__ == 64
    if (lo != 0xc000000000001234ULL || hi != 0x7fffULL) return 14;
#else
    if (lo != 0x1234ULL || hi != 0x7fff800000000000ULL) return 14;
#endif
    return 0;
}
"#;

#[test]
fn builtins_nan_payloads_survive() {
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("nan_payload{opt}"),
                NAN_PAYLOAD_PROGRAM,
                &[opt.to_string()]
            ),
            0,
            "host {opt}"
        );
        if let Some(rc) =
            compile_and_run_aarch64(&format!("nan_payload_a64{opt}"), NAN_PAYLOAD_PROGRAM, opt)
        {
            assert_eq!(rc, 0, "aarch64 {opt}");
        }
    }
}

/// A string gcc does not fold is not folded here either. `__builtin_nan` of
/// one is a call to the library's `nan`, which reads the string at run time;
/// `__builtin_nans` has no library function, so it is an error -- gcc's is a
/// link failure against `__builtin_nans`.
#[test]
fn builtins_nan_of_a_string_that_does_not_fold() {
    let code = r#"
double nan(const char *);
float nanf(const char *);
int main(void) {
    const char *volatile s = "0x77";
    if (!__builtin_isnan(__builtin_nan(s))) return 1;
    if (!__builtin_isnan(__builtin_nan("abc"))) return 2;
    if (!__builtin_isnan(__builtin_nanf("08"))) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("nan_library_call", code, &["-lm".to_string()]),
        0
    );
    compile_expect_error(
        "nans_malformed",
        "double f(void) { return __builtin_nans(\"zz\"); }\n",
        "is not a string literal naming a NaN payload",
    );
}

// ============================================================================
// signbit and copysign are computed in place
// ============================================================================

// Linked without -lm, and run at both levels on both targets. Every value
// was confirmed with gcc and aarch64-linux-gnu-gcc under qemu. The inputs are
// built from bits at run time, so the instructions are what is tested; the
// constant cases at the end are what the optimizer folds, and must agree.
// `copysign` takes only the sign of `y` -- of a NaN or a zero too -- and
// changes nothing else of `x`, so a NaN keeps its payload and a signalling
// one stays signalling.
const SIGN_PROGRAM: &str = r#"
double copysign(double, double); float copysignf(float, float);
long double copysignl(long double, long double);
typedef unsigned long long u64; typedef unsigned int u32; typedef unsigned char u8;
#define D_SIGN 0x8000000000000000ULL
#define F_SIGN 0x80000000U
static u64 dbits(double d) { u64 u; __builtin_memcpy(&u, &d, 8); return u; }
static double dfrom(u64 u) { double d; __builtin_memcpy(&d, &u, 8); return d; }
static u32 fbits(float f) { u32 u; __builtin_memcpy(&u, &f, 4); return u; }
static float ffrom(u32 u) { float f; __builtin_memcpy(&f, &u, 4); return f; }
/* +0, 1.5, inf, a quiet NaN and a signalling NaN with payloads; each is
   also tried with its sign bit set. */
static volatile u64 dcase[] = {
    0, 0x3ff8000000000000ULL, 0x7ff0000000000000ULL,
    0x7ff8000000001234ULL, 0x7ff0000000005678ULL,
};
static volatile u32 fcase[] = {
    0, 0x3fc00000U, 0x7f800000U, 0x7fc01234U, 0x7f805678U,
};
#define NCASE 5
#if __LDBL_MANT_DIG__ == 64
#define LD_BYTES 10 /* x87: the rest of the object is padding */
#else
#define LD_BYTES ((int)sizeof(long double))
#endif
typedef struct { u8 b[sizeof(long double)]; } ldb;
static ldb ld_bytes(long double v) {
    ldb r;
    __builtin_memset(&r, 0, sizeof r);
    __builtin_memcpy(r.b, &v, LD_BYTES);
    return r;
}
static long double ld_from(ldb r) { long double v; __builtin_memcpy(&v, r.b, sizeof v); return v; }
/* The byte holding the sign bit, as its top bit. */
static int sign_index(void) {
    ldb a = ld_bytes(1.0L), b = ld_bytes(-1.0L);
    for (int i = 0; i < LD_BYTES; i++)
        if (a.b[i] != b.b[i]) return i;
    return -1;
}
static ldb ldcase(int i, int si) {
    int lo = si == 0 ? LD_BYTES - 1 : 0; /* the least significant byte */
    ldb r;
    switch (i) {
    case 0: r = ld_bytes(0.0L); break;
    case 1: r = ld_bytes(1.5L); break;
    case 2: r = ld_bytes(__builtin_infl()); break;
    case 3: r = ld_bytes(__builtin_nanl("")); r.b[lo] |= 0x5a; break;
    default: r = ld_bytes(__builtin_infl()); r.b[lo] |= 0x5a; break; /* sNaN */
    }
    return r;
}
static int check_double(void) {
    for (int i = 0; i < 2 * NCASE; i++) {
        u64 xb = dcase[i % NCASE] | (i >= NCASE ? D_SIGN : 0);
        double x = dfrom(xb);
        int neg = i >= NCASE;
        if ((__builtin_signbit(x) != 0) != neg) return 1;
        for (int j = 0; j < 2 * NCASE; j++) {
            u64 yb = dcase[j % NCASE] | (j >= NCASE ? D_SIGN : 0);
            double y = dfrom(yb);
            u64 want = (xb & ~D_SIGN) | (yb & D_SIGN);
            if (dbits(copysign(x, y)) != want) return 2;
            if (dbits(__builtin_copysign(x, y)) != want) return 3;
        }
    }
    return 0;
}
static int check_float(void) {
    for (int i = 0; i < 2 * NCASE; i++) {
        u32 xb = fcase[i % NCASE] | (i >= NCASE ? F_SIGN : 0);
        float x = ffrom(xb);
        int neg = i >= NCASE;
        if ((__builtin_signbit(x) != 0) != neg) return 11;
        if ((__builtin_signbitf(x) != 0) != neg) return 12;
        for (int j = 0; j < 2 * NCASE; j++) {
            u32 yb = fcase[j % NCASE] | (j >= NCASE ? F_SIGN : 0);
            float y = ffrom(yb);
            u32 want = (xb & ~F_SIGN) | (yb & F_SIGN);
            if (fbits(copysignf(x, y)) != want) return 13;
            if (fbits(__builtin_copysignf(x, y)) != want) return 14;
        }
    }
    return 0;
}
static int check_long_double(void) {
    int si = sign_index();
    if (si < 0) return 21;
    for (int i = 0; i < 2 * NCASE; i++) {
        ldb xb = ldcase(i % NCASE, si);
        if (i >= NCASE) xb.b[si] |= 0x80;
        volatile long double x = ld_from(xb);
        int neg = i >= NCASE;
        if ((__builtin_signbit(x) != 0) != neg) return 22;
        if ((__builtin_signbitl(x) != 0) != neg) return 23;
        for (int j = 0; j < 2 * NCASE; j++) {
            ldb yb = ldcase(j % NCASE, si);
            if (j >= NCASE) yb.b[si] |= 0x80;
            volatile long double y = ld_from(yb);
            ldb r1 = ld_bytes(copysignl(x, y));
            ldb r2 = ld_bytes(__builtin_copysignl(x, y));
            for (int k = 0; k < LD_BYTES; k++) {
                u8 want = k == si ? (u8)((xb.b[k] & 0x7f) | (yb.b[k] & 0x80)) : xb.b[k];
                if (r1.b[k] != want) return 24;
                if (r2.b[k] != want) return 25;
            }
        }
    }
    return 0;
}
/* Constant arguments, which the optimizer folds: the same answers. */
static int check_constants(void) {
    if (dbits(copysign(1.5, -0.0)) != 0xbff8000000000000ULL) return 31;
    if (dbits(copysign(-1.5, 0.0)) != 0x3ff8000000000000ULL) return 32;
    if (dbits(copysign(__builtin_nan("0x1234"), -1.0)) != 0xfff8000000001234ULL) return 33;
    if (dbits(copysign(2.0, -__builtin_nan(""))) != 0xc000000000000000ULL) return 34;
    if (dbits(copysign(-__builtin_inf(), 1.0)) != 0x7ff0000000000000ULL) return 35;
    if (fbits(copysignf(__builtin_nanf("0x55"), -1.0f)) != 0xffc00055U) return 36;
    if (fbits(copysignf(3.0f, -0.0f)) != 0xc0400000U) return 37;
    if (copysignl(2.5L, -0.0L) != -2.5L) return 38;
    if (copysignl(-2.5L, __builtin_infl()) != 2.5L) return 39;
    if (__builtin_signbit(-0.0) != 1 || __builtin_signbit(0.0) != 0) return 40;
    if (__builtin_signbit(-0.0f) != 1 || __builtin_signbitf(-1.0f) != 1) return 41;
    if (__builtin_signbit(-0.0L) != 1 || __builtin_signbitl(1.0L) != 0) return 42;
    if (__builtin_signbit(-__builtin_nan("")) != 1) return 43;
    if (__builtin_signbit(__builtin_nan("")) != 0) return 44;
    return 0;
}
int main(void) {
    int rc;
    if ((rc = check_double())) return rc;
    if ((rc = check_float())) return rc;
    if ((rc = check_long_double())) return rc;
    return check_constants();
}
"#;

#[test]
fn builtins_signbit_and_copysign_need_no_libm() {
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("sign_ops{opt}"), SIGN_PROGRAM, &[opt.to_string()]),
            0,
            "host {opt}"
        );
        if let Some(rc) = compile_and_run_aarch64(&format!("sign_ops_a64{opt}"), SIGN_PROGRAM, opt)
        {
            assert_eq!(rc, 0, "aarch64 {opt}");
        }
    }
}

/// Whether `asm` calls a function whose name ends in one of `names`.
pub(super) fn calls_any(asm: &str, names: &[&str]) -> bool {
    asm.lines().any(|l| {
        let mut w = l.split_whitespace();
        matches!(w.next(), Some("call" | "bl" | "jmp" | "b"))
            && w.next().is_some_and(|t| {
                let t = t.trim_end_matches("@PLT");
                names.iter().any(|n| t.ends_with(n))
            })
    })
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
        let asm = asm_for_at("sign_ops_inline", src, &[opt]);
        assert!(!calls_any(&asm, CALLEES), "{opt}: a call remains:\n{asm}");
        let asm = asm_for_at(
            "sign_ops_inline_a64",
            src,
            &[opt, "--target", "aarch64-unknown-linux-gnu"],
        );
        assert!(
            !calls_any(&asm, CALLEES),
            "{opt} aarch64: a call remains:\n{asm}"
        );
    }
}

/// glibc's `signbit` macro and the `copysign` family `<math.h>` declares
/// are the builtins, so a program using them needs no -lm either.
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
    assert_eq!(compile_and_run("sign_ops_math_h", code, &[]), 0);
    let asm = asm_for_at("sign_ops_math_h_asm", code, &["-O2"]);
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
    let asm = asm_for_at("copysign_nb", src, &["-fno-builtin-copysign"]);
    assert!(
        calls_any(&asm, &["copysign"]),
        "-fno-builtin-copysign kept copysign inline:\n{asm}"
    );
    assert!(
        !calls_any(&asm, &["copysignf"]),
        "-fno-builtin-copysign displaced copysignf:\n{asm}"
    );
}

/// As in gcc: `signbit` of a constant is an integer constant expression, and
/// `copysign`, `fabs` and `abs` of constants fold in a static initializer --
/// but, being calls, are not integer constant expressions, so an array bound
/// of one at file scope is rejected.
#[test]
fn builtins_sign_ops_in_constant_expressions() {
    let code = r#"
double copysign(double, double); double fabs(double); int abs(int);
static double a = copysign(2.0, -0.0);
static float b = __builtin_copysignf(-1.5f, 1.0f);
static double c = fabs(-3.0);
static int d = abs(-4);
static int e = __builtin_signbit(-1.0) + __builtin_signbitl(-0.0L);
enum { E = __builtin_signbit(-2.0f) };
_Static_assert(__builtin_signbit(-1.0), "signbit is a constant");
static int arr[__builtin_signbit(-1.0) + 1];
int main(void) {
    if (a != -2.0 || b != 1.5f || c != 3.0 || d != 4 || e != 2) return 1;
    if (E != 1 || sizeof arr != 2 * sizeof(int)) return 2;
    switch (d) { case __builtin_signbit(-1.0) + 3: return 0; }
    return 3;
}
"#;
    assert_eq!(compile_and_run("sign_ops_constexpr", code, &[]), 0);
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
        for target in [None, Some("aarch64-unknown-linux-gnu")] {
            let mut args = vec![opt];
            if let Some(t) = target {
                args.extend(["--target", t]);
            }
            let asm = asm_for_at("sign_ops_const", src, &args);
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
