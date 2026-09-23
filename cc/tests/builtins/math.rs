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

use crate::common::{asm_for_at, compile_and_run};

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
/// rather than left for someone to discover from a disassembly.
#[test]
fn builtins_math_narrowing_happens_at_every_level() {
    let src = "double floor(double);\nfloat q(float a) { return floor(a); }\n";
    for opt in ["-O0", "-O1", "-O2"] {
        let asm = asm_for_at("math_narrow_level", src, &[opt]);
        assert!(
            asm.contains("floorf"),
            "at {opt} the call should be narrowed:\n{asm}"
        );
    }
}
