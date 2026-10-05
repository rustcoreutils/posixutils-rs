//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Floating arithmetic runs only where the program runs it.
//
// Under Annex F (c17 defines `__STDC_IEC_559__`) floating operations raise
// exceptions the program can read with `fetestexcept`: overflow, inexact,
// invalid. Evaluating `a * b` on a path the program never takes -- to turn a
// `?:` into a select -- raises them anyway, so `c ? a * b : 0` with `c` zero
// reports an overflow nothing caused. gcc keeps the branch at every level.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

const UNTAKEN_ARMS: &str = r#"
#include <fenv.h>

/* A conditional's untaken arm must not run: its float arithmetic would raise
   exceptions the program never caused. */
__attribute__((noinline)) static double mul(int c, double a, double b) { return c ? a * b : 0; }
__attribute__((noinline)) static double add(int c, double a, double b) { return c ? a + b : 0; }
__attribute__((noinline)) static double sub(int c, double a, double b) { return c ? 0 : a - b; }
__attribute__((noinline)) static float cvt(int c, double a) { return c ? (float)a : 0; }
__attribute__((noinline)) static long double lmul(int c, long double a, long double b) { return c ? a * b : 0; }
__attribute__((noinline)) static float fmul(int c, float a, float b) { return c ? a * b : 0; }
__attribute__((noinline)) static int fix(int c, double a) { return c ? (int)a : 0; }

static volatile double sink;
static int raised(int e) { return fetestexcept(e) != 0; }

int main(void)
{
    volatile double big = 1e308, inf = __builtin_inf(), nan = __builtin_nan("");
    feclearexcept(FE_ALL_EXCEPT);
    sink = mul(0, big, big);
    if (raised(FE_OVERFLOW | FE_INEXACT)) return 1;
    sink = add(0, inf, -inf);
    if (raised(FE_INVALID)) return 2;
    sink = sub(1, inf, inf);
    if (raised(FE_INVALID)) return 3;
    sink = cvt(0, big);
    if (raised(FE_OVERFLOW)) return 4;
    sink = fix(0, nan);
    if (raised(FE_INVALID)) return 5;
    sink = lmul(0, __LDBL_MAX__, __LDBL_MAX__);
    if (raised(FE_OVERFLOW | FE_INEXACT)) return 7;
    sink = fmul(0, __FLT_MAX__, __FLT_MAX__);
    if (raised(FE_OVERFLOW | FE_INEXACT)) return 8;
    /* and the taken arm still raises */
    sink = mul(1, big, big);
    if (!raised(FE_OVERFLOW)) return 6;
    return 0;
}
"#;

#[test]
fn codegen_an_untaken_conditional_arm_raises_no_float_exception() {
    for level in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run(
                "fp_untaken_arms",
                UNTAKEN_ARMS,
                &[level.to_string(), "-lm".to_string()]
            ),
            0,
            "{level}"
        );
        if let Some(rc) =
            compile_and_run_aarch64_with("fp_untaken_arms_a64", UNTAKEN_ARMS, &[level], &["-lm"])
        {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

const UNTAKEN_CONVERSIONS: &str = r#"
#include <fenv.h>

/* An arm is converted to the conditional's type inside the arm, and that
   conversion is part of what must not run: `int` to `float` and `long` to
   `double` are inexact for large values. So are the conversions and libm
   calls nested inside an arm. */
__attribute__((noinline)) static float i2f(int c, int n) { return c ? n : 0.5f; }
__attribute__((noinline)) static double l2d(int c, long n) { return c ? n : 0.5; }
__attribute__((noinline)) static double root(int c, double a) { return c ? __builtin_sqrt(a) : 0; }
__attribute__((noinline)) static double nest(int c, int d, double a, double b) { return c ? (d ? a * b : 1.0) : 0; }
__attribute__((noinline)) static double elvis(double c, double a, double b) { return c ?: a * b; }
__attribute__((noinline)) static float narrow(int c, double a) { return c ? (float)a + 1.0f : 0; }

static volatile double sink;
static int raised(int e) { return fetestexcept(e) != 0; }

int main(void)
{
    volatile int big = 0x7fffffff;
    volatile long lbig = 0x7fffffffffffffffL;
    volatile double huge = 1e308;
    feclearexcept(FE_ALL_EXCEPT);
    sink = i2f(0, big);
    if (raised(FE_INEXACT)) return 1;
    sink = l2d(0, lbig);
    if (raised(FE_INEXACT)) return 2;
    sink = root(0, -1.0);
    if (raised(FE_INVALID)) return 3;
    sink = nest(1, 0, huge, huge);
    if (raised(FE_OVERFLOW)) return 4;
    sink = elvis(1.0, huge, huge);
    if (raised(FE_OVERFLOW)) return 5;
    sink = narrow(0, huge);
    if (raised(FE_OVERFLOW)) return 6;
    /* and the taken arm still converts */
    sink = i2f(1, big);
    if (!raised(FE_INEXACT)) return 7;
    return 0;
}
"#;

/// `-fno-math-errno` makes `sqrt` an instruction rather than a call that
/// may set `errno`, which is the form that could be speculated.
#[test]
fn codegen_an_untaken_arms_conversion_or_square_root_raises_nothing() {
    for level in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run(
                "fp_untaken_conversions",
                UNTAKEN_CONVERSIONS,
                &[
                    level.to_string(),
                    "-fno-math-errno".to_string(),
                    "-lm".to_string()
                ]
            ),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with(
            "fp_untaken_conversions_a64",
            UNTAKEN_CONVERSIONS,
            &[level, "-fno-math-errno"],
            &["-lm"],
        ) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// An `unsigned __int128` constant arm. Whether converting a constant is
/// exact is decided from its value, and one at or above 2^127 does not fit
/// the `i128` a constant is evaluated in: `(unsigned __int128)-1` read as -1
/// looked exact, so the arm became a select and converted 2^128 - 1 -- inexact,
/// and an overflow for `float` -- whatever the condition.
const UNTAKEN_U128_CONSTANTS: &str = r#"
#include <fenv.h>

/* An unsigned __int128 constant at or above 2^127 converts to float and
   double inexactly (and overflows float), so an untaken arm holding one must
   not be converted. */
__attribute__((noinline)) static float to_f(int c) { return c ? (unsigned __int128)-1 : 0.0f; }
__attribute__((noinline)) static double to_d(int c) { return c ? (unsigned __int128)-1 : 0.0; }
__attribute__((noinline)) static double to_d_top(int c)
{
    return c ? ((unsigned __int128)1 << 127) + 1 : 0.0;
}

static volatile double sink;

int main(void)
{
    feclearexcept(FE_ALL_EXCEPT);
    sink = to_f(0);
    if (fetestexcept(FE_INEXACT | FE_OVERFLOW)) return 1;
    sink = to_d(0);
    if (fetestexcept(FE_INEXACT)) return 2;
    sink = to_d_top(0);
    if (fetestexcept(FE_INEXACT)) return 3;
    /* The taken arm's value: 2^128 - 1 rounds to 2^128. */
    sink = to_d(1);
    if (sink != 0x1p128) return 4;
    if (to_f(1) != __builtin_inff()) return 5;
    return 0;
}
"#;

#[test]
fn codegen_an_untaken_unsigned_int128_constant_arm_raises_nothing() {
    for level in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run(
                "fp_untaken_u128",
                UNTAKEN_U128_CONSTANTS,
                &[level.to_string(), "-lm".to_string()]
            ),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with(
            "fp_untaken_u128_a64",
            UNTAKEN_U128_CONSTANTS,
            &[level],
            &["-lm"],
        ) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}
