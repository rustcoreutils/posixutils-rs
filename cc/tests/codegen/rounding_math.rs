//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-frounding-math`: the program may change the rounding direction with
// `fesetround`, so a floating-point operation whose result depends on it is
// not folded -- in the optimizer or in the front end -- and runs in whatever
// direction is in force. A result every direction agrees on is still a
// constant, and a static initializer is still evaluated as at translation
// time, to nearest (C17 F.8.5). glibc compiles everything with it.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

/// Each operation, computed in the direction set just before it, against the
/// same operation on `volatile` operands, which nothing folds. gcc 13 with
/// `-frounding-math` gives the same answers at -O0 and -O2, but for the zero
/// difference: gcc folds `1.0 - 1.0` to `+0` though the direction is
/// downward, and C says `-0`.
const ROUNDING_PROGRAM: &str = r#"
#include <fenv.h>
#include <math.h>

static const double static_third = 1.0 / 3.0;   /* at translation time */

static double third(void) { return 1.0 / 3.0; }
static double sum(void) { return 0.1 + 0.7; }
static double product(void) { return 0.1 * 3.0; }
static float narrowed(void) { return (float)0.1; }
static double from_integer(void) { return (double)9007199254740993LL; }
static double cancelled(void) { return 1.0 - 1.0; }
static double exact(void) { return 1.0 / 4.0 + 0.5; }

int main(void)
{
    volatile double one = 1.0, three = 3.0, p1 = 0.1, p7 = 0.7;
    volatile float fl;
    volatile long long big = 9007199254740993LL;
    const int modes[] = { FE_UPWARD, FE_DOWNWARD, FE_TOWARDZERO, FE_TONEAREST };
    for (int i = 0; i < 4; i++) {
        fesetround(modes[i]);
        double want_third = one / three;
        double want_sum = p1 + p7;
        double want_product = p1 * three;
        fl = (float)p1;
        float want_narrowed = fl;
        double want_integer = (double)big;
        if (third() != want_third) return 10 + i;
        if (1.0 / 3.0 != want_third) return 20 + i;
        if (sum() != want_sum) return 30 + i;
        if (product() != want_product) return 40 + i;
        if (narrowed() != want_narrowed) return 50 + i;
        if (from_integer() != want_integer) return 60 + i;
        if (signbit(cancelled()) != signbit(one - one)) return 70 + i;
        if (exact() != 0.75) return 80 + i;
        /* What the front end folds at every level: a condition, and a
           conversion to an integer. */
        if ((1.0 / 3.0 > 0x1.5555555555555p-2) != (want_third > 0x1.5555555555555p-2))
            return 90 + i;
        if ((int)(1.0 / 3.0 * 3.0) != (int)(want_third * three)) return 100 + i;
        int truncated = 1.0 / 3.0 * 3.0;
        if (truncated != (int)(want_third * three)) return 110 + i;
    }
    fesetround(FE_UPWARD);
    double up = one / three;
    fesetround(FE_DOWNWARD);
    double down = one / three;
    fesetround(FE_TONEAREST);
    if (!(up > down)) return 2;
    /* Translation time is to nearest, whatever the direction at run time. */
    if (static_third != one / three) return 3;
    return 0;
}
"#;

/// Under `-frounding-math` every operation above runs in the direction in
/// force, on both targets, at -O0 and -O2.
#[test]
fn codegen_rounding_math_honours_the_dynamic_direction() {
    for opt in ["-O0", "-O2"] {
        let flags = [
            opt.to_string(),
            "-frounding-math".to_string(),
            "-lm".to_string(),
        ];
        assert_eq!(
            compile_and_run("rounding_math", ROUNDING_PROGRAM, &flags),
            0,
            "at {opt}"
        );
        if let Some(code) = compile_and_run_aarch64_with(
            "rounding_math",
            ROUNDING_PROGRAM,
            &[opt, "-frounding-math"],
            &["-lm"],
        ) {
            assert_eq!(code, 0, "on aarch64 at {opt}");
        }
    }
}

/// `-fno-rounding-math` is the default and takes back an earlier
/// `-frounding-math`: at -O2 the inexact quotient is folded, to nearest, so
/// under an upward direction it is not what the division gives.
#[test]
fn codegen_rounding_math_is_off_by_default() {
    let code = r#"
#include <fenv.h>
static double third(void) { return 1.0 / 3.0; }
int main(void)
{
    volatile double one = 1.0, three = 3.0;
    fesetround(FE_UPWARD);
    double folded = third();
    double run = one / three;
    fesetround(FE_TONEAREST);
    return folded == run ? 1 : 0;
}
"#;
    for flags in [
        &["-O2", "-lm"][..],
        &["-O2", "-frounding-math", "-fno-rounding-math", "-lm"],
    ] {
        let flags: Vec<String> = flags.iter().map(|s| s.to_string()).collect();
        assert_eq!(
            compile_and_run("rounding_math_off", code, &flags),
            0,
            "{flags:?}"
        );
    }
}
