//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_issignaling` and `-fsignaling-nans`.
//

use crate::common::{
    compile_and_run, compile_and_run_aarch64_with, compile_expect_error, preprocess_text,
};

const ISSIGNALING: &str = r#"
/* `__builtin_issignaling(x)` (gcc 13): 1 for a signalling NaN of any
   floating type, 0 for everything else -- quiet NaNs, infinities, zeros,
   numbers. Built with -fsignaling-nans, which keeps the optimizer from
   assuming NaNs are quiet. Values go through volatile so nothing folds. */
#define CHECK(T, SNAN, QNAN, INF, code)                                        \
    do {                                                                       \
        volatile T s = SNAN, q = QNAN, i = INF, z = 0, n = 1.5;                \
        if (!__builtin_issignaling(s)) return code;                            \
        if (__builtin_issignaling(q)) return code + 1;                         \
        if (__builtin_issignaling(i) || __builtin_issignaling(-i)) return code + 2; \
        if (__builtin_issignaling(z) || __builtin_issignaling(n)) return code + 3; \
        if (!__builtin_issignaling(-s)) return code + 4;                       \
    } while (0)

int main(void)
{
    CHECK(float, __builtin_nansf(""), __builtin_nanf(""), __builtin_inff(), 10);
    CHECK(double, __builtin_nans(""), __builtin_nan(""), __builtin_inf(), 20);
    CHECK(long double, __builtin_nansl(""), __builtin_nanl(""), __builtin_infl(), 30);
    CHECK(_Float32, __builtin_nansf32(""), __builtin_nanf32(""), __builtin_inff32(), 40);
    CHECK(_Float64, __builtin_nansf64(""), __builtin_nanf64(""), __builtin_inff64(), 50);
#ifdef __FLT128_MANT_DIG__ /* no _Float128 on Apple targets */
    CHECK(_Float128, __builtin_nansf128(""), __builtin_nanf128(""), __builtin_inff128(), 60);
#endif
    CHECK(_Float32x, __builtin_nansf32x(""), __builtin_nanf32x(""), __builtin_inff32x(), 70);
    /* A constant operand folds the same way. */
    if (!__builtin_issignaling(__builtin_nans("")) || __builtin_issignaling(1.0))
        return 90;
    return 0;
}
"#;

#[test]
fn builtins_issignaling_tells_signalling_nans_apart() {
    for level in ["-O0", "-O2"] {
        let flags = [level.to_string(), "-fsignaling-nans".to_string()];
        assert_eq!(
            compile_and_run("issignaling", ISSIGNALING, &flags),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with(
            "issignaling_a64",
            ISSIGNALING,
            &[level, "-fsignaling-nans"],
            &[],
        ) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// What `-fsignaling-nans` leaves gcc folding, c17 agrees with, and every
/// fold that would hand back a signalling operand where the operation
/// quiets it is one neither makes: `x * 1.0`, `x / 1.0`, `x - 0.0`,
/// `x + -0.0` and `fmin(x, x)` all quiet. A negation, `fabs` and `copysign`
/// only move the sign bit and keep it signalling. Checked against gcc 13 at
/// -O0 and -O2 with the flag.
const SIGNALING_NANS_FOLDS: &str = r#"
volatile double vd;
volatile float vf;
volatile long double vl;
int main(void)
{
    vd = __builtin_nans(""); vf = __builtin_nansf(""); vl = __builtin_nansl("");
    double x = vd;
    float xf = vf;
    long double xl = vl;
    if (!__builtin_issignaling(x) || !__builtin_issignaling(xf)
        || !__builtin_issignaling(xl)) return 1;
    if (__builtin_issignaling(x * 1.0) || __builtin_issignaling(x / 1.0)) return 2;
    if (__builtin_issignaling(x - 0.0) || __builtin_issignaling(x + -0.0)) return 3;
    if (__builtin_issignaling(x + 0.0)) return 4;
    if (__builtin_issignaling(xf * 1.0f) || __builtin_issignaling(xl * 1.0L)) return 5;
    if (__builtin_issignaling(__builtin_fmin(x, x))
        || __builtin_issignaling(__builtin_fmax(x, x))) return 6;
    if (__builtin_issignaling((double)(float)x)) return 7;
    if (!__builtin_issignaling(-(-x)) || !__builtin_issignaling(-xl)) return 8;
    if (!__builtin_issignaling(__builtin_fabs(x))
        || !__builtin_issignaling(__builtin_copysign(x, -1.0))) return 9;
    if (__builtin_issignaling(x * 2.0 * 0.5)) return 10;
    /* A comparison sees a NaN, of either kind. */
    if (x == x || !(x != x) || !__builtin_isnan(x)) return 11;
    return 0;
}
"#;

#[test]
fn builtins_issignaling_signaling_nans_folds_keep_quieting() {
    for level in ["-O0", "-O2"] {
        let flags = [level, "-fsignaling-nans", "-lm"].map(String::from);
        assert_eq!(
            compile_and_run("snan_folds", SIGNALING_NANS_FOLDS, &flags),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with(
            "snan_folds_a64",
            SIGNALING_NANS_FOLDS,
            &[level, "-fsignaling-nans"],
            &["-lm"],
        ) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// The x87 encodings no IEEE format has: gcc 13 (and glibc's `issignaling`)
/// count every one with a nonzero exponent and the integer bit clear --
/// pseudo-infinity, pseudo-NaN, unnormal -- as signalling, since the x87
/// raises *invalid* on each; a pseudo-denormal is a number. gcc's own
/// gcc.target/i386/builtin-issignaling-1.c, cut down.
const X87_ENCODINGS: &str = r#"
#if __LDBL_MANT_DIG__ == 64
union U { struct { unsigned long long m; unsigned short e; } p; long double l; };
static const struct { unsigned long long m; unsigned short e; int want; } cases[] = {
    { 0, 0, 0 },                                /* zero */
    { 42, 0x8000, 0 },                          /* denormal */
    { 0x8000000000000042ULL, 0, 0 },            /* pseudo-denormal */
    { 0, 0x7fff, 1 },                           /* pseudo-infinity */
    { 42, 0xffff, 1 },                          /* pseudo-NaN */
    { 0x4000000000000042ULL, 0x7fff, 1 },       /* pseudo-NaN, quiet bit set */
    { 0x8000000000000000ULL, 0xffff, 0 },       /* infinity */
    { 0x8000000000000042ULL, 0x7fff, 1 },       /* signalling NaN */
    { 0x8000000000000042ULL, 0xffff, 1 },
    { 0xc000000000000000ULL, 0xffff, 0 },       /* indefinite */
    { 0xc000000000000042ULL, 0x7fff, 0 },       /* quiet NaN */
    { 0, 0x8042, 1 },                           /* unnormal */
    { 42, 0x42, 1 },
    { 0x8000000000000042ULL, 0x8042, 0 },       /* normal */
};
#endif

int main(void)
{
#if __LDBL_MANT_DIG__ == 64
    for (unsigned i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        volatile union U u;
        u.p.m = cases[i].m;
        u.p.e = cases[i].e;
        if (__builtin_issignaling(u.l) != cases[i].want)
            return 10 + i;
    }
#endif
    return 0;
}
"#;

#[test]
fn builtins_issignaling_x87_encodings() {
    for level in ["-O0", "-O2"] {
        let flags = [level.to_string(), "-fsignaling-nans".to_string()];
        assert_eq!(
            compile_and_run("snan_x87", X87_ENCODINGS, &flags),
            0,
            "{level}"
        );
    }
}

/// `_Float16` is tested at its own width, never widened to `float` (which
/// would quiet it), and a signalling NaN of either sign passes through a
/// call intact. The negative ones are built from their bits: `-s` is
/// computed at `float` on x86-64, where it quiets, and natively on aarch64,
/// where it does not.
const FLOAT16: &str = r#"
__attribute__((noinline)) int f(_Float16 x) { return __builtin_issignaling(x); }
volatile _Float16 s = __builtin_nansf16("0x12"), q = __builtin_nanf16("");
union H { unsigned short u; _Float16 h; };
volatile union H ms = { 0xfd00 }, mq = { 0xfe00 };
int main(void)
{
    if (!f(s) || !f(ms.h) || f(q) || f(mq.h)) return 1;
    if (f(__builtin_inff16()) || f(1.5f16) || f(0.0f16)) return 2;
    if (!__builtin_issignaling(s)) return 3;
    if (__builtin_issignaling((float)s)) return 4;
    return 0;
}
"#;

#[test]
fn builtins_issignaling_float16() {
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("snan_f16", FLOAT16, &[level.to_string()]),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with("snan_f16_a64", FLOAT16, &[level], &[]) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// A constant operand makes an integer constant expression, as in gcc 13 --
/// an enumerator, a `_Static_assert`, a static initializer -- with the
/// constant quieted where the program would quiet it; and the builtin is
/// one `__has_builtin` knows.
const CONSTANT: &str = r#"
#if !__has_builtin(__builtin_issignaling)
#error "no __builtin_issignaling"
#endif
enum {
    A = __builtin_issignaling(__builtin_nans("")),
    B = __builtin_issignaling(__builtin_nanf("")),
    C = __builtin_issignaling(-__builtin_nansl("0x5")),
    D = __builtin_issignaling((float)__builtin_nans("")),
    E = __builtin_issignaling(__builtin_nans("") * 1.0),
};
_Static_assert(A && !B && C && !D && !E, "issignaling folds");
static int s = __builtin_issignaling(__builtin_nansf(""));
int main(void)
{
    switch (1) {
    case __builtin_issignaling(__builtin_nans("")):
        return s == 1 ? 0 : 2;
    }
    return 1;
}
"#;

#[test]
fn builtins_issignaling_of_a_constant_is_a_constant() {
    assert_eq!(compile_and_run("snan_const", CONSTANT, &[]), 0);
}

/// gcc's error for an argument that is not a real floating value: an
/// integer, a complex value, a pointer.
#[test]
fn builtins_issignaling_rejects_a_non_floating_argument() {
    for (decl, arg) in [
        ("int x = 1;", "x"),
        ("_Complex double x = 0;", "x"),
        ("double d, *x = &d;", "x"),
    ] {
        let src = format!("int f(void) {{ {decl} return __builtin_issignaling({arg}); }}\n");
        compile_expect_error(
            "snan_reject",
            &src,
            "non-floating-point argument in call to function '__builtin_issignaling'",
        );
    }
}

/// `-fsignaling-nans` and `-fno-signaling-nans` are options c17 knows: no
/// "unrecognized option" warning, gcc's `__SUPPORT_SNAN__` defined under the
/// first and not the second, and the last one wins.
#[test]
fn builtins_signaling_nans_flag_is_accepted() {
    let src = "#ifdef __SUPPORT_SNAN__\nsnan __SUPPORT_SNAN__\n#else\nnosnan\n#endif\n";
    for (flags, want) in [
        (&["-fsignaling-nans"][..], "snan 1"),
        (&["-fno-signaling-nans"][..], "nosnan"),
        (&[][..], "nosnan"),
        (&["-fsignaling-nans", "-fno-signaling-nans"][..], "nosnan"),
        (&["-fno-signaling-nans", "-fsignaling-nans"][..], "snan 1"),
    ] {
        let run = preprocess_text("snan_flag", src, flags);
        assert!(run.success, "{flags:?}: {}", run.stderr);
        assert!(run.stderr.is_empty(), "{flags:?}: {}", run.stderr);
        assert!(run.stdout.contains(want), "{flags:?}: {}", run.stdout);
    }
}

/// Under `-fsignaling-nans` an operation that consumes a signalling NaN
/// constant is left to run time, where it raises *invalid*, as gcc 13 leaves
/// it: arithmetic, a conversion, `sqrt`, `fmin`. Moving the sign bit and
/// `issignaling` itself raise nothing.
const SNAN_CONSTANTS_RAISE: &str = r#"
#include <fenv.h>
volatile double sink;
volatile float sinkf;
volatile long double sinkl;
#define RAISES(e) (feclearexcept(FE_ALL_EXCEPT), (e), fetestexcept(FE_INVALID) != 0)
int main(void)
{
    if (!RAISES(sink = __builtin_nans("") * 1.0)) return 1;
    if (!RAISES(sink = __builtin_nans("") + 0.0)) return 2;
    if (!RAISES(sinkf = (float)__builtin_nans(""))) return 3;
    if (!RAISES(sinkl = (long double)__builtin_nans(""))) return 4;
    if (!RAISES(sink = (double)__builtin_nansf(""))) return 5;
    if (!RAISES(sink = __builtin_sqrt(__builtin_nans("")))) return 6;
    if (!RAISES(sink = __builtin_fmin(__builtin_nans(""), 1.0))) return 7;
    if (RAISES(sink = -__builtin_nans(""))) return 8;
    if (RAISES(sink = __builtin_fabs(__builtin_nans("")))) return 9;
    if (RAISES(sink = __builtin_issignaling(__builtin_nans("")))) return 10;
    return 0;
}
"#;

#[test]
fn builtins_signaling_nans_constants_raise_at_run_time() {
    for level in ["-O0", "-O2"] {
        let flags = [level, "-fsignaling-nans", "-lm"].map(String::from);
        assert_eq!(
            compile_and_run("snan_raise", SNAN_CONSTANTS_RAISE, &flags),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with(
            "snan_raise_a64",
            SNAN_CONSTANTS_RAISE,
            &[level, "-fsignaling-nans"],
            &["-lm"],
        ) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}
