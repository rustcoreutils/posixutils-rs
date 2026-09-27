//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// libm functions computed by the machine's own instructions
//
// Every expected value below was produced by glibc's libm under gcc on
// x86-64 and on aarch64 (qemu), which agree bit for bit; the programs are
// self-contained so that the aarch64 build, which has no target headers,
// runs the same source as the host.
//

use super::math::calls_any;
use crate::common::{
    asm_for_at, compile_and_run, compile_and_run_aarch64_with, compile_expect_error,
};

/// Run `code` at -O0 and -O2 on the host and on aarch64, with `opts` and,
/// when `libm` is set, `-lm`.
fn run_everywhere(name: &str, code: &str, opts: &[&str], libm: bool) {
    let libs: &[&str] = if libm { &["-lm"] } else { &[] };
    for opt in ["-O0", "-O2"] {
        let mut args: Vec<String> = vec![opt.to_string()];
        args.extend(opts.iter().map(|o| o.to_string()));
        args.extend(libs.iter().map(|l| l.to_string()));
        assert_eq!(
            compile_and_run(&format!("{name}{opt}"), code, &args),
            0,
            "host {opt} {opts:?}"
        );
        let mut a64: Vec<&str> = vec![opt];
        a64.extend_from_slice(opts);
        if let Some(rc) =
            compile_and_run_aarch64_with(&format!("{name}_a64{opt}"), code, &a64, libs)
        {
            assert_eq!(rc, 0, "aarch64 {opt} {opts:?}");
        }
    }
}

/// `asm` for the host and for aarch64 Linux, with `opts`.
fn asm_both(name: &str, src: &str, opts: &[&str]) -> [(&'static str, String); 2] {
    let mut a64 = opts.to_vec();
    a64.extend_from_slice(&["--target", "aarch64-unknown-linux-gnu"]);
    [
        ("host", asm_for_at(name, src, opts)),
        ("aarch64", asm_for_at(&format!("{name}_a64"), src, &a64)),
    ]
}

/// Whether `asm` contains the instruction `mnemonic` (with its operands).
fn has_insn(asm: &str, mnemonic: &str) -> bool {
    asm.lines()
        .any(|l| l.split_whitespace().next() == Some(mnemonic))
}

// ============================================================================
// sqrt, sqrtf, sqrtl
// ============================================================================

/// The run-time root, bit for bit, and `errno`: set to `EDOM` for an
/// argument below zero -- `-0` and a NaN are not -- and left alone
/// otherwise. A call whose value is discarded still sets it, and one in the
/// arm of a conditional that is not taken does not.
const SQRT_PROGRAM: &str = r#"
typedef unsigned long long u64; typedef unsigned int u32;
double sqrt(double); float sqrtf(float); long double sqrtl(long double);
extern int *__errno_location(void);
#define ERRNO (*__errno_location())
#define EDOM 33
static u64 dbits(double d) { u64 u; __builtin_memcpy(&u, &d, 8); return u; }
static double dfrom(u64 u) { double d; __builtin_memcpy(&d, &u, 8); return d; }
static u32 fbits(float f) { u32 u; __builtin_memcpy(&u, &f, 4); return u; }
static float ffrom(u32 u) { float f; __builtin_memcpy(&f, &u, 4); return f; }
/* +-0, 1, 2, 0.25, the smallest and largest subnormal, the largest finite
   value, +inf, a quiet NaN of each sign with a payload, 10, 1 + ulp, 2^63. */
static const u64 dcase[][2] = {
    {0x0000000000000000ULL, 0x0000000000000000ULL},
    {0x8000000000000000ULL, 0x8000000000000000ULL},
    {0x3ff0000000000000ULL, 0x3ff0000000000000ULL},
    {0x4000000000000000ULL, 0x3ff6a09e667f3bcdULL},
    {0x3fd0000000000000ULL, 0x3fe0000000000000ULL},
    {0x0000000000000001ULL, 0x1e60000000000000ULL},
    {0x000fffffffffffffULL, 0x1fffffffffffffffULL},
    {0x7fefffffffffffffULL, 0x5fefffffffffffffULL},
    {0x7ff0000000000000ULL, 0x7ff0000000000000ULL},
    {0x7ff8000000001234ULL, 0x7ff8000000001234ULL},
    {0xfff8000000001234ULL, 0xfff8000000001234ULL},
    {0x4024000000000000ULL, 0x40094c583ada5b53ULL},
    {0x3ff0000000000001ULL, 0x3ff0000000000000ULL},
    {0x43e0000000000000ULL, 0x41e6a09e667f3bcdULL},
};
static const u32 fcase[][2] = {
    {0x00000000U, 0x00000000U}, {0x80000000U, 0x80000000U},
    {0x3f800000U, 0x3f800000U}, {0x40000000U, 0x3fb504f3U},
    {0x3e800000U, 0x3f000000U}, {0x00000001U, 0x1a3504f3U},
    {0x007fffffU, 0x1fffffffU}, {0x7f7fffffU, 0x5f7fffffU},
    {0x7f800000U, 0x7f800000U}, {0x7fc01234U, 0x7fc01234U},
    {0xffc01234U, 0xffc01234U}, {0x41200000U, 0x404a62c2U},
    {0x3f800001U, 0x3f800000U},
};
/* -1, -inf and the smallest negative subnormal: domain errors. */
static const u64 dneg[] = {0xbff0000000000000ULL, 0xfff0000000000000ULL, 0x8000000000000001ULL};
static const u32 fneg[] = {0xbf800000U, 0xff800000U, 0x80000001U};
#define N(a) (int)(sizeof(a) / sizeof((a)[0]))
static int check_double(void) {
    for (int i = 0; i < N(dcase); i++) {
        volatile double x = dfrom(dcase[i][0]);
        ERRNO = 0;
        if (dbits(sqrt(x)) != dcase[i][1]) return 1;
        if (dbits(__builtin_sqrt(x)) != dcase[i][1]) return 2;
        if (ERRNO != 0) return 3;
    }
    for (int i = 0; i < N(dneg); i++) {
        volatile double x = dfrom(dneg[i]);
        ERRNO = 0;
        if (!__builtin_isnan(sqrt(x)) || ERRNO != EDOM) return 4;
        ERRNO = 0;
        if (!__builtin_isnan(__builtin_sqrt(x)) || ERRNO != EDOM) return 5;
        ERRNO = 0;
        (void)sqrt(x);
        if (ERRNO != EDOM) return 6;
        volatile int no = 0;
        ERRNO = 0;
        double r = no ? sqrt(x) : 1.0;
        if (r != 1.0 || ERRNO != 0) return 7;
    }
    /* A `float` argument is converted, not narrowed: the root of 2.0f as a
       double. */
    volatile float two = 2.0f;
    if (dbits(sqrt(two)) != 0x3ff6a09e667f3bcdULL) return 8;
    return 0;
}
static int check_float(void) {
    for (int i = 0; i < N(fcase); i++) {
        volatile float x = ffrom(fcase[i][0]);
        ERRNO = 0;
        if (fbits(sqrtf(x)) != fcase[i][1]) return 11;
        if (fbits(__builtin_sqrtf(x)) != fcase[i][1]) return 12;
        if (ERRNO != 0) return 13;
    }
    for (int i = 0; i < N(fneg); i++) {
        volatile float x = ffrom(fneg[i]);
        ERRNO = 0;
        if (!__builtin_isnan(sqrtf(x)) || ERRNO != EDOM) return 14;
        ERRNO = 0;
        if (!__builtin_isnan(__builtin_sqrtf(x)) || ERRNO != EDOM) return 15;
    }
    return 0;
}
static int check_long_double(void) {
    volatile long double four = 4.0L, quarter = 0.25L, two = 2.0L, mz = -0.0L;
    volatile long double inf = __builtin_infl(), m1 = -1.0L, nan = __builtin_nanl("");
    ERRNO = 0;
    if (sqrtl(four) != 2.0L || __builtin_sqrtl(four) != 2.0L) return 21;
    if (sqrtl(quarter) != 0.5L) return 22;
    if (sqrtl(two) != 1.4142135623730950488016887242096980785697L) return 23;
    long double z = sqrtl(mz);
    if (z != 0.0L || !__builtin_signbit(z)) return 24;
    if (sqrtl(inf) != inf) return 25;
    if (!__builtin_isnan(sqrtl(nan))) return 26;
    if (ERRNO != 0) return 27;
    if (!__builtin_isnan(sqrtl(m1)) || ERRNO != EDOM) return 28;
    ERRNO = 0;
    if (!__builtin_isnan(__builtin_sqrtl(m1)) || ERRNO != EDOM) return 29;
    return 0;
}
/* Constant arguments, which the optimizer folds: the same answers, and a
   domain error is still reported. */
static int check_constants(void) {
    ERRNO = 0;
    if (dbits(sqrt(2.0)) != 0x3ff6a09e667f3bcdULL) return 31;
    if (dbits(sqrt(-0.0)) != 0x8000000000000000ULL) return 32;
    if (dbits(sqrt(__builtin_nan("0x1234"))) != 0x7ff8000000001234ULL) return 33;
    if (fbits(sqrtf(2.0f)) != 0x3fb504f3U) return 34;
    if (sqrtl(2.0L) != 1.4142135623730950488016887242096980785697L) return 35;
    if (sqrt(__builtin_inf()) != __builtin_inf()) return 36;
    if (ERRNO != 0) return 37;
    if (!__builtin_isnan(sqrt(-4.0)) || ERRNO != EDOM) return 38;
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
fn libm_sqrt_values_and_errno() {
    run_everywhere("sqrt_errno", SQRT_PROGRAM, &[], true);
}

/// Under `-fno-math-errno` nothing is called at `-O2`, so the program links
/// without `-lm`, as gcc's does. A negative argument gives the instruction's
/// NaN.
const SQRT_NO_ERRNO_PROGRAM: &str = r#"
typedef unsigned long long u64; typedef unsigned int u32;
double sqrt(double); float sqrtf(float); long double sqrtl(long double);
static u64 dbits(double d) { u64 u; __builtin_memcpy(&u, &d, 8); return u; }
static u32 fbits(float f) { u32 u; __builtin_memcpy(&u, &f, 4); return u; }
int main(void) {
    volatile double two = 2.0, m1 = -1.0, mz = -0.0;
    volatile float twof = 2.0f, m1f = -1.0f;
    if (dbits(sqrt(two)) != 0x3ff6a09e667f3bcdULL) return 1;
    if (dbits(__builtin_sqrt(mz)) != 0x8000000000000000ULL) return 2;
    if (fbits(sqrtf(twof)) != 0x3fb504f3U) return 3;
    if (!__builtin_isnan(sqrt(m1)) || !__builtin_isnan(sqrtf(m1f))) return 4;
    if (!__builtin_isnan(__builtin_sqrtf(m1f))) return 5;
    return 0;
}
"#;

#[test]
fn libm_sqrt_without_math_errno_needs_no_libm() {
    assert_eq!(
        compile_and_run(
            "sqrt_no_errno",
            SQRT_NO_ERRNO_PROGRAM,
            &["-O2".to_string(), "-fno-math-errno".to_string()]
        ),
        0
    );
    if let Some(rc) = compile_and_run_aarch64_with(
        "sqrt_no_errno_a64",
        SQRT_NO_ERRNO_PROGRAM,
        &["-O2", "-fno-math-errno"],
        &[],
    ) {
        assert_eq!(rc, 0);
    }
}

const SQRT_SRC: &str = "double sqrt(double); float sqrtf(float);\n\
                        double d(double x) { return sqrt(x); }\n\
                        float f(float x) { return sqrtf(x); }\n\
                        double bd(double x) { return __builtin_sqrt(x); }\n\
                        float bf(float x) { return __builtin_sqrtf(x); }\n";

/// At `-O2` each root is the instruction, with a call to the library only
/// for an argument below zero, so that `errno` is set; `-fno-math-errno`
/// drops the call. As in gcc.
#[test]
fn libm_sqrt_is_the_instruction() {
    for (target, asm) in asm_both("sqrt_o2", SQRT_SRC, &["-O2"]) {
        let (d, s) = if target == "host" && cfg!(target_arch = "x86_64") {
            ("sqrtsd", "sqrtss")
        } else {
            ("fsqrt", "fsqrt")
        };
        assert!(has_insn(&asm, d) && has_insn(&asm, s), "{target}:\n{asm}");
        assert!(
            calls_any(&asm, &["sqrt", "sqrtf"]),
            "{target}: no errno path:\n{asm}"
        );
    }
    for (target, asm) in asm_both("sqrt_no_errno", SQRT_SRC, &["-O2", "-fno-math-errno"]) {
        assert!(!calls_any(&asm, &["sqrt", "sqrtf"]), "{target}:\n{asm}");
    }
    // The last of the pair wins.
    let opts = ["-O2", "-fno-math-errno", "-fmath-errno"];
    for (target, asm) in asm_both("sqrt_errno_again", SQRT_SRC, &opts) {
        assert!(calls_any(&asm, &["sqrt", "sqrtf"]), "{target}:\n{asm}");
    }
}

/// `-fmath-errno` and `-fno-math-errno` are options c17 acts on, so neither
/// is reported as ignored.
#[test]
fn libm_math_errno_flags_are_recognized() {
    let c = crate::common::create_c_file("math_errno_flags", SQRT_SRC);
    let path = c.path().to_string_lossy().to_string();
    for flag in ["-fmath-errno", "-fno-math-errno"] {
        let out = crate::common::run_c17(&["-S", "-o", "/dev/null", flag, &path]);
        assert!(out.success, "{flag}: {}", out.stderr);
        assert!(out.stderr.is_empty(), "{flag}: {}", out.stderr);
    }
}

/// At `-O0` gcc calls the library for a bare `sqrt`, and for
/// `__builtin_sqrt` too while `errno` is to be set, since only the optimizer
/// can split the call; under `-fno-math-errno` the builtin spelling is the
/// instruction.
#[test]
fn libm_sqrt_at_o0_follows_gcc() {
    for (target, asm) in asm_both("sqrt_o0", SQRT_SRC, &["-O0"]) {
        assert!(
            !has_insn(&asm, "sqrtsd") && !has_insn(&asm, "fsqrt"),
            "{target}:\n{asm}"
        );
        assert!(calls_any(&asm, &["sqrt"]), "{target}:\n{asm}");
    }
    let builtin_only = "double bd(double x) { return __builtin_sqrt(x); }\n\
                        float bf(float x) { return __builtin_sqrtf(x); }\n";
    for (target, asm) in asm_both("sqrt_o0_ne", builtin_only, &["-O0", "-fno-math-errno"]) {
        assert!(!calls_any(&asm, &["sqrt", "sqrtf"]), "{target}:\n{asm}");
    }
    let bare_only = "double sqrt(double);\ndouble d(double x) { return sqrt(x); }\n";
    for (target, asm) in asm_both("sqrt_o0_ne_bare", bare_only, &["-O0", "-fno-math-errno"]) {
        assert!(calls_any(&asm, &["sqrt"]), "{target}:\n{asm}");
    }
}

/// `sqrtl`: the x87 `fsqrt` on x86-64, guarded like the others; binary128 on
/// aarch64 has no instruction, so it is only the call -- with no comparison
/// in front of it, which would be a libgcc call of its own.
#[test]
fn libm_sqrtl_by_target() {
    let src = "long double sqrtl(long double);\n\
               long double l(long double x) { return sqrtl(x); }\n";
    let [(_, host), (_, a64)] = asm_both("sqrtl", src, &["-O2"]);
    if cfg!(target_arch = "x86_64") {
        assert!(has_insn(&host, "fsqrt"), "{host}");
    }
    assert!(calls_any(&a64, &["sqrtl"]), "{a64}");
    assert!(!calls_any(&a64, &["__lttf2"]), "{a64}");
}

/// `-fno-builtin-sqrt` keeps the call to `sqrt`, and only that one.
#[test]
fn libm_sqrt_fno_builtin_keeps_the_call() {
    let opts = ["-O2", "-fno-builtin-sqrt"];
    let src = "double sqrt(double);\ndouble d(double x) { return sqrt(x); }\n";
    for (target, asm) in asm_both("sqrt_nb", src, &opts) {
        assert!(
            calls_any(&asm, &["sqrt"]) && !has_insn(&asm, "sqrtsd") && !has_insn(&asm, "fsqrt"),
            "{target}: -fno-builtin-sqrt computed sqrt in place:\n{asm}"
        );
    }
    let src = "float sqrtf(float);\nfloat f(float x) { return sqrtf(x); }\n";
    for (target, asm) in asm_both("sqrtf_nb", src, &opts) {
        assert!(
            has_insn(&asm, "sqrtss") || has_insn(&asm, "fsqrt"),
            "{target}: -fno-builtin-sqrt displaced sqrtf:\n{asm}"
        );
    }
}

/// A constant root folds, in code and in a static initializer, to what the
/// instruction computes; a domain error does not, in either.
#[test]
fn libm_sqrt_of_constants() {
    let src = "double sqrt(double);\n\
               double f(void) { return sqrt(2.0) + __builtin_sqrtf(16.0f); }\n";
    for (target, asm) in asm_both("sqrt_fold", src, &["-O2"]) {
        assert!(
            !calls_any(&asm, &["sqrt", "sqrtf"]) && !has_insn(&asm, "sqrtsd"),
            "{target}: not folded:\n{asm}"
        );
        assert!(!has_insn(&asm, "fsqrt"), "{target}: not folded:\n{asm}");
    }
    let src = "double sqrt(double);\ndouble f(void) { return sqrt(-2.0); }\n";
    for (target, asm) in asm_both("sqrt_fold_neg", src, &["-O2"]) {
        assert!(calls_any(&asm, &["sqrt"]), "{target}: errno lost:\n{asm}");
    }

    let code = r#"
typedef unsigned long long u64;
double sqrt(double); float sqrtf(float); long double sqrtl(long double);
static double a = sqrt(2.0);
static float b = sqrtf(2.0f);
static long double c = __builtin_sqrtl(2.0L);
static double d = sqrt(-0.0);
int main(void) {
    u64 u; __builtin_memcpy(&u, &a, 8);
    if (u != 0x3ff6a09e667f3bcdULL) return 1;
    if (b != 1.41421353816986083984375f) return 2;
    if (c != 1.4142135623730950488016887242096980785697L) return 3;
    if (d != 0.0 || !__builtin_signbit(d)) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("sqrt_static", code, &[]), 0);
    compile_expect_error(
        "sqrt_static_domain",
        "double sqrt(double);\nstatic double z = sqrt(-1.0);\ndouble *p = &z;\n",
        "cannot initialize an object with static storage duration",
    );
}
