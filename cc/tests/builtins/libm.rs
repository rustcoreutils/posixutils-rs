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
///
/// That is glibc's `errno`. Apple's libm reports a domain error by the
/// exception alone (its `math_errhandling` is `MATH_ERREXCEPT`), so there the
/// same calls must leave `errno` at zero: c17 still calls the library for a
/// negative argument, and nothing on that path may write it.
const SQRT_PROGRAM: &str = r#"
typedef unsigned long long u64; typedef unsigned int u32;
double sqrt(double); float sqrtf(float); long double sqrtl(long double);
#ifdef __APPLE__
extern int *__error(void);
#define ERRNO (*__error())
#define DOMAIN_ERRNO 0
#else
extern int *__errno_location(void);
#define ERRNO (*__errno_location())
#define DOMAIN_ERRNO 33 /* EDOM */
#endif
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
        if (!__builtin_isnan(sqrt(x)) || ERRNO != DOMAIN_ERRNO) return 4;
        ERRNO = 0;
        if (!__builtin_isnan(__builtin_sqrt(x)) || ERRNO != DOMAIN_ERRNO) return 5;
        ERRNO = 0;
        (void)sqrt(x);
        if (ERRNO != DOMAIN_ERRNO) return 6;
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
        if (!__builtin_isnan(sqrtf(x)) || ERRNO != DOMAIN_ERRNO) return 14;
        ERRNO = 0;
        if (!__builtin_isnan(__builtin_sqrtf(x)) || ERRNO != DOMAIN_ERRNO) return 15;
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
    if (!__builtin_isnan(sqrtl(m1)) || ERRNO != DOMAIN_ERRNO) return 28;
    ERRNO = 0;
    if (!__builtin_isnan(__builtin_sqrtl(m1)) || ERRNO != DOMAIN_ERRNO) return 29;
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
    if (!__builtin_isnan(sqrt(-4.0)) || ERRNO != DOMAIN_ERRNO) return 38;
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

// ============================================================================
// floor, ceil, trunc, round, rint, nearbyint and their f forms
// ============================================================================

/// The rounding direction, set in the SSE or FP control register directly
/// so that the program needs no `<fenv.h>` and no libm: 0 to nearest, 1
/// downward, 2 upward, 3 toward zero.
const SET_ROUNDING: &str = r#"
static void set_rounding(int m) {
#if defined(__x86_64__)
    unsigned csr;
    __asm__ volatile("stmxcsr %0" : "=m"(csr));
    csr = (csr & ~0x6000u) | ((unsigned)m << 13);
    __asm__ volatile("ldmxcsr %0" : : "m"(csr));
#elif defined(__aarch64__)
    static const unsigned long rmode[4] = {0, 2, 1, 3};
    unsigned long fpcr;
    __asm__ volatile("mrs %0, fpcr" : "=r"(fpcr));
    fpcr = (fpcr & ~(3UL << 22)) | rmode[m] << 22;
    __asm__ volatile("msr fpcr, %0" : : "r"(fpcr));
#endif
}
"#;

/// Each input with its floor, ceil, trunc and round, and its rint -- which
/// nearbyint equals -- in each rounding direction: +-0, 0.5, 0.25, 0.75,
/// 1.5, 2.5, 2.75, 2^52 - 0.5, 2^52, 2^52 + 1, 2^53, 2^51 + 0.5, 1e300, the
/// smallest subnormal, the largest finite value, inf, a quiet NaN with a
/// payload and 100, each of both signs; then the same for `float`, around
/// 2^23. Every value is glibc's, identical on x86-64 and aarch64.
const ROUND_TABLES: &str = r#"
typedef unsigned long long u64; typedef unsigned int u32;
struct drow { u64 in, fl, ce, tr, ro, ri[4]; };
static const struct drow dcase[] = {
    {0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, {0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000}},
    {0x3fe0000000000000, 0x0000000000000000, 0x3ff0000000000000, 0x0000000000000000, 0x3ff0000000000000, {0x0000000000000000, 0x0000000000000000, 0x3ff0000000000000, 0x0000000000000000}},
    {0x3fd0000000000000, 0x0000000000000000, 0x3ff0000000000000, 0x0000000000000000, 0x0000000000000000, {0x0000000000000000, 0x0000000000000000, 0x3ff0000000000000, 0x0000000000000000}},
    {0x3fe8000000000000, 0x0000000000000000, 0x3ff0000000000000, 0x0000000000000000, 0x3ff0000000000000, {0x3ff0000000000000, 0x0000000000000000, 0x3ff0000000000000, 0x0000000000000000}},
    {0x3ff8000000000000, 0x3ff0000000000000, 0x4000000000000000, 0x3ff0000000000000, 0x4000000000000000, {0x4000000000000000, 0x3ff0000000000000, 0x4000000000000000, 0x3ff0000000000000}},
    {0x4004000000000000, 0x4000000000000000, 0x4008000000000000, 0x4000000000000000, 0x4008000000000000, {0x4000000000000000, 0x4000000000000000, 0x4008000000000000, 0x4000000000000000}},
    {0x4006000000000000, 0x4000000000000000, 0x4008000000000000, 0x4000000000000000, 0x4008000000000000, {0x4008000000000000, 0x4000000000000000, 0x4008000000000000, 0x4000000000000000}},
    {0x432fffffffffffff, 0x432ffffffffffffe, 0x4330000000000000, 0x432ffffffffffffe, 0x4330000000000000, {0x4330000000000000, 0x432ffffffffffffe, 0x4330000000000000, 0x432ffffffffffffe}},
    {0x4330000000000000, 0x4330000000000000, 0x4330000000000000, 0x4330000000000000, 0x4330000000000000, {0x4330000000000000, 0x4330000000000000, 0x4330000000000000, 0x4330000000000000}},
    {0x4330000000000001, 0x4330000000000001, 0x4330000000000001, 0x4330000000000001, 0x4330000000000001, {0x4330000000000001, 0x4330000000000001, 0x4330000000000001, 0x4330000000000001}},
    {0x4340000000000000, 0x4340000000000000, 0x4340000000000000, 0x4340000000000000, 0x4340000000000000, {0x4340000000000000, 0x4340000000000000, 0x4340000000000000, 0x4340000000000000}},
    {0x4320000000000001, 0x4320000000000000, 0x4320000000000002, 0x4320000000000000, 0x4320000000000002, {0x4320000000000000, 0x4320000000000000, 0x4320000000000002, 0x4320000000000000}},
    {0x7e37e43c8800759c, 0x7e37e43c8800759c, 0x7e37e43c8800759c, 0x7e37e43c8800759c, 0x7e37e43c8800759c, {0x7e37e43c8800759c, 0x7e37e43c8800759c, 0x7e37e43c8800759c, 0x7e37e43c8800759c}},
    {0x0000000000000001, 0x0000000000000000, 0x3ff0000000000000, 0x0000000000000000, 0x0000000000000000, {0x0000000000000000, 0x0000000000000000, 0x3ff0000000000000, 0x0000000000000000}},
    {0x7fefffffffffffff, 0x7fefffffffffffff, 0x7fefffffffffffff, 0x7fefffffffffffff, 0x7fefffffffffffff, {0x7fefffffffffffff, 0x7fefffffffffffff, 0x7fefffffffffffff, 0x7fefffffffffffff}},
    {0x7ff0000000000000, 0x7ff0000000000000, 0x7ff0000000000000, 0x7ff0000000000000, 0x7ff0000000000000, {0x7ff0000000000000, 0x7ff0000000000000, 0x7ff0000000000000, 0x7ff0000000000000}},
    {0x7ff8000000001234, 0x7ff8000000001234, 0x7ff8000000001234, 0x7ff8000000001234, 0x7ff8000000001234, {0x7ff8000000001234, 0x7ff8000000001234, 0x7ff8000000001234, 0x7ff8000000001234}},
    {0x4059000000000000, 0x4059000000000000, 0x4059000000000000, 0x4059000000000000, 0x4059000000000000, {0x4059000000000000, 0x4059000000000000, 0x4059000000000000, 0x4059000000000000}},
    {0x8000000000000000, 0x8000000000000000, 0x8000000000000000, 0x8000000000000000, 0x8000000000000000, {0x8000000000000000, 0x8000000000000000, 0x8000000000000000, 0x8000000000000000}},
    {0xbfe0000000000000, 0xbff0000000000000, 0x8000000000000000, 0x8000000000000000, 0xbff0000000000000, {0x8000000000000000, 0xbff0000000000000, 0x8000000000000000, 0x8000000000000000}},
    {0xbfd0000000000000, 0xbff0000000000000, 0x8000000000000000, 0x8000000000000000, 0x8000000000000000, {0x8000000000000000, 0xbff0000000000000, 0x8000000000000000, 0x8000000000000000}},
    {0xbfe8000000000000, 0xbff0000000000000, 0x8000000000000000, 0x8000000000000000, 0xbff0000000000000, {0xbff0000000000000, 0xbff0000000000000, 0x8000000000000000, 0x8000000000000000}},
    {0xbff8000000000000, 0xc000000000000000, 0xbff0000000000000, 0xbff0000000000000, 0xc000000000000000, {0xc000000000000000, 0xc000000000000000, 0xbff0000000000000, 0xbff0000000000000}},
    {0xc004000000000000, 0xc008000000000000, 0xc000000000000000, 0xc000000000000000, 0xc008000000000000, {0xc000000000000000, 0xc008000000000000, 0xc000000000000000, 0xc000000000000000}},
    {0xc006000000000000, 0xc008000000000000, 0xc000000000000000, 0xc000000000000000, 0xc008000000000000, {0xc008000000000000, 0xc008000000000000, 0xc000000000000000, 0xc000000000000000}},
    {0xc32fffffffffffff, 0xc330000000000000, 0xc32ffffffffffffe, 0xc32ffffffffffffe, 0xc330000000000000, {0xc330000000000000, 0xc330000000000000, 0xc32ffffffffffffe, 0xc32ffffffffffffe}},
    {0xc330000000000000, 0xc330000000000000, 0xc330000000000000, 0xc330000000000000, 0xc330000000000000, {0xc330000000000000, 0xc330000000000000, 0xc330000000000000, 0xc330000000000000}},
    {0xc330000000000001, 0xc330000000000001, 0xc330000000000001, 0xc330000000000001, 0xc330000000000001, {0xc330000000000001, 0xc330000000000001, 0xc330000000000001, 0xc330000000000001}},
    {0xc340000000000000, 0xc340000000000000, 0xc340000000000000, 0xc340000000000000, 0xc340000000000000, {0xc340000000000000, 0xc340000000000000, 0xc340000000000000, 0xc340000000000000}},
    {0xc320000000000001, 0xc320000000000002, 0xc320000000000000, 0xc320000000000000, 0xc320000000000002, {0xc320000000000000, 0xc320000000000002, 0xc320000000000000, 0xc320000000000000}},
    {0xfe37e43c8800759c, 0xfe37e43c8800759c, 0xfe37e43c8800759c, 0xfe37e43c8800759c, 0xfe37e43c8800759c, {0xfe37e43c8800759c, 0xfe37e43c8800759c, 0xfe37e43c8800759c, 0xfe37e43c8800759c}},
    {0x8000000000000001, 0xbff0000000000000, 0x8000000000000000, 0x8000000000000000, 0x8000000000000000, {0x8000000000000000, 0xbff0000000000000, 0x8000000000000000, 0x8000000000000000}},
    {0xffefffffffffffff, 0xffefffffffffffff, 0xffefffffffffffff, 0xffefffffffffffff, 0xffefffffffffffff, {0xffefffffffffffff, 0xffefffffffffffff, 0xffefffffffffffff, 0xffefffffffffffff}},
    {0xfff0000000000000, 0xfff0000000000000, 0xfff0000000000000, 0xfff0000000000000, 0xfff0000000000000, {0xfff0000000000000, 0xfff0000000000000, 0xfff0000000000000, 0xfff0000000000000}},
    {0xfff8000000001234, 0xfff8000000001234, 0xfff8000000001234, 0xfff8000000001234, 0xfff8000000001234, {0xfff8000000001234, 0xfff8000000001234, 0xfff8000000001234, 0xfff8000000001234}},
    {0xc059000000000000, 0xc059000000000000, 0xc059000000000000, 0xc059000000000000, 0xc059000000000000, {0xc059000000000000, 0xc059000000000000, 0xc059000000000000, 0xc059000000000000}},
};
struct frow { u32 in, fl, ce, tr, ro, ri[4]; };
static const struct frow fcase[] = {
    {0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, {0x00000000, 0x00000000, 0x00000000, 0x00000000}},
    {0x3f000000, 0x00000000, 0x3f800000, 0x00000000, 0x3f800000, {0x00000000, 0x00000000, 0x3f800000, 0x00000000}},
    {0x3e800000, 0x00000000, 0x3f800000, 0x00000000, 0x00000000, {0x00000000, 0x00000000, 0x3f800000, 0x00000000}},
    {0x3f400000, 0x00000000, 0x3f800000, 0x00000000, 0x3f800000, {0x3f800000, 0x00000000, 0x3f800000, 0x00000000}},
    {0x3fc00000, 0x3f800000, 0x40000000, 0x3f800000, 0x40000000, {0x40000000, 0x3f800000, 0x40000000, 0x3f800000}},
    {0x40200000, 0x40000000, 0x40400000, 0x40000000, 0x40400000, {0x40000000, 0x40000000, 0x40400000, 0x40000000}},
    {0x40300000, 0x40000000, 0x40400000, 0x40000000, 0x40400000, {0x40400000, 0x40000000, 0x40400000, 0x40000000}},
    {0x4affffff, 0x4afffffe, 0x4b000000, 0x4afffffe, 0x4b000000, {0x4b000000, 0x4afffffe, 0x4b000000, 0x4afffffe}},
    {0x4b000000, 0x4b000000, 0x4b000000, 0x4b000000, 0x4b000000, {0x4b000000, 0x4b000000, 0x4b000000, 0x4b000000}},
    {0x4b000001, 0x4b000001, 0x4b000001, 0x4b000001, 0x4b000001, {0x4b000001, 0x4b000001, 0x4b000001, 0x4b000001}},
    {0x4b800000, 0x4b800000, 0x4b800000, 0x4b800000, 0x4b800000, {0x4b800000, 0x4b800000, 0x4b800000, 0x4b800000}},
    {0x4a800001, 0x4a800000, 0x4a800002, 0x4a800000, 0x4a800002, {0x4a800000, 0x4a800000, 0x4a800002, 0x4a800000}},
    {0x7e967699, 0x7e967699, 0x7e967699, 0x7e967699, 0x7e967699, {0x7e967699, 0x7e967699, 0x7e967699, 0x7e967699}},
    {0x00000001, 0x00000000, 0x3f800000, 0x00000000, 0x00000000, {0x00000000, 0x00000000, 0x3f800000, 0x00000000}},
    {0x7f7fffff, 0x7f7fffff, 0x7f7fffff, 0x7f7fffff, 0x7f7fffff, {0x7f7fffff, 0x7f7fffff, 0x7f7fffff, 0x7f7fffff}},
    {0x7f800000, 0x7f800000, 0x7f800000, 0x7f800000, 0x7f800000, {0x7f800000, 0x7f800000, 0x7f800000, 0x7f800000}},
    {0x7fc01234, 0x7fc01234, 0x7fc01234, 0x7fc01234, 0x7fc01234, {0x7fc01234, 0x7fc01234, 0x7fc01234, 0x7fc01234}},
    {0x42c80000, 0x42c80000, 0x42c80000, 0x42c80000, 0x42c80000, {0x42c80000, 0x42c80000, 0x42c80000, 0x42c80000}},
    {0x80000000, 0x80000000, 0x80000000, 0x80000000, 0x80000000, {0x80000000, 0x80000000, 0x80000000, 0x80000000}},
    {0xbf000000, 0xbf800000, 0x80000000, 0x80000000, 0xbf800000, {0x80000000, 0xbf800000, 0x80000000, 0x80000000}},
    {0xbe800000, 0xbf800000, 0x80000000, 0x80000000, 0x80000000, {0x80000000, 0xbf800000, 0x80000000, 0x80000000}},
    {0xbf400000, 0xbf800000, 0x80000000, 0x80000000, 0xbf800000, {0xbf800000, 0xbf800000, 0x80000000, 0x80000000}},
    {0xbfc00000, 0xc0000000, 0xbf800000, 0xbf800000, 0xc0000000, {0xc0000000, 0xc0000000, 0xbf800000, 0xbf800000}},
    {0xc0200000, 0xc0400000, 0xc0000000, 0xc0000000, 0xc0400000, {0xc0000000, 0xc0400000, 0xc0000000, 0xc0000000}},
    {0xc0300000, 0xc0400000, 0xc0000000, 0xc0000000, 0xc0400000, {0xc0400000, 0xc0400000, 0xc0000000, 0xc0000000}},
    {0xcaffffff, 0xcb000000, 0xcafffffe, 0xcafffffe, 0xcb000000, {0xcb000000, 0xcb000000, 0xcafffffe, 0xcafffffe}},
    {0xcb000000, 0xcb000000, 0xcb000000, 0xcb000000, 0xcb000000, {0xcb000000, 0xcb000000, 0xcb000000, 0xcb000000}},
    {0xcb000001, 0xcb000001, 0xcb000001, 0xcb000001, 0xcb000001, {0xcb000001, 0xcb000001, 0xcb000001, 0xcb000001}},
    {0xcb800000, 0xcb800000, 0xcb800000, 0xcb800000, 0xcb800000, {0xcb800000, 0xcb800000, 0xcb800000, 0xcb800000}},
    {0xca800001, 0xca800002, 0xca800000, 0xca800000, 0xca800002, {0xca800000, 0xca800002, 0xca800000, 0xca800000}},
    {0xfe967699, 0xfe967699, 0xfe967699, 0xfe967699, 0xfe967699, {0xfe967699, 0xfe967699, 0xfe967699, 0xfe967699}},
    {0x80000001, 0xbf800000, 0x80000000, 0x80000000, 0x80000000, {0x80000000, 0xbf800000, 0x80000000, 0x80000000}},
    {0xff7fffff, 0xff7fffff, 0xff7fffff, 0xff7fffff, 0xff7fffff, {0xff7fffff, 0xff7fffff, 0xff7fffff, 0xff7fffff}},
    {0xff800000, 0xff800000, 0xff800000, 0xff800000, 0xff800000, {0xff800000, 0xff800000, 0xff800000, 0xff800000}},
    {0xffc01234, 0xffc01234, 0xffc01234, 0xffc01234, 0xffc01234, {0xffc01234, 0xffc01234, 0xffc01234, 0xffc01234}},
    {0xc2c80000, 0xc2c80000, 0xc2c80000, 0xc2c80000, 0xc2c80000, {0xc2c80000, 0xc2c80000, 0xc2c80000, 0xc2c80000}},
};
#define N(a) (int)(sizeof(a) / sizeof((a)[0]))
static u64 dbits(double d) { u64 u; __builtin_memcpy(&u, &d, 8); return u; }
static double dfrom(u64 u) { double d; __builtin_memcpy(&d, &u, 8); return d; }
static u32 fbits(float f) { u32 u; __builtin_memcpy(&u, &f, 4); return u; }
static float ffrom(u32 u) { float f; __builtin_memcpy(&f, &u, 4); return f; }
"#;

/// Every function of every row, bare and `__builtin_`, in all four rounding
/// directions, at run time; then the constants the optimizer folds. The
/// code says which: 1 + function + 6 * direction for `double`, 40 + the
/// same for `float`.
const ROUND_CHECKS: &str = r#"
double floor(double); double ceil(double); double trunc(double);
double round(double); double rint(double); double nearbyint(double);
float floorf(float); float ceilf(float); float truncf(float);
float roundf(float); float rintf(float); float nearbyintf(float);
__attribute__((noinline)) static int check_double(int m) {
    for (int i = 0; i < N(dcase); i++) {
        const struct drow *r = &dcase[i];
        volatile double x = dfrom(r->in);
        u64 want[6] = {r->fl, r->ce, r->tr, r->ro, r->ri[m], r->ri[m]};
        u64 bare[6] = {dbits(floor(x)), dbits(ceil(x)), dbits(trunc(x)),
                       dbits(round(x)), dbits(rint(x)), dbits(nearbyint(x))};
        u64 blt[6] = {dbits(__builtin_floor(x)), dbits(__builtin_ceil(x)),
                      dbits(__builtin_trunc(x)), dbits(__builtin_round(x)),
                      dbits(__builtin_rint(x)), dbits(__builtin_nearbyint(x))};
        for (int f = 0; f < 6; f++)
            if (bare[f] != want[f] || blt[f] != want[f]) return 1 + f + 6 * m;
    }
    return 0;
}
__attribute__((noinline)) static int check_float(int m) {
    for (int i = 0; i < N(fcase); i++) {
        const struct frow *r = &fcase[i];
        volatile float x = ffrom(r->in);
        u32 want[6] = {r->fl, r->ce, r->tr, r->ro, r->ri[m], r->ri[m]};
        u32 bare[6] = {fbits(floorf(x)), fbits(ceilf(x)), fbits(truncf(x)),
                       fbits(roundf(x)), fbits(rintf(x)), fbits(nearbyintf(x))};
        u32 blt[6] = {fbits(__builtin_floorf(x)), fbits(__builtin_ceilf(x)),
                      fbits(__builtin_truncf(x)), fbits(__builtin_roundf(x)),
                      fbits(__builtin_rintf(x)), fbits(__builtin_nearbyintf(x))};
        /* A float argument to the double function is narrowed, exactly. */
        double wide[6] = {floor(x), ceil(x), trunc(x), round(x), rint(x), nearbyint(x)};
        for (int f = 0; f < 6; f++) {
            if (bare[f] != want[f] || blt[f] != want[f]) return 40 + f + 6 * m;
            if (fbits((float)wide[f]) != want[f]) return 70 + f + 6 * m;
        }
    }
    return 0;
}
/* rint and nearbyint of a constant that is not an integer depend on the
   direction, so they are not folded; everything else is. */
__attribute__((noinline)) static int check_constants(int m) {
    static const u64 rint_2_5[4] = {0x4000000000000000, 0x4000000000000000,
                                    0x4008000000000000, 0x4000000000000000};
    static const u64 nearbyint_m2_5[4] = {0xc000000000000000, 0xc008000000000000,
                                          0xc000000000000000, 0xc000000000000000};
    if (dbits(rint(2.5)) != rint_2_5[m]) return 101;
    if (dbits(nearbyint(-2.5)) != nearbyint_m2_5[m]) return 102;
    if (dbits(floor(-0.5)) != 0xbff0000000000000) return 103;
    if (dbits(ceil(-0.5)) != 0x8000000000000000) return 104;
    if (dbits(trunc(-2.75)) != 0xc000000000000000) return 105;
    if (dbits(round(2.5)) != 0x4008000000000000) return 106;
    if (dbits(round(-0.25)) != 0x8000000000000000) return 107;
    if (dbits(rint(-3.0)) != 0xc008000000000000) return 108;
    if (fbits(floorf(2.5f)) != 0x40000000) return 109;
    if (fbits(__builtin_roundf(-2.5f)) != 0xc0400000) return 110;
    if (dbits(floor(__builtin_nan("0x1234"))) != 0x7ff8000000001234) return 111;
    if (dbits(ceil(-__builtin_inf())) != 0xfff0000000000000) return 112;
    return 0;
}
int main(void) {
    int rc;
    for (int m = 0; m < 4; m++) {
        set_rounding(m);
        if ((rc = check_double(m)) || (rc = check_float(m)) || (rc = check_constants(m))) {
            set_rounding(0);
            return rc;
        }
    }
    set_rounding(0);
    /* The result of the double function is a double, narrowed or not. */
    volatile float f = 1.5f;
    if (sizeof(floor(f)) != sizeof(double)) return 120;
    if (_Generic(ceil(f), double: 0, default: 1)) return 121;
    return 0;
}
"#;

fn round_program() -> String {
    format!("{SET_ROUNDING}{ROUND_TABLES}{ROUND_CHECKS}")
}

#[test]
fn libm_rounding_values_in_every_direction() {
    run_everywhere("round_modes", &round_program(), &[], true);
}

/// What each target computes in place needs no libm, as under gcc: all six
/// on aarch64; on x86-64 all but `round` and `nearbyint`, which SSE2 has no
/// sequence for (the `rint` one raises *inexact*). At `-O0` only the
/// `__builtin_` spellings are computed in place.
const ROUND_NO_LIBM_PROGRAM: &str = r#"
double floor(double); double ceil(double); double trunc(double);
double round(double); double rint(double); double nearbyint(double);
float floorf(float); float ceilf(float); float truncf(float);
float roundf(float); float rintf(float); float nearbyintf(float);
/* Not in `main`, nor in anything only `main` calls: gcc compiles code it
   knows runs once for size, and calls these there. */
int check(void);
int check(void) {
    volatile double x = -2.5;
    volatile float f = 2.5f;
    if (__builtin_floor(x) != -3.0 || __builtin_ceil(x) != -2.0) return 1;
    if (__builtin_trunc(x) != -2.0 || __builtin_rint(x) != -2.0) return 2;
    if (__builtin_floorf(f) != 2.0f || __builtin_ceilf(f) != 3.0f) return 3;
    if (__builtin_truncf(f) != 2.0f || __builtin_rintf(f) != 2.0f) return 4;
#ifdef __OPTIMIZE__
    if (floor(x) != -3.0 || ceil(x) != -2.0 || trunc(x) != -2.0 || rint(x) != -2.0) return 5;
    if (floorf(f) != 2.0f || ceilf(f) != 3.0f || truncf(f) != 2.0f || rintf(f) != 2.0f) return 6;
#endif
#ifdef __aarch64__
    if (__builtin_round(x) != -3.0 || __builtin_nearbyint(x) != -2.0) return 7;
    if (__builtin_roundf(f) != 3.0f || __builtin_nearbyintf(f) != 2.0f) return 8;
#ifdef __OPTIMIZE__
    if (round(x) != -3.0 || nearbyint(x) != -2.0) return 9;
    if (roundf(f) != 3.0f || nearbyintf(f) != 2.0f) return 10;
#endif
#endif
    return 0;
}
int main(void) { return check(); }
"#;

#[test]
fn libm_rounding_in_place_needs_no_libm() {
    run_everywhere("round_no_libm", ROUND_NO_LIBM_PROGRAM, &[], false);
}

const ROUND_NAMES: [&str; 12] = [
    "floor",
    "ceil",
    "trunc",
    "round",
    "rint",
    "nearbyint",
    "floorf",
    "ceilf",
    "truncf",
    "roundf",
    "rintf",
    "nearbyintf",
];

/// A function computing each of the twelve, spelled `prefix` + name.
fn round_source(prefix: &str) -> String {
    let mut src = String::from(
        "double floor(double); double ceil(double); double trunc(double);\n\
         double round(double); double rint(double); double nearbyint(double);\n\
         float floorf(float); float ceilf(float); float truncf(float);\n\
         float roundf(float); float rintf(float); float nearbyintf(float);\n",
    );
    for name in ROUND_NAMES {
        let t = if name.ends_with('f') {
            "float"
        } else {
            "double"
        };
        src.push_str(&format!(
            "{t} t_{name}({t} x) {{ return {prefix}{name}(x); }}\n"
        ));
    }
    src
}

/// The instructions: `frintm`, `frintp`, `frintz`, `frinta`, `frintx` and
/// `frinti` on aarch64; on x86-64 gcc's SSE2 sequences through `cvttsd2si`
/// for `floor`, `ceil` and `trunc` and the 2^52 addition for `rint`, and
/// calls to `round` and `nearbyint`, which gcc keeps too.
#[test]
fn libm_rounding_is_computed_in_place() {
    for prefix in ["", "__builtin_"] {
        let [(_, host), (_, a64)] = asm_both("round_o2", &round_source(prefix), &["-O2"]);
        for insn in ["frintm", "frintp", "frintz", "frinta", "frintx", "frinti"] {
            assert!(has_insn(&a64, insn), "{prefix}: no {insn}:\n{a64}");
        }
        assert!(
            !calls_any(&a64, &ROUND_NAMES),
            "{prefix}: a call remains:\n{a64}"
        );
        if cfg!(target_arch = "x86_64") {
            assert!(
                has_insn(&host, "cvttsd2si") || has_insn(&host, "cvttsd2siq"),
                "{host}"
            );
            assert!(
                has_insn(&host, "cvttss2si") || has_insn(&host, "cvttss2sil"),
                "{host}"
            );
            let called = [
                "floor", "ceil", "trunc", "rint", "floorf", "ceilf", "truncf", "rintf",
            ];
            assert!(
                !calls_any(&host, &called),
                "{prefix}: a call remains:\n{host}"
            );
            for kept in ["round", "nearbyint", "roundf", "nearbyintf"] {
                assert!(
                    calls_any(&host, &[kept]),
                    "{prefix}: {kept} not called:\n{host}"
                );
            }
        }
    }
}

/// At `-O0` the bare spellings are calls, and the `__builtin_` ones are
/// computed in place where the target can, as in gcc.
#[test]
fn libm_rounding_at_o0_follows_gcc() {
    for (target, asm) in asm_both("round_o0", &round_source(""), &["-O0"]) {
        for name in ROUND_NAMES {
            assert!(
                calls_any(&asm, &[name]),
                "{target}: {name} not called:\n{asm}"
            );
        }
    }
    let [(_, host), (_, a64)] = asm_both("round_o0_b", &round_source("__builtin_"), &["-O0"]);
    assert!(!calls_any(&a64, &ROUND_NAMES), "a call remains:\n{a64}");
    if cfg!(target_arch = "x86_64") {
        assert!(
            !calls_any(&host, &["floor", "ceil", "trunc", "rint"]),
            "{host}"
        );
    }
}

/// `-fno-builtin-floor` keeps the call to `floor`, and only that one.
#[test]
fn libm_rounding_fno_builtin_keeps_the_call() {
    let opts = ["-O2", "-fno-builtin-floor"];
    let src = "double floor(double);\ndouble d(double x) { return floor(x); }\n";
    for (target, asm) in asm_both("floor_nb", src, &opts) {
        assert!(calls_any(&asm, &["floor"]), "{target}:\n{asm}");
    }
    let src = "double ceil(double);\ndouble d(double x) { return ceil(x); }\n";
    for (target, asm) in asm_both("ceil_nb", src, &opts) {
        assert!(!calls_any(&asm, &["ceil"]), "{target}:\n{asm}");
    }
}

/// Constants fold, in code and in static initializers, except a `rint` or
/// `nearbyint` of a value that is not already an integer: its answer is the
/// current rounding direction's, and gcc leaves it too.
#[test]
fn libm_rounding_of_constants() {
    let src = "double floor(double); double ceil(double); double round(double);\n\
               double f(void) { return floor(-0.5) + ceil(2.5) + round(0.5)\n\
               + __builtin_trunc(-1.5) + __builtin_rint(3.0) + __builtin_nearbyintf(-4.0f); }\n";
    for (target, asm) in asm_both("round_fold", src, &["-O2"]) {
        assert!(!calls_any(&asm, &ROUND_NAMES), "{target}:\n{asm}");
        assert!(
            !asm.contains("frint") && !asm.contains("cvtt"),
            "{target}:\n{asm}"
        );
    }
    let src = "double rint(double);\ndouble f(void) { return rint(2.5); }\n";
    let [(_, _), (_, a64)] = asm_both("rint_nofold", src, &["-O2"]);
    assert!(has_insn(&a64, "frintx"), "rint(2.5) folded:\n{a64}");

    let code = r#"
double floor(double); double ceil(double); double trunc(double);
double round(double); double rint(double); float roundf(float);
static double a = floor(-2.5);
static double b = ceil(-0.5);
static double c = trunc(2.75);
static double d = round(-2.5);
static float e = roundf(0.5f);
static double f = rint(-7.0);
int main(void) {
    if (a != -3.0 || b != 0.0 || !__builtin_signbit(b)) return 1;
    if (c != 2.0 || d != -3.0 || e != 1.0f || f != -7.0) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("round_static", code, &[]), 0);
    compile_expect_error(
        "rint_static_inexact",
        "double rint(double);\nstatic double z = rint(2.5);\ndouble *p = &z;\n",
        "cannot initialize an object with static storage duration",
    );
}

/// A translation unit's own definition of `sqrt` or a rounding is the
/// function its calls reach -- above the definition too, and through an
/// old-style definition that takes no arguments at all (gcc's torture test
/// ieee/20030331-1 defines `float rintf()` so) -- as under gcc, which folds
/// only `abs`, `fabs`, `copysign` and the like regardless.
const OWN_DEFINITIONS_PROGRAM: &str = r#"
double sqrt(double); double floor(double); double fma(double, double, double);
static volatile int calls;
double sqrt(double x) { calls++; return x + 100.0; }
static double use_floor(double x) { return floor(x); }
double floor(double x) { calls++; return x + 200.0; }
float x = -1.5f;
float rintf() { calls++; return x + 300.0f; }
double fma(double a, double b, double c) { calls++; return a + b + c; }
int main(void) {
    volatile double v = 4.0;
    if (sqrt(v) != 104.0 || use_floor(v) != 204.0 || rintf() != 298.5f) return 1;
    if (fma(v, v, v) != 12.0) return 4;
    if (calls != 4) return 2;
    /* Not pure any more: an untaken arm does not call. */
    volatile int no = 0;
    double r = no ? floor(v) : 1.0;
    if (r != 1.0 || calls != 4) return 3;
    return 0;
}
"#;

#[test]
fn libm_own_definition_is_called() {
    run_everywhere("own_defs", OWN_DEFINITIONS_PROGRAM, &[], false);
}

/// A `float` argument does not hide the program's `floor` behind `floorf`:
/// the definition displaces the call the program wrote, which is to
/// `floor`, and that call receives the argument converted to `double` and
/// answers a `double` -- `x + 0.1` is not a `float`, so a result narrowed on
/// the way would show. Above the definition and below it, bare and
/// reserved, as a `double` and a `float` result.
const OWN_FLOOR_OF_A_FLOAT_PROGRAM: &str = r#"
double floor(double);
static volatile int calls;
volatile float fv = 2.5f;
static double below_bare(float x) { return floor(x); }
static double below_reserved(float x) { return __builtin_floor(x); }
static float below_float(float x) { return floor(x); }
double floor(double x) { calls++; return x + 0.1; }
static double above_bare(float x) { return floor(x); }
static double above_reserved(float x) { return __builtin_floor(x); }
static float above_float(float x) { return floor(x); }
int main(void) {
    float x = fv;
    if (below_bare(x) != 2.5 + 0.1) return 1;
    if (below_reserved(x) != 2.5 + 0.1) return 2;
    if (below_float(x) != (float)(2.5 + 0.1)) return 3;
    if (above_bare(x) != 2.5 + 0.1) return 4;
    if (above_reserved(x) != 2.5 + 0.1) return 5;
    if (above_float(x) != (float)(2.5 + 0.1)) return 6;
    if (calls != 6) return 7;
    return 0;
}
"#;

#[test]
fn libm_own_definition_is_called_for_a_float_argument() {
    run_everywhere("own_floor_float", OWN_FLOOR_OF_A_FLOAT_PROGRAM, &[], false);
}

/// A weak definition displaces nothing: a `float` argument is still floored
/// in place, at `float`. At `-O0` the bare spelling is a call, as under gcc,
/// and the call is the one the program wrote -- to `floor`, which this
/// program defines.
const WEAK_FLOOR_OF_A_FLOAT_PROGRAM: &str = r#"
__attribute__((weak)) double floor(double x) { return -1.0; }
static double bare(float x) { return floor(x); }
static double reserved(float x) { return __builtin_floor(x); }
volatile float fv = 2.5f;
int main(void) {
    if (reserved(fv) != 2.0) return 1;
#ifdef __OPTIMIZE__
    if (bare(fv) != 2.0) return 2;
#else
    if (bare(fv) != -1.0) return 3;
#endif
    return 0;
}
"#;

/// A call whose answer is a constant is that constant at every level, as
/// under gcc: at `-O0`, where a bare libm spelling is otherwise a call, it
/// still initializes a static object, and a rounding of a constant calls
/// nothing. (A root keeps its call for `errno` until the optimizer folds it.)
/// One whose answer is not a constant -- a root's domain error, a `rint` of
/// a value that is not an integer -- stays a call, and cannot.
const CONSTANT_LIBM_CALLS_PROGRAM: &str = r#"
double floor(double), sqrt(double), fmin(double, double), fma(double, double, double);
float floorf(float);
static double a = floor(2.5);
static double b = sqrt(4.0);
static double c = fmin(1.0, 2.0);
static double d = fma(1.0, 2.0, 3.0);
static float e = floorf(2.5f);
static double f = floor(2.5f);
int main(void) {
    if (a != 2.0 || b != 2.0 || c != 1.0 || d != 5.0 || e != 2.0f || f != 2.0) return 1;
    if (floor(-2.5) != -3.0 || sqrt(9.0) != 3.0) return 2;
    return 0;
}
"#;

#[test]
fn libm_constant_call_is_a_constant_at_every_level() {
    run_everywhere("libm_const_calls", CONSTANT_LIBM_CALLS_PROGRAM, &[], true);
    let src = "double floor(double);\ndouble g(void) { return floor(2.5); }\n";
    for target in [None, Some("aarch64-unknown-linux-gnu")] {
        let mut args = vec!["-O0"];
        if let Some(t) = target {
            args.extend(["--target", t]);
        }
        let asm = asm_for_at("libm_const_o0", src, &args);
        assert!(
            !calls_any(&asm, &["floor"]),
            "{target:?}: a call remains:\n{asm}"
        );
    }
    compile_expect_error(
        "sqrt_static_domain_error",
        "double sqrt(double);\nstatic double z = sqrt(-1.0);\ndouble *p = &z;\n",
        "cannot initialize an object with static storage duration",
    );
}

#[test]
fn libm_weak_definition_does_not_displace_a_float_argument() {
    run_everywhere(
        "weak_floor_float",
        WEAK_FLOOR_OF_A_FLOAT_PROGRAM,
        &[],
        false,
    );
}

// ============================================================================
// fmin, fmax, fma and their f forms
// ============================================================================

/// Run-time values bit for bit against glibc's, which agree on x86-64 and
/// aarch64 except where C leaves the answer open: the signed zeros of
/// `fmin` and `fmax` (glibc's x86-64 functions answer the first operand;
/// aarch64's `fminnm` and `fmaxnm`, and gcc's fold, the -0 and +0), checked
/// on aarch64 only, and which NaN two NaNs give, checked for being one. The
/// `fma` cases are the ones a separate multiply and add get wrong: the
/// product's low half cancelling against the addend, a tie decided by the
/// bits below it, an overflow, and a subnormal result.
const MIN_MAX_FMA_PROGRAM: &str = r#"
typedef unsigned long long u64; typedef unsigned int u32;
double fmin(double, double); double fmax(double, double); double fma(double, double, double);
float fminf(float, float); float fmaxf(float, float); float fmaf(float, float, float);
static u64 dbits(double d) { u64 u; __builtin_memcpy(&u, &d, 8); return u; }
static double dfrom(u64 u) { double d; __builtin_memcpy(&d, &u, 8); return d; }
static u32 fbits(float f) { u32 u; __builtin_memcpy(&u, &f, 4); return u; }
static float ffrom(u32 u) { float f; __builtin_memcpy(&f, &u, 4); return f; }
#define N(a) (int)(sizeof(a) / sizeof((a)[0]))
static const u64 dfma[][4] = {
    {0x3ff0000000000001, 0x3fefffffffffffff, 0xbff0000000000000, 0x3c9ffffffffffffe},
    {0x3fb999999999999a, 0x4024000000000000, 0xbff0000000000000, 0x3c90000000000000},
    {0x4000000000000000, 0x4008000000000000, 0x4010000000000000, 0x4024000000000000},
    {0x8000000000000000, 0x3ff0000000000000, 0x0000000000000000, 0x0000000000000000},
    {0x8000000000000000, 0x3ff0000000000000, 0x8000000000000000, 0x8000000000000000},
    {0x7fe0000000000000, 0x4000000000000000, 0xffe0000000000000, 0x7fe0000000000000},
    {0x0010000000000000, 0x3fe0000000000000, 0x0000000000000001, 0x0008000000000001},
    {0x3ff5555555555555, 0x4008000000000000, 0xc010000000000000, 0xbcb0000000000000},
    {0x4340000000000001, 0x3ff0000000000001, 0xc340000000000002, 0x3cc0000000000000},
    {0x3ff0000000000000, 0x3ff0000000000000, 0x3ca0000000000000, 0x3ff0000000000000},
    {0x3ff0000000000000, 0x3ff0000000000001, 0x3ca0000000000000, 0x3ff0000000000002},
    {0x7fefffffffffffff, 0x4000000000000000, 0x0000000000000000, 0x7ff0000000000000},
};
static const u32 ffma[][4] = {
    {0x3f800001, 0x3f7fffff, 0xbf800000, 0x337ffffe},
    {0x3dcccccd, 0x41200000, 0xbf800000, 0x32800000},
    {0x40000000, 0x40400000, 0x40800000, 0x41200000},
    {0x80000000, 0x3f800000, 0x00000000, 0x00000000},
    {0x7f000000, 0x40000000, 0xff000000, 0x7f000000},
    {0x00800000, 0x3f000000, 0x00000001, 0x00400001},
    {0x3faaaaab, 0x40400000, 0xc0800000, 0x34000000},
    {0x7f7fffff, 0x40000000, 0x00000000, 0x7f800000},
};
/* x, y, fmin, fmax: ordered pairs both ways, -inf, a quiet NaN on each
   side, two negatives, inf against the smallest subnormal, a tie. */
static const u64 dmm[][4] = {
    {0x3ff0000000000000, 0x4000000000000000, 0x3ff0000000000000, 0x4000000000000000},
    {0x4000000000000000, 0x3ff0000000000000, 0x3ff0000000000000, 0x4000000000000000},
    {0xfff0000000000000, 0x4008000000000000, 0xfff0000000000000, 0x4008000000000000},
    {0x7ff8000000001234, 0x4008000000000000, 0x4008000000000000, 0x4008000000000000},
    {0x4008000000000000, 0x7ff8000000001234, 0x4008000000000000, 0x4008000000000000},
    {0xc000000000000000, 0xbff0000000000000, 0xc000000000000000, 0xbff0000000000000},
    {0x7ff0000000000000, 0x0000000000000001, 0x0000000000000001, 0x7ff0000000000000},
    {0x3ff0000000000000, 0x3ff0000000000000, 0x3ff0000000000000, 0x3ff0000000000000},
};
static const u32 fmm[][4] = {
    {0x3f800000, 0x40000000, 0x3f800000, 0x40000000},
    {0x40000000, 0x3f800000, 0x3f800000, 0x40000000},
    {0xff800000, 0x40400000, 0xff800000, 0x40400000},
    {0x7fc01234, 0x40400000, 0x40400000, 0x40400000},
    {0x40400000, 0x7fc01234, 0x40400000, 0x40400000},
    {0xc0000000, 0xbf800000, 0xc0000000, 0xbf800000},
};
static int check_fma(void) {
    for (int i = 0; i < N(dfma); i++) {
        volatile double x = dfrom(dfma[i][0]), y = dfrom(dfma[i][1]), z = dfrom(dfma[i][2]);
        if (dbits(fma(x, y, z)) != dfma[i][3]) return 1;
        if (dbits(__builtin_fma(x, y, z)) != dfma[i][3]) return 2;
    }
    for (int i = 0; i < N(ffma); i++) {
        volatile float x = ffrom(ffma[i][0]), y = ffrom(ffma[i][1]), z = ffrom(ffma[i][2]);
        if (fbits(fmaf(x, y, z)) != ffma[i][3]) return 3;
        if (fbits(__builtin_fmaf(x, y, z)) != ffma[i][3]) return 4;
    }
    volatile double nan = __builtin_nan(""), one = 1.0;
    if (!__builtin_isnan(fma(nan, one, one)) || !__builtin_isnan(fma(one, one, nan))) return 5;
    return 0;
}
static int check_min_max(void) {
    for (int i = 0; i < N(dmm); i++) {
        volatile double x = dfrom(dmm[i][0]), y = dfrom(dmm[i][1]);
        if (dbits(fmin(x, y)) != dmm[i][2] || dbits(__builtin_fmin(x, y)) != dmm[i][2]) return 11;
        if (dbits(fmax(x, y)) != dmm[i][3] || dbits(__builtin_fmax(x, y)) != dmm[i][3]) return 12;
    }
    for (int i = 0; i < N(fmm); i++) {
        volatile float x = ffrom(fmm[i][0]), y = ffrom(fmm[i][1]);
        if (fbits(fminf(x, y)) != fmm[i][2] || fbits(__builtin_fminf(x, y)) != fmm[i][2]) return 13;
        if (fbits(fmaxf(x, y)) != fmm[i][3] || fbits(__builtin_fmaxf(x, y)) != fmm[i][3]) return 14;
    }
    volatile double n1 = __builtin_nan("1"), n2 = __builtin_nan("2");
    if (!__builtin_isnan(fmin(n1, n2)) || !__builtin_isnan(fmax(n1, n2))) return 15;
/* fminnm's zeros. At -O0 the bare call is the C library's, which C leaves
   free here: glibc's aarch64 function is fminnm, Apple's is not known to be. */
#if defined(__aarch64__) && (defined(__OPTIMIZE__) || !defined(__APPLE__))
    volatile double pz = 0.0, nz = -0.0;
    if (dbits(fmin(pz, nz)) != 0x8000000000000000 || dbits(fmin(nz, pz)) != 0x8000000000000000) return 16;
    if (dbits(fmax(pz, nz)) != 0 || dbits(fmax(nz, pz)) != 0) return 17;
#endif
    return 0;
}
/* Constants, which fold: the same answers, and gcc's for the zeros. */
static int check_constants(void) {
    if (dbits(fma(0x1.0000000000001p0, 0x1.fffffffffffffp-1, -1.0)) != 0x3c9ffffffffffffe) return 21;
    if (dbits(fma(0.1, 10.0, -1.0)) != 0x3c90000000000000) return 22;
    if (fbits(fmaf(0x1.000002p0f, 0x1.fffffep-1f, -1.0f)) != 0x337ffffe) return 23;
#if defined(__OPTIMIZE__) || defined(__aarch64__)
    /* Folded, or fminnm: at -O0 on x86-64 the call is glibc's, which
       answers the first operand. */
    if (dbits(fmin(0.0, -0.0)) != 0x8000000000000000 || dbits(fmin(-0.0, 0.0)) != 0x8000000000000000) return 24;
    if (dbits(fmax(0.0, -0.0)) != 0 || dbits(fmax(-0.0, 0.0)) != 0) return 25;
#endif
    if (fmin(__builtin_nan(""), 3.0) != 3.0 || fmaxf(2.0f, __builtin_nanf("")) != 2.0f) return 26;
    if (fmin(-__builtin_inf(), 1.0) != -__builtin_inf()) return 27;
    return 0;
}
int main(void) {
    int rc;
    if ((rc = check_fma()) || (rc = check_min_max())) return rc;
    return check_constants();
}
"#;

#[test]
fn libm_min_max_fma_values() {
    run_everywhere("min_max_fma", MIN_MAX_FMA_PROGRAM, &[], true);
}

/// On aarch64 all six are instructions, so the program needs no libm once
/// optimizing -- and at `-O0` only through the `__builtin_` spellings.
#[test]
fn libm_min_max_fma_on_aarch64_need_no_libm() {
    let code = r#"
double fmin(double, double); double fmax(double, double); double fma(double, double, double);
float fminf(float, float); float fmaxf(float, float); float fmaf(float, float, float);
int main(void) {
    volatile double a = 2.0, b = -3.0, c = 0.5;
    volatile float x = 2.0f, y = -3.0f, z = 0.5f;
    if (__builtin_fmin(a, b) != -3.0 || __builtin_fmax(a, b) != 2.0) return 1;
    if (__builtin_fma(a, b, c) != -5.5) return 2;
    if (__builtin_fminf(x, y) != -3.0f || __builtin_fmaxf(x, y) != 2.0f) return 3;
    if (__builtin_fmaf(x, y, z) != -5.5f) return 4;
#ifdef __OPTIMIZE__
    if (fmin(a, b) != -3.0 || fmax(a, b) != 2.0 || fma(a, b, c) != -5.5) return 5;
    if (fminf(x, y) != -3.0f || fmaxf(x, y) != 2.0f || fmaf(x, y, z) != -5.5f) return 6;
#endif
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        if let Some(rc) =
            compile_and_run_aarch64_with(&format!("mmf_nolibm{opt}"), code, &[opt], &[])
        {
            assert_eq!(rc, 0, "aarch64 {opt}");
        }
    }
}

const MIN_MAX_FMA_NAMES: [&str; 6] = ["fmin", "fmax", "fma", "fminf", "fmaxf", "fmaf"];

/// A function computing each of the six, spelled `prefix` + name.
fn min_max_fma_source(prefix: &str) -> String {
    let mut src = String::from(
        "double fmin(double, double); double fmax(double, double);\n\
         double fma(double, double, double); float fminf(float, float);\n\
         float fmaxf(float, float); float fmaf(float, float, float);\n",
    );
    for name in MIN_MAX_FMA_NAMES {
        let t = if name.ends_with('f') {
            "float"
        } else {
            "double"
        };
        let (params, args) = if name == "fma" || name == "fmaf" {
            (format!("{t} x, {t} y, {t} z"), "x, y, z")
        } else {
            (format!("{t} x, {t} y"), "x, y")
        };
        src.push_str(&format!(
            "{t} t_{name}({params}) {{ return {prefix}{name}({args}); }}\n"
        ));
    }
    src
}

/// `fminnm`, `fmaxnm` and `fmadd` on aarch64, as gcc emits them; calls on
/// x86-64, whose baseline has no FMA and whose `minsd` is not `fmin`, as in
/// gcc. At `-O0` the bare spellings are calls everywhere.
#[test]
fn libm_min_max_fma_instructions() {
    for prefix in ["", "__builtin_"] {
        let [(_, host), (_, a64)] = asm_both("mmf_o2", &min_max_fma_source(prefix), &["-O2"]);
        for insn in ["fminnm", "fmaxnm", "fmadd"] {
            assert!(has_insn(&a64, insn), "{prefix}: no {insn}:\n{a64}");
        }
        assert!(!calls_any(&a64, &MIN_MAX_FMA_NAMES), "{prefix}:\n{a64}");
        if cfg!(target_arch = "x86_64") {
            for name in MIN_MAX_FMA_NAMES {
                assert!(
                    calls_any(&host, &[name]),
                    "{prefix}: {name} not called:\n{host}"
                );
            }
        }
    }
    for (target, asm) in asm_both("mmf_o0", &min_max_fma_source(""), &["-O0"]) {
        for name in MIN_MAX_FMA_NAMES {
            assert!(
                calls_any(&asm, &[name]),
                "{target}: {name} not called:\n{asm}"
            );
        }
    }
    let [_, (_, a64)] = asm_both("mmf_o0_b", &min_max_fma_source("__builtin_"), &["-O0"]);
    assert!(!calls_any(&a64, &MIN_MAX_FMA_NAMES), "{a64}");
}

/// `-fno-builtin-fma` keeps the call to `fma`, and only that one.
#[test]
fn libm_fma_fno_builtin_keeps_the_call() {
    let src = "double fma(double, double, double); double fmin(double, double);\n\
               double f(double x, double y) { return fma(x, y, x) + fmin(x, y); }\n";
    let asm = asm_for_at(
        "fma_nb",
        src,
        &[
            "-O2",
            "-fno-builtin-fma",
            "--target",
            "aarch64-unknown-linux-gnu",
        ],
    );
    assert!(
        calls_any(&asm, &["fma"]) && !has_insn(&asm, "fmadd"),
        "{asm}"
    );
    assert!(
        has_insn(&asm, "fminnm") && !calls_any(&asm, &["fmin"]),
        "{asm}"
    );
}

/// Constants fold on both targets -- on x86-64 too, where the function is
/// otherwise a call -- exactly: `fma` rounds once, and the zeros of `fmin`
/// and `fmax` fold to gcc's -0 and +0. In a static initializer gcc refuses a
/// NaN argument, and so does this.
#[test]
fn libm_min_max_fma_of_constants() {
    let src = "double fmin(double, double); double fmax(double, double);\n\
               double fma(double, double, double);\n\
               double f(void) { return fmin(1.0, 2.0) + fmax(-1.0, __builtin_nan(\"\"))\n\
               + fma(0x1.0000000000001p0, 0x1.fffffffffffffp-1, -1.0)\n\
               + __builtin_fmaf(2.0f, 3.0f, 4.0f); }\n";
    for (target, asm) in asm_both("mmf_fold", src, &["-O2"]) {
        assert!(!calls_any(&asm, &MIN_MAX_FMA_NAMES), "{target}:\n{asm}");
        assert!(
            !asm.contains("fminnm") && !asm.contains("fmadd"),
            "{target}:\n{asm}"
        );
    }

    let code = r#"
typedef unsigned long long u64;
double fmin(double, double); double fmax(double, double);
double fma(double, double, double); float fmaf(float, float, float);
static double a = fmin(0.0, -0.0);
static double b = fmax(-0.0, 0.0);
static double c = fma(0x1.0000000000001p0, 0x1.fffffffffffffp-1, -1.0);
static float d = fmaf(2.0f, 3.0f, 4.0f);
static double e = fmin(-2.0, 1.0);
int main(void) {
    u64 u;
    __builtin_memcpy(&u, &a, 8);
    if (u != 0x8000000000000000ULL) return 1;
    __builtin_memcpy(&u, &b, 8);
    if (u != 0) return 2;
    __builtin_memcpy(&u, &c, 8);
    if (u != 0x3c9ffffffffffffeULL) return 3;
    if (d != 10.0f || e != -2.0) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("mmf_static", code, &[]), 0);
    compile_expect_error(
        "fmin_static_nan",
        "double fmin(double, double);\nstatic double z = fmin(__builtin_nan(\"\"), 3.0);\n\
         double *p = &z;\n",
        "cannot initialize an object with static storage duration",
    );
}
