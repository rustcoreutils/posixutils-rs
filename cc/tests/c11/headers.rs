//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Bundled headers and conditional feature macros.
//

use crate::common::{compile_and_run, preprocess_text, run_c17};

/// #H1: `<stdint.h>` is in the *freestanding* header set (C17 4p6), so the
/// implementation must supply it rather than lean on the host's.
#[test]
fn c17_bundled_stdint_provides_the_mandated_surface() {
    let src = r#"
        #include <stdint.h>
        int main(void) {
            if (sizeof(int8_t) != 1 || sizeof(int16_t) != 2) return 1;
            if (sizeof(int32_t) != 4 || sizeof(int64_t) != 8) return 2;
            if (sizeof(uint8_t) != 1 || sizeof(uint64_t) != 8) return 3;
            if (INT8_MAX != 127 || INT8_MIN != -128 || UINT8_MAX != 255) return 4;
            if (INT32_MAX != 2147483647) return 5;
            if (UINT64_MAX != 18446744073709551615ULL) return 6;
            if (SIZE_MAX == 0 || PTRDIFF_MAX == 0 || INTMAX_MAX == 0) return 7;
            /* least / fast / pointer-holding families */
            int_least16_t a = 1; int_fast32_t b = 2;
            intptr_t c = 3; uintptr_t d = 4; intmax_t e = 5; uintmax_t f = 6;
            if (a + b + (int)c + (int)d + (int)e + (int)f != 21) return 8;
            /* constant macros must produce the right *type*, not just value */
            if (sizeof(INT64_C(1)) != 8 || sizeof(UINT64_C(1)) != 8) return 9;
            if (INT64_C(-9223372036854775807) >= 0) return 10;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c17_bundled_stdint", src, &[]), 0);
}

/// The bundled copy must agree with the host's about what `int64_t` is, or
/// any translation unit including both fails. On LP64 that means `long`, not
/// `long long` — they are distinct types even at the same width.
#[test]
fn c17_bundled_stdint_agrees_with_system_headers() {
    let src = r#"
        #include <stdio.h>
        #include <inttypes.h>
        #include <stdint.h>
        #include <stdlib.h>
        #include <string.h>
        int main(void) {
            int64_t v = INT64_C(-1);
            uint64_t u = UINT64_C(1);
            char buf[64];
            snprintf(buf, sizeof buf, "%" PRId64 " %" PRIu64, v, u);
            return strcmp(buf, "-1 1") == 0 ? 0 : 1;
        }
    "#;
    assert_eq!(compile_and_run("c17_stdint_agrees", src, &[]), 0);
}

/// #X6: C11 7.17.6.1 mandates far more atomic aliases than the fixed-width
/// ones, and 7.17.5.1 mandates `atomic_is_lock_free`.
#[test]
fn c11_stdatomic_provides_the_mandated_typedefs() {
    let src = r#"
        #include <stdatomic.h>
        int main(void) {
            atomic_size_t sz; atomic_ptrdiff_t pd; atomic_intptr_t ip; atomic_uintptr_t up;
            atomic_intmax_t im; atomic_uintmax_t um; atomic_wchar_t wc;
            atomic_char16_t c16; atomic_char32_t c32;
            atomic_int_least8_t l8; atomic_uint_least64_t l64;
            atomic_int_fast16_t f16; atomic_uint_fast32_t f32;
            atomic_init(&sz, 1); atomic_init(&pd, 1); atomic_init(&ip, 1);
            atomic_init(&up, 1); atomic_init(&im, 1); atomic_init(&um, 1);
            atomic_init(&wc, 1); atomic_init(&c16, 1); atomic_init(&c32, 1);
            atomic_init(&l8, 1); atomic_init(&l64, 1);
            atomic_init(&f16, 1); atomic_init(&f32, 1);
            atomic_int ai; atomic_init(&ai, 0);
            if (!atomic_is_lock_free(&ai)) return 1;
            return (int)(atomic_load(&sz) + atomic_load(&l64)) - 2;
        }
    "#;
    assert_eq!(compile_and_run("c11_stdatomic_typedefs", src, &[]), 0);
}

/// #X7: `CMPLX` exists precisely so an infinite or NaN imaginary part can be
/// constructed exactly — `x + y*I` propagates the special value into the real
/// part too.
#[test]
fn c11_cmplx_constructs_exactly() {
    let src = r#"
        #include <complex.h>
        #include <math.h>
        int main(void) {
            double complex c = CMPLX(1.0, INFINITY);
            if (creal(c) != 1.0) return 1;
            if (!isinf(cimag(c))) return 2;
            /* the naive spelling cannot do this */
            double complex n = 1.0 + INFINITY * I;
            if (!isnan(creal(n))) return 3;
            return 0;
        }
    "#;
    assert_eq!(
        compile_and_run("c11_cmplx_exact", src, &["-lm".to_string()]),
        0
    );
}

/// #P7 / #X8: the conditional feature macros of C17 6.10.8.3.
#[test]
fn c17_conditional_feature_macros() {
    let r = preprocess_text(
        "c17_feature_macros",
        "IEC559=__STDC_IEC_559__ ISO10646=__STDC_ISO_10646__ U16=__STDC_UTF_16__ U32=__STDC_UTF_32__\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stdout.contains("IEC559=1"),
        "floats are native IEEE-754, so __STDC_IEC_559__ must be defined:\n{}",
        r.stdout
    );
    assert!(
        r.stdout.contains("U16=1") && r.stdout.contains("U32=1"),
        "{}",
        r.stdout
    );
    assert!(
        !r.stdout.contains("ISO10646=__STDC_ISO_10646__"),
        "__STDC_ISO_10646__ must be defined:\n{}",
        r.stdout
    );
}

/// `__STDC_IEC_559_COMPLEX__` asserts Annex G conformance. It stays undefined,
/// but the reason has changed and is worth stating precisely.
///
/// It was undefined because complex support was broken -- `float _Complex` lost
/// its imaginary part, `long double _Complex` emitted an invalid instruction.
/// Both are fixed, and c17's complex arithmetic is now byte-identical to gcc's
/// at all three precisions. What is still missing is G.5.1p4: an infinite
/// operand must give an infinite result even when the other operand is a NaN,
/// and `CMPLX(INFINITY,0) * CMPLX(NAN,NAN)` gives NaN here.
///
/// gcc fails that same rule and defines the macro anyway, so this is a
/// deliberate divergence rather than an oversight: the macro is a claim about
/// the arithmetic, and the arithmetic does not yet support it. See
/// `c17_complex_infinity_rules_match_gcc`, which pins the behaviour so the
/// day G.5.1 is implemented, this decision gets revisited rather than
/// inherited.
#[test]
fn c17_does_not_claim_annex_g_complex() {
    let r = preprocess_text(
        "c17_no_annex_g",
        "#ifdef __STDC_IEC_559_COMPLEX__\nint claims_annex_g;\n#else\nint honest;\n#endif\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stdout.contains("honest"),
        "__STDC_IEC_559_COMPLEX__ must not be defined:\n{}",
        r.stdout
    );
}

/// The complex arithmetic behind the `__STDC_IEC_559_COMPLEX__` decision.
///
/// Two things are pinned. The parts of Annex G that c17 *does* get right, so a
/// regression in complex lowering is caught here rather than in whatever
/// numeric code notices first -- these all agree with gcc, verified
/// differentially at float, double and long double. And the one rule it gets
/// wrong, G.5.1p4, which is the whole reason the macro stays undefined.
///
/// If the G.5.1 case ever starts passing, this test fails, and that is the
/// signal to define the macro rather than let the decision go stale.
#[test]
fn c17_complex_infinity_rules_match_gcc() {
    let src = r#"
#include <complex.h>
#include <math.h>

int main(void) {
    double _Complex inf_c = CMPLX(INFINITY, 0.0);
    double _Complex nan_c = CMPLX(NAN, NAN);

    /* G.5.1: infinity over a finite value stays infinite. */
    double _Complex d = inf_c / CMPLX(2.0, 0.0);
    if (!isinf(creal(d))) return 1;

    /* ...and a finite value over an infinity is a zero. */
    double _Complex z = CMPLX(2.0, 0.0) / inf_c;
    if (creal(z) != 0.0 || cimag(z) != 0.0) return 2;

    /* G.6p2: cproj folds every infinity onto one point on the real axis,
       whichever part is infinite. */
    if (!isinf(creal(cproj(CMPLX(INFINITY, 2.0))))) return 3;
    if (!isinf(creal(cproj(CMPLX(1.0, INFINITY))))) return 4;
    if (creal(cproj(CMPLX(-INFINITY, -0.0))) <= 0.0) return 5;

    /* G.6.3.1: clog's branch cut runs along the negative real axis, and the
       sign of a zero imaginary part decides which side we are on. */
    if (cimag(clog(CMPLX(-1.0, 0.0))) <= 0.0) return 6;
    if (cimag(clog(CMPLX(-1.0, -0.0))) >= 0.0) return 7;

    /* The same rules hold at the other two precisions -- these were the
       precisions that used to be miscompiled outright (#C1/#C2). */
    float _Complex fz = CMPLXF(2.0f, 0.0f) / CMPLXF(INFINITY, 0.0f);
    if (crealf(fz) != 0.0f) return 8;
    long double _Complex lz = CMPLXL(2.0L, 0.0L) / CMPLXL(INFINITY, 0.0L);
    if (creall(lz) != 0.0L) return 9;

    /* G.5.1p4 is NOT met: an infinite operand should give an infinite result
       even against a NaN. It gives NaN, exactly as gcc does -- which is why
       __STDC_IEC_559_COMPLEX__ stays undefined. Asserted in the failing
       direction on purpose: this is the gate on that decision.  */
    double _Complex m = inf_c * nan_c;
    if (isinf(creal(m)) || isinf(cimag(m))) return 10;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c17_complex_infinity_rules", src, &["-lm".to_string()]),
        0
    );
}

/// #H12: accepting a flag that does nothing is misleading. There is no
/// freestanding environment to enter, so say so.
#[test]
fn c17_ffreestanding_is_diagnosed() {
    let dir = tempfile::Builder::new()
        .prefix("c17_freestanding_")
        .tempdir()
        .unwrap();
    let src = dir.path().join("t.c");
    std::fs::write(&src, "int main(void){return 0;}\n").unwrap();
    let exe = dir.path().join("t.out");

    let r = run_c17(&[
        "-ffreestanding",
        &src.to_string_lossy(),
        "-o",
        &exe.to_string_lossy(),
    ]);
    assert!(!r.success, "-ffreestanding must not be silently accepted");
    assert!(
        r.stderr.contains("freestanding"),
        "expected a diagnostic naming the flag:\n{}",
        r.stderr
    );
}

/// #H4: a `long double` round trip. The audit flagged aarch64's float.rs as
/// mapping `LongDouble` to a 64-bit slot while the rest of the port is
/// consistent with 128 bits, but could not verify it — that audit and this
/// development both ran on x86_64. This test runs everywhere; aarch64 CI is
/// what adjudicates the claim.
#[test]
fn c17_long_double_round_trip() {
    let src = r#"
        #include <float.h>
        static long double store;
        static long double round_trip(long double v) { store = v; return store; }
        int main(void) {
            /* A value that needs more than 53 bits of mantissa: if the slot
               collapsed to double, this comes back changed. */
            long double v = 1.0L + LDBL_EPSILON;
            if (round_trip(v) != v) return 1;
            if (v == 1.0L) return 2;   /* would mean epsilon was lost */

            /* And one that exceeds double's exponent range on targets where
               long double is wider. */
            long double big = LDBL_MAX / 2.0L;
            if (round_trip(big) != big) return 3;

            /* Through an array, so the load/store path is exercised too. */
            long double a[3];
            a[0] = v; a[1] = big; a[2] = -v;
            if (a[0] != v || a[1] != big || a[2] != -v) return 4;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c17_long_double_round_trip", src, &[]), 0);
}

/// #X8: `__STDC_NO_THREADS__` is a claim about the host, so the only thing
/// worth asserting is that the claim is true. Whether it holds varies —
/// glibc ships `<threads.h>`, macOS SDKs do not — so a test that demands one
/// answer is testing the build machine, not the compiler.
#[test]
fn c11_no_threads_macro_agrees_with_the_host() {
    let r = preprocess_text(
        "c11_threads_probe",
        "#ifdef __STDC_NO_THREADS__\nNOTHREADS\n#else\nHASTHREADS\n#endif\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    let claims_threads = r.stdout.contains("HASTHREADS");

    let src = "#include <threads.h>\nint main(void){ return 0; }\n";
    let compiles = {
        let dir = tempfile::Builder::new()
            .prefix("c11_threads_")
            .tempdir()
            .unwrap();
        let c = dir.path().join("t.c");
        std::fs::write(&c, src).unwrap();
        let asm = dir.path().join("t.s");
        run_c17(&["-S", &c.to_string_lossy(), "-o", &asm.to_string_lossy()]).success
    };

    assert_eq!(
        claims_threads,
        compiles,
        "__STDC_NO_THREADS__ is {}, but <threads.h> {} — the macro must \
         describe the host it was built for",
        if claims_threads {
            "undefined"
        } else {
            "defined"
        },
        if compiles {
            "compiles"
        } else {
            "does not compile"
        },
    );
}

// ============================================================================
// <tgmath.h> — C11 7.25 type-generic math
// ============================================================================

/// The bundled `<tgmath.h>` dispatches over float / double / long double and
/// their complex counterparts.
///
/// It is bundled rather than delegated because no glibc `<tgmath.h>` is usable
/// here: each one needs compiler internals c17 lacks (`__builtin_tgmath` for
/// GCC >= 8, `__builtin_classify_type` and `__real__` for the older path), and
/// every version `#error`s out before reaching them unless `__HAVE_FLOAT128`
/// agrees with `__HAVE_FLOAT64X` -- which is decided solely by the `__GNUC__`
/// version c17 advertises.
#[test]
fn c17_tgmath_dispatches_by_type() {
    let src = r#"
        #include <tgmath.h>

        int main(void) {
            float f = 4.0f;
            double d = 4.0;
            long double l = 4.0L;

            /* ---- real, one argument: the result keeps the argument's type,
                    which is what makes the dispatch observable ---- */
            if (sqrt(f) != 2.0f) return 1;
            if (sqrt(d) != 2.0) return 2;
            if (sqrt(l) != 2.0L) return 3;

            if (fabs(-3.0f) != 3.0f) return 4;
            if (fabs(-3.0) != 3.0) return 5;
            if (fabs(-3.0L) != 3.0L) return 6;

            /* ---- an integer argument promotes to double ---- */
            if (sqrt(16) != 4.0) return 7;
            if (ceil(1) != 1.0) return 8;

            /* ---- real, two arguments: dispatched on the combined type ---- */
            if (pow(2.0, 10.0) != 1024.0) return 10;
            if (pow(2.0f, 10.0f) != 1024.0f) return 11;
            if (pow(2.0L, 10.0L) != 1024.0L) return 12;
            if (atan2(0.0, 1.0) != 0.0) return 13;
            if (fmax(1.0f, 2.0f) != 2.0f) return 14;
            if (fmin(1.0, 2.0) != 1.0) return 15;
            if (fmod(7.0, 4.0) != 3.0) return 16;
            if (hypot(3.0, 4.0) != 5.0) return 17;

            /* ---- rounding family ---- */
            if (ceil(1.2) != 2.0) return 20;
            if (floor(1.8f) != 1.0f) return 21;
            if (trunc(-1.8) != -1.0) return 22;
            if (round(1.5) != 2.0) return 23;

            /* ---- three arguments ---- */
            if (fma(2.0, 3.0, 4.0) != 10.0) return 30;

            /* ---- an argument with a side effect must be evaluated exactly
                    once: the controlling expression of _Generic is not
                    evaluated, so `n` must end at 1 ---- */
            int n = 0;
            double side = sqrt((n++, 4.0));
            if (side != 2.0) return 40;
            if (n != 1) return 41;

            return 0;
        }
    "#;
    assert_eq!(
        compile_and_run("c17_tgmath_real", src, &["-lm".to_string()]),
        0
    );
}

#[test]
fn c17_tgmath_handles_complex() {
    let src = r#"
        #include <tgmath.h>

        int main(void) {
            double _Complex z = 1.0 + 0.0 * I;
            float _Complex zf = 1.0f + 0.0f * I;

            /* A real-or-complex macro selects the complex function. */
            double _Complex e = exp(z);
            if (creal(e) < 2.71 || creal(e) > 2.72) return 1;
            float _Complex ef = exp(zf);
            if (crealf(ef) < 2.71f || crealf(ef) > 2.72f) return 2;

            /* fabs of a complex is cabs, and yields a real. */
            if (fabs(z) != 1.0) return 3;

            /* Complex-only macros. */
            if (creal(z) != 1.0) return 4;
            if (cimag(z) != 0.0) return 5;
            if (creal(conj(z)) != 1.0) return 6;

            /* They accept a real argument too, treating it as a complex
               value with a zero imaginary part. */
            if (creal(2.0) != 2.0) return 7;
            if (cimag(2.0) != 0.0) return 8;

            return 0;
        }
    "#;
    assert_eq!(
        compile_and_run("c17_tgmath_complex", src, &["-lm".to_string()]),
        0
    );
}
