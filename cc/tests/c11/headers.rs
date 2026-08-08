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

/// `__STDC_IEC_559_COMPLEX__` asserts Annex G conformance, which complex
/// support does not meet (see cc/audit.md). It must stay undefined rather
/// than claim something untrue.
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
