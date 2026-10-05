//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The bundled intrinsic headers -- SSE through SSE4.2 on x86-64, a core
// `arm_neon.h` on aarch64 -- checked against values gcc computed on the
// hardware. Each fixture in simd/ exits 0, or the number of its first
// failing check. They need no -m flags: every function is available at
// every level, and only the feature macros follow the flags.
//

#[cfg(target_arch = "x86_64")]
use crate::common::compile_and_run;
use crate::common::{aarch64_cross_available, compile_and_run_aarch64};

/// Run an x86-64 fixture at -O0 and -O2.
#[cfg(target_arch = "x86_64")]
fn run_x86(name: &str, src: &str) {
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(name, src, &[opt.to_string()]),
            0,
            "{name} at {opt}"
        );
    }
}

/// Also consolidates `simd_xmmintrin_brings_sse2` (check 180 of the
/// fixture): gcc's `<xmmintrin.h>` brings the SSE2 header with it, and mesa's
/// `half_float.h` relies on that: it includes only `<xmmintrin.h>` and
/// writes `__m128i`, beside a `"v"` asm operand of type `__m128`. The
/// fixture includes only `<xmmintrin.h>`, so that property holds.
#[cfg(target_arch = "x86_64")]
#[test]
fn simd_mmintrin_xmmintrin() {
    run_x86("simd_sse", include_str!("simd/e2e_xmmintrin.c"));
}

#[cfg(target_arch = "x86_64")]
#[test]
fn simd_emmintrin() {
    run_x86("simd_sse2", include_str!("simd/e2e_emmintrin.c"));
}

#[cfg(target_arch = "x86_64")]
#[test]
fn simd_pmmintrin_tmmintrin() {
    run_x86("simd_ssse3", include_str!("simd/e2e_tmmintrin.c"));
}

#[cfg(target_arch = "x86_64")]
#[test]
fn simd_smmintrin_nmmintrin() {
    run_x86("simd_sse4", include_str!("simd/e2e_smmintrin.c"));
}

/// The umbrella headers gather the rest, `<x86intrin.h>` adds the time-stamp
/// counter and the bit helpers, and the intrinsic headers exist on x86-64
/// alone.
#[cfg(target_arch = "x86_64")]
#[test]
fn simd_umbrella_headers() {
    let src = r#"
#include <x86intrin.h>
int main(void) {
    __m128i a = _mm_set_epi32(4, 3, 2, 1);
    __m128i b = _mm_shuffle_epi8(a, _mm_set1_epi8(0));
    __m128 f = _mm_set1_ps(2.0f);
    void *p = _mm_malloc(64, 64);
    int ok = _mm_cvtsi128_si32(b) == 0x01010101
        && _mm_cvtss_f32(_mm_sqrt_ps(_mm_mul_ps(f, f))) == 2.0f
        && _mm_popcnt_u32(0xff) == 8
        && _mm_crc32_u8(0, 1) != 0
        && ((unsigned long)p & 63) == 0
        && __rdtsc() != 0
        && _bit_scan_forward(8) == 3;
    _mm_free(p);
    return ok ? 0 : 1;
}
"#;
    run_x86("simd_umbrella", src);
}

#[test]
fn simd_arm_neon() {
    let src = include_str!("simd/e2e_arm_neon.c");
    if cfg!(target_arch = "aarch64") {
        for opt in ["-O0", "-O2"] {
            assert_eq!(
                crate::common::compile_and_run("simd_neon", src, &[opt.to_string()]),
                0,
                "simd_neon at {opt}"
            );
        }
        return;
    }
    if !aarch64_cross_available() {
        return;
    }
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("simd_neon", src, opt) {
            assert_eq!(code, 0, "simd_neon on aarch64 at {opt}");
        }
    }
}
