//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's aarch64 builtin vector types and `aarch64_vector_pcs` at run time:
// glibc's <math.h> declares its AdvSIMD functions with both, and libmvec
// defines them under the vector procedure call standard. The compile-only
// cases are in cc/test_diagnostics/aarch64_builtin_types.rs.
//

use crate::common::{
    aarch64_cross_available, compile_and_run_aarch64_with, create_c_file, cross_link_and_run,
    run_c17, AARCH64_TARGET_ARGS,
};

/// `__Float32x4_t` and `__Float64x2_t` as values: built, converted to and
/// from the GNU vector of their lanes, operated on, indexed and passed --
/// beside the SVE typedefs and declarations glibc's <math.h> makes. Exit
/// codes 1..=6.
const ADVSIMD_VALUES: &str = r#"
#include <math.h>
typedef float v4sf __attribute__((vector_size(16)));
typedef __SVFloat32_t sv_f32;
typedef __SVBool_t sv_bool;
sv_f32 sve_decl(sv_f32, sv_bool);

__attribute__((noinline)) __Float32x4_t scale(__Float32x4_t a, v4sf b) {
    return a * b + 1.0f;
}
__attribute__((noinline)) __Float64x2_t twice(__Float64x2_t d) { return d + d; }

int main(void) {
    volatile float k = 2.0f;
    __Float32x4_t a = {1.0f, 2.0f, 3.0f, 4.0f};
    v4sf b = {k, k, k, k};
    v4sf r = scale(a, b);
    if (r[0] != 3.0f || r[3] != 9.0f) return 1;
    __Float32x4_t s = r;
    if (s[1] != 5.0f) return 2;
    __Float64x2_t d = {0.5, -1.5};
    d = twice(d);
    if (d[0] != 1.0 || d[1] != -3.0) return 3;
    if (sizeof(__Float32x4_t) != 16 || _Alignof(__Float64x2_t) != 16) return 4;
    if (_Generic(s, v4sf: 1, __Float32x4_t: 0)) return 5;
    if (!isfinite(d[1]) || !signbit(d[1]) || isnan(d[0])) return 6;
    return 0;
}
"#;

/// On an aarch64 host -- macOS on Apple silicon in CI, so Darwin too -- the
/// names are there and the values right.
#[cfg(target_arch = "aarch64")]
#[test]
fn aarch64_builtin_vector_types_host() {
    for opt in ["-O0", "-O2"] {
        let code =
            crate::common::compile_and_run("advsimd_host", ADVSIMD_VALUES, &[opt.to_string()]);
        assert_eq!(code, 0, "{opt}");
    }
}

/// The same for linux-aarch64 under qemu, against the target's glibc
/// headers.
#[test]
fn aarch64_builtin_vector_types_linux() {
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64_with("advsimd_a64", ADVSIMD_VALUES, &[opt], &[])
        {
            assert_eq!(code, 0, "{opt}");
        }
    }
}

/// A call to libmvec's `_ZGVnN4v_sinf` and `_ZGVnN2v_exp`, which glibc
/// defines under the vector procedure call standard and declares as here.
/// c17 calls them with the base convention, which keeps less live across
/// the call than the vector one lets it, and gets libmvec's answers. Exit
/// codes 1..=4.
#[test]
fn aarch64_vector_pcs_call_into_libmvec() {
    let src = r#"
#include <math.h>
#define __vpcs __attribute__ ((__aarch64_vector_pcs__))
__vpcs __Float32x4_t _ZGVnN4v_sinf (__Float32x4_t);
__vpcs __Float64x2_t _ZGVnN2v_exp (__Float64x2_t);
volatile float x0 = 0.5f;
int main(void) {
    __Float32x4_t x = {x0, x0 * 2, x0 * 3, x0 * 4};
    __Float32x4_t keep = x * 3.0f;
    __Float32x4_t s = _ZGVnN4v_sinf(x);
    for (int i = 0; i < 4; i++)
        if (fabsf(s[i] - sinf(x[i])) > 1e-6f) return 1 + i / 2;
    __Float64x2_t d = {1.0, -2.0};
    __Float64x2_t e = _ZGVnN2v_exp(d);
    if (fabs(e[0] - exp(1.0)) > 1e-12 || fabs(e[1] - exp(-2.0)) > 1e-12) return 3;
    if (keep[3] != 6.0f) return 4;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        if let Some(code) =
            compile_and_run_aarch64_with("vpcs_libmvec", src, &[opt], &["-lmvec", "-lm"])
        {
            assert_eq!(code, 0, "{opt}");
        }
    }
}

/// A gcc-compiled `aarch64_vector_pcs` function that changes every register
/// the vector convention lets it -- v0-v7 and v24-v31 among the SIMD ones,
/// and the caller-saved general registers -- called by c17 through a
/// declaration and through a pointer, with values live across each call.
#[test]
fn aarch64_vector_pcs_call_into_gcc() {
    if !aarch64_cross_available() {
        eprintln!("SKIP aarch64_vector_pcs_call_into_gcc: no aarch64 cross toolchain");
        return;
    }
    let callee = r#"
typedef float v4sf __attribute__((vector_size(16)));
__attribute__((aarch64_vector_pcs)) v4sf vf(v4sf a, double d) {
    __asm__ volatile(
        "movi v1.16b, #0x55\n movi v2.16b, #0x55\n movi v3.16b, #0x55\n"
        "movi v4.16b, #0x55\n movi v5.16b, #0x55\n movi v6.16b, #0x55\n"
        "movi v7.16b, #0x55\n movi v24.16b, #0x55\n movi v25.16b, #0x55\n"
        "movi v26.16b, #0x55\n movi v27.16b, #0x55\n movi v28.16b, #0x55\n"
        "movi v29.16b, #0x55\n movi v30.16b, #0x55\n movi v31.16b, #0x55\n"
        "mov x9, #0x55\n mov x10, #0x55\n mov x11, #0x55\n mov x12, #0x55\n"
        "mov x13, #0x55\n mov x14, #0x55\n mov x15, #0x55\n"
        ::: "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v24", "v25", "v26",
            "v27", "v28", "v29", "v30", "v31", "x9", "x10", "x11", "x12",
            "x13", "x14", "x15");
    return a * (float)d;
}
"#;
    let caller = r#"
typedef float v4sf __attribute__((vector_size(16)));
__attribute__((aarch64_vector_pcs)) v4sf vf(v4sf, double);
v4sf (__attribute__((aarch64_vector_pcs)) *volatile fp)(v4sf, double) = vf;
volatile float one = 1.0f;
int main(void) {
    v4sf a = {one, 2, 3, 4};
    double live_d = one * 7.0;
    long live_x = (long)one + 40;
    v4sf r = vf(a, 2.0);
    v4sf q = fp(r, 0.5);
    for (int i = 0; i < 4; i++) {
        if (r[i] != a[i] * 2) return 1;
        if (q[i] != a[i]) return 2;
    }
    if (live_d != 7.0 || live_x != 41) return 3;
    return 0;
}
"#;
    let callee_c = create_c_file("vpcs_gcc_callee", callee);
    let caller_c = create_c_file("vpcs_c17_caller", caller);
    let dir = crate::common::work_dir("vpcs_gcc");
    for opt in ["-O0", "-O2"] {
        let s = dir.path().join(format!("caller{opt}.s"));
        let mut args = AARCH64_TARGET_ARGS.to_vec();
        args.extend_from_slice(&[
            opt,
            "-S",
            "-o",
            s.to_str().unwrap(),
            caller_c.path().to_str().unwrap(),
        ]);
        let run = run_c17(&args);
        assert!(run.success, "{}", run.stderr);
        let code = cross_link_and_run(
            "vpcs_gcc",
            &[s.to_str().unwrap(), callee_c.path().to_str().unwrap()],
        );
        assert_eq!(code, 0, "{opt}");
    }
}
