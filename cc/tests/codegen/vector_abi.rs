//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU vectors at call boundaries, checked against gcc in every pairing: as
// arguments, parameters and return values, through `...`, inside structs,
// and under `ms_abi`. gcc's conventions: sixteen bytes in one SSE (V)
// register, eight in one too, four bytes or fewer of integer lanes in a
// general register, more than sixteen in memory or by reference; a struct
// holding a vector classified by it.
//

use super::asm_probe::{asm_for, body_of, AARCH64_DARWIN, AARCH64_LINUX};
use crate::common::{aarch64_cross_available, interop_aarch64, interop_host};

const DECLS: &str = r#"
#include <stdarg.h>
typedef int v4si __attribute__((vector_size(16)));
typedef int v2si __attribute__((vector_size(8)));
typedef short v2hi __attribute__((vector_size(4)));
typedef char v2qi __attribute__((vector_size(2)));
typedef int v8si __attribute__((vector_size(32)));
typedef double v2df __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));
typedef float v2sf __attribute__((vector_size(8)));
typedef double v4df __attribute__((vector_size(32)));
struct sv1 { v4si a; };
struct sv2 { v2si a; int b; };
struct sv3 { v4sf a, b; };
struct sv4 { v2sf a, b, c; };
struct sv5 { v4sf a; double d; };
v4si f16(v4si a, v4si b);
v2si f8(v2si a, v2si b);
v2hi f4(v2hi a, v2hi b);
v2qi f2(v2qi a, v2qi b);
v8si f32(v8si a, v8si b);
v4df f32d(v4df a, int k);
v2df fd(v2df a, double k);
v2sf fs(int n, v2sf a, double d, v2si b);
v4sf many(v4sf a, v4sf b, v4sf c, v4sf d, v4sf e, v4sf f, v4sf g, v4sf h, v4sf i, v4sf j);
int vsum(int n, ...);
struct sv1 gs1(struct sv1 x, int k);
struct sv2 gs2(struct sv2 x);
struct sv3 gs3(struct sv3 x, float k);
struct sv4 gs4(struct sv4 x);
struct sv5 gs5(struct sv5 x);
/* One lane of `double` or `float`: gcc passes these in memory on System V,
   and a struct holding one is memory class there too. The aarch64 one-float
   vector is a gcc quirk c17 does not reproduce, so it is x86-64 only. */
typedef double v1df __attribute__((vector_size(8)));
typedef long long v1di __attribute__((vector_size(8)));
v1df g1df(v1df a, double k);
v1di g1di(v1di a);
struct s1df { v1df a; int i; };
struct s1df gs1df(struct s1df x);
#ifdef __x86_64__
typedef float v1sf __attribute__((vector_size(4)));
v1sf g1sf(v1sf a, float k);
struct s1sf { v1sf a; float f; };
struct s1sf gs1sf(struct s1sf x);
#endif
"#;

const CALLEE: &str = r#"
v4si f16(v4si a, v4si b) { return a + b; }
v2si f8(v2si a, v2si b) { return a * b; }
v2hi f4(v2hi a, v2hi b) { return a - b; }
v2qi f2(v2qi a, v2qi b) { return a + b; }
v8si f32(v8si a, v8si b) { return a + b; }
v4df f32d(v4df a, int k) { return a * k; }
v2df fd(v2df a, double k) { return a * k; }
v2sf fs(int n, v2sf a, double d, v2si b) { return a + (float)(n + d + b[1]); }
v4sf many(v4sf a, v4sf b, v4sf c, v4sf d, v4sf e, v4sf f, v4sf g, v4sf h, v4sf i, v4sf j)
{ return a + b + c + d + e + f + g + h + i * 2 + j * 3; }
int vsum(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int s = 0;
    for (int i = 0; i < n; i++) {
        v4si v = va_arg(ap, v4si); v2si w = va_arg(ap, v2si); v8si x = va_arg(ap, v8si);
        s += v[3] + w[1] + x[7];
    }
    va_end(ap);
    return s;
}
struct sv1 gs1(struct sv1 x, int k) { x.a += k; return x; }
struct sv2 gs2(struct sv2 x) { x.a *= x.b; x.b++; return x; }
struct sv3 gs3(struct sv3 x, float k) { x.a = x.a * k + x.b; return x; }
struct sv4 gs4(struct sv4 x) { x.a += x.c; x.b -= x.c; return x; }
struct sv5 gs5(struct sv5 x) { x.a += (float)x.d; x.d *= 2; return x; }
v1df g1df(v1df a, double k) { return a * k; }
v1di g1di(v1di a) { return a + 1; }
struct s1df gs1df(struct s1df x) { x.a += x.i; x.i++; return x; }
#ifdef __x86_64__
v1sf g1sf(v1sf a, float k) { return a + k; }
struct s1sf gs1sf(struct s1sf x) { x.a *= x.f; x.f += 1; return x; }
#endif
"#;

const CALLER: &str = r#"
int main(void) {
    v4si a = {1, 2, 3, 4};
    v2si p = {3, 4};
    v2hi h = {10, 20}, h1 = {1, 2};
    v2qi c = {5, 6};
    v8si w = {1, 2, 3, 4, 5, 6, 7, 8};
    v4df wd = {1, 2, 3, 4};
    v2df d2 = {1.5, 2.5};
    v2sf s2 = {0.5f, 1.5f};
    v4sf o = {1, 1, 1, 1}, ramp = {1, 2, 3, 4};
    v4si r = f16(a, a);
    if (r[3] != 8) return 1;
    if (f8(p, p)[1] != 16) return 2;
    if (f4(h, h1)[1] != 18) return 3;
    if (f2(c, c)[1] != 12) return 4;
    v8si x = f32(w, w);
    if (x[0] != 2 || x[7] != 16) return 5;
    if (f32d(wd, 3)[3] != 12) return 6;
    if (fd(d2, 2.0)[1] != 5) return 7;
    v2sf fs2 = fs(1, s2, 2.0, p);
    if (fs2[0] != 7.5f || fs2[1] != 8.5f) return 8;
    v4sf m = many(o, o, o, o, o, o, o, o, o, ramp);
    if (m[0] != 13 || m[3] != 22) return 9;
    if (vsum(2, a, p, w, r, f8(p, p), x) != 56) return 10;
    struct sv1 t1 = gs1((struct sv1){{1, 2, 3, 4}}, 5);
    if (t1.a[3] != 9) return 11;
    struct sv2 t2 = gs2((struct sv2){{3, 4}, 2});
    if (t2.a[1] != 8 || t2.b != 3) return 12;
    struct sv3 t3 = gs3((struct sv3){{1, 2, 3, 4}, {1, 1, 1, 1}}, 2.0f);
    if (t3.a[3] != 9 || t3.b[0] != 1) return 13;
    struct sv4 t4 = gs4((struct sv4){{1, 2}, {3, 4}, {5, 6}});
    if (t4.a[1] != 8 || t4.b[0] != -2) return 14;
    struct sv5 t5 = gs5((struct sv5){{1, 2, 3, 4}, 1.5});
    if (t5.a[0] != 2.5f || t5.a[3] != 5.5f || t5.d != 3) return 15;
    v1df one = {1.5};
    if (g1df(one, 4.0)[0] != 6.0) return 16;
    v1di onei = {41};
    if (g1di(onei)[0] != 42) return 17;
    struct s1df t6 = gs1df((struct s1df){{2.5}, 3});
    if (t6.a[0] != 5.5 || t6.i != 4) return 18;
#ifdef __x86_64__
    v1sf onef = {1.5f};
    if (g1sf(onef, 2.0f)[0] != 3.5f) return 19;
    struct s1sf t7 = gs1sf((struct s1sf){{1.5f}, 2.0f});
    if (t7.a[0] != 3.0f || t7.f != 3.0f) return 20;
#endif
    return 0;
}
"#;

#[test]
fn vector_abi_interop_host() {
    interop_host(
        "vec_abi",
        &format!("{DECLS}{CALLEE}"),
        &format!("{DECLS}{CALLER}"),
    );
}

#[test]
fn vector_abi_interop_aarch64() {
    if !aarch64_cross_available() {
        return;
    }
    interop_aarch64(
        "vec_abi",
        &format!("{DECLS}{CALLEE}"),
        &format!("{DECLS}{CALLER}"),
    );
}

/// Win64 passes a sixteen-byte vector by reference and returns it in XMM0,
/// and an eight-byte one in a general register.
#[cfg(target_arch = "x86_64")]
#[test]
fn vector_abi_interop_ms_abi() {
    let decls = "typedef int v4si __attribute__((vector_size(16)));\n\
                 typedef int v2si __attribute__((vector_size(8)));\n\
                 __attribute__((ms_abi)) v4si ms16(v4si a, v2si b, int c);\n";
    let callee = format!("{decls}__attribute__((ms_abi)) v4si ms16(v4si a, v2si b, int c) {{ return a + b[1] + c; }}\n");
    let caller = format!(
        "{decls}int main(void) {{ v4si a = {{1, 2, 3, 4}}; v2si p = {{3, 4}}; \
         v4si m = ms16(a, p, 10); return (m[0] == 15 && m[3] == 18) ? 0 : 1; }}\n"
    );
    interop_host("vec_ms_abi", &callee, &caller);
}

/// gcc's aarch64 passes a one-float vector on the stack along with the
/// arguments after it, and returns it in a general register -- like no type
/// c17 has. c17 refuses it there, and passes it in memory on System V as gcc
/// does.
#[test]
fn vector_abi_small_float_vector_is_refused_on_aarch64() {
    let src = "typedef float v1sf __attribute__((vector_size(4)));\nv1sf f(v1sf a) { return a; }\n";
    let c = crate::common::create_c_file("vec_abi_v1sf", src);
    let path = c.path().to_string_lossy().into_owned();
    let a64 = crate::common::run_c17(&[
        "--target",
        "aarch64-unknown-linux-gnu",
        "-S",
        "-o",
        "/dev/null",
        &path,
    ]);
    assert!(!a64.success, "aarch64 accepted it");
    assert!(
        a64.stderr
            .contains("c17 does not pass or return this vector type on this target"),
        "{}",
        a64.stderr
    );
    let x86 = crate::common::run_c17(&[
        "--target",
        "x86_64-unknown-linux-gnu",
        "-S",
        "-o",
        "/dev/null",
        &path,
    ]);
    assert!(x86.success, "{}", x86.stderr);
}

/// On x86-64 gcc lays a vector out on a boundary of its own size, while
/// `_Alignof` answers at most sixteen; aarch64 caps both at sixteen.
#[test]
fn vector_alignment_matches_gcc() {
    let src = r#"
typedef int v8si __attribute__((vector_size(32)));
typedef int v32si __attribute__((vector_size(128)));
struct s8 { char c; v8si v; };
struct s32 { char c; v32si v; };
int main(void) {
#ifdef __x86_64__
    if (__builtin_offsetof(struct s8, v) != 32 || sizeof(struct s32) != 256) return 1;
#else
    if (__builtin_offsetof(struct s8, v) != 16 || sizeof(struct s32) != 144) return 1;
#endif
    if (_Alignof(v8si) != 16 || _Alignof(v32si) != 16) return 2;
    return 0;
}
"#;
    crate::common::compile_and_run_everywhere("vec_align", src);
}

/// clang -- Darwin's compiler -- returns an integer vector of four bytes or
/// fewer in V0: one lane in its low bits, several widened to fill D0
/// (`v2hi` as two 32-bit lanes, `v4qi` as four 16-bit ones). It passes one
/// in a general register, as gcc does on Linux, where the return stays in
/// W0. Read off `llc -mtriple=arm64-apple-macos` for clang's lowering; a
/// clang caller of a c17 callee returning one in W0 read garbage.
#[test]
fn vector_abi_darwin_returns_small_integer_vectors_in_v0() {
    let src = r#"
typedef short v2hi __attribute__((vector_size(4)));
typedef unsigned char v4qi __attribute__((vector_size(4)));
typedef int v1si __attribute__((vector_size(4)));
v2hi r2(v2hi a, v2hi b) { return a - b; }
v4qi r4(v4qi a) { return a + a; }
v1si r1(v1si a) { return a + 1; }
v2hi ext(v2hi);
int c2(v2hi a) { return ext(a)[1]; }
"#;
    for triple in [AARCH64_DARWIN, AARCH64_LINUX] {
        let asm = asm_for("vec_small_ret", triple, src);
        let darwin = triple == AARCH64_DARWIN;
        for f in ["r2", "r4", "r1"] {
            let body = body_of(&asm, f);
            assert_eq!(mentions_v0(body), darwin, "{triple} {f}:\n{body}");
        }
        // The caller takes the result from V0 on Darwin, W0 on Linux.
        let body = body_of(&asm, "c2");
        let after_call = body.split_once("bl").map(|(_, rest)| rest).unwrap_or("");
        assert_eq!(mentions_v0(after_call), darwin, "{triple} c2:\n{body}");
    }
}

/// Whether `asm` names V0 at any width: `v0`, `d0`, `s0`, `h0` or `b0`.
fn mentions_v0(asm: &str) -> bool {
    asm.split(|c: char| !c.is_ascii_alphanumeric())
        .any(|t| matches!(t, "v0" | "d0" | "s0" | "h0" | "b0"))
}
