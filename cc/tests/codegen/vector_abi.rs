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
// The shapes outside the machine's own widths -- wider than sixteen bytes, or
// one lane -- are gcc's conventions alone; clang has its own, so on Apple
// they are checked with c17 on both sides rather than against the host
// compiler. `GCC_VECTORS` below marks them.
//
// The assembly and diagnostic checks compile in process, in
// `cc/test_asm/codegen_vector_abi.rs`.
//

use crate::common::{
    aarch64_cross_available, compile_and_run_two_units, interop_aarch64, interop_host,
};

const DECLS: &str = r#"
#include <stdarg.h>
/* Whether this source may use a vector whose convention gcc and clang
   disagree about, which is every vector that is not one of the machine's own
   widths. A vector wider than sixteen bytes has no register class without
   AVX, so gcc gives it memory and a hidden return pointer while clang
   legalizes it into a pair of SSE registers; a one-lane vector is memory to
   gcc and the bare scalar to clang. c17 follows gcc throughout, so on Apple
   -- the one host where the other half of an interop pair is clang -- these
   shapes stay out of the cross-check and run only in the pairings c17
   compiles alone, which define `C17_ALONE`. `cc/DECISIONS.md` records it. */
#if !defined(__APPLE__) || defined(C17_ALONE)
#define GCC_VECTORS 1
#endif
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
#ifdef GCC_VECTORS
v8si f32(v8si a, v8si b);
v4df f32d(v4df a, int k);
#endif
v2df fd(v2df a, double k);
v2sf fs(int n, v2sf a, double d, v2si b);
v4sf many(v4sf a, v4sf b, v4sf c, v4sf d, v4sf e, v4sf f, v4sf g, v4sf h, v4sf i, v4sf j);
#ifdef GCC_VECTORS
int vsum(int n, ...);
#endif
struct sv1 gs1(struct sv1 x, int k);
struct sv2 gs2(struct sv2 x);
struct sv3 gs3(struct sv3 x, float k);
struct sv4 gs4(struct sv4 x);
struct sv5 gs5(struct sv5 x);
/* One lane of `double`, `long long` or `float`: gcc passes these in memory on
   System V, and a struct holding one small enough to fit a register is
   memory class there too. The aarch64 one-float vector is a gcc quirk c17
   does not reproduce, so it is x86-64 only. */
typedef double v1df __attribute__((vector_size(8)));
typedef long long v1di __attribute__((vector_size(8)));
struct s1df { v1df a; int i; };
struct s1df gs1df(struct s1df x);
#ifdef GCC_VECTORS
v1df g1df(v1df a, double k);
v1di g1di(v1di a);
#endif
#ifdef __x86_64__
typedef float v1sf __attribute__((vector_size(4)));
#ifdef GCC_VECTORS
v1sf g1sf(v1sf a, float k);
struct s1sf { v1sf a; float f; };
struct s1sf gs1sf(struct s1sf x);
#endif
#endif
"#;

const CALLEE: &str = r#"
v4si f16(v4si a, v4si b) { return a + b; }
v2si f8(v2si a, v2si b) { return a * b; }
v2hi f4(v2hi a, v2hi b) { return a - b; }
v2qi f2(v2qi a, v2qi b) { return a + b; }
#ifdef GCC_VECTORS
v8si f32(v8si a, v8si b) { return a + b; }
v4df f32d(v4df a, int k) { return a * k; }
#endif
v2df fd(v2df a, double k) { return a * k; }
v2sf fs(int n, v2sf a, double d, v2si b) { return a + (float)(n + d + b[1]); }
v4sf many(v4sf a, v4sf b, v4sf c, v4sf d, v4sf e, v4sf f, v4sf g, v4sf h, v4sf i, v4sf j)
{ return a + b + c + d + e + f + g + h + i * 2 + j * 3; }
#ifdef GCC_VECTORS
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
#endif
struct sv1 gs1(struct sv1 x, int k) { x.a += k; return x; }
struct sv2 gs2(struct sv2 x) { x.a *= x.b; x.b++; return x; }
struct sv3 gs3(struct sv3 x, float k) { x.a = x.a * k + x.b; return x; }
struct sv4 gs4(struct sv4 x) { x.a += x.c; x.b -= x.c; return x; }
struct sv5 gs5(struct sv5 x) { x.a += (float)x.d; x.d *= 2; return x; }
struct s1df gs1df(struct s1df x) { x.a += x.i; x.i++; return x; }
#ifdef GCC_VECTORS
v1df g1df(v1df a, double k) { return a * k; }
v1di g1di(v1di a) { return a + 1; }
#endif
#if defined(__x86_64__) && defined(GCC_VECTORS)
v1sf g1sf(v1sf a, float k) { return a + k; }
struct s1sf gs1sf(struct s1sf x) { x.a *= x.f; x.f += 1; return x; }
#endif
"#;

const CALLER: &str = r#"
static int run_vectors(void) {
    v4si a = {1, 2, 3, 4};
    v2si p = {3, 4};
    v2hi h = {10, 20}, h1 = {1, 2};
    v2qi c = {5, 6};
#ifdef GCC_VECTORS
    v8si w = {1, 2, 3, 4, 5, 6, 7, 8};
    v4df wd = {1, 2, 3, 4};
#endif
    v2df d2 = {1.5, 2.5};
    v2sf s2 = {0.5f, 1.5f};
    v4sf o = {1, 1, 1, 1}, ramp = {1, 2, 3, 4};
    v4si r = f16(a, a);
    if (r[3] != 8) return 1;
    if (f8(p, p)[1] != 16) return 2;
    if (f4(h, h1)[1] != 18) return 3;
    if (f2(c, c)[1] != 12) return 4;
#ifdef GCC_VECTORS
    v8si x = f32(w, w);
    if (x[0] != 2 || x[7] != 16) return 5;
    if (f32d(wd, 3)[3] != 12) return 6;
#endif
    if (fd(d2, 2.0)[1] != 5) return 7;
    v2sf fs2 = fs(1, s2, 2.0, p);
    if (fs2[0] != 7.5f || fs2[1] != 8.5f) return 8;
    v4sf m = many(o, o, o, o, o, o, o, o, o, ramp);
    if (m[0] != 13 || m[3] != 22) return 9;
#ifdef GCC_VECTORS
    if (vsum(2, a, p, w, r, f8(p, p), x) != 56) return 10;
#endif
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
    struct s1df t6 = gs1df((struct s1df){{2.5}, 3});
    if (t6.a[0] != 5.5 || t6.i != 4) return 18;
#ifdef GCC_VECTORS
    v1df one = {1.5};
    if (g1df(one, 4.0)[0] != 6.0) return 16;
    v1di onei = {41};
    if (g1di(onei)[0] != 42) return 17;
#endif
#if defined(__x86_64__) && defined(GCC_VECTORS)
    v1sf onef = {1.5f};
    if (g1sf(onef, 2.0f)[0] != 3.5f) return 19;
    struct s1sf t7 = gs1sf((struct s1sf){{1.5f}, 2.0f});
    if (t7.a[0] != 3.0f || t7.f != 3.0f) return 20;
#endif
    return 0;
}
"#;

const SHIFT_DECLS: &str = r#"
typedef int v4si __attribute__((vector_size(16)));
typedef unsigned v4su __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));
v4si shl(v4si v, int n);
v4su lsr(v4su v, int n);
v4si asr(v4si v, int n);
v8hi shlw(v8hi v, int n);
"#;

const SHIFT_CALLEE: &str = r#"
v4si shl(v4si v, int n) { return v << n; }
v4su lsr(v4su v, int n) { return v >> n; }
v4si asr(v4si v, int n) { return v >> n; }
v8hi shlw(v8hi v, int n) { return v << n; }
"#;

/// A vector shifted by a scalar count reads the count at its own width.
/// SSE's shifts take the whole low quadword of the count register, and the
/// System V convention leaves the bits above an `int` argument undefined:
/// the count was moved 64 bits wide, so a caller's garbage there shifted
/// every lane out. The caller here passes each count as a `long` with
/// garbage above the 32 bits the callee's `int` reads.
///
/// The caller passes each count as a `long` with garbage above the 32 bits
/// the callee's `int` reads. Run as part of `vector_abi_interop_host`, exit
/// codes 31..=34.
const SHIFT_CALLER: &str = r#"
static int run_shift(void) {
    v4si (*fl)(v4si, long) = (v4si (*)(v4si, long))shl;
    v4su (*fr)(v4su, long) = (v4su (*)(v4su, long))lsr;
    v4si (*fa)(v4si, long) = (v4si (*)(v4si, long))asr;
    v8hi (*fw)(v8hi, long) = (v8hi (*)(v8hi, long))shlw;
    v4si l = fl((v4si){1, 2, 3, 4}, 0x500000002L);
    if (l[0] != 4 || l[3] != 16) return 1;
    v4su r = fr((v4su){16, 32, 64, 0x80000000u}, 0x500000002L);
    if (r[0] != 4 || r[3] != 0x20000000u) return 2;
    v4si a = fa((v4si){-16, 32, -64, 128}, 0x500000002L);
    if (a[0] != -4 || a[2] != -16) return 3;
    v8hi w = fw((v8hi){1, 2, 3, 4, 5, 6, 7, 8}, 0x500000002L);
    if (w[0] != 4 || w[7] != 32) return 4;
    return 0;
}
"#;

/// The vector shapes against gcc in every pairing, and with them the shift
/// count check of `vector_abi_shift_count_reads_its_own_width` (consolidated
/// here; see `SHIFT_CALLER`). Exit codes: the vector checks 1..=20, the shift
/// counts 31..=34.
#[test]
fn vector_abi_interop_host() {
    let callee = format!("{DECLS}{CALLEE}{SHIFT_DECLS}{SHIFT_CALLEE}");
    let caller = format!(
        "{DECLS}{CALLER}{SHIFT_DECLS}{SHIFT_CALLER}\
         int main(void) {{\n\
             int r;\n\
             if ((r = run_vectors()) != 0) return r;\n\
             if ((r = run_shift()) != 0) return 30 + r;\n\
             return 0;\n\
         }}\n"
    );
    interop_host("vec_abi", &callee, &caller);
    // The shapes gcc and clang lower differently sat out the pairings above
    // on Apple, where the host compiler is clang; c17 on both sides still
    // carries them, so their lowering stays covered there. See `DECLS`.
    if cfg!(target_os = "macos") {
        for opt in ["-O0", "-O2"] {
            assert_eq!(
                compile_and_run_two_units(
                    "vec_abi_gcc_shapes",
                    &callee,
                    &caller,
                    &["-w".to_string(), "-DC17_ALONE".to_string(), opt.to_string()],
                ),
                0,
                "c17 both with gcc's own vector shapes, {opt}"
            );
        }
    }
}

#[test]
fn vector_abi_interop_aarch64() {
    if !aarch64_cross_available() {
        return;
    }
    interop_aarch64(
        "vec_abi",
        &format!("{DECLS}{CALLEE}"),
        &format!("{DECLS}{CALLER}int main(void) {{ return run_vectors(); }}\n"),
    );
}

/// Win64 passes a sixteen-byte vector by reference and returns it in XMM0,
/// and an eight-byte one in a general register.
///
/// Only the sixteen-byte half is cross-checked on Apple: clang passes an
/// eight-byte vector under `ms_abi` by reference too, and returns it the same
/// way, where gcc -- the convention c17 follows -- uses a general register.
/// `cc/DECISIONS.md` records it. `C17_ALONE` brings that half back for the
/// run c17 compiles on both sides.
#[cfg(target_arch = "x86_64")]
#[test]
fn vector_abi_interop_ms_abi() {
    const MS_DECLS: &str = r#"
typedef int v4si __attribute__((vector_size(16)));
typedef int v2si __attribute__((vector_size(8)));
#if !defined(__APPLE__) || defined(C17_ALONE)
#define EIGHT_BYTE_VECTOR 1
#endif
#define MS __attribute__((ms_abi))
MS v4si ms16(v4si a, int c);
#ifdef EIGHT_BYTE_VECTOR
MS v4si ms16_8(v4si a, v2si b, int c);
#endif
"#;
    const MS_CALLEE: &str = r#"
MS v4si ms16(v4si a, int c) { return a + c; }
#ifdef EIGHT_BYTE_VECTOR
MS v4si ms16_8(v4si a, v2si b, int c) { return a + b[1] + c; }
#endif
"#;
    const MS_CALLER: &str = r#"
int main(void) {
    v4si a = {1, 2, 3, 4};
    v4si m = ms16(a, 10);
    if (m[0] != 11 || m[3] != 14) return 1;
#ifdef EIGHT_BYTE_VECTOR
    v2si p = {3, 4};
    v4si n = ms16_8(a, p, 10);
    if (n[0] != 15 || n[3] != 18) return 2;
#endif
    return 0;
}
"#;
    let callee = format!("{MS_DECLS}{MS_CALLEE}");
    let caller = format!("{MS_DECLS}{MS_CALLER}");
    interop_host("vec_ms_abi", &callee, &caller);
    if cfg!(target_os = "macos") {
        for opt in ["-O0", "-O2"] {
            assert_eq!(
                compile_and_run_two_units(
                    "vec_ms_abi_8",
                    &callee,
                    &caller,
                    &["-w".to_string(), "-DC17_ALONE".to_string(), opt.to_string()],
                ),
                0,
                "c17 both with the eight-byte vector, {opt}"
            );
        }
    }
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
