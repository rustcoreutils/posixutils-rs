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
   memory class there too. The one-float vector is x86-64 only here; every
   target's rule for it is `SMALL_DECLS`'s. */
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

/// Floating vectors of four bytes or fewer -- `v1sf`, `v2hf`, `v1hf` -- which
/// no machine register width describes, so each compiler has its own rule:
///
/// - gcc on aarch64 lays each one on the stack, in an eight-byte slot, and
///   sends every later general-register argument there too (NGRN becomes
///   8) while the V registers stay available; it returns one in W0.
/// - clang on Darwin passes one in a general register and returns it in V0.
/// - gcc on System V passes a one-lane one in memory and `v2hf` in XMM0.
///
/// Clang on x86-64 differs from gcc, so Apple x86-64 sits this out
/// (`GCC_VECTORS`); on Apple arm64 the other half is clang, which is the
/// point.
const SMALL_DECLS: &str = r#"
#if !defined(__APPLE__) || defined(C17_ALONE) || defined(__aarch64__)
#define SMALL_FLOAT_VECTORS 1
#endif
#ifdef SMALL_FLOAT_VECTORS
typedef float v1sf __attribute__((vector_size(4)));
typedef _Float16 v2hf __attribute__((vector_size(4)));
typedef _Float16 v1hf __attribute__((vector_size(2)));
v1sf sf_ret(float x);
v2hf hf2_ret(_Float16 x, _Float16 y);
v1hf hf1_ret(_Float16 x);
float sf_arg(v1sf a);
float hf2_arg(v2hf a);
float hf1_arg(v1hf a);
double sf_mix(int i, v1sf a, float f, long l, v1sf b, double d, int j);
double hf_mix(v2hf a, int i, v1hf b, float f, long l);
double sf_full(long a, long b, long c, long d, long e, long f, long g, long h,
               v1sf v, long k, float z);
double sf_many(v1sf a0, v1sf a1, v1sf a2, v1sf a3, v1sf a4, v1sf a5,
               v1sf a6, v1sf a7, v1sf a8, v1sf a9, int k, float z);
double hf_many(v2hf a0, v1hf a1, v2hf a2, v1hf a3, v2hf a4, v1hf a5,
               v2hf a6, v1hf a7, v2hf a8, v1hf a9, double d, long l);
v1sf sf_chain(v1sf a, int k, v1sf b);
v2hf hf2_chain(v2hf a, int k);
v1hf hf1_chain(int k, v1hf a);
/* A struct holding one is an ordinary composite on aarch64, and classed by
   the vector's own SSE eightbyte on System V. */
struct sa { v1sf a; float f; };
struct sb { v2hf a; int i; };
struct sc { v1hf a, b; };
struct sa gsa(struct sa x, int k);
struct sb gsb(struct sb x);
struct sc gsc(struct sc x);
#endif
"#;

const SMALL_CALLEE: &str = r#"
#ifdef SMALL_FLOAT_VECTORS
v1sf sf_ret(float x) { v1sf r = {x * 2}; return r; }
v2hf hf2_ret(_Float16 x, _Float16 y) { v2hf r = {x + 1, y * 2}; return r; }
v1hf hf1_ret(_Float16 x) { v1hf r = {x - 1}; return r; }
float sf_arg(v1sf a) { return a[0] + 1; }
float hf2_arg(v2hf a) { return (float)a[0] * 10 + (float)a[1]; }
float hf1_arg(v1hf a) { return (float)a[0] * 3; }
double sf_mix(int i, v1sf a, float f, long l, v1sf b, double d, int j)
{ return i + a[0] * 10 + f * 100 + l * 1000 + b[0] * 10000 + d * 100000 + j * 1000000.0; }
double hf_mix(v2hf a, int i, v1hf b, float f, long l)
{ return (double)a[0] + a[1] * 10 + i * 100 + b[0] * 1000 + f * 10000 + l * 100000.0; }
double sf_full(long a, long b, long c, long d, long e, long f, long g, long h,
               v1sf v, long k, float z)
{ return a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8 + v[0] * 100 + k * 1000 + z * 10000.0; }
double sf_many(v1sf a0, v1sf a1, v1sf a2, v1sf a3, v1sf a4, v1sf a5,
               v1sf a6, v1sf a7, v1sf a8, v1sf a9, int k, float z)
{
    return a0[0] + a1[0] * 2 + a2[0] * 3 + a3[0] * 4 + a4[0] * 5 + a5[0] * 6
         + a6[0] * 7 + a7[0] * 8 + a8[0] * 9 + a9[0] * 10 + k * 1000 + z * 10000.0;
}
double hf_many(v2hf a0, v1hf a1, v2hf a2, v1hf a3, v2hf a4, v1hf a5,
               v2hf a6, v1hf a7, v2hf a8, v1hf a9, double d, long l)
{
    return a0[0] + a0[1] * 2 + a1[0] * 3 + a2[1] * 4 + a3[0] * 5 + a4[0] * 6
         + a5[0] * 7 + a6[1] * 8 + a7[0] * 9 + a8[0] * 10 + a8[1] * 11
         + a9[0] * 12 + d * 1000 + l * 10000.0;
}
v1sf sf_chain(v1sf a, int k, v1sf b) { return a * b + (float)k; }
v2hf hf2_chain(v2hf a, int k) { v2hf r = {a[1] + k, a[0] - k}; return r; }
v1hf hf1_chain(int k, v1hf a) { v1hf r = {a[0] * (_Float16)k}; return r; }
struct sa gsa(struct sa x, int k) { x.a += x.f; x.f = k; return x; }
struct sb gsb(struct sb x) { x.a[0] += 1; x.i++; return x; }
struct sc gsc(struct sc x) { struct sc r = {x.b, x.a}; return r; }
#endif
"#;

const SMALL_CALLER: &str = r#"
#ifdef SMALL_FLOAT_VECTORS
/* Inlined at -O2: the inlined body reads the vector from the address the
   call passes, as the out-of-line callee reads its stacked bytes. */
static v1sf local_sf(v1sf a, long n, v1sf b) { return a * 2 + b + (float)n; }
static v2hf local_hf(int k, v2hf a) { v2hf r = {a[1] + k, a[0]}; return r; }
#endif
static int run_small(void) {
#ifdef SMALL_FLOAT_VECTORS
    v1sf s1 = {1.5f}, s2 = {2.0f}, s3 = {3.0f};
    v2hf h2 = {2, 3};
    v1hf h1 = {4};
    if (sf_ret(1.25f)[0] != 2.5f) return 1;
    v2hf r2 = hf2_ret(2, 3);
    if (r2[0] != 3 || r2[1] != 6) return 2;
    if (hf1_ret(5)[0] != 4) return 3;
    if (sf_arg(s1) != 2.5f) return 4;
    if (hf2_arg(h2) != 23) return 5;
    if (hf1_arg(h1) != 12) return 6;
    if (sf_mix(1, s2, 3.0f, 4, s3, 5.0, 6) != 6534321.0) return 7;
    if (hf_mix(h2, 4, h1, 5.0f, 6) != 654432.0) return 8;
    if (sf_full(1, 1, 1, 1, 1, 1, 1, 1, s2, 3, 4.0f) != 43236.0) return 9;
    v1sf o = {1.0f};
    if (sf_many(o, o, o, o, o, o, o, o, s2, s3, 5, 6.0f) != 65084.0) return 10;
    if (hf_many(h2, h1, h2, h1, h2, h1, h2, h1, h2, h1, 7.0, 8) != 87253.0) return 11;
    v1sf c = sf_chain(s2, 7, s3);
    if (c[0] != 13.0f) return 12;
    v2hf c2 = hf2_chain(h2, 1);
    if (c2[0] != 4 || c2[1] != 1) return 13;
    if (hf1_chain(3, h1)[0] != 12) return 14;
    struct sa ta = gsa((struct sa){{1.5f}, 2.0f}, 7);
    if (ta.a[0] != 3.5f || ta.f != 7) return 15;
    struct sb tb = gsb((struct sb){{2, 3}, 4});
    if (tb.a[0] != 3 || tb.a[1] != 3 || tb.i != 5) return 16;
    struct sc tc = gsc((struct sc){{1}, {2}});
    if (tc.a[0] != 2 || tc.b[0] != 1) return 17;
    if (local_sf(s1, 3, s2)[0] != 8.0f) return 18;
    v2hf lh = local_hf(5, h2);
    if (lh[0] != 8 || lh[1] != 2) return 19;
#endif
    return 0;
}
"#;

/// The floating vectors of four bytes or fewer (`SMALL_DECLS`) against the
/// host compiler in every pairing.
#[test]
fn vector_abi_small_float_interop_host() {
    let caller = format!("{SMALL_DECLS}{SMALL_CALLER}int main(void) {{ return run_small(); }}\n");
    interop_host(
        "vec_small_float",
        &format!("{SMALL_DECLS}{SMALL_CALLEE}"),
        &caller,
    );
}

/// The floating vectors of four bytes or fewer against aarch64 gcc in every
/// pairing, under qemu. Exit codes 1..=19 name the check in `SMALL_CALLER`.
#[test]
fn vector_abi_small_float_interop_aarch64() {
    if !aarch64_cross_available() {
        return;
    }
    interop_aarch64(
        "vec_small_float",
        &format!("{SMALL_DECLS}{SMALL_CALLEE}"),
        &format!("{SMALL_DECLS}{SMALL_CALLER}int main(void) {{ return run_small(); }}\n"),
    );
}

/// Vectors whose alignment is written -- raised by a typedef, raised or
/// lowered beside `vector_size` -- travel as their natural shape does: gcc
/// passes a vector as its main variant. A `typedef v8si w
/// __attribute__((aligned(64)))` was refused at every call boundary, and a
/// 64-byte-aligned `v8si` or a 16-byte-aligned `v1sf` started on its own
/// boundary on the stack where gcc starts it on the natural one.
const ALIGNED_DECLS: &str = r#"
typedef int v8si __attribute__((vector_size(32)));
typedef v8si w8si __attribute__((aligned(64)));
typedef int l8si __attribute__((vector_size(32), aligned(16)));
typedef int a8si __attribute__((vector_size(32), aligned(64)));
typedef a8si t8si __attribute__((aligned(128)));
typedef float v1sf __attribute__((vector_size(4)));
typedef v1sf w1sf __attribute__((aligned(16)));
typedef float a1sf __attribute__((vector_size(4), aligned(16)));
typedef int a4si __attribute__((vector_size(16), aligned(64)));
typedef short v2hi __attribute__((vector_size(4)));
typedef v2hi w2hi __attribute__((aligned(16)));
int f8(int a, v8si v, int b, v8si w);
int w8(int a, w8si v, int b, w8si w);
int l8(int a, l8si v, int b, l8si w);
int a8(long x1, long x2, long x3, long x4, long x5, long x6, int a, a8si v, int b, a8si w);
int t8(int a, t8si v, int b, t8si w);
float w1(int a, w1sf v, double d, w1sf w, int b);
float a1(int a, a1sf v, int b, a1sf w);
int a4(int a, a4si v, int b, a4si w);
int w2(int a, w2hi v, int b, w2hi w);
a8si ra8(int k);
w8si rw8(int k);
w1sf rw1(float k);
a4si ra4(int k);
w2hi rw2(short k);
"#;

const ALIGNED_CALLEE: &str = r#"
int f8(int a, v8si v, int b, v8si w) { return a + v[1] + b * 10 + w[2] * 100; }
int w8(int a, w8si v, int b, w8si w) { return a + v[1] + b * 10 + w[2] * 100; }
int l8(int a, l8si v, int b, l8si w) { return a + v[1] + b * 10 + w[2] * 100; }
int a8(long x1, long x2, long x3, long x4, long x5, long x6, int a, a8si v, int b, a8si w) {
    return a + v[1] + b * 10 + w[2] * 100 + (int)(x1 + x6);
}
int t8(int a, t8si v, int b, t8si w) { return a + v[1] + b * 10 + w[2] * 100; }
float w1(int a, w1sf v, double d, w1sf w, int b) { return a + v[0] + d + w[0] * 10 + b * 100; }
float a1(int a, a1sf v, int b, a1sf w) { return a + v[0] + b * 10 + w[0] * 100; }
int a4(int a, a4si v, int b, a4si w) { return a + v[1] + b * 10 + w[2] * 100; }
int w2(int a, w2hi v, int b, w2hi w) { return a + v[1] + b * 10 + w[0] * 100; }
a8si ra8(int k) { a8si v = {k, 2, 3, 4, 5, 6, 7, k * 9}; return v; }
w8si rw8(int k) { w8si v = {k, 2, 3, 4, 5, 6, 7, k * 3}; return v; }
w1sf rw1(float k) { w1sf v = {k * 3}; return v; }
a4si ra4(int k) { a4si v = {k, k + 1, k + 2, k * 9}; return v; }
w2hi rw2(short k) { w2hi v = {k, (short)(k * 3)}; return v; }
"#;

const ALIGNED_CALLER: &str = r#"
int main(void) {
    v8si v = {1, 2, 3, 4}, w = {5, 6, 7, 8};
    w8si wv = {1, 2, 3, 4}, ww = {5, 6, 7, 8};
    l8si lv = {1, 2, 3, 4}, lw = {5, 6, 7, 8};
    a8si av = {1, 2, 3, 4}, aw = {5, 6, 7, 8};
    t8si tv = {1, 2, 3, 4}, tw = {5, 6, 7, 8};
    w1sf sv = {1.5f}, sw = {2.5f};
    a1sf xv = {1.5f}, xw = {2.5f};
    a4si qv = {1, 2, 3, 4}, qw = {5, 6, 7, 8};
    w2hi hv = {3, 4}, hw = {5, 6};
    if (f8(1, v, 2, w) != 723) return 1;
    if (w8(1, wv, 2, ww) != 723) return 2;
    if (l8(1, lv, 2, lw) != 723) return 3;
    if (a8(10, 0, 0, 0, 0, 20, 1, av, 2, aw) != 753) return 4;
    if (t8(1, tv, 2, tw) != 723) return 5;
    if (w1(1, sv, 0.5, sw, 3) != 328.0f) return 6;
    if (a1(1, xv, 2, xw) != 272.5f) return 7;
    if (a4(1, qv, 2, qw) != 723) return 8;
    if (w2(1, hv, 2, hw) != 525) return 9;
    a8si r8 = ra8(3);
    if (r8[0] != 3 || r8[7] != 27) return 10;
    w8si s8 = rw8(4);
    if (s8[0] != 4 || s8[7] != 12) return 11;
    if (rw1(1.5f)[0] != 4.5f) return 12;
    a4si r4 = ra4(4);
    if (r4[1] != 5 || r4[3] != 36) return 13;
    w2hi r2 = rw2(7);
    if (r2[0] != 7 || r2[1] != 21) return 14;
    return 0;
}
"#;

/// The written-alignment vectors (`ALIGNED_DECLS`) against the host gcc in
/// every pairing. Linux only: these are gcc's rules, and clang's for the
/// wide and one-lane shapes are its own.
#[cfg(target_os = "linux")]
#[test]
fn vector_abi_aligned_interop_host() {
    interop_host(
        "vec_aligned",
        &format!("{ALIGNED_DECLS}{ALIGNED_CALLEE}"),
        &format!("{ALIGNED_DECLS}{ALIGNED_CALLER}"),
    );
}

/// The written-alignment vectors against aarch64 gcc in every pairing,
/// under qemu. Exit codes 1..=14 name the check in `ALIGNED_CALLER`.
#[test]
fn vector_abi_aligned_interop_aarch64() {
    if !aarch64_cross_available() {
        return;
    }
    interop_aarch64(
        "vec_aligned",
        &format!("{ALIGNED_DECLS}{ALIGNED_CALLEE}"),
        &format!("{ALIGNED_DECLS}{ALIGNED_CALLER}"),
    );
}

/// A floating vector of four bytes or fewer through `...` on aarch64 Linux.
///
/// gcc contradicts itself here: its caller lays the vector on the stack and
/// moves NGRN to 8, as for a named one, while its `va_arg` reads the vector
/// out of the general-register save area. c17's `va_arg` follows gcc's
/// caller, so a gcc caller and c17 on both sides agree, and the one pairing
/// left out -- a c17 caller of a gcc `va_arg` -- fails between two gcc units
/// as well.
#[test]
fn vector_abi_small_float_variadic_aarch64() {
    if !aarch64_cross_available() {
        return;
    }
    const VA_CALLEE: &str = r#"
#include <stdarg.h>
typedef float v1sf __attribute__((vector_size(4)));
typedef _Float16 v2hf __attribute__((vector_size(4)));
typedef _Float16 v1hf __attribute__((vector_size(2)));
double va_small(int n, ...) {
    va_list ap;
    va_start(ap, n);
    double s = 0;
    for (int i = 0; i < n; i++) {
        v1sf a = va_arg(ap, v1sf);
        long l = va_arg(ap, long);
        v2hf b = va_arg(ap, v2hf);
        double d = va_arg(ap, double);
        v1hf c = va_arg(ap, v1hf);
        int k = va_arg(ap, int);
        s = s * 10 + a[0] + l * 2 + b[0] * 3 + b[1] * 4 + d * 5 + c[0] * 6 + k * 7;
    }
    va_end(ap);
    return s;
}
"#;
    const VA_CALLER: &str = r#"
typedef float v1sf __attribute__((vector_size(4)));
typedef _Float16 v2hf __attribute__((vector_size(4)));
typedef _Float16 v1hf __attribute__((vector_size(2)));
double va_small(int n, ...);
int main(void) {
    v1sf a = {1.0f};
    v2hf b = {2, 3};
    v1hf c = {4};
    /* 1 + 2 + 6 + 12 + 5 + 24 + 7 = 57; then 570 + 57 */
    if (va_small(1, a, 1L, b, 1.0, c, 1) != 57.0) return 1;
    if (va_small(2, a, 1L, b, 1.0, c, 1, a, 1L, b, 1.0, c, 1) != 627.0) return 2;
    return 0;
}
"#;
    let dir = plib::tmp::Builder::new()
        .prefix("vec_small_va_")
        .tempdir()
        .unwrap();
    let callee_c = crate::common::create_c_file("vec_small_va_callee", VA_CALLEE);
    let caller_c = crate::common::create_c_file("vec_small_va_caller", VA_CALLER);
    let callee_src = callee_c.path().to_string_lossy().into_owned();
    let caller_src = caller_c.path().to_string_lossy().into_owned();
    for opt in ["-O0", "-O2"] {
        let asm = |src: &str, n: &str| {
            let s = dir.path().join(format!("{n}{opt}.s"));
            let mut args = crate::common::AARCH64_TARGET_ARGS.to_vec();
            args.extend_from_slice(&[opt, "-w", "-S", "-o", s.to_str().unwrap(), src]);
            let run = crate::common::run_c17(&args);
            assert!(run.success, "c17 failed on {n}:\n{}", run.stderr);
            s.to_string_lossy().into_owned()
        };
        let callee_s = asm(&callee_src, "callee");
        let caller_s = asm(&caller_src, "caller");
        assert_eq!(
            crate::common::cross_link_and_run("vec_small_va_cc", &[&caller_s, &callee_s]),
            0,
            "c17 both, {opt}"
        );
        assert_eq!(
            crate::common::cross_link_and_run("vec_small_va_gc", &[&caller_src, &callee_s]),
            0,
            "gcc caller, c17 callee, {opt}"
        );
    }
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
