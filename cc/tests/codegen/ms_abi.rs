//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((ms_abi))`: the Microsoft x64 calling convention on x86-64
//

use crate::common::{
    aarch64_cross_available, asm_for_at, compile_and_run, compile_expect_error,
    compile_expect_warning, compile_expect_warning_with, interop_aarch64, interop_host,
    AARCH64_TARGET_ARGS,
};

const CALLEE: &str = r#"
#define MS __attribute__((ms_abi))
struct S8 { int x, y; };
struct S12 { int a, b, c; };
struct S24 { long p, q, r; };
MS long add6(long a, long b, long c, long d, long e, long f) {
    return a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f;
}
MS double mixf(int a, double b, int c, double d, float e, double f) {
    return a + b * 10 + c * 100 + d * 1000 + e * 10000 + f * 100000;
}
MS long by_value(struct S8 s, struct S12 t, char k, short m) {
    return s.x + s.y * 10 + t.a * 100 + t.b * 1000 + t.c * 10000 + k * 100000L + m * 1000000L;
}
MS struct S8 ret_small(int x) { struct S8 s = { x, x + 1 }; return s; }
MS struct S24 ret_big(long a) { struct S24 s = { a, a * 2, a * 3 }; return s; }
MS long apply(MS long (*fp)(long, long, long, long, long, long), long v) {
    return fp(v, v, v, v, v, v);
}
/* Enough live values that an allocator uses the registers Win64 makes
   callee-saved (rsi, rdi, xmm6-xmm15) -- which a gcc caller may then keep its
   own values in across the call. */
MS double pressure(long n, double x) {
    long a = n + 1, b = n * 3, c = n ^ 5, d = n - 7, e = n * n, f = n + 11, g = n * 13, h = n - 17;
    double p = x + 1, q = x * 3, r = x - 5, s = x * x, t = x + 7, u = x * 11, v = x - 13, w = x * 17;
    for (long i = 0; i < n; i++) {
        a += b; b ^= c; c += d; d -= e; e += f; f ^= g; g += h; h -= a;
        p += q; q *= 1.0001; r += s; s -= t; t += u; u *= 0.9999; v += w; w -= p;
    }
    return (double)(a + b + c + d + e + f + g + h) + p + q + r + s + t + u + v + w;
}
"#;

const CALLER: &str = r#"
#define MS __attribute__((ms_abi))
struct S8 { int x, y; };
struct S12 { int a, b, c; };
struct S24 { long p, q, r; };
MS long add6(long, long, long, long, long, long);
MS double mixf(int, double, int, double, float, double);
MS long by_value(struct S8, struct S12, char, short);
MS struct S8 ret_small(int);
MS struct S24 ret_big(long);
MS long apply(MS long (*)(long, long, long, long, long, long), long);
MS double pressure(long, double);
static double ref_pressure(long n, double x) {
    long a = n + 1, b = n * 3, c = n ^ 5, d = n - 7, e = n * n, f = n + 11, g = n * 13, h = n - 17;
    double p = x + 1, q = x * 3, r = x - 5, s = x * x, t = x + 7, u = x * 11, v = x - 13, w = x * 17;
    for (long i = 0; i < n; i++) {
        a += b; b ^= c; c += d; d -= e; e += f; f ^= g; g += h; h -= a;
        p += q; q *= 1.0001; r += s; s -= t; t += u; u *= 0.9999; v += w; w -= p;
    }
    return (double)(a + b + c + d + e + f + g + h) + p + q + r + s + t + u + v + w;
}
int main(void) {
    struct S8 s8 = { 1, 2 };
    struct S12 s12 = { 3, 4, 5 };
    if (add6(1, 2, 3, 4, 5, 6) != 91) return 1;
    if (mixf(1, 2.0, 3, 4.0, 5.0f, 6.0) != 654321.0) return 2;
    if (by_value(s8, s12, 6, 7) != 7654321L) return 3;
    struct S8 r = ret_small(40);
    if (r.x != 40 || r.y != 41) return 4;
    struct S24 b = ret_big(5);
    if (b.p != 5 || b.q != 10 || b.r != 15) return 5;
    if (apply(add6, 1) != 21) return 6;
    /* Values live across the calls, in whatever registers the caller's
       compiler trusts the callee to preserve. */
    long k0 = add6(1, 0, 0, 0, 0, 0), k1 = k0 * 3, k2 = k1 ^ 9, k3 = k2 + 4, k4 = k3 * 7, k5 = k4 - 2;
    double f0 = mixf(1, 0, 0, 0, 0, 0), f1 = f0 * 3, f2 = f1 + 9, f3 = f2 * 4, f4 = f3 - 7, f5 = f4 * 2;
    double total = 0;
    for (long i = 1; i <= 20; i++) {
        double got = pressure(i, (double)i / 3);
        if (got != ref_pressure(i, (double)i / 3)) return 7;
        total += got + k0 + k1 + k2 + k3 + k4 + k5 + f0 + f1 + f2 + f3 + f4 + f5;
        k0 += 1; k1 += 2; k2 += 3; k3 += 4; k4 += 5; k5 += 6;
        f0 += 1; f1 += 2; f2 += 3; f3 += 4; f4 += 5; f5 += 6;
    }
    double expect = 0;
    long j0 = 1, j1 = 3, j2 = 3 ^ 9, j3 = (3 ^ 9) + 4, j4 = ((3 ^ 9) + 4) * 7, j5 = (((3 ^ 9) + 4) * 7) - 2;
    double g0 = 1, g1 = 3, g2 = 12, g3 = 48, g4 = 41, g5 = 82;
    for (long i = 1; i <= 20; i++) {
        expect += ref_pressure(i, (double)i / 3) + j0 + j1 + j2 + j3 + j4 + j5 + g0 + g1 + g2 + g3 + g4 + g5;
        j0 += 1; j1 += 2; j2 += 3; j3 += 4; j4 += 5; j5 += 6;
        g0 += 1; g1 += 2; g2 += 3; g3 += 4; g4 += 5; g5 += 6;
    }
    if (total != expect) return 8;
    return 0;
}
"#;

/// An `ms_abi` function is called with the Microsoft x64 convention: the
/// first four arguments by *position* in rcx/rdx/r8/r9 or xmm0-xmm3, the
/// caller's 32-byte shadow space, an aggregate of 1, 2, 4 or 8 bytes in a
/// register and any other by reference, a larger return through a hidden
/// pointer in rcx, and rsi, rdi and xmm6-xmm15 preserved across the call.
///
/// c17 accepted the attribute and used System V for all of it, so every
/// pairing with gcc read its arguments from the wrong registers. Each side is
/// a separate translation unit, built by c17 and by gcc in every pairing; the
/// `pressure` calls hold enough values live that gcc keeps its own in the
/// registers Win64 makes callee-saved.
#[test]
fn codegen_ms_abi_interoperates_with_gcc() {
    if !cfg!(target_arch = "x86_64") {
        return;
    }
    interop_host("ms_abi", CALLEE, CALLER);
}

/// The declarations every interop pair below shares.
const TYPES: &str = r#"
#define MS __attribute__((ms_abi))
struct S1 { char a; };
struct S2 { short a; };
struct S3 { char a, b, c; };
struct S4 { float f; };
struct S8f { float a, b; };
struct S16 { long a, b; };
struct S24 { long p, q, r; };
struct S200 { long v[25]; };
"#;

const TYPES_CALLEE: &str = r#"
MS long t_s3(struct S3 s, long x) { return s.a + s.b * 10 + s.c * 100 + x * 1000; }
/* The callee owns its by-reference copy, and may write it. */
MS long t_s3_mod(struct S3 s) { s.a = 99; return s.a + s.b; }
MS long t_small(struct S1 a, struct S2 b, struct S4 c, struct S8f d) {
    return a.a + b.a * 10 + (long)(c.f * 100) + (long)(d.a * 1000) + (long)(d.b * 10000);
}
/* By reference past the fourth position: a pointer in an incoming slot. */
MS long t_byref5(long a, long b, long c, long d, struct S16 e, struct S24 f) {
    return a + b + c + d + e.a * 10 + e.b * 100 + f.p * 1000 + f.q * 10000 + f.r * 100000;
}
MS long t_big5(long a, long b, long c, long d, struct S200 s, double e) {
    long t = a + b + c + d + (long)e;
    for (int i = 0; i < 25; i++) t += s.v[i] * (i + 1);
    s.v[0] = -1;
    return t;
}
/* The hidden return pointer takes RCX and moves four arguments along. */
MS struct S24 t_sret4(long a, long b, long c, double d) {
    struct S24 s = { a + b, b + c, c + (long)d }; return s;
}
MS struct S3 t_rs3(char x) { struct S3 s = { x, (char)(x + 1), (char)(x + 2) }; return s; }
MS struct S8f t_rs8f(float x) { struct S8f s = { x, x * 2 }; return s; }
MS long double t_ld(long double a, long x, long double b) { return a * 2 + x + b; }
MS __int128 t_i128(__int128 a, __int128 b) { return a * 3 + b; }
MS float _Complex t_fc(float _Complex a, double _Complex b) { return a + (float _Complex)b; }
MS double _Complex t_dc(double _Complex a) { return a * 2; }
MS _Float16 t_h(_Float16 a, _Float16 b) { return a + b; }
MS __float128 t_q(__float128 a, long x) { return a + x; }
MS _Bool t_bool(_Bool b, unsigned char c, short s) { return b && c == 200 && s == -5; }
MS long t_add6(long a, long b, long c, long d, long e, long f) {
    return a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6;
}
MS long t_call6(MS long (*f)(long, long, long, long, long, long), long x) {
    return f(x, x + 1, x + 2, x + 3, x + 4, x + 5) + t_add6(1, 2, 3, 4, 5, 6);
}
MS long t_aligned(long x, double y) {
    _Alignas(64) long buf[8];
    for (int i = 0; i < 8; i++) buf[i] = x * i;
    long s = 0;
    for (int i = 0; i < 8; i++) s += buf[i];
    return s + (long)y + ((unsigned long)buf & 63);
}
"#;

const TYPES_CALLER: &str = r#"
MS long t_s3(struct S3, long);
MS long t_s3_mod(struct S3);
MS long t_small(struct S1, struct S2, struct S4, struct S8f);
MS long t_byref5(long, long, long, long, struct S16, struct S24);
MS long t_big5(long, long, long, long, struct S200, double);
MS struct S24 t_sret4(long, long, long, double);
MS struct S3 t_rs3(char);
MS struct S8f t_rs8f(float);
MS long double t_ld(long double, long, long double);
MS __int128 t_i128(__int128, __int128);
MS float _Complex t_fc(float _Complex, double _Complex);
MS double _Complex t_dc(double _Complex);
MS _Float16 t_h(_Float16, _Float16);
MS __float128 t_q(__float128, long);
MS _Bool t_bool(_Bool, unsigned char, short);
MS long t_add6(long, long, long, long, long, long);
MS long t_call6(MS long (*)(long, long, long, long, long, long), long);
MS long t_aligned(long, double);
int main(void) {
    struct S3 s3 = { 1, 2, 3 };
    if (t_s3(s3, 4) != 4321) return 1;
    if (t_s3_mod(s3) != 101 || s3.a != 1) return 2;
    struct S1 a = { 5 }; struct S2 b = { 6 }; struct S4 c = { 0.07f };
    struct S8f d = { 0.008f, 0.0009f };
    if (t_small(a, b, c, d) != 5 + 60 + 7 + 8 + 9) return 3;
    struct S16 e = { 1, 2 }; struct S24 f = { 3, 4, 5 };
    if (t_byref5(1, 2, 3, 4, e, f) != 10 + 10 + 200 + 3000 + 40000 + 500000) return 4;
    struct S200 huge;
    long want = 1 + 2 + 3 + 4 + 5;
    for (int i = 0; i < 25; i++) { huge.v[i] = i * 7; want += huge.v[i] * (i + 1); }
    if (t_big5(1, 2, 3, 4, huge, 5.0) != want || huge.v[0] != 0) return 5;
    struct S24 r = t_sret4(1, 2, 3, 4.0);
    if (r.p != 3 || r.q != 5 || r.r != 7) return 6;
    struct S3 rs = t_rs3(10);
    if (rs.a != 10 || rs.b != 11 || rs.c != 12) return 7;
    struct S8f rf = t_rs8f(1.5f);
    if (rf.a != 1.5f || rf.b != 3.0f) return 8;
    if (t_ld(1.5L, 2, 0.25L) != 5.25L) return 9;
    __int128 big = ((__int128)1 << 100) + 7;
    if (t_i128(big, 5) != big * 3 + 5) return 10;
    float _Complex fc = t_fc(1.0f + 2.0f * 1.0iF, 3.0 + 4.0 * 1.0i);
    if (__real__ fc != 4.0f || __imag__ fc != 6.0f) return 11;
    double _Complex dc = t_dc(1.5 + 2.5 * 1.0i);
    if (__real__ dc != 3.0 || __imag__ dc != 5.0) return 12;
    if (t_h((_Float16)1.5, (_Float16)2.25) != (_Float16)3.75) return 13;
    if (t_q((__float128)0.5, 3) != (__float128)3.5) return 14;
    if (t_bool(1, 200, -5) != 1) return 15;
    if (t_call6(t_add6, 1) != 1 + 4 + 9 + 16 + 25 + 36 + 91) return 16;
    if (t_aligned(3, 2.0) != 3 * 28 + 2) return 17;
    return 0;
}
"#;

/// Every class the Microsoft convention distinguishes, against gcc in each
/// direction: aggregates of 1, 2, 4 and 8 bytes in a register whatever their
/// members, every other size by reference to a copy the callee may write --
/// in a register position and past the fourth, where the position holds a
/// pointer -- the hidden return pointer ahead of four more arguments, and
/// the wide scalars: `long double`, `__int128` (returned whole in XMM0),
/// complex values, `_Float16` (in the integer position, as gcc has it) and
/// `__float128`.
#[test]
fn codegen_ms_abi_types_interoperate_with_gcc() {
    if !cfg!(target_arch = "x86_64") {
        return;
    }
    interop_host(
        "ms_abi_types",
        &format!("{TYPES}{TYPES_CALLEE}"),
        &format!("{TYPES}{TYPES_CALLER}"),
    );
}

const VARIADIC_CALLEE: &str = r#"
MS int t_vsum(int n, ...) {
    __builtin_ms_va_list ap;
    __builtin_ms_va_start(ap, n);
    int s = 0;
    for (int i = 0; i < n; i++) s += __builtin_va_arg(ap, int);
    __builtin_ms_va_end(ap);
    return s;
}
MS double t_vmix(int n, ...) {
    __builtin_ms_va_list ap, ap2;
    __builtin_ms_va_start(ap, n);
    __builtin_ms_va_copy(ap2, ap);
    double s = 0;
    for (int i = 0; i < n; i++) s += __builtin_va_arg(ap, double) * (i + 1);
    struct S8f q = __builtin_va_arg(ap, struct S8f);
    s += q.a + q.b;
    s += __builtin_va_arg(ap2, double);
    __builtin_ms_va_end(ap2);
    __builtin_ms_va_end(ap);
    return s;
}
/* A named floating-point parameter, then the variadic ones. */
MS double t_vnamed(double a, int n, ...) {
    __builtin_ms_va_list ap;
    __builtin_ms_va_start(ap, n);
    double s = a;
    for (int i = 0; i < n; i++) s += __builtin_va_arg(ap, double);
    long l = __builtin_va_arg(ap, long);
    __builtin_ms_va_end(ap);
    return s + l;
}
/* A System V function reading a Microsoft list it was handed. */
long t_vtake(int n, __builtin_ms_va_list ap) {
    long s = 0;
    for (int i = 0; i < n; i++) s += __builtin_va_arg(ap, long);
    return s;
}
MS long t_vfwd(int n, ...) {
    __builtin_ms_va_list ap;
    __builtin_ms_va_start(ap, n);
    long s = t_vtake(n, ap);
    __builtin_ms_va_end(ap);
    return s;
}
"#;

const VARIADIC_CALLER: &str = r#"
MS int t_vsum(int, ...);
MS double t_vmix(int, ...);
MS double t_vnamed(double, int, ...);
MS long t_vfwd(int, ...);
int main(void) {
    if (t_vsum(6, 1, 2, 3, 4, 5, 6) != 21) return 1;
    struct S8f q = { 0.5f, 0.25f };
    if (t_vmix(5, 1.0, 2.0, 3.0, 4.0, 5.0, q) != 1 + 4 + 9 + 16 + 25 + 0.75 + 1.0) return 2;
    if (t_vnamed(0.5, 3, 1.0, 2.0, 4.0, 10L) != 17.5) return 3;
    if (t_vfwd(5, 1L, 2L, 3L, 4L, 5L) != 15) return 4;
    return 0;
}
"#;

/// A variadic `ms_abi` function, defined with `__builtin_ms_va_list` and
/// called with floating-point arguments, which travel in both register files
/// for the first four positions -- the callee reads every position from
/// memory, the integer registers spilled to its shadow area. gcc reads a
/// by-reference type out of the position itself when it walks the list, so
/// only by-value types are asked for here.
#[test]
fn codegen_ms_abi_variadic_interoperates_with_gcc() {
    if !cfg!(target_arch = "x86_64") {
        return;
    }
    interop_host(
        "ms_abi_variadic",
        &format!("{TYPES}{VARIADIC_CALLEE}"),
        &format!("{TYPES}{VARIADIC_CALLER}"),
    );
}

const PRESERVE_CALLEE: &str = r#"
#define MS __attribute__((ms_abi))
long sysv_clobber(long x);
/* Values live across a call to a System V function, which may destroy
   rsi, rdi and xmm6-xmm15 -- registers this function's own caller expects
   back intact. */
MS double t_calls_sysv(long n, double d) {
    long a = n * 3, b = n ^ 7, c = n + 11, e = n - 2, f = n * n;
    double p = d * 2, q = d + 3, r = d - 1, s = d * d, t = d + 0.5;
    long k = sysv_clobber(n);
    return (double)(a + b + c + e + f + k) + p + q + r + s + t;
}
"#;

const PRESERVE_CALLER: &str = r#"
#define MS __attribute__((ms_abi))
MS double t_calls_sysv(long n, double d);
__attribute__((noinline)) long sysv_clobber(long x) {
    __asm__ volatile("movq $1, %%rsi\n\tmovq $2, %%rdi\n\t"
                     "xorps %%xmm6, %%xmm6\n\txorps %%xmm7, %%xmm7\n\t"
                     "xorps %%xmm8, %%xmm8\n\txorps %%xmm9, %%xmm9\n\t"
                     "xorps %%xmm10, %%xmm10\n\txorps %%xmm11, %%xmm11\n\t"
                     "xorps %%xmm12, %%xmm12\n\txorps %%xmm13, %%xmm13\n\t"
                     "xorps %%xmm14, %%xmm14\n\txorps %%xmm15, %%xmm15"
                     ::: "rsi", "rdi", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10",
                         "xmm11", "xmm12", "xmm13", "xmm14", "xmm15");
    return x + 1;
}
/* gcc spills its own XMM values around an ms_abi call rather than trusting
   the callee with them, so whether the callee preserved XMM6-XMM15 is
   asked directly: sentinels in every register Win64 makes callee-saved,
   the call, and a look at what came back. Below the red zone and on the
   sixteen-byte boundary the call wants, with its shadow area. */
static long kept[12];
__attribute__((noinline)) static int preserved(void) {
    __asm__ volatile(
        "movq $106, %%rax\n\tmovq %%rax, %%xmm6\n\t"
        "movq $107, %%rax\n\tmovq %%rax, %%xmm7\n\t"
        "movq $108, %%rax\n\tmovq %%rax, %%xmm8\n\t"
        "movq $109, %%rax\n\tmovq %%rax, %%xmm9\n\t"
        "movq $110, %%rax\n\tmovq %%rax, %%xmm10\n\t"
        "movq $111, %%rax\n\tmovq %%rax, %%xmm11\n\t"
        "movq $112, %%rax\n\tmovq %%rax, %%xmm12\n\t"
        "movq $113, %%rax\n\tmovq %%rax, %%xmm13\n\t"
        "movq $114, %%rax\n\tmovq %%rax, %%xmm14\n\t"
        "movq $115, %%rax\n\tmovq %%rax, %%xmm15\n\t"
        "movq $116, %%rsi\n\tmovq $117, %%rdi\n\t"
        "movq %%rsp, %%rbx\n\tsubq $128, %%rsp\n\tandq $-16, %%rsp\n\tsubq $32, %%rsp\n\t"
        "movl $5, %%ecx\n\tmovq $0x3ff8000000000000, %%rax\n\tmovq %%rax, %%xmm1\n\t"
        /* A C symbol is `_name` in Mach-O and `name` everywhere else, and
           this `call` names one by hand. */
#ifdef __APPLE__
        "call _t_calls_sysv\n\t"
#else
        "call t_calls_sysv\n\t"
#endif
        "movq %%rbx, %%rsp\n\t"
        "movq %%xmm6, 0(%0)\n\tmovq %%xmm7, 8(%0)\n\tmovq %%xmm8, 16(%0)\n\t"
        "movq %%xmm9, 24(%0)\n\tmovq %%xmm10, 32(%0)\n\tmovq %%xmm11, 40(%0)\n\t"
        "movq %%xmm12, 48(%0)\n\tmovq %%xmm13, 56(%0)\n\tmovq %%xmm14, 64(%0)\n\t"
        "movq %%xmm15, 72(%0)\n\tmovq %%rsi, 80(%0)\n\tmovq %%rdi, 88(%0)"
        : : "r"(kept)
        : "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11",
          "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
          "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
          "memory");
    for (int i = 0; i < 12; i++)
        if (kept[i] != 106 + i) return 0;
    return 1;
}
static double expect(long n, double d) {
    long a = n * 3, b = n ^ 7, c = n + 11, e = n - 2, f = n * n;
    double p = d * 2, q = d + 3, r = d - 1, s = d * d, t = d + 0.5;
    return (double)(a + b + c + e + f + (n + 1)) + p + q + r + s + t;
}
int main(void) {
    /* Enough values live across each call that the caller keeps some in
       the registers Win64 makes callee-saved. */
    double f0 = 1, f1 = 2, f2 = 3, f3 = 4, f4 = 5, f5 = 6, f6 = 7, f7 = 8;
    long k0 = 1, k1 = 2, k2 = 3, k3 = 4, k4 = 5, k5 = 6;
    double got[10], total = 0, want = 0;
    /* Nothing but the ms_abi call in the loop, so the values stay in
       registers across it rather than being spilled for another call. */
    for (long i = 1; i <= 10; i++) {
        got[i - 1] = t_calls_sysv(i, (double)i / 4);
        total += f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + k0 + k1 + k2 + k3 + k4 + k5;
        f0 += 1; f1 += 2; f2 += 3; f3 += 4; f4 += 5; f5 += 6; f6 += 7; f7 += 8;
        k0 += 1; k1 += 2; k2 += 3; k3 += 4; k4 += 5; k5 += 6;
    }
    for (long i = 1; i <= 10; i++) {
        if (got[i - 1] != expect(i, (double)i / 4)) return 1;
        want += 36 + (i - 1) * 36 + 21 + (i - 1) * 21;
    }
    if (total != want) return 2;
    return preserved() ? 0 : 3;
}
"#;

/// An `ms_abi` function calling System V code: the callee may destroy RSI,
/// RDI and XMM6-XMM15, which the `ms_abi` function's own caller expects
/// back, so it saves and restores them -- whole XMM registers. The System V
/// function clobbers all of them, and the caller keeps values live across
/// the `ms_abi` call; with c17 on either side, and gcc on the other.
#[test]
fn codegen_ms_abi_preserves_registers_across_sysv_calls() {
    if !cfg!(target_arch = "x86_64") {
        return;
    }
    interop_host("ms_abi_preserve", PRESERVE_CALLEE, PRESERVE_CALLER);
}

/// Both conventions in one translation unit, so the inliner meets them: an
/// `ms_abi` function inlined into a System V caller and the reverse must
/// agree on how each argument travels.
#[test]
fn codegen_ms_abi_inlines_across_conventions() {
    if !cfg!(target_arch = "x86_64") {
        return;
    }
    let src = format!("{TYPES}{TYPES_CALLEE}{}", TYPES_CALLER);
    for opt in ["-O0", "-O1", "-O2", "-O3"] {
        assert_eq!(
            compile_and_run("ms_abi_inline", &src, &[opt.to_string()]),
            0,
            "{opt}"
        );
    }
    let src = format!("{TYPES}{VARIADIC_CALLEE}{VARIADIC_CALLER}");
    assert_eq!(
        compile_and_run("ms_abi_inline_va", &src, &["-O2".to_string()]),
        0
    );
}

/// The convention is part of the function type: a pointer to an `ms_abi`
/// function and one to an ordinary function are incompatible, as gcc warns,
/// and a redeclaration that changes the convention conflicts.
#[test]
fn codegen_ms_abi_function_types_are_distinct() {
    if !cfg!(target_arch = "x86_64") {
        return;
    }
    compile_expect_warning(
        "ms_abi_ptr",
        "long s(long);\n\
         typedef __attribute__((ms_abi)) long (*msfp)(long);\n\
         msfp p = s;\n",
        "incompatible pointer type",
    );
    compile_expect_warning(
        "ms_abi_ptr_rev",
        "__attribute__((ms_abi)) long m(long);\n\
         long (*p)(long) = m;\n",
        "incompatible pointer type",
    );
    compile_expect_error(
        "ms_abi_redecl",
        "__attribute__((ms_abi)) long m(long);\n\
         long m(long x) { return x; }\n",
        "conflicting types",
    );
    compile_expect_error(
        "ms_abi_both",
        "__attribute__((ms_abi, sysv_abi)) long m(long);\n",
        "'ms_abi' and 'sysv_abi' attributes are not compatible",
    );
    compile_expect_warning(
        "ms_abi_object",
        "__attribute__((ms_abi)) int x;\n",
        "'ms_abi' attribute only applies to function types",
    );
    compile_expect_error(
        "ms_abi_va_start",
        "__attribute__((ms_abi)) int f(int n, ...) {\n\
         __builtin_va_list ap; __builtin_va_start(ap, n); return 0; }\n",
        "'va_start' used in Win64 ABI function",
    );
}

/// gcc's unwinder walks out of an `ms_abi` function through the rules its
/// prologue records: the frame, and the XMM registers it saved, each as the
/// DWARF register it is (XMM6 is 23).
#[test]
fn codegen_ms_abi_unwinds() {
    if !cfg!(all(target_arch = "x86_64", target_os = "linux")) {
        return;
    }
    let src = r#"
#include <execinfo.h>
__attribute__((noinline)) int depth(void) {
    void *frames[16];
    return backtrace(frames, 16);
}
__attribute__((ms_abi, noinline)) int ms(double x) {
    double y = x * 3;
    int d = depth();
    return d + (y == 6.0 ? 0 : 100);
}
int main(void) {
    /* depth, ms, main, and the C library's start-up beneath it. */
    return ms(2.0) >= 4 ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("ms_abi_unwind", src, &["-g".to_string()]),
        0
    );
    let asm = asm_for_at("ms_abi_cfi", src, &["-g", "-O2"]);
    for n in 23..=32 {
        assert!(
            asm.contains(&format!(".cfi_offset {n}, ")),
            "no unwind rule for DWARF register {n}:\n{asm}"
        );
    }
}

/// gcc for aarch64 has no `ms_abi` or `sysv_abi`: it warns that each is
/// ignored, and the function keeps the native convention. So does c17 --
/// the warning is in the attributes group -- and a call still uses AAPCS64,
/// against gcc in every pairing.
#[test]
fn codegen_ms_abi_is_ignored_on_aarch64() {
    let src = "__attribute__((ms_abi)) long f(long a);\n\
               __attribute__((sysv_abi)) long g(long a);\n";
    let mut args: Vec<String> = AARCH64_TARGET_ARGS.iter().map(|s| s.to_string()).collect();
    let stderr = compile_expect_warning_with("ms_abi_a64", src, &args);
    assert!(
        stderr.contains("'ms_abi' attribute directive ignored")
            && stderr.contains("'sysv_abi' attribute directive ignored"),
        "{stderr}"
    );
    args.push("-Wno-attributes".to_string());
    let quiet = compile_expect_warning_with("ms_abi_a64_quiet", src, &args);
    assert!(!quiet.contains("ms_abi"), "{quiet}");
    if !aarch64_cross_available() {
        eprintln!("skipping the aarch64 run: no cross toolchain");
        return;
    }
    interop_aarch64(
        "ms_abi_a64",
        r#"
__attribute__((ms_abi)) long add6(long a, long b, long c, long d, long e, long f) {
    return a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f;
}
struct S3 { char a, b, c; };
__attribute__((ms_abi)) long s3(struct S3 s, double d) { return s.a + s.b + s.c + (long)d; }
"#,
        r#"
__attribute__((ms_abi)) long add6(long, long, long, long, long, long);
struct S3 { char a, b, c; };
__attribute__((ms_abi)) long s3(struct S3, double);
int main(void) {
    struct S3 s = { 1, 2, 3 };
    return add6(1, 2, 3, 4, 5, 6) == 91 && s3(s, 4.0) == 10 ? 0 : 1;
}
"#,
    );
}

/// A pointer argument that arrived in the incoming area holds the pointer:
/// taking the address of its object loads it. A Win64 by-reference struct
/// past the fourth position is one; so is a System V pointer past the sixth,
/// which at -O2 is forwarded straight to a call passing `*p` by value. Both
/// had the *slot's* address taken as though the argument were the object.
#[test]
fn codegen_stacked_pointer_argument_addresses_its_object() {
    if !cfg!(target_arch = "x86_64") {
        return;
    }
    let src = r#"
struct Big { long v[5]; };
__attribute__((noinline)) long g(struct Big b) { return b.v[0] + b.v[4]; }
__attribute__((noinline)) long f(long a, long b, long c, long d, long e, long h,
                                 struct Big *p) { return g(*p) + a; }
__attribute__((ms_abi, noinline)) long m(long a, long b, long c, long d, struct Big *p) {
    return g(*p) + a;
}
__attribute__((ms_abi, noinline)) long r(long a, long b, long c, long d, struct Big s) {
    return g(s) + a;
}
int main(void) {
    struct Big x = { { 1, 2, 3, 4, 5 } };
    if (f(1, 2, 3, 4, 5, 6, &x) != 7) return 1;
    if (m(1, 2, 3, 4, &x) != 7) return 2;
    if (r(1, 2, 3, 4, x) != 7) return 3;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("stacked_pointer_arg", src, &[opt.to_string()]),
            0,
            "{opt}"
        );
    }
}
