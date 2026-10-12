//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// IEEE binary128: `long double` on aarch64 Linux, `_Float128` everywhere.
// No target computes it in hardware, so every operation is a libgcc call --
// but only after the optimizer, which folds the operations exactly first.
// The assembly checks compile in process, in `cc/test_asm/codegen_binary128.rs`.
//

use crate::common::{compile_and_run, compile_and_run_aarch64};

/// Every binary128 result, folded from constants and computed at run time by
/// libgcc from `volatile` operands, bit for bit, and against gcc's answer.
///
/// Returns the number of the first check that fails. The expected bit
/// patterns are what aarch64-linux-gnu-gcc and x86-64 gcc print for the same
/// source; the NaN is compared only folded against computed, because its sign
/// is the target's default NaN and differs between the two.
const EXACT: &str = r#"
#include <string.h>
typedef TY ty;
typedef unsigned __int128 u128;
#define K(x) x##SFX

static int n;
static u128 bits(ty x) { u128 u; memcpy(&u, &x, 16); return u; }
static int agree(ty c, ty r) { n++; return bits(c) == bits(r); }
static int is(ty c, ty r, unsigned long long hi, unsigned long long lo)
{
    return agree(c, r) && bits(c) == (((u128)hi << 64) | lo);
}
static int same_int(long long c, long long r, long long want) { n++; return c == r && c == want; }

volatile ty v1 = K(1.0), v3 = K(3.0), v01 = K(0.1), vbig = K(1.0e4000),
    vtiny = K(1.0e-4940), vz = K(0.0), vm = K(-7.9), v4e9 = K(4.0e9);
volatile double vd = 0.1;
volatile int vi = -123456789;
volatile unsigned long long vu = 0xffffffffffffffffULL;
volatile long long vl = 0x7fffffffffffffffLL;

int main(void)
{
    ty c1 = K(1.0), c3 = K(3.0), c01 = K(0.1), cbig = K(1.0e4000),
        ctiny = K(1.0e-4940), cz = K(0.0), cm = K(-7.9), c4e9 = K(4.0e9);
    double cd = 0.1; int ci = -123456789;
    unsigned long long cu = 0xffffffffffffffffULL; long long cl = 0x7fffffffffffffffLL;
    ty r1 = v1, r3 = v3, r01 = v01, rbig = vbig, rtiny = vtiny, rz = vz, rm = vm, r4e9 = v4e9;
    double rd = vd; int ri = vi; unsigned long long ru = vu; long long rl = vl;
    ty cn = cz / cz, rn = rz / rz;

#define F(E, hi, lo) if (!is(E(c), E(r), hi, lo)) return n;
#define ADD(p) (p##1 + p##3)
#define SUB(p) (p##01 - p##3)
#define MUL(p) (p##01 * p##3)
#define DIV(p) (p##1 / p##3)
#define NEG(p) (-p##01)
#define NEGZ(p) (-p##z)
#define OVF(p) (p##big * p##big)
#define UNF(p) (p##tiny / p##3)
#define SUBN(p) (p##tiny * p##01)
#define DZ(p) (p##1 / p##z)
#define FROMD(p) ((ty)p##d)
#define FROMI(p) ((ty)p##i)
#define FROMU(p) ((ty)p##u)
#define FROML(p) ((ty)p##l)
    F(ADD, 0x4001000000000000ULL, 0)
    F(SUB, 0xc000733333333333ULL, 0x3333333333333333ULL)
    F(MUL, 0x3ffd333333333333ULL, 0x3333333333333334ULL)
    F(DIV, 0x3ffd555555555555ULL, 0x5555555555555555ULL)
    F(NEG, 0xbffb999999999999ULL, 0x999999999999999aULL)
    F(NEGZ, 0x8000000000000000ULL, 0)
    F(OVF, 0x7fff000000000000ULL, 0)
    F(UNF, 0x000000000004421aULL, 0x5eec127a7f358513ULL)
    F(SUBN, 0x0000000000014707ULL, 0xe946d257f2f674b9ULL)
    F(DZ, 0x7fff000000000000ULL, 0)
    F(FROMD, 0x3ffb999999999999ULL, 0xa000000000000000ULL)
    F(FROMI, 0xc019d6f345400000ULL, 0)
    F(FROMU, 0x403effffffffffffULL, 0xfffe000000000000ULL)
    F(FROML, 0x403dffffffffffffULL, 0xfffc000000000000ULL)
    if (!agree(cn + c1, rn + r1) || rn == rn) return n;

#define I(E, want) if (!same_int(E(c), E(r), want)) return n;
#define TOI(p) ((long long)(int)p##m)
#define TOU(p) ((long long)(unsigned)p##4e9)
#define TOL(p) ((long long)(p##4e9 * p##4e9 / K(2.0)))
#define TOUL(p) ((long long)(unsigned long long)(p##4e9 * p##4e9))
#define EQ(p) (p##1 == p##1)
#define NE(p) (p##1 != p##3)
#define LT(p) (p##1 < p##3)
#define LE(p) (p##3 <= p##1)
#define GT(p) (p##3 > p##1)
#define GE(p) (p##1 >= p##1)
#define NEQN(p) (p##n == p##n)
#define NNEN(p) (p##n != p##n)
#define NLT(p) (p##n < p##1)
#define NLE(p) (p##n <= p##1)
#define NGT(p) (p##n > p##1)
#define NGE(p) (p##1 >= p##n)
#define ZEQ(p) (NEGZ(p) == p##z)
#define TOD(p) ((double)DIV(p) == 1.0 / 3.0)
#define TOF(p) ((float)DIV(p) == 1.0f / 3.0f)
#define TODOVF(p) ((double)p##big == __builtin_inf())
    I(TOI, -7) I(TOU, 4000000000LL) I(TOL, 8000000000000000000LL)
    I(TOUL, -2446744073709551616LL)
    I(EQ, 1) I(NE, 1) I(LT, 1) I(LE, 0) I(GT, 1) I(GE, 1)
    I(NEQN, 0) I(NNEN, 1) I(NLT, 0) I(NLE, 0) I(NGT, 0) I(NGE, 0) I(ZEQ, 1)
    I(TOD, 1) I(TOF, 1) I(TODOVF, 1)
    return 0;
}
"#;

/// aarch64 `long double`, under qemu, at `-O0` and `-O2`.
#[test]
fn binary128_long_double_folds_exactly_on_aarch64() {
    let src = EXACT.replace("TY", "long double").replace("SFX", "L");
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("b128_exact_a64", &src, opt) {
            assert_eq!(code, 0, "check {code} failed at {opt}");
        }
    }
}

/// `_Float128` on the host, which links libgcc's soft-float routines. Apple's
/// runtime library does not provide them.
#[test]
fn binary128_float128_folds_exactly_on_the_host() {
    if !cfg!(target_os = "linux") {
        return;
    }
    let src = EXACT.replace("TY", "_Float128").replace("SFX", "F128");
    assert_eq!(compile_and_run("b128_exact_host", &src, &[]), 0);
}

/// Eighteen binary128 values live at once outrun the fourteen allocatable
/// XMM registers. A 16-byte value that lost the coloring was given an
/// 8-byte stack slot, so two spilled neighbours overlapped and the stores
/// ran into the frame: the program crashed at every `-O` level.
///
/// Not on macOS: Apple's targets have no `_Float128`, as with clang.
#[cfg(not(target_os = "macos"))]
#[test]
fn binary128_values_spilled_under_register_pressure() {
    const N: usize = 18;
    let decl: Vec<String> = (0..N).map(|i| format!("a{i} = s[{i}]")).collect();
    let stores: String = (0..N)
        .map(|i| format!("    d[{i}] = a{};\n", N - 1 - i))
        .collect();
    let src = format!(
        "typedef _Float128 Q;\n\
         __attribute__((noinline)) void rev(Q *d, const Q *s) {{\n    Q {};\n{stores}}}\n\
         int main(void) {{\n\
             Q s[{N}], d[{N}];\n\
             for (int i = 0; i < {N}; i++) s[i] = (Q)(i * 3 + 1) / 7;\n\
             rev(d, s);\n\
             for (int i = 0; i < {N}; i++) if (d[i] != s[{} - i]) return i + 1;\n\
             return 0;\n\
         }}\n",
        decl.join(", "),
        N - 1
    );
    crate::common::compile_and_run_everywhere("binary128_spill_pressure", &src);
}

/// A `long double` argument for a `_Float128` parameter, and the reverse,
/// is converted (C17 6.5.2.2p7). Calls decided by size whether a floating
/// argument needed converting, and on x86-64 both types are sixteen bytes:
/// the x87 value went over unconverted, on the stack, and the callee read
/// %xmm0. The other conversions between the two are checked alongside.
///
/// Not on macOS: Apple's targets have no `_Float128`, as with clang.
#[cfg(not(target_os = "macos"))]
#[test]
fn binary128_long_double_arguments_are_converted() {
    let src = r#"
long double g_ld = 0.5L;
_Float128 g_q = 0.25f128;
__attribute__((noinline)) double one(_Float128 x) { return (double)x; }
__attribute__((noinline)) double two(_Float128 x, _Float128 y) { return (double)x * 10 + (double)y; }
__attribute__((noinline)) double ldp(long double x) { return (double)x; }
__attribute__((noinline)) _Float128 ret_q(long double x) { return x; }
__attribute__((noinline)) long double ret_ld(_Float128 x) { return x; }
struct S { _Float128 q; long double l; };
int main(void) {
    long double a = 0.5L, b = 0.25L;
    if (one(g_ld) != 0.5) return 1;
    if (two(g_ld, g_ld) != 5.5) return 2;
    if (two(a, b) != 5.25) return 3;
    if (ldp(g_q) != 0.25) return 4;
    if ((double)ret_q(a) != 0.5) return 5;
    if ((double)ret_ld(g_q) != 0.25) return 6;
    struct S s = { a, g_q };
    if ((double)s.q != 0.5 || (double)s.l != 0.25) return 7;
    _Float128 q = 1; q += a;
    if ((double)q != 1.5) return 8;
    long double l = 1; l += g_q;
    if ((double)l != 1.25) return 9;
    if (!(a > g_q) || (a < g_q) || a == g_q) return 10;
    if ((double)(a + g_q) != 0.75 || sizeof(a + g_q) != 16) return 11;
    _Float128 arr[2] = { a, b };
    if ((double)arr[1] != 0.25) return 12;
    return 0;
}
"#;
    crate::common::compile_and_run_everywhere("binary128_ld_args", src);
}

/// `_Float16` to and from binary128 (`long double` on aarch64, `_Float128`)
/// and `__int128`, by libgcc's `hf` routines. On aarch64 the binary128
/// conversions came out as an `fcvt` between an `h` and a `q` register, an
/// instruction A64 does not have, and `__int128` from `_Float16` was wrong.
///
/// Not on macOS: Apple's targets have no `_Float128`, as with clang.
#[cfg(not(target_os = "macos"))]
#[test]
fn binary128_half_conversions() {
    let src = r#"
_Float16 h = 1.5f16;
volatile long double vld = 2.5L;
volatile _Float128 vq = 3.5f128;
volatile __int128 vi = 1000;
volatile unsigned __int128 vu = 2000;
volatile _Float16 vh = 1500.0f16;
__attribute__((noinline)) long double a(void) { return h; }
__attribute__((noinline)) _Float128 b(void) { return h; }
__attribute__((noinline)) _Float16 c(long double x) { return x; }
__attribute__((noinline)) _Float16 d(_Float128 x) { return x; }
__attribute__((noinline)) double use(_Float128 q) { return (double)q; }
__attribute__((noinline)) double e(void) { return use(h); }
__attribute__((noinline)) _Float128 f(_Float16 x) { return (_Float128)x * 2; }
__attribute__((noinline)) long double g(_Float16 x) { return (long double)x * 2; }
int main(void) {
    if (a() != 1.5L) return 1;
    if (b() != 1.5f128) return 2;
    if (c(vld) != 2.5f16) return 3;
    if (d(vq) != 3.5f16) return 4;
    if (e() != 1.5) return 5;
    if (f(h) != 3.0f128) return 6;
    if (g(h) != 3.0L) return 7;
    _Float16 k = vld;
    if (k != 2.5f16) return 8;
    _Float16 m = vq;
    if (m != 3.5f16) return 9;
    if ((_Float128)h + vq != 5.0f128) return 10;
    if (h + vld != 4.0L) return 11;
    _Float16 i1 = vi, u1 = vu;
    __int128 i2 = vh;
    unsigned __int128 u2 = vh;
    if (i1 != 1000.0f16 || u1 != 2000.0f16) return 12;
    if (i2 != 1500 || u2 != 1500) return 13;
    return 0;
}
"#;
    crate::common::compile_and_run_everywhere("binary128_half", src);
}
