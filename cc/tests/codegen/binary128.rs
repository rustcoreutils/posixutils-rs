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
//

use crate::codegen::asm_probe::{asm_for_with, AARCH64_LINUX, X86_64_LINUX};
use crate::common::{compile_and_run, compile_and_run_aarch64};

/// Each constant operation below has its answer at compile time, and every
/// `link_error` call goes with its branch.
///
/// The operations used to become `__addtf3`, `__netf2` and the rest before the
/// optimizer ran, and a call is not a value any folder can see into: all four
/// branches survived `-O2` on aarch64, and `builtins/complex-1` failed to link
/// for its `long double _Complex` cases. `-3.5L` was a `__negtf2` call.
#[test]
fn binary128_constants_fold_before_the_libcalls() {
    let src = "
        extern void link_error(void);
        TY neg(void) { return -3.5SFX; }
        void f(void) {
            if (1.0SFX + 2.0SFX != 3.0SFX) link_error();
            if (6.0SFX * 0.5SFX != 3.0SFX) link_error();
            if ((double)(1.0SFX / 4.0SFX) != 0.25) link_error();
            if ((int)(7.0SFX - 0.5SFX) != 6) link_error();
            if (!(-1.0SFX < 0.0SFX)) link_error();
        }
    ";
    for (triple, ty, sfx) in [
        (AARCH64_LINUX, "long double", "L"),
        (X86_64_LINUX, "_Float128", "F128"),
        (AARCH64_LINUX, "_Float128", "F128"),
    ] {
        let src = src.replace("TY", ty).replace("SFX", sfx);
        for opt in ["-O1", "-O2"] {
            let asm = asm_for_with("b128_fold", triple, &src, &[opt]);
            for callee in [
                "link_error",
                "__addtf3",
                "__multf3",
                "__divtf3",
                "__subtf3",
                "__negtf2",
                "__netf2",
                "__lttf2",
                "__trunctfdf2",
                "__fixtfsi",
            ] {
                assert!(
                    !asm.contains(callee),
                    "{triple} {ty} {opt}: {callee} should have folded away:\n{asm}"
                );
            }
        }
    }
}

/// Without the optimizer the same operations are still the libgcc calls.
#[test]
fn binary128_is_still_a_libcall_at_o0() {
    let src = "long double f(long double a, long double b) { return -(a + b); }";
    let asm = asm_for_with("b128_o0", AARCH64_LINUX, src, &["-O0"]);
    for callee in ["__addtf3", "__negtf2"] {
        assert!(asm.contains(callee), "missing {callee}:\n{asm}");
    }
}

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
