//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Variadic calls and stack-passed arguments that have to be run: a
// round trip on the host, and c17 against gcc under qemu. The assembly
// assertions for the same shapes compile in process, in
// `cc/test_asm/codegen_cross_abi_varargs.rs`.
//

use crate::common::{aarch64_cross_available, create_c_file, cross_link_and_run, run_c17};

/// Every shape of argument that overflows the register file and arrives in the
/// **caller's** frame.
///
/// aarch64 used to describe such an argument with the same `Loc::Stack` variant
/// as a local, telling the two frames apart by the *sign* of an `i32` (#C34).
/// Splitting them into `Loc::Stack(LocalSlot)` and `Loc::IncomingArg(IncomingOff)`
/// made the compiler find the sites that mattered -- but only where a `match`
/// was exhaustive. Where one had a `_` arm, an incoming argument silently fell
/// through it and the parameter was left uninitialized, which is exactly the
/// failure `codegen_aarch64_stacked_hfa_uses_its_own_element_size` caught.
///
/// So these run the values rather than inspecting assembly, and they fill the
/// argument registers first so the interesting parameter is genuinely stacked.
/// On this host they exercise the x86-64 path; the aarch64 sweep extracts and
/// runs the same programs under qemu, which is where they earn their keep.
#[test]
fn codegen_stacked_arguments_of_every_class_round_trip() {
    let code = r#"
#include <stdarg.h>
#include <complex.h>

/* Eight of each fills the integer and floating-point argument registers. */
#define I8 long a,long b,long c,long d,long e,long f,long g,long h
#define D8 double a,double b,double c,double d,double e,double f,double g,double h
#define IA 1,2,3,4,5,6,7,8

struct P2f { float x, y; };
struct P2d { double x, y; };
struct Q4f { float a, b, c, d; };
struct S2i { int a, b; };
struct S2l { long a, b; };
struct Big { long v[8]; };

static long   s_long(I8, long z)            { return z; }
static double s_double(D8, double z)        { return z; }
static float  s_float(D8, float z)          { return z; }
static float  s_hfa2f(D8, struct P2f zp)    { return zp.x + zp.y; }
static double s_hfa2d(D8, struct P2d zp)    { return zp.x + zp.y; }
static float  s_hfa4f(D8, struct Q4f zq)    { return zq.a + zq.b + zq.c + zq.d; }
static int    s_s2i(I8, struct S2i zs)      { return zs.a + zs.b; }
static long   s_s2l(I8, struct S2l zs)      { return zs.a + zs.b; }
static long   s_big(I8, struct Big zb)      { return zb.v[0] + zb.v[7]; }
static __int128 s_i128(I8, __int128 z)      { return z; }
static unsigned __int128 s_u128(I8, unsigned __int128 z) { return z; }
static double s_cplx(D8, double _Complex z) { return __real__ z + __imag__ z; }
static long double s_ld(D8, long double z)  { return z; }
static int    s_branch(I8, int z)           { if (z) return 7; return 9; }
static int    s_switch(I8, int z)           { switch (z) { case 5: return 7; default: return 9; } }
/* The address of a stacked argument, and a stacked argument forwarded on. */
static long   s_addr(I8, struct S2l zs)     { struct S2l *zp = &zs; return zp->a + zp->b; }
static double s_inner(struct P2d zp)        { return zp.x * 10 + zp.y; }
static double s_fwd(D8, struct P2d zp)      { return s_inner(zp); }
static __int128 s_i128_inner(__int128 v)    { return v + 1; }
static __int128 s_i128_fwd(I8, __int128 zv) { return s_i128_inner(zv); }

static long s_varargs(int n, ...) {
    va_list ap; va_start(ap, n);
    long t = 0;
    for (int i = 0; i < n; i++) t += va_arg(ap, long);
    va_end(ap);
    return t;
}

int main(void) {
    if (s_long(IA, 42) != 42) return 1;
    if (s_double(IA, 2.5) != 2.5) return 2;
    if (s_float(IA, 2.5f) != 2.5f) return 3;

    struct P2f p2f = {1.5f, 2.5f};
    if (s_hfa2f(IA, p2f) != 4.0f) return 4;
    struct P2d p2d = {1.5, 2.5};
    if (s_hfa2d(IA, p2d) != 4.0) return 5;
    struct Q4f q4f = {1, 2, 3, 4};
    if (s_hfa4f(IA, q4f) != 10.0f) return 6;

    struct S2i s2i = {3, 4};
    if (s_s2i(IA, s2i) != 7) return 7;
    struct S2l s2l = {3, 4};
    if (s_s2l(IA, s2l) != 7) return 8;
    struct Big big = {{1, 0, 0, 0, 0, 0, 0, 9}};
    if (s_big(IA, big) != 10) return 9;

    /* A 128-bit argument must survive whole: a fallback that loads 64 bits
       and zeroes the top half is a silent wrong answer, not a crash. */
    if (s_i128(IA, (__int128)-1) != -1) return 10;
    unsigned __int128 hi = (unsigned __int128)1 << 100;
    if (s_u128(IA, hi) != hi) return 11;

    double _Complex z = 1.5 + 2.5 * _Complex_I;
    if (s_cplx(IA, z) != 4.0) return 12;
    if (s_ld(IA, 4.0L) != 4.0L) return 13;

    if (s_branch(IA, 1) != 7) return 14;
    if (s_switch(IA, 5) != 7) return 15;

    if (s_addr(IA, s2l) != 7) return 16;
    if (s_fwd(IA, p2d) != 17.5) return 17;  /* 1.5*10 + 2.5 */
    if (s_i128_fwd(IA, (__int128)41) != 42) return 18;

    if (s_varargs(10, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L) != 55) return 19;
    return 0;
}
"#;
    assert_eq!(
        crate::common::compile_and_run("stacked_args_all", code, &[]),
        0
    );
    assert_eq!(
        crate::common::compile_and_run_optimized("stacked_args_all_o2", code),
        0
    );
}

/// A stacked composite passed by reference takes an eight-aligned slot.
///
/// A composite over sixteen bytes travels as a pointer to a copy (AAPCS64
/// C.4), so its stacked slot is a pointer's: eight bytes, eight-aligned,
/// whatever the composite's own members want. Both c17's caller and callee
/// aligned the slot to the composite's sixteen-byte member alignment, so
/// against gcc a c17 callee read the wrong slot and crashed, and a gcc callee
/// read a c17 caller's `after` from the wrong place.
#[test]
fn codegen_aarch64_stacked_by_reference_argument_slot() {
    if !aarch64_cross_available() {
        eprintln!("SKIP: no aarch64 cross toolchain");
        return;
    }
    let callee_src = r#"
struct L { long a; long double b; };
long callee(long a0, long a1, long a2, long a3, long a4, long a5, long a6, long a7,
            long s0, struct L big, long after)
{
    return s0 + big.a + (long)big.b + after;
}
"#;
    let caller_src = r#"
struct L { long a; long double b; };
long callee(long, long, long, long, long, long, long, long, long, struct L, long);
int main(void)
{
    struct L l = { 10, 20.0L };
    return callee(0, 0, 0, 0, 0, 0, 0, 0, 1, l, 300) == 331 ? 0 : 1;
}
"#;
    let callee_c = create_c_file("a64_byref_callee", callee_src);
    let caller_c = create_c_file("a64_byref_caller", caller_src);
    let callee_path = callee_c.path().to_string_lossy().to_string();
    let caller_path = caller_c.path().to_string_lossy().to_string();
    assert_eq!(
        cross_link_and_run("a64_byref_ref", &[&caller_path, &callee_path]),
        0,
        "the gcc/gcc reference must pass"
    );
    for opt in ["-O0", "-O2"] {
        // c17 on one side, gcc on the other, each way round.
        for (c17_side, gcc_side, what) in [
            (&callee_path, &caller_path, "c17 callee"),
            (&caller_path, &callee_path, "c17 caller"),
        ] {
            let out = plib::tmp::Builder::new()
                .prefix("c17_a64_byref_")
                .suffix(".s")
                .tempfile()
                .expect("failed to create temp file");
            let out_path = out.path().to_string_lossy().to_string();
            let run = run_c17(&[
                "--target",
                "aarch64-unknown-linux-gnu",
                opt,
                "-S",
                "-o",
                &out_path,
                c17_side,
            ]);
            assert!(run.success, "c17 failed at {opt}:\n{}", run.stderr);
            assert_eq!(
                cross_link_and_run("a64_byref", &[&out_path, gcc_side]),
                0,
                "{what} at {opt}"
            );
        }
    }
}
