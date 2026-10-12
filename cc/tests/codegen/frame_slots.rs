//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Frame slots shared between spilled values: each program keeps values in
// the frame that are live together, apart, and across control flow the
// interference graph cannot see, and checks every one of them. Run on both
// targets at several levels.
//

use crate::common::compile_and_run_everywhere;

/// Values live across a `setjmp` and read only on its second return keep
/// their slots: the values the first return computes before the `longjmp`
/// -- spilled too, and dead-disjoint from them along every CFG path -- may
/// not take those slots, because the path back from the `longjmp` is no
/// edge the liveness sees.
#[test]
fn spilled_values_survive_longjmp() {
    let src = r#"
#include <setjmp.h>

static jmp_buf jb;
static double sink;

__attribute__((noinline)) static void jump(void) { longjmp(jb, 1); }
__attribute__((noinline)) static double id(double x) { return x; }
__attribute__((noinline)) static long lid(long x) { return x; }

/* `a`..`d` are live across the setjmp and read only on its second return.
   On the first return they are dead, and the values computed there --
   each spilled, live across calls -- are what a slot shared by liveness
   alone would hand their slots to. */
__attribute__((noinline)) static int across(long n) {
    long a = lid(n) * 3, b = lid(n) + 7;
    double c = id((double)n) * 1.5, d = id((double)n) - 2.0;
    if (setjmp(jb)) {
        if (a != n * 3) return 1;
        if (b != n + 7) return 2;
        if (c != n * 1.5) return 3;
        if (d != n - 2.0) return 4;
        return 0;
    }
    double e = id(n * 9.0), f = id(e + 1.0), g = id(f * 2.0), h = id(g - e);
    long i = lid(n * 11), j = lid(i + 5), k = lid(j * 2), l = lid(k - i);
    long m = lid(l + 1), o = lid(m + 1), p = lid(o + 1), q = lid(p + 1);
    sink = e + f + g + h + (double)(i + j + k + l + m + o + p + q);
    jump();
    return 5;
}

int main(void) {
    for (long n = 1; n < 4; n++) {
        int r = across(n);
        if (r) return r;
    }
    return 0;
}
"#;
    compile_and_run_everywhere("spilled_values_survive_longjmp", src);
}

/// Two spilled values swapped through phis every iteration, as `double`s
/// and as `__int128`s: the copies that implement the swap may not land
/// one value on the other's slot before it is read.
#[test]
fn spilled_phi_swap() {
    let src = r#"
__attribute__((noinline)) static double id(double x) { return x; }
__attribute__((noinline)) static __int128 qid(__int128 x) { return x; }

/* `x` and `y` are swapped every iteration -- a phi of each other -- and
   each is live across a call, so both live in frame slots. */
__attribute__((noinline)) static int swap_fp(long n) {
    double x = id(1.0), y = id(2.0);
    for (long i = 0; i < n; i++) {
        double t = x;
        x = y;
        y = t;
        id(x + y);
    }
    double want_x = n % 2 ? 2.0 : 1.0, want_y = n % 2 ? 1.0 : 2.0;
    return x == want_x && y == want_y ? 0 : 1;
}

/* The same with 128-bit integers, which always live in the frame. */
__attribute__((noinline)) static int swap_int128(long n) {
    __int128 x = qid((__int128)1 << 100), y = qid(((__int128)3 << 90) + 7);
    for (long i = 0; i < n; i++) {
        __int128 t = x;
        x = y;
        y = t;
        qid(x ^ y);
    }
    __int128 one = (__int128)1 << 100, two = ((__int128)3 << 90) + 7;
    __int128 want_x = n % 2 ? two : one, want_y = n % 2 ? one : two;
    return x == want_x && y == want_y ? 0 : 2;
}

int main(void) {
    for (long n = 0; n < 5; n++) {
        int r = swap_fp(n);
        if (r) return r;
        r = swap_int128(n);
        if (r) return r;
    }
    return 0;
}
"#;
    compile_and_run_everywhere("spilled_phi_swap", src);
}

/// Spilled values read at labels reached only by computed gotos.
#[test]
fn spilled_values_across_computed_goto() {
    let src = r#"
__attribute__((noinline)) static double id(double x) { return x; }

/* A threaded interpreter in miniature: each label reads values that live
   across calls -- frame slots -- and every dispatch is a computed goto, so
   any label may follow any other. */
__attribute__((noinline)) static double run(const unsigned char *code) {
    static void *const ops[] = {&&op_add, &&op_mul, &&op_sub, &&op_end};
    double acc = id(1.0), a = id(2.0), m = id(3.0), s = id(0.5);
    goto *ops[*code++];
op_add: {
        double t = id(acc + a);
        acc = t;
        goto *ops[*code++];
    }
op_mul: {
        double t = id(acc * m);
        acc = t;
        goto *ops[*code++];
    }
op_sub: {
        double t = id(acc - s);
        acc = t;
        goto *ops[*code++];
    }
op_end:
    return acc + a + m + s;
}

int main(void) {
    static const unsigned char prog[] = {0, 1, 2, 0, 1, 2, 3};
    /* ((1+2)*3-0.5+2)*3-0.5 = 31, plus 2+3+0.5 */
    return run(prog) == 36.5 ? 0 : 1;
}
"#;
    compile_and_run_everywhere("spilled_values_across_computed_goto", src);
}

/// 128-bit integers and long doubles in the frame, live together and apart:
/// sixteen-byte slots shared between values whose lives do not meet, never
/// between two that do.
#[test]
fn spilled_int128_and_long_double_side_by_side() {
    let src = r#"
__attribute__((noinline)) static long lid(long x) { return x; }
__attribute__((noinline)) static long double did(long double x) { return x; }
__attribute__((noinline)) static __int128 qid(__int128 x) { return x; }

/* 128-bit integers and long doubles live in the frame, live together and
   apart, with values whose lives end between them so their slots could be
   shared. Every value is checked at the end. */
__attribute__((noinline)) static int wide(long n) {
    __int128 a = qid((__int128)lid(n) << 70);
    long double b = did((long double)n * 1.25L);
    __int128 c = qid(a + lid(3));
    long double d = did(b * 3);
    int r = 0;
    {
        __int128 t = qid(c - a);         /* 3, then dead */
        long double u = did(d - b);      /* 2.5n, then dead */
        if (t != 3) r |= 1;
        if (u != (long double)n * 2.5L) r |= 2;
    }
    __int128 e = qid(a >> 70);           /* n */
    long double f = did(b + d);          /* 5n */
    if (a != (__int128)n << 70) r |= 4;
    if (b != (long double)n * 1.25L) r |= 8;
    if (c != ((__int128)n << 70) + 3) r |= 16;
    if (d != (long double)n * 3.75L) r |= 32;
    if (e != n) r |= 64;
    if (f != (long double)n * 5) r |= 128;
    return r;
}

int main(void) {
    for (long n = 1; n < 5; n++) {
        int r = wide(n);
        if (r) return r;
    }
    return 0;
}
"#;
    compile_and_run_everywhere("spilled_int128_and_long_double_side_by_side", src);
}
