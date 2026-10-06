//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Builtins gcc made for its own lowering that code may also call:
// __builtin_stack_save / __builtin_stack_restore, __builtin_clear_padding,
// __builtin_cexpi and __builtin_cpow.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with, compile_and_run_everywhere};

const SMALL: &str = r#"
/* Four builtins gcc provides for its own lowering, which code can also
   call: __builtin_stack_save/__builtin_stack_restore (the marks around a
   VLA's lifetime), __builtin_clear_padding (zero an object's padding
   bytes), __builtin_cexpi (cos x + i sin x) and __builtin_cpow. */
#include <string.h>
#include <complex.h>

struct padded { char c; int i; short s; long l; };

static int vla_sum(int n)
{
    int total = 0;
    for (int round = 0; round < 1000; round++) {
        void *mark = __builtin_stack_save();
        int a[n];
        for (int k = 0; k < n; k++)
            a[k] = k;
        total += a[n - 1];
        __builtin_stack_restore(mark);
    }
    return total;
}

int main(void)
{
    /* 1000 rounds of a 100000-int VLA would overflow the stack without
       the restore. */
    if (vla_sum(100000) != 1000 * 99999) return 1;

    struct padded p;
    memset(&p, 0xff, sizeof p);
    p.c = 1; p.i = 2; p.s = 3; p.l = 4;
    __builtin_clear_padding(&p);
    struct padded q;
    memset(&q, 0, sizeof q);
    q.c = 1; q.i = 2; q.s = 3; q.l = 4;
    if (memcmp(&p, &q, sizeof p) != 0) return 2;

    volatile double x = 0.5;
    double _Complex e = __builtin_cexpi(x);
    double _Complex ref = cexp(I * x);
    if (creal(e) != creal(ref) || cimag(e) != cimag(ref)) return 3;

    volatile double _Complex b = 1.0 + 2.0 * I, z = 2.0;
    double _Complex pw = __builtin_cpow(b, z);
    double _Complex pr = cpow(b, z);
    if (creal(pw) != creal(pr) || cimag(pw) != cimag(pr)) return 4;
    return 0;
}
"#;

#[test]
fn builtins_gcc_lowering_builtins() {
    for level in ["-O0", "-O2"] {
        let flags = [level.to_string(), "-lm".to_string()];
        assert_eq!(compile_and_run("gcc_lowering", SMALL, &flags), 0, "{level}");
        if let Some(rc) =
            compile_and_run_aarch64_with("gcc_lowering_a64", SMALL, &[level], &["-lm"])
        {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// Run `src` at -O0 and -O2, on the host and under qemu on aarch64, linked
/// with libm.
fn run_with_libm(name: &str, src: &str) {
    for level in ["-O0", "-O2"] {
        let flags = [level.to_string(), "-lm".to_string()];
        assert_eq!(compile_and_run(name, src, &flags), 0, "{name} {level}");
        if let Some(rc) = compile_and_run_aarch64_with(name, src, &[level], &["-lm"]) {
            assert_eq!(rc, 0, "{name} aarch64 {level}");
        }
    }
}

/// The restore is what frees an `alloca`: nothing else does before the
/// function returns, so each loop below overflows the stack without it. The
/// mark survives a `setjmp` that a `longjmp` resumes, and an inlined helper
/// brackets its own allocation.
#[test]
fn builtins_stack_save_restore_release_alloca() {
    let src = r#"
#include <setjmp.h>

static jmp_buf jb;

__attribute__((noinline)) static void bounce(void) { longjmp(jb, 1); }

static inline void scratch(int n)
{
    void *mark = __builtin_stack_save();
    volatile char *p = __builtin_alloca(n);
    p[0] = 1;
    p[n - 1] = 2;
    __builtin_stack_restore(mark);
}

int main(void)
{
    for (int i = 0; i < 100000; i++) {
        void *mark = __builtin_stack_save();
        volatile char *p = __builtin_alloca(100000);
        p[0] = 1;
        p[99999] = (char)i;
        __builtin_stack_restore(mark);
    }
    for (int i = 0; i < 100000; i++)
        scratch(100000);
    for (int i = 0; i < 10000; i++) {
        void *mark = __builtin_stack_save();
        volatile char *p = __builtin_alloca(100000);
        p[0] = 1;
        if (setjmp(jb) == 0)
            bounce();
        __builtin_stack_restore(mark);
    }
    return 0;
}
"#;
    compile_and_run_everywhere("stack_save_restore", src);
}

/// Every padding bit cleared and every member bit kept, byte for byte as
/// gcc 13 does it: gaps and tails, the unused bits beside a bit-field and an
/// unnamed bit-field, a union's bytes no member covers (a bit-field directly
/// in a union covers whole bytes), x87 `long double`'s six unused bytes, an
/// array argument (its first element only) against a pointer to the whole
/// array, a variable length array, and a `volatile` object. The argument is
/// evaluated once.
#[test]
fn builtins_clear_padding_matches_gcc() {
    let src = r#"
#include <string.h>

struct A { char c; int i; short s; long l; };
struct BF { char a; int b:3; int :4; char d; };
struct S { char a; int b; };
union V1 { struct { char a; int b; } s; struct { char x; char p; int q; } t; };
union V2 { int c:12; char d; };
struct LD { char c; long double d; };

static int check(const void *obj, unsigned n, const unsigned char *want)
{
    return memcmp(obj, want, n) == 0;
}

#define FILL(v) memset(&(v), 0xff, sizeof (v))

int main(void)
{
    struct A a; FILL(a);
    __builtin_clear_padding(&a);
    static const unsigned char want_a[24] = {
        0xff, 0, 0, 0, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0, 0, 0,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
    if (sizeof a != 24 || !check(&a, 24, want_a)) return 1;

    struct BF bf; FILL(bf);
    __builtin_clear_padding(&bf);
    static const unsigned char want_bf[4] = { 0xff, 0x07, 0xff, 0 };
    if (sizeof bf != 4 || !check(&bf, 4, want_bf)) return 2;

    union V1 v1; FILL(v1);
    __builtin_clear_padding(&v1);
    static const unsigned char want_v1[8] = { 0xff, 0xff, 0, 0, 0xff, 0xff, 0xff, 0xff };
    if (!check(&v1, 8, want_v1)) return 3;

    union V2 v2; FILL(v2);
    __builtin_clear_padding(&v2);
    static const unsigned char want_v2[4] = { 0xff, 0xff, 0, 0 };
    if (!check(&v2, 4, want_v2)) return 4;

    struct LD ld; FILL(ld);
    __builtin_clear_padding(&ld);
    unsigned char want_ld[32];
    memset(want_ld, 0, sizeof want_ld);
    want_ld[0] = 0xff;
#if defined(__x86_64__)
    memset(want_ld + 16, 0xff, 10);
#else
    memset(want_ld + 16, 0xff, 16);
#endif
    if (sizeof ld != 32 || !check(&ld, 32, want_ld)) return 5;

    static const unsigned char want_s[8] = { 0xff, 0, 0, 0, 0xff, 0xff, 0xff, 0xff };
    static const unsigned char full[8] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
    struct S arr[3]; FILL(arr);
    __builtin_clear_padding(arr);
    if (!check(&arr[0], 8, want_s) || !check(&arr[1], 8, full)) return 6;
    __builtin_clear_padding(&arr);
    for (int k = 0; k < 3; k++)
        if (!check(&arr[k], 8, want_s)) return 7;

    volatile int n = 5;
    struct S vla[n][2];
    FILL(vla);
    __builtin_clear_padding(vla);
    if (!check(&vla[0][1], 8, want_s) || !check(&vla[1][0], 8, full)) return 8;
    __builtin_clear_padding(&vla);
    for (int k = 0; k < n; k++)
        for (int j = 0; j < 2; j++)
            if (!check(&vla[k][j], 8, want_s)) return 9;

    volatile struct S vs;
    memset((void *)&vs, 0xff, sizeof vs);
    __builtin_clear_padding(&vs);
    if (!check((const void *)&vs, 8, want_s)) return 10;

    struct S row[4]; FILL(row);
    int i = 1;
    __builtin_clear_padding(&row[i++]);
    if (i != 2 || !check(&row[1], 8, want_s) || !check(&row[2], 8, full)) return 11;

    void (*fp)(void) = 0;
    __builtin_clear_padding(fp);
    return 0;
}
"#;
    compile_and_run_everywhere("clear_padding", src);
}

/// `__builtin_cexpi` in each precision is exactly `cexp(I * x)`, and
/// `__builtin_cpow` in each precision exactly the library's `cpow`.
#[test]
fn builtins_cexpi_and_cpow_every_precision() {
    let src = r#"
#include <complex.h>

int main(void)
{
    volatile double x = -1.25;
    volatile float xf = 0.75f;
    volatile long double xl = 2.5L;
    double _Complex e = __builtin_cexpi(x), r = cexp(I * x);
    if (creal(e) != creal(r) || cimag(e) != cimag(r)) return 1;
    float _Complex ef = __builtin_cexpif(xf), rf = cexpf(I * xf);
    if (crealf(ef) != crealf(rf) || cimagf(ef) != cimagf(rf)) return 2;
    long double _Complex el = __builtin_cexpil(xl), rl = cexpl(I * xl);
    if (creall(el) != creall(rl) || cimagl(el) != cimagl(rl)) return 3;
    /* An integer argument converts to the parameter type. */
    double _Complex ei = __builtin_cexpi(4);
    if (cimag(ei) != __builtin_sin(4.0)) return 4;

    volatile float _Complex bf = 1.5f - 0.5f * I, zf = 0.25f + 1.0f * I;
    float _Complex pf = __builtin_cpowf(bf, zf), qf = cpowf(bf, zf);
    if (crealf(pf) != crealf(qf) || cimagf(pf) != cimagf(qf)) return 5;
    volatile long double _Complex bl = 3.0L + 1.0L * I, zl = 0.5L;
    long double _Complex pl = __builtin_cpowl(bl, zl), ql = cpowl(bl, zl);
    if (creall(pl) != creall(ql) || cimagl(pl) != cimagl(ql)) return 6;
    /* A real exponent converts to the complex parameter. */
    double _Complex p75 = __builtin_cpow(1.0 + 0.0 * I, 75);
    if (creal(p75) != 1.0) return 7;
    return 0;
}
"#;
    run_with_libm("cexpi_cpow", src);
}
