//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((cleanup(fn)))` (GNU): `fn(&var)` runs when `var` goes out
// of scope -- systemd's `_cleanup_*` and glib's `g_autoptr`/`g_autofree`
// are built on it. Every way out of a scope is exercised, and the order is
// gcc's: innermost scope first, reverse declaration order, after a returned
// value is computed, and never on a computed goto.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

const CLEANUP: &str = r#"
#include <stdlib.h>
#include <string.h>

/* Every cleanup appends the value it was handed to a log, so each check
   compares the log against the order gcc runs them in. */
static int log_[64], n;
static void c(int *p) { log_[n++] = *p; }

#define LOGGED(...)                                                            \
    ({                                                                         \
        static const int want_[] = {__VA_ARGS__};                              \
        n == (int)(sizeof want_ / sizeof want_[0]) &&                          \
            memcmp(log_, want_, sizeof want_) == 0;                            \
    })

/* Reverse declaration order at the end of a block, inner block first. */
static void t_block(void)
{
    int a __attribute__((cleanup(c))) = 1;
    {
        int b __attribute__((cleanup(c))) = 2;
        int d __attribute__((cleanup(c))) = 3;
    }
    int e __attribute__((cleanup(c))) = 4;
}

/* `return` runs every enclosing cleanup, innermost first, after the
   returned value is computed: the cleanup sees the post-increment. */
static int t_return(int x)
{
    int a __attribute__((cleanup(c))) = 10;
    {
        int b __attribute__((cleanup(c))) = 11;
        if (x)
            return b++;
    }
    return 0;
}

/* A returned aggregate is copied before its own cleanup scrubs it. */
struct S { int v[4]; };
static void cs(struct S *s) { log_[n++] = s->v[3]; memset(s, 0, sizeof *s); }
static struct S t_return_struct(void)
{
    struct S s __attribute__((cleanup(cs))) = {{1, 2, 3, 4}};
    return s;
}

/* break and continue leave the loop body's scope each iteration. */
static void t_loops(void)
{
    for (int i = 0; i < 4; i++) {
        int v __attribute__((cleanup(c))) = 20 + i;
        if (i == 1)
            continue;
        if (i == 2)
            break;
    }
    int k = 0;
    while (1) {
        int w __attribute__((cleanup(c))) = 30 + k;
        if (++k == 2)
            break;
    }
}

/* A switch's break leaves the scope of a variable declared in its body. */
static void t_switch(int x)
{
    switch (x) {
    case 1: {
        int v __attribute__((cleanup(c))) = 40;
        break;
    }
    default:
        break;
    }
}

/* goto out of nested scopes, forward and backward. */
static void t_goto(void)
{
    int round = 0;
again:
    {
        int a __attribute__((cleanup(c))) = 50 + round;
        {
            int b __attribute__((cleanup(c))) = 60 + round;
            if (round++ == 0)
                goto again;
            goto out;
        }
    }
out:
    return;
}

/* A statement expression's value is computed before its cleanups run. */
static int t_stmt_expr(void)
{
    return ({
        int v __attribute__((cleanup(c))) = 70;
        v + 1;
    });
}

/* A VLA in the same scope as a cleanup variable. */
static int t_vla(int len)
{
    int sum = 0;
    {
        int v __attribute__((cleanup(c))) = 80;
        int a[len];
        for (int i = 0; i < len; i++)
            a[i] = i;
        for (int i = 0; i < len; i++)
            sum += a[i];
    }
    return sum;
}

/* glib's g_autofree shape: the cleanup frees the pointer. */
static int freed;
static void autofree(void *p) { free(*(void **)p); freed++; }
static int t_autofree(int fail)
{
    char *buf __attribute__((cleanup(autofree))) = malloc(16);
    if (fail)
        return -1;
    strcpy(buf, "ok");
    return buf[0] == 'o' ? 0 : 1;
}

/* Attribute spellings and placements; several declarators in one
   declaration run in reverse. */
static void t_spellings(void)
{
    __attribute__((cleanup(c))) int a = 90;
    int __attribute__((__cleanup__(c))) b = 91;
    int d __attribute__((cleanup(c))) = 92, e __attribute__((cleanup(c))) = 93;
}

/* gcc runs no cleanup on a computed goto. */
static void t_computed_goto(void)
{
    static void *t[] = {&&out};
    {
        int v __attribute__((cleanup(c))) = 99;
        goto *t[0];
    }
out:
    return;
}

/* A `for` declaration's cleanup runs once, when the loop ends. */
static void t_for_decl(void)
{
    for (int i __attribute__((cleanup(c))) = 0; i < 3; i++)
        ;
}

int main(void)
{
    n = 0; t_block();
    if (!LOGGED(3, 2, 4, 1)) return 1;
    n = 0;
    if (t_return(1) != 11 || !LOGGED(12, 10)) return 2;
    n = 0;
    if (t_return(0) != 0 || !LOGGED(11, 10)) return 3;
    n = 0;
    struct S s = t_return_struct();
    if (s.v[0] != 1 || s.v[3] != 4 || !LOGGED(4)) return 4;
    n = 0; t_loops();
    if (!LOGGED(20, 21, 22, 30, 31)) return 5;
    n = 0; t_switch(1);
    if (!LOGGED(40)) return 6;
    n = 0; t_goto();
    if (!LOGGED(60, 50, 61, 51)) return 7;
    n = 0;
    if (t_stmt_expr() != 71 || !LOGGED(70)) return 8;
    n = 0;
    if (t_vla(5) != 10 || !LOGGED(80)) return 9;
    freed = 0;
    if (t_autofree(0) != 0 || t_autofree(1) != -1 || freed != 2) return 10;
    n = 0; t_spellings();
    if (!LOGGED(93, 92, 91, 90)) return 11;
    n = 0; t_computed_goto();
    if (n != 0) return 12;
    n = 0; t_for_decl();
    if (!LOGGED(3)) return 13;
    return 0;
}
"#;

#[test]
fn cleanup_attribute_runs_on_every_scope_exit_in_gcc_order() {
    for level in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("cleanup_attr", CLEANUP, &[level.to_string()]),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with("cleanup_attr_a64", CLEANUP, &[level], &[]) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// A returned value is computed before the cleanups run, and a cleanup that
/// scrubs the variable being returned does not reach the caller's copy --
/// whichever way the ABI returns it: in general registers, in SSE or FP
/// registers, on the x87 stack, as an HFA, through the hidden pointer, or by
/// address within the compiler (a complex number, a vector, a wide struct).
/// A statement expression's value is kept the same way.
const CLEANUP_RETURNS: &str = r#"
#include <string.h>

#define SCRUB(T) static void scrub_##T(T *p) { memset(p, 0, sizeof *p); }
#define RET(T, ...)                                                            \
    static T r_##T(void)                                                       \
    {                                                                          \
        T v __attribute__((cleanup(scrub_##T))) = __VA_ARGS__;                 \
        return v;                                                              \
    }

typedef struct { char c[3]; } S3;
typedef struct { int a, b; } S8;
typedef struct { long a, b; } S16;
typedef struct { double a, b; } D16;
typedef struct { float a, b, c; } F12;
typedef struct { double a, b, c, d; } D32;
typedef struct { long double x; } LD;
typedef struct { long a[5]; } Big;
typedef long double ldbl;
typedef double _Complex dcx;
typedef float _Complex fcx;
typedef int v4 __attribute__((vector_size(16)));

SCRUB(S3) SCRUB(S8) SCRUB(S16) SCRUB(D16) SCRUB(F12) SCRUB(D32) SCRUB(LD)
SCRUB(Big) SCRUB(ldbl) SCRUB(dcx) SCRUB(fcx) SCRUB(float) SCRUB(double)
SCRUB(v4)

RET(S3, {{1, 2, 3}})
RET(S8, {4, 5})
RET(S16, {6, 7})
RET(D16, {1.5, 2.5})
RET(F12, {1, 2, 3})
RET(D32, {1, 2, 3, 4})
RET(LD, {9.5L})
RET(Big, {{1, 2, 3, 4, 5}})
RET(ldbl, 3.25L)
RET(dcx, 1.0 + 2.0i)
RET(fcx, 3.0f + 4.0fi)
RET(float, 7.5f)
RET(double, 8.5)
RET(v4, {1, 2, 3, 4})

int main(void)
{
    S3 a = r_S3();
    if (a.c[0] != 1 || a.c[2] != 3) return 1;
    S8 b = r_S8();
    if (b.a != 4 || b.b != 5) return 2;
    S16 c = r_S16();
    if (c.a != 6 || c.b != 7) return 3;
    D16 d = r_D16();
    if (d.a != 1.5 || d.b != 2.5) return 4;
    F12 e = r_F12();
    if (e.a != 1 || e.c != 3) return 5;
    D32 f = r_D32();
    if (f.a != 1 || f.d != 4) return 6;
    LD g = r_LD();
    if (g.x != 9.5L) return 7;
    Big h = r_Big();
    if (h.a[0] != 1 || h.a[4] != 5) return 8;
    if (r_ldbl() != 3.25L) return 9;
    dcx i = r_dcx();
    if (__real__ i != 1.0 || __imag__ i != 2.0) return 10;
    fcx j = r_fcx();
    if (__real__ j != 3.0f || __imag__ j != 4.0f) return 11;
    if (r_float() != 7.5f || r_double() != 8.5) return 12;
    v4 k = r_v4();
    if (k[0] != 1 || k[3] != 4) return 13;
    Big l = ({ Big v __attribute__((cleanup(scrub_Big))) = {{6, 0, 0, 0, 7}}; v; });
    if (l.a[0] != 6 || l.a[4] != 7) return 14;
    v4 m = ({ v4 v __attribute__((cleanup(scrub_v4))) = {5, 6, 7, 8}; v; });
    if (m[0] != 5 || m[3] != 8) return 15;
    return 0;
}
"#;

#[test]
fn cleanup_attribute_leaves_the_returned_value_alone() {
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("cleanup_ret", CLEANUP_RETURNS, &[level.to_string()]),
            0,
            "{level}"
        );
        if let Some(rc) =
            compile_and_run_aarch64_with("cleanup_ret_a64", CLEANUP_RETURNS, &[level], &[])
        {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}
