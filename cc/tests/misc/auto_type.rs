//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__auto_type` (GNU): a declaration whose type is its initializer's.
//

use crate::common::compile_and_run_everywhere;

#[test]
fn auto_type_takes_the_initializers_type_and_evaluates_it_once() {
    compile_and_run_everywhere(
        "auto_type",
        r#"
/* `__auto_type` (GNU): the declared object takes the type of its
   initializer, lvalue-converted -- arrays and functions decay, qualifiers
   drop -- and the initializer is evaluated once. Macro libraries use it
   for type-generic min/max/swap that do not evaluate arguments twice. */
#include <string.h>

#define MAX(a, b) ({ __auto_type _a = (a); __auto_type _b = (b); _a > _b ? _a : _b; })
#define SWAP(x, y) do { __auto_type _t = (x); (x) = (y); (y) = _t; } while (0)

static int calls;
static int next(void) { return ++calls; }
static double half(void) { return 0.5; }

struct pt { int x, y; };

int main(void)
{
    /* Evaluated once. */
    calls = 0;
    if (MAX(next(), 0) != 1 || calls != 1) return 1;

    /* The type is the initializer's. */
    __auto_type d = half();
    if (sizeof d != sizeof(double) || d != 0.5) return 2;
    __auto_type l = 1L << 40;
    if (sizeof l != sizeof(long) || l != 1L << 40) return 3;
    __auto_type u = 3u;
    if (_Generic(u, unsigned: 0, default: 1)) return 4;
    __auto_type c = 'x';
    if (_Generic(c, int: 0, default: 1)) return 5; /* 'x' is an int */

    /* An array decays to a pointer, a function to a function pointer. */
    int arr[4] = {1, 2, 3, 4};
    __auto_type p = arr;
    if (sizeof p != sizeof(int *) || p[3] != 4) return 6;
    __auto_type f = next;
    calls = 10;
    if (f() != 11) return 7;

    /* Qualifiers are dropped: the object is modifiable. */
    const int ci = 5;
    __auto_type m = ci;
    m++;
    if (m != 6) return 8;

    /* Aggregates and qualifiers on the declaration itself. */
    struct pt a = {1, 2};
    __auto_type b = a;
    b.x = 9;
    if (a.x != 1 || b.x != 9) return 9;
    const __auto_type k = 7;
    if (k != 7) return 10;
    static __auto_type s = 3;
    if (s != 3) return 11;

    /* Generic macros over different types. */
    int i1 = 3, i2 = 4;
    SWAP(i1, i2);
    if (i1 != 4 || i2 != 3) return 12;
    double x1 = 1.5, x2 = 2.5;
    SWAP(x1, x2);
    if (x1 != 2.5 || MAX(x1, 0.0) != 2.5) return 13;
    char *s1 = "a", *s2 = "b";
    SWAP(s1, s2);
    if (strcmp(s1, "b")) return 14;
    return 0;
}
"#,
    );
}

#[test]
fn auto_type_in_a_for_clause_at_file_scope_and_under_typeof() {
    compile_and_run_everywhere(
        "auto_type_scopes",
        r#"
/* A `for` clause, file scope, `typeof` of the object, and the name not yet
   in scope inside its own initializer -- all as gcc reads them. */
__auto_type g = 1.5;
static const char msg[] = "hi";
__auto_type gp = msg;

int x = 40;

int main(void)
{
    if (sizeof g != sizeof(double) || g != 1.5) return 1;
    if (sizeof gp != sizeof(char *) || gp[1] != 'i') return 2;

    long total = 0;
    for (__auto_type i = 0UL; i < 4; i++)
        total += (long)i;
    if (total != 6) return 3;

    {
        /* The outer `x`: the inner one is declared after its initializer. */
        __auto_type x = &x;
        if (*x != 40) return 4;
    }

    volatile short vs = 3;
    __auto_type n = vs;
    typeof(n) m = 4;
    if (sizeof m != sizeof(short) || n + m != 7) return 5;
    if (_Generic(&n, short *: 0, default: 1)) return 6; /* volatile dropped */
    return 0;
}
"#,
    );
}
