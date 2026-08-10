//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C11 `_Generic` type-generic selection (C17 6.5.1.1), audit #X3.
//

use crate::common::{compile_and_run, compile_and_run_optimized};

/// Selection, lvalue conversion, non-evaluation, and constant-expression use.
///
/// Every band returns a distinct code so a failure names itself.
#[test]
fn c11_generic_mega() {
    let code = r#"
#include <string.h>

#define typename(x) _Generic((x),                       \
    _Bool: "bool",                                      \
    char: "char",                                       \
    signed char: "schar",                               \
    unsigned char: "uchar",                             \
    short: "short",                                     \
    unsigned short: "ushort",                           \
    int: "int",                                         \
    unsigned int: "uint",                               \
    long: "long",                                       \
    unsigned long: "ulong",                             \
    long long: "llong",                                 \
    unsigned long long: "ullong",                       \
    float: "float",                                     \
    double: "double",                                   \
    long double: "ldouble",                             \
    int *: "int*",                                      \
    char *: "char*",                                    \
    void *: "void*",                                    \
    default: "other")

static int counter = 0;
static int bump(void) { counter++; return 0; }

typedef int MyInt;
typedef unsigned long MyULong;

int main(void) {
    /* ---------- 1-29: every basic type dispatches ---------- */
    _Bool b = 0;            if (strcmp(typename(b), "bool")) return 1;
    char c = 0;             if (strcmp(typename(c), "char")) return 2;
    signed char sc = 0;     if (strcmp(typename(sc), "schar")) return 3;
    unsigned char uc = 0;   if (strcmp(typename(uc), "uchar")) return 4;
    short sh = 0;           if (strcmp(typename(sh), "short")) return 5;
    unsigned short ush = 0; if (strcmp(typename(ush), "ushort")) return 6;
    int i = 0;              if (strcmp(typename(i), "int")) return 7;
    unsigned int ui = 0;    if (strcmp(typename(ui), "uint")) return 8;
    long l = 0;             if (strcmp(typename(l), "long")) return 9;
    unsigned long ul = 0;   if (strcmp(typename(ul), "ulong")) return 10;
    long long ll = 0;       if (strcmp(typename(ll), "llong")) return 11;
    unsigned long long ull = 0; if (strcmp(typename(ull), "ullong")) return 12;
    float f = 0;            if (strcmp(typename(f), "float")) return 13;
    double d = 0;           if (strcmp(typename(d), "double")) return 14;
    long double ld = 0;     if (strcmp(typename(ld), "ldouble")) return 15;
    int *ip = &i;           if (strcmp(typename(ip), "int*")) return 16;
    void *vp = &i;          if (strcmp(typename(vp), "void*")) return 17;

    /* A type with no association falls to default. */
    struct S { int x; } s = {0};
    if (strcmp(typename(s), "other")) return 18;

    /* ---------- 30-39: lvalue conversion (6.5.1.1p2) ---------- */

    /* An array decays to a pointer to its element. */
    int arr[4];
    if (strcmp(typename(arr), "int*")) return 30;

    /* A string literal is char[N], so it decays to char*. */
    if (strcmp(typename("abc"), "char*")) return 31;

    /* Top-level qualifiers are stripped, so `const int` selects `int`. */
    const int ci = 1;
    if (strcmp(typename(ci), "int")) return 32;
    volatile int vi = 1;
    if (strcmp(typename(vi), "int")) return 33;
    const volatile long cvl = 1;
    if (strcmp(typename(cvl), "long")) return 34;

    /* A function designator decays to a pointer to function, which matches
       none of the associations above. */
    if (strcmp(typename(main), "other")) return 35;

    /* ---------- 40-49: typedefs are transparent ---------- */
    MyInt mi = 0;
    if (strcmp(typename(mi), "int")) return 40;
    MyULong mul = 0;
    if (strcmp(typename(mul), "ulong")) return 41;
    /* And an association may itself be spelled with a typedef. */
    if (_Generic(i, MyInt: 1, default: 0) != 1) return 42;

    /* ---------- 50-59: the controlling expression is NOT evaluated ------ */
    counter = 0;
    if (_Generic(bump(), int: 1, default: 0) != 1) return 50;
    if (counter != 0) return 51;

    /* Nor are the unselected associations. */
    counter = 0;
    (void)_Generic(1, int: 0, default: bump());
    if (counter != 0) return 52;

    /* The selected association *is* evaluated. */
    counter = 0;
    (void)_Generic(1, int: bump(), default: 0);
    if (counter != 1) return 53;

    /* ---------- 60-69: integer constant expression ---------- */
    _Static_assert(_Generic(1, int: 1, default: 0), "_Generic must fold");
    _Static_assert(_Generic(1.0, double: 1, default: 0), "double arm");
    _Static_assert(sizeof(char[_Generic(1, int: 3, default: 1)]) == 3, "array size");

    switch (i) {
        case _Generic(1, int: 7, default: 8):
            return 60;   /* i is 0, so this must not run */
        default:
            break;
    }

    /* ---------- 70-79: nesting ---------- */
    if (_Generic(1.0, double: _Generic(2, int: 42, default: 0), default: -1) != 42) return 70;
    if (_Generic(1, int: _Generic(1.0f, float: 5, default: 0), default: -1) != 5) return 71;

    /* ---------- 80-89: the result is an lvalue-usable value ---------- */
    int chosen = _Generic(i, int: 100, default: 200);
    if (chosen != 100) return 80;

    /* Selecting a function and calling it -- the <tgmath.h> idiom. */
    if (_Generic(i, int: strlen, default: strlen)("abcd") != 4) return 81;

    /* Multi-argument dispatch via the combined type, which relies on the
       controlling expression being typed but not evaluated. */
    counter = 0;
    if (_Generic(i + bump(), int: 1, default: 0) != 1) return 82;
    if (counter != 0) return 83;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_generic_mega", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("c11_generic_mega_opt", code), 0);
}

/// `_Generic` is reported through `__has_feature` / `__has_extension`.
#[test]
fn c11_generic_has_feature() {
    let code = r#"
int main(void) {
#if !__has_feature(c_generic_selections)
    return 1;
#endif
#if !__has_extension(c_generic_selections)
    return 2;
#endif
    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_generic_has_feature", code, &[]), 0);
}
