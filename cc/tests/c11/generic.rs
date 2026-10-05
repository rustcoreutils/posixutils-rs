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

use crate::common::{compile_and_run, compile_and_run_everywhere, compile_and_run_optimized};

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

/// What `_Generic` and `__typeof__` see: feature macros, unqualified rvalues and
/// the type of a pointer conditional, as one program; each section keeps its
/// original test name and doc comment.
///
/// Consolidates: c11_generic_has_feature, c11_an_rvalue_carries_no_qualifier and
/// c11_conditional_pointer_result_type.
#[test]
fn c11_generic_types_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  2  c11_generic_has_feature
 *    11- 22  c11_an_rvalue_carries_no_qualifier
 *    31- 72  c11_conditional_pointer_result_type
 */

/* ---- c11_generic_has_feature (exit codes 1-2) ----
 *
 *  `_Generic` is reported through `__has_feature` / `__has_extension`.
 */
static int t_c11_generic_has_feature(void) {
#if !__has_feature(c_generic_selections)
    return 1;
#endif
#if !__has_extension(c_generic_selections)
    return 2;
#endif
    return 0;
}


/* ---- c11_an_rvalue_carries_no_qualifier (exit codes 11-22) ----
 *
 *  An rvalue has no qualifiers: the value of an assignment, of `++`/`--`, of
 *  a cast, of a call and of a conditional is the unqualified type (C17
 *  6.5.16p3, 6.5.4p5, 6.7.6.3p4, 6.5.15). `_Generic` lvalue-converts its
 *  operand and so cannot see it; `__typeof__` behind a pointer can. c17 typed
 *  `v = 1` for a `volatile int v` as `volatile int`.
 *
 *  Two pointer arms merge to a pointer to the composite type qualified with
 *  *both* pointees' qualifiers (6.5.15p6), or to qualified `void`: taking the
 *  first arm's type dropped `const` from `c ? p : cp` and kept it for
 *  `c ? cp : p`. The last case is the control: an lvalue keeps its qualifier.
 */
#define QUALS(e) ({ __typeof__(e) *p_ = 0; _Generic(p_, int *: 0, const int *: 1, \
    volatile int *: 2, const volatile int *: 3, default: 9); })
#define PTR(e) _Generic((e), int *: 0, const int *: 1, void *: 2, const void *: 3, \
    default: 9)
volatile int rv_v; const int rv_c = 1; int rv_x;
volatile int rv_vf(void) { return 1; }
int *rv_p; const int *rv_cp; void *rv_vp;
static int t_c11_an_rvalue_carries_no_qualifier(void) {
    if (QUALS(rv_v = 1)) return 1;
    if (QUALS(rv_v += 1)) return 2;
    if (QUALS(++rv_v)) return 3;
    if (QUALS(rv_v--)) return 4;
    if (QUALS((const int)rv_x)) return 5;
    if (QUALS(rv_vf())) return 6;
    if (QUALS(rv_x ? rv_v : rv_c)) return 7;
    if (PTR(rv_x ? rv_p : rv_cp) != 1) return 8;
    if (PTR(rv_x ? rv_cp : rv_p) != 1) return 9;
    if (PTR(rv_x ? rv_vp : rv_cp) != 3) return 10;
    if (PTR(rv_x ? rv_vp : rv_p) != 2) return 11;
    if (QUALS(rv_v) != 2) return 12;
    return 0;
}
#undef PTR
#undef QUALS


/* ---- c11_conditional_pointer_result_type (exit codes 31-72) ----
 *
 *  The type of a conditional whose arms are pointers (C17 6.5.15p6), as
 *  `_Generic`, `sizeof` and pointer arithmetic see it. Every expected type is
 *  gcc's.
 *
 *  A null pointer constant takes the other arm's type even spelled
 *  `(void *)0`; compatible pointees merge to the composite type, so the
 *  result knows the array extent and the prototype one arm supplied;
 *  incompatible pointees give `void *`, and a pointer beside a nonzero
 *  integer stays a pointer.
 */
#define T(x) _Generic((x), int *: 1, const int *: 2, const volatile int *: 3, \
    void *: 4, const void *: 5, int (*)[3]: 6, const int (*)[3]: 7, \
    int (*)(void): 8, char *: 9, default: 0)
int c = 1;
int a[3] = {10, 20, 30};
int *p = a; char *cp; const int *cip; volatile int *vip; void *vp;
int (*a3)[3] = &a; int (*ap)[] = &a; const int (*cap)[];
int f(void) { return 42; }
int (*fp)(void) = f; int (*fnp)() = f;
static int t_c11_conditional_pointer_result_type(void) {
    if (T(c ? p : (void *)0) != 1) return 1;
    if (T(c ? (void *)0 : p) != 1) return 2;
    if (T(c ? cip : (void *)0) != 2) return 3;
    if (T(c ? fp : (void *)0) != 8) return 4;
    if (T(c ? p : 0) != 1) return 5;
    if (T(c ? p : (const void *)0) != 5) return 6;
    if (T(c ? p : (char *)0) != 4) return 7;
    if (T(c ? cp : p) != 4) return 8;
    if (T(c ? p : 1) != 1) return 9;
    if (T(c ? cip : vip) != 3) return 10;
    if (T(c ? vip : cip) != 3) return 11;
    if (T(c ? cip : vp) != 5) return 12;
    if (T(c ? ap : a3) != 6) return 13;
    if (T(c ? cap : a3) != 7) return 14;
    if (T(c ? fnp : fp) != 8) return 15;
    if (T(c ? fp : fnp) != 8) return 16;
    /* The composite type is complete where one arm was. */
    if (sizeof *(c ? ap : a3) != 3 * sizeof(int)) return 17;
    if ((c ? ap : a3)[0][2] != 30) return 18;
    if (*((c ? p : (void *)0) + 1) != 20) return 19;
    if ((c ? fp : (void *)0)() != 42) return 20;
    if ((c ? fnp : fp)() != 42) return 21;
    /* GNU `a ?: b` follows the same rules. */
    if (T(p ?: (void *)0) != 1) return 22;
    if (T(p ?: (char *)0) != 4) return 23;
    return 0;
}
#undef T

int main(void)
{
    int r;
    if ((r = t_c11_generic_has_feature()) != 0)
        return 0 + r;
    if ((r = t_c11_an_rvalue_carries_no_qualifier()) != 0)
        return 10 + r;
    if ((r = t_c11_conditional_pointer_result_type()) != 0)
        return 30 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_generic_types_mega", code, &[]), 0);
}

/// An enumeration is compatible with, and computes as, its integer type, on
/// every level and target; each section keeps its original test name and doc
/// comment.
///
/// Consolidates: c11_generic_enum_is_compatible_with_its_integer_type and
/// c11_enum_computes_as_its_integer_type.
#[test]
fn c11_enum_types_everywhere_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  6  c11_generic_enum_is_compatible_with_its_integer_type
 *    11- 26  c11_enum_computes_as_its_integer_type
 */

/* ---- c11_generic_enum_is_compatible_with_its_integer_type (exit codes 1-6) ----
 *
 *  Each enumerated type is compatible with one integer type (C17 6.7.2.2p4),
 *  the one the implementation chooses to represent it -- for gcc, `unsigned
 *  int` when no enumerator is negative and `int` otherwise. c17 made an enum
 *  compatible with no integer type, so `_Generic` fell to `default` and
 *  `__builtin_types_compatible_p` answered 0 where gcc answers 1.
 */
enum E { A, B };           /* gcc: compatible with unsigned int */
enum F { M = -1, N };      /* gcc: compatible with int */
static int t_c11_generic_enum_is_compatible_with_its_integer_type(void) {
    if (__builtin_types_compatible_p(enum E, unsigned) != 1) return 1;
    if (__builtin_types_compatible_p(enum E, int) != 0) return 2;
    if (__builtin_types_compatible_p(enum F, int) != 1) return 3;
    if (_Generic((enum E)0, unsigned: 1, int: 2, default: 3) != 1) return 4;
    if (_Generic((enum F)0, unsigned: 1, int: 2, default: 3) != 2) return 5;
    if (_Generic(0u, enum E: 1, default: 2) != 1) return 6;
    return 0;
}


/* ---- c11_enum_computes_as_its_integer_type (exit codes 11-26) ----
 *
 *  The integer type gcc picks for each range, and that the choice is what an
 *  enum *computes* as: `e + 1` has the enum's integer type, as for gcc, where
 *  c17 kept the enum type itself and compared a signed 64-bit enum against
 *  `unsigned` as though it ranked with `int`. Two enumerated types stay
 *  incompatible with each other whatever their integer types.
 */
#define T(x) _Generic((x), int: 1, unsigned: 2, long: 3, unsigned long: 4, \
                      long long: 5, unsigned long long: 6, default: 0)
enum U { U0, U1 };                        /* unsigned int */
enum S { S0 = -1, S1 };                   /* int */
enum UB { UB0 = 0x80000000u };            /* unsigned int */
enum UL { UL0 = 0x100000000 };            /* unsigned long */
enum SL { SL0 = -1, SL1 = 0x80000000 };   /* long */
static int t_c11_enum_computes_as_its_integer_type(void) {
    enum U u = U1; enum S s = S1; enum UL ul = UL0; enum SL sl = SL1;
    if (T(u) != 2 || T((enum UB)0) != 2 || T(sl) != 3) return 1;
    if (T(u + 1) != 2 || T(s + 1u) != 2 || T(u + s) != 2) return 2;
    if (T(ul + 1L) != 4 || T(sl + 1u) != 3 || T(ul + sl) != 4) return 3;
    if (T(sl + 1LL) != 5 || T(ul + 1LL) != 6) return 4;
    if (!(u - 2 > 0) || !(sl + 0u < 0x80000001L)) return 5;
    if (!__builtin_types_compatible_p(enum UB, unsigned)) return 6;
    if (!__builtin_types_compatible_p(enum UL, unsigned long)) return 7;
    if (__builtin_types_compatible_p(enum UL, unsigned long long)) return 8;
    if (!__builtin_types_compatible_p(enum SL, long)) return 9;
    if (__builtin_types_compatible_p(enum SL, unsigned long)) return 10;
    if (__builtin_types_compatible_p(enum U, enum UB)) return 11;
    if (!__builtin_types_compatible_p(const enum U, unsigned)) return 12;
    if (!__builtin_types_compatible_p(enum U *, unsigned *)) return 13;
    if (__builtin_types_compatible_p(enum U *, const unsigned *)) return 14;
    if (_Generic((enum S)0, enum U: 1, enum S: 2, default: 3) != 2) return 15;
    if (_Generic(0L, enum SL: 1, default: 2) != 1) return 16;
    return 0;
}
#undef T

int main(void)
{
    int r;
    if ((r = t_c11_generic_enum_is_compatible_with_its_integer_type()) != 0)
        return 0 + r;
    if ((r = t_c11_enum_computes_as_its_integer_type()) != 0)
        return 10 + r;
    return 0;
}
"#;
    compile_and_run_everywhere("c11_enum_types_everywhere_mega", code);
}
