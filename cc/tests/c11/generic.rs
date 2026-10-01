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

/// An rvalue has no qualifiers: the value of an assignment, of `++`/`--`, of
/// a cast, of a call and of a conditional is the unqualified type (C17
/// 6.5.16p3, 6.5.4p5, 6.7.6.3p4, 6.5.15). `_Generic` lvalue-converts its
/// operand and so cannot see it; `__typeof__` behind a pointer can. c17 typed
/// `v = 1` for a `volatile int v` as `volatile int`.
///
/// Two pointer arms merge to a pointer to the composite type qualified with
/// *both* pointees' qualifiers (6.5.15p6), or to qualified `void`: taking the
/// first arm's type dropped `const` from `c ? p : cp` and kept it for
/// `c ? cp : p`. The last case is the control: an lvalue keeps its qualifier.
#[test]
fn c11_an_rvalue_carries_no_qualifier() {
    let code = r#"
#define QUALS(e) ({ __typeof__(e) *p_ = 0; _Generic(p_, int *: 0, const int *: 1, \
    volatile int *: 2, const volatile int *: 3, default: 9); })
#define PTR(e) _Generic((e), int *: 0, const int *: 1, void *: 2, const void *: 3, \
    default: 9)
volatile int v; const int c = 1; int x;
volatile int vf(void) { return 1; }
int *p; const int *cp; void *vp;
int main(void) {
    if (QUALS(v = 1)) return 1;
    if (QUALS(v += 1)) return 2;
    if (QUALS(++v)) return 3;
    if (QUALS(v--)) return 4;
    if (QUALS((const int)x)) return 5;
    if (QUALS(vf())) return 6;
    if (QUALS(x ? v : c)) return 7;
    if (PTR(x ? p : cp) != 1) return 8;
    if (PTR(x ? cp : p) != 1) return 9;
    if (PTR(x ? vp : cp) != 3) return 10;
    if (PTR(x ? vp : p) != 2) return 11;
    if (QUALS(v) != 2) return 12;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_rvalue_unqualified", code, &[]), 0);
}

/// Each enumerated type is compatible with one integer type (C17 6.7.2.2p4),
/// the one the implementation chooses to represent it -- for gcc, `unsigned
/// int` when no enumerator is negative and `int` otherwise. c17 made an enum
/// compatible with no integer type, so `_Generic` fell to `default` and
/// `__builtin_types_compatible_p` answered 0 where gcc answers 1.
#[test]
fn c11_generic_enum_is_compatible_with_its_integer_type() {
    crate::common::compile_and_run_everywhere(
        "generic_enum_compat",
        r#"
enum E { A, B };           /* gcc: compatible with unsigned int */
enum F { M = -1, N };      /* gcc: compatible with int */
int main(void) {
    if (__builtin_types_compatible_p(enum E, unsigned) != 1) return 1;
    if (__builtin_types_compatible_p(enum E, int) != 0) return 2;
    if (__builtin_types_compatible_p(enum F, int) != 1) return 3;
    if (_Generic((enum E)0, unsigned: 1, int: 2, default: 3) != 1) return 4;
    if (_Generic((enum F)0, unsigned: 1, int: 2, default: 3) != 2) return 5;
    if (_Generic(0u, enum E: 1, default: 2) != 1) return 6;
    return 0;
}
"#,
    );
}

/// The integer type gcc picks for each range, and that the choice is what an
/// enum *computes* as: `e + 1` has the enum's integer type, as for gcc, where
/// c17 kept the enum type itself and compared a signed 64-bit enum against
/// `unsigned` as though it ranked with `int`. Two enumerated types stay
/// incompatible with each other whatever their integer types.
#[test]
fn c11_enum_computes_as_its_integer_type() {
    crate::common::compile_and_run_everywhere(
        "enum_integer_type",
        r#"
#define T(x) _Generic((x), int: 1, unsigned: 2, long: 3, unsigned long: 4, \
                      long long: 5, unsigned long long: 6, default: 0)
enum U { U0, U1 };                        /* unsigned int */
enum S { S0 = -1, S1 };                   /* int */
enum UB { UB0 = 0x80000000u };            /* unsigned int */
enum UL { UL0 = 0x100000000 };            /* unsigned long */
enum SL { SL0 = -1, SL1 = 0x80000000 };   /* long */
int main(void) {
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
"#,
    );
}

/// A pointer to an enum and a pointer to its integer type point at compatible
/// types (C17 6.7.6.1p2), so assigning one to the other is silent in gcc.
#[test]
fn c11_enum_pointer_to_its_integer_type_is_silent() {
    crate::common::compile_expect_no_diagnostic(
        "enum_ptr_same_int",
        r#"
enum E { A, B };
enum S { M = -1 };
int main(void) {
    enum E e = A; enum S s = M;
    unsigned *pu = &e;
    enum E *pe = pu;
    int *ps = &s;
    enum S *pes = ps;
    return *pe + *pes;
}
"#,
        "pointer",
    );
}

/// The signedness has to agree: `enum E` is `unsigned int`, so an `int *`
/// does not point at a compatible type, and gcc warns.
#[test]
fn c11_enum_pointer_to_the_other_signedness_warns() {
    crate::common::compile_expect_warning(
        "enum_ptr_other_int",
        "enum E { A, B };\nint main(void) { enum E e = A; int *p = &e; return *p; }\n",
        "incompatible pointer type",
    );
    crate::common::compile_expect_warning(
        "enum_ptr_other_uint",
        "enum S { M = -1 };\nint main(void) { enum S s = M; unsigned *p = &s; return *p; }\n",
        "incompatible pointer type",
    );
}

/// Compatible types make a compatible redeclaration (C17 6.2.7), which gcc
/// accepts -- warning only under `-Wenum-int-mismatch`, part of `-Wall`.
#[test]
fn c11_enum_redeclared_as_its_integer_type() {
    crate::common::compile_expect_no_diagnostic(
        "enum_redecl_int",
        r#"
enum E { E0, E1 };
enum S { S0 = -1 };
enum E f(void);
unsigned f(void) { return E1; }
int g(enum S);
int g(int x) { return x; }
extern enum E v;
unsigned v = E1;
int main(void) { return f() + g(S0) + (int)v - 1 != 0; }
"#,
        "conflicting",
    );
}

/// A typedef may be redefined only as the *same* type (C17 6.7p3), and
/// neither an enum and its integer type nor `int` and `const int` are that.
#[test]
fn c11_typedef_redefinition_needs_the_same_type() {
    crate::common::compile_expect_error(
        "typedef_enum_vs_int",
        "enum E { A };\ntypedef enum E T;\ntypedef unsigned T;\n",
        "typedef 'T' redefined with a different type ('enum E' then 'unsigned int')",
    );
    crate::common::compile_expect_error(
        "typedef_const_int",
        "typedef int T;\ntypedef const int T;\n",
        "typedef 'T' redefined with a different type ('int' then 'const int')",
    );
    crate::common::compile_expect_ok(
        "typedef_enum_repeated",
        "enum E { A };\ntypedef enum E T;\ntypedef enum E T;\ntypedef const int C;\ntypedef const int C;\n",
    );
}

/// An enum and its integer type are compatible, so they cannot both be
/// `_Generic` associations (C17 6.5.1.1p2).
#[test]
fn c11_generic_rejects_an_enum_beside_its_integer_type() {
    crate::common::compile_expect_error(
        "generic_enum_dup",
        "enum E { A };\nint main(void) { return _Generic(0u, enum E: 0, unsigned: 1, default: 2); }\n",
        "two associations with compatible type",
    );
}
