//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Types, Keywords, and Qualifiers Mega-Test
//
// Covers Sections 1-3 of C99 checklist:
// - All type specifier synonyms (signed short int, etc.)
// - Storage class specifiers (auto, extern, register, static)
// - Type qualifiers (const, volatile, restrict) and combinations
// - Function specifiers (inline, static inline, extern inline)
// - _Bool type and stdbool.h
// - Type compatibility and derived types
//

use crate::common::compile_and_run;

#[test]
fn c99_types_keywords_mega() {
    let code = r#"
#include <stdbool.h>
#include <stddef.h>

// === Storage class: static at file scope (internal linkage) ===
static int file_static_var = 100;

// === Storage class: extern ===
extern int extern_func(void);
int extern_func(void) { return 77; }

// === Function specifiers ===
static inline int inline_add(int a, int b) { return a + b; }

// C99 6.7.4p6: an `inline` definition with no `extern` in any declaration
// provides no external definition, so calling it is only well-defined if
// every call is inlined. The `extern` declaration is what makes this one an
// external definition, and it is the spelling the header comment above claims
// to cover. Without it this file links only at -O and above -- under gcc too.
extern int inline_mul(int a, int b);
inline int inline_mul(int a, int b) { return a * b; }

// Variadic for default promotion test
#include <stdarg.h>
int sum_va(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int s = 0;
    for (int i = 0; i < n; i++) s += va_arg(ap, int);
    va_end(ap);
    return s;
}

int main(void) {
    // ========== TYPE SPECIFIER SYNONYMS (returns 1-19) ==========
    {
        // All integer type synonyms — verify sizeof for each group
        // short synonyms
        short s1 = 1;
        short int s2 = 2;
        signed short s3 = 3;
        signed short int s4 = 4;
        if (sizeof(s1) != sizeof(s2) || sizeof(s2) != sizeof(s3) || sizeof(s3) != sizeof(s4))
            return 1;

        // unsigned short synonyms
        unsigned short us1 = 1;
        unsigned short int us2 = 2;
        if (sizeof(us1) != sizeof(us2)) return 2;

        // int synonyms
        int i1 = 1;
        signed i2 = 2;
        signed int i3 = 3;
        if (sizeof(i1) != sizeof(i2) || sizeof(i2) != sizeof(i3)) return 3;

        // unsigned synonyms
        unsigned u1 = 1;
        unsigned int u2 = 2;
        if (sizeof(u1) != sizeof(u2)) return 4;

        // long synonyms
        long l1 = 1;
        long int l2 = 2;
        signed long l3 = 3;
        signed long int l4 = 4;
        if (sizeof(l1) != sizeof(l2) || sizeof(l2) != sizeof(l3) || sizeof(l3) != sizeof(l4))
            return 5;

        // unsigned long synonyms
        unsigned long ul1 = 1;
        unsigned long int ul2 = 2;
        if (sizeof(ul1) != sizeof(ul2)) return 6;

        // long long synonyms (C99)
        long long ll1 = 1;
        long long int ll2 = 2;
        signed long long ll3 = 3;
        signed long long int ll4 = 4;
        if (sizeof(ll1) != sizeof(ll2) || sizeof(ll2) != sizeof(ll3) || sizeof(ll3) != sizeof(ll4))
            return 7;

        // unsigned long long synonyms (C99)
        unsigned long long ull1 = 1;
        unsigned long long int ull2 = 2;
        if (sizeof(ull1) != sizeof(ull2)) return 8;

        // signed char vs unsigned char vs char
        char c1 = 'A';
        signed char sc = 'B';
        unsigned char uc = 'C';
        if (sizeof(c1) != 1 || sizeof(sc) != 1 || sizeof(uc) != 1) return 9;

        // void (as pointer base)
        void *vp = &c1;
        if (vp == 0) return 10;

        // float, double, long double
        float f = 1.0f;
        double d = 2.0;
        long double ld = 3.0L;
        if (sizeof(f) != 4) return 11;
        if (sizeof(d) != 8) return 12;
        if (sizeof(ld) < 8) return 13;  // at least 8, usually 16
    }

    // ========== _BOOL / STDBOOL.H (returns 20-29) ==========
    {
        // _Bool type
        _Bool b1 = 1;
        _Bool b2 = 0;
        if (b1 != 1 || b2 != 0) return 20;

        // Implicit conversion: any scalar -> _Bool (0 or 1)
        _Bool b3 = 42;    // non-zero -> 1
        _Bool b4 = -1;    // non-zero -> 1
        _Bool b5 = 0;     // zero -> 0
        if (b3 != 1 || b4 != 1 || b5 != 0) return 21;

        // stdbool.h: bool, true, false
        bool bt = true;
        bool bf = false;
        if (bt != 1 || bf != 0) return 22;

        // sizeof
        if (sizeof(_Bool) != 1) return 23;
    }

    // ========== STORAGE CLASS SPECIFIERS (returns 30-39) ==========
    {
        // auto (block scope default)
        auto int a = 10;
        if (a != 10) return 30;

        // register hint
        register int r = 42;
        if (r != 42) return 31;

        // static at file scope (tested via file_static_var above)
        if (file_static_var != 100) return 32;

        // static at block scope (persistent across calls)
        for (int i = 0; i < 3; i++) {
            static int counter = 0;
            counter++;
            if (i == 2 && counter != 3) return 33;
        }

        // extern
        if (extern_func() != 77) return 34;
    }

    // ========== TYPE QUALIFIERS (returns 40-49) ==========
    {
        // const
        const int ci = 42;
        if (ci != 42) return 40;

        // volatile
        volatile int vi = 99;
        if (vi != 99) return 41;

        // restrict (C99) — used on pointer params
        int arr[3] = {1, 2, 3};
        int * restrict rp = arr;
        if (rp[0] != 1) return 42;

        // Multiple qualifiers on same type
        const volatile int cvi = 77;
        if (cvi != 77) return 43;

        // Qualifier through pointer: pointer to const
        const int *pci = &ci;
        if (*pci != 42) return 44;

        // Const pointer (pointer itself is const)
        int x = 10;
        int * const cp = &x;
        *cp = 20;
        if (x != 20) return 45;
    }

    // ========== FUNCTION SPECIFIERS (returns 50-59) ==========
    {
        // static inline
        if (inline_add(3, 4) != 7) return 50;

        // inline without static
        if (inline_mul(5, 6) != 30) return 51;
    }

    // ========== DERIVED TYPES (returns 60-69) ==========
    {
        // Array (fixed size)
        int arr[5] = {1, 2, 3, 4, 5};
        if (arr[4] != 5) return 60;

        // VLA (C99)
        int n = 4;
        int vla[n];
        vla[3] = 99;
        if (vla[3] != 99) return 61;

        // Pointer
        int val = 42;
        int *p = &val;
        if (*p != 42) return 62;

        // Pointer to function
        int (*fp)(int, int) = inline_add;
        if (fp(10, 20) != 30) return 63;

        // Structure
        struct S { int a; int b; };
        struct S s = {1, 2};
        if (s.a + s.b != 3) return 64;

        // Union
        union U { int i; float f; };
        union U u;
        u.i = 42;
        if (u.i != 42) return 65;

        // Function type (as return type)
        if (extern_func() != 77) return 66;
    }

    // ========== ENUM TYPES (returns 70-79) ==========
    {
        // Enum constants as int
        enum E { A, B, C };
        int x = A;
        if (sizeof(x) != sizeof(int)) return 70;

        // Negative enumerator values
        enum Neg { NEG = -10, ZERO = 0, POS = 10 };
        if (NEG != -10 || ZERO != 0 || POS != 10) return 71;

        // Implicit sequential
        enum Seq { FIRST, SECOND, THIRD };
        if (FIRST != 0 || SECOND != 1 || THIRD != 2) return 72;
    }

    // ========== TYPE COMPATIBILITY (returns 80-89) ==========
    {
        // Compatible types: same type across expressions
        int a = 42;
        int b = a;
        if (b != 42) return 80;

        // Composite type: pointer compatibility
        int arr[5] = {10, 20, 30, 40, 50};
        int *p = arr;
        if (*(p + 2) != 30) return 81;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_types_keywords_mega", code, &[]), 0);
}

/// C17 6.7.2p2 gives the declaration specifiers as an unordered set, so a
/// size specifier names the same type whichever side of `int` it falls on.
///
/// The specifier tally took the size only when it had not already settled on
/// `int`, so `int long` was a four-byte `long` and `int short` a four-byte
/// `short`. Two independent copies of the tally had the same defect -- one
/// for declarations, one for type-names -- so `long int x;` was right while
/// `sizeof(int long)` was wrong even after the first was fixed. Both spellings
/// of both paths are asserted here.
#[test]
fn c99_size_specifiers_are_order_independent() {
    let code = r#"
/* Declaration path. */
long int a1;    int long a2;
short int b1;   int short b2;
long long int c1;   int long long c2;   long int long c3;
unsigned long int d1;   int unsigned long d2;   long unsigned int d3;
signed short int e1;    int signed short e2;

int main(void) {
    /* Declarations */
    if (sizeof a1 != sizeof a2) return 1;
    if (sizeof a1 != 8) return 2;
    if (sizeof b1 != sizeof b2) return 3;
    if (sizeof b1 != 2) return 4;
    if (sizeof c1 != sizeof c2 || sizeof c1 != sizeof c3) return 5;
    if (sizeof c1 != 8) return 6;
    if (sizeof d1 != sizeof d2 || sizeof d1 != sizeof d3) return 7;
    if (sizeof d1 != 8) return 8;
    if (sizeof e1 != sizeof e2) return 9;
    if (sizeof e1 != 2) return 10;

    /* Type-name path: a separate specifier tally from the one above. */
    if (sizeof(int long) != 8) return 11;
    if (sizeof(int short) != 2) return 12;
    if (sizeof(int long long) != 8) return 13;
    if (sizeof(int unsigned long) != 8) return 14;
    if (sizeof(long int) != sizeof(int long)) return 15;
    if (sizeof(short int) != sizeof(int short)) return 16;

    /* The cast path shares the type-name tally. */
    {
        int long v = 5000000000;
        if ((int long)v != 5000000000) return 17;
        if (sizeof((int long)1) != 8) return 18;
    }

    /* Both spellings name one type, so they are compatible. */
    if (!__builtin_types_compatible_p(int long, long)) return 19;
    if (!__builtin_types_compatible_p(int short, short)) return 20;
    if (__builtin_types_compatible_p(int long, int)) return 21;
    if (__builtin_types_compatible_p(int short, int)) return 22;

    /* `long double` must not be caught by the same rule. */
    if (sizeof(long double) != sizeof(double long)) return 23;
    if (sizeof(long double) == sizeof(double)) return 24;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_size_specifiers_order_independent", code, &[]),
        0
    );
}

/// `_Generic` selects on the controlling expression's type, and a type has
/// to compare equal to itself for that to work.
///
/// `TypeKind` already carries the size, but a parsed specifier list also set
/// a `SHORT`/`LONG`/`LONGLONG` modifier bit that the compatibility test then
/// treated as significant. The canonical interned types -- what a literal's
/// type and the result of the usual arithmetic conversions come from -- carry
/// no such bit, so `1L` and `long` were two incompatible types both spelled
/// "long". A variable of type `long` matched, because its type came from the
/// same specifier path as the association.
#[test]
fn c99_generic_matches_the_long_family() {
    let code = r#"
#define SEL(x) _Generic((x),                                     \
    char: 1, signed char: 2, short: 3, int: 4, long: 5,          \
    long long: 6, unsigned char: 7, unsigned short: 8,           \
    unsigned: 9, unsigned long: 10, unsigned long long: 11,      \
    float: 12, double: 13, long double: 14, default: 0)

typedef long L;

int main(void) {
    /* Literals: the half that never matched. */
    if (SEL(1L) != 5) return 1;
    if (SEL(1UL) != 10) return 2;
    if (SEL(1LL) != 6) return 3;
    if (SEL(1ULL) != 11) return 4;
    if (SEL(1.0L) != 14) return 5;

    /* Controls: these always worked. */
    if (SEL(1) != 4) return 6;
    if (SEL(1u) != 9) return 7;
    if (SEL(1.0) != 13) return 8;
    if (SEL(1.0f) != 12) return 9;
    if (SEL('a') != 4) return 10;

    /* Variables, whose type comes from the specifier path. */
    {
        long v = 1; unsigned long uv = 1; long long llv = 1; long double ldv = 1;
        if (SEL(v) != 5) return 11;
        if (SEL(uv) != 10) return 12;
        if (SEL(llv) != 6) return 13;
        if (SEL(ldv) != 14) return 14;
    }

    /* The usual arithmetic conversions produce a canonical type too. */
    {
        long w = 1;
        if (SEL(w + 1) != 5) return 15;
        if (SEL(w * 2) != 5) return 16;
        if (SEL(-w) != 5) return 17;
    }

    /* A typedef names the same type. */
    if (SEL((L)1) != 5) return 18;

    /* A constant too large for int becomes long, and must match as one. */
    if (SEL(2147483648) != 5) return 19;

    /* Casts were always right; keep them as the control. */
    if (SEL((long)1) != 5) return 20;

    /* Same story through __builtin_types_compatible_p. */
    if (!__builtin_types_compatible_p(__typeof__(1L), long)) return 21;
    if (!__builtin_types_compatible_p(__typeof__(1.0L), long double)) return 22;
    if (!__builtin_types_compatible_p(__typeof__(1UL), unsigned long)) return 23;
    {
        long w = 1;
        if (!__builtin_types_compatible_p(__typeof__(w + 1), long)) return 24;
    }

    /* Nothing new became compatible that should not have. */
    if (__builtin_types_compatible_p(long, int)) return 25;
    if (__builtin_types_compatible_p(long, long long)) return 26;
    if (__builtin_types_compatible_p(long, unsigned long)) return 27;
    if (__builtin_types_compatible_p(short, int)) return 28;
    if (__builtin_types_compatible_p(double, long double)) return 29;
    if (__builtin_types_compatible_p(char, signed char)) return 30;
    if (__builtin_types_compatible_p(char, unsigned char)) return 31;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_generic_matches_the_long_family", code, &[]),
        0
    );
}

/// An enumerated type must be able to represent every one of its members
/// (C17 6.7.2.2p4). c17 gave every enum four signed bytes, so an enumerator
/// that did not fit was silently truncated: `X = 5000000000` came back as
/// 705032704 and `H = 0xFFFFFFFFU` as -1.
///
/// C17 6.7.2.2p2 makes an enumerator outside `int` range a constraint
/// violation, so a diagnostic is required; gcc issues one under -pedantic and
/// widens the type. c17 does both -- warn, and widen -- because truncating in
/// silence is the one response that leaves a program running on a wrong value.
#[test]
fn c99_enum_is_wide_enough_for_its_enumerators() {
    let code = r#"
enum Small  { S = 1 };
enum Neg    { N = -1, NP = 1 };
enum UMax   { U = 0xFFFFFFFFU };
enum Big    { B = 5000000000 };
enum BigNeg { BN = -5000000000 };
enum Mixed  { M0 = -1, M1 = 5000000000 };

int main(void) {
    /* An enum that fits in int keeps int's size. */
    if (sizeof(enum Small) != 4) return 1;
    if (S != 1) return 2;
    if (sizeof(enum Neg) != 4) return 3;
    if (N != -1 || NP != 1) return 4;

    /* Too large for int, but representable unsigned in 32 bits. */
    if (U != 0xFFFFFFFFU) return 5;
    if ((unsigned long long)U != 4294967295ULL) return 6;

    /* Too large for 32 bits either way. */
    if (sizeof(enum Big) != 8) return 7;
    if (B != 5000000000) return 8;
    if ((long long)B != 5000000000LL) return 9;

    if (sizeof(enum BigNeg) != 8) return 10;
    if ((long long)BN != -5000000000LL) return 11;

    /* A negative member forces a signed underlying type even though the
       other member would fit unsigned. */
    if (sizeof(enum Mixed) != 8) return 12;
    if ((long long)M0 != -1LL) return 13;
    if ((long long)M1 != 5000000000LL) return 14;

    /* The values survive arithmetic, not just comparison. */
    if (B / 2 != 2500000000) return 15;
    if (B - 5000000000 != 0) return 16;

    /* A variable of the type holds them too. */
    {
        enum Big v = B;
        if ((long long)v != 5000000000LL) return 17;
        if (sizeof v != 8) return 18;
    }

    /* Implicit successor values continue from a widened predecessor. */
    {
        enum Succ { P = 5000000000, Q };
        if ((long long)Q != 5000000001LL) return 19;
    }
    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_enum_underlying_type", code, &[]), 0);
}

/// `#pragma pack` caps the alignment of every member of the structures
/// declared while it is in effect. c17 discarded every pragma in the
/// preprocessor, so the pragma compiled and did nothing -- silently laying
/// out a protocol structure at natural alignment and disagreeing with every
/// gcc-compiled peer that shares it.
#[test]
fn c99_pragma_pack_caps_member_alignment() {
    let code = r#"
#include <stddef.h>

#pragma pack(push, 1)
struct A { char a; int b; };
#pragma pack(pop)
struct B { char a; int b; };

#pragma pack(1)
struct C { char a; int b; };
#pragma pack()
struct D { char a; int b; };

#pragma pack(push, 2)
struct E { char a; int b; };
#pragma pack(push, 4)
struct F { char a; double b; };
#pragma pack(pop)
struct G { char a; int b; };   /* back to 2 */
#pragma pack(pop)
struct H { char a; int b; };   /* back to natural */

struct I { char a; int b; } __attribute__((packed));

#pragma pack(8)
struct J { char a; int b; };   /* 8 exceeds natural: no effect */
#pragma pack()

/* Nested and array members still obey the cap. */
#pragma pack(1)
struct K { char a; struct { int x; } inner; short b[3]; };
#pragma pack()

/* C99 6.10.9: `_Pragma("pack(...)")` is the same directive, and must not be
   the spelling that quietly does nothing. */
_Pragma("pack(push, 1)")
struct L { char a; int b; };
_Pragma("pack(pop)")
struct M { char a; int b; };

/* A union's alignment is capped too, though its size follows its widest
   member either way. */
#pragma pack(1)
union U { char a; int b; };
#pragma pack()
union V { char a; int b; };

int main(void) {
    if (sizeof(struct A) != 5 || offsetof(struct A, b) != 1) return 1;
    if (sizeof(struct B) != 8 || offsetof(struct B, b) != 4) return 2;
    if (sizeof(struct C) != 5) return 3;
    if (sizeof(struct D) != 8) return 4;
    if (sizeof(struct E) != 6 || offsetof(struct E, b) != 2) return 5;
    if (sizeof(struct F) != 12 || offsetof(struct F, b) != 4) return 6;
    if (sizeof(struct G) != 6) return 7;
    if (sizeof(struct H) != 8) return 8;
    if (sizeof(struct I) != 5) return 9;
    if (sizeof(struct J) != 8) return 10;
    if (sizeof(struct K) != 11) return 11;
    if (offsetof(struct K, inner) != 1 || offsetof(struct K, b) != 5) return 12;
    if (sizeof(struct L) != 5) return 18;
    if (sizeof(struct M) != 8) return 19;
    if (_Alignof(union U) != 1 || sizeof(union U) != 4) return 20;
    if (_Alignof(union V) != 4 || sizeof(union V) != 4) return 21;

    /* The alignment of the type follows the cap, not just its size. */
    if (_Alignof(struct A) != 1) return 13;
    if (_Alignof(struct B) != 4) return 14;
    if (_Alignof(struct E) != 2) return 15;

    /* Members read and write at their packed offsets. */
    {
        struct A a;
        a.a = 'x'; a.b = 0x11223344;
        if (a.a != 'x' || a.b != 0x11223344) return 16;
        struct K k;
        k.a = 1; k.inner.x = 2; k.b[0] = 3; k.b[2] = 4;
        if (k.a != 1 || k.inner.x != 2 || k.b[0] != 3 || k.b[2] != 4) return 17;
    }
    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_pragma_pack", code, &[]), 0);
}
