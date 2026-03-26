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
