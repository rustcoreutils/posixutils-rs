//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89 Data Types Mega-Test
//
// Consolidates ALL data type tests: char, short, int, long, float, double,
// long double, array, struct, union, pointer, typedef, void.
//
// Return code convention (within each section):
//   Section A (type 1): 1-49
//   Section B (type 2): 50-99
//   etc.
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: ALL C89 integral types (char, short, int, long)
// ============================================================================

#[test]
fn c89_integral_types_mega() {
    let code = r#"
int main(void) {
    // ========== CHAR SECTION (returns 1-29) ==========
    {
        // Basic char operations
        char c = 'A';
        if (c != 65) return 1;

        // Escape sequences
        c = '\n'; if (c != 10) return 2;
        c = '\t'; if (c != 9) return 3;
        c = '\0'; if (c != 0) return 4;

        // Hex/octal escapes
        c = '\x41'; if (c != 'A') return 5;
        c = '\101'; if (c != 'A') return 6;

        // Arithmetic
        char a = 30, b = 12;
        if (a + b != 42) return 7;
        if (a - b != 18) return 8;
        if (a * 2 != 60) return 9;
        if (a / 2 != 15) return 10;

        // Comparison
        a = 'X'; b = 'X';
        if ((a == b) != 1) return 11;
        a = 'A'; b = 'Z';
        if ((a < b) != 1) return 12;

        // Bitwise
        a = 0x3F; b = 0x0F;
        if ((a & b) != 0x0F) return 13;
        if ((a | b) != 0x3F) return 14;

        // signed char
        signed char sc = -42;
        if (sc != -42) return 15;
        if (-sc != 42) return 16;

        // unsigned char
        unsigned char uc = 200;
        if (uc != 200) return 17;
        uc = 255;
        if (uc != 255) return 18;

        // Wide char literal
        int wc = L'A';
        if (wc != 65) return 19;
    }

    // ========== SHORT SECTION (returns 30-59) ==========
    {
        short a = 30, b = 12;

        // Arithmetic
        if (a + b != 42) return 30;
        if (a - b != 18) return 31;
        if (a * 2 != 60) return 32;
        if (a / 2 != 15) return 33;
        if (a % 7 != 2) return 34;

        // Comparison
        if ((a == 30) != 1) return 35;
        if ((a < b) != 0) return 36;
        if ((a > b) != 1) return 37;

        // Bitwise
        a = 0xFF; b = 0x0F;
        if ((a & b) != 0x0F) return 38;
        if ((a | 0xF0) != 0xFF) return 39;
        if ((a << 4) != 0xFF0) return 40;

        // unsigned short
        unsigned short us = 60000;
        if (us != 60000) return 41;
        us = 65535;
        if (us != 65535) return 42;

        // Range edge cases
        short max_s = 32767;
        if (max_s != 32767) return 43;
        short min_s = -32768;
        if (min_s != -32768) return 44;
    }

    // ========== INT SECTION (returns 60-89) ==========
    {
        int a = 30, b = 12;

        // Arithmetic
        if (a + b != 42) return 60;
        if (a - b != 18) return 61;
        if (a * b != 360) return 62;
        if (84 / 2 != 42) return 63;
        if (47 % 10 != 7) return 64;

        // Unary
        a = -42;
        if (-a != 42) return 65;
        a = 42;
        if (+a != 42) return 66;

        // Comparison
        a = 42; b = 42;
        if ((a == b) != 1) return 67;
        if ((a != b) != 0) return 68;

        // Logical
        a = 1; b = 1;
        if ((a && b) != 1) return 69;
        if ((a || 0) != 1) return 70;
        if (!0 != 1) return 71;

        // Bitwise
        a = 0xFF; b = 0x0F;
        if ((a & b) != 0x0F) return 72;
        if ((a | 0xF00) != 0xFFF) return 73;
        if ((a ^ 0xF0) != 0x0F) return 74;
        if ((1 << 4) != 16) return 75;
        if ((64 >> 2) != 16) return 76;

        // Assignment operators
        a = 40; a += 2;
        if (a != 42) return 77;
        a = 50; a -= 8;
        if (a != 42) return 78;

        // Inc/dec
        a = 41;
        if (++a != 42) return 79;
        a = 43;
        if (--a != 42) return 80;

        // Ternary
        a = 1;
        if ((a ? 42 : 0) != 42) return 81;

        // unsigned int
        unsigned int ui = 0xFFFFFFFF;
        if ((ui & 0xFF) != 0xFF) return 82;
    }

    // ========== LONG SECTION (returns 90-119) ==========
    {
        long a = 30L, b = 12L;

        // Arithmetic
        if (a + b != 42L) return 90;
        if (a - b != 18L) return 91;
        if (a * b != 360L) return 92;
        if (84L / 2L != 42L) return 93;
        if (47L % 10L != 7L) return 94;

        // Comparison
        a = 42L; b = 42L;
        if ((a == b) != 1) return 95;
        a = 10L; b = 20L;
        if ((a < b) != 1) return 96;

        // Bitwise
        a = 0xFFL; b = 0x0FL;
        if ((a & b) != 0x0FL) return 97;
        if ((1L << 4) != 16L) return 98;

        // unsigned long
        unsigned long ul = 0xFFFFFFFFFFFFFF00UL;
        if ((ul & 0xFF) != 0) return 99;

        // Large values
        long big = 3000000000L;
        if (big != 3000000000L) return 100;

        // Suffixes
        if (100L + 23L != 123L) return 101;
        if (100UL + 23UL != 123UL) return 102;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_integral_mega", code, &[]), 0);
}

// ============================================================================
// Mega-test: ALL C89 floating-point types (float, double, long double)
// ============================================================================

#[test]
fn c89_floating_types_mega() {
    let code = r#"
int main(void) {
    // ========== FLOAT SECTION (returns 1-29) ==========
    {
        float a = 3.14f, b = 2.0f;

        // Basic operations
        if (a < 3.0f) return 1;
        if (a > 4.0f) return 2;

        // Arithmetic
        float sum = a + b;
        if (sum < 5.0f || sum > 5.2f) return 3;

        float diff = a - b;
        if (diff < 1.0f || diff > 1.2f) return 4;

        float prod = 2.0f * 3.0f;
        if (prod < 5.9f || prod > 6.1f) return 5;

        float quot = 6.0f / 2.0f;
        if (quot < 2.9f || quot > 3.1f) return 6;

        // Comparison
        a = 1.0f; b = 2.0f;
        if ((a < b) != 1) return 7;
        if ((a > b) != 0) return 8;

        a = 2.0f; b = 2.0f;
        if ((a == b) != 1) return 9;
        if ((a != b) != 0) return 10;

        // Conversion to int
        a = 42.7f;
        int i = (int)a;
        if (i != 42) return 11;

        // Float literals
        float f1 = 1.5f;
        float f2 = 1.5F;
        if (f1 < 1.4f || f1 > 1.6f) return 12;
        if (f2 < 1.4f || f2 > 1.6f) return 13;

        // Scientific notation
        float sci = 1.5e2f;  // 150.0
        if (sci < 149.0f || sci > 151.0f) return 14;
    }

    // ========== DOUBLE SECTION (returns 30-59) ==========
    {
        double a = 3.14159265358979, b = 2.0;

        // Basic
        if (a < 3.14) return 30;
        if (a > 3.15) return 31;

        // Arithmetic
        double sum = a + b;
        if (sum < 5.14 || sum > 5.15) return 32;

        double diff = a - b;
        if (diff < 1.14 || diff > 1.15) return 33;

        double prod = 2.5 * 4.0;
        if (prod < 9.9 || prod > 10.1) return 34;

        double quot = 10.0 / 4.0;
        if (quot < 2.4 || quot > 2.6) return 35;

        // Comparison
        a = 1.0; b = 2.0;
        if ((a < b) != 1) return 36;

        // Conversion
        a = 42.9;
        int i = (int)a;
        if (i != 42) return 37;

        // Precision test (double has ~15 digits)
        double precise = 1.23456789012345;
        if (precise < 1.234567890123 || precise > 1.234567890124) return 38;

        // Large values
        double big = 1e100;
        if (big < 9e99) return 39;

        // Negative
        a = -3.14;
        if (a > -3.13) return 40;
    }

    // ========== LONG DOUBLE SECTION (returns 60-79) ==========
    {
        long double a = 3.14159265358979323846L;

        // Basic
        if (a < 3.14L) return 60;
        if (a > 3.15L) return 61;

        // Arithmetic
        long double b = 2.0L;
        long double sum = a + b;
        if (sum < 5.14L || sum > 5.15L) return 62;

        // Conversion
        a = 42.9L;
        int i = (int)a;
        if (i != 42) return 63;

        // Suffix variations
        long double ld1 = 1.5L;
        long double ld2 = 1.5l;
        if (ld1 < 1.4L || ld1 > 1.6L) return 64;
        if (ld2 < 1.4L || ld2 > 1.6L) return 65;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_floating_mega", code, &[]), 0);
}

// ============================================================================
// Mega-test: ALL C89 aggregate types (array, struct, union)
// ============================================================================

#[test]
fn c89_aggregate_types_mega() {
    let code = r#"
struct Point { int x; int y; };
struct Rect { struct Point tl; struct Point br; };
union Data { int i; float f; char c; };

int sum_point(struct Point p) { return p.x + p.y; }
struct Point make_point(int x, int y) {
    struct Point p; p.x = x; p.y = y; return p;
}

int sum_array(int arr[], int size) {
    int total = 0, i;
    for (i = 0; i < size; i++) total += arr[i];
    return total;
}

int main(void) {
    // ========== ARRAY SECTION (returns 1-39) ==========
    {
        // Basic int array
        int arr[3];
        arr[0] = 10; arr[1] = 20; arr[2] = 30;
        if (arr[0] != 10) return 1;
        if (arr[1] != 20) return 2;
        if (arr[2] != 30) return 3;

        // char array
        char carr[4] = {'a', 'b', 'c', 0};
        if (carr[0] != 'a') return 4;
        if (carr[3] != 0) return 5;

        // long array
        long larr[3] = {1000000000L, 2000000000L, 3000000000L};
        if (larr[0] != 1000000000L) return 6;
        if (larr[2] != 3000000000L) return 7;

        // Array initialization
        int arr_init[5] = {1, 2, 3, 4, 5};
        if (arr_init[0] != 1) return 8;
        if (arr_init[4] != 5) return 9;

        // sizeof
        int arr_sz[10];
        if (sizeof(arr_sz) != 40) return 10;

        // Array decay to pointer
        int arr_d[3] = {10, 20, 30};
        int *p = arr_d;
        if (*p != 10) return 11;
        if (p[1] != 20) return 12;
        if (*(p + 2) != 30) return 13;

        // Multi-dimensional array
        int arr2d[2][3] = {{1, 2, 3}, {4, 5, 6}};
        if (arr2d[0][0] != 1) return 14;
        if (arr2d[1][2] != 6) return 15;
        if (sizeof(arr2d) != 24) return 16;

        // Array of pointers
        int a = 10, b = 20, c = 30;
        int *ptrs[3] = {&a, &b, &c};
        if (*ptrs[0] != 10) return 17;
        if (*ptrs[2] != 30) return 18;

        // Array as function param
        int arr_f[5] = {1, 2, 3, 4, 5};
        if (sum_array(arr_f, 5) != 15) return 19;

        // Pointer arithmetic equivalence
        int arr_e[5] = {10, 20, 30, 40, 50};
        if (arr_e[2] != *(arr_e + 2)) return 20;
        if (2[arr_e] != 30) return 21;  // commutative
    }

    // ========== STRUCT SECTION (returns 40-69) ==========
    {
        // Basic struct
        struct Point p;
        p.x = 10; p.y = 20;
        if (p.x != 10) return 40;
        if (p.y != 20) return 41;
        if (p.x + p.y != 30) return 42;

        // Nested struct
        struct Rect r;
        r.tl.x = 0; r.tl.y = 0;
        r.br.x = 100; r.br.y = 50;
        if (r.br.x - r.tl.x != 100) return 43;
        if (r.br.y - r.tl.y != 50) return 44;

        // Struct assignment (copy)
        struct Point p1, p2;
        p1.x = 10; p1.y = 20;
        p2 = p1;
        if (p2.x != 10) return 45;
        p1.x = 100;
        if (p2.x != 10) return 46;  // p2 independent

        // Pointer to struct
        struct Point p3;
        struct Point *ptr = &p3;
        ptr->x = 30; ptr->y = 40;
        if (p3.x != 30) return 47;
        if (ptr->x != 30) return 48;

        // Array of structs
        struct Point points[3];
        points[0].x = 1; points[0].y = 2;
        points[1].x = 3; points[1].y = 4;
        if (points[0].x + points[1].y != 5) return 49;

        // Struct as function param
        struct Point pf;
        pf.x = 10; pf.y = 20;
        if (sum_point(pf) != 30) return 50;

        // Struct as function return
        struct Point pr = make_point(5, 7);
        if (pr.x != 5) return 51;
        if (pr.y != 7) return 52;

        // Large struct return (sret)
        struct { long a; long b; } large;
        large.a = 100; large.b = 200;
        if (large.a + large.b != 300) return 53;
    }

    // ========== UNION SECTION (returns 70-89) ==========
    {
        // Basic union
        union Data d;
        d.i = 42;
        if (d.i != 42) return 70;

        // Union reinterpretation
        d.f = 1.5f;
        // Can't compare d.i now (undefined), but accessing works

        d.c = 'X';
        if (d.c != 'X') return 71;

        // sizeof union (max member size)
        if (sizeof(union Data) < sizeof(int)) return 72;
        if (sizeof(union Data) < sizeof(float)) return 73;

        // Union with struct members
        union { struct Point p; int arr[2]; } u;
        u.p.x = 10; u.p.y = 20;
        if (u.arr[0] != 10) return 74;  // Same memory layout
        if (u.arr[1] != 20) return 75;

        // Pointer to union
        union Data *up = &d;
        up->i = 100;
        if (d.i != 100) return 76;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_aggregate_mega", code, &[]), 0);
}

// ============================================================================
// Mega-test: Pointer and typedef types
// ============================================================================

#[test]
fn c89_pointer_typedef_mega() {
    let code = r#"
typedef int INT;
typedef int* INTPTR;
typedef struct { int x; int y; } Point;
typedef int IntArray[5];

int double_val(int x) { return x * 2; }

int main(void) {
    // ========== POINTER SECTION (returns 1-39) ==========
    {
        // Basic pointer
        int x = 42;
        int *p = &x;
        if (*p != 42) return 1;

        // Pointer modification
        *p = 100;
        if (x != 100) return 2;

        // Pointer arithmetic
        int arr[5] = {10, 20, 30, 40, 50};
        int *pa = arr;
        if (*(pa + 2) != 30) return 3;
        pa++;
        if (*pa != 20) return 4;

        // Pointer comparison
        int *p1 = &arr[0];
        int *p2 = &arr[2];
        if ((p1 < p2) != 1) return 5;
        if ((p1 == p2) != 0) return 6;

        // Pointer subtraction
        long diff = p2 - p1;
        if (diff != 2) return 7;

        // Null pointer
        int *pn = 0;
        if (pn) return 8;  // Should be false
        pn = &x;
        if (!pn) return 9;  // Should be true now

        // Double pointer
        int **pp = &p;
        if (**pp != 100) return 10;
        **pp = 200;
        if (x != 200) return 11;

        // Function pointer
        int (*fp)(int) = double_val;
        if (fp(21) != 42) return 12;

        // Void pointer
        void *vp = &x;
        int *ip = vp;
        if (*ip != 200) return 13;

        // const pointer
        const int ci = 50;
        const int *pci = &ci;
        if (*pci != 50) return 14;

        // Pointer to array element
        int *pe = &arr[2];
        if (pe[-1] != 20) return 15;
        if (pe[1] != 40) return 16;
    }

    // ========== TYPEDEF SECTION (returns 40-59) ==========
    {
        // Basic typedef
        INT i = 42;
        if (i != 42) return 40;

        // Pointer typedef
        INT x = 100;
        INTPTR p = &x;
        if (*p != 100) return 41;

        // Struct typedef
        Point pt;
        pt.x = 10; pt.y = 20;
        if (pt.x + pt.y != 30) return 42;

        // Array typedef
        IntArray arr = {1, 2, 3, 4, 5};
        if (arr[0] != 1) return 43;
        if (arr[4] != 5) return 44;

        // Typedef in function context
        INT sum = 0;
        for (INT j = 0; j < 5; j++) {
            sum += arr[j];
        }
        if (sum != 15) return 45;

        // sizeof with typedef
        if (sizeof(INT) != sizeof(int)) return 46;
        if (sizeof(Point) != 8) return 47;  // Two ints
    }

    // ========== VOID TYPE SECTION (returns 60-69) ==========
    {
        // void pointer cast
        int x = 42;
        void *vp = &x;
        int *ip = (int*)vp;
        if (*ip != 42) return 60;

        // void pointer arithmetic (via cast)
        int arr[3] = {1, 2, 3};
        void *va = arr;
        if (((int*)va)[1] != 2) return 61;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_ptr_typedef_mega", code, &[]), 0);
}
