//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `struct` composite type
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Struct Operations (declaration, access, nested, assignment)
// ============================================================================

#[test]
fn struct_basic_operations() {
    let code = r#"
int main(void) {
    // Test 1-3: Basic declaration and access
    struct point {
        int x;
        int y;
    };
    struct point p;
    p.x = 10;
    p.y = 20;
    if (p.x != 10) return 1;
    if (p.y != 20) return 2;
    if (p.x + p.y != 30) return 3;

    // Test 4-9: Nested structs
    struct rect {
        struct point top_left;
        struct point bottom_right;
    };
    struct rect r;
    r.top_left.x = 0;
    r.top_left.y = 0;
    r.bottom_right.x = 100;
    r.bottom_right.y = 50;
    if (r.top_left.x != 0) return 4;
    if (r.top_left.y != 0) return 5;
    if (r.bottom_right.x != 100) return 6;
    if (r.bottom_right.y != 50) return 7;
    int width = r.bottom_right.x - r.top_left.x;
    int height = r.bottom_right.y - r.top_left.y;
    if (width != 100) return 8;
    if (height != 50) return 9;

    // Test 10-12: Struct assignment (copy)
    struct point p1, p2;
    p1.x = 10;
    p1.y = 20;
    p2 = p1;
    if (p2.x != 10) return 10;
    if (p2.y != 20) return 11;
    p1.x = 100;
    if (p2.x != 10) return 12;  // p2 should be independent

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_basic", code), 0);
}

// ============================================================================
// Struct as Array and Pointer (array of structs, pointer to struct)
// ============================================================================

#[test]
fn struct_array_pointer() {
    let code = r#"
struct point {
    int x;
    int y;
};

int main(void) {
    // Test 1-7: Array of structs
    struct point points[3];
    points[0].x = 0;  points[0].y = 0;
    points[1].x = 10; points[1].y = 20;
    points[2].x = 30; points[2].y = 40;
    if (points[0].x != 0) return 1;
    if (points[0].y != 0) return 2;
    if (points[1].x != 10) return 3;
    if (points[1].y != 20) return 4;
    if (points[2].x != 30) return 5;
    if (points[2].y != 40) return 6;
    int sum = points[0].x + points[1].x + points[2].x;
    if (sum != 40) return 7;

    // Test 8-12: Pointer to struct (->)
    struct point p;
    struct point *ptr = &p;
    ptr->x = 10;
    ptr->y = 20;
    if (ptr->x != 10) return 8;
    if (ptr->y != 20) return 9;
    if (p.x != 10) return 10;
    if (p.y != 20) return 11;
    if (ptr->x + ptr->y != 30) return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_arr_ptr", code), 0);
}

// ============================================================================
// Struct as Function Parameter and Return Value
// ============================================================================

#[test]
fn struct_functions() {
    let code = r#"
struct point {
    int x;
    int y;
};

int sum_point(struct point p) {
    return p.x + p.y;
}

struct point make_point(int x, int y) {
    struct point p;
    p.x = x;
    p.y = y;
    return p;
}

int main(void) {
    // Test 1: Pass struct by value
    struct point p;
    p.x = 10;
    p.y = 20;
    int result = sum_point(p);
    if (result != 30) return 1;

    // Test 2-3: Return struct by value
    struct point p2 = make_point(10, 20);
    if (p2.x != 10) return 2;
    if (p2.y != 20) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_funcs", code), 0);
}

// ============================================================================
// Large Struct Return (sret ABI, two-register return)
// ============================================================================

#[test]
fn struct_return_large() {
    let code = r#"
// 16-byte struct - uses sret
struct large {
    long first;
    long second;
};

struct large make_large(long a, long b) {
    struct large s;
    s.first = a;
    s.second = b;
    return s;
}

// 16-byte struct with padding (12 bytes data)
struct medium {
    long first;   // 8 bytes
    int second;   // 4 bytes (+ 4 padding)
};

struct medium make_medium(long a, int b) {
    struct medium s;
    s.first = a;
    s.second = b;
    return s;
}

// 16-byte struct - boundary case
struct sixteen {
    long first;
    long second;
};

struct sixteen make_sixteen(long a, long b) {
    struct sixteen s;
    s.first = a;
    s.second = b;
    return s;
}

// 24-byte struct - must use sret
struct huge {
    long a;
    long b;
    long c;
};

struct huge make_huge(long x, long y, long z) {
    struct huge h;
    h.a = x;
    h.b = y;
    h.c = z;
    return h;
}

int main(void) {
    // Test 1-4: Basic large struct return
    struct large result = make_large(300000, 200000);
    if (result.first != 300000) return 1;
    if (result.second != 200000) return 2;
    result = make_large(42, 84);
    if (result.first != 42) return 3;
    if (result.second != 84) return 4;

    // Test 5-8: Two-register return (12-byte struct with padding)
    struct medium m = make_medium(0x123456789ABCDEF0L, 42);
    if (m.first != 0x123456789ABCDEF0L) return 5;
    if (m.second != 42) return 6;
    m = make_medium(999, 888);
    if (m.first != 999) return 7;
    if (m.second != 888) return 8;

    // Test 9-10: 16-byte boundary case
    struct sixteen s = make_sixteen(100, 200);
    if (s.first != 100) return 9;
    if (s.second != 200) return 10;

    // Test 11-16: 24-byte struct (sret)
    struct huge h = make_huge(111, 222, 333);
    if (h.a != 111) return 11;
    if (h.b != 222) return 12;
    if (h.c != 333) return 13;
    h = make_huge(1000000, 2000000, 3000000);
    if (h.a != 1000000) return 14;
    if (h.b != 2000000) return 15;
    if (h.c != 3000000) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_return_large", code), 0);
}

// ============================================================================
// Compound Literals (C99 6.5.2.5)
// ============================================================================

#[test]
fn struct_compound_literals() {
    let code = r#"
struct Point { int x; int y; };

int sum_point(struct Point p) {
    return p.x + p.y;
}

// File-scope compound literal - static storage
struct Point origin = (struct Point){0, 0};

// File-scope pointer to compound literal array
int *primes = (int[]){2, 3, 5, 7, 11};

int main(void) {
    // Test 1-2: Basic compound literal
    struct Point p = (struct Point){10, 20};
    if (p.x != 10) return 1;
    if (p.y != 20) return 2;

    // Test 3-4: Designated initializer
    struct Point p2 = (struct Point){.y = 30, .x = 40};
    if (p2.x != 40) return 3;
    if (p2.y != 30) return 4;

    // Test 5-7: Array compound literal
    int *arr = (int[]){1, 2, 3, 4, 5};
    if (arr[0] != 1) return 5;
    if (arr[2] != 3) return 6;
    if (arr[4] != 5) return 7;

    // Test 8: As function argument
    int result = sum_point((struct Point){5, 7});
    if (result != 12) return 8;

    // Test 9-10: Address of compound literal
    struct Point *ptr = &(struct Point){100, 200};
    if (ptr->x != 100) return 9;
    if (ptr->y != 200) return 10;

    // Test 11-12: File-scope compound literal
    if (origin.x != 0) return 11;
    if (origin.y != 0) return 12;

    // Test 13-15: File-scope array pointer
    if (primes[0] != 2) return 13;
    if (primes[2] != 5) return 14;
    if (primes[4] != 11) return 15;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_compound_lit", code), 0);
}
