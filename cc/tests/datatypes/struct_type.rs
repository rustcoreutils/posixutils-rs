//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `struct` composite type
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Struct: Declaration, member access, assignment
// ============================================================================

#[test]
fn struct_basic() {
    let code = r#"
int main(void) {
    struct point {
        int x;
        int y;
    };

    struct point p;

    // Member assignment
    p.x = 10;
    p.y = 20;

    // Member read
    if (p.x != 10) return 1;
    if (p.y != 20) return 2;

    // Member arithmetic
    if (p.x + p.y != 30) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_basic", code), 0);
}

// ============================================================================
// Nested Structs: Struct containing another struct
// ============================================================================

#[test]
fn struct_nested() {
    let code = r#"
int main(void) {
    struct point {
        int x;
        int y;
    };

    struct rect {
        struct point top_left;
        struct point bottom_right;
    };

    struct rect r;

    // Assign nested struct members
    r.top_left.x = 0;
    r.top_left.y = 0;
    r.bottom_right.x = 100;
    r.bottom_right.y = 50;

    // Read nested struct members
    if (r.top_left.x != 0) return 1;
    if (r.top_left.y != 0) return 2;
    if (r.bottom_right.x != 100) return 3;
    if (r.bottom_right.y != 50) return 4;

    // Compute width and height
    int width;
    int height;
    width = r.bottom_right.x - r.top_left.x;
    height = r.bottom_right.y - r.top_left.y;
    if (width != 100) return 5;
    if (height != 50) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_nested", code), 0);
}

// ============================================================================
// Struct Assignment: Copy whole struct to another variable
// ============================================================================

#[test]
fn struct_assignment() {
    let code = r#"
int main(void) {
    struct point {
        int x;
        int y;
    };

    struct point p1;
    struct point p2;

    // Initialize p1
    p1.x = 10;
    p1.y = 20;

    // Copy p1 to p2
    p2 = p1;

    // Verify p2 has the copied values
    if (p2.x != 10) return 1;
    if (p2.y != 20) return 2;

    // Modify p1 and verify p2 is independent
    p1.x = 100;
    if (p2.x != 10) return 3;  // p2 should still be 10

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_assignment", code), 0);
}

// ============================================================================
// Struct as Function Parameter: Pass struct by value
// ============================================================================

#[test]
fn struct_param() {
    let code = r#"
struct point {
    int x;
    int y;
};

int sum_point(struct point p) {
    return p.x + p.y;
}

int main(void) {
    struct point p;
    p.x = 10;
    p.y = 20;

    int result;
    result = sum_point(p);
    if (result != 30) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_param", code), 0);
}

// ============================================================================
// Struct as Function Return: Return struct by value
// ============================================================================

#[test]
fn struct_return() {
    let code = r#"
struct point {
    int x;
    int y;
};

struct point make_point(int x, int y) {
    struct point p;
    p.x = x;
    p.y = y;
    return p;
}

int main(void) {
    struct point p;
    p = make_point(10, 20);

    if (p.x != 10) return 1;
    if (p.y != 20) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_return", code), 0);
}

// ============================================================================
// Array of Structs
// ============================================================================

#[test]
fn struct_array() {
    let code = r#"
struct point {
    int x;
    int y;
};

int main(void) {
    struct point points[3];

    // Initialize array elements
    points[0].x = 0;
    points[0].y = 0;
    points[1].x = 10;
    points[1].y = 20;
    points[2].x = 30;
    points[2].y = 40;

    // Verify values
    if (points[0].x != 0) return 1;
    if (points[0].y != 0) return 2;
    if (points[1].x != 10) return 3;
    if (points[1].y != 20) return 4;
    if (points[2].x != 30) return 5;
    if (points[2].y != 40) return 6;

    // Sum all x values
    int sum;
    sum = points[0].x + points[1].x + points[2].x;
    if (sum != 40) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_array", code), 0);
}

// ============================================================================
// Pointer to Struct: Arrow operator (->)
// ============================================================================

#[test]
fn struct_pointer() {
    let code = r#"
struct point {
    int x;
    int y;
};

int main(void) {
    struct point p;
    struct point *ptr;

    ptr = &p;

    // Write through pointer using ->
    ptr->x = 10;
    ptr->y = 20;

    // Read through pointer using ->
    if (ptr->x != 10) return 1;
    if (ptr->y != 20) return 2;

    // Verify p was modified
    if (p.x != 10) return 3;
    if (p.y != 20) return 4;

    // Arithmetic with ->
    if (ptr->x + ptr->y != 30) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_pointer", code), 0);
}

// ============================================================================
// Large Struct Return: Return struct >8 bytes requiring sret (hidden pointer)
// This tests the sret ABI where large structs are returned via a hidden pointer.
// On ARM64, the sret pointer goes in X8 (not X0 like other args).
// On x86-64, the sret pointer goes in RDI (first arg register).
// ============================================================================

#[test]
fn struct_return_large() {
    let code = r#"
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

int main(void) {
    struct large result;
    result = make_large(300000, 200000);

    // Verify the struct was correctly returned
    if (result.first != 300000) return 1;
    if (result.second != 200000) return 2;

    // Test with different values to ensure correct member mapping
    result = make_large(42, 84);
    if (result.first != 42) return 3;
    if (result.second != 84) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_return_large", code), 0);
}

// ============================================================================
// Two-Register Struct Return: Structs 9-16 bytes returned in RAX+RDX or X0+X1
// Per System V AMD64 ABI and AAPCS64, structs up to 16 bytes are returned
// in two general-purpose registers (not via sret hidden pointer).
// ============================================================================

#[test]
fn struct_return_two_register() {
    let code = r#"
// 12-byte struct - uses two-register return (RAX+RDX or X0+X1)
struct medium {
    long first;   // 8 bytes
    int second;   // 4 bytes (+ 4 padding = 16 total with alignment)
};

struct medium make_medium(long a, int b) {
    struct medium s;
    s.first = a;
    s.second = b;
    return s;
}

// 16-byte struct - boundary case, also uses two registers
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

int main(void) {
    // Test 12-byte struct (with padding to 16)
    struct medium m = make_medium(0x123456789ABCDEF0L, 42);
    if (m.first != 0x123456789ABCDEF0L) return 1;
    if (m.second != 42) return 2;

    // Test 16-byte struct (boundary case)
    struct sixteen s = make_sixteen(100, 200);
    if (s.first != 100) return 3;
    if (s.second != 200) return 4;

    // Test with different values to ensure correct member mapping
    m = make_medium(999, 888);
    if (m.first != 999) return 5;
    if (m.second != 888) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_return_two_register", code), 0);
}

// ============================================================================
// Large Struct Return (>16 bytes): Must use sret (hidden pointer parameter)
// ============================================================================

#[test]
fn struct_return_sret_24_bytes() {
    let code = r#"
// 24-byte struct - must use sret (hidden pointer)
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
    struct huge h = make_huge(111, 222, 333);
    if (h.a != 111) return 1;
    if (h.b != 222) return 2;
    if (h.c != 333) return 3;

    // Test with different values
    h = make_huge(1000000, 2000000, 3000000);
    if (h.a != 1000000) return 4;
    if (h.b != 2000000) return 5;
    if (h.c != 3000000) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_return_sret_24_bytes", code), 0);
}

// ============================================================================
// Compound Literals (C99 6.5.2.5)
// ============================================================================

#[test]
fn compound_literal_struct_basic() {
    let code = r#"
struct Point { int x; int y; };

int main(void) {
    struct Point p = (struct Point){10, 20};
    if (p.x != 10) return 1;
    if (p.y != 20) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("compound_literal_struct_basic", code), 0);
}

#[test]
fn compound_literal_struct_designated() {
    let code = r#"
struct Point { int x; int y; };

int main(void) {
    struct Point p = (struct Point){.y = 30, .x = 40};
    if (p.x != 40) return 1;
    if (p.y != 30) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("compound_literal_struct_designated", code),
        0
    );
}

#[test]
fn compound_literal_array() {
    let code = r#"
int main(void) {
    int *p = (int[]){1, 2, 3, 4, 5};
    if (p[0] != 1) return 1;
    if (p[2] != 3) return 2;
    if (p[4] != 5) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("compound_literal_array", code), 0);
}

#[test]
fn compound_literal_as_argument() {
    let code = r#"
struct Point { int x; int y; };

int sum_point(struct Point p) {
    return p.x + p.y;
}

int main(void) {
    int result = sum_point((struct Point){5, 7});
    if (result != 12) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("compound_literal_as_argument", code), 0);
}

#[test]
fn compound_literal_address_of() {
    let code = r#"
struct Point { int x; int y; };

int main(void) {
    struct Point *ptr = &(struct Point){100, 200};
    if (ptr->x != 100) return 1;
    if (ptr->y != 200) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("compound_literal_address_of", code), 0);
}

#[test]
fn compound_literal_file_scope_struct() {
    let code = r#"
struct Point { int x; int y; };

// File-scope compound literal - static storage
struct Point origin = (struct Point){0, 0};

int main(void) {
    if (origin.x != 0) return 1;
    if (origin.y != 0) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("compound_literal_file_scope_struct", code),
        0
    );
}

#[test]
fn compound_literal_file_scope_array_ptr() {
    let code = r#"
// File-scope pointer to compound literal array
int *primes = (int[]){2, 3, 5, 7, 11};

int main(void) {
    if (primes[0] != 2) return 1;
    if (primes[2] != 5) return 2;
    if (primes[4] != 11) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("compound_literal_file_scope_array_ptr", code),
        0
    );
}
