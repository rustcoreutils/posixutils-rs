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
