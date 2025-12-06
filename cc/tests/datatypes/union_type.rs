//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `union` composite type
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Union: Declaration, member access, assignment
// ============================================================================

#[test]
fn union_basic() {
    let code = r#"
int main(void) {
    union data {
        int i;
        float f;
        char c;
    };

    union data d;

    // Member assignment
    d.i = 42;

    // Member read
    if (d.i != 42) return 1;

    // Assign different member
    d.c = 'A';
    if (d.c != 'A') return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_basic", code), 0);
}

// ============================================================================
// Union Size: Size should be max of all members
// ============================================================================

#[test]
fn union_size() {
    let code = r#"
int main(void) {
    union data {
        char c;          // 1 byte
        int i;           // 4 bytes
        double d;        // 8 bytes
    };

    // Union size should be size of largest member (double = 8)
    if (sizeof(union data) != 8) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_size", code), 0);
}

// ============================================================================
// Union Type Punning: All members share the same memory
// ============================================================================

#[test]
fn union_type_punning() {
    let code = r#"
int main(void) {
    union data {
        int i;
        char bytes[4];
    };

    union data d;
    d.i = 0x01020304;

    // On little-endian, bytes[0] should be 0x04 (LSB)
    if (d.bytes[0] != 0x04) return 1;
    if (d.bytes[3] != 0x01) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_type_punning", code), 0);
}

// ============================================================================
// Union Assignment: Copy whole union to another variable
// ============================================================================

#[test]
fn union_assignment() {
    let code = r#"
int main(void) {
    union data {
        int i;
        float f;
    };

    union data d1;
    union data d2;

    // Initialize d1
    d1.i = 12345;

    // Copy d1 to d2
    d2 = d1;

    // Verify d2 has the copied value
    if (d2.i != 12345) return 1;

    // Modify d1 and verify d2 is independent
    d1.i = 99999;
    if (d2.i != 12345) return 2;  // d2 should still be 12345

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_assignment", code), 0);
}

// ============================================================================
// Union as Function Parameter: Pass union by value
// ============================================================================

#[test]
fn union_param() {
    let code = r#"
union data {
    int i;
    float f;
};

int get_int(union data d) {
    return d.i;
}

int main(void) {
    union data d;
    d.i = 789;

    int result;
    result = get_int(d);
    if (result != 789) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_param", code), 0);
}

// ============================================================================
// Union as Function Return: Return union by value
// ============================================================================

#[test]
fn union_return() {
    let code = r#"
union data {
    int i;
    float f;
};

union data make_data(int val) {
    union data d;
    d.i = val;
    return d;
}

int main(void) {
    union data d;
    d = make_data(456);

    if (d.i != 456) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_return", code), 0);
}

// ============================================================================
// Array of Unions
// ============================================================================

#[test]
fn union_array() {
    let code = r#"
union data {
    int i;
    float f;
};

int main(void) {
    union data arr[3];

    // Initialize array elements
    arr[0].i = 10;
    arr[1].i = 20;
    arr[2].i = 30;

    // Verify values
    if (arr[0].i != 10) return 1;
    if (arr[1].i != 20) return 2;
    if (arr[2].i != 30) return 3;

    // Sum all values
    int sum;
    sum = arr[0].i + arr[1].i + arr[2].i;
    if (sum != 60) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_array", code), 0);
}

// ============================================================================
// Pointer to Union: Arrow operator (->)
// ============================================================================

#[test]
fn union_pointer() {
    let code = r#"
union data {
    int i;
    float f;
};

int main(void) {
    union data d;
    union data *ptr;

    ptr = &d;

    // Write through pointer using ->
    ptr->i = 555;

    // Read through pointer using ->
    if (ptr->i != 555) return 1;

    // Verify d was modified
    if (d.i != 555) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_pointer", code), 0);
}

// ============================================================================
// Nested Union in Struct
// ============================================================================

#[test]
fn union_nested_in_struct() {
    let code = r#"
int main(void) {
    struct tagged_value {
        int type;
        union {
            int i;
            float f;
            char c;
        } data;
    };

    struct tagged_value tv;
    tv.type = 1;
    tv.data.i = 100;

    if (tv.type != 1) return 1;
    if (tv.data.i != 100) return 2;

    // Change to char type
    tv.type = 3;
    tv.data.c = 'X';
    if (tv.data.c != 'X') return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_nested_in_struct", code), 0);
}

// ============================================================================
// Union Containing Struct
// ============================================================================

#[test]
fn union_containing_struct() {
    let code = r#"
int main(void) {
    union variant {
        int i;
        struct {
            int x;
            int y;
        } point;
    };

    union variant v;
    v.point.x = 10;
    v.point.y = 20;

    if (v.point.x != 10) return 1;
    if (v.point.y != 20) return 2;

    // Writing to i should overwrite part of point
    v.i = 999;
    if (v.i != 999) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_containing_struct", code), 0);
}

// ============================================================================
// Union with Different Integer Sizes
// ============================================================================

#[test]
fn union_integer_sizes() {
    let code = r#"
int main(void) {
    union sizes {
        char c;
        short s;
        int i;
        long l;
    };

    union sizes u;

    // Write long, read back
    u.l = 0x0123456789ABCDEFLL;
    if (u.l != 0x0123456789ABCDEFLL) return 1;

    // Check smaller members see lower bytes (little-endian)
    u.l = 0x0000000000000042LL;
    if (u.c != 0x42) return 2;
    if (u.s != 0x42) return 3;
    if (u.i != 0x42) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_integer_sizes", code), 0);
}

// ============================================================================
// Forward Declared Union
// ============================================================================

#[test]
fn union_forward_decl() {
    let code = r#"
union data;  // Forward declaration

union data {
    int i;
    float f;
};

int main(void) {
    union data d;
    d.i = 42;
    if (d.i != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("union_forward_decl", code), 0);
}

// ============================================================================
// Union Pointer Arithmetic
// ============================================================================

#[test]
fn union_pointer_arithmetic() {
    let code = r#"
union data {
    int i;
    double d;  // 8 bytes
};

int main(void) {
    union data arr[3];
    union data *p = arr;

    arr[0].i = 100;
    arr[1].i = 200;
    arr[2].i = 300;

    if (p->i != 100) return 1;
    p++;  // Should advance by sizeof(union data) = 8
    if (p->i != 200) return 2;
    p++;
    if (p->i != 300) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_pointer_arithmetic", code), 0);
}
