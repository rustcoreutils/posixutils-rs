//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `union` composite type
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Union Operations (declaration, size, type punning, assignment)
// ============================================================================

#[test]
fn union_basic_operations() {
    let code = r#"
int main(void) {
    // Test 1-2: Basic declaration and access
    union data {
        int i;
        float f;
        char c;
    };
    union data d;
    d.i = 42;
    if (d.i != 42) return 1;
    d.c = 'A';
    if (d.c != 'A') return 2;

    // Test 3: Union size (max of members)
    union sized {
        char c;    // 1 byte
        int i;     // 4 bytes
        double d;  // 8 bytes
    };
    if (sizeof(union sized) != 8) return 3;

    // Test 4-5: Type punning (all members share memory)
    union punned {
        int i;
        char bytes[4];
    };
    union punned p;
    p.i = 0x01020304;
    if (p.bytes[0] != 0x04) return 4;  // Little-endian LSB
    if (p.bytes[3] != 0x01) return 5;

    // Test 6-7: Union assignment (copy)
    union data d1, d2;
    d1.i = 12345;
    d2 = d1;
    if (d2.i != 12345) return 6;
    d1.i = 99999;
    if (d2.i != 12345) return 7;  // d2 independent

    // Test 8: Forward declaration
    union fwd;
    union fwd { int i; float f; };
    union fwd fv;
    fv.i = 42;
    if (fv.i != 42) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_basic", code, &[]), 0);
}

// ============================================================================
// Union as Function Param/Return, Array, Pointer
// ============================================================================

#[test]
fn union_functions_and_collections() {
    let code = r#"
union data {
    int i;
    float f;
    double d;
};

int get_int(union data d) {
    return d.i;
}

union data make_data(int val) {
    union data d;
    d.i = val;
    return d;
}

int main(void) {
    // Test 1: Pass union by value
    union data d;
    d.i = 789;
    if (get_int(d) != 789) return 1;

    // Test 2: Return union by value
    d = make_data(456);
    if (d.i != 456) return 2;

    // Test 3-6: Array of unions
    union data arr[3];
    arr[0].i = 10;
    arr[1].i = 20;
    arr[2].i = 30;
    if (arr[0].i != 10) return 3;
    if (arr[1].i != 20) return 4;
    if (arr[2].i != 30) return 5;
    if (arr[0].i + arr[1].i + arr[2].i != 60) return 6;

    // Test 7-8: Pointer to union (->)
    union data u;
    union data *ptr = &u;
    ptr->i = 555;
    if (ptr->i != 555) return 7;
    if (u.i != 555) return 8;

    // Test 9-11: Pointer arithmetic
    union data parr[3];
    union data *p = parr;
    parr[0].i = 100;
    parr[1].i = 200;
    parr[2].i = 300;
    if (p->i != 100) return 9;
    p++;
    if (p->i != 200) return 10;
    p++;
    if (p->i != 300) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_funcs", code, &[]), 0);
}

// ============================================================================
// Union Nesting (in struct, containing struct, integer sizes)
// ============================================================================

#[test]
fn union_nesting_and_sizes() {
    let code = r#"
int main(void) {
    // Test 1-3: Union nested in struct
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
    tv.type = 3;
    tv.data.c = 'X';
    if (tv.data.c != 'X') return 3;

    // Test 4-6: Union containing struct
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
    if (v.point.x != 10) return 4;
    if (v.point.y != 20) return 5;
    v.i = 999;
    if (v.i != 999) return 6;

    // Test 7-10: Different integer sizes
    union sizes {
        char c;
        short s;
        int i;
        long l;
    };
    union sizes u;
    u.l = 0x0123456789ABCDEFLL;
    if (u.l != 0x0123456789ABCDEFLL) return 7;
    u.l = 0x0000000000000042LL;
    if (u.c != 0x42) return 8;
    if (u.s != 0x42) return 9;
    if (u.i != 0x42) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("union_nesting", code, &[]), 0);
}
