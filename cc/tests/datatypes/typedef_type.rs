//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `typedef` type aliasing
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Typedef (int, unsigned, char, const, local)
// ============================================================================

#[test]
fn typedef_basic_types() {
    let code = r#"
typedef int myint;
typedef unsigned int uint;
typedef char byte;

int main(void) {
    // Test 1: Basic int typedef
    myint x = 42;
    if (x != 42) return 1;

    // Test 2: Unsigned typedef
    uint y = 100;
    if (y != 100) return 2;

    // Test 3: Char typedef
    byte b = 65;
    if (b != 65) return 3;

    // Test 4: Const with typedef
    const myint z = 42;
    if (z != 42) return 4;

    // Test 5: Local typedef
    typedef int localint;
    localint w = 42;
    if (w != 42) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_basic", code), 0);
}

// ============================================================================
// Typedef for Pointers
// ============================================================================

#[test]
fn typedef_pointers() {
    let code = r#"
typedef int *intptr;
typedef int INT, *INTPTR;

int main(void) {
    // Test 1-2: Basic pointer typedef
    int x = 42;
    intptr p = &x;
    if (*p != 42) return 1;
    *p = 100;
    if (x != 100) return 2;

    // Test 3-4: Multiple typedef in one declaration
    INT a = 42;
    INTPTR pa = &a;
    if (a != 42) return 3;
    if (*pa != 42) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_ptr", code), 0);
}

// ============================================================================
// Typedef for Structs
// ============================================================================

#[test]
fn typedef_structs() {
    let code = r#"
typedef struct {
    int x;
    int y;
} Point;

struct namedpoint {
    int x;
    int y;
};
typedef struct namedpoint NamedPoint;

typedef struct node {
    int value;
} Node, *NodePtr;

int main(void) {
    // Test 1-3: Anonymous struct typedef
    Point p;
    p.x = 10;
    p.y = 20;
    if (p.x != 10) return 1;
    if (p.y != 20) return 2;
    if (p.x + p.y != 30) return 3;

    // Test 4-5: Named struct typedef
    NamedPoint np;
    np.x = 10;
    np.y = 20;
    if (np.x != 10) return 4;
    if (np.y != 20) return 5;

    // Test 6-8: Struct and struct pointer typedef
    Node n;
    NodePtr ptr;
    n.value = 42;
    ptr = &n;
    if (ptr->value != 42) return 6;
    ptr->value = 100;
    if (n.value != 100) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_struct", code), 0);
}

// ============================================================================
// Typedef Advanced (arrays, chained, functions, sizeof, cast)
// ============================================================================

#[test]
fn typedef_advanced() {
    let code = r#"
typedef int IntArray[10];
typedef int myint;
typedef myint myint2;
typedef long mylong;
typedef long long mylong2;

int double_it(myint x) {
    return x * 2;
}

myint get_value(void) {
    return 42;
}

int main(void) {
    // Test 1-3: Array typedef
    IntArray arr;
    arr[0] = 1;
    arr[1] = 2;
    arr[9] = 10;
    if (arr[0] != 1) return 1;
    if (arr[1] != 2) return 2;
    if (arr[9] != 10) return 3;

    // Test 4: Chained typedef
    myint2 x = 42;
    if (x != 42) return 4;

    // Test 5: Function parameter with typedef
    myint n = 21;
    if (double_it(n) != 42) return 5;

    // Test 6: Function return typedef
    myint v = get_value();
    if (v != 42) return 6;

    // Test 7-8: Sizeof with typedef
    if (sizeof(myint) != sizeof(int)) return 7;
    if (sizeof(mylong2) != sizeof(long long)) return 8;

    // Test 9: Cast with typedef
    int a = 42;
    mylong b = (mylong)a;
    if (b != 42) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_adv", code), 0);
}

// ============================================================================
// Typedef for Function Types (C99 function type typedefs)
// ============================================================================

#[test]
fn typedef_function_types() {
    let code = r#"
// Function type typedef with parenthesized name: typedef return_type (name)(params);
typedef int (int_func)(void);
typedef int (int_int_func)(int);
typedef void (void_func)(void);

// Function pointer typedef using function type
typedef int_func *int_func_ptr;
typedef int_int_func *int_int_func_ptr;

// Regular function pointer typedef for comparison
typedef int (*regular_func_ptr)(int);

int get_value(void) {
    return 42;
}

int double_it(int x) {
    return x * 2;
}

void do_nothing(void) {
    // Empty
}

int main(void) {
    // Test 1: Function type typedef can be used as pointer base
    int_func *fp1 = get_value;
    if (fp1() != 42) return 1;

    // Test 2: Function pointer typedef from function type
    int_func_ptr fp2 = get_value;
    if (fp2() != 42) return 2;

    // Test 3: Function type with parameter
    int_int_func *fp3 = double_it;
    if (fp3(21) != 42) return 3;

    // Test 4: Function pointer typedef with parameter
    int_int_func_ptr fp4 = double_it;
    if (fp4(10) != 20) return 4;

    // Test 5: Regular function pointer typedef for sanity
    regular_func_ptr fp5 = double_it;
    if (fp5(5) != 10) return 5;

    // Test 6: Void function type
    void_func *fp6 = do_nothing;
    fp6();  // Should not crash

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_func", code), 0);
}
