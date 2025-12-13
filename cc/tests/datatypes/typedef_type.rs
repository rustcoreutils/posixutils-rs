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

use crate::common::compile_and_run;

// ============================================================================
// Basic Typedef: Simple type alias for int
// ============================================================================

#[test]
fn typedef_basic_int() {
    let code = r#"
typedef int myint;

int main(void) {
    myint x;
    x = 42;
    if (x != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_basic_int", code), 0);
}

// ============================================================================
// Typedef for unsigned types
// ============================================================================

#[test]
fn typedef_unsigned() {
    let code = r#"
typedef unsigned int uint;

int main(void) {
    uint x;
    x = 100;
    if (x != 100) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_unsigned", code), 0);
}

// ============================================================================
// Typedef for pointer types
// ============================================================================

#[test]
fn typedef_pointer() {
    let code = r#"
typedef int *intptr;

int main(void) {
    int x;
    intptr p;

    x = 42;
    p = &x;

    if (*p != 42) return 1;

    *p = 100;
    if (x != 100) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_pointer", code), 0);
}

// ============================================================================
// Typedef for struct types
// ============================================================================

#[test]
fn typedef_struct() {
    let code = r#"
typedef struct {
    int x;
    int y;
} Point;

int main(void) {
    Point p;

    p.x = 10;
    p.y = 20;

    if (p.x != 10) return 1;
    if (p.y != 20) return 2;
    if (p.x + p.y != 30) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_struct", code), 0);
}

// ============================================================================
// Typedef for named struct
// ============================================================================

#[test]
fn typedef_named_struct() {
    let code = r#"
struct point {
    int x;
    int y;
};

typedef struct point Point;

int main(void) {
    Point p;

    p.x = 10;
    p.y = 20;

    if (p.x != 10) return 1;
    if (p.y != 20) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_named_struct", code), 0);
}

// ============================================================================
// Chained typedef: typedef of typedef
// ============================================================================

#[test]
fn typedef_chained() {
    let code = r#"
typedef int myint;
typedef myint myint2;

int main(void) {
    myint2 x;
    x = 42;
    if (x != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_chained", code), 0);
}

// ============================================================================
// Typedef for arrays
// ============================================================================

#[test]
fn typedef_array() {
    let code = r#"
typedef int IntArray[10];

int main(void) {
    IntArray arr;

    arr[0] = 1;
    arr[1] = 2;
    arr[9] = 10;

    if (arr[0] != 1) return 1;
    if (arr[1] != 2) return 2;
    if (arr[9] != 10) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_array", code), 0);
}

// ============================================================================
// Typedef used in function parameter
// ============================================================================

#[test]
fn typedef_function_param() {
    let code = r#"
typedef int myint;

int double_it(myint x) {
    return x * 2;
}

int main(void) {
    myint n;
    n = 21;
    if (double_it(n) != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_function_param", code), 0);
}

// ============================================================================
// Typedef used as function return type
// ============================================================================

#[test]
fn typedef_function_return() {
    let code = r#"
typedef int myint;

myint get_value(void) {
    return 42;
}

int main(void) {
    myint x;
    x = get_value();
    if (x != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_function_return", code), 0);
}

// ============================================================================
// Local typedef (inside function)
// ============================================================================

#[test]
fn typedef_local() {
    let code = r#"
int main(void) {
    typedef int localint;
    localint x;
    x = 42;
    if (x != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_local", code), 0);
}

// ============================================================================
// Multiple typedefs in one declaration
// ============================================================================

#[test]
fn typedef_multiple() {
    let code = r#"
typedef int INT, *INTPTR;

int main(void) {
    INT x;
    INTPTR p;

    x = 42;
    p = &x;

    if (x != 42) return 1;
    if (*p != 42) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_multiple", code), 0);
}

// ============================================================================
// Typedef for char
// ============================================================================

#[test]
fn typedef_char() {
    let code = r#"
typedef char byte;

int main(void) {
    byte b;
    b = 65;
    if (b != 65) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_char", code), 0);
}

// ============================================================================
// Typedef with const qualifier
// ============================================================================

#[test]
fn typedef_const() {
    let code = r#"
typedef int myint;

int main(void) {
    const myint x = 42;
    if (x != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_const", code), 0);
}

// ============================================================================
// Typedef for struct pointer (common pattern)
// ============================================================================

#[test]
fn typedef_struct_pointer() {
    let code = r#"
typedef struct node {
    int value;
} Node, *NodePtr;

int main(void) {
    Node n;
    NodePtr p;

    n.value = 42;
    p = &n;

    if (p->value != 42) return 1;

    p->value = 100;
    if (n.value != 100) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_struct_pointer", code), 0);
}

// ============================================================================
// Typedef in sizeof
// ============================================================================

#[test]
fn typedef_sizeof() {
    let code = r#"
typedef int myint;
typedef long long mylong;

int main(void) {
    if (sizeof(myint) != sizeof(int)) return 1;
    if (sizeof(mylong) != sizeof(long long)) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_sizeof", code), 0);
}

// ============================================================================
// Typedef in cast
// ============================================================================

#[test]
fn typedef_cast() {
    let code = r#"
typedef long mylong;

int main(void) {
    int x;
    mylong y;

    x = 42;
    y = (mylong)x;

    if (y != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_cast", code), 0);
}
