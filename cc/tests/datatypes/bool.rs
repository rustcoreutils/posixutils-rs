//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 `_Bool` data type
//
// C99 _Bool has special semantics:
// - When any scalar value is converted to _Bool:
//   - Result is 0 if the value compares equal to 0
//   - Result is 1 otherwise
// - _Bool is an unsigned integer type that can store 0 and 1
// - sizeof(_Bool) is implementation-defined but typically 1
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Declaration and Assignment
// ============================================================================

#[test]
fn bool_basic() {
    let code = r#"
int main(void) {
    _Bool a, b;

    // Direct assignment of 0
    a = 0;
    if (a != 0) return 1;

    // Direct assignment of 1
    b = 1;
    if (b != 1) return 2;

    // Size of _Bool (should be 1 byte on most platforms)
    if (sizeof(_Bool) != 1) return 3;

    // _Bool in conditions
    a = 0;
    if (a) return 4;  // Should not execute

    b = 1;
    if (!b) return 5;  // Should not execute

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_basic", code), 0);
}

// ============================================================================
// Conversion Semantics (The key C99 _Bool feature)
// ============================================================================

#[test]
fn bool_conversion_from_int() {
    let code = r#"
int main(void) {
    _Bool b;
    int i;

    // Zero converts to 0
    i = 0;
    b = i;
    if (b != 0) return 1;

    // One converts to 1
    i = 1;
    b = i;
    if (b != 1) return 2;

    // Any non-zero positive converts to 1
    i = 42;
    b = i;
    if (b != 1) return 3;

    // Large positive converts to 1
    i = 1000000;
    b = i;
    if (b != 1) return 4;

    // Negative converts to 1 (non-zero)
    i = -1;
    b = i;
    if (b != 1) return 5;

    // Large negative converts to 1
    i = -1000000;
    b = i;
    if (b != 1) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_conv_int", code), 0);
}

#[test]
fn bool_conversion_from_char() {
    let code = r#"
int main(void) {
    _Bool b;
    char c;
    unsigned char uc;

    // Zero char converts to 0
    c = 0;
    b = c;
    if (b != 0) return 1;

    // Non-zero char converts to 1
    c = 'A';
    b = c;
    if (b != 1) return 2;

    // Null character
    c = '\0';
    b = c;
    if (b != 0) return 3;

    // Unsigned char 255 converts to 1
    uc = 255;
    b = uc;
    if (b != 1) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_conv_char", code), 0);
}

#[test]
fn bool_conversion_literal() {
    let code = r#"
int main(void) {
    _Bool b;

    // Literal 0
    b = 0;
    if (b != 0) return 1;

    // Literal 1
    b = 1;
    if (b != 1) return 2;

    // Literal non-zero becomes 1
    b = 42;
    if (b != 1) return 3;

    // Literal 255 becomes 1
    b = 255;
    if (b != 1) return 4;

    // Literal negative becomes 1
    b = -1;
    if (b != 1) return 5;

    // Explicit cast of non-zero
    b = (_Bool)100;
    if (b != 1) return 6;

    // Explicit cast of zero
    b = (_Bool)0;
    if (b != 0) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_conv_lit", code), 0);
}

// ============================================================================
// Logical Operators
// ============================================================================

#[test]
fn bool_logical_operators() {
    let code = r#"
int main(void) {
    _Bool a, b, result;

    // Logical NOT on false
    a = 0;
    result = !a;
    if (result != 1) return 1;

    // Logical NOT on true
    a = 1;
    result = !a;
    if (result != 0) return 2;

    // Logical AND: true && true
    a = 1; b = 1;
    result = a && b;
    if (result != 1) return 3;

    // Logical AND: true && false
    a = 1; b = 0;
    result = a && b;
    if (result != 0) return 4;

    // Logical AND: false && true
    a = 0; b = 1;
    result = a && b;
    if (result != 0) return 5;

    // Logical AND: false && false
    a = 0; b = 0;
    result = a && b;
    if (result != 0) return 6;

    // Logical OR: true || true
    a = 1; b = 1;
    result = a || b;
    if (result != 1) return 7;

    // Logical OR: true || false
    a = 1; b = 0;
    result = a || b;
    if (result != 1) return 8;

    // Logical OR: false || true
    a = 0; b = 1;
    result = a || b;
    if (result != 1) return 9;

    // Logical OR: false || false
    a = 0; b = 0;
    result = a || b;
    if (result != 0) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_logical", code), 0);
}

// ============================================================================
// Comparison Operators
// ============================================================================

#[test]
fn bool_comparison_operators() {
    let code = r#"
int main(void) {
    _Bool a, b;

    // Equal - both true
    a = 1; b = 1;
    if ((a == b) != 1) return 1;

    // Equal - both false
    a = 0; b = 0;
    if ((a == b) != 1) return 2;

    // Equal - different
    a = 1; b = 0;
    if ((a == b) != 0) return 3;

    // Not equal - same
    a = 1; b = 1;
    if ((a != b) != 0) return 4;

    // Not equal - different
    a = 1; b = 0;
    if ((a != b) != 1) return 5;

    // Less than: 0 < 1
    a = 0; b = 1;
    if ((a < b) != 1) return 6;

    // Less than: 1 < 0 (false)
    a = 1; b = 0;
    if ((a < b) != 0) return 7;

    // Greater than: 1 > 0
    a = 1; b = 0;
    if ((a > b) != 1) return 8;

    // Less than or equal: 0 <= 1
    a = 0; b = 1;
    if ((a <= b) != 1) return 9;

    // Less than or equal: 1 <= 1
    a = 1; b = 1;
    if ((a <= b) != 1) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_compare", code), 0);
}

// ============================================================================
// Arithmetic Operators (Limited for _Bool)
// ============================================================================

#[test]
fn bool_arithmetic_operators() {
    let code = r#"
int main(void) {
    _Bool a, b;
    int result;

    // _Bool values promote to int in arithmetic
    a = 1; b = 1;
    result = a + b;
    if (result != 2) return 1;

    a = 1; b = 0;
    result = a + b;
    if (result != 1) return 2;

    a = 1; b = 1;
    result = a * b;
    if (result != 1) return 3;

    // Storing result back to _Bool (should normalize)
    a = 1; b = 1;
    a = a + b;  // 2 -> 1
    if (a != 1) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_arith", code), 0);
}

// ============================================================================
// Bitwise Operators
// ============================================================================

#[test]
fn bool_bitwise_operators() {
    let code = r#"
int main(void) {
    _Bool a, b, result;

    // Bitwise AND
    a = 1; b = 1;
    result = a & b;
    if (result != 1) return 1;

    a = 1; b = 0;
    result = a & b;
    if (result != 0) return 2;

    // Bitwise OR
    a = 1; b = 0;
    result = a | b;
    if (result != 1) return 3;

    a = 0; b = 0;
    result = a | b;
    if (result != 0) return 4;

    // Bitwise XOR
    a = 1; b = 1;
    result = a ^ b;
    if (result != 0) return 5;

    a = 1; b = 0;
    result = a ^ b;
    if (result != 1) return 6;

    // Bitwise NOT (result stored to _Bool normalizes)
    a = 0;
    result = ~a;  // ~0 in int is all 1s, but stored to _Bool becomes 1
    // Note: ~0 is -1 (or large value), which converts to 1 when stored to _Bool
    if (result != 1) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_bitwise", code), 0);
}

// ============================================================================
// Assignment Operators
// ============================================================================

#[test]
fn bool_assignment_operators() {
    let code = r#"
int main(void) {
    _Bool a;

    // Simple assignment
    a = 1;
    if (a != 1) return 1;

    // Assignment from expression
    a = (5 > 3);  // true -> 1
    if (a != 1) return 2;

    a = (3 > 5);  // false -> 0
    if (a != 0) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_assign", code), 0);
}

// ============================================================================
// Increment and Decrement (Special behavior for _Bool)
// ============================================================================

#[test]
fn bool_increment_decrement() {
    let code = r#"
int main(void) {
    _Bool a;

    // Pre-increment on 0 -> 1
    a = 0;
    ++a;
    if (a != 1) return 1;

    // Pre-increment on 1 -> still 1 (2 normalizes to 1)
    a = 1;
    ++a;
    if (a != 1) return 2;

    // Post-increment on 0
    a = 0;
    if (a++ != 0) return 3;  // Returns old value 0
    if (a != 1) return 4;    // New value is 1

    // Post-increment on 1
    a = 1;
    if (a++ != 1) return 5;  // Returns old value 1
    if (a != 1) return 6;    // New value is still 1 (2 normalizes)

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_incdec", code), 0);
}

// ============================================================================
// _Bool in Control Flow
// ============================================================================

#[test]
fn bool_control_flow() {
    let code = r#"
int main(void) {
    _Bool flag;
    int count;

    // if with _Bool
    flag = 1;
    if (flag) {
        count = 1;
    } else {
        return 1;
    }

    flag = 0;
    if (flag) {
        return 2;
    } else {
        count = 2;
    }

    // while with _Bool
    flag = 1;
    count = 0;
    while (flag) {
        count = count + 1;
        if (count >= 3) flag = 0;
    }
    if (count != 3) return 3;

    // for with _Bool condition
    count = 0;
    flag = 1;
    for (; flag; ) {
        count = count + 1;
        if (count >= 5) flag = 0;
    }
    if (count != 5) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_flow", code), 0);
}

// ============================================================================
// _Bool with Pointers
// ============================================================================

#[test]
fn bool_pointers() {
    let code = r#"
int main(void) {
    _Bool a, b;
    _Bool *p;

    a = 1;
    p = &a;

    // Read through pointer
    if (*p != 1) return 1;

    // Write through pointer
    *p = 0;
    if (a != 0) return 2;

    // Pointer to different _Bool
    b = 1;
    p = &b;
    if (*p != 1) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_ptr", code), 0);
}

// ============================================================================
// _Bool in Arrays
// ============================================================================

#[test]
fn bool_arrays() {
    let code = r#"
int main(void) {
    _Bool arr[4];
    int i;

    // Initialize array
    arr[0] = 0;
    arr[1] = 1;
    arr[2] = 42;  // Should normalize to 1
    arr[3] = 0;

    // Verify values
    if (arr[0] != 0) return 1;
    if (arr[1] != 1) return 2;
    if (arr[2] != 1) return 3;  // 42 normalized to 1
    if (arr[3] != 0) return 4;

    // Count true values
    int count = 0;
    for (i = 0; i < 4; i = i + 1) {
        if (arr[i]) count = count + 1;
    }
    if (count != 2) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_array", code), 0);
}

// ============================================================================
// _Bool Type Conversions
// ============================================================================

#[test]
fn bool_type_conversions() {
    let code = r#"
int main(void) {
    _Bool b;
    int i;
    long l;
    char c;

    // _Bool to int
    b = 1;
    i = b;
    if (i != 1) return 1;

    b = 0;
    i = b;
    if (i != 0) return 2;

    // _Bool to long
    b = 1;
    l = b;
    if (l != 1) return 3;

    // _Bool to char
    b = 1;
    c = b;
    if (c != 1) return 4;

    // int to _Bool (explicit)
    i = 100;
    b = (_Bool)i;
    if (b != 1) return 5;

    // Comparison result (always 0 or 1)
    i = 10;
    b = (i > 5);  // true
    if (b != 1) return 6;

    b = (i < 5);  // false
    if (b != 0) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_typeconv", code), 0);
}

// ============================================================================
// _Bool: Structs, Functions, and Initializers
// ============================================================================

#[test]
fn bool_structs_functions_initializers() {
    let code = r#"
// Struct with _Bool member
struct bool_container {
    _Bool flag;
    _Bool active;
};

// Function taking _Bool parameter and returning _Bool
_Bool negate_bool(_Bool b) {
    return !b;
}

// Function taking struct parameter
int count_true(struct bool_container c) {
    int count = 0;
    if (c.flag) count = count + 1;
    if (c.active) count = count + 1;
    return count;
}

// Function returning struct
struct bool_container make_container(_Bool a, _Bool b) {
    struct bool_container c;
    c.flag = a;
    c.active = b;
    return c;
}

int main(void) {
    // Variable initializers
    _Bool t = 1;
    if (t != 1) return 1;

    _Bool f = 0;
    if (f != 0) return 2;

    // Initializer with non-zero value (should normalize to 1)
    _Bool norm = 42;
    if (norm != 1) return 3;

    // Struct member access
    struct bool_container bc;
    bc.flag = 1;
    bc.active = 0;
    if (bc.flag != 1) return 4;
    if (bc.active != 0) return 5;

    // _Bool normalization in struct member
    bc.flag = 100;  // Should normalize to 1
    if (bc.flag != 1) return 6;

    // Function with _Bool param/return
    if (negate_bool(1) != 0) return 7;
    if (negate_bool(0) != 1) return 8;

    // Struct as function parameter
    bc.flag = 1;
    bc.active = 1;
    if (count_true(bc) != 2) return 9;

    bc.flag = 1;
    bc.active = 0;
    if (count_true(bc) != 1) return 10;

    // Struct as function return
    struct bool_container bc2;
    bc2 = make_container(1, 0);
    if (bc2.flag != 1) return 11;
    if (bc2.active != 0) return 12;

    // Pointer to _Bool in struct
    struct bool_container *p;
    p = &bc;
    if (p->flag != 1) return 13;
    p->flag = 0;
    if (bc.flag != 0) return 14;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_advanced", code), 0);
}
