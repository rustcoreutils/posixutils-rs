//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `int` and `unsigned int` data types with all applicable operators
// Tests are aggregated by category to reduce compile/link overhead
//

use crate::common::compile_and_run;

// ============================================================================
// Arithmetic Operators (add, sub, mul, div, mod, unary neg, unary plus)
// ============================================================================

#[test]
fn int_arithmetic_operators() {
    let code = r#"
int main(void) {
    int a, b, result;

    // Addition
    a = 30; b = 12;
    if (a + b != 42) return 1;

    // Subtraction
    a = 100; b = 58;
    if (a - b != 42) return 2;

    // Multiplication
    a = 6; b = 7;
    if (a * b != 42) return 3;

    // Division
    a = 84; b = 2;
    if (a / b != 42) return 4;

    // Modulo
    a = 47; b = 10;
    if (a % b != 7) return 5;

    // Unary negation
    a = -42;
    if (-a != 42) return 6;

    // Unary plus
    a = 42;
    if (+a != 42) return 7;

    // Literal without suffix (default int)
    if (100 + 23 != 123) return 8;

    // Hex literal
    if (0x10 + 0x20 != 0x30) return 9;

    // Octal literal
    if (010 + 010 != 020) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int_arith", code), 0);
}

// ============================================================================
// Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn int_comparison_operators() {
    let code = r#"
int main(void) {
    int a, b;

    // Equal - true
    a = 42; b = 42;
    if ((a == b) != 1) return 1;

    // Equal - false
    a = 42; b = 43;
    if ((a == b) != 0) return 2;

    // Not equal - true
    a = 42; b = 43;
    if ((a != b) != 1) return 3;

    // Not equal - false
    a = 42; b = 42;
    if ((a != b) != 0) return 4;

    // Less than - true
    a = 10; b = 20;
    if ((a < b) != 1) return 5;

    // Less than - false
    a = 20; b = 10;
    if ((a < b) != 0) return 6;

    // Less or equal - true (less)
    a = 10; b = 20;
    if ((a <= b) != 1) return 7;

    // Less or equal - true (equal)
    a = 20; b = 20;
    if ((a <= b) != 1) return 8;

    // Less or equal - false
    a = 30; b = 20;
    if ((a <= b) != 0) return 9;

    // Greater than - true
    a = 20; b = 10;
    if ((a > b) != 1) return 10;

    // Greater than - false
    a = 10; b = 20;
    if ((a > b) != 0) return 11;

    // Greater or equal - true (greater)
    a = 30; b = 20;
    if ((a >= b) != 1) return 12;

    // Greater or equal - true (equal)
    a = 20; b = 20;
    if ((a >= b) != 1) return 13;

    // Greater or equal - false
    a = 10; b = 20;
    if ((a >= b) != 0) return 14;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int_cmp", code), 0);
}

// ============================================================================
// Logical Operators (&&, ||, !)
// ============================================================================

#[test]
fn int_logical_operators() {
    let code = r#"
int main(void) {
    int a, b;

    // Logical AND - true
    a = 1; b = 1;
    if ((a && b) != 1) return 1;

    // Logical AND - false (left)
    a = 0; b = 1;
    if ((a && b) != 0) return 2;

    // Logical AND - false (right)
    a = 1; b = 0;
    if ((a && b) != 0) return 3;

    // Logical OR - true (left)
    a = 1; b = 0;
    if ((a || b) != 1) return 4;

    // Logical OR - true (right)
    a = 0; b = 1;
    if ((a || b) != 1) return 5;

    // Logical OR - false
    a = 0; b = 0;
    if ((a || b) != 0) return 6;

    // Logical NOT - true (input 0)
    a = 0;
    if (!a != 1) return 7;

    // Logical NOT - false (input non-zero)
    a = 42;
    if (!a != 0) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int_logical", code), 0);
}

// ============================================================================
// Bitwise Operators (&, |, ^, ~, <<, >>)
// ============================================================================

#[test]
fn int_bitwise_operators() {
    let code = r#"
int main(void) {
    int a, b;

    // Bitwise AND
    a = 0xFF; b = 0x0F;
    if ((a & b) != 0x0F) return 1;

    // Bitwise OR
    a = 0xF0; b = 0x0F;
    if ((a | b) != 0xFF) return 2;

    // Bitwise XOR
    a = 0xFF; b = 0xF0;
    if ((a ^ b) != 0x0F) return 3;

    // Bitwise NOT
    a = 0;
    if (~a != -1) return 4;

    // Left shift
    a = 1;
    if ((a << 4) != 16) return 5;

    // Right shift
    a = 64;
    if ((a >> 2) != 16) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int_bitwise", code), 0);
}

// ============================================================================
// Assignment Operators (=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=)
// ============================================================================

#[test]
fn int_assignment_operators() {
    let code = r#"
int main(void) {
    int a;

    // Simple assignment
    a = 42;
    if (a != 42) return 1;

    // Add assign
    a = 40;
    a += 2;
    if (a != 42) return 2;

    // Sub assign
    a = 50;
    a -= 8;
    if (a != 42) return 3;

    // Mul assign
    a = 21;
    a *= 2;
    if (a != 42) return 4;

    // Div assign
    a = 84;
    a /= 2;
    if (a != 42) return 5;

    // Mod assign
    a = 50;
    a %= 8;
    if (a != 2) return 6;

    // And assign
    a = 0xFF;
    a &= 0x0F;
    if (a != 15) return 7;

    // Or assign
    a = 0xF0;
    a |= 0x0F;
    if (a != 255) return 8;

    // Xor assign
    a = 0xFF;
    a ^= 0xF0;
    if (a != 15) return 9;

    // Shl assign
    a = 1;
    a <<= 4;
    if (a != 16) return 10;

    // Shr assign
    a = 64;
    a >>= 2;
    if (a != 16) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int_assign", code), 0);
}

// ============================================================================
// Increment/Decrement Operators (++a, a++, --a, a--)
// ============================================================================

#[test]
fn int_increment_decrement_operators() {
    let code = r#"
int main(void) {
    int a, b;

    // Pre-increment
    a = 41;
    if (++a != 42) return 1;

    // Post-increment (returns original)
    a = 42;
    b = a++;
    if (b != 42) return 2;

    // Post-increment (side effect)
    a = 41;
    a++;
    if (a != 42) return 3;

    // Pre-decrement
    a = 43;
    if (--a != 42) return 4;

    // Post-decrement (returns original)
    a = 42;
    b = a--;
    if (b != 42) return 5;

    // Post-decrement (side effect)
    a = 43;
    a--;
    if (a != 42) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int_incdec", code), 0);
}

// ============================================================================
// Ternary and Comma Operators
// ============================================================================

#[test]
fn int_ternary_comma_operators() {
    let code = r#"
int main(void) {
    int a, b;

    // Ternary - true branch
    a = 1;
    if ((a ? 42 : 0) != 42) return 1;

    // Ternary - false branch
    a = 0;
    if ((a ? 0 : 42) != 42) return 2;

    // Comma operator
    a = 1;
    b = (a = 10, a + 32);
    if (b != 42) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int_ternary_comma", code), 0);
}

// ============================================================================
// Complex Expressions
// ============================================================================

#[test]
fn int_complex_expressions() {
    let code = r#"
int main(void) {
    int a, b, c, result;

    // Mixed arithmetic with precedence
    a = 10; b = 5; c = 2;
    result = a + b * c - 3;  // 10 + 10 - 3 = 17
    if (result != 17) return 1;

    // Chained comparison
    a = 5; b = 10; c = 15;
    if (((a < b) && (b < c)) != 1) return 2;

    // Mixed bitwise and arithmetic
    a = 8; b = 3;
    result = ((a | b) & 0x0F) + (a >> 1);  // (11 & 15) + 4 = 15
    if (result != 15) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int_complex", code), 0);
}

// ############################################################################
// UNSIGNED INT TESTS
// ############################################################################

// ============================================================================
// Unsigned Int: Arithmetic Operators (add, sub, mul, div, mod, unary plus)
// ============================================================================

#[test]
fn uint_arithmetic_operators() {
    let code = r#"
int main(void) {
    unsigned int a, b;

    // Addition
    a = 30; b = 12;
    if (a + b != 42) return 1;

    // Subtraction
    a = 100; b = 58;
    if (a - b != 42) return 2;

    // Multiplication
    a = 6; b = 7;
    if (a * b != 42) return 3;

    // Division
    a = 84; b = 2;
    if (a / b != 42) return 4;

    // Modulo
    a = 47; b = 10;
    if (a % b != 7) return 5;

    // Unary plus
    a = 42;
    if (+a != 42) return 6;

    // U suffix for unsigned int literals
    if (100U + 23U != 123U) return 7;

    // Hex with U suffix
    if (0x10U + 0x20U != 0x30U) return 8;

    // Mixed case suffix (u)
    if (50u + 50u != 100u) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uint_arith", code), 0);
}

// ============================================================================
// Unsigned Int: Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn uint_comparison_operators() {
    let code = r#"
int main(void) {
    unsigned int a, b;

    // Equal - true
    a = 42; b = 42;
    if ((a == b) != 1) return 1;

    // Equal - false
    a = 42; b = 43;
    if ((a == b) != 0) return 2;

    // Not equal - true
    a = 42; b = 43;
    if ((a != b) != 1) return 3;

    // Not equal - false
    a = 42; b = 42;
    if ((a != b) != 0) return 4;

    // Less than - true
    a = 10; b = 20;
    if ((a < b) != 1) return 5;

    // Less than - false
    a = 20; b = 10;
    if ((a < b) != 0) return 6;

    // Less or equal - true (less)
    a = 10; b = 20;
    if ((a <= b) != 1) return 7;

    // Less or equal - true (equal)
    a = 20; b = 20;
    if ((a <= b) != 1) return 8;

    // Less or equal - false
    a = 30; b = 20;
    if ((a <= b) != 0) return 9;

    // Greater than - true
    a = 20; b = 10;
    if ((a > b) != 1) return 10;

    // Greater than - false
    a = 10; b = 20;
    if ((a > b) != 0) return 11;

    // Greater or equal - true (greater)
    a = 30; b = 20;
    if ((a >= b) != 1) return 12;

    // Greater or equal - true (equal)
    a = 20; b = 20;
    if ((a >= b) != 1) return 13;

    // Greater or equal - false
    a = 10; b = 20;
    if ((a >= b) != 0) return 14;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uint_cmp", code), 0);
}

// ============================================================================
// Unsigned Int: Logical Operators (&&, ||, !)
// ============================================================================

#[test]
fn uint_logical_operators() {
    let code = r#"
int main(void) {
    unsigned int a, b;

    // Logical AND - true
    a = 1; b = 1;
    if ((a && b) != 1) return 1;

    // Logical AND - false (left)
    a = 0; b = 1;
    if ((a && b) != 0) return 2;

    // Logical AND - false (right)
    a = 1; b = 0;
    if ((a && b) != 0) return 3;

    // Logical OR - true (left)
    a = 1; b = 0;
    if ((a || b) != 1) return 4;

    // Logical OR - true (right)
    a = 0; b = 1;
    if ((a || b) != 1) return 5;

    // Logical OR - false
    a = 0; b = 0;
    if ((a || b) != 0) return 6;

    // Logical NOT - true (input 0)
    a = 0;
    if (!a != 1) return 7;

    // Logical NOT - false (input non-zero)
    a = 42;
    if (!a != 0) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uint_logical", code), 0);
}

// ============================================================================
// Unsigned Int: Bitwise Operators (&, |, ^, ~, <<, >>)
// ============================================================================

#[test]
fn uint_bitwise_operators() {
    let code = r#"
int main(void) {
    unsigned int a, b;

    // Bitwise AND
    a = 0xFF; b = 0x0F;
    if ((a & b) != 0x0F) return 1;

    // Bitwise OR
    a = 0xF0; b = 0x0F;
    if ((a | b) != 0xFF) return 2;

    // Bitwise XOR
    a = 0xFF; b = 0xF0;
    if ((a ^ b) != 0x0F) return 3;

    // Bitwise NOT - check low byte is 0xFF when input is 0xFFFFFF00
    a = 0xFFFFFF00;
    if ((~a & 0xFF) != 0xFF) return 4;

    // Left shift
    a = 1;
    if ((a << 4) != 16) return 5;

    // Right shift (logical for unsigned)
    a = 64;
    if ((a >> 2) != 16) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uint_bitwise", code), 0);
}

// ============================================================================
// Unsigned Int: Assignment Operators (=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=)
// ============================================================================

#[test]
fn uint_assignment_operators() {
    let code = r#"
int main(void) {
    unsigned int a;

    // Simple assignment
    a = 42;
    if (a != 42) return 1;

    // Add assign
    a = 40;
    a += 2;
    if (a != 42) return 2;

    // Sub assign
    a = 50;
    a -= 8;
    if (a != 42) return 3;

    // Mul assign
    a = 21;
    a *= 2;
    if (a != 42) return 4;

    // Div assign
    a = 84;
    a /= 2;
    if (a != 42) return 5;

    // Mod assign
    a = 50;
    a %= 8;
    if (a != 2) return 6;

    // And assign
    a = 0xFF;
    a &= 0x0F;
    if (a != 15) return 7;

    // Or assign
    a = 0xF0;
    a |= 0x0F;
    if (a != 255) return 8;

    // Xor assign
    a = 0xFF;
    a ^= 0xF0;
    if (a != 15) return 9;

    // Shl assign
    a = 1;
    a <<= 4;
    if (a != 16) return 10;

    // Shr assign
    a = 64;
    a >>= 2;
    if (a != 16) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uint_assign", code), 0);
}

// ============================================================================
// Unsigned Int: Increment/Decrement Operators (++a, a++, --a, a--)
// ============================================================================

#[test]
fn uint_increment_decrement_operators() {
    let code = r#"
int main(void) {
    unsigned int a, b;

    // Pre-increment
    a = 41;
    if (++a != 42) return 1;

    // Post-increment (returns original)
    a = 42;
    b = a++;
    if (b != 42) return 2;

    // Post-increment (side effect)
    a = 41;
    a++;
    if (a != 42) return 3;

    // Pre-decrement
    a = 43;
    if (--a != 42) return 4;

    // Post-decrement (returns original)
    a = 42;
    b = a--;
    if (b != 42) return 5;

    // Post-decrement (side effect)
    a = 43;
    a--;
    if (a != 42) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uint_incdec", code), 0);
}

// ============================================================================
// Unsigned Int: Ternary and Comma Operators
// ============================================================================

#[test]
fn uint_ternary_comma_operators() {
    let code = r#"
int main(void) {
    unsigned int a, b;

    // Ternary - true branch
    a = 1;
    if ((a ? 42 : 0) != 42) return 1;

    // Ternary - false branch
    a = 0;
    if ((a ? 0 : 42) != 42) return 2;

    // Comma operator
    a = 1;
    b = (a = 10, a + 32);
    if (b != 42) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uint_ternary_comma", code), 0);
}

// ============================================================================
// Unsigned Int: Complex Expressions
// ============================================================================

#[test]
fn uint_complex_expressions() {
    let code = r#"
int main(void) {
    unsigned int a, b, c, result;

    // Mixed arithmetic with precedence
    a = 10; b = 5; c = 2;
    result = a + b * c - 3;  // 10 + 10 - 3 = 17
    if (result != 17) return 1;

    // Chained comparison
    a = 5; b = 10; c = 15;
    if (((a < b) && (b < c)) != 1) return 2;

    // Mixed bitwise and arithmetic
    a = 8; b = 3;
    result = ((a | b) & 0x0F) + (a >> 1);  // (11 & 15) + 4 = 15
    if (result != 15) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uint_complex", code), 0);
}

// ============================================================================
// Int: Structs, Functions, and Initializers
// ============================================================================

#[test]
fn int_structs_functions_initializers() {
    let code = r#"
// Struct with int members
struct int_container {
    int value;
    int second;
};

// Struct with unsigned int members
struct uint_container {
    unsigned int value;
};

// Function taking int parameter and returning int
int double_int(int x) {
    return x * 2;
}

// Function taking unsigned int parameter and returning unsigned int
unsigned int double_uint(unsigned int x) {
    return x * 2;
}

// Function taking struct parameter
int sum_container(struct int_container c) {
    return c.value + c.second;
}

// Function returning struct
struct int_container make_container(int a, int b) {
    struct int_container c;
    c.value = a;
    c.second = b;
    return c;
}

int main(void) {
    // Variable initializers
    int x = 42;
    if (x != 42) return 1;

    unsigned int ux = 100U;
    if (ux != 100) return 2;

    int a = 10, b = 20, c = 30;
    if (a + b + c != 60) return 3;

    // Struct member access
    struct int_container ic;
    ic.value = 42;
    ic.second = 58;
    if (ic.value != 42) return 4;
    if (ic.second != 58) return 5;

    // Unsigned in struct
    struct uint_container uc;
    uc.value = 0xFFFFFFFF;
    if (uc.value != 0xFFFFFFFF) return 6;

    // Function with int param/return
    if (double_int(21) != 42) return 7;
    if (double_int(-21) != -42) return 8;

    // Function with unsigned int param/return
    if (double_uint(21) != 42) return 9;

    // Struct as function parameter
    if (sum_container(ic) != 100) return 10;

    // Struct as function return
    struct int_container ic2;
    ic2 = make_container(10, 20);
    if (ic2.value != 10) return 11;
    if (ic2.second != 20) return 12;

    // Pointer to int in struct
    struct int_container *p;
    p = &ic;
    if (p->value != 42) return 13;
    p->value = 100;
    if (ic.value != 100) return 14;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int_advanced", code), 0);
}
