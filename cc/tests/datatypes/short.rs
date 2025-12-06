//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `short` and `unsigned short` data types with all applicable operators
// Tests are aggregated by category to reduce compile/link overhead
//

use crate::common::compile_and_run;

// ============================================================================
// Arithmetic Operators (add, sub, mul, div, mod, unary neg, unary plus)
// ============================================================================

#[test]
fn short_arithmetic_operators() {
    let code = r#"
int main(void) {
    short a, b, result;

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

    // Literal operations (result fits in short range)
    if (100 + 23 != 123) return 8;

    // Hex literal
    if (0x10 + 0x20 != 0x30) return 9;

    // Octal literal
    if (010 + 010 != 020) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("short_arith", code), 0);
}

// ============================================================================
// Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn short_comparison_operators() {
    let code = r#"
int main(void) {
    short a, b;

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
    assert_eq!(compile_and_run("short_cmp", code), 0);
}

// ============================================================================
// Logical Operators (&&, ||, !)
// ============================================================================

#[test]
fn short_logical_operators() {
    let code = r#"
int main(void) {
    short a, b;

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
    assert_eq!(compile_and_run("short_logical", code), 0);
}

// ============================================================================
// Bitwise Operators (&, |, ^, ~, <<, >>)
// ============================================================================

#[test]
fn short_bitwise_operators() {
    let code = r#"
int main(void) {
    short a, b;

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
    assert_eq!(compile_and_run("short_bitwise", code), 0);
}

// ============================================================================
// Assignment Operators (=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=)
// ============================================================================

#[test]
fn short_assignment_operators() {
    let code = r#"
int main(void) {
    short a;

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
    assert_eq!(compile_and_run("short_assign", code), 0);
}

// ============================================================================
// Increment/Decrement Operators (++a, a++, --a, a--)
// ============================================================================

#[test]
fn short_increment_decrement_operators() {
    let code = r#"
int main(void) {
    short a, b;

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
    assert_eq!(compile_and_run("short_incdec", code), 0);
}

// ============================================================================
// Ternary and Comma Operators
// ============================================================================

#[test]
fn short_ternary_comma_operators() {
    let code = r#"
int main(void) {
    short a, b;

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
    assert_eq!(compile_and_run("short_ternary_comma", code), 0);
}

// ============================================================================
// Complex Expressions
// ============================================================================

#[test]
fn short_complex_expressions() {
    let code = r#"
int main(void) {
    short a, b, c, result;

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
    assert_eq!(compile_and_run("short_complex", code), 0);
}

// ############################################################################
// UNSIGNED SHORT TESTS
// ############################################################################

// ============================================================================
// Unsigned Short: Arithmetic Operators (add, sub, mul, div, mod, unary plus)
// ============================================================================

#[test]
fn ushort_arithmetic_operators() {
    let code = r#"
int main(void) {
    unsigned short a, b;

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

    // Literal operations
    if (100 + 23 != 123) return 7;

    // Hex literal
    if (0x10 + 0x20 != 0x30) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ushort_arith", code), 0);
}

// ============================================================================
// Unsigned Short: Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn ushort_comparison_operators() {
    let code = r#"
int main(void) {
    unsigned short a, b;

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
    assert_eq!(compile_and_run("ushort_cmp", code), 0);
}

// ============================================================================
// Unsigned Short: Logical Operators (&&, ||, !)
// ============================================================================

#[test]
fn ushort_logical_operators() {
    let code = r#"
int main(void) {
    unsigned short a, b;

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
    assert_eq!(compile_and_run("ushort_logical", code), 0);
}

// ============================================================================
// Unsigned Short: Bitwise Operators (&, |, ^, ~, <<, >>)
// ============================================================================

#[test]
fn ushort_bitwise_operators() {
    let code = r#"
int main(void) {
    unsigned short a, b;

    // Bitwise AND
    a = 0xFF; b = 0x0F;
    if ((a & b) != 0x0F) return 1;

    // Bitwise OR
    a = 0xF0; b = 0x0F;
    if ((a | b) != 0xFF) return 2;

    // Bitwise XOR
    a = 0xFF; b = 0xF0;
    if ((a ^ b) != 0x0F) return 3;

    // Bitwise NOT - check low byte is 0xFF when input is 0xFF00
    a = 0xFF00;
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
    assert_eq!(compile_and_run("ushort_bitwise", code), 0);
}

// ============================================================================
// Unsigned Short: Assignment Operators (=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=)
// ============================================================================

#[test]
fn ushort_assignment_operators() {
    let code = r#"
int main(void) {
    unsigned short a;

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
    assert_eq!(compile_and_run("ushort_assign", code), 0);
}

// ============================================================================
// Unsigned Short: Increment/Decrement Operators (++a, a++, --a, a--)
// ============================================================================

#[test]
fn ushort_increment_decrement_operators() {
    let code = r#"
int main(void) {
    unsigned short a, b;

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
    assert_eq!(compile_and_run("ushort_incdec", code), 0);
}

// ============================================================================
// Unsigned Short: Ternary and Comma Operators
// ============================================================================

#[test]
fn ushort_ternary_comma_operators() {
    let code = r#"
int main(void) {
    unsigned short a, b;

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
    assert_eq!(compile_and_run("ushort_ternary_comma", code), 0);
}

// ============================================================================
// Unsigned Short: Complex Expressions
// ============================================================================

#[test]
fn ushort_complex_expressions() {
    let code = r#"
int main(void) {
    unsigned short a, b, c, result;

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
    assert_eq!(compile_and_run("ushort_complex", code), 0);
}

// ============================================================================
// Short: Structs, Functions, and Initializers
// ============================================================================

#[test]
fn short_structs_functions_initializers() {
    let code = r#"
struct short_container {
    short value;
    short second;
};

struct ushort_container {
    unsigned short value;
};

short double_short(short x) {
    return x * 2;
}

unsigned short double_ushort(unsigned short x) {
    return x * 2;
}

int sum_container(struct short_container c) {
    return c.value + c.second;
}

struct short_container make_container(short a, short b) {
    struct short_container c;
    c.value = a;
    c.second = b;
    return c;
}

int main(void) {
    // Variable initializers
    short x = 42;
    if (x != 42) return 1;

    unsigned short ux = 100;
    if (ux != 100) return 2;

    // Multiple variable declarations with initializers
    short a = 10, b = 20, c = 30;
    if (a + b + c != 60) return 3;

    // Struct member access
    struct short_container sc;
    sc.value = 1000;
    sc.second = 500;
    if (sc.value != 1000) return 4;
    if (sc.second != 500) return 5;

    struct ushort_container usc;
    usc.value = 60000;
    if (usc.value != 60000) return 6;

    // Function with short parameter and return
    short doubled = double_short(21);
    if (doubled != 42) return 7;

    // Function with unsigned short parameter and return
    unsigned short udoubled = double_ushort(50);
    if (udoubled != 100) return 8;

    // Negative short
    short neg = -100;
    if (double_short(neg) != -200) return 9;

    // Struct as function parameter
    struct short_container param;
    param.value = 100;
    param.second = 50;
    if (sum_container(param) != 150) return 10;

    // Struct as function return value
    struct short_container returned = make_container(300, 200);
    if (returned.value != 300) return 11;
    if (returned.second != 200) return 12;

    // Pointer to struct
    struct short_container target;
    struct short_container *ptr = &target;
    ptr->value = 2000;
    ptr->second = 1000;
    if (target.value != 2000) return 13;
    if (target.second != 1000) return 14;

    // Short range edge cases
    short max_short = 32767;
    if (max_short != 32767) return 15;

    short min_short = -32768;
    if (min_short != -32768) return 16;

    unsigned short max_ushort = 65535;
    if (max_ushort != 65535) return 17;

    return 0;
}
"#;
    assert_eq!(compile_and_run("short_advanced", code), 0);
}
