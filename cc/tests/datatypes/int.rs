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
// Return code convention:
//   - int tests: return codes 1-99
//   - unsigned int tests: return codes 101-199
//

use crate::common::compile_and_run;

// ============================================================================
// Arithmetic Operators (add, sub, mul, div, mod, unary neg, unary plus)
// ============================================================================

#[test]
fn integer_arithmetic_operators() {
    let code = r#"
int main(void) {
    // === int arithmetic (returns 1-20) ===
    {
        int a, b;

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
    }

    // === unsigned int arithmetic (returns 101-120) ===
    {
        unsigned int a, b;

        // Addition
        a = 30; b = 12;
        if (a + b != 42) return 101;

        // Subtraction
        a = 100; b = 58;
        if (a - b != 42) return 102;

        // Multiplication
        a = 6; b = 7;
        if (a * b != 42) return 103;

        // Division
        a = 84; b = 2;
        if (a / b != 42) return 104;

        // Modulo
        a = 47; b = 10;
        if (a % b != 7) return 105;

        // Unary plus
        a = 42;
        if (+a != 42) return 106;

        // U suffix for unsigned int literals
        if (100U + 23U != 123U) return 107;

        // Hex with U suffix
        if (0x10U + 0x20U != 0x30U) return 108;

        // Mixed case suffix (u)
        if (50u + 50u != 100u) return 109;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("integer_arith", code, &[]), 0);
}

// ============================================================================
// Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn integer_comparison_operators() {
    let code = r#"
int main(void) {
    // === int comparison (returns 1-20) ===
    {
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
    }

    // === unsigned int comparison (returns 101-120) ===
    {
        unsigned int a, b;

        // Equal - true
        a = 42; b = 42;
        if ((a == b) != 1) return 101;

        // Equal - false
        a = 42; b = 43;
        if ((a == b) != 0) return 102;

        // Not equal - true
        a = 42; b = 43;
        if ((a != b) != 1) return 103;

        // Not equal - false
        a = 42; b = 42;
        if ((a != b) != 0) return 104;

        // Less than - true
        a = 10; b = 20;
        if ((a < b) != 1) return 105;

        // Less than - false
        a = 20; b = 10;
        if ((a < b) != 0) return 106;

        // Less or equal - true (less)
        a = 10; b = 20;
        if ((a <= b) != 1) return 107;

        // Less or equal - true (equal)
        a = 20; b = 20;
        if ((a <= b) != 1) return 108;

        // Less or equal - false
        a = 30; b = 20;
        if ((a <= b) != 0) return 109;

        // Greater than - true
        a = 20; b = 10;
        if ((a > b) != 1) return 110;

        // Greater than - false
        a = 10; b = 20;
        if ((a > b) != 0) return 111;

        // Greater or equal - true (greater)
        a = 30; b = 20;
        if ((a >= b) != 1) return 112;

        // Greater or equal - true (equal)
        a = 20; b = 20;
        if ((a >= b) != 1) return 113;

        // Greater or equal - false
        a = 10; b = 20;
        if ((a >= b) != 0) return 114;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("integer_cmp", code, &[]), 0);
}

// ============================================================================
// Logical Operators (&&, ||, !)
// ============================================================================

#[test]
fn integer_logical_operators() {
    let code = r#"
int main(void) {
    // === int logical (returns 1-10) ===
    {
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
    }

    // === unsigned int logical (returns 101-110) ===
    {
        unsigned int a, b;

        // Logical AND - true
        a = 1; b = 1;
        if ((a && b) != 1) return 101;

        // Logical AND - false (left)
        a = 0; b = 1;
        if ((a && b) != 0) return 102;

        // Logical AND - false (right)
        a = 1; b = 0;
        if ((a && b) != 0) return 103;

        // Logical OR - true (left)
        a = 1; b = 0;
        if ((a || b) != 1) return 104;

        // Logical OR - true (right)
        a = 0; b = 1;
        if ((a || b) != 1) return 105;

        // Logical OR - false
        a = 0; b = 0;
        if ((a || b) != 0) return 106;

        // Logical NOT - true (input 0)
        a = 0;
        if (!a != 1) return 107;

        // Logical NOT - false (input non-zero)
        a = 42;
        if (!a != 0) return 108;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("integer_logical", code, &[]), 0);
}

// ============================================================================
// Bitwise Operators (&, |, ^, ~, <<, >>)
// ============================================================================

#[test]
fn integer_bitwise_operators() {
    let code = r#"
int main(void) {
    // === int bitwise (returns 1-10) ===
    {
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
    }

    // === unsigned int bitwise (returns 101-110) ===
    {
        unsigned int a, b;

        // Bitwise AND
        a = 0xFF; b = 0x0F;
        if ((a & b) != 0x0F) return 101;

        // Bitwise OR
        a = 0xF0; b = 0x0F;
        if ((a | b) != 0xFF) return 102;

        // Bitwise XOR
        a = 0xFF; b = 0xF0;
        if ((a ^ b) != 0x0F) return 103;

        // Bitwise NOT - check low byte is 0xFF when input is 0xFFFFFF00
        a = 0xFFFFFF00;
        if ((~a & 0xFF) != 0xFF) return 104;

        // Left shift
        a = 1;
        if ((a << 4) != 16) return 105;

        // Right shift (logical for unsigned)
        a = 64;
        if ((a >> 2) != 16) return 106;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("integer_bitwise", code, &[]), 0);
}

// ============================================================================
// Assignment Operators (=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=)
// ============================================================================

#[test]
fn integer_assignment_operators() {
    let code = r#"
int main(void) {
    // === int assignment (returns 1-15) ===
    {
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
    }

    // === unsigned int assignment (returns 101-115) ===
    {
        unsigned int a;

        // Simple assignment
        a = 42;
        if (a != 42) return 101;

        // Add assign
        a = 40;
        a += 2;
        if (a != 42) return 102;

        // Sub assign
        a = 50;
        a -= 8;
        if (a != 42) return 103;

        // Mul assign
        a = 21;
        a *= 2;
        if (a != 42) return 104;

        // Div assign
        a = 84;
        a /= 2;
        if (a != 42) return 105;

        // Mod assign
        a = 50;
        a %= 8;
        if (a != 2) return 106;

        // And assign
        a = 0xFF;
        a &= 0x0F;
        if (a != 15) return 107;

        // Or assign
        a = 0xF0;
        a |= 0x0F;
        if (a != 255) return 108;

        // Xor assign
        a = 0xFF;
        a ^= 0xF0;
        if (a != 15) return 109;

        // Shl assign
        a = 1;
        a <<= 4;
        if (a != 16) return 110;

        // Shr assign
        a = 64;
        a >>= 2;
        if (a != 16) return 111;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("integer_assign", code, &[]), 0);
}

// ============================================================================
// Increment/Decrement Operators (++a, a++, --a, a--)
// ============================================================================

#[test]
fn integer_increment_decrement_operators() {
    let code = r#"
int main(void) {
    // === int increment/decrement (returns 1-10) ===
    {
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
    }

    // === unsigned int increment/decrement (returns 101-110) ===
    {
        unsigned int a, b;

        // Pre-increment
        a = 41;
        if (++a != 42) return 101;

        // Post-increment (returns original)
        a = 42;
        b = a++;
        if (b != 42) return 102;

        // Post-increment (side effect)
        a = 41;
        a++;
        if (a != 42) return 103;

        // Pre-decrement
        a = 43;
        if (--a != 42) return 104;

        // Post-decrement (returns original)
        a = 42;
        b = a--;
        if (b != 42) return 105;

        // Post-decrement (side effect)
        a = 43;
        a--;
        if (a != 42) return 106;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("integer_incdec", code, &[]), 0);
}

// ============================================================================
// Ternary and Comma Operators
// ============================================================================

#[test]
fn integer_ternary_comma_operators() {
    let code = r#"
int main(void) {
    // === int ternary/comma (returns 1-5) ===
    {
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
    }

    // === unsigned int ternary/comma (returns 101-105) ===
    {
        unsigned int a, b;

        // Ternary - true branch
        a = 1;
        if ((a ? 42 : 0) != 42) return 101;

        // Ternary - false branch
        a = 0;
        if ((a ? 0 : 42) != 42) return 102;

        // Comma operator
        a = 1;
        b = (a = 10, a + 32);
        if (b != 42) return 103;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("integer_ternary_comma", code, &[]), 0);
}

// ============================================================================
// Complex Expressions
// ============================================================================

#[test]
fn integer_complex_expressions() {
    let code = r#"
int main(void) {
    // === int complex expressions (returns 1-5) ===
    {
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
    }

    // === unsigned int complex expressions (returns 101-105) ===
    {
        unsigned int a, b, c, result;

        // Mixed arithmetic with precedence
        a = 10; b = 5; c = 2;
        result = a + b * c - 3;  // 10 + 10 - 3 = 17
        if (result != 17) return 101;

        // Chained comparison
        a = 5; b = 10; c = 15;
        if (((a < b) && (b < c)) != 1) return 102;

        // Mixed bitwise and arithmetic
        a = 8; b = 3;
        result = ((a | b) & 0x0F) + (a >> 1);  // (11 & 15) + 4 = 15
        if (result != 15) return 103;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("integer_complex", code, &[]), 0);
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
    assert_eq!(compile_and_run("int_advanced", code, &[]), 0);
}
