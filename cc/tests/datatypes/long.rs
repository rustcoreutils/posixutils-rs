//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `long` and `unsigned long` data types with all applicable operators
// Tests are aggregated by category to reduce compile/link overhead
//
// Return code convention:
//   - long tests: return codes 1-99
//   - unsigned long tests: return codes 101-199
//

use crate::common::compile_and_run;

// ============================================================================
// Arithmetic Operators (add, sub, mul, div, mod, unary neg, unary plus)
// ============================================================================

#[test]
fn long_arithmetic_operators() {
    let code = r#"
int main(void) {
    // === long arithmetic (returns 1-20) ===
    {
        long a, b;

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

        // L suffix for long literals
        if (100L + 23L != 123L) return 8;

        // Hex with L suffix
        if (0x10L + 0x20L != 0x30L) return 9;

        // Mixed case suffix (l)
        if (50l + 50l != 100l) return 10;
    }

    // === unsigned long arithmetic (returns 101-120) ===
    {
        unsigned long a, b;

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

        // UL suffix for unsigned long literals
        if (100UL + 23UL != 123UL) return 107;

        // Hex with UL suffix
        if (0x10UL + 0x20UL != 0x30UL) return 108;

        // Mixed case/order suffixes (ul, Lu, lU)
        if (50ul + 50ul != 100ul) return 109;
        if (25LU + 25LU != 50LU) return 110;

        // Large unsigned long value (tests u64 parsing)
        a = 0xFFFFFFFFFFFFFF00UL;
        if ((a & 0xFF) != 0) return 111;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("long_arith", code, &[]), 0);
}

// ============================================================================
// Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn long_comparison_operators() {
    let code = r#"
int main(void) {
    // === long comparison (returns 1-20) ===
    {
        long a, b;

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

    // === unsigned long comparison (returns 101-120) ===
    {
        unsigned long a, b;

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
    assert_eq!(compile_and_run("long_cmp", code, &[]), 0);
}

// ============================================================================
// Logical Operators (&&, ||, !)
// ============================================================================

#[test]
fn long_logical_operators() {
    let code = r#"
int main(void) {
    // === long logical (returns 1-10) ===
    {
        long a, b;

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

    // === unsigned long logical (returns 101-110) ===
    {
        unsigned long a, b;

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
    assert_eq!(compile_and_run("long_logical", code, &[]), 0);
}

// ============================================================================
// Bitwise Operators (&, |, ^, ~, <<, >>)
// ============================================================================

#[test]
fn long_bitwise_operators() {
    let code = r#"
int main(void) {
    // === long bitwise (returns 1-10) ===
    {
        long a, b;

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

    // === unsigned long bitwise (returns 101-110) ===
    {
        unsigned long a, b;

        // Bitwise AND
        a = 0xFF; b = 0x0F;
        if ((a & b) != 0x0F) return 101;

        // Bitwise OR
        a = 0xF0; b = 0x0F;
        if ((a | b) != 0xFF) return 102;

        // Bitwise XOR
        a = 0xFF; b = 0xF0;
        if ((a ^ b) != 0x0F) return 103;

        // Bitwise NOT - check low byte is 0xFF when input is 0xFFFFFFFFFFFFFF00
        a = 0xFFFFFFFFFFFFFF00UL;
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
    assert_eq!(compile_and_run("long_bitwise", code, &[]), 0);
}

// ============================================================================
// Assignment Operators (=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=)
// ============================================================================

#[test]
fn long_assignment_operators() {
    let code = r#"
int main(void) {
    // === long assignment (returns 1-15) ===
    {
        long a;

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

    // === unsigned long assignment (returns 101-115) ===
    {
        unsigned long a;

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
    assert_eq!(compile_and_run("long_assign", code, &[]), 0);
}

// ============================================================================
// Increment/Decrement Operators (++a, a++, --a, a--)
// ============================================================================

#[test]
fn long_increment_decrement_operators() {
    let code = r#"
int main(void) {
    // === long increment/decrement (returns 1-10) ===
    {
        long a, b;

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

    // === unsigned long increment/decrement (returns 101-110) ===
    {
        unsigned long a, b;

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
    assert_eq!(compile_and_run("long_incdec", code, &[]), 0);
}

// ============================================================================
// Ternary and Comma Operators
// ============================================================================

#[test]
fn long_ternary_comma_operators() {
    let code = r#"
int main(void) {
    // === long ternary/comma (returns 1-5) ===
    {
        long a, b;

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

    // === unsigned long ternary/comma (returns 101-105) ===
    {
        unsigned long a, b;

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
    assert_eq!(compile_and_run("long_ternary_comma", code, &[]), 0);
}

// ============================================================================
// Complex Expressions
// ============================================================================

#[test]
fn long_complex_expressions() {
    let code = r#"
int main(void) {
    // === long complex expressions (returns 1-5) ===
    {
        long a, b, c, result;

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

    // === unsigned long complex expressions (returns 101-105) ===
    {
        unsigned long a, b, c, result;

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
    assert_eq!(compile_and_run("long_complex", code, &[]), 0);
}

// ============================================================================
// Long: Structs, Functions, and Initializers
// ============================================================================

#[test]
fn long_structs_functions_initializers() {
    let code = r#"
struct long_container {
    long value;
    long second;
};

struct ulong_container {
    unsigned long value;
};

long double_long(long x) {
    return x * 2;
}

unsigned long double_ulong(unsigned long x) {
    return x * 2;
}

long sum_container(struct long_container c) {
    return c.value + c.second;
}

struct long_container make_container(long a, long b) {
    struct long_container c;
    c.value = a;
    c.second = b;
    return c;
}

int main(void) {
    // Variable initializers
    long x = 42L;
    if (x != 42) return 1;

    unsigned long ux = 100UL;
    if (ux != 100) return 2;

    // Multiple variable declarations with initializers
    long a = 10L, b = 20L, c = 30L;
    if (a + b + c != 60) return 3;

    // Struct member access
    struct long_container lc;
    lc.value = 1000000L;
    lc.second = 500000L;
    if (lc.value != 1000000L) return 4;
    if (lc.second != 500000L) return 5;

    struct ulong_container ulc;
    ulc.value = 4000000000UL;
    if (ulc.value != 4000000000UL) return 6;

    // Function with long parameter and return
    long doubled = double_long(21L);
    if (doubled != 42) return 7;

    // Function with unsigned long parameter and return
    unsigned long udoubled = double_ulong(2000000000UL);
    if (udoubled != 4000000000UL) return 8;

    // Negative long
    long neg = -100L;
    if (double_long(neg) != -200) return 9;

    // Struct as function parameter
    struct long_container param;
    param.value = 100000L;
    param.second = 50000L;
    if (sum_container(param) != 150000L) return 10;

    // Struct as function return value
    struct long_container returned = make_container(300000L, 200000L);
    if (returned.value != 300000L) return 11;
    if (returned.second != 200000L) return 12;

    // Pointer to struct
    struct long_container target;
    struct long_container *ptr = &target;
    ptr->value = 2000000L;
    ptr->second = 1000000L;
    if (target.value != 2000000L) return 13;
    if (target.second != 1000000L) return 14;

    // Large long values (beyond 32-bit int range)
    long big = 3000000000L;
    if (big != 3000000000L) return 15;

    unsigned long ubig = 10000000000UL;
    if (ubig != 10000000000UL) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("long_advanced", code, &[]), 0);
}
