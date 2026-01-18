//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `long long` and `unsigned long long` data types with all applicable operators
// Tests are aggregated by category to reduce compile/link overhead
//
// Return code convention:
//   - long long tests: return codes 1-99
//   - unsigned long long tests: return codes 101-199
//

use crate::common::compile_and_run;

// ============================================================================
// Arithmetic Operators (add, sub, mul, div, mod, unary neg, unary plus)
// ============================================================================

#[test]
fn longlong_arithmetic_operators() {
    let code = r#"
int main(void) {
    // === long long arithmetic (returns 1-20) ===
    {
        long long a, b;

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

        // LL suffix for long long literals
        if (100LL + 23LL != 123LL) return 8;

        // Hex with LL suffix
        if (0x10LL + 0x20LL != 0x30LL) return 9;

        // Mixed case suffix (ll)
        if (50ll + 50ll != 100ll) return 10;

        // Large long long value (beyond 32-bit range)
        a = 0x100000000LL;  // 2^32
        b = 0x100000000LL;
        if (a + b != 0x200000000LL) return 11;

        // Multiplication producing large result
        a = 1000000LL;
        b = 1000000LL;
        if (a * b != 1000000000000LL) return 12;
    }

    // === unsigned long long arithmetic (returns 101-120) ===
    {
        unsigned long long a, b;

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

        // ULL suffix for unsigned long long literals
        if (100ULL + 23ULL != 123ULL) return 107;

        // Hex with ULL suffix
        if (0x10ULL + 0x20ULL != 0x30ULL) return 108;

        // Mixed case/order suffixes (ull, LLU, llu)
        if (50ull + 50ull != 100ull) return 109;
        if (25LLU + 25LLU != 50LLU) return 110;

        // Large unsigned long long value (beyond 32-bit range)
        a = 0x100000000ULL;
        b = 0x100000000ULL;
        if (a + b != 0x200000000ULL) return 111;

        // Very large value (tests full 64-bit range)
        a = 0xFFFFFFFFFFFFFF00ULL;
        if ((a & 0xFF) != 0) return 112;

        // Multiplication producing large result
        a = 0x100000000ULL;
        b = 16ULL;
        if (a * b != 0x1000000000ULL) return 113;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("longlong_arith", code, &[]), 0);
}

// ============================================================================
// Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn longlong_comparison_operators() {
    let code = r#"
int main(void) {
    // === long long comparison (returns 1-20) ===
    {
        long long a, b;

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

        // Large value comparisons (beyond 32-bit range)
        a = 0x100000000LL;
        b = 0x100000001LL;
        if ((a < b) != 1) return 15;
        if ((a > b) != 0) return 16;
    }

    // === unsigned long long comparison (returns 101-120) ===
    {
        unsigned long long a, b;

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

        // Large value comparisons (beyond 32-bit range)
        a = 0x100000000ULL;
        b = 0x100000001ULL;
        if ((a < b) != 1) return 115;
        if ((a > b) != 0) return 116;

        // Very large values
        a = 0xFFFFFFFFFFFFFFFEULL;
        b = 0xFFFFFFFFFFFFFFFFULL;
        if ((a < b) != 1) return 117;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("longlong_cmp", code, &[]), 0);
}

// ============================================================================
// Logical Operators (&&, ||, !)
// ============================================================================

#[test]
fn longlong_logical_operators() {
    let code = r#"
int main(void) {
    // === long long logical (returns 1-10) ===
    {
        long long a, b;

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

        // Large non-zero value is truthy
        a = 0x100000000LL;
        if (!a != 0) return 9;
    }

    // === unsigned long long logical (returns 101-110) ===
    {
        unsigned long long a, b;

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

        // Large non-zero value is truthy
        a = 0xFFFFFFFFFFFFFFFFULL;
        if (!a != 0) return 109;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("longlong_logical", code, &[]), 0);
}

// ============================================================================
// Bitwise Operators (&, |, ^, ~, <<, >>)
// ============================================================================

#[test]
fn longlong_bitwise_operators() {
    let code = r#"
int main(void) {
    // === long long bitwise (returns 1-10) ===
    {
        long long a, b;

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

        // Right shift (arithmetic for signed)
        a = 64;
        if ((a >> 2) != 16) return 6;

        // Large shifts (beyond 32 bits)
        a = 1LL;
        if ((a << 40) != 0x10000000000LL) return 7;

        // Bitwise ops on large values
        a = 0xFF00000000LL;
        b = 0x00FF000000LL;
        if ((a | b) != 0xFFFF000000LL) return 8;
    }

    // === unsigned long long bitwise (returns 101-110) ===
    {
        unsigned long long a, b;

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
        a = 0xFFFFFFFFFFFFFF00ULL;
        if ((~a & 0xFF) != 0xFF) return 104;

        // Left shift
        a = 1;
        if ((a << 4) != 16) return 105;

        // Right shift (logical for unsigned)
        a = 64;
        if ((a >> 2) != 16) return 106;

        // Large shifts (beyond 32 bits)
        a = 1ULL;
        if ((a << 40) != 0x10000000000ULL) return 107;

        // Shift high bit (logical shift, not arithmetic)
        a = 0x8000000000000000ULL;
        if ((a >> 63) != 1) return 108;

        // Bitwise ops on large values
        a = 0xFF00000000000000ULL;
        b = 0x00FF000000000000ULL;
        if ((a | b) != 0xFFFF000000000000ULL) return 109;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("longlong_bitwise", code, &[]), 0);
}

// ============================================================================
// Assignment Operators (=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=)
// ============================================================================

#[test]
fn longlong_assignment_operators() {
    let code = r#"
int main(void) {
    // === long long assignment (returns 1-15) ===
    {
        long long a;

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

        // Large value assignment
        a = 0x123456789ABCLL;
        if (a != 0x123456789ABCLL) return 12;
    }

    // === unsigned long long assignment (returns 101-115) ===
    {
        unsigned long long a;

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

        // Large value assignment
        a = 0xFEDCBA9876543210ULL;
        if (a != 0xFEDCBA9876543210ULL) return 112;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("longlong_assign", code, &[]), 0);
}

// ============================================================================
// Increment/Decrement Operators (++a, a++, --a, a--)
// ============================================================================

#[test]
fn longlong_increment_decrement_operators() {
    let code = r#"
int main(void) {
    // === long long increment/decrement (returns 1-10) ===
    {
        long long a, b;

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

        // Increment on large value
        a = 0xFFFFFFFFLL;
        a++;
        if (a != 0x100000000LL) return 7;
    }

    // === unsigned long long increment/decrement (returns 101-110) ===
    {
        unsigned long long a, b;

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

        // Increment crossing 32-bit boundary
        a = 0xFFFFFFFFULL;
        a++;
        if (a != 0x100000000ULL) return 107;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("longlong_incdec", code, &[]), 0);
}

// ============================================================================
// Ternary and Comma Operators
// ============================================================================

#[test]
fn longlong_ternary_comma_operators() {
    let code = r#"
int main(void) {
    // === long long ternary/comma (returns 1-5) ===
    {
        long long a, b;

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

        // Ternary with large values
        a = 0x100000000LL;
        b = (a ? 0x200000000LL : 0);
        if (b != 0x200000000LL) return 4;
    }

    // === unsigned long long ternary/comma (returns 101-105) ===
    {
        unsigned long long a, b;

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

        // Ternary with large values
        a = 0x100000000ULL;
        b = (a ? 0x200000000ULL : 0);
        if (b != 0x200000000ULL) return 104;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("longlong_ternary_comma", code, &[]), 0);
}

// ============================================================================
// Complex Expressions
// ============================================================================

#[test]
fn longlong_complex_expressions() {
    let code = r#"
int main(void) {
    // === long long complex expressions (returns 1-5) ===
    {
        long long a, b, c, result;

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

        // Large value expressions
        a = 0x100000000LL;
        b = 0x200000000LL;
        result = (a + b) >> 1;
        if (result != 0x180000000LL) return 4;
    }

    // === unsigned long long complex expressions (returns 101-105) ===
    {
        unsigned long long a, b, c, result;

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

        // Large value expressions
        a = 0x100000000ULL;
        b = 0x200000000ULL;
        result = (a + b) >> 1;
        if (result != 0x180000000ULL) return 104;

        // Division of large values
        a = 0x1000000000000ULL;
        b = 0x1000000ULL;
        if (a / b != 0x1000000ULL) return 105;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("longlong_complex", code, &[]), 0);
}

// ============================================================================
// Long Long: Structs, Functions, and Initializers
// ============================================================================

#[test]
fn longlong_structs_functions_initializers() {
    let code = r#"
struct llong_container {
    long long value;
    long long second;
};

struct ullong_container {
    unsigned long long value;
};

long long double_llong(long long x) {
    return x * 2;
}

unsigned long long double_ullong(unsigned long long x) {
    return x * 2;
}

long long sum_container(struct llong_container c) {
    return c.value + c.second;
}

struct llong_container make_container(long long a, long long b) {
    struct llong_container c;
    c.value = a;
    c.second = b;
    return c;
}

int main(void) {
    // Variable initializers
    long long x = 42LL;
    if (x != 42) return 1;

    unsigned long long ux = 100ULL;
    if (ux != 100) return 2;

    // Multiple variable declarations with initializers
    long long a = 10LL, b = 20LL, c = 30LL;
    if (a + b + c != 60) return 3;

    // Struct member access
    struct llong_container llc;
    llc.value = 1000000000000LL;
    llc.second = 500000000000LL;
    if (llc.value != 1000000000000LL) return 4;
    if (llc.second != 500000000000LL) return 5;

    struct ullong_container ullc;
    ullc.value = 10000000000000000000ULL;
    if (ullc.value != 10000000000000000000ULL) return 6;

    // Function with long long parameter and return
    long long doubled = double_llong(21LL);
    if (doubled != 42) return 7;

    // Function with unsigned long long parameter and return
    unsigned long long udoubled = double_ullong(5000000000ULL);
    if (udoubled != 10000000000ULL) return 8;

    // Negative long long
    long long neg = -100LL;
    if (double_llong(neg) != -200) return 9;

    // Struct as function parameter
    struct llong_container param;
    param.value = 100000000000LL;
    param.second = 50000000000LL;
    if (sum_container(param) != 150000000000LL) return 10;

    // Struct as function return value
    struct llong_container returned = make_container(300000000000LL, 200000000000LL);
    if (returned.value != 300000000000LL) return 11;
    if (returned.second != 200000000000LL) return 12;

    // Pointer to struct
    struct llong_container target;
    struct llong_container *ptr = &target;
    ptr->value = 2000000000000LL;
    ptr->second = 1000000000000LL;
    if (target.value != 2000000000000LL) return 13;
    if (target.second != 1000000000000LL) return 14;

    // Very large long long values
    long long big = 9000000000000000000LL;
    if (big != 9000000000000000000LL) return 15;

    unsigned long long ubig = 18000000000000000000ULL;
    if (ubig != 18000000000000000000ULL) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longlong_advanced", code, &[]), 0);
}
