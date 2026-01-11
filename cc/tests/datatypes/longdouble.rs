//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `long double` data type with all applicable operators
// Tests are aggregated by category to reduce compile/link overhead
//
// Note: On many platforms (including macOS ARM64), long double is equivalent
// to double. These tests verify the type is correctly recognized and behaves
// as expected for floating-point operations.
//

use crate::common::compile_and_run;

// ============================================================================
// Long Double Arithmetic Operators (add, sub, mul, div, unary neg, unary plus)
// ============================================================================

#[test]
fn longdouble_arithmetic_operators() {
    let code = r#"
int main(void) {
    long double a, b, result;

    // Addition
    a = 30.5L; b = 11.5L;
    result = a + b;
    if (result < 41.9L || result > 42.1L) return 1;

    // Subtraction
    a = 100.0L; b = 58.0L;
    result = a - b;
    if (result < 41.9L || result > 42.1L) return 2;

    // Multiplication
    a = 6.0L; b = 7.0L;
    result = a * b;
    if (result < 41.9L || result > 42.1L) return 3;

    // Division
    a = 84.0L; b = 2.0L;
    result = a / b;
    if (result < 41.9L || result > 42.1L) return 4;

    // Unary negation
    a = -42.0L;
    result = -a;
    if (result < 41.9L || result > 42.1L) return 5;

    // Unary plus
    a = 42.0L;
    result = +a;
    if (result < 41.9L || result > 42.1L) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longdouble_arith", code, &[]), 0);
}

// ============================================================================
// Long Double Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn longdouble_comparison_operators() {
    let code = r#"
int main(void) {
    long double a, b;

    // Less than - true
    a = 10.0L; b = 20.0L;
    if ((a < b) != 1) return 1;

    // Less than - false
    a = 20.0L; b = 10.0L;
    if ((a < b) != 0) return 2;

    // Less or equal - true (less)
    a = 10.0L; b = 20.0L;
    if ((a <= b) != 1) return 3;

    // Less or equal - true (equal)
    a = 20.0L; b = 20.0L;
    if ((a <= b) != 1) return 4;

    // Less or equal - false
    a = 30.0L; b = 20.0L;
    if ((a <= b) != 0) return 5;

    // Greater than - true
    a = 20.0L; b = 10.0L;
    if ((a > b) != 1) return 6;

    // Greater than - false
    a = 10.0L; b = 20.0L;
    if ((a > b) != 0) return 7;

    // Greater or equal - true (greater)
    a = 30.0L; b = 20.0L;
    if ((a >= b) != 1) return 8;

    // Greater or equal - true (equal)
    a = 20.0L; b = 20.0L;
    if ((a >= b) != 1) return 9;

    // Greater or equal - false
    a = 10.0L; b = 20.0L;
    if ((a >= b) != 0) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longdouble_cmp", code, &[]), 0);
}

// ============================================================================
// Long Double Assignment Operators (=, +=, -=, *=, /=)
// ============================================================================

#[test]
fn longdouble_assignment_operators() {
    let code = r#"
int main(void) {
    long double a;

    // Simple assignment
    a = 42.0L;
    if (a < 41.9L || a > 42.1L) return 1;

    // Add assign
    a = 40.0L;
    a += 2.0L;
    if (a < 41.9L || a > 42.1L) return 2;

    // Sub assign
    a = 50.0L;
    a -= 8.0L;
    if (a < 41.9L || a > 42.1L) return 3;

    // Mul assign
    a = 21.0L;
    a *= 2.0L;
    if (a < 41.9L || a > 42.1L) return 4;

    // Div assign
    a = 84.0L;
    a /= 2.0L;
    if (a < 41.9L || a > 42.1L) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longdouble_assign", code, &[]), 0);
}

// ============================================================================
// Long Double Increment/Decrement Operators (++a, a++, --a, a--)
// ============================================================================

#[test]
fn longdouble_increment_decrement_operators() {
    let code = r#"
int main(void) {
    long double a, b;

    // Pre-increment
    a = 41.0L;
    b = ++a;
    if (b < 41.9L || b > 42.1L) return 1;

    // Post-increment (returns original)
    a = 42.0L;
    b = a++;
    if (b < 41.9L || b > 42.1L) return 2;

    // Post-increment (side effect)
    a = 41.0L;
    a++;
    if (a < 41.9L || a > 42.1L) return 3;

    // Pre-decrement
    a = 43.0L;
    b = --a;
    if (b < 41.9L || b > 42.1L) return 4;

    // Post-decrement (returns original)
    a = 42.0L;
    b = a--;
    if (b < 41.9L || b > 42.1L) return 5;

    // Post-decrement (side effect)
    a = 43.0L;
    a--;
    if (a < 41.9L || a > 42.1L) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longdouble_incdec", code, &[]), 0);
}

// ============================================================================
// Long Double-Int Conversions
// ============================================================================

#[test]
fn longdouble_int_conversions() {
    let code = r#"
int main(void) {
    long double ld;
    int i;

    // Int to long double conversion
    i = 42;
    ld = i;
    if (ld < 41.9L || ld > 42.1L) return 1;

    // Long double to int conversion (truncation)
    ld = 42.7L;
    i = (int)ld;
    if (i != 42) return 2;

    // Long double to int conversion (negative)
    ld = -42.7L;
    i = (int)ld;
    if (i != -42) return 3;

    // Mixed arithmetic: long double + int
    ld = 40.0L;
    i = 2;
    ld = ld + i;
    if (ld < 41.9L || ld > 42.1L) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longdouble_int_conv", code, &[]), 0);
}

// ============================================================================
// Long Double-Float-Double Conversions
// ============================================================================

#[test]
fn longdouble_float_double_conversions() {
    let code = r#"
int main(void) {
    float f;
    double d;
    long double ld;

    // Float to long double (implicit promotion)
    f = 42.0f;
    ld = f;
    if (ld < 41.9L || ld > 42.1L) return 1;

    // Long double to float (explicit conversion)
    ld = 42.0L;
    f = (float)ld;
    if (f < 41.9f || f > 42.1f) return 2;

    // Double to long double (implicit promotion)
    d = 42.0;
    ld = d;
    if (ld < 41.9L || ld > 42.1L) return 3;

    // Long double to double (explicit conversion)
    ld = 42.0L;
    d = (double)ld;
    if (d < 41.9 || d > 42.1) return 4;

    // Mixed arithmetic: float + long double promotes to long double
    f = 20.0f;
    ld = 22.0L;
    ld = f + ld;
    if (ld < 41.9L || ld > 42.1L) return 5;

    // Mixed arithmetic: double + long double promotes to long double
    d = 20.0;
    ld = 22.0L;
    ld = d + ld;
    if (ld < 41.9L || ld > 42.1L) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longdouble_float_conv", code, &[]), 0);
}

// ============================================================================
// Long Double Literals
// ============================================================================

#[test]
fn longdouble_literals() {
    let code = r#"
int main(void) {
    long double ld;

    // Long double literal with L suffix
    ld = 42.0L;
    if (ld < 41.9L || ld > 42.1L) return 1;

    // Long double literal with l suffix (lowercase)
    ld = 42.0l;
    if (ld < 41.9L || ld > 42.1L) return 2;

    // Exponential notation
    ld = 4.2e1L;
    if (ld < 41.9L || ld > 42.1L) return 3;

    // Negative exponent
    ld = 4200.0e-2L;
    if (ld < 41.9L || ld > 42.1L) return 4;

    // No suffix defaults to double, assigned to long double
    ld = 42.0;
    if (ld < 41.9L || ld > 42.1L) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longdouble_literals", code, &[]), 0);
}

// ============================================================================
// Complex Long Double Expressions
// ============================================================================

#[test]
fn longdouble_complex_expressions() {
    let code = r#"
int main(void) {
    long double a, b, c, result;

    // Mixed arithmetic with precedence
    a = 10.0L; b = 5.0L; c = 2.0L;
    result = a + b * c - 3.0L;  // 10 + 10 - 3 = 17
    if (result < 16.9L || result > 17.1L) return 1;

    // Chained comparison
    a = 5.0L; b = 10.0L; c = 15.0L;
    if (((a < b) && (b < c)) != 1) return 2;

    // Division precision test
    a = 1.0L; b = 3.0L;
    result = a / b;  // Should be approximately 0.333...
    if (result < 0.3L || result > 0.4L) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longdouble_complex", code, &[]), 0);
}

// ============================================================================
// Long Double: Structs, Functions, and Initializers
// ============================================================================

#[test]
fn longdouble_structs_functions_initializers() {
    let code = r#"
struct longdouble_container {
    long double value;
    long double second;
};

long double double_longdouble(long double x) {
    return x * 2.0L;
}

long double sum_longdouble_container(struct longdouble_container c) {
    return c.value + c.second;
}

struct longdouble_container make_longdouble_container(long double a, long double b) {
    struct longdouble_container c;
    c.value = a;
    c.second = b;
    return c;
}

int main(void) {
    // Variable initializers
    long double ld = 42.0L;
    if (ld < 41.9L || ld > 42.1L) return 1;

    // Multiple variable declarations with initializers
    long double a = 10.0L, b = 20.0L, c = 30.0L;
    if (a + b + c < 59.9L || a + b + c > 60.1L) return 2;

    // Struct member access - long double
    struct longdouble_container ldc;
    ldc.value = 100.5L;
    ldc.second = 50.25L;
    if (ldc.value < 100.4L || ldc.value > 100.6L) return 3;
    if (ldc.second < 50.2L || ldc.second > 50.3L) return 4;

    // Function with long double parameter and return
    long double lddoubled = double_longdouble(21.0L);
    if (lddoubled < 41.9L || lddoubled > 42.1L) return 5;

    // Negative long double
    long double ldneg = -100.0L;
    long double ldneg_doubled = double_longdouble(ldneg);
    if (ldneg_doubled < -200.1L || ldneg_doubled > -199.9L) return 6;

    // Struct as function parameter - long double
    struct longdouble_container ldparam;
    ldparam.value = 100.0L;
    ldparam.second = 50.0L;
    long double ldsum = sum_longdouble_container(ldparam);
    if (ldsum < 149.9L || ldsum > 150.1L) return 7;

    // Struct as function return value - long double
    struct longdouble_container ldreturned = make_longdouble_container(300.0L, 200.0L);
    if (ldreturned.value < 299.9L || ldreturned.value > 300.1L) return 8;
    if (ldreturned.second < 199.9L || ldreturned.second > 200.1L) return 9;

    // Pointer to struct - long double
    struct longdouble_container ldtarget;
    struct longdouble_container *ldptr = &ldtarget;
    ldptr->value = 2000.0L;
    ldptr->second = 1000.0L;
    if (ldtarget.value < 1999.9L || ldtarget.value > 2000.1L) return 10;
    if (ldtarget.second < 999.9L || ldtarget.second > 1000.1L) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("longdouble_advanced", code, &[]), 0);
}
