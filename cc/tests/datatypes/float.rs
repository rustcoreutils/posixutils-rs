//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `float` and `double` data types with all applicable operators
// Tests are aggregated by category to reduce compile/link overhead
//

use crate::common::compile_and_run;

// ============================================================================
// Float Arithmetic Operators (add, sub, mul, div, unary neg, unary plus)
// ============================================================================

#[test]
fn float_arithmetic_operators() {
    let code = r#"
int main(void) {
    float a, b, result;

    // Addition
    a = 30.5f; b = 11.5f;
    result = a + b;
    if (result < 41.9f || result > 42.1f) return 1;

    // Subtraction
    a = 100.0f; b = 58.0f;
    result = a - b;
    if (result < 41.9f || result > 42.1f) return 2;

    // Multiplication
    a = 6.0f; b = 7.0f;
    result = a * b;
    if (result < 41.9f || result > 42.1f) return 3;

    // Division
    a = 84.0f; b = 2.0f;
    result = a / b;
    if (result < 41.9f || result > 42.1f) return 4;

    // Unary negation
    a = -42.0f;
    result = -a;
    if (result < 41.9f || result > 42.1f) return 5;

    // Unary plus
    a = 42.0f;
    result = +a;
    if (result < 41.9f || result > 42.1f) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_arith", code), 0);
}

// ============================================================================
// Float Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn float_comparison_operators() {
    let code = r#"
int main(void) {
    float a, b;

    // Less than - true
    a = 10.0f; b = 20.0f;
    if ((a < b) != 1) return 1;

    // Less than - false
    a = 20.0f; b = 10.0f;
    if ((a < b) != 0) return 2;

    // Less or equal - true (less)
    a = 10.0f; b = 20.0f;
    if ((a <= b) != 1) return 3;

    // Less or equal - true (equal)
    a = 20.0f; b = 20.0f;
    if ((a <= b) != 1) return 4;

    // Less or equal - false
    a = 30.0f; b = 20.0f;
    if ((a <= b) != 0) return 5;

    // Greater than - true
    a = 20.0f; b = 10.0f;
    if ((a > b) != 1) return 6;

    // Greater than - false
    a = 10.0f; b = 20.0f;
    if ((a > b) != 0) return 7;

    // Greater or equal - true (greater)
    a = 30.0f; b = 20.0f;
    if ((a >= b) != 1) return 8;

    // Greater or equal - true (equal)
    a = 20.0f; b = 20.0f;
    if ((a >= b) != 1) return 9;

    // Greater or equal - false
    a = 10.0f; b = 20.0f;
    if ((a >= b) != 0) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_cmp", code), 0);
}

// ============================================================================
// Float Assignment Operators (=, +=, -=, *=, /=)
// ============================================================================

#[test]
fn float_assignment_operators() {
    let code = r#"
int main(void) {
    float a;

    // Simple assignment
    a = 42.0f;
    if (a < 41.9f || a > 42.1f) return 1;

    // Add assign
    a = 40.0f;
    a += 2.0f;
    if (a < 41.9f || a > 42.1f) return 2;

    // Sub assign
    a = 50.0f;
    a -= 8.0f;
    if (a < 41.9f || a > 42.1f) return 3;

    // Mul assign
    a = 21.0f;
    a *= 2.0f;
    if (a < 41.9f || a > 42.1f) return 4;

    // Div assign
    a = 84.0f;
    a /= 2.0f;
    if (a < 41.9f || a > 42.1f) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_assign", code), 0);
}

// ============================================================================
// Float Increment/Decrement Operators (++a, a++, --a, a--)
// ============================================================================

#[test]
fn float_increment_decrement_operators() {
    let code = r#"
int main(void) {
    float a, b;

    // Pre-increment
    a = 41.0f;
    b = ++a;
    if (b < 41.9f || b > 42.1f) return 1;

    // Post-increment (returns original)
    a = 42.0f;
    b = a++;
    if (b < 41.9f || b > 42.1f) return 2;

    // Post-increment (side effect)
    a = 41.0f;
    a++;
    if (a < 41.9f || a > 42.1f) return 3;

    // Pre-decrement
    a = 43.0f;
    b = --a;
    if (b < 41.9f || b > 42.1f) return 4;

    // Post-decrement (returns original)
    a = 42.0f;
    b = a--;
    if (b < 41.9f || b > 42.1f) return 5;

    // Post-decrement (side effect)
    a = 43.0f;
    a--;
    if (a < 41.9f || a > 42.1f) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_incdec", code), 0);
}

// ============================================================================
// Float-Int Conversions
// ============================================================================

#[test]
fn float_int_conversions() {
    let code = r#"
int main(void) {
    float f;
    int i;

    // Int to float conversion
    i = 42;
    f = i;
    if (f < 41.9f || f > 42.1f) return 1;

    // Float to int conversion (truncation)
    f = 42.7f;
    i = (int)f;
    if (i != 42) return 2;

    // Float to int conversion (negative)
    f = -42.7f;
    i = (int)f;
    if (i != -42) return 3;

    // Mixed arithmetic: float + int
    f = 40.0f;
    i = 2;
    f = f + i;
    if (f < 41.9f || f > 42.1f) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_int_conv", code), 0);
}

// ############################################################################
// DOUBLE TESTS
// ############################################################################

// ============================================================================
// Double Arithmetic Operators (add, sub, mul, div, unary neg, unary plus)
// ============================================================================

#[test]
fn double_arithmetic_operators() {
    let code = r#"
int main(void) {
    double a, b, result;

    // Addition
    a = 30.5; b = 11.5;
    result = a + b;
    if (result < 41.9 || result > 42.1) return 1;

    // Subtraction
    a = 100.0; b = 58.0;
    result = a - b;
    if (result < 41.9 || result > 42.1) return 2;

    // Multiplication
    a = 6.0; b = 7.0;
    result = a * b;
    if (result < 41.9 || result > 42.1) return 3;

    // Division
    a = 84.0; b = 2.0;
    result = a / b;
    if (result < 41.9 || result > 42.1) return 4;

    // Unary negation
    a = -42.0;
    result = -a;
    if (result < 41.9 || result > 42.1) return 5;

    // Unary plus
    a = 42.0;
    result = +a;
    if (result < 41.9 || result > 42.1) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("double_arith", code), 0);
}

// ============================================================================
// Double Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn double_comparison_operators() {
    let code = r#"
int main(void) {
    double a, b;

    // Less than - true
    a = 10.0; b = 20.0;
    if ((a < b) != 1) return 1;

    // Less than - false
    a = 20.0; b = 10.0;
    if ((a < b) != 0) return 2;

    // Less or equal - true (less)
    a = 10.0; b = 20.0;
    if ((a <= b) != 1) return 3;

    // Less or equal - true (equal)
    a = 20.0; b = 20.0;
    if ((a <= b) != 1) return 4;

    // Less or equal - false
    a = 30.0; b = 20.0;
    if ((a <= b) != 0) return 5;

    // Greater than - true
    a = 20.0; b = 10.0;
    if ((a > b) != 1) return 6;

    // Greater than - false
    a = 10.0; b = 20.0;
    if ((a > b) != 0) return 7;

    // Greater or equal - true (greater)
    a = 30.0; b = 20.0;
    if ((a >= b) != 1) return 8;

    // Greater or equal - true (equal)
    a = 20.0; b = 20.0;
    if ((a >= b) != 1) return 9;

    // Greater or equal - false
    a = 10.0; b = 20.0;
    if ((a >= b) != 0) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("double_cmp", code), 0);
}

// ============================================================================
// Double Assignment Operators (=, +=, -=, *=, /=)
// ============================================================================

#[test]
fn double_assignment_operators() {
    let code = r#"
int main(void) {
    double a;

    // Simple assignment
    a = 42.0;
    if (a < 41.9 || a > 42.1) return 1;

    // Add assign
    a = 40.0;
    a += 2.0;
    if (a < 41.9 || a > 42.1) return 2;

    // Sub assign
    a = 50.0;
    a -= 8.0;
    if (a < 41.9 || a > 42.1) return 3;

    // Mul assign
    a = 21.0;
    a *= 2.0;
    if (a < 41.9 || a > 42.1) return 4;

    // Div assign
    a = 84.0;
    a /= 2.0;
    if (a < 41.9 || a > 42.1) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("double_assign", code), 0);
}

// ============================================================================
// Double-Int Conversions
// ============================================================================

#[test]
fn double_int_conversions() {
    let code = r#"
int main(void) {
    double d;
    int i;

    // Int to double conversion
    i = 42;
    d = i;
    if (d < 41.9 || d > 42.1) return 1;

    // Double to int conversion (truncation)
    d = 42.7;
    i = (int)d;
    if (i != 42) return 2;

    // Double to int conversion (negative)
    d = -42.7;
    i = (int)d;
    if (i != -42) return 3;

    // Mixed arithmetic: double + int
    d = 40.0;
    i = 2;
    d = d + i;
    if (d < 41.9 || d > 42.1) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("double_int_conv", code), 0);
}

// ============================================================================
// Float-Double Conversions
// ============================================================================

#[test]
fn float_double_conversions() {
    let code = r#"
int main(void) {
    float f;
    double d;

    // Float to double (implicit promotion)
    f = 42.0f;
    d = f;
    if (d < 41.9 || d > 42.1) return 1;

    // Double to float (explicit conversion)
    d = 42.0;
    f = (float)d;
    if (f < 41.9f || f > 42.1f) return 2;

    // Mixed arithmetic: float + double promotes to double
    f = 20.0f;
    d = 22.0;
    d = f + d;
    if (d < 41.9 || d > 42.1) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_double_conv", code), 0);
}

// ============================================================================
// Float Literals
// ============================================================================

#[test]
fn float_literals() {
    let code = r#"
int main(void) {
    float f;
    double d;

    // Float literal with f suffix
    f = 42.0f;
    if (f < 41.9f || f > 42.1f) return 1;

    // Float literal with F suffix
    f = 42.0F;
    if (f < 41.9f || f > 42.1f) return 2;

    // Double literal (no suffix)
    d = 42.0;
    if (d < 41.9 || d > 42.1) return 3;

    // Exponential notation
    d = 4.2e1;
    if (d < 41.9 || d > 42.1) return 4;

    // Negative exponent
    d = 4200.0e-2;
    if (d < 41.9 || d > 42.1) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_literals", code), 0);
}

// ============================================================================
// Complex Float Expressions
// ============================================================================

#[test]
fn float_complex_expressions() {
    let code = r#"
int main(void) {
    float a, b, c, result;

    // Mixed arithmetic with precedence
    a = 10.0f; b = 5.0f; c = 2.0f;
    result = a + b * c - 3.0f;  // 10 + 10 - 3 = 17
    if (result < 16.9f || result > 17.1f) return 1;

    // Chained comparison
    a = 5.0f; b = 10.0f; c = 15.0f;
    if (((a < b) && (b < c)) != 1) return 2;

    // Division precision test
    a = 1.0f; b = 3.0f;
    result = a / b;  // Should be approximately 0.333...
    if (result < 0.3f || result > 0.4f) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_complex", code), 0);
}

// ============================================================================
// Float/Double: Structs, Functions, and Initializers
// ============================================================================

#[test]
fn float_structs_functions_initializers() {
    let code = r#"
struct float_container {
    float value;
    float second;
};

struct double_container {
    double value;
    double second;
};

float double_float(float x) {
    return x * 2.0f;
}

double double_double(double x) {
    return x * 2.0;
}

float sum_float_container(struct float_container c) {
    return c.value + c.second;
}

double sum_double_container(struct double_container c) {
    return c.value + c.second;
}

struct float_container make_float_container(float a, float b) {
    struct float_container c;
    c.value = a;
    c.second = b;
    return c;
}

struct double_container make_double_container(double a, double b) {
    struct double_container c;
    c.value = a;
    c.second = b;
    return c;
}

int main(void) {
    // Variable initializers
    float f = 42.0f;
    if (f < 41.9f || f > 42.1f) return 1;

    double d = 42.0;
    if (d < 41.9 || d > 42.1) return 2;

    // Multiple variable declarations with initializers
    float a = 10.0f, b = 20.0f, c = 30.0f;
    if (a + b + c < 59.9f || a + b + c > 60.1f) return 3;

    // Struct member access - float
    struct float_container fc;
    fc.value = 100.5f;
    fc.second = 50.25f;
    if (fc.value < 100.4f || fc.value > 100.6f) return 4;
    if (fc.second < 50.2f || fc.second > 50.3f) return 5;

    // Struct member access - double
    struct double_container dc;
    dc.value = 1000.5;
    dc.second = 500.25;
    if (dc.value < 1000.4 || dc.value > 1000.6) return 6;
    if (dc.second < 500.2 || dc.second > 500.3) return 7;

    // Function with float parameter and return
    float fdoubled = double_float(21.0f);
    if (fdoubled < 41.9f || fdoubled > 42.1f) return 8;

    // Function with double parameter and return
    double ddoubled = double_double(21.0);
    if (ddoubled < 41.9 || ddoubled > 42.1) return 9;

    // Negative float
    float fneg = -100.0f;
    float fneg_doubled = double_float(fneg);
    if (fneg_doubled < -200.1f || fneg_doubled > -199.9f) return 10;

    // Negative double
    double dneg = -100.0;
    double dneg_doubled = double_double(dneg);
    if (dneg_doubled < -200.1 || dneg_doubled > -199.9) return 11;

    // Struct as function parameter - float
    struct float_container fparam;
    fparam.value = 100.0f;
    fparam.second = 50.0f;
    float fsum = sum_float_container(fparam);
    if (fsum < 149.9f || fsum > 150.1f) return 12;

    // Struct as function parameter - double
    struct double_container dparam;
    dparam.value = 100.0;
    dparam.second = 50.0;
    double dsum = sum_double_container(dparam);
    if (dsum < 149.9 || dsum > 150.1) return 13;

    // Struct as function return value - float
    struct float_container freturned = make_float_container(300.0f, 200.0f);
    if (freturned.value < 299.9f || freturned.value > 300.1f) return 14;
    if (freturned.second < 199.9f || freturned.second > 200.1f) return 15;

    // Struct as function return value - double
    struct double_container dreturned = make_double_container(300.0, 200.0);
    if (dreturned.value < 299.9 || dreturned.value > 300.1) return 16;
    if (dreturned.second < 199.9 || dreturned.second > 200.1) return 17;

    // Pointer to struct - float
    struct float_container ftarget;
    struct float_container *fptr = &ftarget;
    fptr->value = 2000.0f;
    fptr->second = 1000.0f;
    if (ftarget.value < 1999.9f || ftarget.value > 2000.1f) return 18;
    if (ftarget.second < 999.9f || ftarget.second > 1000.1f) return 19;

    // Pointer to struct - double
    struct double_container dtarget;
    struct double_container *dptr = &dtarget;
    dptr->value = 2000.0;
    dptr->second = 1000.0;
    if (dtarget.value < 1999.9 || dtarget.value > 2000.1) return 20;
    if (dtarget.second < 999.9 || dtarget.second > 1000.1) return 21;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_advanced", code), 0);
}
