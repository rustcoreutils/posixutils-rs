//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `_Float16` data type (C23 / TS 18661-3)
// Tests are aggregated by category to reduce compile/link overhead
//
// Note: _Float16 requires soft-float runtime library support (__extendhfsf2, etc.)
// On platforms without these libraries, tests may fail to link.
//

use crate::common::compile_and_run;

// ============================================================================
// Float16 Type Declaration and Sizeof
// ============================================================================

#[test]
fn float16_sizeof() {
    let code = r#"
int main(void) {
    // sizeof(_Float16) should be 2 bytes
    if (sizeof(_Float16) != 2) return 1;

    // Variable declaration
    _Float16 x;
    if (sizeof(x) != 2) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float16_sizeof", code, &[]), 0);
}

// ============================================================================
// Float16 Literals with f16/F16 suffix
// ============================================================================

#[test]
fn float16_literals() {
    let code = r#"
int main(void) {
    _Float16 x;

    // Lowercase suffix
    x = 1.0f16;
    if (x < 0.9f16 || x > 1.1f16) return 1;

    // Uppercase suffix
    x = 2.5F16;
    if (x < 2.4f16 || x > 2.6f16) return 2;

    // Integer with f16 suffix
    x = 42f16;
    if (x < 41.9f16 || x > 42.1f16) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float16_literals", code, &[]), 0);
}

// ============================================================================
// Float16 Arithmetic Operators (add, sub, mul, div, unary neg, unary plus)
// ============================================================================

#[test]
fn float16_arithmetic_operators() {
    let code = r#"
int main(void) {
    _Float16 a, b, result;

    // Addition
    a = 30.5f16; b = 11.5f16;
    result = a + b;
    if (result < 41.0f16 || result > 43.0f16) return 1;

    // Subtraction
    a = 100.0f16; b = 58.0f16;
    result = a - b;
    if (result < 41.0f16 || result > 43.0f16) return 2;

    // Multiplication
    a = 6.0f16; b = 7.0f16;
    result = a * b;
    if (result < 41.0f16 || result > 43.0f16) return 3;

    // Division
    a = 84.0f16; b = 2.0f16;
    result = a / b;
    if (result < 41.0f16 || result > 43.0f16) return 4;

    // Unary negation
    a = -42.0f16;
    result = -a;
    if (result < 41.0f16 || result > 43.0f16) return 5;

    // Unary plus
    a = 42.0f16;
    result = +a;
    if (result < 41.0f16 || result > 43.0f16) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float16_arith", code, &[]), 0);
}

// ============================================================================
// Float16 Comparison Operators (==, !=, <, <=, >, >=)
// ============================================================================

#[test]
fn float16_comparison_operators() {
    let code = r#"
int main(void) {
    _Float16 a, b;

    // Less than - true
    a = 10.0f16; b = 20.0f16;
    if ((a < b) != 1) return 1;

    // Less than - false
    a = 20.0f16; b = 10.0f16;
    if ((a < b) != 0) return 2;

    // Less or equal - true (less)
    a = 10.0f16; b = 20.0f16;
    if ((a <= b) != 1) return 3;

    // Less or equal - true (equal)
    a = 20.0f16; b = 20.0f16;
    if ((a <= b) != 1) return 4;

    // Less or equal - false
    a = 30.0f16; b = 20.0f16;
    if ((a <= b) != 0) return 5;

    // Greater than - true
    a = 20.0f16; b = 10.0f16;
    if ((a > b) != 1) return 6;

    // Greater than - false
    a = 10.0f16; b = 20.0f16;
    if ((a > b) != 0) return 7;

    // Greater or equal - true (greater)
    a = 30.0f16; b = 20.0f16;
    if ((a >= b) != 1) return 8;

    // Greater or equal - true (equal)
    a = 20.0f16; b = 20.0f16;
    if ((a >= b) != 1) return 9;

    // Greater or equal - false
    a = 10.0f16; b = 20.0f16;
    if ((a >= b) != 0) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float16_cmp", code, &[]), 0);
}

// ============================================================================
// Float16 Assignment Operators (=, +=, -=, *=, /=)
// ============================================================================

#[test]
fn float16_assignment_operators() {
    let code = r#"
int main(void) {
    _Float16 a;

    // Simple assignment
    a = 42.0f16;
    if (a < 41.0f16 || a > 43.0f16) return 1;

    // Add assign
    a = 40.0f16;
    a += 2.0f16;
    if (a < 41.0f16 || a > 43.0f16) return 2;

    // Sub assign
    a = 50.0f16;
    a -= 8.0f16;
    if (a < 41.0f16 || a > 43.0f16) return 3;

    // Mul assign
    a = 21.0f16;
    a *= 2.0f16;
    if (a < 41.0f16 || a > 43.0f16) return 4;

    // Div assign
    a = 84.0f16;
    a /= 2.0f16;
    if (a < 41.0f16 || a > 43.0f16) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float16_assign", code, &[]), 0);
}

// ============================================================================
// Float16 Type Conversions
// ============================================================================

#[test]
#[ignore = "requires soft-float runtime library (__extendhfdf2, __fixhfsi, etc.)"]
fn float16_conversions() {
    let code = r#"
int main(void) {
    _Float16 h;
    float f;
    double d;
    int i;

    // int -> _Float16
    i = 42;
    h = (_Float16)i;
    if (h < 41.0f16 || h > 43.0f16) return 1;

    // _Float16 -> int
    h = 42.5f16;
    i = (int)h;
    if (i != 42) return 2;

    // float -> _Float16
    f = 42.0f;
    h = (_Float16)f;
    if (h < 41.0f16 || h > 43.0f16) return 3;

    // _Float16 -> float
    h = 42.0f16;
    f = (float)h;
    if (f < 41.0f || f > 43.0f) return 4;

    // double -> _Float16
    d = 42.0;
    h = (_Float16)d;
    if (h < 41.0f16 || h > 43.0f16) return 5;

    // _Float16 -> double
    h = 42.0f16;
    d = (double)h;
    if (d < 41.0 || d > 43.0) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float16_conv", code, &[]), 0);
}

// ============================================================================
// _Float32 Alias Tests (should behave like float)
// ============================================================================

#[test]
fn float32_alias() {
    let code = r#"
int main(void) {
    // _Float32 should be alias for float
    if (sizeof(_Float32) != sizeof(float)) return 1;

    _Float32 f32 = 42.0f32;
    float f = f32;  // Implicit conversion, same type
    if (f < 41.9f || f > 42.1f) return 2;

    // f32 suffix
    _Float32 x = 3.14f32;
    if (x < 3.13f || x > 3.15f) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float32_alias", code, &[]), 0);
}

// ============================================================================
// _Float64 Alias Tests (should behave like double)
// ============================================================================

#[test]
fn float64_alias() {
    let code = r#"
int main(void) {
    // _Float64 should be alias for double
    if (sizeof(_Float64) != sizeof(double)) return 1;

    _Float64 f64 = 42.0f64;
    double d = f64;  // Implicit conversion, same type
    if (d < 41.9 || d > 42.1) return 2;

    // f64 suffix
    _Float64 x = 3.14f64;
    if (x < 3.13 || x > 3.15) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float64_alias", code, &[]), 0);
}
