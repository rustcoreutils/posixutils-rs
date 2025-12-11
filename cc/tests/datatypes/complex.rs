//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 `_Complex` data types (float _Complex, double _Complex)
// Tests are aggregated by category to reduce compile/link overhead
//

use crate::common::compile_and_run;

// ============================================================================
// Double _Complex Basic Arithmetic (add, sub, mul, div)
// ============================================================================

#[test]
fn double_complex_addition() {
    // Test: (1+2i) + (3+4i) = (4+6i)
    let code = r#"
double _Complex a;
double _Complex b;
double _Complex c;

int main(void) {
    double *ap = (double*)&a;
    double *bp = (double*)&b;
    double *cp = (double*)&c;

    // a = 1 + 2i
    ap[0] = 1.0; ap[1] = 2.0;
    // b = 3 + 4i
    bp[0] = 3.0; bp[1] = 4.0;

    c = a + b;

    // c should be 4 + 6i
    // Check real part: 4.0
    if (cp[0] < 3.9 || cp[0] > 4.1) return 1;
    // Check imaginary part: 6.0
    if (cp[1] < 5.9 || cp[1] > 6.1) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_add", code), 0);
}

#[test]
fn double_complex_subtraction() {
    // Test: (5+7i) - (3+4i) = (2+3i)
    let code = r#"
double _Complex a;
double _Complex b;
double _Complex c;

int main(void) {
    double *ap = (double*)&a;
    double *bp = (double*)&b;
    double *cp = (double*)&c;

    // a = 5 + 7i
    ap[0] = 5.0; ap[1] = 7.0;
    // b = 3 + 4i
    bp[0] = 3.0; bp[1] = 4.0;

    c = a - b;

    // c should be 2 + 3i
    // Check real part: 2.0
    if (cp[0] < 1.9 || cp[0] > 2.1) return 1;
    // Check imaginary part: 3.0
    if (cp[1] < 2.9 || cp[1] > 3.1) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_sub", code), 0);
}

#[test]
fn double_complex_multiplication() {
    // Test: (1+2i) * (3+4i) = (1*3 - 2*4) + (1*4 + 2*3)i = -5 + 10i
    let code = r#"
double _Complex a;
double _Complex b;
double _Complex c;

int main(void) {
    double *ap = (double*)&a;
    double *bp = (double*)&b;
    double *cp = (double*)&c;

    // a = 1 + 2i
    ap[0] = 1.0; ap[1] = 2.0;
    // b = 3 + 4i
    bp[0] = 3.0; bp[1] = 4.0;

    c = a * b;

    // c should be -5 + 10i
    // Check real part: -5.0
    if (cp[0] < -5.1 || cp[0] > -4.9) return 1;
    // Check imaginary part: 10.0
    if (cp[1] < 9.9 || cp[1] > 10.1) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_mul", code), 0);
}

#[test]
fn double_complex_division() {
    // Test: (1+2i) / (3+4i)
    // = ((1*3 + 2*4) + (2*3 - 1*4)i) / (3*3 + 4*4)
    // = (3+8 + (6-4)i) / 25
    // = (11 + 2i) / 25
    // = 0.44 + 0.08i
    let code = r#"
double _Complex a;
double _Complex b;
double _Complex c;

int main(void) {
    double *ap = (double*)&a;
    double *bp = (double*)&b;
    double *cp = (double*)&c;

    // a = 1 + 2i
    ap[0] = 1.0; ap[1] = 2.0;
    // b = 3 + 4i
    bp[0] = 3.0; bp[1] = 4.0;

    c = a / b;

    // c should be 0.44 + 0.08i
    // Check real part: 0.44
    if (cp[0] < 0.43 || cp[0] > 0.45) return 1;
    // Check imaginary part: 0.08
    if (cp[1] < 0.07 || cp[1] > 0.09) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_div", code), 0);
}

// ============================================================================
// Float _Complex Basic Arithmetic
// ============================================================================

#[test]
fn float_complex_addition() {
    // Test: (1+2i) + (3+4i) = (4+6i)
    let code = r#"
float _Complex a;
float _Complex b;
float _Complex c;

int main(void) {
    float *ap = (float*)&a;
    float *bp = (float*)&b;
    float *cp = (float*)&c;

    // a = 1 + 2i
    ap[0] = 1.0f; ap[1] = 2.0f;
    // b = 3 + 4i
    bp[0] = 3.0f; bp[1] = 4.0f;

    c = a + b;

    // c should be 4 + 6i
    if (cp[0] < 3.9f || cp[0] > 4.1f) return 1;
    if (cp[1] < 5.9f || cp[1] > 6.1f) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_complex_add", code), 0);
}

#[test]
fn float_complex_multiplication() {
    // Test: (1+2i) * (3+4i) = -5 + 10i
    let code = r#"
float _Complex a;
float _Complex b;
float _Complex c;

int main(void) {
    float *ap = (float*)&a;
    float *bp = (float*)&b;
    float *cp = (float*)&c;

    // a = 1 + 2i
    ap[0] = 1.0f; ap[1] = 2.0f;
    // b = 3 + 4i
    bp[0] = 3.0f; bp[1] = 4.0f;

    c = a * b;

    // c should be -5 + 10i
    if (cp[0] < -5.1f || cp[0] > -4.9f) return 1;
    if (cp[1] < 9.9f || cp[1] > 10.1f) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_complex_mul", code), 0);
}

// ============================================================================
// Complex Type Declaration Variants
// ============================================================================

#[test]
fn complex_type_variants() {
    // Test various ways to declare complex types
    let code = r#"
double _Complex dc1;
_Complex double dc2;
float _Complex fc1;
_Complex float fc2;

int main(void) {
    double *p1 = (double*)&dc1;
    double *p2 = (double*)&dc2;
    float *p3 = (float*)&fc1;
    float *p4 = (float*)&fc2;

    // Set values
    p1[0] = 1.0; p1[1] = 2.0;
    p2[0] = 3.0; p2[1] = 4.0;
    p3[0] = 5.0f; p3[1] = 6.0f;
    p4[0] = 7.0f; p4[1] = 8.0f;

    // Check sizes - double _Complex should be 16 bytes
    if (sizeof(dc1) != 16) return 1;
    if (sizeof(dc2) != 16) return 2;

    // float _Complex should be 8 bytes
    if (sizeof(fc1) != 8) return 3;
    if (sizeof(fc2) != 8) return 4;

    // Check values were stored correctly
    if (p1[0] < 0.9 || p1[0] > 1.1) return 5;
    if (p2[1] < 3.9 || p2[1] > 4.1) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_variants", code), 0);
}

// ============================================================================
// Complex Assignment
// ============================================================================

#[test]
fn complex_assignment() {
    // Test assignment between complex variables
    let code = r#"
double _Complex a;
double _Complex b;

int main(void) {
    double *ap = (double*)&a;
    double *bp = (double*)&b;

    // Set a = 10 + 20i
    ap[0] = 10.0; ap[1] = 20.0;

    // Assign b = a
    b = a;

    // Check b has same values
    if (bp[0] < 9.9 || bp[0] > 10.1) return 1;
    if (bp[1] < 19.9 || bp[1] > 20.1) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_assign", code), 0);
}

// ============================================================================
// Complex in Local Variables
// ============================================================================

#[test]
fn complex_local_variables() {
    // Test complex types as local variables
    let code = r#"
int main(void) {
    double _Complex a;
    double _Complex b;
    double _Complex c;

    double *ap = (double*)&a;
    double *bp = (double*)&b;
    double *cp = (double*)&c;

    // a = 2 + 3i
    ap[0] = 2.0; ap[1] = 3.0;
    // b = 4 + 5i
    bp[0] = 4.0; bp[1] = 5.0;

    c = a + b;

    // c should be 6 + 8i
    if (cp[0] < 5.9 || cp[0] > 6.1) return 1;
    if (cp[1] < 7.9 || cp[1] > 8.1) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_local", code), 0);
}

// ============================================================================
// Complex Arithmetic: Multiple Operations
// ============================================================================

#[test]
fn complex_multiple_operations() {
    // Test performing multiple operations
    let code = r#"
double _Complex a;
double _Complex b;
double _Complex sum;
double _Complex diff;
double _Complex prod;
double _Complex quot;

int main(void) {
    double *ap = (double*)&a;
    double *bp = (double*)&b;
    double *sp = (double*)&sum;
    double *dp = (double*)&diff;
    double *pp = (double*)&prod;
    double *qp = (double*)&quot;

    // a = 1 + 2i, b = 3 + 4i
    ap[0] = 1.0; ap[1] = 2.0;
    bp[0] = 3.0; bp[1] = 4.0;

    // Test all four operations with separate result variables
    sum = a + b;   // 4 + 6i
    diff = a - b;  // -2 - 2i
    prod = a * b;  // -5 + 10i
    quot = a / b;  // 0.44 + 0.08i

    // Check sum: 4 + 6i
    if (sp[0] < 3.9 || sp[0] > 4.1) return 1;
    if (sp[1] < 5.9 || sp[1] > 6.1) return 2;

    // Check diff: -2 - 2i
    if (dp[0] < -2.1 || dp[0] > -1.9) return 3;
    if (dp[1] < -2.1 || dp[1] > -1.9) return 4;

    // Check prod: -5 + 10i
    if (pp[0] < -5.1 || pp[0] > -4.9) return 5;
    if (pp[1] < 9.9 || pp[1] > 10.1) return 6;

    // Check quot: 0.44 + 0.08i
    if (qp[0] < 0.43 || qp[0] > 0.45) return 7;
    if (qp[1] < 0.07 || qp[1] > 0.09) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_multi_ops", code), 0);
}

// ============================================================================
// Complex in Arrays
// ============================================================================

#[test]
fn complex_arrays() {
    // Test arrays of complex numbers
    let code = r#"
double _Complex arr[3];

int main(void) {
    double *p = (double*)arr;

    // arr[0] = 1 + 2i
    p[0] = 1.0; p[1] = 2.0;
    // arr[1] = 3 + 4i
    p[2] = 3.0; p[3] = 4.0;
    // arr[2] = 5 + 6i
    p[4] = 5.0; p[5] = 6.0;

    // Check sizeof array
    if (sizeof(arr) != 48) return 1;  // 3 * 16 bytes

    // Add first two elements
    double _Complex sum = arr[0] + arr[1];
    double *sp = (double*)&sum;

    // sum should be 4 + 6i
    if (sp[0] < 3.9 || sp[0] > 4.1) return 2;
    if (sp[1] < 5.9 || sp[1] > 6.1) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_array", code), 0);
}

// ============================================================================
// Float _Complex Function Calls (Tests for FP size handling)
// ============================================================================

#[test]
fn float_complex_function_return() {
    // Test function returning float _Complex (tests complex local return fix)
    let code = r#"
float _Complex make_complex(float real, float imag) {
    float _Complex result;
    float *rp = (float*)&result;
    rp[0] = real;
    rp[1] = imag;
    return result;
}

int main(void) {
    float _Complex c = make_complex(1.5f, 2.5f);
    float *cp = (float*)&c;

    // Check real part: 1.5
    if (cp[0] < 1.4f || cp[0] > 1.6f) return 1;
    // Check imaginary part: 2.5
    if (cp[1] < 2.4f || cp[1] > 2.6f) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_complex_return", code), 0);
}

#[test]
fn float_complex_function_argument() {
    // Test function taking float _Complex argument
    // Tests that float _Complex uses 4-byte offsets for real/imag parts
    let code = r#"
int check_real(float _Complex c) {
    float *cp = (float*)&c;
    float real = cp[0];
    if (real < 3.4f || real > 3.6f) return 1;
    return 0;
}

int check_imag(float _Complex c) {
    float *cp = (float*)&c;
    float imag = cp[1];
    if (imag < 4.4f || imag > 4.6f) return 1;
    return 0;
}

int main(void) {
    float _Complex a;
    float *ap = (float*)&a;
    ap[0] = 3.5f;
    ap[1] = 4.5f;

    if (check_real(a) != 0) return 1;
    if (check_imag(a) != 0) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_complex_arg", code), 0);
}

#[test]
fn float_complex_add_function() {
    // Test function that adds two float _Complex values
    // This tests that float _Complex uses 4-byte offsets (not 8-byte like double)
    let code = r#"
float _Complex add_fc(float _Complex x, float _Complex y) {
    return x + y;
}

int main(void) {
    float _Complex a, b, c;
    float *ap = (float*)&a;
    float *bp = (float*)&b;
    float *cp = (float*)&c;

    // a = 1 + 2i
    ap[0] = 1.0f; ap[1] = 2.0f;
    // b = 3 + 4i
    bp[0] = 3.0f; bp[1] = 4.0f;

    c = add_fc(a, b);

    // c should be 4 + 6i
    if (cp[0] < 3.9f || cp[0] > 4.1f) return 1;
    if (cp[1] < 5.9f || cp[1] > 6.1f) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_complex_add_func", code), 0);
}

#[test]
fn float_complex_sub_function() {
    // Test function that subtracts two float _Complex values
    let code = r#"
float _Complex sub_fc(float _Complex x, float _Complex y) {
    return x - y;
}

int main(void) {
    float _Complex a, b, c;
    float *ap = (float*)&a;
    float *bp = (float*)&b;
    float *cp = (float*)&c;

    // a = 5 + 7i
    ap[0] = 5.0f; ap[1] = 7.0f;
    // b = 3 + 4i
    bp[0] = 3.0f; bp[1] = 4.0f;

    c = sub_fc(a, b);

    // c should be 2 + 3i
    if (cp[0] < 1.9f || cp[0] > 2.1f) return 1;
    if (cp[1] < 2.9f || cp[1] > 3.1f) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_complex_sub_func", code), 0);
}

#[test]
fn float_complex_mul_function() {
    // Test function that multiplies two float _Complex values
    let code = r#"
float _Complex mul_fc(float _Complex x, float _Complex y) {
    return x * y;
}

int main(void) {
    float _Complex a, b, c;
    float *ap = (float*)&a;
    float *bp = (float*)&b;
    float *cp = (float*)&c;

    // a = 1 + 2i
    ap[0] = 1.0f; ap[1] = 2.0f;
    // b = 3 + 4i
    bp[0] = 3.0f; bp[1] = 4.0f;

    c = mul_fc(a, b);

    // c should be -5 + 10i (1*3 - 2*4 + (1*4 + 2*3)i)
    if (cp[0] < -5.1f || cp[0] > -4.9f) return 1;
    if (cp[1] < 9.9f || cp[1] > 10.1f) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_complex_mul_func", code), 0);
}
