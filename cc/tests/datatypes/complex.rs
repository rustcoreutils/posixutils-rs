//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 `_Complex` data types (float _Complex, double _Complex)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Double _Complex Arithmetic (add, sub, mul, div)
// ============================================================================

#[test]
fn double_complex_arithmetic() {
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

    // Test 1-2: Addition (4 + 6i)
    sum = a + b;
    if (sp[0] < 3.9 || sp[0] > 4.1) return 1;
    if (sp[1] < 5.9 || sp[1] > 6.1) return 2;

    // Test 3-4: Subtraction (-2 - 2i)
    ap[0] = 5.0; ap[1] = 7.0;
    diff = a - b;
    if (dp[0] < 1.9 || dp[0] > 2.1) return 3;
    if (dp[1] < 2.9 || dp[1] > 3.1) return 4;

    // Test 5-6: Multiplication (-5 + 10i)
    ap[0] = 1.0; ap[1] = 2.0;
    prod = a * b;
    if (pp[0] < -5.1 || pp[0] > -4.9) return 5;
    if (pp[1] < 9.9 || pp[1] > 10.1) return 6;

    // Test 7-8: Division (0.44 + 0.08i)
    quot = a / b;
    if (qp[0] < 0.43 || qp[0] > 0.45) return 7;
    if (qp[1] < 0.07 || qp[1] > 0.09) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("double_complex_arith", code), 0);
}

// ============================================================================
// Float _Complex Operations and Functions
// ============================================================================

#[test]
fn float_complex_operations() {
    let code = r#"
float _Complex make_fc(float real, float imag) {
    float _Complex result;
    float *rp = (float*)&result;
    rp[0] = real;
    rp[1] = imag;
    return result;
}

float _Complex add_fc(float _Complex x, float _Complex y) {
    return x + y;
}

float _Complex sub_fc(float _Complex x, float _Complex y) {
    return x - y;
}

float _Complex mul_fc(float _Complex x, float _Complex y) {
    return x * y;
}

int check_real(float _Complex c) {
    float *cp = (float*)&c;
    if (cp[0] < 3.4f || cp[0] > 3.6f) return 1;
    return 0;
}

int main(void) {
    float _Complex a, b, c;
    float *ap = (float*)&a;
    float *bp = (float*)&b;
    float *cp = (float*)&c;

    // Test 1-2: Basic addition (4 + 6i)
    ap[0] = 1.0f; ap[1] = 2.0f;
    bp[0] = 3.0f; bp[1] = 4.0f;
    c = a + b;
    if (cp[0] < 3.9f || cp[0] > 4.1f) return 1;
    if (cp[1] < 5.9f || cp[1] > 6.1f) return 2;

    // Test 3-4: Multiplication (-5 + 10i)
    c = a * b;
    if (cp[0] < -5.1f || cp[0] > -4.9f) return 3;
    if (cp[1] < 9.9f || cp[1] > 10.1f) return 4;

    // Test 5-6: Function return
    c = make_fc(1.5f, 2.5f);
    if (cp[0] < 1.4f || cp[0] > 1.6f) return 5;
    if (cp[1] < 2.4f || cp[1] > 2.6f) return 6;

    // Test 7: Function argument
    ap[0] = 3.5f; ap[1] = 4.5f;
    if (check_real(a) != 0) return 7;

    // Test 8-9: Add function
    ap[0] = 1.0f; ap[1] = 2.0f;
    bp[0] = 3.0f; bp[1] = 4.0f;
    c = add_fc(a, b);
    if (cp[0] < 3.9f || cp[0] > 4.1f) return 8;
    if (cp[1] < 5.9f || cp[1] > 6.1f) return 9;

    // Test 10-11: Sub function
    ap[0] = 5.0f; ap[1] = 7.0f;
    c = sub_fc(a, b);
    if (cp[0] < 1.9f || cp[0] > 2.1f) return 10;
    if (cp[1] < 2.9f || cp[1] > 3.1f) return 11;

    // Test 12-13: Mul function
    ap[0] = 1.0f; ap[1] = 2.0f;
    c = mul_fc(a, b);
    if (cp[0] < -5.1f || cp[0] > -4.9f) return 12;
    if (cp[1] < 9.9f || cp[1] > 10.1f) return 13;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_complex_ops", code), 0);
}

// ============================================================================
// Complex Types, Arrays, and Assignment
// ============================================================================

#[test]
fn complex_types_and_storage() {
    let code = r#"
double _Complex dc1;
_Complex double dc2;
float _Complex fc1;
_Complex float fc2;
double _Complex arr[3];

int main(void) {
    double *p1 = (double*)&dc1;
    double *p2 = (double*)&dc2;
    float *p3 = (float*)&fc1;
    float *p4 = (float*)&fc2;

    // Test 1-4: Type variants and sizes
    p1[0] = 1.0; p1[1] = 2.0;
    p2[0] = 3.0; p2[1] = 4.0;
    p3[0] = 5.0f; p3[1] = 6.0f;
    p4[0] = 7.0f; p4[1] = 8.0f;
    if (sizeof(dc1) != 16) return 1;
    if (sizeof(dc2) != 16) return 2;
    if (sizeof(fc1) != 8) return 3;
    if (sizeof(fc2) != 8) return 4;

    // Test 5-6: Values stored correctly
    if (p1[0] < 0.9 || p1[0] > 1.1) return 5;
    if (p2[1] < 3.9 || p2[1] > 4.1) return 6;

    // Test 7-8: Assignment
    double _Complex a, b;
    double *ap = (double*)&a;
    double *bp = (double*)&b;
    ap[0] = 10.0; ap[1] = 20.0;
    b = a;
    if (bp[0] < 9.9 || bp[0] > 10.1) return 7;
    if (bp[1] < 19.9 || bp[1] > 20.1) return 8;

    // Test 9: Local variables
    double _Complex la, lb, lc;
    double *lap = (double*)&la;
    double *lbp = (double*)&lb;
    double *lcp = (double*)&lc;
    lap[0] = 2.0; lap[1] = 3.0;
    lbp[0] = 4.0; lbp[1] = 5.0;
    lc = la + lb;
    if (lcp[0] < 5.9 || lcp[0] > 6.1) return 9;

    // Test 10-12: Arrays
    double *arrp = (double*)arr;
    arrp[0] = 1.0; arrp[1] = 2.0;
    arrp[2] = 3.0; arrp[3] = 4.0;
    arrp[4] = 5.0; arrp[5] = 6.0;
    if (sizeof(arr) != 48) return 10;
    double _Complex sum = arr[0] + arr[1];
    double *sp = (double*)&sum;
    if (sp[0] < 3.9 || sp[0] > 4.1) return 11;
    if (sp[1] < 5.9 || sp[1] > 6.1) return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_types", code), 0);
}
