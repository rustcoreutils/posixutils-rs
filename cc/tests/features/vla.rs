//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 Variable Length Arrays (VLAs)
//
// VLAs allow array sizes to be determined at runtime:
//   void f(int n) { int arr[n]; ... }
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// VLA: Basic Operations (basic, computed_size, multiple, nested, types, expr)
// ============================================================================

#[test]
fn vla_basic_operations() {
    let code = r#"
int test_basic(int n) {
    int arr[n];
    arr[0] = 1;
    arr[n-1] = 2;
    return arr[0] + arr[n-1];
}

int test_computed(int n) {
    int arr[n];
    for (int i = 0; i < n; i++) {
        arr[i] = i * 10;
    }
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

int test_multiple(int n, int m) {
    int arr1[n];
    int arr2[m];
    for (int i = 0; i < n; i++) arr1[i] = i + 1;
    for (int i = 0; i < m; i++) arr2[i] = (i + 1) * 100;
    int sum1 = 0, sum2 = 0;
    for (int i = 0; i < n; i++) sum1 += arr1[i];
    for (int i = 0; i < m; i++) sum2 += arr2[i];
    return sum1 + sum2;
}

int test_char(int n) {
    char arr[n];
    for (int i = 0; i < n; i++) arr[i] = 'A' + i;
    return arr[0] + arr[n-1] - 'A' - 'A';
}

int test_short(int n) {
    short arr[n];
    for (int i = 0; i < n; i++) arr[i] = i * 100;
    return arr[n-1];
}

int test_long(int n) {
    long arr[n];
    for (int i = 0; i < n; i++) arr[i] = (long)i * 1000000L;
    return (int)(arr[n-1] / 1000000L);
}

int test_expr(int a, int b) {
    int arr[a + b];
    for (int i = 0; i < a + b; i++) arr[i] = i;
    int sum = 0;
    for (int i = 0; i < a + b; i++) sum += arr[i];
    return sum;
}

void fill_array(int n, int *arr) {
    for (int i = 0; i < n; i++) arr[i] = i * 3;
}

int sum_array(int n, int *arr) {
    int sum = 0;
    for (int i = 0; i < n; i++) sum += arr[i];
    return sum;
}

int main(void) {
    // Test 1-2: Basic VLA
    if (test_basic(5) != 3) return 1;
    if (test_basic(10) != 3) return 2;

    // Test 3-4: Computed size
    if (test_computed(5) != 100) return 3;   // 0+10+20+30+40
    if (test_computed(10) != 450) return 4;  // 0+10+...+90

    // Test 5: Multiple VLAs
    if (test_multiple(3, 4) != 1006) return 5;  // (1+2+3) + (100+200+300+400)

    // Test 6: Nested scope
    int n = 5;
    int total = 0;
    {
        int arr[n];
        for (int i = 0; i < n; i++) arr[i] = i * 2;
        for (int i = 0; i < n; i++) total += arr[i];
    }
    if (total != 20) return 6;  // 0+2+4+6+8

    // Test 7-9: Different types
    if (test_char(5) != 4) return 7;
    if (test_short(5) != 400) return 8;
    if (test_long(5) != 4) return 9;

    // Test 10-11: Expression size
    if (test_expr(3, 4) != 21) return 10;  // 0+1+2+3+4+5+6
    if (test_expr(5, 5) != 45) return 11;  // 0+1+...+9

    // Test 12: Passed to function
    n = 5;
    int arr[n];
    fill_array(n, arr);
    if (sum_array(n, arr) != 30) return 12;  // 0+3+6+9+12

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_basic_ops", code), 0);
}

// ============================================================================
// VLA: sizeof (runtime size computation)
// ============================================================================

#[test]
fn vla_sizeof() {
    let code = r#"
int test_sizeof(int n) {
    int arr[n];
    return sizeof(arr);
}

int main(void) {
    // Test 1-3: Basic sizeof
    if (test_sizeof(5) != 20) return 1;   // 5 * 4
    if (test_sizeof(10) != 40) return 2;  // 10 * 4
    if (test_sizeof(1) != 4) return 3;    // 1 * 4

    // Test 4-6: Different element types
    int n = 5;
    char carr[n];
    if (sizeof(carr) != 5) return 4;      // 5 * 1
    short sarr[n];
    if (sizeof(sarr) != 10) return 5;     // 5 * 2
    long larr[n];
    if (sizeof(larr) != 40) return 6;     // 5 * 8

    // Test 7-8: Computed size expression (sizeof evaluated once at declaration)
    int a = 3;
    int b = 4;
    int arr[a + b];  // size = 7
    if (sizeof(arr) != 28) return 7;      // 7 * 4
    a = 100;  // Changing a should not affect sizeof
    if (sizeof(arr) != 28) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_sizeof", code), 0);
}

// ============================================================================
// VLA: Function Parameter Syntax (int arr[n])
// ============================================================================

#[test]
fn vla_function_params() {
    let code = r#"
void fill_array(int n, int arr[n]) {
    for (int i = 0; i < n; i++) {
        arr[i] = i * 2;
    }
}

int sum_array(int n, int arr[n]) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

int main(void) {
    // Test 1: Size 5
    int arr[5];
    fill_array(5, arr);
    int sum = sum_array(5, arr);
    if (sum != 20) return 1;  // 0+2+4+6+8

    // Test 2: Size 10
    int arr2[10];
    fill_array(10, arr2);
    sum = sum_array(10, arr2);
    if (sum != 90) return 2;  // 0+2+4+6+8+10+12+14+16+18

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_func_params", code), 0);
}

// ============================================================================
// VLA: Multidimensional (2D VLA with runtime stride)
// ============================================================================

#[test]
fn vla_multidimensional() {
    let code = r#"
int main(void) {
    // Test 1-7: Basic 2D VLA
    int n = 3;
    int m = 4;
    int arr[n][m];

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            arr[i][j] = i * 10 + j;
        }
    }

    if (arr[0][0] != 0) return 1;
    if (arr[0][3] != 3) return 2;
    if (arr[1][0] != 10) return 3;
    if (arr[1][3] != 13) return 4;
    if (arr[2][0] != 20) return 5;
    if (arr[2][3] != 23) return 6;
    if (sizeof(arr) != 48) return 7;  // 3 * 4 * 4

    // Test 8-9: No overlap (bug regression test)
    int arr2[n][m];
    arr2[0][3] = 100;
    arr2[2][3] = 200;
    if (arr2[0][3] != 100) return 8;
    if (arr2[2][3] != 200) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_multidim", code), 0);
}

// ============================================================================
// VLA: [*] Syntax in Function Prototypes (C99 6.7.5.2)
// ============================================================================

#[test]
fn vla_star_syntax() {
    let code = r#"
// Prototypes with [*] - size unspecified
void fill_array_star(int n, int arr[*]);
int sum_1d(int n, int arr[*]);
void zero_array(int n, int arr[*]);
int product_1d(int n, int arr[*]);

// Prototype with const qualifier
void read_array(int n, const int arr[*]);

// Prototype with restrict qualifier
void write_array(int n, int arr[restrict *]);

// Prototype with mixed dimensions
void process_rows(int n, int mat[*][4]);

// Definitions
void fill_array_star(int n, int arr[n]) {
    for (int i = 0; i < n; i++) arr[i] = i * 3;
}

int sum_1d(int n, int arr[n]) {
    int sum = 0;
    for (int i = 0; i < n; i++) sum += arr[i];
    return sum;
}

void zero_array(int n, int arr[n]) {
    for (int i = 0; i < n; i++) arr[i] = 0;
}

int product_1d(int n, int arr[n]) {
    int prod = 1;
    for (int i = 0; i < n; i++) prod *= arr[i];
    return prod;
}

void read_array(int n, const int arr[n]) {
    int sum = 0;
    for (int i = 0; i < n; i++) sum += arr[i];
}

void write_array(int n, int arr[restrict n]) {
    for (int i = 0; i < n; i++) arr[i] = i;
}

void process_rows(int n, int mat[n][4]) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < 4; j++) {
            mat[i][j] = (i + 1) * 10 + j;
        }
    }
}

int main(void) {
    // Test 1: Basic [*] prototype
    int n = 5;
    int arr[n];
    fill_array_star(n, arr);
    if (sum_1d(n, arr) != 30) return 1;  // 0+3+6+9+12

    // Test 2-3: Multiple [*] prototypes
    int arr2[5] = {1, 2, 3, 4, 5};
    if (sum_1d(5, arr2) != 15) return 2;
    if (product_1d(5, arr2) != 120) return 3;

    // Test 4: Zero array
    zero_array(5, arr2);
    if (sum_1d(5, arr2) != 0) return 4;

    // Test 5-6: Qualifiers (const, restrict)
    int arr3[5] = {1, 2, 3, 4, 5};
    read_array(5, arr3);
    write_array(5, arr3);  // arr3 = {0, 1, 2, 3, 4}
    if (arr3[0] != 0) return 5;
    if (arr3[4] != 4) return 6;

    // Test 7-10: Mixed dimensions [*][4]
    n = 3;
    int matrix[n][4];
    process_rows(n, matrix);
    if (matrix[0][0] != 10) return 7;   // 1*10 + 0
    if (matrix[0][3] != 13) return 8;   // 1*10 + 3
    if (matrix[2][0] != 30) return 9;   // 3*10 + 0
    if (matrix[2][3] != 33) return 10;  // 3*10 + 3

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_star_syntax", code), 0);
}
