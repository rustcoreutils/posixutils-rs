//
// Copyright (c) 2024 Jeff Garzik
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

use crate::common::compile_and_run;

// ============================================================================
// VLA: Basic tests
// ============================================================================

#[test]
fn vla_basic() {
    let code = r#"
int test_vla(int n) {
    int arr[n];
    arr[0] = 1;
    arr[n-1] = 2;
    return arr[0] + arr[n-1];
}

int main(void) {
    int result = test_vla(5);
    if (result != 3) return 1;

    result = test_vla(10);
    if (result != 3) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_basic", code), 0);
}

#[test]
fn vla_computed_size() {
    let code = r#"
int test_vla(int n) {
    int arr[n];

    // Initialize array
    for (int i = 0; i < n; i++) {
        arr[i] = i * 10;
    }

    // Sum and return
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

int main(void) {
    // Test with size 5: sum = 0 + 10 + 20 + 30 + 40 = 100
    int result = test_vla(5);
    if (result != 100) return 1;

    // Test with size 10: sum = 0 + 10 + 20 + ... + 90 = 450
    result = test_vla(10);
    if (result != 450) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_computed_size", code), 0);
}

#[test]
fn vla_multiple_arrays() {
    let code = r#"
int test_vla(int n, int m) {
    int arr1[n];
    int arr2[m];

    // Initialize first array
    for (int i = 0; i < n; i++) {
        arr1[i] = i + 1;
    }

    // Initialize second array
    for (int i = 0; i < m; i++) {
        arr2[i] = (i + 1) * 100;
    }

    // Verify first array wasn't corrupted
    int sum1 = 0;
    for (int i = 0; i < n; i++) {
        sum1 += arr1[i];
    }

    // Sum second array
    int sum2 = 0;
    for (int i = 0; i < m; i++) {
        sum2 += arr2[i];
    }

    return sum1 + sum2;
}

int main(void) {
    // n=3: 1+2+3=6, m=4: 100+200+300+400=1000, total=1006
    int result = test_vla(3, 4);
    if (result != 1006) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_multiple_arrays", code), 0);
}

#[test]
fn vla_in_nested_scope() {
    let code = r#"
int main(void) {
    int n = 5;
    int total = 0;

    {
        int arr[n];
        for (int i = 0; i < n; i++) {
            arr[i] = i * 2;
        }
        for (int i = 0; i < n; i++) {
            total += arr[i];
        }
    }

    // Sum = 0 + 2 + 4 + 6 + 8 = 20
    if (total != 20) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_in_nested_scope", code), 0);
}

#[test]
fn vla_different_types() {
    let code = r#"
int test_char_vla(int n) {
    char arr[n];
    for (int i = 0; i < n; i++) {
        arr[i] = 'A' + i;
    }
    return arr[0] + arr[n-1] - 'A' - 'A';
}

int test_short_vla(int n) {
    short arr[n];
    for (int i = 0; i < n; i++) {
        arr[i] = i * 100;
    }
    return arr[n-1];
}

int test_long_vla(int n) {
    long arr[n];
    for (int i = 0; i < n; i++) {
        arr[i] = (long)i * 1000000L;
    }
    return (int)(arr[n-1] / 1000000L);
}

int main(void) {
    // char VLA: arr[0]='A', arr[4]='E', result = 0 + 4 = 4
    if (test_char_vla(5) != 4) return 1;

    // short VLA: arr[4] = 400
    if (test_short_vla(5) != 400) return 2;

    // long VLA: arr[4] = 4000000, result = 4
    if (test_long_vla(5) != 4) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_different_types", code), 0);
}

#[test]
fn vla_expression_size() {
    let code = r#"
// Test VLA with expressions for size
int test(int a, int b) {
    int arr[a + b];

    // Initialize: arr[i] = i
    for (int i = 0; i < a + b; i++) {
        arr[i] = i;
    }

    // Sum: 0 + 1 + 2 + ... + (n-1) = n*(n-1)/2
    int sum = 0;
    for (int i = 0; i < a + b; i++) {
        sum += arr[i];
    }
    return sum;
}

int main(void) {
    // a=3, b=4 -> size=7, sum = 0+1+2+3+4+5+6 = 21
    int result = test(3, 4);
    if (result != 21) return 1;

    // a=5, b=5 -> size=10, sum = 0+1+...+9 = 45
    result = test(5, 5);
    if (result != 45) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_expression_size", code), 0);
}

#[test]
fn vla_passed_to_function() {
    let code = r#"
// VLA passed to function as pointer
void fill_array(int n, int *arr) {
    for (int i = 0; i < n; i++) {
        arr[i] = i * 3;
    }
}

int sum_array(int n, int *arr) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

int main(void) {
    int n = 5;
    int arr[n];
    fill_array(n, arr);
    int result = sum_array(n, arr);

    // Sum = 0 + 3 + 6 + 9 + 12 = 30
    if (result != 30) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_passed_to_function", code), 0);
}

#[test]
fn vla_sizeof() {
    let code = r#"
// Test sizeof(vla) returns correct runtime size
int test_sizeof(int n) {
    int arr[n];
    // sizeof(arr) should equal n * sizeof(int)
    return sizeof(arr);
}

int main(void) {
    // n=5: sizeof should be 5 * 4 = 20
    int size5 = test_sizeof(5);
    if (size5 != 20) return 1;

    // n=10: sizeof should be 10 * 4 = 40
    int size10 = test_sizeof(10);
    if (size10 != 40) return 2;

    // n=1: sizeof should be 1 * 4 = 4
    int size1 = test_sizeof(1);
    if (size1 != 4) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_sizeof", code), 0);
}

#[test]
fn vla_sizeof_different_types() {
    let code = r#"
// Test sizeof(vla) with different element types
int main(void) {
    int n = 5;

    // char array: sizeof = 5 * 1 = 5
    char carr[n];
    if (sizeof(carr) != 5) return 1;

    // short array: sizeof = 5 * 2 = 10
    short sarr[n];
    if (sizeof(sarr) != 10) return 2;

    // long array: sizeof = 5 * 8 = 40
    long larr[n];
    if (sizeof(larr) != 40) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_sizeof_different_types", code), 0);
}

#[test]
fn vla_sizeof_computed_size() {
    let code = r#"
// Test sizeof(vla) with computed size expression
int main(void) {
    int a = 3;
    int b = 4;
    int arr[a + b];  // size = 7

    // sizeof should be 7 * 4 = 28
    if (sizeof(arr) != 28) return 1;

    // Verify the size is computed once at declaration time
    a = 100;  // Changing a should not affect sizeof
    if (sizeof(arr) != 28) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_sizeof_computed_size", code), 0);
}

#[test]
fn vla_function_parameter_syntax() {
    let code = r#"
// Test VLA function parameter syntax: int arr[n]
// C99 6.7.5.3: VLA parameters decay to pointers like regular arrays

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
    int arr[5];
    fill_array(5, arr);
    // arr = {0, 2, 4, 6, 8}
    int sum = sum_array(5, arr);
    // sum = 0 + 2 + 4 + 6 + 8 = 20
    if (sum != 20) return 1;

    // Test with different size
    int arr2[10];
    fill_array(10, arr2);
    // arr2 = {0, 2, 4, 6, 8, 10, 12, 14, 16, 18}
    sum = sum_array(10, arr2);
    // sum = 0+2+4+6+8+10+12+14+16+18 = 90
    if (sum != 90) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_function_parameter_syntax", code), 0);
}

#[test]
fn vla_multidimensional() {
    let code = r#"
// Test multi-dimensional VLA with runtime stride computation
int main(void) {
    int n = 3;
    int m = 4;
    int arr[n][m];  // 2D VLA: 3 rows, 4 columns

    // Initialize all elements: arr[i][j] = i * 10 + j
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            arr[i][j] = i * 10 + j;
        }
    }

    // Verify values at various positions
    if (arr[0][0] != 0) return 1;
    if (arr[0][3] != 3) return 2;
    if (arr[1][0] != 10) return 3;
    if (arr[1][3] != 13) return 4;
    if (arr[2][0] != 20) return 5;
    if (arr[2][3] != 23) return 6;

    // Test sizeof: should be n * m * sizeof(int) = 3 * 4 * 4 = 48
    if (sizeof(arr) != 48) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_multidimensional", code), 0);
}

#[test]
fn vla_multidimensional_no_overlap() {
    let code = r#"
// Test that multi-dimensional VLA elements don't overlap
// This was a bug where outer dimension indexing used zero stride
int main(void) {
    int n = 3;
    int m = 4;
    int arr[n][m];

    // Write to arr[0][3] and arr[2][3]
    // These should be at different memory locations
    arr[0][3] = 100;
    arr[2][3] = 200;

    // arr[0][3] should still be 100
    if (arr[0][3] != 100) return 1;
    if (arr[2][3] != 200) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_multidimensional_no_overlap", code), 0);
}

// ============================================================================
// VLA: [*] in Function Prototypes (C99 6.7.5.2)
// ============================================================================

#[test]
fn vla_star_basic_prototype() {
    // Test [*] syntax in function prototypes for VLA with unspecified size
    let code = r#"
// Prototype with [*] - size unspecified
void fill_array(int n, int arr[*]);

// Definition with actual size expression
void fill_array(int n, int arr[n]) {
    for (int i = 0; i < n; i++) {
        arr[i] = i * 3;
    }
}

int sum_array(int n, int *arr) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

int main(void) {
    int n = 5;
    int arr[n];
    fill_array(n, arr);
    int result = sum_array(n, arr);
    // Sum = 0 + 3 + 6 + 9 + 12 = 30
    if (result != 30) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_star_basic_prototype", code), 0);
}

#[test]
fn vla_star_with_qualifiers() {
    // Test [*] with type qualifiers (const, restrict)
    let code = r#"
// Prototype with const qualifier
void read_array(int n, const int arr[*]);

// Prototype with restrict qualifier
void write_array(int n, int arr[restrict *]);

// Prototype with static keyword (optimization hint)
void process_array(int n, int arr[static *]);

// Definitions
void read_array(int n, const int arr[n]) {
    // Can read but not modify
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
}

void write_array(int n, int arr[restrict n]) {
    for (int i = 0; i < n; i++) {
        arr[i] = i;
    }
}

void process_array(int n, int arr[static n]) {
    for (int i = 0; i < n; i++) {
        arr[i] *= 2;
    }
}

int main(void) {
    int arr[5] = {1, 2, 3, 4, 5};
    read_array(5, arr);
    write_array(5, arr);      // arr = {0, 1, 2, 3, 4}
    process_array(5, arr);    // arr = {0, 2, 4, 6, 8}

    if (arr[0] != 0) return 1;
    if (arr[4] != 8) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_star_with_qualifiers", code), 0);
}

#[test]
fn vla_star_multidimensional() {
    // Test [*][*] syntax parsing in function prototypes
    // Note: This tests parsing of multi-dimensional [*] syntax
    // The prototype declares a 2D VLA parameter with unspecified dimensions
    let code = r#"
// Prototype for 2D VLA with [*][*]
void fill_matrix(int rows, int cols, int mat[*][*]);

// Definition - but we test only that the prototype parses correctly
// by calling a 1D version (2D VLA function params are a separate issue)
void fill_1d_as_2d(int total, int *arr) {
    for (int i = 0; i < total; i++) {
        arr[i] = i;
    }
}

int main(void) {
    // Test that [*][*] prototypes are accepted by the parser
    // Use 1D array to verify the function works
    int arr[12];
    fill_1d_as_2d(12, arr);

    // Verify values
    if (arr[0] != 0) return 1;
    if (arr[5] != 5) return 2;
    if (arr[11] != 11) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_star_multidimensional", code), 0);
}

#[test]
fn vla_star_mixed_dimensions() {
    // Test mixing [*] with fixed-size dimensions
    let code = r#"
// Prototype with mixed dimensions: first is VLA, second is fixed
void process_rows(int n, int mat[*][4]);

// Definition
void process_rows(int n, int mat[n][4]) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < 4; j++) {
            mat[i][j] = (i + 1) * 10 + j;
        }
    }
}

int main(void) {
    int n = 3;
    int matrix[n][4];
    process_rows(n, matrix);

    // Check values: mat[i][j] = (i+1)*10 + j
    if (matrix[0][0] != 10) return 1;   // 1*10 + 0
    if (matrix[0][3] != 13) return 2;   // 1*10 + 3
    if (matrix[2][0] != 30) return 3;   // 3*10 + 0
    if (matrix[2][3] != 33) return 4;   // 3*10 + 3

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_star_mixed_dimensions", code), 0);
}

#[test]
fn vla_star_multiple_prototypes() {
    // Test multiple function prototypes with [*]
    let code = r#"
// Multiple prototypes using [*]
int sum_1d(int n, int arr[*]);
void zero_array(int n, int arr[*]);
int product_1d(int n, int arr[*]);

// Also test that 2D [*][*] is parseable (even if we don't call it)
int sum_2d(int rows, int cols, int mat[*][*]);

// Definitions
int sum_1d(int n, int arr[n]) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

void zero_array(int n, int arr[n]) {
    for (int i = 0; i < n; i++) {
        arr[i] = 0;
    }
}

int product_1d(int n, int arr[n]) {
    int prod = 1;
    for (int i = 0; i < n; i++) {
        prod *= arr[i];
    }
    return prod;
}

int main(void) {
    int arr[5] = {1, 2, 3, 4, 5};

    // Test sum_1d: 1+2+3+4+5 = 15
    if (sum_1d(5, arr) != 15) return 1;

    // Test product_1d: 1*2*3*4*5 = 120
    if (product_1d(5, arr) != 120) return 2;

    // Test zero_array
    zero_array(5, arr);
    if (sum_1d(5, arr) != 0) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_star_multiple_prototypes", code), 0);
}
