//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 array types
//
// C99 array semantics:
// - Arrays are contiguous sequences of elements of the same type
// - Array names decay to pointers to the first element in most contexts
// - arr[i] is equivalent to *(arr + i)
// - Multi-dimensional arrays are arrays of arrays
// - Arrays cannot be assigned or passed by value
// - sizeof(array) returns total size in bytes
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Array Declaration and Access (int, char, long)
// ============================================================================

#[test]
fn array_basic_types() {
    let code = r#"
int main(void) {
    // Test 1-3: int array
    int arr_int[3];
    arr_int[0] = 10;
    arr_int[1] = 20;
    arr_int[2] = 30;
    if (arr_int[0] != 10) return 1;
    if (arr_int[1] != 20) return 2;
    if (arr_int[2] != 30) return 3;

    // Test 4-7: char array
    char arr_char[4];
    arr_char[0] = 'a';
    arr_char[1] = 'b';
    arr_char[2] = 'c';
    arr_char[3] = 0;
    if (arr_char[0] != 'a') return 4;
    if (arr_char[1] != 'b') return 5;
    if (arr_char[2] != 'c') return 6;
    if (arr_char[3] != 0) return 7;

    // Test 8-10: long array
    long arr_long[3];
    arr_long[0] = 1000000000L;
    arr_long[1] = 2000000000L;
    arr_long[2] = 3000000000L;
    if (arr_long[0] != 1000000000L) return 8;
    if (arr_long[1] != 2000000000L) return 9;
    if (arr_long[2] != 3000000000L) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_basic_types", code), 0);
}

// ============================================================================
// Array Initialization (full, designated, char list)
// ============================================================================

#[test]
fn array_initialization() {
    let code = r#"
int main(void) {
    // Test 1-5: Full initialization
    int arr_full[5] = {1, 2, 3, 4, 5};
    if (arr_full[0] != 1) return 1;
    if (arr_full[1] != 2) return 2;
    if (arr_full[2] != 3) return 3;
    if (arr_full[3] != 4) return 4;
    if (arr_full[4] != 5) return 5;

    // Test 6-7: Designated initialization
    int arr_desig[5] = {[1] = 10, [3] = 30};
    if (arr_desig[1] != 10) return 6;
    if (arr_desig[3] != 30) return 7;

    // Test 8-12: Character list initialization
    char arr_char[5] = {'h', 'e', 'l', 'l', 'o'};
    if (arr_char[0] != 'h') return 8;
    if (arr_char[1] != 'e') return 9;
    if (arr_char[2] != 'l') return 10;
    if (arr_char[3] != 'l') return 11;
    if (arr_char[4] != 'o') return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_init", code), 0);
}

// ============================================================================
// Array sizeof (int, char, long)
// ============================================================================

#[test]
fn array_sizeof() {
    let code = r#"
int main(void) {
    // Test 1: int array sizeof (10 * 4 = 40)
    int arr_int[10];
    if (sizeof(arr_int) != 40) return 1;

    // Test 2: char array sizeof (20 * 1 = 20)
    char arr_char[20];
    if (sizeof(arr_char) != 20) return 2;

    // Test 3: long array sizeof (5 * 8 = 40)
    long arr_long[5];
    if (sizeof(arr_long) != 40) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_sizeof", code), 0);
}

// ============================================================================
// Array Decay to Pointer
// ============================================================================

#[test]
fn array_decay() {
    let code = r#"
int main(void) {
    // Test 1-4: Basic decay
    int arr1[3] = {10, 20, 30};
    int *p1 = arr1;
    if (*p1 != 10) return 1;
    if (p1[0] != 10) return 2;
    if (p1[1] != 20) return 3;
    if (p1[2] != 30) return 4;

    // Test 5-9: Decay with pointer arithmetic
    int arr2[5] = {1, 2, 3, 4, 5};
    int *p2 = arr2;
    if (*(p2 + 0) != 1) return 5;
    if (*(p2 + 1) != 2) return 6;
    if (*(p2 + 2) != 3) return 7;
    if (*(p2 + 3) != 4) return 8;
    if (*(p2 + 4) != 5) return 9;

    // Test 10-12: Modify through decayed pointer
    int arr3[3] = {1, 2, 3};
    int *p3 = arr3;
    *p3 = 100;
    *(p3 + 1) = 200;
    *(p3 + 2) = 300;
    if (arr3[0] != 100) return 10;
    if (arr3[1] != 200) return 11;
    if (arr3[2] != 300) return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_decay", code), 0);
}

// ============================================================================
// Multi-dimensional Arrays (2D, 3D)
// ============================================================================

#[test]
fn array_multidimensional() {
    let code = r#"
int main(void) {
    // Test 1-6: 2D basic access
    int arr2d[2][3];
    arr2d[0][0] = 1; arr2d[0][1] = 2; arr2d[0][2] = 3;
    arr2d[1][0] = 4; arr2d[1][1] = 5; arr2d[1][2] = 6;
    if (arr2d[0][0] != 1) return 1;
    if (arr2d[0][1] != 2) return 2;
    if (arr2d[0][2] != 3) return 3;
    if (arr2d[1][0] != 4) return 4;
    if (arr2d[1][1] != 5) return 5;
    if (arr2d[1][2] != 6) return 6;

    // Test 7-12: 2D with initializer
    int arr2d_init[2][3] = {{1, 2, 3}, {4, 5, 6}};
    if (arr2d_init[0][0] != 1) return 7;
    if (arr2d_init[0][1] != 2) return 8;
    if (arr2d_init[0][2] != 3) return 9;
    if (arr2d_init[1][0] != 4) return 10;
    if (arr2d_init[1][1] != 5) return 11;
    if (arr2d_init[1][2] != 6) return 12;

    // Test 13-14: 2D sizeof
    int arr2d_sz[2][3];
    if (sizeof(arr2d_sz) != 24) return 13;    // 2 * 3 * 4 = 24
    if (sizeof(arr2d_sz[0]) != 12) return 14; // 3 * 4 = 12

    // Test 15-22: 3D array
    int arr3d[2][2][2];
    arr3d[0][0][0] = 1; arr3d[0][0][1] = 2;
    arr3d[0][1][0] = 3; arr3d[0][1][1] = 4;
    arr3d[1][0][0] = 5; arr3d[1][0][1] = 6;
    arr3d[1][1][0] = 7; arr3d[1][1][1] = 8;
    if (arr3d[0][0][0] != 1) return 15;
    if (arr3d[0][0][1] != 2) return 16;
    if (arr3d[0][1][0] != 3) return 17;
    if (arr3d[0][1][1] != 4) return 18;
    if (arr3d[1][0][0] != 5) return 19;
    if (arr3d[1][0][1] != 6) return 20;
    if (arr3d[1][1][0] != 7) return 21;
    if (arr3d[1][1][1] != 8) return 22;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_multidim", code), 0);
}

// ============================================================================
// Arrays of Pointers
// ============================================================================

#[test]
fn array_of_pointers() {
    let code = r#"
int main(void) {
    // Test 1-4: Array of int pointers
    int a = 10, b = 20, c = 30;
    int *arr_int[3];
    arr_int[0] = &a;
    arr_int[1] = &b;
    arr_int[2] = &c;
    if (*arr_int[0] != 10) return 1;
    if (*arr_int[1] != 20) return 2;
    if (*arr_int[2] != 30) return 3;
    *arr_int[0] = 100;
    if (a != 100) return 4;

    // Test 5-7: Array of char pointers
    char x = 'X', y = 'Y', z = 'Z';
    char *arr_char[3];
    arr_char[0] = &x;
    arr_char[1] = &y;
    arr_char[2] = &z;
    if (*arr_char[0] != 'X') return 5;
    if (*arr_char[1] != 'Y') return 6;
    if (*arr_char[2] != 'Z') return 7;

    // Test 8-11: Pointer to array element
    int arr[5] = {1, 2, 3, 4, 5};
    int *p = &arr[2];
    if (*p != 3) return 8;
    if (p[-1] != 2) return 9;
    if (p[0] != 3) return 10;
    if (p[1] != 4) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_of_ptrs", code), 0);
}

// ============================================================================
// Array as Function Parameter
// ============================================================================

#[test]
fn array_function_params() {
    let code = r#"
int sum_array(int arr[], int size) {
    int total = 0;
    int i;
    for (i = 0; i < size; i = i + 1) {
        total = total + arr[i];
    }
    return total;
}

void double_array(int arr[], int size) {
    int i;
    for (i = 0; i < size; i = i + 1) {
        arr[i] = arr[i] * 2;
    }
}

int get_first(int *arr) {
    return arr[0];
}

int get_second(int *arr) {
    return arr[1];
}

int main(void) {
    // Test 1: Sum array via function
    int arr1[5] = {1, 2, 3, 4, 5};
    int result = sum_array(arr1, 5);
    if (result != 15) return 1;  // 1+2+3+4+5 = 15

    // Test 2-4: Modify array via function
    int arr2[3] = {1, 2, 3};
    double_array(arr2, 3);
    if (arr2[0] != 2) return 2;
    if (arr2[1] != 4) return 3;
    if (arr2[2] != 6) return 4;

    // Test 5-6: Array as pointer parameter
    int arr3[3] = {10, 20, 30};
    if (get_first(arr3) != 10) return 5;
    if (get_second(arr3) != 20) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_func_params", code), 0);
}

// ============================================================================
// Struct and Array Interactions
// ============================================================================

#[test]
fn array_struct_interactions() {
    let code = r#"
struct data {
    int id;
    int values[3];
};

struct point {
    int x;
    int y;
};

int sum_values(int *arr, int size) {
    int total = 0;
    int i;
    for (i = 0; i < size; i = i + 1) {
        total = total + arr[i];
    }
    return total;
}

int main(void) {
    // Test 1-4: Struct with array member
    struct data d1;
    d1.id = 42;
    d1.values[0] = 10;
    d1.values[1] = 20;
    d1.values[2] = 30;
    if (d1.id != 42) return 1;
    if (d1.values[0] != 10) return 2;
    if (d1.values[1] != 20) return 3;
    if (d1.values[2] != 30) return 4;

    // Test 5-8: Struct with array member - initialized
    struct data d2 = {42, {10, 20, 30}};
    if (d2.id != 42) return 5;
    if (d2.values[0] != 10) return 6;
    if (d2.values[1] != 20) return 7;
    if (d2.values[2] != 30) return 8;

    // Test 9: Struct array member passed to function
    struct data d3 = {1, {10, 20, 30}};
    int sum = sum_values(d3.values, 3);
    if (sum != 60) return 9;

    // Test 10-15: Array of structs
    struct point points[3];
    points[0].x = 1; points[0].y = 2;
    points[1].x = 3; points[1].y = 4;
    points[2].x = 5; points[2].y = 6;
    if (points[0].x != 1) return 10;
    if (points[0].y != 2) return 11;
    if (points[1].x != 3) return 12;
    if (points[1].y != 4) return 13;
    if (points[2].x != 5) return 14;
    if (points[2].y != 6) return 15;

    // Test 16-21: Array of structs - initialized
    struct point pts[3] = {{1, 2}, {3, 4}, {5, 6}};
    if (pts[0].x != 1) return 16;
    if (pts[0].y != 2) return 17;
    if (pts[1].x != 3) return 18;
    if (pts[1].y != 4) return 19;
    if (pts[2].x != 5) return 20;
    if (pts[2].y != 6) return 21;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_struct", code), 0);
}

// ============================================================================
// Array Index Expressions
// ============================================================================

#[test]
fn array_index_expressions() {
    let code = r#"
int main(void) {
    // Test 1: Loop with variable index
    int arr1[5] = {10, 20, 30, 40, 50};
    int i;
    int sum = 0;
    for (i = 0; i < 5; i = i + 1) {
        sum = sum + arr1[i];
    }
    if (sum != 150) return 1;  // 10+20+30+40+50 = 150

    // Test 2-4: Complex index expressions
    int arr2[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    int j = 2;
    if (arr2[j * 2] != 4) return 2;      // arr2[4]
    if (arr2[j + 3] != 5) return 3;      // arr2[5]
    if (arr2[j * 2 + 1] != 5) return 4;  // arr2[5]

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_idx_expr", code), 0);
}

// ============================================================================
// Pointer and Array Equivalence
// ============================================================================

#[test]
fn array_pointer_equivalence() {
    let code = r#"
int main(void) {
    int arr[5] = {10, 20, 30, 40, 50};

    // Test 1-5: arr[i] == *(arr + i)
    if (arr[0] != *(arr + 0)) return 1;
    if (arr[1] != *(arr + 1)) return 2;
    if (arr[2] != *(arr + 2)) return 3;
    if (arr[3] != *(arr + 3)) return 4;
    if (arr[4] != *(arr + 4)) return 5;

    // Test 6-7: i[arr] == arr[i] (commutative property)
    if (0[arr] != 10) return 6;
    if (1[arr] != 20) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_ptr_equiv", code), 0);
}

// ============================================================================
// Array Memory Layout
// ============================================================================

#[test]
fn array_memory_layout() {
    let code = r#"
int main(void) {
    int arr[3] = {1, 2, 3};
    int *p = arr;

    // Test 1: Contiguous elements via pointer
    if (*(p + 1) != 2) return 1;

    // Test 2-3: Address differences (each int is 4 bytes apart)
    long addr0 = (long)&arr[0];
    long addr1 = (long)&arr[1];
    long addr2 = (long)&arr[2];
    if (addr1 - addr0 != 4) return 2;
    if (addr2 - addr1 != 4) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_layout", code), 0);
}

// ============================================================================
// Void Pointer and Array
// ============================================================================

#[test]
fn array_void_pointer() {
    let code = r#"
int main(void) {
    int arr[3] = {100, 200, 300};
    void *vp = arr;

    int *ip = vp;
    if (ip[0] != 100) return 1;
    if (ip[1] != 200) return 2;
    if (ip[2] != 300) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_void_ptr", code), 0);
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn array_edge_cases() {
    let code = r#"
int main(void) {
    // Test 1-2: Single element array
    int arr_single[1] = {42};
    if (arr_single[0] != 42) return 1;
    if (sizeof(arr_single) != 4) return 2;

    // Test 3-5: Nested array indexing
    int arr[3] = {0, 1, 2};
    int indices[3] = {2, 0, 1};
    if (arr[indices[0]] != 2) return 3;  // arr[2] = 2
    if (arr[indices[1]] != 0) return 4;  // arr[0] = 0
    if (arr[indices[2]] != 1) return 5;  // arr[1] = 1

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_edge", code), 0);
}
