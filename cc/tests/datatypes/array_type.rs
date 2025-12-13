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

use crate::common::compile_and_run;

// ============================================================================
// Basic Array Declaration and Access
// ============================================================================

#[test]
fn array_basic_int() {
    let code = r#"
int main(void) {
    int arr[3];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;

    if (arr[0] != 10) return 1;
    if (arr[1] != 20) return 2;
    if (arr[2] != 30) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_basic_int", code), 0);
}

#[test]
fn array_basic_char() {
    let code = r#"
int main(void) {
    char arr[4];
    arr[0] = 'a';
    arr[1] = 'b';
    arr[2] = 'c';
    arr[3] = 0;

    if (arr[0] != 'a') return 1;
    if (arr[1] != 'b') return 2;
    if (arr[2] != 'c') return 3;
    if (arr[3] != 0) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_basic_char", code), 0);
}

#[test]
fn array_basic_long() {
    let code = r#"
int main(void) {
    long arr[3];
    arr[0] = 1000000000L;
    arr[1] = 2000000000L;
    arr[2] = 3000000000L;

    if (arr[0] != 1000000000L) return 1;
    if (arr[1] != 2000000000L) return 2;
    if (arr[2] != 3000000000L) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_basic_long", code), 0);
}

// ============================================================================
// Array Initialization
// ============================================================================

#[test]
fn array_init_full() {
    let code = r#"
int main(void) {
    int arr[5] = {1, 2, 3, 4, 5};

    if (arr[0] != 1) return 1;
    if (arr[1] != 2) return 2;
    if (arr[2] != 3) return 3;
    if (arr[3] != 4) return 4;
    if (arr[4] != 5) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_init_full", code), 0);
}

#[test]
fn array_init_designated() {
    let code = r#"
int main(void) {
    int arr[5] = {[1] = 10, [3] = 30};

    if (arr[1] != 10) return 1;
    if (arr[3] != 30) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_init_desig", code), 0);
}

#[test]
fn array_init_char_list() {
    let code = r#"
int main(void) {
    char arr[5] = {'h', 'e', 'l', 'l', 'o'};

    if (arr[0] != 'h') return 1;
    if (arr[1] != 'e') return 2;
    if (arr[2] != 'l') return 3;
    if (arr[3] != 'l') return 4;
    if (arr[4] != 'o') return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_init_char", code), 0);
}

// ============================================================================
// Array sizeof
// ============================================================================

#[test]
fn array_sizeof_int() {
    let code = r#"
int main(void) {
    int arr[10];

    // sizeof(int) is 4, so 10 * 4 = 40
    if (sizeof(arr) != 40) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_sizeof_int", code), 0);
}

#[test]
fn array_sizeof_char() {
    let code = r#"
int main(void) {
    char arr[20];

    // sizeof(char) is 1, so 20 * 1 = 20
    if (sizeof(arr) != 20) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_sizeof_char", code), 0);
}

#[test]
fn array_sizeof_long() {
    let code = r#"
int main(void) {
    long arr[5];

    // sizeof(long) is 8, so 5 * 8 = 40
    if (sizeof(arr) != 40) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_sizeof_long", code), 0);
}

// ============================================================================
// Array Decay to Pointer
// ============================================================================

#[test]
fn array_decay_basic() {
    let code = r#"
int main(void) {
    int arr[3] = {10, 20, 30};
    int *p = arr;  // Array decays to pointer

    if (*p != 10) return 1;
    if (p[0] != 10) return 2;
    if (p[1] != 20) return 3;
    if (p[2] != 30) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_decay_basic", code), 0);
}

#[test]
fn array_decay_arithmetic() {
    let code = r#"
int main(void) {
    int arr[5] = {1, 2, 3, 4, 5};
    int *p = arr;

    // Pointer arithmetic on decayed array
    if (*(p + 0) != 1) return 1;
    if (*(p + 1) != 2) return 2;
    if (*(p + 2) != 3) return 3;
    if (*(p + 3) != 4) return 4;
    if (*(p + 4) != 5) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_decay_arith", code), 0);
}

#[test]
fn array_decay_modify() {
    let code = r#"
int main(void) {
    int arr[3] = {1, 2, 3};
    int *p = arr;

    // Modify through pointer
    *p = 100;
    *(p + 1) = 200;
    *(p + 2) = 300;

    if (arr[0] != 100) return 1;
    if (arr[1] != 200) return 2;
    if (arr[2] != 300) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_decay_modify", code), 0);
}

// ============================================================================
// Multi-dimensional Arrays
// ============================================================================

#[test]
fn array_2d_basic() {
    let code = r#"
int main(void) {
    int arr[2][3];

    arr[0][0] = 1;
    arr[0][1] = 2;
    arr[0][2] = 3;
    arr[1][0] = 4;
    arr[1][1] = 5;
    arr[1][2] = 6;

    if (arr[0][0] != 1) return 1;
    if (arr[0][1] != 2) return 2;
    if (arr[0][2] != 3) return 3;
    if (arr[1][0] != 4) return 4;
    if (arr[1][1] != 5) return 5;
    if (arr[1][2] != 6) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_2d_basic", code), 0);
}

#[test]
fn array_2d_init() {
    let code = r#"
int main(void) {
    int arr[2][3] = {{1, 2, 3}, {4, 5, 6}};

    if (arr[0][0] != 1) return 1;
    if (arr[0][1] != 2) return 2;
    if (arr[0][2] != 3) return 3;
    if (arr[1][0] != 4) return 4;
    if (arr[1][1] != 5) return 5;
    if (arr[1][2] != 6) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_2d_init", code), 0);
}

#[test]
fn array_2d_sizeof() {
    let code = r#"
int main(void) {
    int arr[2][3];

    // Total size: 2 * 3 * 4 = 24 bytes
    if (sizeof(arr) != 24) return 1;

    // Row size: 3 * 4 = 12 bytes
    if (sizeof(arr[0]) != 12) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_2d_sizeof", code), 0);
}

#[test]
fn array_3d_basic() {
    let code = r#"
int main(void) {
    int arr[2][2][2];

    arr[0][0][0] = 1;
    arr[0][0][1] = 2;
    arr[0][1][0] = 3;
    arr[0][1][1] = 4;
    arr[1][0][0] = 5;
    arr[1][0][1] = 6;
    arr[1][1][0] = 7;
    arr[1][1][1] = 8;

    if (arr[0][0][0] != 1) return 1;
    if (arr[0][0][1] != 2) return 2;
    if (arr[0][1][0] != 3) return 3;
    if (arr[0][1][1] != 4) return 4;
    if (arr[1][0][0] != 5) return 5;
    if (arr[1][0][1] != 6) return 6;
    if (arr[1][1][0] != 7) return 7;
    if (arr[1][1][1] != 8) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_3d_basic", code), 0);
}

// ============================================================================
// Arrays of Pointers
// ============================================================================

#[test]
fn array_of_int_pointers() {
    let code = r#"
int main(void) {
    int a = 10, b = 20, c = 30;
    int *arr[3];

    arr[0] = &a;
    arr[1] = &b;
    arr[2] = &c;

    if (*arr[0] != 10) return 1;
    if (*arr[1] != 20) return 2;
    if (*arr[2] != 30) return 3;

    // Modify through pointer array
    *arr[0] = 100;
    if (a != 100) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_int_ptrs", code), 0);
}

#[test]
fn array_of_char_pointers() {
    let code = r#"
int main(void) {
    char x = 'X', y = 'Y', z = 'Z';
    char *arr[3];

    arr[0] = &x;
    arr[1] = &y;
    arr[2] = &z;

    if (*arr[0] != 'X') return 1;
    if (*arr[1] != 'Y') return 2;
    if (*arr[2] != 'Z') return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_char_ptrs", code), 0);
}

// ============================================================================
// Pointer to Array
// ============================================================================

#[test]
fn pointer_to_array_element() {
    let code = r#"
int main(void) {
    int arr[5] = {1, 2, 3, 4, 5};
    int *p;

    p = &arr[2];  // Pointer to third element

    if (*p != 3) return 1;
    if (p[-1] != 2) return 2;
    if (p[0] != 3) return 3;
    if (p[1] != 4) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_to_elem", code), 0);
}

// ============================================================================
// Array as Function Parameter
// ============================================================================

#[test]
fn array_param_basic() {
    let code = r#"
int sum_array(int arr[], int size) {
    int total = 0;
    int i;
    for (i = 0; i < size; i = i + 1) {
        total = total + arr[i];
    }
    return total;
}

int main(void) {
    int arr[5] = {1, 2, 3, 4, 5};
    int result = sum_array(arr, 5);

    // 1 + 2 + 3 + 4 + 5 = 15
    if (result != 15) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_param_basic", code), 0);
}

#[test]
fn array_param_modify() {
    let code = r#"
void double_array(int arr[], int size) {
    int i;
    for (i = 0; i < size; i = i + 1) {
        arr[i] = arr[i] * 2;
    }
}

int main(void) {
    int arr[3] = {1, 2, 3};
    double_array(arr, 3);

    if (arr[0] != 2) return 1;
    if (arr[1] != 4) return 2;
    if (arr[2] != 6) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_param_modify", code), 0);
}

#[test]
fn array_param_as_pointer() {
    let code = r#"
int get_first(int *arr) {
    return arr[0];
}

int get_second(int *arr) {
    return arr[1];
}

int main(void) {
    int arr[3] = {10, 20, 30};

    if (get_first(arr) != 10) return 1;
    if (get_second(arr) != 20) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_param_ptr", code), 0);
}

// ============================================================================
// Struct with Array Member
// ============================================================================

#[test]
fn struct_with_array() {
    let code = r#"
struct data {
    int id;
    int values[3];
};

int main(void) {
    struct data d;
    d.id = 42;
    d.values[0] = 10;
    d.values[1] = 20;
    d.values[2] = 30;

    if (d.id != 42) return 1;
    if (d.values[0] != 10) return 2;
    if (d.values[1] != 20) return 3;
    if (d.values[2] != 30) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_arr", code), 0);
}

#[test]
fn struct_with_array_init() {
    let code = r#"
struct data {
    int id;
    int values[3];
};

int main(void) {
    struct data d = {42, {10, 20, 30}};

    if (d.id != 42) return 1;
    if (d.values[0] != 10) return 2;
    if (d.values[1] != 20) return 3;
    if (d.values[2] != 30) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_arr_init", code), 0);
}

#[test]
fn struct_array_member_pointer() {
    let code = r#"
struct data {
    int id;
    int values[3];
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
    struct data d = {1, {10, 20, 30}};

    // Pass struct array member to function
    int sum = sum_values(d.values, 3);
    if (sum != 60) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_arr_ptr", code), 0);
}

// ============================================================================
// Array of Structs
// ============================================================================

#[test]
fn array_of_structs() {
    let code = r#"
struct point {
    int x;
    int y;
};

int main(void) {
    struct point points[3];

    points[0].x = 1;
    points[0].y = 2;
    points[1].x = 3;
    points[1].y = 4;
    points[2].x = 5;
    points[2].y = 6;

    if (points[0].x != 1) return 1;
    if (points[0].y != 2) return 2;
    if (points[1].x != 3) return 3;
    if (points[1].y != 4) return 4;
    if (points[2].x != 5) return 5;
    if (points[2].y != 6) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_of_structs", code), 0);
}

#[test]
fn array_of_structs_init() {
    let code = r#"
struct point {
    int x;
    int y;
};

int main(void) {
    struct point points[3] = {{1, 2}, {3, 4}, {5, 6}};

    if (points[0].x != 1) return 1;
    if (points[0].y != 2) return 2;
    if (points[1].x != 3) return 3;
    if (points[1].y != 4) return 4;
    if (points[2].x != 5) return 5;
    if (points[2].y != 6) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_structs_init", code), 0);
}

// ============================================================================
// Array Index Expressions
// ============================================================================

#[test]
fn array_index_variable() {
    let code = r#"
int main(void) {
    int arr[5] = {10, 20, 30, 40, 50};
    int i;
    int sum = 0;

    for (i = 0; i < 5; i = i + 1) {
        sum = sum + arr[i];
    }

    // 10 + 20 + 30 + 40 + 50 = 150
    if (sum != 150) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_idx_var", code), 0);
}

#[test]
fn array_index_expression() {
    let code = r#"
int main(void) {
    int arr[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    int i = 2;

    if (arr[i * 2] != 4) return 1;      // arr[4]
    if (arr[i + 3] != 5) return 2;      // arr[5]
    if (arr[i * 2 + 1] != 5) return 3;  // arr[5]

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

    // arr[i] is equivalent to *(arr + i)
    if (arr[0] != *(arr + 0)) return 1;
    if (arr[1] != *(arr + 1)) return 2;
    if (arr[2] != *(arr + 2)) return 3;
    if (arr[3] != *(arr + 3)) return 4;
    if (arr[4] != *(arr + 4)) return 5;

    // Also equivalent: i[arr] == arr[i] == *(arr + i) == *(i + arr)
    if (0[arr] != 10) return 6;
    if (1[arr] != 20) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_ptr_equiv", code), 0);
}

// ============================================================================
// Array Bounds (no checking, but layout)
// ============================================================================

#[test]
fn array_adjacent_layout() {
    let code = r#"
int main(void) {
    int arr[3] = {1, 2, 3};
    int *p = arr;

    // Elements are contiguous in memory
    // Second element is at p + 1
    if (*(p + 1) != 2) return 1;

    // Using pointer arithmetic to compute addresses
    long addr0 = (long)&arr[0];
    long addr1 = (long)&arr[1];
    long addr2 = (long)&arr[2];

    // Each int is 4 bytes apart
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
fn array_single_element() {
    let code = r#"
int main(void) {
    int arr[1] = {42};

    if (arr[0] != 42) return 1;
    if (sizeof(arr) != 4) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_single", code), 0);
}

#[test]
fn array_nested_index() {
    let code = r#"
int main(void) {
    int arr[3] = {0, 1, 2};
    int indices[3] = {2, 0, 1};

    // arr[indices[i]] - nested array access
    if (arr[indices[0]] != 2) return 1;  // arr[2] = 2
    if (arr[indices[1]] != 0) return 2;  // arr[0] = 0
    if (arr[indices[2]] != 1) return 3;  // arr[1] = 1

    return 0;
}
"#;
    assert_eq!(compile_and_run("arr_nested_idx", code), 0);
}
