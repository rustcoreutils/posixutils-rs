//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 Array Parameter Qualifiers (C99 6.7.5.3)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Basic array parameter qualifiers (static, const, restrict, volatile)
// ============================================================================

#[test]
fn array_param_basic_qualifiers() {
    let code = r#"
// static in array parameter indicates minimum guaranteed elements
void fill_static(int n, int arr[static 10]) {
    for (int i = 0; i < n && i < 10; i++) {
        arr[i] = i;
    }
}

// const qualifier applies to the resulting pointer
int sum_const(int n, int arr[const]) {
    int s = 0;
    for (int i = 0; i < n; i++) s += arr[i];
    return s;
}

// restrict qualifier for non-overlapping pointers
void copy_restrict(int n, int dest[restrict], int src[restrict]) {
    for (int i = 0; i < n; i++) dest[i] = src[i];
}

// volatile qualifier
void read_volatile(int n, int arr[volatile]) {
    int sum = 0;
    for (int i = 0; i < n; i++) sum += arr[i];
}

int main(void) {
    // Test 1-2: static
    int arr1[10];
    fill_static(10, arr1);
    if (arr1[0] != 0) return 1;
    if (arr1[9] != 9) return 2;

    // Test 3: const
    int arr2[] = {1, 2, 3, 4, 5};
    if (sum_const(5, arr2) != 15) return 3;

    // Test 4: restrict
    int a[5] = {1, 2, 3, 4, 5};
    int b[5];
    copy_restrict(5, b, a);
    if (b[2] != 3) return 4;

    // Test 5: volatile
    int arr3[3] = {1, 2, 3};
    read_volatile(3, arr3);

    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_basic", code, &[]), 0);
}

// ============================================================================
// Combined qualifiers and ordering variations
// ============================================================================

#[test]
fn array_param_combined() {
    let code = r#"
// Combined: const + static
int sum_const_static(int arr[const static 5]) {
    return arr[0] + arr[1] + arr[2] + arr[3] + arr[4];
}

// static const order variation
int sum_a(int arr[static const 3]) {
    return arr[0] + arr[1] + arr[2];
}

// const static order variation
int sum_b(int arr[const static 3]) {
    return arr[0] + arr[1] + arr[2];
}

// static with expression that uses multiplication
void process_expr(int arr[static 2 * 5]) {
    for (int i = 0; i < 10; i++) arr[i] = i;
}

int main(void) {
    // Test 1: const + static
    int arr1[] = {10, 20, 30, 40, 50};
    if (sum_const_static(arr1) != 150) return 1;

    // Test 2-3: Order variations
    int arr2[] = {5, 10, 15};
    if (sum_a(arr2) != 30) return 2;
    if (sum_b(arr2) != 30) return 3;

    // Test 4-5: Expression in static
    int arr3[10];
    process_expr(arr3);
    if (arr3[0] != 0) return 4;
    if (arr3[9] != 9) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_combined", code, &[]), 0);
}

// ============================================================================
// VLA star syntax and complex expressions
// ============================================================================

#[test]
fn array_param_vla_and_expressions() {
    let code = r#"
// Prototype with [*] - VLA with unspecified size
void process_star(int n, int arr[*]);

// Definition with actual VLA parameter
void process_star(int n, int arr[n]) {
    for (int i = 0; i < n; i++) arr[i] *= 2;
}

// Ensure [*ptr] is still parsed as an expression, not [*]
int get_expr(int arr[*((int[]){3})]) {
    return arr[0];
}

int main(void) {
    // Test 1-3: VLA star
    int arr1[3] = {1, 2, 3};
    process_star(3, arr1);
    if (arr1[0] != 2) return 1;
    if (arr1[1] != 4) return 2;
    if (arr1[2] != 6) return 3;

    // Test 4: Pointer expression
    int arr2[5] = {42, 1, 2, 3, 4};
    if (get_expr(arr2) != 42) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_vla", code, &[]), 0);
}
