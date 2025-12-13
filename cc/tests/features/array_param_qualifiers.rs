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

use crate::common::compile_and_run;

// ============================================================================
// C99 6.7.5.3: Array parameter qualifiers
// ============================================================================

#[test]
fn array_param_static() {
    let code = r#"
// static in array parameter indicates minimum guaranteed elements
void fill(int n, int arr[static 10]) {
    for (int i = 0; i < n && i < 10; i++) {
        arr[i] = i;
    }
}

int main(void) {
    int arr[10];
    fill(10, arr);
    if (arr[0] != 0) return 1;
    if (arr[9] != 9) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_static", code), 0);
}

#[test]
fn array_param_const() {
    let code = r#"
// const qualifier applies to the resulting pointer
int sum(int n, int arr[const]) {
    int s = 0;
    for (int i = 0; i < n; i++) s += arr[i];
    return s;
}

int main(void) {
    int arr[] = {1, 2, 3, 4, 5};
    if (sum(5, arr) != 15) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_const", code), 0);
}

#[test]
fn array_param_restrict() {
    let code = r#"
// restrict qualifier for non-overlapping pointers
void copy(int n, int dest[restrict], int src[restrict]) {
    for (int i = 0; i < n; i++) dest[i] = src[i];
}

int main(void) {
    int a[5] = {1, 2, 3, 4, 5};
    int b[5];
    copy(5, b, a);
    if (b[2] != 3) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_restrict", code), 0);
}

#[test]
fn array_param_volatile() {
    let code = r#"
// volatile qualifier
void read_volatile(int n, int arr[volatile]) {
    int sum = 0;
    for (int i = 0; i < n; i++) sum += arr[i];
}

int main(void) {
    int arr[3] = {1, 2, 3};
    read_volatile(3, arr);
    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_volatile", code), 0);
}

#[test]
fn array_param_combined() {
    let code = r#"
// Combined: const + static
int sum(int arr[const static 5]) {
    return arr[0] + arr[1] + arr[2] + arr[3] + arr[4];
}

int main(void) {
    int arr[] = {10, 20, 30, 40, 50};
    if (sum(arr) != 150) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_combined", code), 0);
}

#[test]
fn array_param_static_const_order() {
    let code = r#"
// static const order variation
int sum_a(int arr[static const 3]) {
    return arr[0] + arr[1] + arr[2];
}

// const static order variation
int sum_b(int arr[const static 3]) {
    return arr[0] + arr[1] + arr[2];
}

int main(void) {
    int arr[] = {5, 10, 15};
    if (sum_a(arr) != 30) return 1;
    if (sum_b(arr) != 30) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_static_const_order", code), 0);
}

#[test]
fn array_param_vla_star() {
    let code = r#"
// Prototype with [*] - VLA with unspecified size
void process(int n, int arr[*]);

// Definition with actual VLA parameter
void process(int n, int arr[n]) {
    for (int i = 0; i < n; i++) arr[i] *= 2;
}

int main(void) {
    int arr[3] = {1, 2, 3};
    process(3, arr);
    if (arr[0] != 2) return 1;
    if (arr[1] != 4) return 2;
    if (arr[2] != 6) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_vla_star", code), 0);
}

#[test]
fn array_param_static_with_expression() {
    let code = r#"
// static with expression that uses multiplication
void process(int arr[static 2 * 5]) {
    for (int i = 0; i < 10; i++) arr[i] = i;
}

int main(void) {
    int arr[10];
    process(arr);
    if (arr[0] != 0) return 1;
    if (arr[9] != 9) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("array_param_static_with_expression", code),
        0
    );
}

#[test]
fn array_param_pointer_expression() {
    let code = r#"
// Ensure [*ptr] is still parsed as an expression, not [*]
int get(int arr[*((int[]){3})]) {
    return arr[0];
}

int main(void) {
    int arr[5] = {42, 1, 2, 3, 4};
    if (get(arr) != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("array_param_pointer_expression", code), 0);
}
