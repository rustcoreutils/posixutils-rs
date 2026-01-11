//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Integration tests for global variables
//

use crate::common::{cleanup_exe, compile, create_c_file, run};

// ============================================================================
// Basic Global Variable Tests
// ============================================================================

#[test]
fn test_global_int_initialized() {
    let c_file = create_c_file(
        "global_int_init",
        r#"
int global_counter = 42;
int main(void) {
    return global_counter;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

#[test]
fn test_global_int_uninitialized() {
    let c_file = create_c_file(
        "global_int_uninit",
        r#"
int global_uninit;
int main(void) {
    return global_uninit;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // Uninitialized global should be zero
    assert_eq!(exit_code, 0, "expected exit code 0");

    cleanup_exe(&exe);
}

#[test]
fn test_global_static() {
    let c_file = create_c_file(
        "global_static",
        r#"
static int static_var = 10;
int main(void) {
    return static_var;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 10, "expected exit code 10");

    cleanup_exe(&exe);
}

#[test]
fn test_global_static_uninitialized() {
    let c_file = create_c_file(
        "global_static_uninit",
        r#"
static int static_uninit;
int main(void) {
    return static_uninit;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // Static uninitialized should be zero
    assert_eq!(exit_code, 0, "expected exit code 0");

    cleanup_exe(&exe);
}

// ============================================================================
// Multiple Global Variables
// ============================================================================

#[test]
fn test_multiple_globals() {
    let c_file = create_c_file(
        "multiple_globals",
        r#"
int a = 10;
int b = 20;
int c = 12;
int main(void) {
    return a + b + c;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

#[test]
fn test_globals_different_types() {
    let c_file = create_c_file(
        "globals_diff_types",
        r#"
char c = 1;
short s = 2;
int i = 3;
long l = 4;
int main(void) {
    return c + s + i + l;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 10, "expected exit code 10");

    cleanup_exe(&exe);
}

// ============================================================================
// Global Variable Modification
// ============================================================================

#[test]
fn test_global_modification() {
    let c_file = create_c_file(
        "global_modify",
        r#"
int counter = 0;
int main(void) {
    counter = 42;
    return counter;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

#[test]
fn test_global_increment() {
    let c_file = create_c_file(
        "global_increment",
        r#"
int counter = 40;
int main(void) {
    counter = counter + 1;
    counter = counter + 1;
    return counter;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

// ============================================================================
// Float/Double Global Variables
// ============================================================================

#[test]
fn test_global_double() {
    let c_file = create_c_file(
        "global_double",
        r#"
double d = 42.5;
int main(void) {
    return (int)d;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

#[test]
fn test_global_float() {
    let c_file = create_c_file(
        "global_float",
        r#"
float f = 42.9f;
int main(void) {
    return (int)f;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

// ============================================================================
// Pointer Globals
// ============================================================================

#[test]
fn test_global_pointer_null() {
    let c_file = create_c_file(
        "global_ptr_null",
        r#"
int *ptr = 0;
int main(void) {
    return ptr ? 1 : 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 0, "expected exit code 0");

    cleanup_exe(&exe);
}

#[test]
fn test_global_pointer_to_global() {
    let c_file = create_c_file(
        "global_ptr_to_global",
        r#"
int value = 42;
int *ptr;
int main(void) {
    ptr = &value;
    return *ptr;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

// ============================================================================
// Global Array Initializers (C99 Complex Initializers)
// ============================================================================

#[test]
fn test_global_array_init() {
    let c_file = create_c_file(
        "global_array_init",
        r#"
int arr[5] = {10, 20, 3, 4, 5};
int main(void) {
    return arr[0] + arr[1] + arr[2];
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 33, "expected 10+20+3=33");

    cleanup_exe(&exe);
}

#[test]
fn test_global_array_partial_init() {
    let c_file = create_c_file(
        "global_array_partial",
        r#"
int arr[5] = {1, 2, 3};
int main(void) {
    // arr[3] and arr[4] should be 0
    return arr[0] + arr[1] + arr[2] + arr[3] + arr[4];
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 6, "expected 1+2+3+0+0=6");

    cleanup_exe(&exe);
}

#[test]
fn test_global_array_designated() {
    let c_file = create_c_file(
        "global_array_designated",
        r#"
int arr[5] = {[1] = 10, [3] = 30};
int main(void) {
    // arr[0]=0, arr[1]=10, arr[2]=0, arr[3]=30, arr[4]=0
    return arr[0] + arr[1] + arr[2] + arr[3] + arr[4];
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 40, "expected 0+10+0+30+0=40");

    cleanup_exe(&exe);
}

// ============================================================================
// Global Struct Initializers (C99 Complex Initializers)
// ============================================================================

#[test]
fn test_global_struct_init() {
    let c_file = create_c_file(
        "global_struct_init",
        r#"
struct Point {
    int x;
    int y;
};
struct Point p = {10, 32};
int main(void) {
    return p.x + p.y;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected 10+32=42");

    cleanup_exe(&exe);
}

#[test]
fn test_global_struct_designated() {
    let c_file = create_c_file(
        "global_struct_designated",
        r#"
struct Point {
    int x;
    int y;
    int z;
};
struct Point p = {.y = 20, .x = 10, .z = 12};
int main(void) {
    return p.x + p.y + p.z;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected 10+20+12=42");

    cleanup_exe(&exe);
}

#[test]
fn test_global_struct_partial_init() {
    let c_file = create_c_file(
        "global_struct_partial",
        r#"
struct Point {
    int x;
    int y;
    int z;
};
struct Point p = {10, 32};  // z should be 0
int main(void) {
    return p.x + p.y + p.z;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected 10+32+0=42");

    cleanup_exe(&exe);
}

// ============================================================================
// Global String Initializers
// ============================================================================

#[test]
fn test_global_string_array() {
    let c_file = create_c_file(
        "global_string_array",
        r#"
char str[] = "ABC";
int main(void) {
    return str[0] + str[1] + str[2];  // 'A'=65, 'B'=66, 'C'=67 -> 198 % 256 = 198
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // 65 + 66 + 67 = 198
    assert_eq!(exit_code, 198, "expected 65+66+67=198");

    cleanup_exe(&exe);
}

// ============================================================================
// Static Local Array/Struct Initializers
// ============================================================================

#[test]
fn test_static_local_array() {
    let c_file = create_c_file(
        "static_local_array",
        r#"
int main(void) {
    static int arr[3] = {10, 20, 12};
    return arr[0] + arr[1] + arr[2];
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected 10+20+12=42");

    cleanup_exe(&exe);
}

#[test]
fn test_static_local_struct() {
    let c_file = create_c_file(
        "static_local_struct",
        r#"
struct Point {
    int x;
    int y;
};
int main(void) {
    static struct Point p = {10, 32};
    return p.x + p.y;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected 10+32=42");

    cleanup_exe(&exe);
}

// ============================================================================
// Constant Expression Evaluation Tests (C99 6.6)
// ============================================================================

#[test]
fn test_global_const_expr_init() {
    let c_file = create_c_file(
        "global_const_expr_init",
        r#"
int a = 2 + 3;
int b = 10 * 4 + 2;
int c = (1 << 4) | 10;
int d = 100 / 5 % 7;
int e = -10 + 52;
int main(void) {
    if (a != 5) return 1;
    if (b != 42) return 2;
    if (c != 26) return 3;
    if (d != 6) return 4;
    if (e != 42) return 5;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "all constant expression initializations should work"
    );

    cleanup_exe(&exe);
}

#[test]
fn test_array_const_expr_size() {
    let c_file = create_c_file(
        "array_const_expr_size",
        r#"
int arr1[2 + 3];
int arr2[sizeof(int) * 2];
int main(void) {
    int size1 = sizeof(arr1) / sizeof(arr1[0]);
    int size2 = sizeof(arr2) / sizeof(arr2[0]);
    if (size1 != 5) return 1;
    if (size2 != 8) return 2;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "array sizes from constant expressions should work"
    );

    cleanup_exe(&exe);
}

#[test]
fn test_enum_const_expr() {
    let c_file = create_c_file(
        "enum_const_expr",
        r#"
enum { X = 5, Y = X + 3, Z = Y * 2 };
int arr[X + Y];
int val = Z - Y;
int main(void) {
    int arr_size = sizeof(arr) / sizeof(arr[0]);
    if (arr_size != 13) return 1;
    if (val != 8) return 2;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "enum constants in constant expressions should work"
    );

    cleanup_exe(&exe);
}

// ============================================================================
// Array Alignment Tests
// ============================================================================
// Tests for 16-byte alignment of large arrays (>= 16 bytes)
// This matches clang's LargeArrayMinWidth = 128 bits = 16 bytes optimization

#[test]
fn test_global_array_alignment_large() {
    // Arrays >= 16 bytes should get 16-byte alignment
    let c_file = create_c_file(
        "array_align_large",
        r#"
// Define uintptr_t manually to avoid system header dependency
typedef unsigned long uintptr_t;
// 16-byte array - should be 16-byte aligned
char big_array[16];
// 32-byte array - should also be 16-byte aligned
int int_array[8];
int main(void) {
    // Check 16-byte alignment by testing lower 4 bits are zero
    uintptr_t addr1 = (uintptr_t)big_array;
    uintptr_t addr2 = (uintptr_t)int_array;
    if ((addr1 & 0xF) != 0) return 1;  // big_array not 16-byte aligned
    if ((addr2 & 0xF) != 0) return 2;  // int_array not 16-byte aligned
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 0, "large arrays should be 16-byte aligned");

    cleanup_exe(&exe);
}

#[test]
fn test_global_array_alignment_small() {
    // Small arrays (< 16 bytes) should use natural alignment
    // An 8-byte char array should be 1-byte aligned (or maybe 8 due to linking)
    // A 4-int array (16 bytes) should be 16-byte aligned
    // An 8-byte char array is NOT required to be 16-byte aligned
    let c_file = create_c_file(
        "array_align_small",
        r#"
// Define uintptr_t manually to avoid system header dependency
typedef unsigned long uintptr_t;
// 8-byte array - smaller than threshold, natural alignment
char small_array[8];
// 15-byte array - smaller than threshold
char almost_big[15];
// For comparison: 16-byte array should be aligned
char exactly_16[16];
int main(void) {
    // Small arrays don't require 16-byte alignment
    // Just verify they're accessible and work correctly
    small_array[0] = 'a';
    small_array[7] = 'z';
    almost_big[0] = 'b';
    almost_big[14] = 'y';
    exactly_16[0] = 'c';
    exactly_16[15] = 'x';

    // Verify the 16-byte array is 16-byte aligned
    uintptr_t addr = (uintptr_t)exactly_16;
    if ((addr & 0xF) != 0) return 1;

    // Verify data was written correctly
    if (small_array[0] != 'a') return 2;
    if (small_array[7] != 'z') return 3;
    if (almost_big[0] != 'b') return 4;
    if (almost_big[14] != 'y') return 5;
    if (exactly_16[0] != 'c') return 6;
    if (exactly_16[15] != 'x') return 7;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf(), &[]);
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "array alignment and access should work correctly"
    );

    cleanup_exe(&exe);
}
