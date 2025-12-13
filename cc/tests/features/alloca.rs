//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for __builtin_alloca (dynamic stack allocation)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// __builtin_alloca: Basic tests
// ============================================================================

#[test]
fn alloca_basic() {
    let code = r#"
int main(void) {
    // Test 1: Basic allocation and write/read
    char *buf = __builtin_alloca(64);
    buf[0] = 'A';
    buf[1] = 'B';
    buf[2] = 'C';
    buf[3] = '\0';
    if (buf[0] != 'A') return 1;
    if (buf[1] != 'B') return 2;
    if (buf[2] != 'C') return 3;

    // Test 2: Multiple allocations
    int *arr1 = __builtin_alloca(sizeof(int) * 4);
    int *arr2 = __builtin_alloca(sizeof(int) * 4);

    // Write to first array
    arr1[0] = 100;
    arr1[1] = 200;
    arr1[2] = 300;
    arr1[3] = 400;

    // Write to second array
    arr2[0] = 1000;
    arr2[1] = 2000;
    arr2[2] = 3000;
    arr2[3] = 4000;

    // Verify first array wasn't corrupted
    if (arr1[0] != 100) return 4;
    if (arr1[1] != 200) return 5;
    if (arr1[2] != 300) return 6;
    if (arr1[3] != 400) return 7;

    // Verify second array
    if (arr2[0] != 1000) return 8;
    if (arr2[1] != 2000) return 9;
    if (arr2[2] != 3000) return 10;
    if (arr2[3] != 4000) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("alloca_basic", code), 0);
}

#[test]
fn alloca_computed_size() {
    let code = r#"
int test_alloca(int n) {
    // Allocate n integers on the stack
    int *arr = __builtin_alloca(n * sizeof(int));

    // Initialize
    for (int i = 0; i < n; i++) {
        arr[i] = i * 10;
    }

    // Sum and verify
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }

    return sum;
}

int main(void) {
    // Test with size 5: sum = 0 + 10 + 20 + 30 + 40 = 100
    int result = test_alloca(5);
    if (result != 100) return 1;

    // Test with size 10: sum = 0 + 10 + 20 + ... + 90 = 450
    result = test_alloca(10);
    if (result != 450) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("alloca_computed_size", code), 0);
}

#[test]
fn alloca_alignment() {
    let code = r#"
int main(void) {
    // Allocate various sizes and verify alignment
    // All pointers should be 16-byte aligned

    void *p1 = __builtin_alloca(1);
    void *p2 = __builtin_alloca(7);
    void *p3 = __builtin_alloca(16);
    void *p4 = __builtin_alloca(17);
    void *p5 = __builtin_alloca(100);

    // Check 16-byte alignment (pointer & 0xF should be 0)
    if (((long)p1 & 0xF) != 0) return 1;
    if (((long)p2 & 0xF) != 0) return 2;
    if (((long)p3 & 0xF) != 0) return 3;
    if (((long)p4 & 0xF) != 0) return 4;
    if (((long)p5 & 0xF) != 0) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("alloca_alignment", code), 0);
}

#[test]
fn alloca_in_loop() {
    let code = r#"
int main(void) {
    int total = 0;

    // Allocate in a loop - memory should be reclaimed on function return
    for (int i = 0; i < 5; i++) {
        int *p = __builtin_alloca(sizeof(int) * 4);
        p[0] = i;
        p[1] = i * 2;
        p[2] = i * 3;
        p[3] = i * 4;
        total += p[0] + p[1] + p[2] + p[3];
    }

    // Sum: for i=0: 0, i=1: 10, i=2: 20, i=3: 30, i=4: 40 = 100
    if (total != 100) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("alloca_in_loop", code), 0);
}
