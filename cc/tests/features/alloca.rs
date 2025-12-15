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
// __builtin_alloca: Basic allocation, multiple, and alignment
// ============================================================================

#[test]
fn alloca_basic_and_alignment() {
    let code = r#"
int main(void) {
    // Test 1-3: Basic allocation and write/read
    char *buf = __builtin_alloca(64);
    buf[0] = 'A';
    buf[1] = 'B';
    buf[2] = 'C';
    buf[3] = '\0';
    if (buf[0] != 'A') return 1;
    if (buf[1] != 'B') return 2;
    if (buf[2] != 'C') return 3;

    // Test 4-11: Multiple allocations don't interfere
    int *arr1 = __builtin_alloca(sizeof(int) * 4);
    int *arr2 = __builtin_alloca(sizeof(int) * 4);
    arr1[0] = 100; arr1[1] = 200; arr1[2] = 300; arr1[3] = 400;
    arr2[0] = 1000; arr2[1] = 2000; arr2[2] = 3000; arr2[3] = 4000;
    if (arr1[0] != 100) return 4;
    if (arr1[1] != 200) return 5;
    if (arr1[2] != 300) return 6;
    if (arr1[3] != 400) return 7;
    if (arr2[0] != 1000) return 8;
    if (arr2[1] != 2000) return 9;
    if (arr2[2] != 3000) return 10;
    if (arr2[3] != 4000) return 11;

    // Test 12-16: Alignment (16-byte)
    void *p1 = __builtin_alloca(1);
    void *p2 = __builtin_alloca(7);
    void *p3 = __builtin_alloca(16);
    void *p4 = __builtin_alloca(17);
    void *p5 = __builtin_alloca(100);
    if (((long)p1 & 0xF) != 0) return 12;
    if (((long)p2 & 0xF) != 0) return 13;
    if (((long)p3 & 0xF) != 0) return 14;
    if (((long)p4 & 0xF) != 0) return 15;
    if (((long)p5 & 0xF) != 0) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("alloca_basic", code), 0);
}

// ============================================================================
// __builtin_alloca: Computed size and in loops
// ============================================================================

#[test]
fn alloca_computed_and_loop() {
    let code = r#"
int test_alloca(int n) {
    int *arr = __builtin_alloca(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        arr[i] = i * 10;
    }
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

int main(void) {
    // Test 1: Computed size n=5: sum = 0+10+20+30+40 = 100
    if (test_alloca(5) != 100) return 1;

    // Test 2: Computed size n=10: sum = 0+10+...+90 = 450
    if (test_alloca(10) != 450) return 2;

    // Test 3-4: Allocation in loop
    int total = 0;
    for (int i = 0; i < 5; i++) {
        int *p = __builtin_alloca(sizeof(int) * 4);
        p[0] = i;
        p[1] = i * 2;
        p[2] = i * 3;
        p[3] = i * 4;
        total += p[0] + p[1] + p[2] + p[3];
    }
    // Sum: for i=0: 0, i=1: 10, i=2: 20, i=3: 30, i=4: 40 = 100
    if (total != 100) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("alloca_computed", code), 0);
}
