//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for __builtin_types_compatible_p builtin
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// __builtin_types_compatible_p: Comprehensive tests
// ============================================================================

#[test]
fn types_compatible_comprehensive() {
    let code = r#"
struct Point {
    int x;
    int y;
};

struct Size {
    int width;
    int height;
};

int main(void) {
    // Test 1-3: Basic types compatibility
    if (__builtin_types_compatible_p(int, int) != 1) return 1;
    if (__builtin_types_compatible_p(int, long) != 0) return 2;
    if (__builtin_types_compatible_p(char, int) != 0) return 3;

    // Test 4-6: Qualifiers are ignored
    if (__builtin_types_compatible_p(const int, int) != 1) return 4;
    if (__builtin_types_compatible_p(volatile int, int) != 1) return 5;
    if (__builtin_types_compatible_p(const volatile int, int) != 1) return 6;

    // Test 7: Signedness matters
    if (__builtin_types_compatible_p(unsigned int, int) != 0) return 7;

    // Test 8-9: Pointer types
    if (__builtin_types_compatible_p(int*, int*) != 1) return 8;
    if (__builtin_types_compatible_p(int*, char*) != 0) return 9;

    // Test 10-12: Other basic types
    if (__builtin_types_compatible_p(long long, long long) != 1) return 10;
    if (__builtin_types_compatible_p(unsigned long, unsigned long) != 1) return 11;
    if (__builtin_types_compatible_p(float, double) != 0) return 12;

    // Test 13-14: void* compatibility
    if (__builtin_types_compatible_p(void*, void*) != 1) return 13;
    if (__builtin_types_compatible_p(void*, int*) != 0) return 14;

    // Test 15-16: Short and conditional usage
    if (__builtin_types_compatible_p(short, short) != 1) return 15;
    int result;
    if (__builtin_types_compatible_p(int, int)) {
        result = 1;
    } else {
        result = 0;
    }
    if (result != 1) return 16;

    // Test 17-18: Pointer to pointer
    if (__builtin_types_compatible_p(int**, int**) != 1) return 17;
    if (__builtin_types_compatible_p(unsigned int*, int*) != 0) return 18;

    // Test 19-22: Struct types
    if (__builtin_types_compatible_p(struct Point, struct Point) != 1) return 19;
    if (__builtin_types_compatible_p(struct Point, struct Size) != 0) return 20;
    if (__builtin_types_compatible_p(struct Point*, struct Point*) != 1) return 21;
    if (__builtin_types_compatible_p(const struct Point, struct Point) != 1) return 22;

    // Test 23-27: Array types
    if (__builtin_types_compatible_p(int[10], int[10]) != 1) return 23;
    if (__builtin_types_compatible_p(int[10], int[20]) != 0) return 24;
    if (__builtin_types_compatible_p(int[10], char[10]) != 0) return 25;
    if (__builtin_types_compatible_p(int[10], int) != 0) return 26;
    if (__builtin_types_compatible_p(int*[5], int*[5]) != 1) return 27;

    return 0;
}
"#;
    assert_eq!(compile_and_run("types_compatible", code, &[]), 0);
}
