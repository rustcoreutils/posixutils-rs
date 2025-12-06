//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for __builtin_types_compatible_p builtin
//

use crate::common::compile_and_run;

#[test]
fn types_compatible_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Same basic types are compatible
    if (__builtin_types_compatible_p(int, int) != 1) return 1;

    // Test 2: Different basic types are not compatible
    if (__builtin_types_compatible_p(int, long) != 0) return 2;

    // Test 3: char and int are not compatible
    if (__builtin_types_compatible_p(char, int) != 0) return 3;

    // Test 4: const int and int ARE compatible (qualifiers ignored)
    if (__builtin_types_compatible_p(const int, int) != 1) return 4;

    // Test 5: volatile int and int ARE compatible (qualifiers ignored)
    if (__builtin_types_compatible_p(volatile int, int) != 1) return 5;

    // Test 6: const volatile int and int ARE compatible
    if (__builtin_types_compatible_p(const volatile int, int) != 1) return 6;

    // Test 7: unsigned int and int are NOT compatible
    if (__builtin_types_compatible_p(unsigned int, int) != 0) return 7;

    // Test 8: Pointer types - same base type
    if (__builtin_types_compatible_p(int*, int*) != 1) return 8;

    // Test 9: Pointer types - different base types
    if (__builtin_types_compatible_p(int*, char*) != 0) return 9;

    // Test 10: long long and long long
    if (__builtin_types_compatible_p(long long, long long) != 1) return 10;

    // Test 11: unsigned long and unsigned long
    if (__builtin_types_compatible_p(unsigned long, unsigned long) != 1) return 11;

    // Test 12: float and double
    if (__builtin_types_compatible_p(float, double) != 0) return 12;

    // Test 13: void* and void*
    if (__builtin_types_compatible_p(void*, void*) != 1) return 13;

    // Test 14: void* and int* are NOT compatible
    if (__builtin_types_compatible_p(void*, int*) != 0) return 14;

    // Test 15: short and short
    if (__builtin_types_compatible_p(short, short) != 1) return 15;

    // Test 16: Use in conditional expression (compile-time constant)
    int result;
    if (__builtin_types_compatible_p(int, int)) {
        result = 1;
    } else {
        result = 0;
    }
    if (result != 1) return 16;

    // Test 17: double pointer types
    if (__builtin_types_compatible_p(int**, int**) != 1) return 17;

    // Test 18: mixed signedness pointers
    if (__builtin_types_compatible_p(unsigned int*, int*) != 0) return 18;

    return 0;
}
"#;
    assert_eq!(compile_and_run("types_compatible_comprehensive", code), 0);
}

#[test]
fn types_compatible_structs() {
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
    // Test 1: Same struct type is compatible
    if (__builtin_types_compatible_p(struct Point, struct Point) != 1) return 1;

    // Test 2: Different struct types are NOT compatible (even with same layout)
    if (__builtin_types_compatible_p(struct Point, struct Size) != 0) return 2;

    // Test 3: Pointer to same struct
    if (__builtin_types_compatible_p(struct Point*, struct Point*) != 1) return 3;

    // Test 4: const struct and struct (qualifiers ignored)
    if (__builtin_types_compatible_p(const struct Point, struct Point) != 1) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("types_compatible_structs", code), 0);
}

#[test]
fn types_compatible_arrays() {
    let code = r#"
int main(void) {
    // Test 1: Same size arrays are compatible
    if (__builtin_types_compatible_p(int[10], int[10]) != 1) return 1;

    // Test 2: Different size arrays are NOT compatible
    if (__builtin_types_compatible_p(int[10], int[20]) != 0) return 2;

    // Test 3: Different element types are NOT compatible
    if (__builtin_types_compatible_p(int[10], char[10]) != 0) return 3;

    // Test 4: Array and non-array are NOT compatible
    if (__builtin_types_compatible_p(int[10], int) != 0) return 4;

    // Test 5: Pointer to array types
    if (__builtin_types_compatible_p(int*[5], int*[5]) != 1) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("types_compatible_arrays", code), 0);
}
