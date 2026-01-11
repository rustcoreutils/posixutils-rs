//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 inline function support
//
// C99 6.7.4 defines inline semantics:
// - static inline: internal linkage, no restrictions
// - inline (without extern): inline definition only (internal linkage in our impl)
// - extern inline: provides external definition
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Static inline functions: No restrictions
// ============================================================================

#[test]
fn static_inline_functions() {
    let code = r#"
// Static inline function - basic usage
static inline int add(int a, int b) {
    return a + b;
}

// Static inline can have non-const static locals (no restriction)
static inline int counter(void) {
    static int count = 0;
    count = count + 1;
    return count;
}

// Static inline can reference file-scope statics (no restriction)
static int global_val = 10;

static inline int get_global(void) {
    return global_val;
}

// Static inline with recursion
static inline int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main(void) {
    // Test 1: Basic usage
    if (add(20, 22) != 42) return 1;

    // Test 2: With static local
    int a = counter();  // 1
    int b = counter();  // 2
    int c = counter();  // 3
    if (a + b + c != 6) return 2;

    // Test 3: References file-scope static
    if (get_global() + 32 != 42) return 3;

    // Test 4: Recursion
    if (factorial(5) != 120) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("static_inline", code, &[]), 0);
}

// ============================================================================
// Plain inline and extern inline functions
// ============================================================================

#[test]
fn inline_and_extern_inline() {
    let code = r#"
// Inline function (inline definition only)
inline int double_it(int x) {
    return x * 2;
}

// Inline function with const static local (allowed)
inline int get_magic(void) {
    static const int magic = 42;
    return magic;
}

// Extern inline provides external definition
extern inline int triple_it(int x) {
    return x * 3;
}

int main(void) {
    // Test 1: Basic inline
    if (double_it(21) != 42) return 1;

    // Test 2: With const static local
    if (get_magic() != 42) return 2;

    // Test 3: Extern inline
    if (triple_it(14) != 42) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("inline_extern", code, &[]), 0);
}

// ============================================================================
// Multiple inline functions working together
// ============================================================================

#[test]
fn inline_multiple_functions() {
    let code = r#"
static inline int add(int a, int b) {
    return a + b;
}

static inline int mul(int a, int b) {
    return a * b;
}

static inline int compute(int x) {
    return add(mul(x, 2), x);  // x*2 + x = 3x
}

int helper(int x) {
    return x + 1;
}

static inline int wrapper(int x) {
    return helper(x) * 2;
}

int main(void) {
    // Test 1: Multiple inline functions
    if (compute(14) != 42) return 1;

    // Test 2: Inline calling regular function
    if (wrapper(20) != 42) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("inline_multiple", code, &[]), 0);
}

// ============================================================================
// Inline with various types (pointers, structs)
// ============================================================================

#[test]
fn inline_with_types() {
    let code = r#"
static inline void swap(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

struct Point {
    int x;
    int y;
};

static inline int sum_point(struct Point p) {
    return p.x + p.y;
}

int main(void) {
    // Test 1: With pointers
    int x = 10;
    int y = 32;
    swap(&x, &y);
    if (x + y != 42) return 1;

    // Test 2: With struct
    struct Point p = {10, 32};
    if (sum_point(p) != 42) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("inline_types", code, &[]), 0);
}
