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
// Non-static inline functions have restrictions:
// - Cannot define non-const function-local static variables
// - Cannot refer to file-scope static variables
//

use crate::common::compile_and_run;

// ============================================================================
// Static inline functions: No restrictions
// ============================================================================

#[test]
fn static_inline_basic() {
    let code = r#"
// Static inline function - basic usage
static inline int add(int a, int b) {
    return a + b;
}

int main(void) {
    return add(20, 22);  // Should return 42
}
"#;
    assert_eq!(compile_and_run("static_inline_basic", code), 42);
}

#[test]
fn static_inline_with_static_local() {
    let code = r#"
// Static inline can have non-const static locals (no restriction)
static inline int counter(void) {
    static int count = 0;
    count = count + 1;
    return count;
}

int main(void) {
    int a = counter();  // 1
    int b = counter();  // 2
    int c = counter();  // 3
    return a + b + c;   // 6
}
"#;
    assert_eq!(compile_and_run("static_inline_with_static_local", code), 6);
}

#[test]
fn static_inline_references_file_scope_static() {
    let code = r#"
// Static inline can reference file-scope statics (no restriction)
static int global_val = 10;

static inline int get_global(void) {
    return global_val;
}

int main(void) {
    return get_global() + 32;  // 10 + 32 = 42
}
"#;
    assert_eq!(
        compile_and_run("static_inline_references_file_scope_static", code),
        42
    );
}

#[test]
fn static_inline_recursive() {
    let code = r#"
// Static inline with recursion
static inline int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main(void) {
    return factorial(5);  // 120
}
"#;
    assert_eq!(compile_and_run("static_inline_recursive", code), 120);
}

// ============================================================================
// Inline functions (without static or extern)
// ============================================================================

#[test]
fn inline_basic() {
    let code = r#"
// Inline function (inline definition only)
// In our implementation, this has internal linkage
inline int double_it(int x) {
    return x * 2;
}

int main(void) {
    return double_it(21);  // 42
}
"#;
    assert_eq!(compile_and_run("inline_basic", code), 42);
}

#[test]
fn inline_with_const_static_local() {
    let code = r#"
// Inline function with const static local (allowed)
inline int get_magic(void) {
    static const int magic = 42;
    return magic;
}

int main(void) {
    return get_magic();
}
"#;
    assert_eq!(compile_and_run("inline_with_const_static_local", code), 42);
}

// ============================================================================
// Extern inline functions
// ============================================================================

#[test]
fn extern_inline_basic() {
    let code = r#"
// Extern inline provides external definition
extern inline int triple_it(int x) {
    return x * 3;
}

int main(void) {
    return triple_it(14);  // 42
}
"#;
    assert_eq!(compile_and_run("extern_inline_basic", code), 42);
}

// ============================================================================
// Multiple inline functions
// ============================================================================

#[test]
fn multiple_inline_functions() {
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

int main(void) {
    return compute(14);  // 42
}
"#;
    assert_eq!(compile_and_run("multiple_inline_functions", code), 42);
}

#[test]
fn inline_calling_regular_function() {
    let code = r#"
int helper(int x) {
    return x + 1;
}

static inline int wrapper(int x) {
    return helper(x) * 2;
}

int main(void) {
    return wrapper(20);  // (20 + 1) * 2 = 42
}
"#;
    assert_eq!(compile_and_run("inline_calling_regular_function", code), 42);
}

// ============================================================================
// Inline with various types
// ============================================================================

#[test]
fn inline_with_pointers() {
    let code = r#"
static inline void swap(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

int main(void) {
    int x = 10;
    int y = 32;
    swap(&x, &y);
    return x + y;  // Still 42, but swapped
}
"#;
    assert_eq!(compile_and_run("inline_with_pointers", code), 42);
}

#[test]
fn inline_with_struct() {
    let code = r#"
struct Point {
    int x;
    int y;
};

static inline int sum_point(struct Point p) {
    return p.x + p.y;
}

int main(void) {
    struct Point p = {10, 32};
    return sum_point(p);  // 42
}
"#;
    assert_eq!(compile_and_run("inline_with_struct", code), 42);
}
