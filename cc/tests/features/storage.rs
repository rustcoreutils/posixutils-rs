//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for storage class specifiers (static, extern)
//
// C99 defines storage class specifiers that affect:
// - Storage duration (static vs automatic)
// - Linkage (external vs internal vs none)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Static Local Variables: Block scope with static duration
// ============================================================================

#[test]
fn static_local_variables() {
    let code = r#"
// Static local variable persists across function calls
int counter(void) {
    static int count = 0;
    count = count + 1;
    return count;
}

// Static local with non-zero initializer
int get_base(void) {
    static int base = 100;
    base = base + 1;
    return base;
}

// Each function has its own static local
int func_a(void) {
    static int x = 10;
    x = x + 1;
    return x;
}

int func_b(void) {
    static int x = 20;  // Different static, same local name
    x = x + 2;
    return x;
}

// Uninitialized static local is zero-initialized
int get_count(void) {
    static int count;  // Should be 0
    count = count + 5;
    return count;
}

int main(void) {
    // Test 1-3: Basic persistence
    int a = counter();  // Should be 1
    int b = counter();  // Should be 2
    int c = counter();  // Should be 3
    if (a + b + c != 6) return 1;

    // Test 4-5: Non-zero initializer
    int d = get_base();  // 101
    int e = get_base();  // 102
    if (d != 101) return 2;
    if (e != 102) return 3;

    // Test 6-9: Multiple functions, same local name
    int a1 = func_a();  // 11
    int b1 = func_b();  // 22
    int a2 = func_a();  // 12
    int b2 = func_b();  // 24
    if (a1 != 11) return 4;
    if (b1 != 22) return 5;
    if (a2 != 12) return 6;
    if (b2 != 24) return 7;

    // Test 10-11: Uninitialized (zero-initialized)
    int f = get_count();  // 5
    int g = get_count();  // 10
    if (f != 5) return 8;
    if (g != 10) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("static_local_vars", code), 0);
}

// ============================================================================
// Static Functions: Internal linkage
// ============================================================================

#[test]
fn static_functions() {
    let code = r#"
// Static function has internal linkage (not visible to linker)
static int helper(int x) {
    return x * 2;
}

// Multiple static functions
static int double_it(int x) {
    return x * 2;
}

static int triple_it(int x) {
    return x * 3;
}

// Static function calling another static function
static int add_one(int x) {
    return x + 1;
}

static int add_two(int x) {
    return add_one(add_one(x));
}

int main(void) {
    // Test 1: Basic static function
    if (helper(21) != 42) return 1;

    // Test 2-3: Multiple static functions
    int a = double_it(10);  // 20
    int b = triple_it(10);  // 30
    if (a + b != 50) return 2;

    // Test 4: Static function calls static function
    if (add_two(40) != 42) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("static_functions", code), 0);
}

// ============================================================================
// Static Globals and Extern Declarations
// ============================================================================

#[test]
fn static_globals_and_extern() {
    let code = r#"
// Static global has internal linkage
static int global_val = 42;

// Static global can be modified
static int counter = 0;

void increment(void) {
    counter = counter + 1;
}

// Global variable (external linkage by default)
int shared_value = 100;

// Extern declaration references the same variable
extern int shared_value;

int main(void) {
    // Test 1: Static global basic
    if (global_val != 42) return 1;

    // Test 2: Static global modified
    increment();
    increment();
    increment();
    if (counter != 3) return 2;

    // Test 3: Extern declaration
    if (shared_value != 100) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("static_global_extern", code), 0);
}
