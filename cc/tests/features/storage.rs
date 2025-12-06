//
// Copyright (c) 2024 Jeff Garzik
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

use crate::common::compile_and_run;

// ============================================================================
// Static Local Variables: Block scope with static duration
// ============================================================================

#[test]
fn static_local_basic() {
    let code = r#"
// Static local variable persists across function calls
int counter(void) {
    static int count = 0;
    count = count + 1;
    return count;
}

int main(void) {
    int a = counter();  // Should be 1
    int b = counter();  // Should be 2
    int c = counter();  // Should be 3

    // Return the sum: 1 + 2 + 3 = 6
    return a + b + c;
}
"#;
    assert_eq!(compile_and_run("static_local_basic", code), 6);
}

#[test]
fn static_local_initialized() {
    let code = r#"
// Static local with non-zero initializer
int get_base(void) {
    static int base = 100;
    base = base + 1;
    return base;
}

int main(void) {
    int a = get_base();  // 101
    int b = get_base();  // 102
    if (a != 101) return 1;
    if (b != 102) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_local_initialized", code), 0);
}

#[test]
fn static_local_multiple_functions() {
    let code = r#"
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

int main(void) {
    int a1 = func_a();  // 11
    int b1 = func_b();  // 22
    int a2 = func_a();  // 12
    int b2 = func_b();  // 24

    if (a1 != 11) return 1;
    if (b1 != 22) return 2;
    if (a2 != 12) return 3;
    if (b2 != 24) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_local_multiple_functions", code), 0);
}

#[test]
fn static_local_uninitialized() {
    let code = r#"
// Uninitialized static local is zero-initialized
int get_count(void) {
    static int count;  // Should be 0
    count = count + 5;
    return count;
}

int main(void) {
    int a = get_count();  // 5
    int b = get_count();  // 10
    if (a != 5) return 1;
    if (b != 10) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_local_uninitialized", code), 0);
}

// ============================================================================
// Static Functions: Internal linkage
// ============================================================================

#[test]
fn static_function_basic() {
    let code = r#"
// Static function has internal linkage (not visible to linker)
static int helper(int x) {
    return x * 2;
}

int main(void) {
    return helper(21);  // Should return 42
}
"#;
    assert_eq!(compile_and_run("static_function_basic", code), 42);
}

#[test]
fn static_function_multiple() {
    let code = r#"
// Multiple static functions
static int double_it(int x) {
    return x * 2;
}

static int triple_it(int x) {
    return x * 3;
}

int main(void) {
    int a = double_it(10);  // 20
    int b = triple_it(10);  // 30
    return a + b;  // 50
}
"#;
    assert_eq!(compile_and_run("static_function_multiple", code), 50);
}

#[test]
fn static_function_calls_static() {
    let code = r#"
// Static function calling another static function
static int add_one(int x) {
    return x + 1;
}

static int add_two(int x) {
    return add_one(add_one(x));
}

int main(void) {
    return add_two(40);  // 42
}
"#;
    assert_eq!(compile_and_run("static_function_calls_static", code), 42);
}

// ============================================================================
// Static Global Variables (already tested in globals/mod.rs but verify here)
// ============================================================================

#[test]
fn static_global_basic() {
    let code = r#"
// Static global has internal linkage
static int global_val = 42;

int main(void) {
    return global_val;
}
"#;
    assert_eq!(compile_and_run("static_global_basic", code), 42);
}

#[test]
fn static_global_modified() {
    let code = r#"
// Static global can be modified
static int counter = 0;

void increment(void) {
    counter = counter + 1;
}

int main(void) {
    increment();
    increment();
    increment();
    return counter;  // Should be 3
}
"#;
    assert_eq!(compile_and_run("static_global_modified", code), 3);
}

// ============================================================================
// Extern declarations
// ============================================================================

#[test]
fn extern_declaration_basic() {
    let code = r#"
// Global variable (external linkage by default)
int shared_value = 42;

// Extern declaration references the same variable
extern int shared_value;

int main(void) {
    return shared_value;
}
"#;
    assert_eq!(compile_and_run("extern_declaration_basic", code), 42);
}
