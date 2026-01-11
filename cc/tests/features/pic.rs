//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for Position-Independent Code (PIC) support
//
// PIC is required for shared libraries and enables code to be loaded at any
// address without relocation. Key aspects:
// - External symbols are accessed via the Global Offset Table (GOT)
// - Function calls may use the Procedure Linkage Table (PLT)
// - All data references use RIP-relative addressing on x86-64
//

use crate::common::compile_and_run;

/// Basic test: compile with -fPIC flag (uppercase)
#[test]
fn compile_with_fpic_uppercase() {
    let code = r#"
int main(void) {
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_uppercase", code, &["-fPIC".to_string()]),
        0
    );
}

/// Basic test: compile with -fpic flag (lowercase)
#[test]
fn compile_with_fpic_lowercase() {
    let code = r#"
int main(void) {
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_lowercase", code, &["-fpic".to_string()]),
        0
    );
}

/// Test: PIC with global variable access
#[test]
fn pic_global_variable() {
    let code = r#"
int global_value = 42;

int main(void) {
    if (global_value != 42) return 1;
    global_value = 100;
    if (global_value != 100) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_global_var", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test: PIC with static local variable
#[test]
fn pic_static_local() {
    let code = r#"
int increment(void) {
    static int counter = 0;
    return ++counter;
}

int main(void) {
    if (increment() != 1) return 1;
    if (increment() != 2) return 2;
    if (increment() != 3) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_static_local", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test: PIC with function pointers
#[test]
fn pic_function_pointer() {
    let code = r#"
int add(int a, int b) { return a + b; }
int mul(int a, int b) { return a * b; }

int apply(int (*fn)(int, int), int x, int y) {
    return fn(x, y);
}

int main(void) {
    if (apply(add, 3, 4) != 7) return 1;
    if (apply(mul, 3, 4) != 12) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_func_ptr", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test: PIC with global array
#[test]
fn pic_global_array() {
    let code = r#"
int array[5] = {1, 2, 3, 4, 5};

int sum(void) {
    int total = 0;
    for (int i = 0; i < 5; i++) {
        total += array[i];
    }
    return total;
}

int main(void) {
    if (sum() != 15) return 1;
    array[0] = 10;
    if (sum() != 24) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_global_array", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test: PIC with string literals
#[test]
fn pic_string_literals() {
    let code = r#"
const char *get_hello(void) {
    return "hello";
}

const char *get_world(void) {
    return "world";
}

int main(void) {
    const char *h = get_hello();
    const char *w = get_world();
    // Check first chars
    if (h[0] != 'h') return 1;
    if (w[0] != 'w') return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_strings", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test: PIC with struct containing function pointers
#[test]
fn pic_struct_with_func_ptr() {
    let code = r#"
typedef int (*operation)(int, int);

struct Calculator {
    operation add;
    operation sub;
};

int do_add(int a, int b) { return a + b; }
int do_sub(int a, int b) { return a - b; }

int main(void) {
    struct Calculator calc = { do_add, do_sub };
    if (calc.add(10, 5) != 15) return 1;
    if (calc.sub(10, 5) != 5) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_struct_func_ptr", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test: PIC with recursion (tests stack handling in PIC)
#[test]
fn pic_recursion() {
    let code = r#"
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main(void) {
    if (factorial(5) != 120) return 1;
    if (factorial(6) != 720) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_recursion", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test: PIC with external libc function (printf/strlen)
/// This tests GOT access for external symbols
#[test]
fn pic_external_symbol() {
    let code = r#"
extern unsigned long strlen(const char *s);

int main(void) {
    if (strlen("hello") != 5) return 1;
    if (strlen("") != 0) return 2;
    if (strlen("test string") != 11) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_external", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test: PIC with multiple compilation units simulation
/// Using different function styles to stress test GOT/PLT
#[test]
fn pic_complex_calls() {
    let code = r#"
int global_state = 0;

void set_state(int val) { global_state = val; }
int get_state(void) { return global_state; }

int indirect_call(void (*setter)(int), int (*getter)(void), int val) {
    setter(val);
    return getter();
}

int main(void) {
    set_state(42);
    if (get_state() != 42) return 1;

    if (indirect_call(set_state, get_state, 100) != 100) return 2;
    if (global_state != 100) return 3;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_complex", code, &["-fPIC".to_string()]),
        0
    );
}
