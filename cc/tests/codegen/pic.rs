//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Position-Independent Code (PIC) Mega-Test
//
// Consolidates: ALL pic tests
//
// PIC is required for shared libraries and enables code to be loaded at any
// address without relocation. Key aspects:
// - External symbols are accessed via the Global Offset Table (GOT)
// - Function calls may use the Procedure Linkage Table (PLT)
// - All data references use RIP-relative addressing on x86-64
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: PIC code generation
// ============================================================================

#[test]
fn codegen_pic_mega() {
    let code = r#"
extern unsigned long strlen(const char *s);

int global_value = 42;
int array[5] = {1, 2, 3, 4, 5};
int global_state = 0;

int increment(void) {
    static int counter = 0;
    return ++counter;
}

int add(int a, int b) { return a + b; }
int mul(int a, int b) { return a * b; }

int apply(int (*fn)(int, int), int x, int y) {
    return fn(x, y);
}

int sum_array(void) {
    int total = 0;
    for (int i = 0; i < 5; i++) {
        total += array[i];
    }
    return total;
}

const char *get_hello(void) {
    return "hello";
}

const char *get_world(void) {
    return "world";
}

typedef int (*operation)(int, int);

struct Calculator {
    operation add;
    operation sub;
};

int do_add(int a, int b) { return a + b; }
int do_sub(int a, int b) { return a - b; }

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

void set_state(int val) { global_state = val; }
int get_state(void) { return global_state; }

int indirect_call(void (*setter)(int), int (*getter)(void), int val) {
    setter(val);
    return getter();
}

int main(void) {
    // ========== BASIC PIC (returns 1-9) ==========

    // Global variable access
    if (global_value != 42) return 1;
    global_value = 100;
    if (global_value != 100) return 2;
    global_value = 42;  // Reset

    // ========== STATIC LOCAL (returns 10-19) ==========
    {
        if (increment() != 1) return 10;
        if (increment() != 2) return 11;
        if (increment() != 3) return 12;
    }

    // ========== FUNCTION POINTERS (returns 20-29) ==========
    {
        if (apply(add, 3, 4) != 7) return 20;
        if (apply(mul, 3, 4) != 12) return 21;
    }

    // ========== GLOBAL ARRAY (returns 30-39) ==========
    {
        if (sum_array() != 15) return 30;
        array[0] = 10;
        if (sum_array() != 24) return 31;
        array[0] = 1;  // Reset
    }

    // ========== STRING LITERALS (returns 40-49) ==========
    {
        const char *h = get_hello();
        const char *w = get_world();
        if (h[0] != 'h') return 40;
        if (w[0] != 'w') return 41;
    }

    // ========== STRUCT WITH FUNC PTR (returns 50-59) ==========
    {
        struct Calculator calc = { do_add, do_sub };
        if (calc.add(10, 5) != 15) return 50;
        if (calc.sub(10, 5) != 5) return 51;
    }

    // ========== RECURSION (returns 60-69) ==========
    {
        if (factorial(5) != 120) return 60;
        if (factorial(6) != 720) return 61;
    }

    // ========== EXTERNAL SYMBOL (returns 70-79) ==========
    {
        if (strlen("hello") != 5) return 70;
        if (strlen("") != 0) return 71;
        if (strlen("test string") != 11) return 72;
    }

    // ========== COMPLEX CALLS (returns 80-89) ==========
    {
        set_state(42);
        if (get_state() != 42) return 80;

        if (indirect_call(set_state, get_state, 100) != 100) return 81;
        if (global_state != 100) return 82;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_pic_mega", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test both -fPIC (uppercase) and -fpic (lowercase) flags
#[test]
fn codegen_pic_flags() {
    let code = r#"
int main(void) {
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_uppercase", code, &["-fPIC".to_string()]),
        0
    );
    assert_eq!(
        compile_and_run("pic_lowercase", code, &["-fpic".to_string()]),
        0
    );
}
