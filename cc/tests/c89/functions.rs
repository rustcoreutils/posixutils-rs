//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89 Functions Mega-Test
//
// Consolidates: function_pointers, calls, recursion tests from features/
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: C89 functions (pointers, calls, recursion)
// ============================================================================

#[test]
fn c89_functions_mega() {
    let code = r#"
// Basic functions
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int mul(int a, int b) { return a * b; }
int div_fn(int a, int b) { return a / b; }

// Function returning function pointer
typedef int (*binop)(int, int);
binop get_op(char c) {
    switch (c) {
        case '+': return add;
        case '-': return sub;
        case '*': return mul;
        case '/': return div_fn;
        default: return 0;
    }
}

// Recursive functions
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int fibonacci(int n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

// Mutual recursion
int is_even(int n);
int is_odd(int n) {
    if (n == 0) return 0;
    return is_even(n - 1);
}
int is_even(int n) {
    if (n == 0) return 1;
    return is_odd(n - 1);
}

// Variadic count helper
int count_until_zero(int first, ...) {
    // Simple variadic: just return first for testing compilation
    return first;
}

// Function taking function pointer
int apply(int (*f)(int, int), int a, int b) {
    return f(a, b);
}

// Array of function pointers
binop ops[4];

// Struct with function pointer member
struct Calculator {
    binop op;
    int a;
    int b;
};

int calc(struct Calculator *c) {
    return c->op(c->a, c->b);
}

int main(void) {
    // ========== BASIC FUNCTION CALLS (returns 1-19) ==========
    {
        // Direct calls
        if (add(3, 4) != 7) return 1;
        if (sub(10, 3) != 7) return 2;
        if (mul(6, 7) != 42) return 3;
        if (div_fn(84, 2) != 42) return 4;

        // Nested calls
        if (add(mul(2, 3), 1) != 7) return 5;
        if (mul(add(1, 2), add(3, 4)) != 21) return 6;  // 3 * 7

        // Call result as condition
        if (!add(1, 0)) return 7;
        if (sub(5, 5)) return 8;  // 0 is false
    }

    // ========== FUNCTION POINTERS (returns 20-49) ==========
    {
        // Basic function pointer
        int (*fp)(int, int) = add;
        if (fp(10, 20) != 30) return 20;

        // Reassign function pointer
        fp = sub;
        if (fp(30, 10) != 20) return 21;

        fp = mul;
        if (fp(6, 7) != 42) return 22;

        // Function pointer comparison
        fp = add;
        if (fp != add) return 23;
        if (fp == sub) return 24;

        // Typedef function pointer
        binop op = mul;
        if (op(3, 14) != 42) return 25;

        // Function returning function pointer
        op = get_op('+');
        if (op(20, 22) != 42) return 26;

        op = get_op('*');
        if (op(6, 7) != 42) return 27;

        op = get_op('-');
        if (op(50, 8) != 42) return 28;

        // Function taking function pointer
        if (apply(add, 10, 32) != 42) return 29;
        if (apply(mul, 6, 7) != 42) return 30;

        // Array of function pointers
        ops[0] = add;
        ops[1] = sub;
        ops[2] = mul;
        ops[3] = div_fn;
        if (ops[0](10, 20) != 30) return 31;
        if (ops[2](6, 7) != 42) return 32;

        // Loop over function pointer array
        int results[4];
        results[0] = ops[0](10, 5);  // add: 15
        results[1] = ops[1](10, 5);  // sub: 5
        results[2] = ops[2](10, 5);  // mul: 50
        results[3] = ops[3](10, 5);  // div: 2
        if (results[0] != 15) return 33;
        if (results[1] != 5) return 34;
        if (results[2] != 50) return 35;
        if (results[3] != 2) return 36;

        // Struct with function pointer
        struct Calculator c;
        c.op = add;
        c.a = 10;
        c.b = 32;
        if (calc(&c) != 42) return 37;

        c.op = mul;
        c.a = 6;
        c.b = 7;
        if (calc(&c) != 42) return 38;

        // Calling through pointer to function pointer
        binop *pp = &op;
        *pp = add;
        if ((*pp)(20, 22) != 42) return 39;
    }

    // ========== RECURSION (returns 50-69) ==========
    {
        // Factorial
        if (factorial(0) != 1) return 50;
        if (factorial(1) != 1) return 51;
        if (factorial(5) != 120) return 52;
        if (factorial(6) != 720) return 53;

        // Fibonacci
        if (fibonacci(0) != 0) return 54;
        if (fibonacci(1) != 1) return 55;
        if (fibonacci(5) != 5) return 56;
        if (fibonacci(10) != 55) return 57;

        // Mutual recursion
        if (is_even(0) != 1) return 58;
        if (is_even(4) != 1) return 59;
        if (is_even(5) != 0) return 60;
        if (is_odd(0) != 0) return 61;
        if (is_odd(3) != 1) return 62;
        if (is_odd(4) != 0) return 63;
    }

    // ========== FUNCTION DECLARATIONS/PROTOTYPES (returns 70-79) ==========
    {
        // Forward declaration already tested via is_even/is_odd

        // Function with no parameters
        int no_params(void);  // Would need implementation

        // The compilation success of this file proves prototypes work
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_functions_mega", code, &[]), 0);
}

/// Test that return statements properly convert expression types to function return type
/// This tests a bug where `return -1` in a `long long` function returned 0xFFFFFFFF instead of -1
#[test]
fn c89_functions_return_type_conversion() {
    let code = r#"
typedef long long Py_ssize_t;
#define DKIX_EMPTY (-1)

// Function returns 64-bit but expression is 32-bit int
static Py_ssize_t return_minus_one(void) {
    return -1;
}

// Function returns 64-bit via macro (still 32-bit int expression)
static Py_ssize_t return_dkix_empty(void) {
    return DKIX_EMPTY;
}

// More complex case - return from inside loop
static Py_ssize_t return_from_loop(int condition) {
    Py_ssize_t ix;
    for (;;) {
        ix = -1;
        if (ix < 0) {
            return DKIX_EMPTY;
        }
    }
    return -100;
}

// Return smaller types that need sign extension
static long long return_signed_char(void) {
    return (signed char)-1;  // Should be -1, not 255
}

static long long return_short(void) {
    return (short)-1;  // Should be -1, not 65535
}

static long long return_int(void) {
    return (int)-1;  // Should be -1, not 0xFFFFFFFF
}

// Test with unsigned promotion
static unsigned long long return_uint(void) {
    return (unsigned int)0xFFFFFFFF;  // Should stay 0xFFFFFFFF
}

int main(void) {
    // Test direct -1 return
    Py_ssize_t r1 = return_minus_one();
    if (r1 != -1) return 1;
    // Check upper bits are set (sign extended)
    if ((unsigned long long)r1 != 0xFFFFFFFFFFFFFFFFULL) return 2;

    // Test macro return
    Py_ssize_t r2 = return_dkix_empty();
    if (r2 != -1) return 3;
    if ((unsigned long long)r2 != 0xFFFFFFFFFFFFFFFFULL) return 4;

    // Test return from loop
    Py_ssize_t r3 = return_from_loop(1);
    if (r3 != -1) return 5;
    if ((unsigned long long)r3 != 0xFFFFFFFFFFFFFFFFULL) return 6;

    // Test smaller signed types
    if (return_signed_char() != -1) return 7;
    if (return_short() != -1) return 8;
    if (return_int() != -1) return 9;

    // Test unsigned - should zero-extend, not sign-extend
    if (return_uint() != 0xFFFFFFFFULL) return 10;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c89_functions_return_type_conversion", code, &[]),
        0
    );
}
