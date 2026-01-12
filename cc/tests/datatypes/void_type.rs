//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 `void` type
//
// C99 void semantics:
// - void is an incomplete type that cannot be completed
// - Used for functions that don't return a value
// - void* is a generic pointer that can point to any object type
// - (void) parameter list indicates a function takes no arguments
// - Cast to (void) discards a value
// - sizeof(void) is undefined behavior (but some compilers allow it as 1)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Void Functions (basic, statements, returns, params)
// ============================================================================

#[test]
fn void_functions() {
    let code = r#"
void do_nothing(void) {
    // Empty function body
}

void set_value(int *dest, int val) {
    int local = val;
    *dest = local;
}

void with_return(void) {
    return;
}

void with_early_return(int condition) {
    if (condition) {
        return;
    }
}

void no_return_stmt(void) {
    int x = 1;
    int y = 2;
    // No explicit return - should work for void functions
}

void take_int(int x) {
    int local = x;
}

void take_multiple(int a, int b, int c) {
    int sum = a + b + c;
}

void take_pointer(int *p) {
    *p = *p + 1;
}

int main(void) {
    // Test 1: Basic void function
    do_nothing();

    // Test 2-4: Void with statements
    int x = 0;
    set_value(&x, 42);
    if (x != 42) return 1;
    set_value(&x, 100);
    if (x != 100) return 2;

    // Test 3: Explicit return
    with_return();
    with_early_return(1);
    with_early_return(0);

    // Test 4: Implicit return (no return statement)
    no_return_stmt();

    // Test 5-6: Various parameter types
    take_int(42);
    take_multiple(1, 2, 3);
    x = 10;
    take_pointer(&x);
    if (x != 11) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_functions", code, &[]), 0);
}

// ============================================================================
// Void Pointer Operations (basic, multiple types, array, comparison)
// ============================================================================

#[test]
fn void_pointer_operations() {
    let code = r#"
int main(void) {
    // Test 1: Basic void pointer
    int x = 42;
    void *vp = &x;
    int *ip = vp;
    if (*ip != 42) return 1;

    // Test 2-4: Multiple types
    char c = 'A';
    vp = &c;
    char *cp = vp;
    if (*cp != 'A') return 2;

    long l = 1234567890L;
    vp = &l;
    long *lp = vp;
    if (*lp != 1234567890L) return 3;

    // Test 5-7: Array with void pointer
    int arr[3];
    arr[0] = 10; arr[1] = 20; arr[2] = 30;
    vp = arr;
    ip = vp;
    if (ip[0] != 10) return 4;
    if (ip[1] != 20) return 5;
    if (ip[2] != 30) return 6;

    // Test 8-10: Pointer comparison
    int y = 2;
    void *vp1 = &x;
    void *vp2 = &x;
    void *vp3 = &y;
    if (vp1 != vp2) return 7;  // Same address
    if (vp1 == vp3) return 8;  // Different address
    void *null_ptr = 0;
    if (null_ptr != 0) return 9;  // Null comparison

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_ops", code, &[]), 0);
}

// ============================================================================
// Void Pointer with Functions (param, return)
// ============================================================================

#[test]
fn void_pointer_functions() {
    let code = r#"
void set_int(void *ptr, int val) {
    int *ip = ptr;
    *ip = val;
}

void set_char(void *ptr, char val) {
    char *cp = ptr;
    *cp = val;
}

void *get_ptr(int *p) {
    return p;
}

int main(void) {
    // Test 1-2: Void pointer as function parameter
    int x = 0;
    set_int(&x, 42);
    if (x != 42) return 1;

    char c = 0;
    set_char(&c, 'Z');
    if (c != 'Z') return 2;

    // Test 3: Void pointer as return type
    x = 100;
    void *vp = get_ptr(&x);
    int *ip = vp;
    if (*ip != 100) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_funcs", code, &[]), 0);
}

// ============================================================================
// Void Cast (discard result, suppress warning)
// ============================================================================

#[test]
fn void_cast() {
    let code = r#"
int get_value(void) {
    return 42;
}

void unused_param(int x) {
    (void)x;  // Explicitly ignore parameter
}

int main(void) {
    // Test 1-3: Cast to void discards result
    (void)get_value();
    (void)get_value();
    (void)get_value();

    // Test 4: Discard variable
    int x = 10;
    (void)x;

    // Test 5: Discard expression
    (void)(1 + 2 + 3);

    // Test 6: Suppress unused parameter warning
    unused_param(42);

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_cast", code, &[]), 0);
}

// ============================================================================
// Void with Structs
// ============================================================================

#[test]
fn void_pointer_structs() {
    let code = r#"
struct container {
    void *data;
    int size;
};

struct wrapper {
    void *ptr;
};

void set_data(struct wrapper *w, void *data) {
    w->ptr = data;
}

void *get_data(struct wrapper *w) {
    return w->ptr;
}

int main(void) {
    // Test 1-2: Void pointer in struct
    int value = 42;
    struct container c;
    c.data = &value;
    c.size = 4;
    int *ip = c.data;
    if (*ip != 42) return 1;
    if (c.size != 4) return 2;

    // Test 3: Void pointer struct with functions
    struct wrapper w;
    int x = 100;
    set_data(&w, &x);
    int *result = get_data(&w);
    if (*result != 100) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_structs", code, &[]), 0);
}

// ============================================================================
// Void Function Chaining and Control Flow
// ============================================================================

#[test]
fn void_control_flow() {
    let code = r#"
void inner(void) {
    // Do nothing
}

void outer(void) {
    return inner();  // void function returning void call
}

void step1(int *p) { *p = *p + 1; }
void step2(int *p) { *p = *p * 2; }
void step3(int *p) { *p = *p - 3; }

void conditional_action(int *p, int condition) {
    if (condition) {
        *p = 1;
        return;
    } else {
        *p = 0;
        return;
    }
}

void fill_array(int *arr, int size, int value) {
    int i;
    for (i = 0; i < size; i = i + 1) {
        arr[i] = value;
    }
}

void sum_array(int *arr, int size, int *result) {
    int i;
    *result = 0;
    for (i = 0; i < size; i = i + 1) {
        *result = *result + arr[i];
    }
}

void level3(int *p) { *p = *p + 1; }
void level2(int *p) { level3(p); level3(p); }
void level1(int *p) { level2(p); level2(p); }

void countdown(int *p, int n) {
    if (n <= 0) return;
    *p = *p + 1;
    countdown(p, n - 1);
}

int main(void) {
    // Test 1: Return void call
    outer();

    // Test 2-4: Chained calls
    int x = 5;
    step1(&x);  // 5 + 1 = 6
    if (x != 6) return 1;
    step2(&x);  // 6 * 2 = 12
    if (x != 12) return 2;
    step3(&x);  // 12 - 3 = 9
    if (x != 9) return 3;

    // Test 5-6: Conditionals
    conditional_action(&x, 1);
    if (x != 1) return 4;
    conditional_action(&x, 0);
    if (x != 0) return 5;

    // Test 7: Loops
    int arr[5];
    int sum;
    fill_array(arr, 5, 10);
    sum_array(arr, 5, &sum);
    if (sum != 50) return 6;  // 5 * 10 = 50

    // Test 8: Nested calls
    x = 0;
    level1(&x);
    if (x != 4) return 7;  // 4 increments total

    // Test 9: Recursive
    int count = 0;
    countdown(&count, 5);
    if (count != 5) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_control_flow", code, &[]), 0);
}
