//
// Copyright (c) 2024 Jeff Garzik
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

use crate::common::compile_and_run;

// ============================================================================
// Void Functions - Basic
// ============================================================================

#[test]
fn void_function_basic() {
    let code = r#"
void do_nothing(void) {
    // Empty function body
}

int main(void) {
    do_nothing();
    return 0;
}
"#;
    assert_eq!(compile_and_run("void_func_basic", code), 0);
}

#[test]
fn void_function_with_statements() {
    let code = r#"
void set_value(int *dest, int val) {
    int local = val;
    *dest = local;
}

void modify_ptr(int *p, int val) {
    *p = val;
}

int main(void) {
    int x = 0;
    modify_ptr(&x, 42);
    if (x != 42) return 1;

    modify_ptr(&x, 100);
    if (x != 100) return 2;

    set_value(&x, 200);
    if (x != 200) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_func_stmts", code), 0);
}

#[test]
fn void_function_explicit_return() {
    let code = r#"
void with_return(void) {
    return;
}

void with_early_return(int condition) {
    if (condition) {
        return;
    }
    // Fall through
}

int main(void) {
    with_return();
    with_early_return(1);
    with_early_return(0);
    return 0;
}
"#;
    assert_eq!(compile_and_run("void_func_return", code), 0);
}

#[test]
fn void_function_implicit_return() {
    let code = r#"
void no_return_stmt(void) {
    int x = 1;
    int y = 2;
    // No explicit return - should work for void functions
}

int main(void) {
    no_return_stmt();
    return 0;
}
"#;
    assert_eq!(compile_and_run("void_func_implicit", code), 0);
}

// ============================================================================
// Void Functions - Parameters
// ============================================================================

#[test]
fn void_function_with_params() {
    let code = r#"
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
    take_int(42);
    take_multiple(1, 2, 3);

    int x = 10;
    take_pointer(&x);
    if (x != 11) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_func_params", code), 0);
}

#[test]
fn void_function_void_parameter() {
    let code = r#"
// (void) explicitly means no parameters
void no_params(void) {
}

int main(void) {
    no_params();
    return 0;
}
"#;
    assert_eq!(compile_and_run("void_func_void_param", code), 0);
}

// ============================================================================
// Void Pointer (void*)
// ============================================================================

#[test]
fn void_pointer_basic() {
    let code = r#"
int main(void) {
    int x = 42;
    void *vp;

    // Assign address to void pointer
    vp = &x;

    // Convert back to int pointer
    int *ip = vp;

    if (*ip != 42) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_basic", code), 0);
}

#[test]
fn void_pointer_multiple_types() {
    let code = r#"
int main(void) {
    void *vp;

    // Point to int
    int i = 42;
    vp = &i;
    int *ip = vp;
    if (*ip != 42) return 1;

    // Point to char
    char c = 'A';
    vp = &c;
    char *cp = vp;
    if (*cp != 'A') return 2;

    // Point to long
    long l = 1234567890L;
    vp = &l;
    long *lp = vp;
    if (*lp != 1234567890L) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_types", code), 0);
}

#[test]
fn void_pointer_function_param() {
    let code = r#"
void set_int(void *ptr, int val) {
    int *ip = ptr;
    *ip = val;
}

void set_char(void *ptr, char val) {
    char *cp = ptr;
    *cp = val;
}

int main(void) {
    int x = 0;
    set_int(&x, 42);
    if (x != 42) return 1;

    char c = 0;
    set_char(&c, 'Z');
    if (c != 'Z') return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_param", code), 0);
}

#[test]
fn void_pointer_function_return() {
    let code = r#"
void *get_ptr(int *p) {
    return p;
}

int main(void) {
    int x = 100;
    void *vp = get_ptr(&x);
    int *ip = vp;

    if (*ip != 100) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_return", code), 0);
}

#[test]
fn void_pointer_array() {
    let code = r#"
int main(void) {
    int arr[3];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;

    void *vp = arr;
    int *ip = vp;

    if (ip[0] != 10) return 1;
    if (ip[1] != 20) return 2;
    if (ip[2] != 30) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_array", code), 0);
}

#[test]
fn void_pointer_comparison() {
    let code = r#"
int main(void) {
    int x = 1;
    int y = 2;

    void *vp1 = &x;
    void *vp2 = &x;
    void *vp3 = &y;

    // Same address
    if (vp1 != vp2) return 1;

    // Different address
    if (vp1 == vp3) return 2;

    // Null comparison
    void *null_ptr = 0;
    if (null_ptr != 0) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_compare", code), 0);
}

// ============================================================================
// Cast to Void
// ============================================================================

#[test]
fn void_cast_discard_result() {
    let code = r#"
int get_value(void) {
    return 42;
}

int main(void) {
    // Cast to void discards result
    (void)get_value();

    // Multiple discards
    (void)get_value();
    (void)get_value();

    // Discard variable
    int x = 10;
    (void)x;

    // Discard expression
    (void)(1 + 2 + 3);

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_cast_discard", code), 0);
}

#[test]
fn void_cast_suppress_warning() {
    let code = r#"
// Common pattern: cast to void to suppress unused parameter warning
void unused_param(int x) {
    (void)x;  // Explicitly ignore parameter
}

int main(void) {
    unused_param(42);
    return 0;
}
"#;
    assert_eq!(compile_and_run("void_cast_suppress", code), 0);
}

// ============================================================================
// Void Function Return Void Call
// ============================================================================

#[test]
fn void_return_void_call() {
    let code = r#"
void inner(void) {
    // Do nothing
}

void outer(void) {
    return inner();  // void function returning void call
}

int main(void) {
    outer();
    return 0;
}
"#;
    assert_eq!(compile_and_run("void_return_void", code), 0);
}

#[test]
fn void_chained_calls() {
    let code = r#"
void step1(int *p) {
    *p = *p + 1;
}

void step2(int *p) {
    *p = *p * 2;
}

void step3(int *p) {
    *p = *p - 3;
}

int main(void) {
    int x = 5;

    step1(&x);  // 5 + 1 = 6
    if (x != 6) return 1;

    step2(&x);  // 6 * 2 = 12
    if (x != 12) return 2;

    step3(&x);  // 12 - 3 = 9
    if (x != 9) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_chained", code), 0);
}

// ============================================================================
// Void in Structs
// ============================================================================

#[test]
fn void_pointer_in_struct() {
    let code = r#"
struct container {
    void *data;
    int size;
};

int main(void) {
    int value = 42;
    struct container c;
    c.data = &value;
    c.size = 4;

    int *ip = c.data;
    if (*ip != 42) return 1;
    if (c.size != 4) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_struct", code), 0);
}

#[test]
fn void_pointer_struct_param() {
    let code = r#"
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
    struct wrapper w;
    int x = 100;

    set_data(&w, &x);

    int *result = get_data(&w);
    if (*result != 100) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_struct_param", code), 0);
}

// ============================================================================
// Void Function Pointers
// ============================================================================

// Note: Function pointer declarations like `void (*fp)(int *)` are not yet
// supported by the parser. This test is commented out until that feature
// is implemented.
//
// #[test]
// fn void_function_pointer() {
//     let code = r#"
// void action1(int *p) {
//     *p = 10;
// }
//
// void action2(int *p) {
//     *p = 20;
// }
//
// int main(void) {
//     void (*fp)(int *);
//     int x = 0;
//
//     fp = action1;
//     fp(&x);
//     if (x != 10) return 1;
//
//     fp = action2;
//     fp(&x);
//     if (x != 20) return 2;
//
//     return 0;
// }
// "#;
//     assert_eq!(compile_and_run("void_func_ptr", code), 0);
// }

// ============================================================================
// Void with Control Flow
// ============================================================================

#[test]
fn void_with_conditionals() {
    let code = r#"
void conditional_action(int *p, int condition) {
    if (condition) {
        *p = 1;
        return;
    } else {
        *p = 0;
        return;
    }
}

int main(void) {
    int x;

    conditional_action(&x, 1);
    if (x != 1) return 1;

    conditional_action(&x, 0);
    if (x != 0) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_conditional", code), 0);
}

#[test]
fn void_with_loops() {
    let code = r#"
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

int main(void) {
    int arr[5];
    int sum;

    fill_array(arr, 5, 10);
    sum_array(arr, 5, &sum);

    // 5 * 10 = 50
    if (sum != 50) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_loops", code), 0);
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn void_nested_calls() {
    let code = r#"
void level3(int *p) {
    *p = *p + 1;
}

void level2(int *p) {
    level3(p);
    level3(p);
}

void level1(int *p) {
    level2(p);
    level2(p);
}

int main(void) {
    int x = 0;
    level1(&x);
    // level1 calls level2 twice
    // each level2 calls level3 twice
    // total: 4 increments
    if (x != 4) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_nested", code), 0);
}

#[test]
fn void_recursive() {
    let code = r#"
void countdown(int *p, int n) {
    if (n <= 0) {
        return;
    }
    *p = *p + 1;
    countdown(p, n - 1);
}

int main(void) {
    int count = 0;
    countdown(&count, 5);
    if (count != 5) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_recursive", code), 0);
}
