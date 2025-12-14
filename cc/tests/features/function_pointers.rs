//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for function pointer (indirect call) support
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Function Pointer Tests
// ============================================================================

#[test]
fn basic_function_pointer_call() {
    let code = r#"
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }

int main(void) {
    int (*fp)(int, int);

    fp = add;
    if (fp(3, 2) != 5) return 1;

    fp = sub;
    if (fp(3, 2) != 1) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("basic_fp_call", code), 0);
}

#[test]
fn explicit_dereference_call() {
    let code = r#"
int add(int a, int b) { return a + b; }

int main(void) {
    int (*fp)(int, int) = add;

    // Explicit dereference form
    if ((*fp)(10, 20) != 30) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("explicit_deref_fp", code), 0);
}

#[test]
fn function_pointer_from_function_name() {
    let code = r#"
int multiply(int a, int b) { return a * b; }

int main(void) {
    int (*fp)(int, int);

    // Taking address of function
    fp = &multiply;
    if (fp(6, 7) != 42) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_from_func_name", code), 0);
}

// ============================================================================
// Struct Member Function Pointers
// ============================================================================

#[test]
fn struct_member_function_pointer() {
    let code = r#"
int increment(int x) { return x + 1; }
int decrement(int x) { return x - 1; }

struct Operations {
    int (*op)(int);
    int value;
};

int main(void) {
    struct Operations ops;

    ops.op = increment;
    ops.value = 10;
    if (ops.op(ops.value) != 11) return 1;

    ops.op = decrement;
    if (ops.op(ops.value) != 9) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("struct_member_fp", code), 0);
}

#[test]
fn pointer_to_struct_function_pointer() {
    let code = r#"
int double_it(int x) { return x * 2; }

struct Handler {
    int (*callback)(int);
};

int main(void) {
    struct Handler h;
    struct Handler *ptr = &h;

    ptr->callback = double_it;
    if (ptr->callback(5) != 10) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_struct_fp", code), 0);
}

// ============================================================================
// Pointer to Function Pointer
// ============================================================================

#[test]
fn pointer_to_function_pointer() {
    let code = r#"
int square(int x) { return x * x; }

int main(void) {
    int (*fp)(int) = square;
    int (**fpp)(int) = &fp;

    if ((*fpp)(4) != 16) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_to_fp", code), 0);
}

// ============================================================================
// Array of Function Pointers
// ============================================================================

#[test]
fn array_of_function_pointers() {
    let code = r#"
int f0(void) { return 0; }
int f1(void) { return 1; }
int f2(void) { return 2; }

int main(void) {
    int (*arr[3])(void);
    arr[0] = f0;
    arr[1] = f1;
    arr[2] = f2;

    for (int i = 0; i < 3; i++) {
        if (arr[i]() != i) return i + 1;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_array", code), 0);
}

// ============================================================================
// Function Returning Function Pointer
// ============================================================================

// TODO: Fix parser to support complex function returning function pointer syntax:
// int (*get_op(int which))(int, int)
#[test]
#[ignore]
fn function_returning_function_pointer() {
    let code = r#"
int add(int a, int b) { return a + b; }
int mul(int a, int b) { return a * b; }

int (*get_op(int which))(int, int) {
    if (which == 0) return add;
    return mul;
}

int main(void) {
    // Call the returned function pointer immediately
    if (get_op(0)(2, 3) != 5) return 1;
    if (get_op(1)(2, 3) != 6) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_return", code), 0);
}

// ============================================================================
// Function Pointers as Arguments
// ============================================================================

#[test]
fn function_pointer_as_argument() {
    let code = r#"
int apply(int (*f)(int), int x) {
    return f(x);
}

int triple(int x) { return x * 3; }

int main(void) {
    if (apply(triple, 4) != 12) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_as_arg", code), 0);
}

// TODO: Fix register allocator bug where caller-saved registers holding
// function parameters are not preserved across calls in loops.
#[test]
#[ignore]
fn function_pointer_callback() {
    let code = r#"
void for_each(int *arr, int len, void (*callback)(int *)) {
    for (int i = 0; i < len; i++) {
        callback(&arr[i]);
    }
}

void double_val(int *p) {
    *p = *p * 2;
}

int main(void) {
    int arr[3] = {1, 2, 3};
    for_each(arr, 3, double_val);

    if (arr[0] != 2) return 1;
    if (arr[1] != 4) return 2;
    if (arr[2] != 6) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_callback", code), 0);
}

// ============================================================================
// Void Function Pointers
// ============================================================================

#[test]
fn void_function_pointer() {
    let code = r#"
void action1(int *p) { *p = 10; }
void action2(int *p) { *p = 20; }

int main(void) {
    void (*fp)(int *);
    int x = 0;

    fp = action1;
    fp(&x);
    if (x != 10) return 1;

    fp = action2;
    fp(&x);
    if (x != 20) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_func_ptr", code), 0);
}

// ============================================================================
// Typedef Function Pointers
// ============================================================================

#[test]
fn typedef_function_pointer() {
    let code = r#"
typedef int (*binop_t)(int, int);

int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }

int compute(binop_t op, int a, int b) {
    return op(a, b);
}

int main(void) {
    binop_t op = add;
    if (compute(op, 10, 5) != 15) return 1;

    op = sub;
    if (compute(op, 10, 5) != 5) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("typedef_fp", code), 0);
}

// ============================================================================
// Complex Expression Function Pointers
// ============================================================================

#[test]
fn conditional_function_pointer() {
    let code = r#"
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }

int main(void) {
    int (*fp)(int, int);

    int use_add = 1;
    fp = use_add ? add : sub;
    if (fp(10, 3) != 13) return 1;

    use_add = 0;
    fp = use_add ? add : sub;
    if (fp(10, 3) != 7) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("cond_fp", code), 0);
}

// ============================================================================
// Nested Function Pointer Calls
// ============================================================================

#[test]
fn nested_function_pointer_call() {
    let code = r#"
int identity(int x) { return x; }
int double_it(int x) { return x * 2; }

int apply_twice(int (*f)(int), int x) {
    return f(f(x));
}

int main(void) {
    if (apply_twice(identity, 5) != 5) return 1;
    if (apply_twice(double_it, 3) != 12) return 2;  // 3 -> 6 -> 12

    return 0;
}
"#;
    assert_eq!(compile_and_run("nested_fp_call", code), 0);
}
