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
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Function Pointer Operations
// ============================================================================

#[test]
fn function_pointer_basics() {
    let code = r#"
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int multiply(int a, int b) { return a * b; }

int main(void) {
    int (*fp)(int, int);

    // Test 1-2: Basic function pointer call
    fp = add;
    if (fp(3, 2) != 5) return 1;
    fp = sub;
    if (fp(3, 2) != 1) return 2;

    // Test 3: Explicit dereference form
    fp = add;
    if ((*fp)(10, 20) != 30) return 3;

    // Test 4: Taking address of function
    fp = &multiply;
    if (fp(6, 7) != 42) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_basics", code, &[]), 0);
}

// ============================================================================
// Function Pointers in Structs
// ============================================================================

#[test]
fn function_pointer_in_structs() {
    let code = r#"
int increment(int x) { return x + 1; }
int decrement(int x) { return x - 1; }
int double_it(int x) { return x * 2; }

struct Operations {
    int (*op)(int);
    int value;
};

struct Handler {
    int (*callback)(int);
};

int main(void) {
    // Test 1-2: Struct member function pointer
    struct Operations ops;
    ops.op = increment;
    ops.value = 10;
    if (ops.op(ops.value) != 11) return 1;
    ops.op = decrement;
    if (ops.op(ops.value) != 9) return 2;

    // Test 3: Pointer to struct with function pointer
    struct Handler h;
    struct Handler *ptr = &h;
    ptr->callback = double_it;
    if (ptr->callback(5) != 10) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_structs", code, &[]), 0);
}

// ============================================================================
// Advanced Function Pointer Patterns
// ============================================================================

#[test]
fn function_pointer_advanced() {
    let code = r#"
int square(int x) { return x * x; }
int f0(void) { return 0; }
int f1(void) { return 1; }
int f2(void) { return 2; }
int add(int a, int b) { return a + b; }
int mul(int a, int b) { return a * b; }

int (*get_op(int which))(int, int) {
    if (which == 0) return add;
    return mul;
}

int main(void) {
    // Test 1: Pointer to function pointer
    int (*fp)(int) = square;
    int (**fpp)(int) = &fp;
    if ((*fpp)(4) != 16) return 1;

    // Test 2-4: Array of function pointers
    int (*arr[3])(void);
    arr[0] = f0;
    arr[1] = f1;
    arr[2] = f2;
    for (int i = 0; i < 3; i++) {
        if (arr[i]() != i) return 2 + i;  // returns 2, 3, or 4
    }

    // Test 5-6: Function returning function pointer
    if (get_op(0)(2, 3) != 5) return 5;
    if (get_op(1)(2, 3) != 6) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_advanced", code, &[]), 0);
}

// ============================================================================
// Function Pointer as Parameter (Callbacks)
// ============================================================================

#[test]
fn function_pointer_as_parameter() {
    let code = r#"
int apply(int (*f)(int), int x) {
    return f(x);
}

int triple(int x) { return x * 3; }

void for_each(int *arr, int len, void (*callback)(int *)) {
    for (int i = 0; i < len; i++) {
        callback(&arr[i]);
    }
}

void double_val(int *p) {
    *p = *p * 2;
}

int main(void) {
    // Test 1: Simple function pointer as argument
    if (apply(triple, 4) != 12) return 1;

    // Test 2-4: Callback pattern
    int arr[3] = {1, 2, 3};
    for_each(arr, 3, double_val);
    if (arr[0] != 2) return 2;
    if (arr[1] != 4) return 3;
    if (arr[2] != 6) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_param", code, &[]), 0);
}

// ============================================================================
// Function Pointer Variations (void, typedef, conditional, nested)
// ============================================================================

#[test]
fn function_pointer_variations() {
    let code = r#"
void action1(int *p) { *p = 10; }
void action2(int *p) { *p = 20; }

typedef int (*binop_t)(int, int);
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }

int compute(binop_t op, int a, int b) {
    return op(a, b);
}

int identity(int x) { return x; }
int double_it(int x) { return x * 2; }

int apply_twice(int (*f)(int), int x) {
    return f(f(x));
}

int main(void) {
    // Test 1-2: Void function pointer
    void (*vfp)(int *);
    int x = 0;
    vfp = action1;
    vfp(&x);
    if (x != 10) return 1;
    vfp = action2;
    vfp(&x);
    if (x != 20) return 2;

    // Test 3-4: Typedef function pointer
    binop_t op = add;
    if (compute(op, 10, 5) != 15) return 3;
    op = sub;
    if (compute(op, 10, 5) != 5) return 4;

    // Test 5-6: Conditional function pointer
    int (*cfp)(int, int);
    int use_add = 1;
    cfp = use_add ? add : sub;
    if (cfp(10, 3) != 13) return 5;
    use_add = 0;
    cfp = use_add ? add : sub;
    if (cfp(10, 3) != 7) return 6;

    // Test 7-8: Nested function pointer call
    if (apply_twice(identity, 5) != 5) return 7;
    if (apply_twice(double_it, 3) != 12) return 8;  // 3 -> 6 -> 12

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_variations", code, &[]), 0);
}
