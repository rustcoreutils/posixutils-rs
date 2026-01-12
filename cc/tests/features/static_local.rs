//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for static local variables
//
// Static local variables have static storage duration but no linkage.
// They retain their value across function calls and are initialized once.
//

use crate::common::compile_and_run;

/// Test: pre-increment with static local variable
#[test]
fn static_local_preincrement() {
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
    assert_eq!(compile_and_run("static_preincr", code, &[]), 0);
}

/// Test: pre-decrement with static local variable
#[test]
fn static_local_predecrement() {
    let code = r#"
int decrement(void) {
    static int counter = 10;
    return --counter;
}

int main(void) {
    if (decrement() != 9) return 1;
    if (decrement() != 8) return 2;
    if (decrement() != 7) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_predec", code, &[]), 0);
}

/// Test: post-increment with static local variable
#[test]
fn static_local_postincrement() {
    let code = r#"
int increment(void) {
    static int counter = 0;
    return counter++;
}

int main(void) {
    if (increment() != 0) return 1;
    if (increment() != 1) return 2;
    if (increment() != 2) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_postinc", code, &[]), 0);
}

/// Test: post-decrement with static local variable
#[test]
fn static_local_postdecrement() {
    let code = r#"
int decrement(void) {
    static int counter = 10;
    return counter--;
}

int main(void) {
    if (decrement() != 10) return 1;
    if (decrement() != 9) return 2;
    if (decrement() != 8) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_postdec", code, &[]), 0);
}

/// Test: compound assignment with static local variable
#[test]
fn static_local_compound_assign() {
    let code = r#"
int accumulate(int val) {
    static int sum = 0;
    sum += val;
    return sum;
}

int main(void) {
    if (accumulate(5) != 5) return 1;
    if (accumulate(3) != 8) return 2;
    if (accumulate(2) != 10) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_compound", code, &[]), 0);
}

/// Test: multiple static locals in same function
#[test]
fn static_local_multiple() {
    let code = r#"
int get_pair(int which) {
    static int a = 10;
    static int b = 20;
    if (which == 0) {
        return a++;
    } else {
        return b++;
    }
}

int main(void) {
    if (get_pair(0) != 10) return 1;
    if (get_pair(1) != 20) return 2;
    if (get_pair(0) != 11) return 3;
    if (get_pair(1) != 21) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_multiple", code, &[]), 0);
}

/// Test: static local with different types
#[test]
fn static_local_types() {
    let code = r#"
long increment_long(void) {
    static long counter = 0;
    return ++counter;
}

short increment_short(void) {
    static short counter = 0;
    return ++counter;
}

int main(void) {
    if (increment_long() != 1) return 1;
    if (increment_long() != 2) return 2;
    if (increment_short() != 1) return 3;
    if (increment_short() != 2) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_types", code, &[]), 0);
}

/// Test: static local in multiple functions
#[test]
fn static_local_separate_functions() {
    let code = r#"
int func_a(void) {
    static int counter = 0;
    return ++counter;
}

int func_b(void) {
    static int counter = 100;
    return ++counter;
}

int main(void) {
    if (func_a() != 1) return 1;
    if (func_b() != 101) return 2;
    if (func_a() != 2) return 3;
    if (func_b() != 102) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_separate", code, &[]), 0);
}

/// Test: static local pointer increment
#[test]
fn static_local_pointer() {
    let code = r#"
int arr[5] = {10, 20, 30, 40, 50};

int next(void) {
    static int *ptr = arr;
    return *ptr++;
}

int main(void) {
    if (next() != 10) return 1;
    if (next() != 20) return 2;
    if (next() != 30) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_pointer", code, &[]), 0);
}
