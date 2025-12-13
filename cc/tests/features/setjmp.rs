//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for setjmp/longjmp support
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// setjmp/longjmp: Basic tests
// ============================================================================

#[test]
fn setjmp_basic() {
    // Basic setjmp/longjmp test - jump back with value 42
    let code = r#"
// Define jmp_buf as opaque array (platform-specific size)
// macOS: 37 ints, Linux: ~50 ints - we use a larger buffer to be safe
typedef int jmp_buf[64];

// External libc functions
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);

jmp_buf env;

int main(void) {
    int val = setjmp(env);
    if (val == 0) {
        // First return - call longjmp
        longjmp(env, 42);
        // Should never reach here
        return 1;
    }
    // Second return - val should be 42
    return val == 42 ? 0 : 2;
}
"#;
    assert_eq!(compile_and_run("setjmp_basic", code), 0);
}

#[test]
fn setjmp_multiple_returns() {
    // Test setjmp returning multiple times with different values
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);

jmp_buf env;
int count = 0;

int main(void) {
    int val = setjmp(env);
    count++;

    if (count < 4) {
        // Jump back with count * 10
        longjmp(env, count * 10);
        return 99;  // Should never reach here
    }

    // count should be 4, val should be 30 (from count=3 jump)
    if (count != 4) return 1;
    if (val != 30) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("setjmp_multiple_returns", code), 0);
}

#[test]
fn setjmp_zero_becomes_one() {
    // Per C standard, if longjmp is called with val=0, setjmp returns 1
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);

jmp_buf env;

int main(void) {
    int val = setjmp(env);
    if (val == 0) {
        // First return - call longjmp with 0
        longjmp(env, 0);
        return 1;
    }
    // Per C standard, longjmp(env, 0) causes setjmp to return 1
    return val == 1 ? 0 : 2;
}
"#;
    assert_eq!(compile_and_run("setjmp_zero_becomes_one", code), 0);
}

#[test]
fn setjmp_from_function() {
    // Test longjmp from a called function
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);

jmp_buf env;

void do_longjmp(int val) {
    longjmp(env, val);
}

int main(void) {
    int val = setjmp(env);
    if (val == 0) {
        do_longjmp(77);
        return 1;
    }
    return val == 77 ? 0 : 2;
}
"#;
    assert_eq!(compile_and_run("setjmp_from_function", code), 0);
}

#[test]
fn setjmp_nested_calls() {
    // Test longjmp from deeply nested function calls
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);

jmp_buf env;

void level3(int val) {
    longjmp(env, val);
}

void level2(int val) {
    level3(val);
}

void level1(int val) {
    level2(val);
}

int main(void) {
    int val = setjmp(env);
    if (val == 0) {
        level1(123);
        return 1;
    }
    return val == 123 ? 0 : 2;
}
"#;
    assert_eq!(compile_and_run("setjmp_nested_calls", code), 0);
}

#[test]
fn setjmp_volatile_preserved() {
    // Test that volatile variables are preserved across longjmp
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);

jmp_buf env;

int main(void) {
    volatile int x = 5;

    int val = setjmp(env);
    if (val == 0) {
        x = 10;
        longjmp(env, 1);
        return 1;
    }

    // Per C standard, volatile variable x should have value 10
    return x == 10 ? 0 : 2;
}
"#;
    assert_eq!(compile_and_run("setjmp_volatile_preserved", code), 0);
}

#[test]
fn setjmp_underscore_variant() {
    // Test _setjmp variant (no signal mask save/restore)
    let code = r#"
typedef int jmp_buf[64];
extern int _setjmp(jmp_buf);
extern void _longjmp(jmp_buf, int);

jmp_buf env;

int main(void) {
    int val = _setjmp(env);
    if (val == 0) {
        _longjmp(env, 55);
        return 1;
    }
    return val == 55 ? 0 : 2;
}
"#;
    assert_eq!(compile_and_run("setjmp_underscore_variant", code), 0);
}
