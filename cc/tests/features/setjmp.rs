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
// setjmp/longjmp: Basic operations and multiple returns
// ============================================================================

#[test]
fn setjmp_basic_and_multiple() {
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);

jmp_buf env;
int count = 0;

int main(void) {
    // Test 1: Basic setjmp/longjmp
    int val = setjmp(env);
    if (val == 0) {
        longjmp(env, 42);
        return 1;  // Should never reach here
    }
    if (val != 42) return 2;

    // Reset for next test
    count = 0;

    // Test 2: Multiple returns with different values
    val = setjmp(env);
    count++;
    if (count < 4) {
        longjmp(env, count * 10);
        return 3;
    }
    if (count != 4) return 4;
    if (val != 30) return 5;  // From count=3 jump

    // Test 3: Per C standard, longjmp(env, 0) causes setjmp to return 1
    val = setjmp(env);
    if (val == 0) {
        longjmp(env, 0);
        return 6;
    }
    if (val != 1) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("setjmp_basic", code), 0);
}

// ============================================================================
// setjmp/longjmp: From functions and nested calls
// ============================================================================

#[test]
fn setjmp_from_functions() {
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);

jmp_buf env;

void do_longjmp(int val) {
    longjmp(env, val);
}

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
    // Test 1: longjmp from called function
    int val = setjmp(env);
    if (val == 0) {
        do_longjmp(77);
        return 1;
    }
    if (val != 77) return 2;

    // Test 2: longjmp from deeply nested calls
    val = setjmp(env);
    if (val == 0) {
        level1(123);
        return 3;
    }
    if (val != 123) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("setjmp_functions", code), 0);
}

// ============================================================================
// setjmp/longjmp: volatile, _setjmp variants
// ============================================================================

#[test]
fn setjmp_variants() {
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);
extern int _setjmp(jmp_buf);
extern void _longjmp(jmp_buf, int);

jmp_buf env;

int main(void) {
    // Test 1: volatile variables are preserved across longjmp
    volatile int x = 5;
    int val = setjmp(env);
    if (val == 0) {
        x = 10;
        longjmp(env, 1);
        return 1;
    }
    if (x != 10) return 2;

    // Test 2: _setjmp variant (no signal mask save/restore)
    val = _setjmp(env);
    if (val == 0) {
        _longjmp(env, 55);
        return 3;
    }
    if (val != 55) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("setjmp_variants", code), 0);
}
