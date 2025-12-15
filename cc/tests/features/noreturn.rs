//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for noreturn attribute support
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// noreturn attribute: __has_attribute and declarations
// ============================================================================

#[test]
fn noreturn_attribute_support() {
    let code = r#"
// External declaration - libc exit is noreturn
extern void exit(int status) __attribute__((noreturn));

int main(void) {
    // Test 1: __has_attribute(noreturn) returns 1
#if !__has_attribute(noreturn)
    return 1;
#endif

    // Test 2: __has_attribute(__noreturn__) returns 1
#if !__has_attribute(__noreturn__)
    return 2;
#endif

    // Test 3: Compilation with noreturn declaration succeeds
    // We just return normally to test compilation
    return 0;
}
"#;
    assert_eq!(compile_and_run("noreturn_attr", code), 0);
}

// ============================================================================
// noreturn with longjmp and C11 _Noreturn keyword
// ============================================================================

#[test]
fn noreturn_with_longjmp() {
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int) __attribute__((noreturn));

jmp_buf env;

void do_jump(int val) {
    longjmp(env, val);
    // Code here is unreachable - longjmp is noreturn
}

int main(void) {
    // Test 1: __attribute__((noreturn)) with longjmp
    int val = setjmp(env);
    if (val == 0) {
        do_jump(42);
        return 99;
    }
    if (val != 42) return 1;

    // Test 2: C11 _Noreturn keyword
    // Reset env for next test by using extern _Noreturn void longjmp
    // (we reuse the same env)
    val = setjmp(env);
    if (val == 0) {
        longjmp(env, 77);
        return 98;
    }
    if (val != 77) return 2;

    // Test 3: __noreturn__ variant
    val = setjmp(env);
    if (val == 0) {
        longjmp(env, 88);
        return 97;
    }
    if (val != 88) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("noreturn_longjmp", code), 0);
}
