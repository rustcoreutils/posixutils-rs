//
// Copyright (c) 2024 Jeff Garzik
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
// noreturn attribute tests
// ============================================================================

#[test]
fn noreturn_has_attribute() {
    // Test that __has_attribute(noreturn) returns 1
    let code = r#"
int main(void) {
#if __has_attribute(noreturn)
    return 0;  // noreturn attribute is supported
#else
    return 1;  // noreturn attribute is not supported
#endif
}
"#;
    assert_eq!(compile_and_run("noreturn_has_attribute", code), 0);
}

#[test]
fn noreturn_attribute_declaration() {
    // Test that __attribute__((noreturn)) can be declared
    // (We can't fully test noreturn behavior without actually not returning,
    // so we just test that it compiles and the function can be called)
    let code = r#"
// External declaration - libc exit is noreturn
extern void exit(int status) __attribute__((noreturn));

int main(void) {
    // We'll just return normally to test compilation
    // Calling exit would work but not let us verify return code
    return 0;
}
"#;
    assert_eq!(compile_and_run("noreturn_attribute_declaration", code), 0);
}

#[test]
fn noreturn_with_longjmp() {
    // Test that longjmp (which is noreturn) works correctly
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
    int val = setjmp(env);
    if (val == 0) {
        do_jump(42);
        // Should never reach here
        return 99;
    }
    return val == 42 ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("noreturn_with_longjmp", code), 0);
}

#[test]
fn noreturn_c11_keyword() {
    // Test C11 _Noreturn keyword
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern _Noreturn void longjmp(jmp_buf, int);

jmp_buf env;

int main(void) {
    int val = setjmp(env);
    if (val == 0) {
        longjmp(env, 77);
        return 99;
    }
    return val == 77 ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("noreturn_c11_keyword", code), 0);
}

#[test]
fn noreturn_inline_attribute() {
    // Test __noreturn__ variant (used by some code)
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int) __attribute__((__noreturn__));

jmp_buf env;

int main(void) {
    int val = setjmp(env);
    if (val == 0) {
        longjmp(env, 88);
        return 99;
    }
    return val == 88 ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("noreturn_inline_attribute", code), 0);
}
