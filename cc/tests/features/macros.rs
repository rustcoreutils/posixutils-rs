//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for preprocessor macro expansion
//

use crate::common::compile_and_run;

/// Test nested macro expansion at line start
/// This was a bug where macros at line start would break nested expansion
/// because the newline flag was incorrectly propagated to expanded tokens
#[test]
fn nested_macro_at_line_start() {
    let code = r#"
#define STR(x) #x
#define WRAP(y) STR(y)

int main(void) {
    // WRAP at start of line expands to STR(hello) which should stringify to "hello"
    const char *s =
WRAP(hello);
    if (s[0] != 'h') return 1;
    if (s[4] != 'o') return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("nested_macro_at_line_start", code), 0);
}

/// Test identity macro nesting at line start
#[test]
fn nested_identity_macro_at_line_start() {
    let code = r#"
#define ID(x) x
#define WRAP(y) ID(y)

int main(void) {
    int result =
WRAP(42);
    if (result != 42) return 1;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("nested_identity_macro_at_line_start", code),
        0
    );
}

/// Test multiple levels of nested macros at line start
#[test]
fn deeply_nested_macros_at_line_start() {
    let code = r#"
#define ID1(x) x
#define ID2(x) ID1(x)
#define ID3(x) ID2(x)

int main(void) {
    int value =
ID3(100);
    if (value != 100) return 1;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("deeply_nested_macros_at_line_start", code),
        0
    );
}

/// Test function-like macro with multiple arguments at line start
#[test]
fn multi_arg_macro_at_line_start() {
    let code = r#"
#define ADD(a, b) ((a) + (b))
#define CALL(f, x, y) f(x, y)

int main(void) {
    int sum =
CALL(ADD, 10, 20);
    if (sum != 30) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("multi_arg_macro_at_line_start", code), 0);
}

/// Test stringification in nested macros
#[test]
fn stringify_nested_macro() {
    let code = r#"
#define STR(x) #x
#define XSTR(x) STR(x)
#define VALUE 123

int main(void) {
    // XSTR(VALUE) -> STR(123) -> "123"
    const char *s = XSTR(VALUE);
    if (s[0] != '1') return 1;
    if (s[1] != '2') return 2;
    if (s[2] != '3') return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("stringify_nested_macro", code), 0);
}

/// Test token pasting in macros
#[test]
fn token_paste_basic() {
    let code = r#"
#define PASTE(a, b) a ## b

int main(void) {
    int var123 = 42;
    int result = PASTE(var, 123);
    if (result != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("token_paste_basic", code), 0);
}

/// Test token pasting with nested macros
#[test]
fn token_paste_nested() {
    let code = r#"
#define PASTE(a, b) a ## b
#define MAKE_VAR(n) PASTE(var_, n)

int main(void) {
    int var_100 = 55;
    int result = MAKE_VAR(100);
    if (result != 55) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("token_paste_nested", code), 0);
}
