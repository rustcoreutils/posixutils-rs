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
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Nested macro expansion (at line start, identity, deep nesting, multi-arg)
// ============================================================================

#[test]
fn nested_macro_expansion() {
    let code = r#"
#define STR(x) #x
#define WRAP(y) STR(y)

#define ID(x) x
#define WRAP_ID(y) ID(y)

#define ID1(x) x
#define ID2(x) ID1(x)
#define ID3(x) ID2(x)

#define ADD(a, b) ((a) + (b))
#define CALL(f, x, y) f(x, y)

int main(void) {
    // Test 1-2: Nested macro at line start
    const char *s =
WRAP(hello);
    if (s[0] != 'h') return 1;
    if (s[4] != 'o') return 2;

    // Test 3: Identity macro at line start
    int result =
WRAP_ID(42);
    if (result != 42) return 3;

    // Test 4: Deep nesting at line start
    int value =
ID3(100);
    if (value != 100) return 4;

    // Test 5: Multi-arg macro at line start
    int sum =
CALL(ADD, 10, 20);
    if (sum != 30) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("nested_macro", code), 0);
}

// ============================================================================
// Stringification and token pasting
// ============================================================================

#[test]
fn stringify_and_paste() {
    let code = r#"
#define STR(x) #x
#define XSTR(x) STR(x)
#define VALUE 123

#define PASTE(a, b) a ## b
#define MAKE_VAR(n) PASTE(var_, n)

int main(void) {
    // Test 1-3: Stringification in nested macro
    // XSTR(VALUE) -> STR(123) -> "123"
    const char *s = XSTR(VALUE);
    if (s[0] != '1') return 1;
    if (s[1] != '2') return 2;
    if (s[2] != '3') return 3;

    // Test 4: Basic token paste
    int var123 = 42;
    int result = PASTE(var, 123);
    if (result != 42) return 4;

    // Test 5: Nested token paste
    int var_100 = 55;
    result = MAKE_VAR(100);
    if (result != 55) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("stringify_paste", code), 0);
}
