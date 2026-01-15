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
    assert_eq!(compile_and_run("nested_macro", code, &[]), 0);
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
    assert_eq!(compile_and_run("stringify_paste", code, &[]), 0);
}

// ============================================================================
// String Literal Concatenation (C99 6.4.5)
// ============================================================================

#[test]
fn string_literal_concatenation() {
    let code = r#"
int main(void) {
    // Test 1-2: Basic adjacent string literal concatenation
    const char *s1 = "hello" "world";
    if (s1[0] != 'h') return 1;
    if (s1[5] != 'w') return 2;  // "hello" is 5 chars, then "world" starts

    // Test 3-4: Three adjacent strings
    const char *s2 = "a" "b" "c";
    if (s2[0] != 'a') return 3;
    if (s2[2] != 'c') return 4;

    // Test 5-6: With whitespace and newlines between strings
    const char *s3 = "foo"
                     "bar";
    if (s3[0] != 'f') return 5;
    if (s3[3] != 'b') return 6;

    // Test 7-8: Macro expansion producing adjacent strings
#define STR1 "hello"
#define STR2 "world"
    const char *s4 = STR1 STR2;
    if (s4[0] != 'h') return 7;
    if (s4[5] != 'w') return 8;

    // Test 9-10: String with escape sequences
    const char *s5 = "line1\n" "line2";
    if (s5[5] != '\n') return 9;
    if (s5[6] != 'l') return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("string_concat", code, &[]), 0);
}

// ============================================================================
// Macro Expansion in #if/#elif Conditions (C99 6.10.1)
// ============================================================================

#[test]
fn macro_expansion_in_if() {
    let code = r#"
#define VALUE 42
#define ZERO 0
#define ONE 1
#define ADD(a, b) ((a) + (b))

// Test 1: Simple macro in #if
#if VALUE == 42
int test1_passed = 1;
#else
int test1_passed = 0;
#endif

// Test 2: Macro that expands to zero
#if ZERO
int test2_passed = 0;
#else
int test2_passed = 1;
#endif

// Test 3: Macro that expands to one
#if ONE
int test3_passed = 1;
#else
int test3_passed = 0;
#endif

// Test 4: Macro in complex expression
#if VALUE > 40 && VALUE < 50
int test4_passed = 1;
#else
int test4_passed = 0;
#endif

// Test 5: Function-like macro in #if
#if ADD(1, 1) == 2
int test5_passed = 1;
#else
int test5_passed = 0;
#endif

// Test 6: Undefined identifier in #if (should be 0)
#if UNDEFINED_MACRO
int test6_passed = 0;
#else
int test6_passed = 1;
#endif

// Test 7: defined() with macro
#if defined(VALUE)
int test7_passed = 1;
#else
int test7_passed = 0;
#endif

// Test 8: defined() without parens
#if defined VALUE
int test8_passed = 1;
#else
int test8_passed = 0;
#endif

// Test 9: !defined()
#if !defined(NONEXISTENT)
int test9_passed = 1;
#else
int test9_passed = 0;
#endif

// Test 10: Macro in #elif
#define OPTION 2
#if OPTION == 1
int test10_passed = 0;
#elif OPTION == 2
int test10_passed = 1;
#else
int test10_passed = 0;
#endif

int main(void) {
    if (!test1_passed) return 1;
    if (!test2_passed) return 2;
    if (!test3_passed) return 3;
    if (!test4_passed) return 4;
    if (!test5_passed) return 5;
    if (!test6_passed) return 6;
    if (!test7_passed) return 7;
    if (!test8_passed) return 8;
    if (!test9_passed) return 9;
    if (!test10_passed) return 10;
    return 0;
}
"#;
    assert_eq!(compile_and_run("macro_if", code, &[]), 0);
}

// ============================================================================
// #error Directive Exit Code (Bug #1 fix: CPython configure compatibility)
// ============================================================================

#[test]
fn error_directive_exit_code() {
    use plib::testing::run_test_base;
    use std::io::Write;

    // Test that #error directive causes non-zero exit code in preprocess-only mode
    let code = r#"
#ifdef __ANDROID__
android_api = __ANDROID_API__
#else
#error not Android
#endif
"#;

    // Create temp file
    let mut file = tempfile::Builder::new()
        .prefix("pcc_test_error_")
        .suffix(".c")
        .tempfile()
        .expect("failed to create temp file");
    file.write_all(code.as_bytes())
        .expect("failed to write test file");
    let path = file.path().to_path_buf();

    // Run pcc -E (preprocess only)
    let args = vec!["-E".to_string(), path.to_str().unwrap().to_string()];
    let output = run_test_base("pcc", &args, &[]);

    // Should fail with non-zero exit code
    assert!(
        !output.status.success(),
        "#error directive should cause non-zero exit code"
    );
}

// ============================================================================
// __has_builtin comprehensive test
// ============================================================================

#[test]
fn has_builtin_comprehensive() {
    let code = r#"
int main(void) {
    int result = 0;

    // Test variadic builtins
#if __has_builtin(__builtin_va_start)
    result++;
#endif
#if __has_builtin(__builtin_va_end)
    result++;
#endif
#if __has_builtin(__builtin_va_arg)
    result++;
#endif
#if __has_builtin(__builtin_va_copy)
    result++;
#endif

    // Test byte swap builtins
#if __has_builtin(__builtin_bswap16)
    result++;
#endif
#if __has_builtin(__builtin_bswap32)
    result++;
#endif
#if __has_builtin(__builtin_bswap64)
    result++;
#endif

    // Test bit manipulation builtins
#if __has_builtin(__builtin_ctz)
    result++;
#endif
#if __has_builtin(__builtin_clz)
    result++;
#endif
#if __has_builtin(__builtin_popcount)
    result++;
#endif

    // Test memory builtins
#if __has_builtin(__builtin_alloca)
    result++;
#endif

    // Test compile-time builtins
#if __has_builtin(__builtin_constant_p)
    result++;
#endif
#if __has_builtin(__builtin_types_compatible_p)
    result++;
#endif
#if __has_builtin(__builtin_unreachable)
    result++;
#endif
#if __has_builtin(__builtin_offsetof)
    result++;
#endif

    // Test floating-point constant builtins
#if __has_builtin(__builtin_inf)
    result++;
#endif
#if __has_builtin(__builtin_inff)
    result++;
#endif
#if __has_builtin(__builtin_huge_val)
    result++;
#endif

    // Test floating-point math builtins
#if __has_builtin(__builtin_fabs)
    result++;
#endif
#if __has_builtin(__builtin_fabsf)
    result++;
#endif
#if __has_builtin(__builtin_fabsl)
    result++;
#endif

    // Test C11 atomic builtins
#if __has_builtin(__c11_atomic_load)
    result++;
#endif
#if __has_builtin(__c11_atomic_store)
    result++;
#endif
#if __has_builtin(__c11_atomic_exchange)
    result++;
#endif
#if __has_builtin(__c11_atomic_compare_exchange_strong)
    result++;
#endif
#if __has_builtin(__c11_atomic_fetch_add)
    result++;
#endif
#if __has_builtin(__c11_atomic_thread_fence)
    result++;
#endif

    // Test unknown builtin returns 0
#if __has_builtin(__builtin_nonexistent)
    result = -1;  // Should not happen
#endif

    // Expected: 27 builtins detected
    // (4 va + 3 bswap + 3 bit + 1 alloca + 4 compile-time + 3 fp-const + 3 fp-math + 6 atomic)
    return (result == 27) ? 0 : result;
}
"#;
    assert_eq!(compile_and_run("has_builtin", code, &[]), 0);
}
