//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for variadic function support (va_list, va_start, va_arg, va_end, va_copy)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Variadic function implementation tests (callee-side)
// Tests: va_list declaration, sum_ints, mixed_types, pointers, va_copy
// ============================================================================

#[test]
fn varargs_callee_comprehensive() {
    let code = r#"
// Test 1: Simple va_list declaration (must compile and run)
void test_decl(void) {
    __builtin_va_list ap;
    (void)ap;  // suppress unused warning
}

// Test 2: Sum integers using va_arg
int sum_ints(int count, ...) {
    __builtin_va_list ap;
    __builtin_va_start(ap, count);

    int total = 0;
    for (int i = 0; i < count; i++) {
        total += __builtin_va_arg(ap, int);
    }

    __builtin_va_end(ap);
    return total;
}

// Test 3: Mixed types (int + long + int)
long sum_mixed(int count, ...) {
    __builtin_va_list ap;
    __builtin_va_start(ap, count);

    long total = 0;

    if (count > 0) {
        total += __builtin_va_arg(ap, int);
    }
    if (count > 1) {
        total += __builtin_va_arg(ap, long);
    }
    if (count > 2) {
        total += __builtin_va_arg(ap, int);
    }

    __builtin_va_end(ap);
    return total;
}

// Test 4: Pointer arguments
int first_chars_sum(int count, ...) {
    __builtin_va_list ap;
    __builtin_va_start(ap, count);

    int total = 0;
    for (int i = 0; i < count; i++) {
        char *str = __builtin_va_arg(ap, char*);
        if (str) {
            total += str[0];
        }
    }

    __builtin_va_end(ap);
    return total;
}

// Test 5: va_copy
int test_copy(int count, ...) {
    __builtin_va_list ap, ap2;
    __builtin_va_start(ap, count);
    __builtin_va_copy(ap2, ap);

    int val1 = __builtin_va_arg(ap, int);
    int val2 = __builtin_va_arg(ap2, int);

    __builtin_va_end(ap);
    __builtin_va_end(ap2);

    if (val1 != val2) return -1;
    return val1;
}

int main(void) {
    // Run test_decl (just needs to not crash)
    test_decl();

    // Test sum_ints
    if (sum_ints(3, 10, 20, 12) != 42) return 1;
    if (sum_ints(5, 1, 2, 3, 4, 5) != 15) return 2;
    if (sum_ints(1, 100) != 100) return 3;
    if (sum_ints(0) != 0) return 4;

    // Test sum_mixed
    if (sum_mixed(3, 10, 20L, 12) != 42) return 5;

    // Test first_chars_sum: 'A' = 65, 'B' = 66, 'C' = 67 -> 198
    if (first_chars_sum(3, "ABC", "BCD", "CDE") != 198) return 6;

    // Test va_copy
    if (test_copy(1, 42) != 42) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("varargs_callee_comprehensive", code), 0);
}

// ============================================================================
// Variadic function caller tests (calling libc variadic functions)
// ============================================================================

#[test]
fn varargs_caller_comprehensive() {
    let code = r#"
int sprintf(char *buf, const char *fmt, ...);

int main(void) {
    char buf[64];

    // Test sprintf with single integer
    sprintf(buf, "%d", 42);
    if (buf[0] != '4' || buf[1] != '2') return 1;

    // Test sprintf with multiple args: "2+3=5"
    sprintf(buf, "%d+%d=%d", 2, 3, 5);
    if (buf[0] != '2') return 2;
    if (buf[2] != '3') return 3;
    if (buf[4] != '5') return 4;

    // Test sprintf with string
    sprintf(buf, "%s", "hello");
    if (buf[0] != 'h' || buf[4] != 'o') return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("varargs_caller_comprehensive", code), 0);
}

// ============================================================================
// Variadic function calls with floating-point arguments
// Tests the AL register being set correctly (SysV x86-64 ABI requirement)
// ============================================================================

#[test]
fn varargs_float_args() {
    let code = r#"
int sprintf(char *buf, const char *fmt, ...);

int main(void) {
    char buf[128];

    // Test sprintf with double argument
    // This tests that AL is correctly set to indicate XMM register usage
    sprintf(buf, "%d", (int)3.14159);
    if (buf[0] != '3') return 1;

    // Test with actual float formatting (requires libc printf float support)
    // Just test that we can pass floats to variadic functions without crashing
    double d = 42.5;
    sprintf(buf, "%.0f", d);
    // Check first char is '4' (from 42.5 -> "42" or "43")
    if (buf[0] != '4') return 2;

    // Multiple float args
    double a = 10.0;
    double b = 20.0;
    sprintf(buf, "%.0f,%.0f", a, b);
    if (buf[0] != '1' || buf[3] != '2') return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("varargs_float_args", code), 0);
}
