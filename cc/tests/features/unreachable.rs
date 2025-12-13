//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for __builtin_unreachable() support
//
// __builtin_unreachable() tells the compiler that a code path is never reached.
// If control flow actually reaches it, behavior is undefined (typically a trap).
//
// Use cases:
// - Marking impossible switch default cases
// - After exhaustive if-else chains
// - Silencing "control reaches end of non-void function" warnings
// - Enabling optimizations by asserting invariants
//

use crate::common::compile_and_run;

// ============================================================================
// Basic usage - unreachable in code paths that are never taken
// ============================================================================

#[test]
fn unreachable_after_return() {
    // Basic test: unreachable after a return that always executes
    let code = r#"
int foo(void) {
    return 42;
    __builtin_unreachable();  // Never reached
}

int main(void) {
    return foo();
}
"#;
    assert_eq!(compile_and_run("unreachable_after_return", code), 42);
}

#[test]
fn unreachable_in_if_branch() {
    // Unreachable in a branch that's never taken
    let code = r#"
int foo(int x) {
    if (x > 0) {
        return x;
    } else {
        __builtin_unreachable();
    }
}

int main(void) {
    return foo(42);  // Always positive, so unreachable branch is never taken
}
"#;
    assert_eq!(compile_and_run("unreachable_in_if_branch", code), 42);
}

// ============================================================================
// Switch statement usage - common pattern for exhaustive enums
// ============================================================================

#[test]
fn unreachable_switch_default() {
    // Common pattern: unreachable default in exhaustive switch
    let code = r#"
enum Color { RED, GREEN, BLUE };

int color_value(enum Color c) {
    switch (c) {
    case RED:   return 10;
    case GREEN: return 20;
    case BLUE:  return 30;
    default:
        __builtin_unreachable();  // All enum values covered
    }
}

int main(void) {
    int r = color_value(RED);    // 10
    int g = color_value(GREEN);  // 20
    int b = color_value(BLUE);   // 30
    return r + g + b - 18;       // 60 - 18 = 42
}
"#;
    assert_eq!(compile_and_run("unreachable_switch_default", code), 42);
}

#[test]
fn unreachable_after_switch_all_return() {
    // When all switch cases return, code after switch is unreachable
    let code = r#"
int get_val(int x) {
    switch (x) {
    case 0: return 10;
    case 1: return 20;
    case 2: return 30;
    default: return 40;
    }
    __builtin_unreachable();  // All cases return
}

int main(void) {
    return get_val(0) + get_val(1) + get_val(2) - 18;  // 10 + 20 + 30 - 18 = 42
}
"#;
    assert_eq!(
        compile_and_run("unreachable_after_switch_all_return", code),
        42
    );
}

// ============================================================================
// With conditional expressions
// ============================================================================

#[test]
fn unreachable_assert_invariant() {
    // Use unreachable to assert an invariant the compiler can't prove
    let code = r#"
int divide(int a, int b) {
    if (b == 0) {
        __builtin_unreachable();  // Caller guarantees b != 0
    }
    return a / b;
}

int main(void) {
    return divide(84, 2);  // We guarantee b != 0
}
"#;
    assert_eq!(compile_and_run("unreachable_assert_invariant", code), 42);
}

#[test]
fn unreachable_in_else_chain() {
    // Exhaustive else-if chain
    let code = r#"
int categorize(int x) {
    if (x < 0) {
        return -1;
    } else if (x == 0) {
        return 0;
    } else if (x > 0) {
        return 1;
    } else {
        __builtin_unreachable();  // Mathematically impossible
    }
}

int main(void) {
    int neg = categorize(-5);   // -1
    int zero = categorize(0);   // 0
    int pos = categorize(100);  // 1
    // -1 + 0 + 1 + 42 = 42
    return neg + zero + pos + 42;
}
"#;
    assert_eq!(compile_and_run("unreachable_in_else_chain", code), 42);
}

// ============================================================================
// Control flow patterns
// ============================================================================

#[test]
fn unreachable_after_infinite_loop_break() {
    // After a loop that always breaks, code is unreachable
    let code = r#"
int foo(void) {
    int i = 0;
    while (1) {
        i = i + 1;
        if (i >= 42) {
            return i;
        }
    }
    __builtin_unreachable();  // Loop either continues or returns
}

int main(void) {
    return foo();
}
"#;
    assert_eq!(
        compile_and_run("unreachable_after_infinite_loop_break", code),
        42
    );
}

#[test]
fn unreachable_multiple_in_function() {
    // Multiple unreachable statements in different branches
    let code = r#"
int classify(int x) {
    if (x < 0) {
        return 0;
    }
    if (x > 100) {
        return 2;
    }
    // 0 <= x <= 100
    if (x <= 100) {
        return 1;
    }
    __builtin_unreachable();  // x > 100 already handled above
}

int main(void) {
    int a = classify(-1);   // 0
    int b = classify(50);   // 1
    int c = classify(200);  // 2
    // 0 + 1 + 2 + 39 = 42
    return a + b + c + 39;
}
"#;
    assert_eq!(
        compile_and_run("unreachable_multiple_in_function", code),
        42
    );
}

// ============================================================================
// Void expression context
// ============================================================================

#[test]
fn unreachable_as_expression() {
    // __builtin_unreachable() is a void expression
    let code = r#"
void do_nothing(void) {
    // This function always returns normally
    return;
    __builtin_unreachable();
}

int main(void) {
    do_nothing();
    return 42;
}
"#;
    assert_eq!(compile_and_run("unreachable_as_expression", code), 42);
}
