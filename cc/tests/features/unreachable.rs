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
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Basic unreachable patterns (after return, in if branch)
// ============================================================================

#[test]
fn unreachable_basic_patterns() {
    let code = r#"
// Basic test: unreachable after a return that always executes
int foo_return(void) {
    return 42;
    __builtin_unreachable();  // Never reached
}

// Unreachable in a branch that's never taken
int foo_branch(int x) {
    if (x > 0) {
        return x;
    } else {
        __builtin_unreachable();
    }
}

// Use unreachable to assert an invariant
int divide(int a, int b) {
    if (b == 0) {
        __builtin_unreachable();  // Caller guarantees b != 0
    }
    return a / b;
}

// __builtin_unreachable() is a void expression
void do_nothing(void) {
    return;
    __builtin_unreachable();
}

int main(void) {
    // Test 1: After return
    if (foo_return() != 42) return 1;

    // Test 2: In branch
    if (foo_branch(42) != 42) return 2;

    // Test 3: Assert invariant
    if (divide(84, 2) != 42) return 3;

    // Test 4: Void expression
    do_nothing();

    return 0;
}
"#;
    assert_eq!(compile_and_run("unreachable_basic", code, &[]), 0);
}

// ============================================================================
// Switch statement usage - common pattern for exhaustive enums
// ============================================================================

#[test]
fn unreachable_switch_patterns() {
    let code = r#"
enum Color { RED, GREEN, BLUE };

// Common pattern: unreachable default in exhaustive switch
int color_value(enum Color c) {
    switch (c) {
    case RED:   return 10;
    case GREEN: return 20;
    case BLUE:  return 30;
    default:
        __builtin_unreachable();  // All enum values covered
    }
}

// When all switch cases return, code after switch is unreachable
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
    // Test 1-3: Color enum
    int r = color_value(RED);    // 10
    int g = color_value(GREEN);  // 20
    int b = color_value(BLUE);   // 30
    if (r + g + b - 18 != 42) return 1;

    // Test 4-6: All cases return
    if (get_val(0) + get_val(1) + get_val(2) - 18 != 42) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("unreachable_switch", code, &[]), 0);
}

// ============================================================================
// Conditional and control flow patterns
// ============================================================================

#[test]
fn unreachable_control_flow() {
    let code = r#"
// Exhaustive else-if chain
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

// After a loop that always breaks, code is unreachable
int loop_return(void) {
    int i = 0;
    while (1) {
        i = i + 1;
        if (i >= 42) {
            return i;
        }
    }
    __builtin_unreachable();  // Loop either continues or returns
}

// Multiple unreachable statements in different branches
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
    // Test 1-3: Exhaustive else-if
    int neg = categorize(-5);   // -1
    int zero = categorize(0);   // 0
    int pos = categorize(100);  // 1
    if (neg + zero + pos + 42 != 42) return 1;

    // Test 4: Loop return
    if (loop_return() != 42) return 2;

    // Test 5-7: Multiple unreachable
    int a = classify(-1);   // 0
    int b = classify(50);   // 1
    int c = classify(200);  // 2
    if (a + b + c + 39 != 42) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("unreachable_control_flow", code, &[]), 0);
}
