//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for compiler optimization passes (-O flag)
//
// These tests verify that optimizations don't break program correctness.
// Tests are aggregated to reduce compile/link cycles.
//

use crate::common::compile_and_run_optimized;

// ============================================================================
// Basic optimization correctness tests
// ============================================================================

/// Test that -O doesn't break basic arithmetic and control flow
/// Focuses on algebraic simplifications that the MVP supports
#[test]
fn optimization_basic_correctness() {
    let code = r#"
int main(void) {
    // Basic arithmetic (should not be broken by optimizer)
    int a = 2 + 3;
    int b = 10 - 10;
    int c = 4 * 1;
    int d = 0 + 7;

    if (a != 5) return 1;
    if (b != 0) return 2;
    if (c != 4) return 3;
    if (d != 7) return 4;

    // Algebraic simplifications (MVP supports these)
    int x = 42;
    int y = x + 0;              // Should simplify to x
    int z = x * 1;              // Should simplify to x
    int w = x - 0;              // Should simplify to x

    if (y != 42) return 5;
    if (z != 42) return 6;
    if (w != 42) return 7;

    // Identity patterns (MVP supports x & x -> x, x | x -> x)
    int r = x & x;              // Should be x
    int s = x | x;              // Should be x

    if (r != 42) return 10;
    if (s != 42) return 11;

    // Bitwise with zero constants
    int u = x | 0;              // Should be x
    int v = x ^ 0;              // Should be x

    if (u != 42) return 13;
    if (v != 42) return 14;

    // Shifts by zero
    int sh1 = x << 0;           // Should be x
    int sh2 = x >> 0;           // Should be x

    if (sh1 != 42) return 15;
    if (sh2 != 42) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("opt_basic", code), 0);
}

/// Test optimization with loops (dead code inside loops)
#[test]
fn optimization_loops() {
    let code = r#"
int main(void) {
    int sum = 0;

    // Loop with potential dead code
    for (int i = 0; i < 10; i++) {
        int live = i + 1;       // Live: contributes to sum
        sum += live;
    }

    // sum should be 1+2+3+4+5+6+7+8+9+10 = 55
    if (sum != 55) return 1;

    // While loop
    int count = 0;
    int j = 5;
    while (j > 0) {
        int unused = j + 0;     // Simplifies to j, but unused (DCE candidate)
        count++;
        j--;
    }

    if (count != 5) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("opt_loops", code), 0);
}

/// Test optimization with function calls (calls must not be eliminated)
#[test]
fn optimization_function_calls() {
    let code = r#"
int counter = 0;

int side_effect(int x) {
    counter++;
    return x * 2;
}

int main(void) {
    // Call with unused result - must still execute for side effect
    int unused = side_effect(5);

    // Call with used result
    int used = side_effect(10);
    if (used != 20) return 1;

    // Counter should be 2 (both calls executed)
    if (counter != 2) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("opt_calls", code), 0);
}

/// Test that comparisons work correctly with optimization
#[test]
fn optimization_comparisons() {
    let code = r#"
int main(void) {
    int x = 5;
    int y = 5;
    int z = 10;

    // Basic comparisons should still work
    if (x != y) return 1;
    if (x == z) return 2;
    if (!(x < z)) return 3;
    if (!(z > x)) return 4;
    if (x > z) return 5;
    if (z < x) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("opt_cmp", code), 0);
}

/// Test that stores are not eliminated (side effects)
#[test]
fn optimization_stores_preserved() {
    let code = r#"
int global_var = 0;

int main(void) {
    // Stores to global must be preserved
    global_var = 42;

    // Local that's written but read later
    int local = 10;
    local = local + 5;

    if (global_var != 42) return 1;
    if (local != 15) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("opt_stores", code), 0);
}
