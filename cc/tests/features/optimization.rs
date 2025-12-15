//
// Copyright (c) 2025-2026 Jeff Garzik
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
// Basic optimization correctness (algebraic, identity, bitwise)
// ============================================================================

#[test]
fn optimization_basic_correctness() {
    let code = r#"
int main(void) {
    // Test 1-4: Basic arithmetic (should not be broken by optimizer)
    int a = 2 + 3;
    int b = 10 - 10;
    int c = 4 * 1;
    int d = 0 + 7;
    if (a != 5) return 1;
    if (b != 0) return 2;
    if (c != 4) return 3;
    if (d != 7) return 4;

    // Test 5-7: Algebraic simplifications
    int x = 42;
    int y = x + 0;              // Should simplify to x
    int z = x * 1;              // Should simplify to x
    int w = x - 0;              // Should simplify to x
    if (y != 42) return 5;
    if (z != 42) return 6;
    if (w != 42) return 7;

    // Test 8-9: Identity patterns (x & x -> x, x | x -> x)
    int r = x & x;              // Should be x
    int s = x | x;              // Should be x
    if (r != 42) return 8;
    if (s != 42) return 9;

    // Test 10-11: Bitwise with zero constants
    int u = x | 0;              // Should be x
    int v = x ^ 0;              // Should be x
    if (u != 42) return 10;
    if (v != 42) return 11;

    // Test 12-13: Shifts by zero
    int sh1 = x << 0;           // Should be x
    int sh2 = x >> 0;           // Should be x
    if (sh1 != 42) return 12;
    if (sh2 != 42) return 13;

    // Test 14-19: Comparisons should still work
    int cmp = 5;
    int cmp2 = 10;
    if (cmp != cmp) return 14;
    if (cmp == cmp2) return 15;
    if (!(cmp < cmp2)) return 16;
    if (!(cmp2 > cmp)) return 17;
    if (cmp > cmp2) return 18;
    if (cmp2 < cmp) return 19;

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("opt_basic", code), 0);
}

// ============================================================================
// Optimization with control flow, function calls, and stores
// ============================================================================

#[test]
fn optimization_advanced() {
    let code = r#"
int counter = 0;

int side_effect(int x) {
    counter++;
    return x * 2;
}

int global_var = 0;

int main(void) {
    // Test 1-2: Loops with potential dead code
    int sum = 0;
    for (int i = 0; i < 10; i++) {
        int live = i + 1;       // Live: contributes to sum
        sum += live;
    }
    if (sum != 55) return 1;    // 1+2+3+4+5+6+7+8+9+10

    // Test 3: While loop
    int count = 0;
    int j = 5;
    while (j > 0) {
        int unused = j + 0;     // Simplifies to j, but unused
        count++;
        j--;
    }
    if (count != 5) return 2;

    // Test 4-5: Function calls must not be eliminated (side effects)
    int unused = side_effect(5);
    int used = side_effect(10);
    if (used != 20) return 3;
    if (counter != 2) return 4;  // Both calls executed

    // Test 6-7: Stores to global must be preserved
    global_var = 42;
    int local = 10;
    local = local + 5;
    if (global_var != 42) return 5;
    if (local != 15) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("opt_advanced", code), 0);
}
