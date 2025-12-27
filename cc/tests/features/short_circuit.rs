//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for short-circuit boolean evaluation and ternary operators.
//
// These tests verify C99-correct evaluation semantics:
// - && short-circuits: if left is false, right is NOT evaluated
// - || short-circuits: if left is true, right is NOT evaluated
// - ?: only evaluates the selected branch (for impure expressions)
// - Pure ternary expressions may use cmov/csel (both branches evaluated)
//

use crate::common::compile_and_run;

// ============================================================================
// Short-circuit AND (&&) - C99 6.5.13
// ============================================================================

#[test]
fn short_circuit_and_basic() {
    let code = r#"
int counter = 0;

int side_effect(void) {
    counter++;
    return 1;
}

int main(void) {
    // Test 1: true && true
    counter = 0;
    int result = (1 && side_effect());
    if (result != 1) return 1;
    if (counter != 1) return 2;  // side_effect was called

    // Test 2: false && ... - second operand NOT evaluated
    counter = 0;
    result = (0 && side_effect());
    if (result != 0) return 3;
    if (counter != 0) return 4;  // side_effect was NOT called (short-circuit)

    // Test 3: Variable forms
    int a = 0;
    counter = 0;
    result = (a && side_effect());
    if (result != 0) return 5;
    if (counter != 0) return 6;  // short-circuit when a=0

    // Test 4: Non-zero left, should evaluate right
    a = 42;
    counter = 0;
    result = (a && side_effect());
    if (result != 1) return 7;
    if (counter != 1) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("sc_and_basic", code), 0);
}

// ============================================================================
// Short-circuit OR (||) - C99 6.5.14
// ============================================================================

#[test]
fn short_circuit_or_basic() {
    let code = r#"
int counter = 0;

int side_effect(void) {
    counter++;
    return 0;
}

int main(void) {
    // Test 1: false || false
    counter = 0;
    int result = (0 || side_effect());
    if (result != 0) return 1;
    if (counter != 1) return 2;  // side_effect was called

    // Test 2: true || ... - second operand NOT evaluated
    counter = 0;
    result = (1 || side_effect());
    if (result != 1) return 3;
    if (counter != 0) return 4;  // side_effect was NOT called (short-circuit)

    // Test 3: Variable forms
    int a = 42;
    counter = 0;
    result = (a || side_effect());
    if (result != 1) return 5;
    if (counter != 0) return 6;  // short-circuit when a!=0

    // Test 4: Zero left, should evaluate right
    a = 0;
    counter = 0;
    result = (a || side_effect());
    if (result != 0) return 7;
    if (counter != 1) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("sc_or_basic", code), 0);
}

// ============================================================================
// Chained short-circuit operators
// ============================================================================

#[test]
fn short_circuit_chained() {
    let code = r#"
int counter = 0;

int inc_and_return(int x) {
    counter++;
    return x;
}

int main(void) {
    // Test 1: a && b && c - stops at first false
    counter = 0;
    int r = (inc_and_return(1) && inc_and_return(0) && inc_and_return(1));
    if (r != 0) return 1;
    if (counter != 2) return 2;  // Only first two evaluated

    // Test 2: a || b || c - stops at first true
    counter = 0;
    r = (inc_and_return(0) || inc_and_return(1) || inc_and_return(1));
    if (r != 1) return 3;
    if (counter != 2) return 4;  // Only first two evaluated

    // Test 3: Mixed && and ||
    counter = 0;
    r = (inc_and_return(1) && inc_and_return(1) || inc_and_return(0));
    if (r != 1) return 5;
    if (counter != 2) return 6;  // (1 && 1) is true, || short-circuits

    // Test 4: All must evaluate
    counter = 0;
    r = (inc_and_return(1) && inc_and_return(1) && inc_and_return(1));
    if (r != 1) return 7;
    if (counter != 3) return 8;  // All three evaluated

    return 0;
}
"#;
    assert_eq!(compile_and_run("sc_chained", code), 0);
}

// ============================================================================
// Ternary operator with side effects - C99 6.5.15
// ============================================================================

#[test]
fn ternary_side_effects() {
    let code = r#"
int counter_true = 0;
int counter_false = 0;

int true_branch(void) {
    counter_true++;
    return 42;
}

int false_branch(void) {
    counter_false++;
    return 99;
}

int main(void) {
    // Test 1: Condition true - only true branch evaluated
    counter_true = 0;
    counter_false = 0;
    int result = (1 ? true_branch() : false_branch());
    if (result != 42) return 1;
    if (counter_true != 1) return 2;
    if (counter_false != 0) return 3;  // false_branch NOT called

    // Test 2: Condition false - only false branch evaluated
    counter_true = 0;
    counter_false = 0;
    result = (0 ? true_branch() : false_branch());
    if (result != 99) return 4;
    if (counter_true != 0) return 5;   // true_branch NOT called
    if (counter_false != 1) return 6;

    // Test 3: Variable condition
    int cond = 5;
    counter_true = 0;
    counter_false = 0;
    result = (cond ? true_branch() : false_branch());
    if (result != 42) return 7;
    if (counter_true != 1) return 8;
    if (counter_false != 0) return 9;

    // Test 4: Zero variable condition
    cond = 0;
    counter_true = 0;
    counter_false = 0;
    result = (cond ? true_branch() : false_branch());
    if (result != 99) return 10;
    if (counter_true != 0) return 11;
    if (counter_false != 1) return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ternary_side_effects", code), 0);
}

// ============================================================================
// Pure ternary expressions (no side effects) - may use cmov/csel
// ============================================================================

#[test]
fn ternary_pure() {
    let code = r#"
int main(void) {
    // Pure ternary expressions with simple values
    int a = 10;
    int b = 20;

    // Test 1-2: Simple ternary with literals
    int max = (a > b) ? a : b;
    if (max != 20) return 1;

    int min = (a < b) ? a : b;
    if (min != 10) return 2;

    // Test 3-4: Ternary with variables
    int cond = 1;
    int r = cond ? a : b;
    if (r != 10) return 3;

    cond = 0;
    r = cond ? a : b;
    if (r != 20) return 4;

    // Test 5-6: Nested pure ternary
    int x = 5, y = 10, z = 15;
    int mid = (x > y) ? ((x > z) ? z : x) : ((y > z) ? z : y);
    if (mid != 10) return 5;

    // Test 7: Ternary result used in expression
    int sum = (a > 5 ? 100 : 0) + (b > 5 ? 200 : 0);
    if (sum != 300) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ternary_pure", code), 0);
}

// ============================================================================
// Combined short-circuit and ternary
// ============================================================================

#[test]
fn short_circuit_with_ternary() {
    let code = r#"
int effect_count = 0;

int has_effect(int x) {
    effect_count++;
    return x;
}

int main(void) {
    // Test 1: && with ternary - condition determines ternary evaluation
    effect_count = 0;
    int r = (0 && (has_effect(1) ? has_effect(2) : has_effect(3)));
    if (r != 0) return 1;
    if (effect_count != 0) return 2;  // Nothing evaluated due to &&

    // Test 2: Ternary with && in branches
    effect_count = 0;
    r = (1 ? (has_effect(1) && has_effect(1)) : has_effect(0));
    if (r != 1) return 3;
    if (effect_count != 2) return 4;  // Only true branch evaluated

    // Test 3: Complex expression
    int a = 1, b = 0, c = 1;
    effect_count = 0;
    r = (a && !b) ? (has_effect(10) + has_effect(20)) : has_effect(99);
    if (r != 30) return 5;
    if (effect_count != 2) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("sc_with_ternary", code), 0);
}

// ============================================================================
// Ternary with different types
// ============================================================================

#[test]
fn ternary_types() {
    let code = r#"
int main(void) {
    // Test 1: Integer types
    int i = 1 ? 100 : 200;
    if (i != 100) return 1;

    // Test 2: Long types
    long l = 0 ? 100L : 200L;
    if (l != 200L) return 2;

    // Test 3: Pointer types
    int arr[2] = {10, 20};
    int *p = 1 ? &arr[0] : &arr[1];
    if (*p != 10) return 3;

    // Test 4: Pointer vs null
    p = 0 ? &arr[0] : (int*)0;
    if (p != 0) return 4;

    // Test 5: Char types
    char c = 1 ? 'A' : 'B';
    if (c != 'A') return 5;

    // Test 6: Short types
    short s = 0 ? (short)100 : (short)200;
    if (s != 200) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ternary_types", code), 0);
}

// ============================================================================
// Pre-increment/pre-decrement in ternary (impure expressions)
// ============================================================================

#[test]
fn ternary_pre_inc_dec() {
    let code = r#"
int main(void) {
    // Test 1: Pre-increment in ternary - only selected branch evaluated
    int x = 10, y = 20;
    int result = (1 ? ++x : ++y);
    if (result != 11) return 1;
    if (x != 11) return 2;     // x was incremented
    if (y != 20) return 3;     // y was NOT incremented

    // Test 2: Pre-increment in false branch
    x = 10; y = 20;
    result = (0 ? ++x : ++y);
    if (result != 21) return 4;
    if (x != 10) return 5;     // x was NOT incremented
    if (y != 21) return 6;     // y was incremented

    // Test 3: Pre-decrement in ternary
    x = 10; y = 20;
    result = (1 ? --x : --y);
    if (result != 9) return 7;
    if (x != 9) return 8;      // x was decremented
    if (y != 20) return 9;     // y was NOT decremented

    // Test 4: Pre-decrement in false branch
    x = 10; y = 20;
    result = (0 ? --x : --y);
    if (result != 19) return 10;
    if (x != 10) return 11;    // x was NOT decremented
    if (y != 19) return 12;    // y was decremented

    // Test 5: Variable condition with pre-increment
    int cond = 5;
    x = 100; y = 200;
    result = (cond ? ++x : ++y);
    if (result != 101) return 13;
    if (x != 101) return 14;
    if (y != 200) return 15;

    // Test 6: Zero variable condition with pre-increment
    cond = 0;
    x = 100; y = 200;
    result = (cond ? ++x : ++y);
    if (result != 201) return 16;
    if (x != 100) return 17;
    if (y != 201) return 18;

    // Test 7: Mixed pre-inc and pre-dec
    x = 50; y = 50;
    result = (1 ? ++x : --y);
    if (result != 51) return 19;
    if (x != 51) return 20;
    if (y != 50) return 21;

    // Test 8: Pre-increment in nested ternary
    x = 0; y = 0;
    int z = 0;
    result = (1 ? (0 ? ++x : ++y) : ++z);
    if (result != 1) return 22;
    if (x != 0) return 23;     // x NOT incremented
    if (y != 1) return 24;     // y incremented (inner false branch)
    if (z != 0) return 25;     // z NOT incremented (outer false branch)

    return 0;
}
"#;
    assert_eq!(compile_and_run("ternary_pre_inc_dec", code), 0);
}

// ============================================================================
// Edge cases and complex scenarios
// ============================================================================

#[test]
fn short_circuit_edge_cases() {
    let code = r#"
int div_counter = 0;

int safe_div(int a, int b) {
    div_counter++;
    return a / b;
}

int main(void) {
    // Test 1: Avoid division by zero with &&
    int x = 0;
    div_counter = 0;
    int safe = (x != 0) && (safe_div(10, x) > 0);
    if (safe != 0) return 1;
    if (div_counter != 0) return 2;  // Division NOT attempted

    // Test 2: Safe dereference pattern
    int arr[3] = {1, 2, 3};
    int *ptr = 0;
    int valid = (ptr != 0) && (*ptr > 0);
    if (valid != 0) return 3;

    // Test 3: Null check with ||
    ptr = &arr[0];
    valid = (ptr == 0) || (*ptr > 0);
    if (valid != 1) return 4;

    // Test 5: Short-circuit with all operands evaluated
    int a = 1, b = 2, c = 0;
    int result = (a && b && c);  // Should be 0, all evaluated up to c
    if (result != 0) return 5;

    // Test 6: Complex boolean with multiple short-circuits
    int p = 1, q = 0, r = 1, s = 0;
    result = ((p && q) || (r && s));
    // (1 && 0) = 0, so continue; (1 && 0) = 0; result = 0
    if (result != 0) return 6;

    result = ((p && r) || (q && s));
    // (1 && 1) = 1, short-circuit ||; result = 1
    if (result != 1) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("sc_edge_cases", code), 0);
}
