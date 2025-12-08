//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for count trailing zeros builtins (__builtin_ctz/ctzl/ctzll)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// __builtin_ctz: All 32-bit count trailing zeros tests in one binary
// ============================================================================

#[test]
fn ctz_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Single trailing zero (0b10 = 2)
    unsigned int val = 2;
    int result = __builtin_ctz(val);
    if (result != 1) return 1;

    // Test 2: Multiple trailing zeros (0b1000 = 8)
    val = 8;
    result = __builtin_ctz(val);
    if (result != 3) return 2;

    // Test 3: No trailing zeros (0b1 = 1)
    val = 1;
    result = __builtin_ctz(val);
    if (result != 0) return 3;

    // Test 4: Power of 2 (0x80000000 = 2^31)
    val = 0x80000000;
    result = __builtin_ctz(val);
    if (result != 31) return 4;

    // Test 5: Constant argument
    result = __builtin_ctz(16);  // 0b10000
    if (result != 4) return 5;

    // Test 6: Mixed bits (0b101000 = 40)
    val = 40;
    result = __builtin_ctz(val);
    if (result != 3) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ctz_comprehensive", code), 0);
}

// ============================================================================
// __builtin_ctzl: All long (64-bit on LP64) count trailing zeros tests
// ============================================================================

#[test]
fn ctzl_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Single trailing zero
    unsigned long val = 2UL;
    int result = __builtin_ctzl(val);
    if (result != 1) return 1;

    // Test 2: Large power of 2 (2^40)
    val = 1UL << 40;
    result = __builtin_ctzl(val);
    if (result != 40) return 2;

    // Test 3: No trailing zeros
    val = 1UL;
    result = __builtin_ctzl(val);
    if (result != 0) return 3;

    // Test 4: High bit only (2^63)
    val = 1UL << 63;
    result = __builtin_ctzl(val);
    if (result != 63) return 4;

    // Test 5: Constant argument
    result = __builtin_ctzl(256UL);  // 2^8
    if (result != 8) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ctzl_comprehensive", code), 0);
}

// ============================================================================
// __builtin_ctzll: All long long (64-bit) count trailing zeros tests
// ============================================================================

#[test]
fn ctzll_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Single trailing zero
    unsigned long long val = 2ULL;
    int result = __builtin_ctzll(val);
    if (result != 1) return 1;

    // Test 2: Large power of 2 (2^50)
    val = 1ULL << 50;
    result = __builtin_ctzll(val);
    if (result != 50) return 2;

    // Test 3: No trailing zeros
    val = 1ULL;
    result = __builtin_ctzll(val);
    if (result != 0) return 3;

    // Test 4: High bit only (2^63)
    val = 1ULL << 63;
    result = __builtin_ctzll(val);
    if (result != 63) return 4;

    // Test 5: Constant argument
    result = __builtin_ctzll(4096ULL);  // 2^12
    if (result != 12) return 5;

    // Test 6: Large value with some trailing zeros
    val = 0xFFFF000000000000ULL;  // 48 trailing zeros
    result = __builtin_ctzll(val);
    if (result != 48) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ctzll_comprehensive", code), 0);
}
