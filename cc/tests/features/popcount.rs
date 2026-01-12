//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for population count builtins (__builtin_popcount/popcountl/popcountll)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// __builtin_popcount: All 32-bit population count tests in one binary
// ============================================================================

#[test]
fn popcount_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Zero has no bits set
    unsigned int val = 0;
    int result = __builtin_popcount(val);
    if (result != 0) return 1;

    // Test 2: Single bit set
    val = 1;
    result = __builtin_popcount(val);
    if (result != 1) return 2;

    // Test 3: Two bits set (0b11 = 3)
    val = 3;
    result = __builtin_popcount(val);
    if (result != 2) return 3;

    // Test 4: All bits set (0xFFFFFFFF)
    val = 0xFFFFFFFF;
    result = __builtin_popcount(val);
    if (result != 32) return 4;

    // Test 5: Alternating bits (0xAAAAAAAA)
    val = 0xAAAAAAAA;
    result = __builtin_popcount(val);
    if (result != 16) return 5;

    // Test 6: Constant argument
    result = __builtin_popcount(0x12345678);  // count bits in hex value
    // 0x12345678 = 0001 0010 0011 0100 0101 0110 0111 1000
    // = 1 + 1 + 2 + 1 + 2 + 2 + 3 + 1 = 13
    if (result != 13) return 6;

    // Test 7: Power of 2
    val = 0x80000000;
    result = __builtin_popcount(val);
    if (result != 1) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("popcount_comprehensive", code, &[]), 0);
}

// ============================================================================
// __builtin_popcountl: All long (64-bit on LP64) population count tests
// ============================================================================

#[test]
fn popcountl_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Zero
    unsigned long val = 0UL;
    int result = __builtin_popcountl(val);
    if (result != 0) return 1;

    // Test 2: Single bit set
    val = 1UL;
    result = __builtin_popcountl(val);
    if (result != 1) return 2;

    // Test 3: All bits set
    val = 0xFFFFFFFFFFFFFFFFUL;
    result = __builtin_popcountl(val);
    if (result != 64) return 3;

    // Test 4: Alternating bits
    val = 0xAAAAAAAAAAAAAAAAUL;
    result = __builtin_popcountl(val);
    if (result != 32) return 4;

    // Test 5: High bit only
    val = 1UL << 63;
    result = __builtin_popcountl(val);
    if (result != 1) return 5;

    // Test 6: Lower 32 bits all set
    val = 0xFFFFFFFFUL;
    result = __builtin_popcountl(val);
    if (result != 32) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("popcountl_comprehensive", code, &[]), 0);
}

// ============================================================================
// __builtin_popcountll: All long long (64-bit) population count tests
// ============================================================================

#[test]
fn popcountll_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Zero
    unsigned long long val = 0ULL;
    int result = __builtin_popcountll(val);
    if (result != 0) return 1;

    // Test 2: Single bit set
    val = 1ULL;
    result = __builtin_popcountll(val);
    if (result != 1) return 2;

    // Test 3: All bits set
    val = 0xFFFFFFFFFFFFFFFFULL;
    result = __builtin_popcountll(val);
    if (result != 64) return 3;

    // Test 4: Alternating bits (0x5555...)
    val = 0x5555555555555555ULL;
    result = __builtin_popcountll(val);
    if (result != 32) return 4;

    // Test 5: High bit only
    val = 1ULL << 63;
    result = __builtin_popcountll(val);
    if (result != 1) return 5;

    // Test 6: Every byte has one bit set (0x0101010101010101)
    val = 0x0101010101010101ULL;
    result = __builtin_popcountll(val);
    if (result != 8) return 6;

    // Test 7: Constant argument
    result = __builtin_popcountll(0x123456789ABCDEF0ULL);
    // Manual count of set bits: 32
    if (result != 32) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("popcountll_comprehensive", code, &[]), 0);
}
