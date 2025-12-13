//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for count leading zeros builtins (__builtin_clz/clzl/clzll)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// __builtin_clz: All 32-bit count leading zeros tests in one binary
// ============================================================================

#[test]
fn clz_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: High bit set (0x80000000)
    unsigned int val = 0x80000000;
    int result = __builtin_clz(val);
    if (result != 0) return 1;

    // Test 2: Second highest bit set (0x40000000)
    val = 0x40000000;
    result = __builtin_clz(val);
    if (result != 1) return 2;

    // Test 3: Lowest bit set (0x1)
    val = 1;
    result = __builtin_clz(val);
    if (result != 31) return 3;

    // Test 4: Power of 2 (0x100 = 256)
    val = 256;
    result = __builtin_clz(val);
    if (result != 23) return 4;  // 31 - 8 = 23

    // Test 5: Constant argument
    result = __builtin_clz(16);  // 0b10000, highest set bit at position 4
    if (result != 27) return 5;  // 31 - 4 = 27

    // Test 6: All ones (0xFFFFFFFF)
    val = 0xFFFFFFFF;
    result = __builtin_clz(val);
    if (result != 0) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("clz_comprehensive", code), 0);
}

// ============================================================================
// __builtin_clzl: All long (64-bit on LP64) count leading zeros tests
// ============================================================================

#[test]
fn clzl_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: High bit set (2^63)
    unsigned long val = 1UL << 63;
    int result = __builtin_clzl(val);
    if (result != 0) return 1;

    // Test 2: Second highest bit set (2^62)
    val = 1UL << 62;
    result = __builtin_clzl(val);
    if (result != 1) return 2;

    // Test 3: Lowest bit set
    val = 1UL;
    result = __builtin_clzl(val);
    if (result != 63) return 3;

    // Test 4: Bit 40 set
    val = 1UL << 40;
    result = __builtin_clzl(val);
    if (result != 23) return 4;  // 63 - 40 = 23

    // Test 5: Constant argument
    result = __builtin_clzl(256UL);  // 2^8
    if (result != 55) return 5;  // 63 - 8 = 55

    return 0;
}
"#;
    assert_eq!(compile_and_run("clzl_comprehensive", code), 0);
}

// ============================================================================
// __builtin_clzll: All long long (64-bit) count leading zeros tests
// ============================================================================

#[test]
fn clzll_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: High bit set (2^63)
    unsigned long long val = 1ULL << 63;
    int result = __builtin_clzll(val);
    if (result != 0) return 1;

    // Test 2: Second highest bit set (2^62)
    val = 1ULL << 62;
    result = __builtin_clzll(val);
    if (result != 1) return 2;

    // Test 3: Lowest bit set
    val = 1ULL;
    result = __builtin_clzll(val);
    if (result != 63) return 3;

    // Test 4: Bit 50 set
    val = 1ULL << 50;
    result = __builtin_clzll(val);
    if (result != 13) return 4;  // 63 - 50 = 13

    // Test 5: Constant argument
    result = __builtin_clzll(4096ULL);  // 2^12
    if (result != 51) return 5;  // 63 - 12 = 51

    // Test 6: Large value with high bits set
    val = 0xFFFF000000000000ULL;
    result = __builtin_clzll(val);
    if (result != 0) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("clzll_comprehensive", code), 0);
}
