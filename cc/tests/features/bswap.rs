//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for byte-swapping builtins (__builtin_bswap16/32/64)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// __builtin_bswap16: All 16-bit byte swap tests in one binary
// ============================================================================

#[test]
fn bswap16_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Basic swap (0xABCD -> 0xCDAB)
    unsigned short val = 0xABCD;
    unsigned short result = __builtin_bswap16(val);
    if (result != 0xCDAB) return 1;

    // Test 2: Identity - swapping twice gives original value
    val = 0x1234;
    result = __builtin_bswap16(__builtin_bswap16(val));
    if (result != val) return 2;

    // Test 3: Constant argument
    result = __builtin_bswap16(0xFF00);
    if (result != 0x00FF) return 3;

    // Test 4: Zero and max values
    if (__builtin_bswap16(0x0000) != 0x0000) return 4;
    if (__builtin_bswap16(0xFFFF) != 0xFFFF) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bswap16_comprehensive", code), 0);
}

// ============================================================================
// __builtin_bswap32: All 32-bit byte swap tests in one binary
// ============================================================================

#[test]
fn bswap32_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Basic swap (0x12345678 -> 0x78563412)
    unsigned int val = 0x12345678;
    unsigned int result = __builtin_bswap32(val);
    if (result != 0x78563412) return 1;

    // Test 2: Identity - swapping twice gives original value
    val = 0xDEADBEEF;
    result = __builtin_bswap32(__builtin_bswap32(val));
    if (result != val) return 2;

    // Test 3: Constant argument
    result = __builtin_bswap32(0xFF000000);
    if (result != 0x000000FF) return 3;

    // Test 4: Zero and max values
    if (__builtin_bswap32(0x00000000) != 0x00000000) return 4;
    if (__builtin_bswap32(0xFFFFFFFF) != 0xFFFFFFFF) return 5;

    // Test 5: Network byte order conversion
    unsigned int network_val = 0x01020304;  // big-endian: 1.2.3.4
    unsigned int host_val = __builtin_bswap32(network_val);
    if (host_val != 0x04030201) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bswap32_comprehensive", code), 0);
}

// ============================================================================
// __builtin_bswap64: All 64-bit byte swap tests in one binary
// ============================================================================

#[test]
fn bswap64_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Basic swap (0x0102030405060708 -> 0x0807060504030201)
    unsigned long long val = 0x0102030405060708ULL;
    unsigned long long result = __builtin_bswap64(val);
    if (result != 0x0807060504030201ULL) return 1;

    // Test 2: Identity - swapping twice gives original value
    val = 0xDEADBEEFCAFEBABEULL;
    result = __builtin_bswap64(__builtin_bswap64(val));
    if (result != val) return 2;

    // Test 3: Constant argument
    result = __builtin_bswap64(0xFF00000000000000ULL);
    if (result != 0x00000000000000FFULL) return 3;

    // Test 4: Zero and max values
    if (__builtin_bswap64(0x0000000000000000ULL) != 0x0000000000000000ULL) return 4;
    if (__builtin_bswap64(0xFFFFFFFFFFFFFFFFULL) != 0xFFFFFFFFFFFFFFFFULL) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bswap64_comprehensive", code), 0);
}
