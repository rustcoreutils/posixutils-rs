//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Bit Operations Builtins Mega-Test
//
// Consolidates: clz, ctz, popcount, bswap tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: Bit operation builtins
// ============================================================================

#[test]
fn builtins_bit_ops_mega() {
    let code = r#"
int main(void) {
    // ========== CLZ (count leading zeros) (returns 1-29) ==========
    {
        unsigned int val;
        int result;

        // 32-bit clz
        val = 0x80000000;  // High bit set
        result = __builtin_clz(val);
        if (result != 0) return 1;

        val = 0x40000000;  // Second highest bit
        result = __builtin_clz(val);
        if (result != 1) return 2;

        val = 1;  // Lowest bit
        result = __builtin_clz(val);
        if (result != 31) return 3;

        val = 256;  // 2^8
        result = __builtin_clz(val);
        if (result != 23) return 4;

        result = __builtin_clz(16);  // constant
        if (result != 27) return 5;

        val = 0xFFFFFFFF;
        result = __builtin_clz(val);
        if (result != 0) return 6;

        // 64-bit clzl
        unsigned long lval = 1UL << 63;
        result = __builtin_clzl(lval);
        if (result != 0) return 7;

        lval = 1UL << 62;
        result = __builtin_clzl(lval);
        if (result != 1) return 8;

        lval = 1UL;
        result = __builtin_clzl(lval);
        if (result != 63) return 9;

        // 64-bit clzll
        unsigned long long llval = 1ULL << 63;
        result = __builtin_clzll(llval);
        if (result != 0) return 10;

        llval = 1ULL;
        result = __builtin_clzll(llval);
        if (result != 63) return 11;
    }

    // ========== CTZ (count trailing zeros) (returns 30-59) ==========
    {
        unsigned int val;
        int result;

        // 32-bit ctz
        val = 1;
        result = __builtin_ctz(val);
        if (result != 0) return 30;

        val = 2;
        result = __builtin_ctz(val);
        if (result != 1) return 31;

        val = 0x80000000;
        result = __builtin_ctz(val);
        if (result != 31) return 32;

        val = 256;  // 2^8
        result = __builtin_ctz(val);
        if (result != 8) return 33;

        val = 0xFFFFFFFF;
        result = __builtin_ctz(val);
        if (result != 0) return 34;

        result = __builtin_ctz(16);  // constant
        if (result != 4) return 35;

        // 64-bit ctzl
        unsigned long lval = 1UL;
        result = __builtin_ctzl(lval);
        if (result != 0) return 36;

        lval = 1UL << 63;
        result = __builtin_ctzl(lval);
        if (result != 63) return 37;

        lval = 1UL << 40;
        result = __builtin_ctzl(lval);
        if (result != 40) return 38;

        // 64-bit ctzll
        unsigned long long llval = 1ULL;
        result = __builtin_ctzll(llval);
        if (result != 0) return 39;

        llval = 1ULL << 63;
        result = __builtin_ctzll(llval);
        if (result != 63) return 40;
    }

    // ========== POPCOUNT (population count) (returns 60-89) ==========
    {
        unsigned int val;
        int result;

        // 32-bit popcount
        val = 0;
        result = __builtin_popcount(val);
        if (result != 0) return 60;

        val = 1;
        result = __builtin_popcount(val);
        if (result != 1) return 61;

        val = 3;  // 0b11
        result = __builtin_popcount(val);
        if (result != 2) return 62;

        val = 0xFFFFFFFF;
        result = __builtin_popcount(val);
        if (result != 32) return 63;

        val = 0xAAAAAAAA;  // alternating bits
        result = __builtin_popcount(val);
        if (result != 16) return 64;

        val = 0x80000000;  // power of 2
        result = __builtin_popcount(val);
        if (result != 1) return 65;

        // 64-bit popcountl
        unsigned long lval = 0UL;
        result = __builtin_popcountl(lval);
        if (result != 0) return 66;

        lval = 0xFFFFFFFFFFFFFFFFUL;
        result = __builtin_popcountl(lval);
        if (result != 64) return 67;

        lval = 0xAAAAAAAAAAAAAAAAUL;
        result = __builtin_popcountl(lval);
        if (result != 32) return 68;

        // 64-bit popcountll
        unsigned long long llval = 0ULL;
        result = __builtin_popcountll(llval);
        if (result != 0) return 69;

        llval = 0xFFFFFFFFFFFFFFFFULL;
        result = __builtin_popcountll(llval);
        if (result != 64) return 70;

        llval = 0x5555555555555555ULL;
        result = __builtin_popcountll(llval);
        if (result != 32) return 71;
    }

    // ========== BSWAP (byte swap) (returns 90-109) ==========
    {
        // 32-bit bswap
        unsigned int val = 0x12345678;
        unsigned int swapped = __builtin_bswap32(val);
        if (swapped != 0x78563412) return 90;

        val = 0x01020304;
        swapped = __builtin_bswap32(val);
        if (swapped != 0x04030201) return 91;

        val = 0;
        swapped = __builtin_bswap32(val);
        if (swapped != 0) return 92;

        val = 0xFFFFFFFF;
        swapped = __builtin_bswap32(val);
        if (swapped != 0xFFFFFFFF) return 93;

        // 64-bit bswap
        unsigned long long llval = 0x0102030405060708ULL;
        unsigned long long llswapped = __builtin_bswap64(llval);
        if (llswapped != 0x0807060504030201ULL) return 94;

        llval = 0x123456789ABCDEF0ULL;
        llswapped = __builtin_bswap64(llval);
        if (llswapped != 0xF0DEBC9A78563412ULL) return 95;

        llval = 0;
        llswapped = __builtin_bswap64(llval);
        if (llswapped != 0) return 96;

        // 16-bit bswap
        unsigned short sval = 0x1234;
        unsigned short sswapped = __builtin_bswap16(sval);
        if (sswapped != 0x3412) return 97;

        sval = 0x0102;
        sswapped = __builtin_bswap16(sval);
        if (sswapped != 0x0201) return 98;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_bit_ops_mega", code, &[]), 0);
}
