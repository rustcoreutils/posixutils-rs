//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 mixed-type comparisons (usual arithmetic conversions)
// Tests are aggregated by category to reduce compile/link overhead
//

use crate::common::compile_and_run;

// ============================================================================
// Integer Promotions and Same-Rank Different-Sign Conversions
// ============================================================================

#[test]
fn mixed_cmp_promotions_and_same_rank() {
    let code = r#"
int main(void) {
    // Test 1-4: signed char vs unsigned char (both promoted to int)
    {
        signed char sc = -1;
        unsigned char uc = 1;
        if ((sc < uc) != 1) return 1;
        if ((sc > uc) != 0) return 2;
        if ((sc == uc) != 0) return 3;
        if ((sc != uc) != 1) return 4;
    }

    // Test 5-8: short vs unsigned short (both promoted to int)
    {
        short ss = -1;
        unsigned short us = 1;
        if ((ss < us) != 1) return 5;
        if ((ss > us) != 0) return 6;
        if ((ss == us) != 0) return 7;
        if ((ss != us) != 1) return 8;
    }

    // Test 9-10: signed char vs short
    {
        signed char sc = -10;
        short ss = 5;
        if ((sc < ss) != 1) return 9;
        if ((sc > ss) != 0) return 10;
    }

    // Test 11-12: unsigned char vs signed short
    {
        unsigned char uc = 200;
        short ss = 100;
        if ((uc > ss) != 1) return 11;
        if ((uc < ss) != 0) return 12;
    }

    // Test 13-14: _Bool comparisons (promoted to int)
    {
        _Bool b = 1;
        char c = 2;
        if ((b < c) != 1) return 13;
        if ((b == c) != 0) return 14;
    }

    // Test 15-17: int vs unsigned int (convert to unsigned)
    // -1 becomes 0xFFFFFFFF, so -1 > 1 as unsigned
    {
        int si = -1;
        unsigned int ui = 1;
        if ((si < ui) != 0) return 15;   // -1 as unsigned is large
        if ((si > ui) != 1) return 16;
        if ((si == ui) != 0) return 17;
    }

    // Test 18-19: long vs unsigned long
    {
        long sl = -1;
        unsigned long ul = 1;
        if ((sl < ul) != 0) return 18;
        if ((sl > ul) != 1) return 19;
    }

    // Test 20-21: long long vs unsigned long long
    {
        long long sll = -1;
        unsigned long long ull = 1;
        if ((sll < ull) != 0) return 20;
        if ((sll > ull) != 1) return 21;
    }

    // Test 22-23: Positive values work normally
    {
        int si = 5;
        unsigned int ui = 10;
        if ((si < ui) != 1) return 22;
        if ((si > ui) != 0) return 23;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("mixed_cmp_promo", code), 0);
}

// ============================================================================
// Different Ranks and Float vs Integer
// ============================================================================

#[test]
fn mixed_cmp_different_ranks_and_float() {
    let code = r#"
int main(void) {
    // Test 1-2: long vs unsigned int (long can represent all unsigned int)
    {
        long sl = -1;
        unsigned int ui = 1;
        if ((sl < ui) != 1) return 1;   // -1 < 1 as signed
        if ((sl > ui) != 0) return 2;
    }

    // Test 3-4: long long vs unsigned int
    {
        long long sll = -1;
        unsigned int ui = 1;
        if ((sll < ui) != 1) return 3;
        if ((sll > ui) != 0) return 4;
    }

    // Test 5-6: int vs unsigned short
    {
        int si = -1;
        unsigned short us = 1;
        if ((si < us) != 1) return 5;
        if ((si > us) != 0) return 6;
    }

    // Test 7-8: long vs short
    {
        long sl = -1000000;
        short ss = 1;
        if ((sl < ss) != 1) return 7;
        if ((sl > ss) != 0) return 8;
    }

    // Test 9-11: int vs float
    {
        int i = -5;
        float f = 2.5f;
        if ((i < f) != 1) return 9;
        if ((i > f) != 0) return 10;
        if ((i == f) != 0) return 11;
    }

    // Test 12-13: unsigned int vs float
    {
        unsigned int ui = 10;
        float f = 5.5f;
        if ((ui > f) != 1) return 12;
        if ((ui < f) != 0) return 13;
    }

    // Test 14-15: int vs double
    {
        int i = 42;
        double d = 42.0;
        if ((i == d) != 1) return 14;
        if ((i != d) != 0) return 15;
    }

    // Test 16: long vs double
    {
        long l = -100;
        double d = -50.5;
        if ((l < d) != 1) return 16;
    }

    // Test 17-18: float vs double
    {
        float f = 1.5f;
        double d = 2.5;
        if ((f < d) != 1) return 17;
        if ((f > d) != 0) return 18;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("mixed_cmp_ranks_float", code), 0);
}

// ============================================================================
// Edge Cases and All Relational Operators
// ============================================================================

#[test]
fn mixed_cmp_edge_and_relational() {
    let code = r#"
int main(void) {
    // Test 1-2: Maximum unsigned char vs signed
    {
        unsigned char uc = 255;
        signed char sc = -1;
        if ((uc > sc) != 1) return 1;
        if ((uc == sc) != 0) return 2;
    }

    // Test 3-5: Zero comparisons
    {
        int si = 0;
        unsigned int ui = 0;
        float f = 0.0f;
        if ((si == ui) != 1) return 3;
        if ((si == f) != 1) return 4;
        if ((ui == f) != 1) return 5;
    }

    // Test 6: Large unsigned vs negative signed (same rank)
    {
        unsigned int ui = 0xFFFFFFFF;
        int si = -1;
        // Both become unsigned, -1 becomes 0xFFFFFFFF
        if ((ui == si) != 1) return 6;
    }

    // Test 7-8: Comparison result is int (0 or 1)
    {
        char a = 10;
        char b = 5;
        int result = (a > b);
        if (result != 1) return 7;
        result = (a < b);
        if (result != 0) return 8;
    }

    // Test 9-10: Chained comparisons
    {
        char c = 5;
        int i = 10;
        long l = 15;
        if ((c < i) != 1) return 9;
        if ((i < l) != 1) return 10;
    }

    // Test 11-14: All relational operators with char vs int
    {
        char c = -10;
        int i = 5;
        if ((c < i) != 1) return 11;
        if ((c <= i) != 1) return 12;
        if ((c > i) != 0) return 13;
        if ((c >= i) != 0) return 14;
    }

    // Test 15-18: All relational with int vs unsigned int
    {
        int si = -1;
        unsigned int ui = 1;
        if ((si < ui) != 0) return 15;  // -1 as unsigned is MAX
        if ((si <= ui) != 0) return 16;
        if ((si > ui) != 1) return 17;
        if ((si >= ui) != 1) return 18;
    }

    // Test 19-22: All relational with int vs float
    {
        int i = 3;
        float f = 3.5f;
        if ((i < f) != 1) return 19;
        if ((i <= f) != 1) return 20;
        if ((i > f) != 0) return 21;
        if ((i >= f) != 0) return 22;
    }

    // Test 23-24: Equality operators
    {
        short s = 100;
        long l = 100;
        if ((s == l) != 1) return 23;
        if ((s != l) != 0) return 24;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("mixed_cmp_edge", code), 0);
}
