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
// C99 Integer Promotions (6.3.1.1) - char/short promoted to int before comparison
// ============================================================================

#[test]
fn mixed_cmp_integer_promotions() {
    let code = r#"
int main(void) {
    // Test 1: signed char vs unsigned char
    // Both promoted to int, then signed comparison
    // -1 < 1 is true (1)
    {
        signed char sc = -1;
        unsigned char uc = 1;
        if ((sc < uc) != 1) return 1;
        if ((sc > uc) != 0) return 2;
        if ((sc == uc) != 0) return 3;
        if ((sc != uc) != 1) return 4;
    }

    // Test 2: short vs unsigned short
    // Both promoted to int, then signed comparison
    // -1 < 1 is true (1)
    {
        short ss = -1;
        unsigned short us = 1;
        if ((ss < us) != 1) return 11;
        if ((ss > us) != 0) return 12;
        if ((ss == us) != 0) return 13;
        if ((ss != us) != 1) return 14;
    }

    // Test 3: signed char vs short
    // Both promoted to int
    {
        signed char sc = -10;
        short ss = 5;
        if ((sc < ss) != 1) return 21;
        if ((sc > ss) != 0) return 22;
    }

    // Test 4: unsigned char vs signed short
    // Both promoted to int
    {
        unsigned char uc = 200;
        short ss = 100;
        if ((uc > ss) != 1) return 31;
        if ((uc < ss) != 0) return 32;
    }

    // Test 5: _Bool comparisons (promoted to int)
    {
        _Bool b = 1;
        char c = 2;
        if ((b < c) != 1) return 41;
        if ((b == c) != 0) return 42;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("mixed_cmp_int_promo", code), 0);
}

// ============================================================================
// C99 Usual Arithmetic Conversions (6.3.1.8) - same rank, different signedness
// ============================================================================

#[test]
fn mixed_cmp_same_rank_different_sign() {
    let code = r#"
int main(void) {
    // Test 1: int vs unsigned int
    // C99: convert to unsigned int
    // -1 becomes 0xFFFFFFFF, so -1 > 1 as unsigned
    {
        int si = -1;
        unsigned int ui = 1;
        if ((si < ui) != 0) return 1;   // -1 as unsigned is large
        if ((si > ui) != 1) return 2;
        if ((si == ui) != 0) return 3;
    }

    // Test 2: long vs unsigned long
    // C99: convert to unsigned long
    {
        long sl = -1;
        unsigned long ul = 1;
        if ((sl < ul) != 0) return 11;  // -1 as unsigned is large
        if ((sl > ul) != 1) return 12;
    }

    // Test 3: long long vs unsigned long long
    {
        long long sll = -1;
        unsigned long long ull = 1;
        if ((sll < ull) != 0) return 21;
        if ((sll > ull) != 1) return 22;
    }

    // Test 4: Positive values should work normally
    {
        int si = 5;
        unsigned int ui = 10;
        if ((si < ui) != 1) return 31;
        if ((si > ui) != 0) return 32;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("mixed_cmp_same_rank", code), 0);
}

// ============================================================================
// C99 Usual Arithmetic Conversions - different ranks
// ============================================================================

#[test]
fn mixed_cmp_different_ranks() {
    let code = r#"
int main(void) {
    // Test 1: long vs unsigned int
    // long (64-bit) can represent all unsigned int values
    // So convert both to long (signed comparison)
    {
        long sl = -1;
        unsigned int ui = 1;
        if ((sl < ui) != 1) return 1;   // -1 < 1 as signed
        if ((sl > ui) != 0) return 2;
    }

    // Test 2: long long vs unsigned int
    // long long can represent all unsigned int values
    {
        long long sll = -1;
        unsigned int ui = 1;
        if ((sll < ui) != 1) return 11;
        if ((sll > ui) != 0) return 12;
    }

    // Test 3: int vs unsigned short
    // int can represent all unsigned short values
    {
        int si = -1;
        unsigned short us = 1;
        if ((si < us) != 1) return 21;  // signed comparison after promotion
        if ((si > us) != 0) return 22;
    }

    // Test 4: long vs short
    // Both signed, long is wider
    {
        long sl = -1000000;
        short ss = 1;
        if ((sl < ss) != 1) return 31;
        if ((sl > ss) != 0) return 32;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("mixed_cmp_diff_ranks", code), 0);
}

// ============================================================================
// Float vs Integer comparisons
// ============================================================================

#[test]
fn mixed_cmp_float_int() {
    let code = r#"
int main(void) {
    // Test 1: int vs float
    {
        int i = -5;
        float f = 2.5f;
        if ((i < f) != 1) return 1;
        if ((i > f) != 0) return 2;
        if ((i == f) != 0) return 3;
    }

    // Test 2: unsigned int vs float
    {
        unsigned int ui = 10;
        float f = 5.5f;
        if ((ui > f) != 1) return 11;
        if ((ui < f) != 0) return 12;
    }

    // Test 3: int vs double
    {
        int i = 42;
        double d = 42.0;
        if ((i == d) != 1) return 21;
        if ((i != d) != 0) return 22;
    }

    // Test 4: long vs double
    {
        long l = -100;
        double d = -50.5;
        if ((l < d) != 1) return 31;
    }

    // Test 5: float vs double
    {
        float f = 1.5f;
        double d = 2.5;
        if ((f < d) != 1) return 41;
        if ((f > d) != 0) return 42;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("mixed_cmp_float_int", code), 0);
}

// ============================================================================
// Edge cases and boundary values
// ============================================================================

#[test]
fn mixed_cmp_edge_cases() {
    let code = r#"
int main(void) {
    // Test 1: Maximum unsigned char vs signed comparison
    {
        unsigned char uc = 255;
        signed char sc = -1;
        // Both promoted to int: 255 vs -1
        if ((uc > sc) != 1) return 1;
        if ((uc == sc) != 0) return 2;
    }

    // Test 2: Zero comparisons across types
    {
        int si = 0;
        unsigned int ui = 0;
        float f = 0.0f;
        if ((si == ui) != 1) return 11;
        if ((si == f) != 1) return 12;
        if ((ui == f) != 1) return 13;
    }

    // Test 3: Large unsigned vs negative signed (same rank)
    {
        unsigned int ui = 0xFFFFFFFF;
        int si = -1;
        // Both become unsigned, -1 becomes 0xFFFFFFFF
        if ((ui == si) != 1) return 21;
    }

    // Test 4: Comparison result is always int (0 or 1)
    {
        char a = 10;
        char b = 5;
        int result = (a > b);
        if (result != 1) return 31;
        result = (a < b);
        if (result != 0) return 32;
    }

    // Test 5: Chained comparisons with mixed types
    {
        char c = 5;
        int i = 10;
        long l = 15;
        // (c < i) is 1, (i < l) is 1
        if ((c < i) != 1) return 41;
        if ((i < l) != 1) return 42;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("mixed_cmp_edge", code), 0);
}

// ============================================================================
// Relational operators (<, <=, >, >=) with all combinations
// ============================================================================

#[test]
fn mixed_cmp_relational_all() {
    let code = r#"
int main(void) {
    // Test all relational operators with char vs int
    {
        char c = -10;
        int i = 5;
        if ((c < i) != 1) return 1;
        if ((c <= i) != 1) return 2;
        if ((c > i) != 0) return 3;
        if ((c >= i) != 0) return 4;
    }

    // Test all relational operators with int vs unsigned int
    {
        int si = -1;
        unsigned int ui = 1;
        // -1 as unsigned is MAX_UINT, so -1 > 1
        if ((si < ui) != 0) return 11;
        if ((si <= ui) != 0) return 12;
        if ((si > ui) != 1) return 13;
        if ((si >= ui) != 1) return 14;
    }

    // Test all relational operators with int vs float
    {
        int i = 3;
        float f = 3.5f;
        if ((i < f) != 1) return 21;
        if ((i <= f) != 1) return 22;
        if ((i > f) != 0) return 23;
        if ((i >= f) != 0) return 24;
    }

    // Test all equality operators
    {
        short s = 100;
        long l = 100;
        if ((s == l) != 1) return 31;
        if ((s != l) != 0) return 32;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("mixed_cmp_relational", code), 0);
}
