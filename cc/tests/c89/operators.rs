//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89 Operators Mega-Test
//
// Consolidates: bitfield, mixed_cmp, ops_struct, short_circuit tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: C89 operators (bitfield, mixed_cmp, ops_struct, short_circuit)
// ============================================================================

#[test]
fn c89_operators_mega() {
    let code = r#"
// Struct for ops_struct tests
struct Ops {
    int a;
    int b;
};

int add_struct(struct Ops o) { return o.a + o.b; }
struct Ops make_ops(int a, int b) {
    struct Ops o; o.a = a; o.b = b; return o;
}

// Global for side effect tracking
int side_effect_count;
int has_side_effect(int val) { side_effect_count++; return val; }

int main(void) {
    // ========== BITFIELD SECTION (returns 1-29) ==========
    {
        struct Bits {
            unsigned int a : 3;  // 0-7
            unsigned int b : 5;  // 0-31
            unsigned int c : 8;  // 0-255
            int d : 4;           // -8 to 7 (signed)
        };

        struct Bits bits;

        // Basic assignment
        bits.a = 5;
        if (bits.a != 5) return 1;

        bits.b = 20;
        if (bits.b != 20) return 2;

        bits.c = 200;
        if (bits.c != 200) return 3;

        // Signed bitfield
        bits.d = -4;
        if (bits.d != -4) return 4;

        bits.d = 7;
        if (bits.d != 7) return 5;

        // Overflow wrapping (unsigned)
        bits.a = 10;  // Truncates to 3 bits: 10 & 7 = 2
        if (bits.a != 2) return 6;

        // Bitfield arithmetic
        bits.a = 3;
        bits.b = 10;
        int sum = bits.a + bits.b;
        if (sum != 13) return 7;

        // Bitfield comparison
        bits.a = 5;
        bits.b = 5;
        if ((bits.a == bits.b) != 1) return 8;

        // Bitfield in expression
        bits.a = 2;
        bits.b = 3;
        if (bits.a * bits.b != 6) return 9;

        // Adjacent bitfields
        struct Adjacent {
            unsigned int x : 4;
            unsigned int y : 4;
        };
        struct Adjacent adj;
        adj.x = 10;
        adj.y = 5;
        if (adj.x != 10) return 10;
        if (adj.y != 5) return 11;

        // Single-bit bitfield
        struct SingleBit {
            unsigned int flag : 1;
        };
        struct SingleBit sb;
        sb.flag = 1;
        if (sb.flag != 1) return 12;
        sb.flag = 0;
        if (sb.flag != 0) return 13;
    }

    // ========== MIXED COMPARISON SECTION (returns 30-59) ==========
    {
        // int vs char
        int i = 100;
        char c = 100;
        if ((i == c) != 1) return 30;
        if ((i != c) != 0) return 31;

        // int vs short
        short s = 1000;
        i = 1000;
        if ((i == s) != 1) return 32;
        if ((i < s) != 0) return 33;

        // int vs long
        long l = 1000000L;
        i = 1000000;
        if ((i == l) != 1) return 34;

        // signed vs unsigned
        int si = -1;
        unsigned int ui = 0xFFFFFFFF;
        // -1 as unsigned is 0xFFFFFFFF
        if ((si == (int)ui) != 1) return 35;

        // char vs int arithmetic
        c = 10;
        i = 32;
        if (c + i != 42) return 36;

        // short vs long arithmetic
        s = 100;
        l = 900L;
        if (s + l != 1000L) return 37;

        // Mixed type comparison chains
        c = 5; s = 10; i = 15;
        if (!((c < s) && (s < i))) return 38;

        // Float vs int
        float f = 42.0f;
        i = 42;
        if ((f == (float)i) != 1) return 39;

        // Double vs int
        double d = 100.0;
        i = 100;
        if ((d == (double)i) != 1) return 40;
    }

    // ========== STRUCT OPERATORS SECTION (returns 60-79) ==========
    {
        struct Ops o1, o2;

        // Struct assignment
        o1.a = 10; o1.b = 20;
        o2 = o1;
        if (o2.a != 10) return 60;
        if (o2.b != 20) return 61;

        // Independent copy
        o1.a = 100;
        if (o2.a != 10) return 62;

        // Struct as function arg
        struct Ops o3;
        o3.a = 15; o3.b = 27;
        if (add_struct(o3) != 42) return 63;

        // Struct as function return
        struct Ops o4 = make_ops(3, 4);
        if (o4.a != 3) return 64;
        if (o4.b != 4) return 65;

        // Member increment
        o1.a = 10;
        o1.a++;
        if (o1.a != 11) return 66;

        ++o1.a;
        if (o1.a != 12) return 67;

        // Member compound assignment
        o1.a = 10;
        o1.a += 5;
        if (o1.a != 15) return 68;

        o1.b = 20;
        o1.b *= 2;
        if (o1.b != 40) return 69;

        // Pointer to member
        struct Ops *p = &o1;
        p->a = 50;
        if (o1.a != 50) return 70;

        p->a += 10;
        if (o1.a != 60) return 71;
    }

    // ========== SHORT-CIRCUIT EVALUATION SECTION (returns 80-99) ==========
    {
        side_effect_count = 0;

        // && short-circuit: false && X should not evaluate X
        int result = 0 && has_side_effect(1);
        if (side_effect_count != 0) return 80;
        if (result != 0) return 81;

        // && no short-circuit when true
        side_effect_count = 0;
        result = 1 && has_side_effect(1);
        if (side_effect_count != 1) return 82;
        if (result != 1) return 83;

        // || short-circuit: true || X should not evaluate X
        side_effect_count = 0;
        result = 1 || has_side_effect(1);
        if (side_effect_count != 0) return 84;
        if (result != 1) return 85;

        // || no short-circuit when false
        side_effect_count = 0;
        result = 0 || has_side_effect(1);
        if (side_effect_count != 1) return 86;
        if (result != 1) return 87;

        // Chained &&
        side_effect_count = 0;
        result = 1 && has_side_effect(1) && has_side_effect(1);
        if (side_effect_count != 2) return 88;

        // Chained && with early exit
        side_effect_count = 0;
        result = 1 && 0 && has_side_effect(1);
        if (side_effect_count != 0) return 89;

        // Chained ||
        side_effect_count = 0;
        result = 0 || has_side_effect(0) || has_side_effect(1);
        if (side_effect_count != 2) return 90;

        // Chained || with early exit
        side_effect_count = 0;
        result = 0 || 1 || has_side_effect(1);
        if (side_effect_count != 0) return 91;

        // Nested short-circuit
        side_effect_count = 0;
        result = (1 && 0) || has_side_effect(1);
        if (side_effect_count != 1) return 92;
        if (result != 1) return 93;

        // Ternary with side effects
        side_effect_count = 0;
        result = 1 ? has_side_effect(42) : has_side_effect(0);
        if (side_effect_count != 1) return 94;
        if (result != 42) return 95;

        side_effect_count = 0;
        result = 0 ? has_side_effect(0) : has_side_effect(42);
        if (side_effect_count != 1) return 96;
        if (result != 42) return 97;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_operators_mega", code, &[]), 0);
}

// ============================================================================
// Mega-test: Operator coverage gaps — compound assignments, bitwise NOT,
// comma operator, precedence (15 levels), associativity
// ============================================================================

#[test]
fn c89_operators_comprehensive_mega() {
    let code = r#"
int side_effect_var;
int side_effect_fn(int v) { side_effect_var = v; return v; }

int main(void) {
    // ========== COMPOUND ASSIGNMENT OPS (returns 1-19) ==========
    {
        int a;

        // -=
        a = 10; a -= 3;
        if (a != 7) return 1;

        // /=
        a = 20; a /= 4;
        if (a != 5) return 2;

        // %=
        a = 17; a %= 5;
        if (a != 2) return 3;

        // &=
        a = 0xFF; a &= 0x0F;
        if (a != 0x0F) return 4;

        // |=
        a = 0xF0; a |= 0x0F;
        if (a != 0xFF) return 5;

        // ^=
        a = 0xFF; a ^= 0x0F;
        if (a != 0xF0) return 6;

        // <<=
        a = 1; a <<= 4;
        if (a != 16) return 7;

        // >>=
        a = 256; a >>= 3;
        if (a != 32) return 8;

        // Verify += and *= too (should exist but confirm)
        a = 5; a += 10;
        if (a != 15) return 9;

        a = 6; a *= 7;
        if (a != 42) return 10;

        // Compound assignment on array element
        int arr[3] = {10, 20, 30};
        arr[1] += 5;
        if (arr[1] != 25) return 11;

        // Compound assignment on struct member
        struct { int x; } s = {100};
        s.x -= 30;
        if (s.x != 70) return 12;
    }

    // ========== BITWISE NOT (returns 20-29) ==========
    {
        unsigned int a = 0;
        unsigned int b = ~a;
        if (b != 0xFFFFFFFF) return 20;

        unsigned char c = 0x0F;
        // ~ promotes to int, mask to get byte
        int d = ~c;
        if ((d & 0xFF) != 0xF0) return 21;

        // Double complement = identity
        int e = 42;
        if (~~e != 42) return 22;

        // Bitwise NOT in expression
        unsigned int f = 0xAAAAAAAA;
        if ((~f) != 0x55555555) return 23;
    }

    // ========== COMMA OPERATOR (returns 30-39) ==========
    {
        // Comma evaluates left, discards, returns right
        int a = (1, 2, 3);
        if (a != 3) return 30;

        // Side effects in comma
        int x = 0, y = 0;
        int r = (x = 10, y = 20, x + y);
        if (r != 30) return 31;
        if (x != 10 || y != 20) return 32;

        // Comma in for-loop (multiple inits/posts)
        int i, sum = 0;
        for (i = 0, sum = 0; i < 5; i++, sum += i) {}
        if (sum != 15) return 33;  // 1+2+3+4+5

        // Comma vs assignment precedence
        // a = (1, 2) assigns 2; (a = 1, 2) assigns 1, expr is 2
        a = (1, 2);
        if (a != 2) return 34;
    }

    // ========== OPERATOR PRECEDENCE (returns 40-69) ==========
    // Test that operators bind at the correct level

    // Level 3 (* / %) before Level 4 (+ -)
    {
        int r = 2 + 3 * 4;       // 2 + 12 = 14, not 20
        if (r != 14) return 40;

        r = 10 - 6 / 2;          // 10 - 3 = 7, not 2
        if (r != 7) return 41;

        r = 2 + 10 % 3;          // 2 + 1 = 3, not 0
        if (r != 3) return 42;
    }

    // Level 5 (<< >>) before Level 6 (< <= > >=)
    {
        int r = 1 << 2 < 8;      // (1<<2) < 8 = 4 < 8 = 1
        if (r != 1) return 43;
    }

    // Level 6 (< <= > >=) before Level 7 (== !=)
    {
        int r = 1 < 2 == 1;      // (1<2) == 1 = 1 == 1 = 1
        if (r != 1) return 44;

        r = 5 >= 5 != 0;         // (5>=5) != 0 = 1 != 0 = 1
        if (r != 1) return 45;
    }

    // Level 7 (== !=) before Level 8 (&)
    {
        int r = 1 & 3 == 3;      // 1 & (3==3) = 1 & 1 = 1
        if (r != 1) return 46;
    }

    // Level 8 (&) before Level 9 (^)
    {
        int r = 0xF & 0x3 ^ 0x1; // (0xF & 0x3) ^ 0x1 = 3 ^ 1 = 2
        if (r != 2) return 47;
    }

    // Level 9 (^) before Level 10 (|)
    {
        int r = 0x1 | 0x2 ^ 0x3; // 0x1 | (0x2 ^ 0x3) = 0x1 | 0x1 = 0x1
        if (r != 1) return 48;
    }

    // Level 10 (|) before Level 11 (&&)
    {
        int r = 0 | 1 && 1;      // (0|1) && 1 = 1 && 1 = 1
        if (r != 1) return 49;
    }

    // Level 11 (&&) before Level 12 (||)
    {
        int r = 0 && 1 || 1;     // (0&&1) || 1 = 0 || 1 = 1
        if (r != 1) return 50;

        r = 1 || 0 && 0;         // 1 || (0&&0) = 1 || 0 = 1
        if (r != 1) return 51;
    }

    // Level 12 (||) before Level 13 (?:)
    {
        int r = 0 || 1 ? 10 : 20; // (0||1) ? 10 : 20 = 10
        if (r != 10) return 52;
    }

    // Level 13 (?:) before Level 14 (=)
    {
        int a;
        a = 1 ? 42 : 99;         // a = (1 ? 42 : 99) = 42
        if (a != 42) return 53;
    }

    // Level 14 (=) before Level 15 (,)
    {
        int a, b;
        a = 1, b = 2;            // (a=1), (b=2)
        if (a != 1 || b != 2) return 54;
    }

    // Level 1 (postfix) before Level 2 (unary)
    {
        int arr[3] = {10, 20, 30};
        int *p = arr;
        int r = *p++;             // *(p++), gets arr[0]=10, p moves to arr[1]
        if (r != 10) return 55;
        if (*p != 20) return 56;
    }

    // Unary before multiplicative
    {
        int r = -3 * -2;         // (-3) * (-2) = 6
        if (r != 6) return 57;
    }

    // sizeof before arithmetic
    {
        int r = sizeof(int) + 1;  // (sizeof(int)) + 1 = 4 + 1 = 5
        if (r != 5) return 58;
    }

    // Complex mixed precedence
    {
        int r = 2 + 3 * 4 - 1;   // 2 + 12 - 1 = 13
        if (r != 13) return 59;

        r = 1 + 2 << 3;          // (1+2) << 3 = 3 << 3 = 24
        if (r != 24) return 60;

        r = 10 >> 1 + 1;         // 10 >> (1+1) = 10 >> 2 = 2
        if (r != 2) return 61;
    }

    // ========== OPERATOR ASSOCIATIVITY (returns 70-79) ==========

    // Right-to-left: assignment
    {
        int a, b, c;
        a = b = c = 42;          // a = (b = (c = 42))
        if (a != 42 || b != 42 || c != 42) return 70;
    }

    // Right-to-left: ternary
    {
        int r = 1 ? 2 : 0 ? 3 : 4;  // 1 ? 2 : (0 ? 3 : 4)
        if (r != 2) return 71;

        r = 0 ? 2 : 1 ? 3 : 4;      // 0 ? 2 : (1 ? 3 : 4) = 3
        if (r != 3) return 72;
    }

    // Left-to-right: subtraction
    {
        int r = 10 - 3 - 2;      // (10-3) - 2 = 5, not 10-(3-2)=9
        if (r != 5) return 73;
    }

    // Left-to-right: division
    {
        int r = 24 / 4 / 2;      // (24/4) / 2 = 3, not 24/(4/2)=12
        if (r != 3) return 74;
    }

    // Left-to-right: shift
    {
        int r = 16 >> 2 >> 1;    // (16>>2) >> 1 = 4 >> 1 = 2
        if (r != 2) return 75;
    }

    // Right-to-left: compound assignment
    {
        int a = 10, b = 20;
        a += b -= 5;             // a += (b -= 5) => b=15, a=25
        if (a != 25 || b != 15) return 76;
    }

    // Left-to-right: comparison chaining
    {
        // 1 < 2 < 3 means (1<2) < 3 = 1 < 3 = 1
        int r = 1 < 2 < 3;
        if (r != 1) return 77;

        // 3 > 2 > 1 means (3>2) > 1 = 1 > 1 = 0
        r = 3 > 2 > 1;
        if (r != 0) return 78;
    }

    // ========== SHORT-CIRCUIT WITH SIDE EFFECTS (returns 80-89) ==========
    {
        // && short-circuits: right side not evaluated if left is false
        side_effect_var = 0;
        int r = 0 && side_effect_fn(42);
        if (side_effect_var != 0) return 80;  // fn not called

        // || short-circuits: right side not evaluated if left is true
        side_effect_var = 0;
        r = 1 || side_effect_fn(42);
        if (side_effect_var != 0) return 81;  // fn not called

        // && does evaluate right when left is true
        side_effect_var = 0;
        r = 1 && side_effect_fn(42);
        if (side_effect_var != 42) return 82;

        // || does evaluate right when left is false
        side_effect_var = 0;
        r = 0 || side_effect_fn(42);
        if (side_effect_var != 42) return 83;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c89_operators_comprehensive_mega", code, &[]),
        0,
    );
}
