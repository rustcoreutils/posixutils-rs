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
