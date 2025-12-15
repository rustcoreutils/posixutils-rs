//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 bitfield support
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Bitfield: Declaration, access, packing, sizeof
// ============================================================================

#[test]
fn bitfield_basic_and_packing() {
    let code = r#"
int main(void) {
    // Test 1-3: Basic declaration and access
    struct flags {
        unsigned int a : 1;
        unsigned int b : 3;
        unsigned int c : 4;
    };
    struct flags f;
    f.a = 1;
    f.b = 5;
    f.c = 15;
    if (f.a != 1) return 1;
    if (f.b != 5) return 2;
    if (f.c != 15) return 3;

    // Test 4-9: Packing - four 8-bit fields should pack into one 32-bit int
    struct packed {
        unsigned int a : 8;
        unsigned int b : 8;
        unsigned int c : 8;
        unsigned int d : 8;
    };
    if (sizeof(struct packed) != 4) return 4;
    struct packed p;
    p.a = 0x12;
    p.b = 0x34;
    p.c = 0x56;
    p.d = 0x78;
    if (p.a != 0x12) return 5;
    if (p.b != 0x34) return 6;
    if (p.c != 0x56) return 7;
    if (p.d != 0x78) return 8;

    // Test 10: Three 1-bit fields should fit in one int (4 bytes)
    struct small {
        unsigned int a : 1;
        unsigned int b : 1;
        unsigned int c : 1;
    };
    if (sizeof(struct small) != 4) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_basic", code), 0);
}

// ============================================================================
// Special bitfield features: zero-width, unnamed, _Bool, mixed
// ============================================================================

#[test]
fn bitfield_special_features() {
    let code = r#"
int main(void) {
    // Test 1-3: Zero-width bitfield forces alignment to next storage unit
    struct aligned {
        unsigned int a : 4;
        unsigned int : 0;   // Force next field to new unit
        unsigned int b : 4;
    };
    if (sizeof(struct aligned) != 8) return 1;
    struct aligned x;
    x.a = 15;
    x.b = 7;
    if (x.a != 15) return 2;
    if (x.b != 7) return 3;

    // Test 4-5: Unnamed bitfield provides padding
    struct padded {
        unsigned int a : 4;
        unsigned int   : 4;  // 4 bits padding (unnamed)
        unsigned int b : 8;
    };
    struct padded pad;
    pad.a = 15;
    pad.b = 200;
    if (pad.a != 15) return 4;
    if (pad.b != 200) return 5;

    // Test 6-8: _Bool bitfield
    struct bool_bits {
        _Bool flag1 : 1;
        _Bool flag2 : 1;
        _Bool flag3 : 1;
    };
    struct bool_bits bb;
    bb.flag1 = 1;
    bb.flag2 = 0;
    bb.flag3 = 1;
    if (bb.flag1 != 1) return 6;
    if (bb.flag2 != 0) return 7;
    if (bb.flag3 != 1) return 8;

    // Test 9-11: Mixed with regular members
    struct mixed {
        int regular;
        unsigned int bits : 8;
        int another;
    };
    struct mixed m;
    m.regular = 100;
    m.bits = 255;
    m.another = 200;
    if (m.regular != 100) return 9;
    if (m.bits != 255) return 10;
    if (m.another != 200) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_special", code), 0);
}

// ============================================================================
// Bitfield operations: overflow, pointer access, arithmetic
// ============================================================================

#[test]
fn bitfield_operations() {
    let code = r#"
int main(void) {
    // Test 1-2: Overflow (value truncation)
    struct overflow {
        unsigned int a : 3;  // Range 0-7
    };
    struct overflow o;
    o.a = 15;  // Should be masked to 7 (15 & 0x7 = 7)
    if (o.a != 7) return 1;
    o.a = 8;  // Should be masked to 0 (8 & 0x7 = 0)
    if (o.a != 0) return 2;

    // Test 3-6: Pointer access
    struct ptrflags {
        unsigned int a : 4;
        unsigned int b : 4;
    };
    struct ptrflags pf;
    struct ptrflags *pp = &pf;
    pp->a = 10;
    pp->b = 5;
    if (pp->a != 10) return 3;
    if (pp->b != 5) return 4;
    if (pf.a != 10) return 5;
    if (pf.b != 5) return 6;

    // Test 7-9: Arithmetic
    struct bits {
        unsigned int a : 4;
        unsigned int b : 4;
    };
    struct bits s;
    s.a = 5;
    s.b = 3;
    int sum = s.a + s.b;
    if (sum != 8) return 7;
    int product = s.a * s.b;
    if (product != 15) return 8;
    s.a = s.a + 1;
    if (s.a != 6) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_operations", code), 0);
}
