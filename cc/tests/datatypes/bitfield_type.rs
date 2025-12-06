//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 bitfield support
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Bitfield: Declaration and access
// ============================================================================

#[test]
fn bitfield_basic() {
    let code = r#"
int main(void) {
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

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_basic", code), 0);
}

// ============================================================================
// Bitfield Packing: Multiple bitfields packed into single storage unit
// ============================================================================

#[test]
fn bitfield_packed() {
    let code = r#"
int main(void) {
    struct packed {
        unsigned int a : 8;
        unsigned int b : 8;
        unsigned int c : 8;
        unsigned int d : 8;
    };

    // Four 8-bit fields should pack into one 32-bit int
    if (sizeof(struct packed) != 4) return 1;

    struct packed p;
    p.a = 0x12;
    p.b = 0x34;
    p.c = 0x56;
    p.d = 0x78;

    if (p.a != 0x12) return 2;
    if (p.b != 0x34) return 3;
    if (p.c != 0x56) return 4;
    if (p.d != 0x78) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_packed", code), 0);
}

// ============================================================================
// Bitfield Sizeof: Verify struct size with bitfields
// ============================================================================

#[test]
fn bitfield_sizeof() {
    let code = r#"
int main(void) {
    struct small {
        unsigned int a : 1;
        unsigned int b : 1;
        unsigned int c : 1;
    };

    // Three 1-bit fields should fit in one int (4 bytes)
    if (sizeof(struct small) != 4) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_sizeof", code), 0);
}

// ============================================================================
// Zero-Width Bitfield: Force alignment to next storage unit
// ============================================================================

#[test]
fn bitfield_zero_width() {
    let code = r#"
int main(void) {
    struct aligned {
        unsigned int a : 4;
        unsigned int : 0;   // Force next field to new unit
        unsigned int b : 4;
    };

    // a and b should be in different storage units
    if (sizeof(struct aligned) != 8) return 1;

    struct aligned x;
    x.a = 15;
    x.b = 7;

    if (x.a != 15) return 2;
    if (x.b != 7) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_zero_width", code), 0);
}

// ============================================================================
// Unnamed Bitfield: Padding bits
// ============================================================================

#[test]
fn bitfield_unnamed() {
    let code = r#"
int main(void) {
    struct padded {
        unsigned int a : 4;
        unsigned int   : 4;  // 4 bits padding (unnamed)
        unsigned int b : 8;
    };

    struct padded p;
    p.a = 15;
    p.b = 200;

    if (p.a != 15) return 1;
    if (p.b != 200) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_unnamed", code), 0);
}

// ============================================================================
// Bitfield with _Bool type
// ============================================================================

#[test]
fn bitfield_bool() {
    let code = r#"
int main(void) {
    struct bool_bits {
        _Bool flag1 : 1;
        _Bool flag2 : 1;
        _Bool flag3 : 1;
    };

    struct bool_bits b;
    b.flag1 = 1;
    b.flag2 = 0;
    b.flag3 = 1;

    if (b.flag1 != 1) return 1;
    if (b.flag2 != 0) return 2;
    if (b.flag3 != 1) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_bool", code), 0);
}

// ============================================================================
// Bitfield with Regular Members
// ============================================================================

#[test]
fn bitfield_mixed() {
    let code = r#"
int main(void) {
    struct mixed {
        int regular;
        unsigned int bits : 8;
        int another;
    };

    struct mixed m;
    m.regular = 100;
    m.bits = 255;
    m.another = 200;

    if (m.regular != 100) return 1;
    if (m.bits != 255) return 2;
    if (m.another != 200) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_mixed", code), 0);
}

// ============================================================================
// Bitfield Overflow: Value truncation
// ============================================================================

#[test]
fn bitfield_overflow() {
    let code = r#"
int main(void) {
    struct overflow {
        unsigned int a : 3;  // Range 0-7
    };

    struct overflow o;
    o.a = 15;  // Should be masked to 7 (15 & 0x7 = 7)

    if (o.a != 7) return 1;

    o.a = 8;  // Should be masked to 0 (8 & 0x7 = 0)
    if (o.a != 0) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_overflow", code), 0);
}

// ============================================================================
// Bitfield with Pointer Access
// ============================================================================

#[test]
fn bitfield_pointer() {
    let code = r#"
int main(void) {
    struct flags {
        unsigned int a : 4;
        unsigned int b : 4;
    };

    struct flags f;
    struct flags *p = &f;

    p->a = 10;
    p->b = 5;

    if (p->a != 10) return 1;
    if (p->b != 5) return 2;
    if (f.a != 10) return 3;
    if (f.b != 5) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_pointer", code), 0);
}

// ============================================================================
// Bitfield Arithmetic
// ============================================================================

#[test]
fn bitfield_arithmetic() {
    let code = r#"
int main(void) {
    struct bits {
        unsigned int a : 4;
        unsigned int b : 4;
    };

    struct bits s;
    s.a = 5;
    s.b = 3;

    int sum = s.a + s.b;
    if (sum != 8) return 1;

    int product = s.a * s.b;
    if (product != 15) return 2;

    // Increment
    s.a = s.a + 1;
    if (s.a != 6) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_arithmetic", code), 0);
}
