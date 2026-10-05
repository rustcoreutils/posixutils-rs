//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU cast to a union type: the operand must have the type of a member.
//
// gcc matches the operand's type, after lvalue conversion, against each
// member's type by compatibility, ignoring qualifiers. No conversion is
// applied to find a match -- a `long` does not select an `int` member, nor a
// `char` an `int` -- and a bit-field never matches. A cast that selects no
// member is "cast to union type from type not present in union". The cast
// yields a value, not an lvalue.
//

use crate::test_compile::{compile_expect_error, compile_expect_ok};

const NOT_PRESENT: &str = "cast to union type from type not present in union";

#[test]
fn cast_to_union_needs_a_member_of_the_operand_type() {
    let cases = [
        (
            "ctu_long_to_int",
            "typedef union { int i; } U;\nU f(long x) { return (U)x; }\n",
        ),
        (
            "ctu_char_to_int",
            "typedef union { int i; double d; } U;\nU f(char x) { return (U)x; }\n",
        ),
        (
            "ctu_float_to_double",
            "typedef union { double d; } U;\nU f(float x) { return (U)x; }\n",
        ),
        (
            "ctu_bitfield",
            "typedef union { int i : 3; double d; } U;\nU f(int x) { return (U)x; }\n",
        ),
        (
            "ctu_struct",
            "struct a { int x; }; struct b { int x; };\n\
             typedef union { struct a a; } U;\nU f(struct b x) { return (U)x; }\n",
        ),
        (
            "ctu_other_union",
            "typedef union { int i; } U; typedef union { int i; } V;\nU f(V x) { return (U)x; }\n",
        ),
        (
            "ctu_pointer",
            "typedef union { int *p; } U;\nU f(long *x) { return (U)x; }\n",
        ),
    ];
    for (name, src) in cases {
        compile_expect_error(name, src, NOT_PRESENT);
    }
}

#[test]
fn cast_to_union_matches_ignoring_qualifiers_and_decay() {
    compile_expect_ok(
        "ctu_accepted",
        "typedef union { const int ci; int *p; double d; } U;\n\
         U f(volatile int v) { return (U)v; }\n\
         U g(int a[2]) { return (U)a; }\n\
         U h(void) { int arr[2] = { 0, 0 }; return (U)arr; }\n\
         U k(U u) { return (U)u; }\n\
         U m(const double d) { return (U)d; }\n",
    );
}

#[test]
fn cast_to_union_is_not_an_lvalue() {
    compile_expect_error(
        "ctu_address",
        "typedef union { int i; } U;\nU *f(int x) { return &(U)x; }\n",
        "lvalue required",
    );
    compile_expect_error(
        "ctu_assign",
        "typedef union { int i; } U;\nvoid f(int x, U u) { (U)x = u; }\n",
        "lvalue required",
    );
}
