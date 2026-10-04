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

/// A vector is a value, and a union with a member of its type takes it.
#[test]
fn cast_to_union_of_a_vector_selects_the_vector_member() {
    let src = "typedef int v4 __attribute__((vector_size(16)));\n\
               typedef union { v4 v; int a[4]; } U;\n\
               int main(void) { v4 x = {1, 2, 3, 4}; U u = (U)x; return u.a[3] == 4 ? 0 : 1; }\n";
    assert_eq!(crate::common::compile_and_run("ctu_vector", src, &[]), 0);
}
