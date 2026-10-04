//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `_Complex` qualifies a floating type (C17 6.7.2p2), or an integer type as
// gcc's extension. Combined with anything else it was silently dropped:
// `typedef double ty; ty _Complex z;` made `z` a plain `double`.
//

use crate::test_compile::{compile_expect_error, compile_expect_ok};

#[test]
fn complex_with_a_non_arithmetic_base_is_rejected() {
    for (name, src, base) in [
        ("cx_typedef", "typedef double ty; ty _Complex z;\n", "ty"),
        ("cx_typeof", "__typeof__(1.0f) _Complex z;\n", "__typeof__"),
        ("cx_struct", "struct S { int a; } _Complex v;\n", "struct"),
        ("cx_enum", "enum e { A } _Complex q;\n", "enum"),
        ("cx_bool", "_Complex _Bool b;\n", "_Bool"),
        ("cx_void", "_Complex void *p;\n", "void"),
    ] {
        compile_expect_error(
            name,
            src,
            &format!("both '_Complex' and '{base}' in declaration specifiers"),
        );
    }
}

#[test]
fn complex_with_an_arithmetic_base_is_accepted() {
    compile_expect_ok(
        "cx_bases",
        "_Complex double a; double _Complex b; _Complex float c;\n\
         long double _Complex d; _Complex int e; _Complex unsigned f;\n\
         _Complex g; short _Complex h; _Complex char i; long _Complex k;\n\
         typedef _Complex double cd; cd x;\n\
         _Static_assert(sizeof g == 2 * sizeof(double), \"bare _Complex\");\n\
         _Static_assert(sizeof f == 2 * sizeof(unsigned), \"_Complex unsigned\");\n",
    );
}
