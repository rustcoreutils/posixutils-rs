//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of tests/c11/member_lists.rs, in process: the
// rejected halves of valid code c17 once rejected, each found in a Linux
// UAPI header. (The accepted programs run in tests/c11/member_lists.rs.)
//

use crate::test_compile::compile_expect_error;

/// C17 6.7.2.1p13: an anonymous union's members are members of the
/// containing struct, so they are the named members a flexible array member
/// needs before it (<linux/bpf.h>). An unnamed bit-field is padding and does
/// not count.
#[test]
fn fam_after_an_anonymous_member() {
    compile_expect_error(
        "fam_only_padding",
        "struct s { int :3; char d[]; };\n",
        "flexible array member in a struct with no named members",
    );
    compile_expect_error(
        "fam_alone",
        "struct s { char d[]; };\n",
        "flexible array member in a struct with no named members",
    );
}

/// An unnamed bit-field may begin a declarator list, as in <linux/ioam6.h>'s
/// `__u8 :1, :1, x:1;`, and a member list may hold a stray `;`
/// (<linux/nfc.h>), which gcc accepts.
#[test]
fn member_list_shapes_from_uapi_headers() {
    compile_expect_error(
        "unnamed_bitfield_too_wide",
        "struct s { unsigned char :1, :9; };\n",
        "width",
    );
}

/// The right operand of `&&` and `||` is not evaluated when the left decides
/// (C17 6.5.13p4, 6.5.14p4), and an operand that is not evaluated may be
/// anything (6.6p3), as for the arm `?:` does not take.
#[test]
fn logical_operators_short_circuit_in_constant_expressions() {
    compile_expect_error(
        "const_short_circuit_taken",
        "int a = 0 || 1/0;\n",
        "not a constant expression",
    );
}
