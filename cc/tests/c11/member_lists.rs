//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Valid code c17 once rejected, each found in a Linux UAPI header: member
// lists, a flexible array member after an anonymous union, a concatenated
// `_Static_assert` message, and an `&&` / `||` whose right operand is not
// evaluated.
//

use crate::common::{compile_and_run, compile_expect_error};

/// C17 6.7.2.1p13: an anonymous union's members are members of the
/// containing struct, so they are the named members a flexible array member
/// needs before it (<linux/bpf.h>). An unnamed bit-field is padding and does
/// not count.
#[test]
fn fam_after_an_anonymous_member() {
    let src = r#"
#include <stddef.h>
struct s { union { int a; long b; }; char d[]; };
struct t { struct { int x, y; }; int tail[]; };
int main(void) {
    if (offsetof(struct s, d) != sizeof(long)) return 1;
    if (offsetof(struct t, tail) != 2 * sizeof(int)) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("fam_anon", src, &[]), 0);

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
    let src = r#"
struct flags { unsigned char :1, :1, x:1, :2, y:1; };
struct semi { int a;; int b; };
struct lead { ; int c; };
int main(void) {
    struct flags f = { 0 };
    f.x = 1; f.y = 1;
    if (sizeof(struct flags) != 1) return 1;
    if (*(unsigned char *)&f != ((1u << 2) | (1u << 5))) return 2;
    if (sizeof(struct semi) != 2 * sizeof(int)) return 3;
    struct semi s = { 1, 2 };
    struct lead l = { 3 };
    return s.a + s.b + l.c - 6;
}
"#;
    assert_eq!(compile_and_run("uapi_member_lists", src, &[]), 0);

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
    let src = r#"
extern int x;
int a = 1 || 1/0;
int b[(0 && 1/0) + 1];
int c = 0 && x;
_Static_assert(1 || 1/0, "short circuit");
_Static_assert(!(0 && 1/0), "short circuit");
enum { E = 0 || 7 };
int main(void) {
    switch (a) { case (1 || 1/0): break; default: return 1; }
    return (sizeof b == sizeof(int) && c == 0 && E == 1) ? 0 : 2;
}
int x;
"#;
    assert_eq!(compile_and_run("const_short_circuit", src, &[]), 0);

    compile_expect_error(
        "const_short_circuit_taken",
        "int a = 0 || 1/0;\n",
        "not a constant expression",
    );
}
