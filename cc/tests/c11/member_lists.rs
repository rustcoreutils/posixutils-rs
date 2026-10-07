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

use crate::common::compile_and_run;

/// The accepted programs of this file as one program; each section keeps its
/// original test name and doc comment. The rejected halves are unit tests in
/// cc/test_asm/c11_member_lists.rs.
///
/// Consolidates: fam_after_an_anonymous_member, member_list_shapes_from_uapi_headers
/// and logical_operators_short_circuit_in_constant_expressions.
#[test]
fn member_lists_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  2  fam_after_an_anonymous_member
 *    11- 14  member_list_shapes_from_uapi_headers
 *    21- 22  logical_operators_short_circuit_in_constant_expressions
 */

/* ---- fam_after_an_anonymous_member (exit codes 1-2) ----
 *
 *  C17 6.7.2.1p13: an anonymous union's members are members of the
 *  containing struct, so they are the named members a flexible array member
 *  needs before it (<linux/bpf.h>). An unnamed bit-field is padding and does
 *  not count.
 */
#include <stddef.h>
struct s { union { int a; long b; }; char d[]; };
struct t { struct { int x, y; }; int tail[]; };
static int t_fam_after_an_anonymous_member(void) {
    if (offsetof(struct s, d) != sizeof(long)) return 1;
    if (offsetof(struct t, tail) != 2 * sizeof(int)) return 2;
    return 0;
}


/* ---- member_list_shapes_from_uapi_headers (exit codes 11-13) ----
 *
 *  An unnamed bit-field may begin a declarator list, as in <linux/ioam6.h>'s
 *  `__u8 :1, :1, x:1;`, and a member list may hold a stray `;`
 *  (<linux/nfc.h>), which gcc accepts.
 */
struct flags { unsigned char :1, :1, x:1, :2, y:1; };
struct semi { int a;; int b; };
struct lead { ; int c; };
static int t_member_list_shapes_from_uapi_headers(void) {
    struct flags f = { 0 };
    f.x = 1; f.y = 1;
    if (sizeof(struct flags) != 1) return 1;
    if (*(unsigned char *)&f != ((1u << 2) | (1u << 5))) return 2;
    if (sizeof(struct semi) != 2 * sizeof(int)) return 3;
    struct semi s = { 1, 2 };
    struct lead l = { 3 };
    return s.a + s.b + l.c - 6;
}


/* ---- logical_operators_short_circuit_in_constant_expressions (exit codes 21-22) ----
 *
 *  The right operand of `&&` and `||` is not evaluated when the left decides
 *  (C17 6.5.13p4, 6.5.14p4), and an operand that is not evaluated may be
 *  anything (6.6p3), as for the arm `?:` does not take.
 */
extern int sc_x;
int sc_a = 1 || 1/0;
int sc_b[(0 && 1/0) + 1];
int sc_c = 0 && sc_x;
_Static_assert(1 || 1/0, "short circuit");
_Static_assert(!(0 && 1/0), "short circuit");
enum { SC_E = 0 || 7 };
static int t_logical_operators_short_circuit_in_constant_expressions(void) {
    switch (sc_a) { case (1 || 1/0): break; default: return 1; }
    return (sizeof sc_b == sizeof(int) && sc_c == 0 && SC_E == 1) ? 0 : 2;
}
int sc_x;

int main(void)
{
    int r;
    if ((r = t_fam_after_an_anonymous_member()) != 0)
        return 0 + r;
    if ((r = t_member_list_shapes_from_uapi_headers()) != 0)
        return r > 0 && r < 4 ? 10 + r : 14;
    if ((r = t_logical_operators_short_circuit_in_constant_expressions()) != 0)
        return 20 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("member_lists_mega", code, &[]), 0);
}

/// C17 6.2.1p7: a tag is in scope from just after it, so a prototype inside
/// the member list names the structure being defined (util-linux's
/// `struct path_cxt`). c17 made the parameter a new tag of the prototype's
/// scope, and warned it would not be visible outside.
#[test]
fn member_prototype_names_the_struct_being_defined() {
    let code = r#"
struct path_cxt {
    int dir_fd;
    void (*free_dialect)(struct path_cxt *);
};
typedef void (*freer)(struct path_cxt *);
static void release(struct path_cxt *pc) { pc->dir_fd = 7; }
int main(void) {
    struct path_cxt c = { 0, release };
    c.free_dialect(&c);
    if (c.dir_fd != 7) return 1;
    return _Generic(c.free_dialect, freer: 0, default: 2);
}
"#;
    assert_eq!(
        compile_and_run("member_prototype_tag", code, &["-Werror".to_string()]),
        0
    );
}
