//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Objects of incomplete type: as members, as parameters, and as values.
//
// Each of these once got past the front end and crashed c17: a structure
// that contained itself sent every walk over its members into unbounded
// recursion (a Rust stack overflow), and a value of a forward-declared
// `enum` -- which has no width -- reached a conversion the IR validator
// refused as an internal compiler error.
//

use crate::common::{compile_bounded, compile_expect_error, compile_expect_ok};

/// C17 6.7.2.1p3: a member has neither incomplete nor function type, and a
/// tag is incomplete until its closing brace -- so a structure cannot
/// contain itself.
#[test]
fn incomplete_member_types_are_rejected() {
    let cases = [
        (
            "itm_self",
            "struct A { int i; struct A a; };\nvoid f(void) { struct A b = {0}; (void)b; }\n",
            "field 'a' has incomplete type",
        ),
        (
            "itm_forward",
            "struct B;\nstruct A { struct B b; };\n",
            "field 'b' has incomplete type",
        ),
        (
            "itm_enum",
            "enum E;\nstruct A { enum E e; };\n",
            "field 'e' has incomplete type",
        ),
        (
            "itm_void",
            "struct A { void v; };\n",
            "field 'v' has incomplete type",
        ),
        (
            "itm_function",
            "struct A { int f(void); };\n",
            "field 'f' declared as a function",
        ),
        (
            "itm_block_scope",
            "void f(void) { struct A { int i; struct A a; } x; (void)x; }\n",
            "field 'a' has incomplete type",
        ),
    ];
    for (name, src, expected) in cases {
        compile_expect_error(name, src, expected);
    }
}

/// The accept side: what may legitimately refer to an incomplete type.
#[test]
fn incomplete_member_types_accept_side() {
    compile_expect_ok(
        "itm_ok",
        "struct A { struct A *next; int n; int tail[]; };\n\
         struct B;\n\
         struct C { struct B *p; struct C (*fp)(struct C); };\n\
         struct D { struct { int x; }; union { int y; float z; }; };\n\
         _Static_assert(sizeof(struct D) == 8, \"anonymous members\");\n\
         int main(void) { struct D d = {{1}, {2}}; return d.x + d.y - 3; }\n",
    );
}

/// C17 6.7.2.1p13: only a specifier with no tag makes an anonymous member.
/// A tagged one inside a member list declares the tag and nothing else --
/// taking it as a member made `struct A { struct A; }` contain itself.
#[test]
fn tagged_struct_in_member_list_is_not_a_member() {
    let run = compile_bounded(
        "itm_tagged_self",
        "struct A { struct A; int x; };\nstruct A v = {0};\n\
         _Static_assert(sizeof v == sizeof(int), \"only x\");\n",
        30,
    );
    assert!(run.success, "{}", run.stderr);
    assert!(
        run.stderr.contains("declaration does not declare anything"),
        "{}",
        run.stderr
    );

    let run = compile_bounded(
        "itm_tagged_inner",
        "struct S { struct T { int b; }; int a; };\nstruct T t;\n\
         _Static_assert(sizeof(struct S) == sizeof(int), \"only a\");\n",
        30,
    );
    assert!(run.success, "{}", run.stderr);
    assert!(
        run.stderr.contains("declaration does not declare anything"),
        "{}",
        run.stderr
    );
}

/// A value of an incomplete type cannot be read or written (C17 6.3.2.1p2,
/// which gcc enforces). A forward-declared `enum` is the GNU extension that
/// reaches this.
#[test]
fn incomplete_enum_values_are_rejected() {
    let uses = [
        ("iev_argument", "void g(int);\nvoid f(void) { g(ve); }\n"),
        ("iev_arith", "int f(void) { return ve + 1; }\n"),
        ("iev_assign", "void f(void) { ve = 1; }\n"),
        ("iev_postinc", "void f(void) { ve++; }\n"),
        ("iev_predec", "void f(void) { --ve; }\n"),
        ("iev_compound", "void f(void) { ve += 2; }\n"),
        ("iev_deref", "int f(enum e *p) { return *p; }\n"),
    ];
    for (name, body) in uses {
        let src = format!("extern enum e ve;\n{body}");
        compile_expect_error(name, &src, "invalid use of incomplete type 'enum e'");
    }

    // Taking its address needs no value.
    compile_expect_ok(
        "iev_address",
        "extern enum e ve;\nvoid *p = &ve;\nvoid *f(void) { return &ve; }\n",
    );
}

/// C17 6.9.1p7: a parameter in a definition has complete type; a call needs
/// every parameter's type complete to assign its argument (6.5.2.2p4). A
/// prototype alone may name an incomplete type.
#[test]
fn incomplete_parameter_types() {
    compile_expect_error(
        "ipt_definition",
        "void foo(enum E e) {}\n",
        "parameter 1 ('e') has incomplete type",
    );
    compile_expect_error(
        "ipt_call_enum",
        "void foo(enum E e);\nvoid bar(void) { foo(0); }\n",
        "type of formal parameter 1 is incomplete",
    );
    compile_expect_error(
        "ipt_call_struct",
        "struct S;\nvoid k(struct S s);\nvoid f(struct S *p) { k(*p); }\n",
        "type of formal parameter 1 is incomplete",
    );
    compile_expect_ok(
        "ipt_completed_later",
        "struct S;\nvoid k(struct S s);\nstruct S { int a; };\n\
         void f(struct S *p) { k(*p); }\nvoid k(struct S s) { (void)s; }\n",
    );
}
