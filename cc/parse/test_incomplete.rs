//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for incomplete types as members and parameters.
//

use super::test_parser::parse_tu;
use crate::symbol::Namespace;

/// The number of members the definition of `struct <tag>` in `src` has.
fn member_count(src: &str, tag: &str) -> usize {
    let (_, types, strings, symbols) = parse_tu(src).unwrap();
    let tag = strings.lookup(tag).expect("tag interned");
    let typ = symbols
        .lookup(tag, Namespace::Tag)
        .expect("tag declared")
        .typ;
    let composite = types.get(typ).composite.as_ref().expect("a composite");
    assert!(composite.is_complete);
    composite.members.len()
}

/// Parse `src`, requiring at least one error to be reported.
fn assert_rejected(src: &str) {
    let before = crate::diag::error_count();
    let _ = parse_tu(src);
    assert!(crate::diag::error_count() > before, "{src}: accepted");
}

/// C17 6.7.2.1p3: no member of incomplete or function type. The member is
/// dropped as well as reported, so the type that would contain itself is
/// never built.
#[test]
fn test_incomplete_members_are_rejected_and_dropped() {
    for src in [
        "struct A { int i; struct A a; };",
        "struct B; struct A { struct B b; };",
        "enum E; struct A { enum E e; };",
        "struct A { void v; };",
        "struct A { int f(void); };",
    ] {
        assert_rejected(src);
    }

    assert_eq!(
        member_count("struct A { int i; struct A a; int j; };", "A"),
        2,
        "the self-typed member was kept"
    );
}

/// C17 6.7.2.1p13: a tagged specifier declares its tag, not an anonymous
/// member.
#[test]
fn test_tagged_specifier_in_member_list_is_not_a_member() {
    assert_eq!(member_count("struct A { struct A; int x; };", "A"), 1);
}

/// C17 6.9.1p7 for a definition's parameters; 6.5.2.2p4 for a call.
#[test]
fn test_incomplete_parameters() {
    assert_rejected("void foo(enum E e) {}");
    assert_rejected("void foo(enum E e); void bar(void) { foo(0); }");
    assert_rejected("struct S; void k(struct S s); void f(struct S *p) { k(*p); }");

    // A prototype alone may name an incomplete type. The error counter is
    // shared by every test thread, so the accept side is proved end to end
    // instead (`incomplete_parameter_types` in the integration suite).
    parse_tu("struct S; void k(struct S s); void f(struct S *p);").unwrap();
}
