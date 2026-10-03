//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for expression, initializer and type-name constraints.
//

use super::test_parser::parse_tu;

/// Parse `src`, requiring it to be rejected: a parse error, or a diagnostic.
fn assert_rejected(src: &str) {
    let before = crate::diag::error_count();
    let failed = parse_tu(src).is_err();
    assert!(
        failed || crate::diag::error_count() > before,
        "{src}: accepted"
    );
}

#[test]
fn test_designators_are_checked() {
    for src in [
        "struct P { int x; } p = { .y = 1 };",
        "struct I { int a; }; struct O { struct I in; } o = { .in = { .b = 2 } };",
        "struct I { int a; }; struct O { int n; struct I in; } o = { 1, { .zz = 2 } };",
        "int a[2] = { .x = 1 };",
        "struct S { int a; } s = { [0] = 1 };",
    ] {
        assert_rejected(src);
    }
    parse_tu(
        "struct I { int a, b; }; struct O { int n; struct I in; struct { int u; }; int arr[3]; };\
         struct O o = { 1, { .b = 2 }, .u = 3, .arr = { [1] = 4 } };",
    )
    .unwrap();
}

#[test]
fn test_member_access_cast_and_assignment_rules() {
    for src in [
        "struct S { int a; }; int f(struct S s) { return s->a; }",
        "struct S { int a, b; }; void f(int x) { struct S s = (struct S)x; }",
        "struct S { int a; }; int f(struct S s) { return (int)s; }",
        "double f(int *p) { return (double)p; }",
        "int *f(double d) { return (int *)d; }",
        "struct S { const int a; }; void f(struct S *s, struct S t) { *s = t; }",
    ] {
        assert_rejected(src);
    }
}

#[test]
fn test_bit_field_and_type_name_rules() {
    for src in [
        "struct S { int a : 3; } s; int *p = &s.a;",
        "struct S { int a : 3; } s; int n = sizeof(s.a);",
        "struct S { int a : 3; } s; int n = _Alignof(s.a);",
        "struct { _Atomic int a : 3; } s;",
        "struct { _Bool a : 2; } s;",
        "int n = _Generic(1, int(void): 1, default: 0);",
        "struct S; int n = _Generic(1, struct S: 1, default: 0);",
        "struct S; int n = _Alignof(struct S);",
        "_Atomic(const int) x;",
        "struct { _Alignas(1) int x; } s;",
        "void f(int a[3][static 4]);",
        "struct S { int a; struct { int a; }; };",
        "void f(int, void);",
    ] {
        assert_rejected(src);
    }
    parse_tu("void f(int a[static 3]); struct T { int a; struct { int b; }; }; _Atomic(int) x;")
        .unwrap();
}

/// Parse `src`, requiring it to be accepted: no parse error and no error
/// diagnostic. `parse_tu(..).unwrap()` alone says nothing about the latter.
fn assert_accepted(src: &str) {
    let before = crate::diag::error_count();
    assert!(parse_tu(src).is_ok(), "{src}: parse error");
    assert_eq!(crate::diag::error_count(), before, "{src}: rejected");
}

/// C17 6.5.3.4p1: `_Alignof` refuses an incomplete type, but a variable
/// length array's extent is written -- its size expression is the type-name's
/// own -- so `int[n]` is complete, exactly as it is to `sizeof`.
#[test]
fn test_alignof_variable_length_array_type_is_complete() {
    for src in [
        "int f(void) { int n = 3; return _Alignof(int[n]); }",
        "int f(void) { int n = 3; return _Alignof(int[n][4]); }",
        "int f(void) { int n = 3; return _Alignof(int(*)[n]); }",
    ] {
        assert_accepted(src);
    }
    for src in [
        "int f(void) { return _Alignof(int[]); }",
        "int f(void) { int n = 3; return _Alignof(int[][n]); }",
    ] {
        assert_rejected(src);
    }
}

/// C17 6.4.5p2: a `u8` literal may join a plain one but no other prefix, in
/// either order and wherever the plain pieces fall in the run.
#[test]
fn test_utf8_string_concatenation_with_wide_prefix_is_rejected() {
    for src in [
        "unsigned long x = sizeof(u8\"a\" L\"b\");",
        "unsigned long x = sizeof(L\"a\" u8\"b\");",
        "unsigned long x = sizeof(u8\"a\" u\"b\");",
        "unsigned long x = sizeof(U\"a\" u8\"b\");",
        "unsigned long x = sizeof(\"a\" u8\"b\" L\"c\");",
        "unsigned long x = sizeof(L\"a\" \"b\" u8\"c\");",
    ] {
        assert_rejected(src);
    }
    for src in [
        "unsigned long x = sizeof(u8\"a\" u8\"b\");",
        "unsigned long x = sizeof(u8\"a\" \"b\");",
        "unsigned long x = sizeof(\"a\" u8\"b\");",
        "unsigned long x = sizeof(\"a\" L\"b\");",
        "unsigned long x = sizeof(L\"a\" L\"b\");",
    ] {
        assert_accepted(src);
    }
}

/// A cast to a structure type is gcc's extension when the operand already
/// has that type, qualifiers aside; any other operand is a "conversion to
/// non-scalar type". The result is a value, not an lvalue.
#[test]
fn test_cast_to_the_operands_own_structure_type() {
    for src in [
        "struct S { int a; }; struct S g(struct S s) { return (struct S)s; }",
        "struct S { int a; }; struct S g(struct S s) { return (const struct S)s; }",
        "struct S { int a; }; struct S g(const struct S s) { return (struct S)s; }",
        "struct S { int a; }; struct S g(volatile struct S s) { return (struct S)s; }",
        "struct S { int a; }; typedef struct S TS; struct S g(TS s) { return (struct S)s; }",
        "struct S { int a; }; struct S g(struct S *p) { return (struct S)*p; }",
        "struct S { int a; }; struct S h(void); int g(void) { return ((struct S)h()).a; }",
        "struct { int a; } x, y; void g(void) { y = (__typeof__(x))x; }",
    ] {
        assert_accepted(src);
    }
    for src in [
        "struct S { int a; }; struct T { int a; }; struct S g(struct T t) { return (struct S)t; }",
        "struct S { int a; }; struct S g(int i) { return (struct S)i; }",
        "struct S { int a; }; void g(struct S s, struct S t) { (struct S)s = t; }",
        "struct S { int a; }; void *g(struct S s) { return &(struct S)s; }",
        "struct S; struct S *p; void g(void) { (struct S)*p; }",
    ] {
        assert_rejected(src);
    }
}
