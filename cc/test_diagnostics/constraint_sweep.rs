//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Findings of a broad differential sweep against gcc: an internal compiler
// error on valid C, several programs compiled into the wrong thing, and the
// constraint violations around them.
//

use crate::test_compile::{
    compile_expect_error, compile_expect_no_diagnostic, compile_expect_ok, compile_expect_warning,
};

fn rejects(cases: &[(&str, &str, &str)]) {
    for (name, src, expected) in cases {
        compile_expect_error(name, src, expected);
    }
}

/// An identifier-list definition after a prototype must agree with it once
/// promoted (6.7.6.3p15) -- `f(3)` read an `int` as a `double` -- and its
/// declaration list declares each listed identifier once (6.9.1p6).
#[test]
fn identifier_list_definitions() {
    rejects(&[
        (
            "knr_mismatch",
            "int f(int);\nint f(a) double a; { return (int)a; }\n",
            "argument 'a' doesn't match prototype",
        ),
        (
            "knr_count",
            "int f(int, int);\nint f(a) int a; { return a; }\n",
            "number of arguments doesn't match prototype",
        ),
        (
            "knr_not_listed",
            "int f(a) int a, b; { return a; }\n",
            "declaration for parameter 'b' but no such parameter",
        ),
        (
            "knr_twice",
            "int f(a) int a; int a; { return a; }\n",
            "redefinition of parameter 'a'",
        ),
        (
            "knr_void",
            "int f(a) void a; { return 0; }\n",
            "parameter 'a' declared with void type",
        ),
    ]);
    compile_expect_warning(
        "knr_implicit_int",
        "int f(a) { return a; }\n",
        "type of 'a' defaults to 'int'",
    );
    compile_expect_ok(
        "knr_legal",
        "int f(char);\nint f(c) char c; { return c; }\n\
         int g(float);\nint g(x) float x; { return x; }\n\
         int h(a, b) int a; long b; { return a + (int)b; }\n",
    );
}

#[test]
fn void_expressions_have_no_value() {
    rejects(&[
        (
            "void_cast",
            "void g(void);\nint f(void) { return (int)g(); }\n",
            "invalid use of void expression",
        ),
        (
            "void_argument",
            "void v(void);\nint g();\nvoid f(void) { g(v()); }\n",
            "invalid use of void expression",
        ),
    ]);
    compile_expect_ok(
        "void_to_void",
        "void g(void);\nvoid f(void) { (void)g(); }\n",
    );
}

/// C17 6.5.6p2: pointer arithmetic needs a complete pointee, whose size is
/// the step -- it was zero, and `p - q` divided by it.
#[test]
fn arithmetic_on_pointer_to_incomplete() {
    for (name, body) in [
        ("inc_add", "struct S *f(struct S *p) { return p + 1; }"),
        (
            "inc_sub",
            "long f(struct S *p, struct S *q) { return p - q; }",
        ),
        ("inc_step", "void f(struct S *p) { p++; }"),
        ("inc_index", "struct S *f(struct S *p) { return &p[1]; }"),
    ] {
        compile_expect_error(
            name,
            &format!("struct S;\n{body}\n"),
            "arithmetic on pointer to an incomplete type",
        );
    }
    compile_expect_ok(
        "inc_legal",
        "struct S { int a; };\nstruct S *f(struct S *p) { p++; return p + 1; }\n\
         void *g(void *v) { return v + 1; }\n\
         long h(int n, int (*p)[n]) { return (p + 1) - p; }\n",
    );
}

#[test]
fn remaining_constraints() {
    rejects(&[
        (
            "ucn_incomplete",
            "int c = '\\u12';\n",
            "incomplete universal character name \\u12",
        ),
        (
            "def_incomplete_return",
            "struct S;\nstruct S f(void) {}\n",
            "return type is an incomplete type",
        ),
        (
            "def_through_typedef",
            "typedef void F(void);\nF f {}\n",
            "function definition declared through a typedef",
        ),
        (
            "call_incomplete_return",
            "struct S;\nstruct S g(void);\nvoid f(void) { g(); }\n",
            "invalid use of undefined type 'struct S'",
        ),
        (
            "complex_to_pointer",
            "int *f(double _Complex z) { return (int *)z; }\n",
            "cannot convert to a pointer type",
        ),
        (
            "qualified_void_parameter",
            "void f(const void);\n",
            "'void' as only parameter may not be qualified",
        ),
        (
            "nested_init_type",
            "struct S { int a, b; };\nstruct S g(void);\nvoid f(void) { struct S s = {g()}; (void)s; }\n",
            "incompatible types when initializing type 'int' using type 'struct S'",
        ),
    ]);
    for (name, src, expected) in [
        (
            "union_excess",
            "union U { int a; float b; } u = {1, 2};\n",
            "excess elements in union initializer",
        ),
        (
            "nested_array_excess",
            "struct { int a[2]; } s = {{1, 2, 3}};\n",
            "excess elements in array initializer",
        ),
    ] {
        compile_expect_warning(name, src, expected);
    }
}

/// Brace elision around string literals and braced elements (C17 6.7.9p20):
/// each element is checked against the subobject it initializes, so none of
/// these valid initializers -- all accepted by gcc -- may be diagnosed.
#[test]
fn brace_elision_initializers_accepted() {
    for (name, src) in [
        (
            "elide_string_then_double",
            "struct In { char s[4]; double d; };\n\
             struct Out { struct In in; int *p; } v = {\"abc\", 2.5, 0};\n",
        ),
        (
            "elide_braced_designated_row",
            "struct Pt { int x, y; };\nstruct Pt grid[2][2] = {1, 2, {.y = 5}, 7};\n",
        ),
        (
            "elide_wide_string_rows",
            "#include <stddef.h>\nstruct In { wchar_t s[3]; int n; };\n\
             struct In a[2] = {L\"ab\", 3, L\"c\", 4};\n",
        ),
        (
            "elide_designated_member",
            "struct In { char s[4]; int n; };\n\
             struct O { int a; struct In in; struct In j; } o = {.in = \"abc\", 7, \"de\", 8};\n",
        ),
        (
            "elide_union_member",
            "union U { struct { char s[4]; int n; } in; int k; } u = {\"uv\", 4};\n",
        ),
        (
            "braced_string_member",
            "struct S { char s[4]; int n; } s = {{\"abc\"}, 1};\nchar c[4] = {\"abc\"};\n",
        ),
    ] {
        compile_expect_ok(name, src);
        for forbidden in ["warning", "error"] {
            compile_expect_no_diagnostic(name, src, forbidden);
        }
    }
    // An array of integers takes a string literal whole, as gcc does, and
    // rejects one that does not suit it -- rather than eliding braces and
    // storing the string's address into its first element.
    rejects(&[
        (
            "string_for_short_array_member",
            "struct S { short h[3]; int n; } s = {\"ab\", 1};\n",
            "invalid initializer",
        ),
        (
            "string_for_bool_array_member",
            "struct S { _Bool b[3]; int n; } s = {\"ab\", 1};\n",
            "invalid initializer",
        ),
    ]);
    // Excess elements are counted subobject by subobject.
    for (name, src, expected) in [
        (
            "elided_union_excess",
            "union U { struct { int x, y; } p; int n; } u = {1, 2, 3};\n",
            "excess elements in union initializer",
        ),
        (
            "elided_struct_excess",
            "struct In { char s[4]; int n; } x = {\"ab\", 1, 2};\n",
            "excess elements in struct initializer",
        ),
        (
            "elided_array_excess",
            "struct Pt { int x, y; };\nstruct Pt a[1] = {1, 2, 3};\n",
            "excess elements in array initializer",
        ),
        (
            "member_string_too_long",
            "struct S { char s[2]; int n; } s = {\"abc\", 1};\n",
            "initializer-string for array of 'char' is too long",
        ),
    ] {
        compile_expect_warning(name, src, expected);
    }
}

/// Positional elements after a designator chain continue inside the chain's
/// aggregate (C17 6.7.9p17), so they are checked -- and counted -- against
/// the subobjects they really land in.
#[test]
fn designator_chain_continuation() {
    for (name, src) in [
        (
            "chain_then_pointer",
            "struct Pt { int x, y; };\n\
             struct S { struct Pt a; double *p; } s = {.a.x = 1, 2, 0};\n",
        ),
        (
            "chain_fills_enclosing",
            "struct Pt { int x, y; };\nstruct E2 { struct Pt a; int t[2]; };\n\
             struct E2 e = {.a.x = 1, 2, 3, 4};\n",
        ),
        (
            "array_chain_elides",
            "struct Pt { int x, y; };\nstruct Pt pc[2][2] = {[1][0] = 5, 6};\n",
        ),
    ] {
        compile_expect_ok(name, src);
        for forbidden in ["warning", "error"] {
            compile_expect_no_diagnostic(name, src, forbidden);
        }
    }
    for (name, src, expected) in [
        (
            "chain_struct_excess",
            "struct Pt { int x, y; };\nstruct E2 { struct Pt a; int t[2]; };\n\
             struct E2 e = {.a.x = 1, 2, 3, 4, 5};\n",
            "excess elements in struct initializer",
        ),
        (
            "chain_array_excess",
            "struct Pt { int x, y; };\nstruct Pt p[1] = {[0].x = 1, 2, 3};\n",
            "excess elements in array initializer",
        ),
        (
            "designated_union_excess",
            "union U { int i; float f; } u = {.f = 1.0f, 2};\n",
            "excess elements in union initializer",
        ),
        (
            "union_chain_excess",
            "struct UZ { union { int i; float f; } u; int z; } s = {.u.f = 1.0f, 2, 3};\n",
            "excess elements in struct initializer",
        ),
        (
            "array_chain_row_excess",
            "struct Pt { int x, y; };\nstruct Pt g[2][2] = {[1][1] = 5, 6, 7};\n",
            "excess elements in array initializer",
        ),
    ] {
        compile_expect_warning(name, src, expected);
    }
}

/// A scalar's initializer may be braced (C17 6.7.9p11). For a member of an
/// automatic structure the braced form reached the linearizer as an untyped
/// list -- an internal compiler error. More than one level is gcc's warning.
#[test]
fn braced_scalar_initializers() {
    compile_expect_warning(
        "braced_scalar_twice",
        "void f(void) { int x = {{9}}; (void)x; }\n",
        "braces around scalar initializer",
    );
}

/// C17 6.7.3p10: a qualifier on an array typedef qualifies its elements. It
/// was dropped, so `const A x` was writable and its elements plain `int`.
#[test]
fn qualified_array_typedef() {
    rejects(&[
        (
            "const_array_typedef_write",
            "typedef int A[3];\nconst A x = {1, 2, 3};\nvoid f(void) { x[0] = 1; }\n",
            "read-only",
        ),
        (
            "const_array_typedef_param",
            "typedef int A[3];\nvoid f(const A a) { a[0] = 1; }\n",
            "read-only",
        ),
    ]);
}

/// C17 6.4.4.1p6: a decimal constant no signed type holds has none; gcc
/// makes it `__int128` with a warning. It wrapped to a negative `long long`.
#[test]
fn decimal_constant_too_large_for_long_long() {
    compile_expect_warning(
        "huge_decimal_warns",
        "unsigned long long x = 18446744073709551615;\n",
        "integer constant is so large that it is unsigned",
    );
}
