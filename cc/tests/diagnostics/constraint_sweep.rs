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

use crate::common::{
    compile_and_run, compile_expect_error, compile_expect_ok, compile_expect_warning,
};

fn rejects(cases: &[(&str, &str, &str)]) {
    for (name, src, expected) in cases {
        compile_expect_error(name, src, expected);
    }
}

/// A scalar's initializer may be braced (C17 6.7.9p11). For a member of an
/// automatic structure the braced form reached the linearizer as an untyped
/// list -- an internal compiler error. More than one level is gcc's warning.
#[test]
fn braced_scalar_initializers() {
    let src = r#"
struct S { int a : 3; int b; };
int main(void) {
    struct S s = {{1}, 2};
    struct S t = {.a = {3}};
    double d = {2.5};
    union U { int i; float f; } u = {{7}};
    int arr[2] = {{1}, {2}};
    int x = {{9}};
    return (s.a == 1 && s.b == 2 && t.a == 3 && d == 2.5 && u.i == 7 && arr[1] == 2 && x == 9)
        ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("braced_scalars", src, &["-w".to_string()]),
        0
    );
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
    let src = r#"
typedef int A[3];
const A x = {1, 2, 3};
volatile A v;
int main(void) {
    return _Generic(&x[0], const int *: 0, int *: 1) + _Generic(&v[0], volatile int *: 0, int *: 2);
}
"#;
    assert_eq!(compile_and_run("const_array_typedef", src, &[]), 0);
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
    let src = r#"
int main(void) {
    if (!(18446744073709551615 > 0)) return 1;
    if (sizeof(18446744073709551615) != 16) return 2;
    if (!(9223372036854775808 > 0)) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("huge_decimal", src, &["-w".to_string()]), 0);
    compile_expect_warning(
        "huge_decimal_warns",
        "unsigned long long x = 18446744073709551615;\n",
        "integer constant is so large that it is unsigned",
    );
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

/// Only a structure or union *specifier* makes an anonymous member
/// (6.7.2.1p13); a typedef name of one declares nothing, as gcc reads it.
/// Taking it as a member changed the layout.
#[test]
fn typedef_struct_is_not_an_anonymous_member() {
    let src = r#"
typedef struct { int a; } T;
struct S { T; int b; };
struct G { struct { int a; }; const struct { int c; }; int d; };
int main(void) {
    return (sizeof(struct S) == sizeof(int) && sizeof(struct G) == 3 * sizeof(int)) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("typedef_member", src, &["-w".to_string()]),
        0
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
