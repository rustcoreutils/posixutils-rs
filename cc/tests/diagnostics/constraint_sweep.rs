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

use crate::common::compile_and_run;

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
