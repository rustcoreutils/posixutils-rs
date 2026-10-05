//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Expression, initializer and type-name constraints of C17 that c17 let
// through. Several compiled into something wrong rather than nothing: an
// unknown designator dropped its value, `s->a` on a structure loaded through
// garbage, a cast reinterpreted a structure as an integer.
//

use crate::common::compile_and_run;

/// C17 6.7.9p7: a designator names a member, or an index, of the object
/// being initialized -- followed into nested lists.
#[test]
fn designators_must_name_something() {
    let src = r#"
struct I { int a, b; };
struct O { int n; struct I in; struct { int u; }; int arr[3]; char s[4]; };
struct O o = { 1, { .b = 2 }, .u = 3, .arr = { [1] = 4 }, "abc" };
int main(void) {
    struct O l = { .in.a = 5, .arr[2] = 6, .s = "xy" };
    return (o.n + o.in.b + o.u + o.arr[1] + l.in.a + l.arr[2] == 21 && o.s[2] == 'c') ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("des_legal", src, &[]), 0);
}

#[test]
fn literal_constraints() {
    // Exactly full, the terminator dropped, is C.
    let src = "char s[3] = \"abc\";\nchar t[] = u8\"x\" \"y\";\n\
               int main(void) { return (s[2] == 'c' && sizeof t == 3) ? 0 : 1; }\n";
    assert_eq!(compile_and_run("string_exact", src, &[]), 0);
}

/// C17 6.4.5p2: a `u8` literal joins a plain one but no other prefix, in
/// either order -- `u8"a" L"b"` was taken as wide because the `u8` check ran
/// before the second piece's prefix was recorded.
#[test]
fn utf8_string_concatenation_with_another_prefix() {
    let src = "char a[] = u8\"x\" u8\"y\";\nchar b[] = u8\"x\" \"y\";\n\
               char c[] = \"x\" u8\"y\";\n\
               int main(void) { return sizeof a + sizeof b + sizeof c == 9 ? 0 : 1; }\n";
    assert_eq!(compile_and_run("u8_concat_ok", src, &[]), 0);
}

/// `_Alignof` of a variable length array type: the extent is written, so the
/// type is complete (C17 6.5.3.4p1), and its alignment is its element's.
#[test]
fn alignof_variable_length_array_type() {
    let src = "int f(int n) { return _Alignof(int[n]) + _Alignof(double[n][4]); }\n\
               int main(void) { return f(3) == 4 + _Alignof(double) ? 0 : 1; }\n";
    assert_eq!(compile_and_run("alignof_vla_type", src, &[]), 0);
}

/// A cast to the operand's own structure type is gcc's extension, accepted
/// whatever either side's qualifiers; its value is a copy, not the operand.
/// Any other operand is still "conversion to non-scalar type requested".
#[test]
fn cast_to_the_operands_own_structure_type() {
    let src = "struct S { int a; double d; char c[20]; };\n\
               struct T { long x; };\n\
               static struct S id(struct S s) { return (struct S)s; }\n\
               static struct S fromp(const struct S *p) { return (struct S)*p; }\n\
               static struct S mk(int a) { struct S s = { a, a * 1.5, \"hello\" }; return s; }\n\
               int main(void) {\n\
                   struct S s = mk(7);\n\
                   struct S t = id(s);\n\
                   if (t.a != 7 || t.d != 10.5 || t.c[4] != 'o') return 1;\n\
                   t = fromp(&s);\n\
                   s.a = 100;\n\
                   if (t.a != 7) return 2;\n\
                   if (((struct S)mk(9)).a != 9) return 3;\n\
                   const struct S cs = mk(4);\n\
                   struct S u = (struct S)cs;\n\
                   u.a++;\n\
                   if (u.a != 5 || cs.a != 4) return 4;\n\
                   struct T small = { 42 };\n\
                   struct T v = (const struct T)small;\n\
                   if (v.x != 42) return 5;\n\
                   union U { int i; float f; } w = { 3 };\n\
                   if (((union U)w).i != 3) return 6;\n\
                   return 0;\n\
               }\n";
    assert_eq!(compile_and_run("cast_struct_self", src, &[]), 0);
}
