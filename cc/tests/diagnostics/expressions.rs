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

use crate::common::{
    compile_and_run, compile_expect_error, compile_expect_ok, compile_expect_warning,
};

fn rejects(cases: &[(&str, &str, &str)]) {
    for (name, src, expected) in cases {
        compile_expect_error(name, src, expected);
    }
}

/// C17 6.7.9p7: a designator names a member, or an index, of the object
/// being initialized -- followed into nested lists.
#[test]
fn designators_must_name_something() {
    rejects(&[
        (
            "des_unknown",
            "struct P { int x; int z; } p = { .y = 1, 7 };\n",
            "'struct P' has no member named 'y'",
        ),
        (
            "des_unknown_block",
            "struct P { int x; };\nvoid f(void) { struct P p = { .q = 1 }; (void)p; }\n",
            "'struct P' has no member named 'q'",
        ),
        (
            "des_nested",
            "struct I { int a; };\nstruct O { struct I in; } o = { .in = { .b = 2 } };\n",
            "'struct I' has no member named 'b'",
        ),
        (
            "des_positional_nested",
            "struct I { int a; };\nstruct O { int n; struct I in; } o = { 1, { .zz = 2 } };\n",
            "'struct I' has no member named 'zz'",
        ),
        (
            "des_field_in_array",
            "int a[2] = { .x = 1 };\n",
            "field name not in record or union initializer",
        ),
        (
            "des_index_in_struct",
            "struct S { int a; } s = { [0] = 1 };\n",
            "array index in non-array initializer",
        ),
        (
            "des_compound_literal",
            "struct P { int x; };\nint f(void) { return (struct P){ .w = 1 }.x; }\n",
            "'struct P' has no member named 'w'",
        ),
    ]);
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
fn member_access_and_casts() {
    rejects(&[
        (
            "arrow_on_struct",
            "struct S { int a; };\nint f(struct S s) { return s->a; }\n",
            "invalid type argument of '->' (have 'struct S')",
        ),
        (
            "cast_to_struct",
            "struct S { int a, b; };\nvoid f(int x) { struct S s = (struct S)x; (void)s; }\n",
            "conversion to non-scalar type requested",
        ),
        (
            "cast_from_struct",
            "struct S { int a; };\nint f(struct S s) { return (int)s; }\n",
            "aggregate value used where an integer was expected",
        ),
        (
            "cast_ptr_to_double",
            "double f(int *p) { return (double)p; }\n",
            "pointer value used where a floating-point was expected",
        ),
        (
            "cast_double_to_ptr",
            "int *f(double d) { return (int *)d; }\n",
            "cannot convert to a pointer type",
        ),
        (
            "assign_const_member",
            "struct S { const int a; int b; };\nvoid f(struct S *s, struct S t) { *s = t; }\n",
            "assignment of a structure or union with a read-only member",
        ),
    ]);
    compile_expect_ok(
        "casts_legal",
        "struct S { int a; } arr[2];\nint f(void) { return arr->a; }\n\
         void *v(long l) { return (void *)l; }\nlong w(void *p) { return (long)p; }\n\
         void x(struct S s) { (void)s; }\nint (*y(void *p))(void) { return (int (*)(void))p; }\n",
    );
}

#[test]
fn bit_fields_have_no_address_or_size() {
    rejects(&[
        (
            "bf_address",
            "struct S { int a : 3; } s;\nint *p = &s.a;\n",
            "cannot take address of bit-field 'a'",
        ),
        (
            "bf_address_arrow",
            "struct S { int a : 3; };\nint *f(struct S *s) { return &s->a; }\n",
            "cannot take address of bit-field 'a'",
        ),
        (
            "bf_sizeof",
            "struct S { int a : 3; } s;\nint n = sizeof(s.a);\n",
            "'sizeof' applied to a bit-field",
        ),
        (
            "bf_alignof",
            "struct S { int a : 3; } s;\nint n = _Alignof(s.a);\n",
            "'_Alignof' applied to a bit-field",
        ),
        (
            "bf_offsetof",
            "#include <stddef.h>\nstruct S { int x; int a : 3; };\nint n = offsetof(struct S, a);\n",
            "attempt to take address of bit-field structure member 'a'",
        ),
        (
            "bf_atomic",
            "struct { _Atomic int a : 3; } s;\n",
            "bit-field has atomic type",
        ),
        (
            "bf_bool_wide",
            "struct { _Bool a : 2; } s;\n",
            "exceeds type size 1",
        ),
    ]);
    compile_expect_ok(
        "bf_legal",
        "struct S { int a : 3; _Bool b : 1; int c; } s;\nint *p = &s.c;\nint n = sizeof(s.c);\n",
    );
}

#[test]
fn type_names_c_forbids() {
    rejects(&[
        (
            "gen_function",
            "int n = _Generic(1, int(void): 1, default: 0);\n",
            "'_Generic' association has function type",
        ),
        (
            "gen_incomplete",
            "struct S;\nint n = _Generic(1, struct S: 1, default: 0);\n",
            "'_Generic' association has incomplete type",
        ),
        (
            "alignof_incomplete",
            "struct S;\nint n = _Alignof(struct S);\n",
            "invalid application of '_Alignof' to incomplete type 'struct S'",
        ),
        (
            "alignof_unsized",
            "int n = _Alignof(int[]);\n",
            "invalid application of '_Alignof' to incomplete type",
        ),
        (
            "atomic_const",
            "_Atomic(const int) x;\n",
            "'_Atomic' applied to a qualified type",
        ),
        (
            "atomic_atomic",
            "_Atomic(_Atomic int) x;\n",
            "'_Atomic' applied to a qualified type",
        ),
        (
            "alignas_member_weaker",
            "struct { _Alignas(1) int x; } s;\n",
            "'_Alignas' specifiers cannot reduce alignment of 'x'",
        ),
        (
            "alignas_huge",
            "_Alignas(1 << 29) char x;\n",
            "requested alignment '536870912' exceeds object file maximum",
        ),
        (
            "array_param_inner_static",
            "void f(int a[3][static 4]);\n",
            "static or type qualifiers in non-parameter array declarator",
        ),
        (
            "array_param_inner_const",
            "void f(int a[3][const 4]);\n",
            "static or type qualifiers in non-parameter array declarator",
        ),
        (
            "duplicate_via_anonymous",
            "struct S { int a; struct { int a; }; };\n",
            "duplicate member 'a'",
        ),
        (
            "duplicate_into_anonymous",
            "struct S { union { int b; }; int b; };\n",
            "duplicate member 'b'",
        ),
        (
            "void_not_only_parameter",
            "void f(int, void);\n",
            "'void' must be the only parameter",
        ),
    ]);
    compile_expect_ok(
        "type_names_legal",
        "int n = _Generic(1, int: 1, void (*)(void): 2, default: 0);\n\
         _Atomic(int) a;\nstruct { _Alignas(8) int x; } s;\nint m = _Alignof(void);\n\
         void f(int a[static 3], int b[const 4]);\nstruct T { int a; struct { int b; }; };\n",
    );
}

#[test]
fn literal_constraints() {
    rejects(&[
        ("empty_char", "int c = '';\n", "empty character constant"),
        (
            "ucn_out_of_range",
            "unsigned u = U'\\U00110000';\n",
            "\\U00110000 is not a valid universal character",
        ),
        (
            "u8_with_wide",
            "void *p = L\"a\" u8\"b\";\n",
            "concatenation of string literals with different encoding prefixes",
        ),
    ]);
    compile_expect_warning(
        "string_too_long",
        "char s[2] = \"abc\";\n",
        "initializer-string for array of 'char' is too long",
    );
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
    let msg = "concatenation of string literals with different encoding prefixes";
    rejects(&[
        (
            "u8_then_wide",
            "unsigned long x = sizeof(u8\"a\" L\"b\");\n",
            msg,
        ),
        (
            "u8_then_u16",
            "unsigned long x = sizeof(u8\"a\" u\"b\");\n",
            msg,
        ),
        (
            "u32_then_u8",
            "unsigned long x = sizeof(U\"a\" u8\"b\");\n",
            msg,
        ),
        (
            "plain_u8_wide",
            "unsigned long x = sizeof(\"a\" u8\"b\" L\"c\");\n",
            msg,
        ),
    ]);
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
    compile_expect_error(
        "alignof_unsized_array",
        "int f(void) { return _Alignof(int[]); }\n",
        "invalid application of '_Alignof' to incomplete type 'int[]'",
    );
}

/// A cast to the operand's own structure type is gcc's extension, accepted
/// whatever either side's qualifiers; its value is a copy, not the operand.
/// Any other operand is still "conversion to non-scalar type requested".
#[test]
fn cast_to_the_operands_own_structure_type() {
    rejects(&[
        (
            "cast_struct_other",
            "struct S { int a; };\nstruct T { int a; };\n\
             struct S g(struct T t) { return (struct S)t; }\n",
            "conversion to non-scalar type requested",
        ),
        (
            "cast_struct_int",
            "struct S { int a; };\nstruct S g(int i) { return (struct S)i; }\n",
            "conversion to non-scalar type requested",
        ),
        (
            "cast_struct_not_lvalue",
            "struct S { int a; };\nvoid g(struct S s, struct S t) { (struct S)s = t; }\n",
            "lvalue required as left operand of assignment",
        ),
    ]);
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
