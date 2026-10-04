//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of the c99 suite (tests/c99), in process.
//

use crate::test_compile::{
    compile_expect_error, compile_expect_ok, compile_expect_warning, compile_rejected,
};

// ============================================================================
// tests/c99/features.rs:
// C99 Features Mega-Test
//
// Consolidates: VLA, inline, varargs, array_param_qualifiers tests
// ============================================================================

// ============================================================================
// Translation limits
// ============================================================================

/// Where gcc lets a flexible array member be initialized (a GNU extension),
/// case by case as gcc 13 decides it: a list of elements only at the static
/// object's own top level, a string literal anywhere but in an array element,
/// `{}` anywhere static, and nothing at all in an automatic object.
#[test]
fn c99_flexible_array_member_initializer_placement_matches_gcc() {
    const NESTED: &str = "initialization of flexible array member in a nested context";
    const NON_STATIC: &str = "non-static initialization of a flexible array member";
    let v = "struct V { int n; const char s[]; };\n\
             struct W { int n; int a[]; };\n\
             struct O { int k; struct V v; };\n\
             struct P { int k; struct W w; };\n";
    let rejected: &[(&str, &str, &str)] = &[
        (
            "fam_empty_string_in_element",
            "static struct V a[] = { { 1, \"\" } };",
            NESTED,
        ),
        (
            "fam_designated_in_element",
            "static struct V a[] = { { .s = \"x\" } };",
            NESTED,
        ),
        (
            "fam_subscript_in_element",
            "static struct V a[] = { [0].s[0] = 'a' };",
            NESTED,
        ),
        (
            "fam_string_in_member_in_element",
            "static struct O a[] = { { 1, { 2, \"z\" } } };",
            NESTED,
        ),
        (
            "fam_braced_chars_in_member",
            "static struct O o = { 1, { 2, { 'a' } } };",
            NESTED,
        ),
        (
            "fam_braced_ints_in_member",
            "static struct P p = { 1, { 2, { 3 } } };",
            NESTED,
        ),
        (
            "fam_elided_ints_in_member",
            "static struct P p = { 1, 2, 3 };",
            NESTED,
        ),
        (
            "fam_elided_ints_in_element",
            "static struct W a[] = { 1, 2, 3, 4 };",
            NESTED,
        ),
        (
            "fam_automatic",
            "void f(void) { struct V a = { 1, \"x\" }; (void)a; }",
            NON_STATIC,
        ),
        (
            "fam_automatic_empty",
            "void f(void) { struct V a = { 1, {} }; (void)a; }",
            NON_STATIC,
        ),
        (
            "fam_automatic_in_element",
            "void f(void) { struct V a[] = { { 1, \"x\" } }; (void)a; }",
            NON_STATIC,
        ),
        (
            "fam_block_compound_literal",
            "void f(void) { const struct V *p = &(struct V){ 1, \"x\" }; (void)p; }",
            NON_STATIC,
        ),
    ];
    for (name, body, expected) in rejected {
        compile_expect_error(name, &format!("{v}{body}\n"), expected);
    }
    let accepted: &[(&str, &str)] = &[
        (
            "fam_absent_in_element",
            "static struct V a[] = { { 1 }, { 2 } };",
        ),
        (
            "fam_empty_braces_in_element",
            "static struct V a[] = { { 1, {} } };",
        ),
        (
            "fam_string_in_member",
            "static struct O o = { 1, { 2, \"z\" } };",
        ),
        (
            "fam_braced_string_in_member",
            "static struct O o = { 1, { 2, { \"z\" } } };",
        ),
        (
            "fam_empty_in_member",
            "static struct P p = { 1, { 2, {} } };",
        ),
        (
            "fam_static_local",
            "void f(void) { static struct V a = { 1, \"x\" }; (void)a; }",
        ),
        (
            "fam_automatic_absent",
            "void f(void) { struct V a = { 1 }; (void)a; }",
        ),
        (
            "fam_file_compound_literal",
            "const struct V *p = &(struct V){ 1, \"x\" };",
        ),
    ];
    for (name, body) in accepted {
        compile_expect_ok(name, &format!("{v}{body}\n"));
    }
}

/// One diagnostic per element, and nothing else: the rejected member's bytes
/// are skipped rather than laid out, so no further error follows.
#[test]
fn c99_flexible_array_member_nested_initializer_reports_each_element_once() {
    let stderr = compile_rejected(
        "fam_nested_once",
        "struct V { int n; const char s[]; };\n\
         static const struct V arr[] = {\n\
           { 1, \"x\" },\n\
           { 2, \"yy\" },\n\
           { 3, \"zzz\" }\n\
         };\n",
    );
    let lines: Vec<&str> = stderr.lines().filter(|l| l.contains("error")).collect();
    assert_eq!(lines.len(), 3, "stderr:\n{stderr}");
    for (line, at) in lines.iter().zip([":3:", ":4:", ":5:"]) {
        assert!(line.contains(at), "{line} should be reported at line {at}");
    }
}

// ============================================================================
// tests/c99/func_name.rs:
// `__func__` (C17 6.4.2.2): implicitly `static const char __func__[] =
// "function-name";`. c17 typed it `char *`, so `sizeof __func__` was the size
// of a pointer, `_Generic` picked `char *`, and writing through it was
// accepted; with an asm label it held the label rather than the name.
// ============================================================================

#[test]
fn func_name_is_read_only() {
    compile_expect_error(
        "func_name_write",
        "void g(void) { __func__[0] = 'x'; }\n",
        "read-only",
    );
}

/// Outside a function gcc warns, and the name is empty.
#[test]
fn func_name_outside_a_function() {
    compile_expect_warning(
        "func_name_file_scope",
        "const char *p = __func__;\nint n = sizeof __func__;\n",
        "'__func__' is not defined outside of function scope",
    );
}

// ============================================================================
// tests/c99/machine_modes.rs:
// The `mode` attribute's named modes: `byte` and `unwind_word` were left
// unimplemented, warned, and kept the declared type's size -- 4 bytes where
// gcc gives 1 and 8.
// ============================================================================

#[test]
fn mode_byte_and_unwind_word() {
    compile_expect_ok(
        "mode_byte_unwind_word",
        "typedef int b __attribute__((mode(byte)));\n\
         typedef unsigned ub __attribute__((__mode__(__byte__)));\n\
         typedef int uw __attribute__((mode(unwind_word)));\n\
         typedef unsigned uuw __attribute__((__mode__(__unwind_word__)));\n\
         _Static_assert(sizeof(b) == 1 && sizeof(ub) == 1, \"byte\");\n\
         _Static_assert(sizeof(uw) == 8 && sizeof(uuw) == 8, \"unwind_word\");\n\
         _Static_assert((b)-1 < 0 && (ub)-1 == 255 && (uuw)-1 > 0, \"signedness\");\n",
    );
}

// ============================================================================
// tests/c99/types.rs:
// C99 Types Mega-Test
//
// Consolidates: longlong, bool, complex tests
// ============================================================================

// ============================================================================
// Declarations that must be accepted
// ============================================================================

/// What gcc rejects around a packed enum: an attribute between the tag and
/// `{`, an empty enumerator list (C17 6.7.2.2p1 does not make it optional),
/// and a bit-field wider than the enum's one byte.
#[test]
fn c99_packed_enum_rejections() {
    for (name, code, expected) in [
        (
            "packed_enum_after_tag",
            "enum B __attribute__((packed)) { B0 };",
            "expected identifier",
        ),
        ("empty_enum", "enum E { };", "empty enum is invalid"),
        (
            "empty_packed_enum",
            "enum __attribute__((packed)) E { };",
            "empty enum is invalid",
        ),
        (
            "packed_enum_bitfield_too_wide",
            "enum __attribute__((packed)) A { A0 }; struct S { enum A a : 9; };",
            "bitfield width 9 exceeds type size 8",
        ),
    ] {
        compile_expect_error(name, &format!("{code}\n"), expected);
    }
}

/// An array's element type must be a complete object type (C17 6.7.6.2p1),
/// wherever the array type is written: a declaration, a pointer to the
/// array, a parameter that adjusts to a pointer, a member, a typedef, or a
/// type name in `sizeof`, `_Alignof`, a cast, a compound literal or `va_arg`
/// -- and when the tag is completed later in the unit. gcc rejects each with
/// "array type has incomplete element type"; an array of `void` or of
/// functions is "declaration of 'x' as array of voids" ("of type name" when
/// abstract). c17 accepted them all.
#[test]
fn c99_array_of_incomplete_element_type_is_rejected() {
    for (name, code, expected) in [
        (
            "arr_incomplete_sizeof",
            "struct I; int n = sizeof(struct I[2]);",
            "array type has incomplete element type",
        ),
        (
            "arr_incomplete_cast",
            "struct I; void *p = (struct I(*)[2])0;",
            "array type has incomplete element type",
        ),
        (
            "arr_incomplete_alignof",
            "struct I; int f(void) { return _Alignof(struct I[3]); }",
            "array type has incomplete element type",
        ),
        (
            "arr_incomplete_ptr_decl",
            "struct I; struct I (*q)[2];",
            "array type has incomplete element type",
        ),
        (
            "arr_incomplete_extern",
            "struct I; extern struct I a[2];",
            "array type has incomplete element type",
        ),
        ("arr_of_void", "int n = sizeof(void[2]);", "array of voids"),
        (
            "arr_incomplete_va_arg",
            "#include <stdarg.h>\nstruct I;\n\
             void f(int n, ...) { va_list ap; va_start(ap, n); va_arg(ap, struct I[2]); }",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_completed_later",
            "struct I; struct I (*q)[2]; struct I { int x; };",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_tentative_completed_later",
            "struct I; struct I a[2]; struct I { int x; };",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_unsized",
            "struct I; extern struct I a[];",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_param",
            "struct I; void f(struct I a[2]);",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_param_unsized",
            "struct I; void f(struct I a[]);",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_param_abstract",
            "struct I; void f(struct I (*)[2]);",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_param_definition",
            "struct I; void f(struct I a[2]) {}",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_typedef_name",
            "struct I; typedef struct I T; T a[2];",
            "array type has incomplete element type",
        ),
        (
            "arr_incomplete_typedef_of_array",
            "struct I; typedef struct I T[2];",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_union",
            "union U; extern union U a[2];",
            "array type has incomplete element type 'union U'",
        ),
        (
            "arr_incomplete_enum",
            "enum E; extern enum E a[2];",
            "array type has incomplete element type 'enum E'",
        ),
        (
            "arr_incomplete_member",
            "struct I; struct S { struct I m[2]; };",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_self_member",
            "struct S { struct S a[2]; };",
            "array type has incomplete element type 'struct S'",
        ),
        (
            "arr_incomplete_block_pointer",
            "struct I; void f(void) { struct I (*p)[2]; }",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_vla_pointer",
            "struct I; void f(int n) { struct I (*p)[n]; }",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_incomplete_compound_literal",
            "struct I; void *g(void) { return (struct I[2]){0}; }",
            "array type has incomplete element type 'struct I'",
        ),
        (
            "arr_of_unsized_array",
            "extern int a[3][];",
            "array type has incomplete element type 'int[]'",
        ),
        (
            "arr_of_unsized_array_grouped",
            "extern int (a[2])[];",
            "array type has incomplete element type 'int[]'",
        ),
        (
            "arr_of_unsized_array_param",
            "void f(int a[3][]);",
            "array type has incomplete element type 'int[]'",
        ),
        (
            "arr_of_void_declaration",
            "extern void x[2];",
            "declaration of 'x' as array of voids",
        ),
        (
            "arr_of_void_grouped",
            "void (a[2]);",
            "declaration of 'a' as array of voids",
        ),
        (
            "arr_of_void_member",
            "struct S { void m[2]; };",
            "declaration of 'm' as array of voids",
        ),
        (
            "arr_of_void_param",
            "void f(void a[]);",
            "declaration of 'a' as array of voids",
        ),
        (
            "arr_of_void_type_name",
            "int n = sizeof(void[2]);",
            "declaration of type name as array of voids",
        ),
        (
            "arr_of_functions",
            "int a[2](void);",
            "declaration of 'a' as array of functions",
        ),
        (
            "arr_of_functions_grouped",
            "int (a[2])(void);",
            "declaration of 'a' as array of functions",
        ),
        (
            "arr_of_functions_param",
            "void f(int a[](void));",
            "declaration of 'a' as array of functions",
        ),
        (
            "arr_of_functions_type_name",
            "int n = sizeof(int[2](void));",
            "declaration of type name as array of functions",
        ),
    ] {
        compile_expect_error(name, &format!("{code}\n"), expected);
    }
}

/// The accept side of C17 6.7.6.2p1, each checked against gcc: an element
/// type completed before the array is formed, a pointer element, a variable
/// length or `[*]` extent (complete, though known only at run time), an
/// incomplete *outer* extent, a grouped declarator over its outer suffix, and
/// a qualified spelling of a tag completed after its first mention.
#[test]
fn c99_array_of_complete_element_type_is_accepted() {
    for (name, code) in [
        (
            "arr_ok_completed",
            "struct I { int x; }; extern struct I a[2];",
        ),
        (
            "arr_ok_completed_after_pointer",
            "struct I; struct I *p; struct I { int x; }; struct I (*q)[2];",
        ),
        (
            "arr_ok_qualified_completed",
            "struct I; struct I { int x; }; const struct I a[2]; \
             int n = sizeof(const struct I[2]);",
        ),
        (
            "arr_ok_enum",
            "enum E { A }; enum E e[2]; const enum E f[2];",
        ),
        ("arr_ok_self_pointer", "struct S { struct S *n; } a[2];"),
        ("arr_ok_pointer_to_incomplete", "struct I; struct I *a[2];"),
        (
            "arr_ok_outer_unsized",
            "extern int a[][3]; void f(int b[][3]);",
        ),
        (
            "arr_ok_grouped",
            "int (a[3]); int (b[2])[3]; void (*c[2])(void); int ((*d)[2]);\n\
             void (*(e[2]))(void); int (*(*g)(void))[3];",
        ),
        (
            "arr_ok_vla",
            "void f(int n, int m) { int a[n][m]; int (*p)[n]; typedef int V[n]; \
             V x[2]; int (*q)[n][m]; (void)a; (void)p; (void)x; (void)q; }",
        ),
        ("arr_ok_star", "void f(int n, int a[*][*]);"),
        (
            "arr_ok_va_arg",
            "#include <stdarg.h>\nvoid f(int n, ...) { va_list ap; va_start(ap, n); \
             int (*p)[2] = va_arg(ap, int(*)[2]); (void)p; va_end(ap); }",
        ),
        (
            "arr_ok_flexible_member",
            "struct F { int n; int a[]; }; struct F *p;",
        ),
    ] {
        compile_expect_ok(name, &format!("{code}\n"));
    }
}
