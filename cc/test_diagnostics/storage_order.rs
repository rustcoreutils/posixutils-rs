//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `scalar_storage_order` attribute and pragma: what gcc 13 refuses or
// warns about, in its words. Every case names an x86-64 Linux target, so it
// runs the same on any host.
//

use crate::test_compile::{compile_accepted, compile_rejected_with};

const LINUX: &str = "--target=x86_64-unknown-linux-gnu";

const BE: &str = "struct __attribute__((scalar_storage_order(\"big-endian\"))) B {\n\
                  int i;\n\
                  int arr[2];\n\
                  struct N { int n; } nested;\n\
                  void *p;\n\
                  };\n\
                  struct B g;\n";

#[track_caller]
fn expect_error(name: &str, src: &str, expected: &str) {
    let stderr = compile_rejected_with(name, src, &[LINUX]);
    assert!(
        stderr.contains(&format!("error: {expected}")),
        "'{name}': no error {expected:?}.\nstderr:\n{stderr}"
    );
}

#[track_caller]
fn expect_warning(name: &str, src: &str, expected: &str) {
    let stderr = compile_accepted(name, src, &[LINUX]);
    assert!(
        stderr.contains(&format!("warning: {expected}")),
        "'{name}': no warning {expected:?}.\nstderr:\n{stderr}"
    );
}

#[track_caller]
fn expect_clean(name: &str, src: &str) {
    let stderr = compile_accepted(name, src, &[LINUX]);
    assert!(
        !stderr.contains("storage order"),
        "'{name}': unexpected diagnostic.\nstderr:\n{stderr}"
    );
}

/// The address of a scalar stored in reverse order is gcc's error, however
/// the scalar is reached: a member, an array element, through `->`, and
/// where the address is never evaluated.
#[test]
fn diagnostics_storage_order_address_of_a_reversed_scalar() {
    const MSG: &str = "cannot take address of scalar with reverse storage order";
    let cases = [
        ("sso_addr_member", "int *f(void) { return &g.i; }\n"),
        ("sso_addr_element", "int *f(void) { return &g.arr[1]; }\n"),
        (
            "sso_addr_arrow",
            "int *f(struct B *p) { return &(p->i); }\n",
        ),
        (
            "sso_addr_sizeof",
            "unsigned long f(void) { return sizeof(&g.i); }\n",
        ),
        ("sso_addr_static", "int *q = &g.i;\n"),
    ];
    for (name, body) in cases {
        expect_error(name, &format!("{BE}{body}"), MSG);
    }
}

/// The addresses gcc allows: the struct's own, a nested struct's (which has
/// the order of its own type), a pointer member's (a pointer is not a scalar
/// here), and anything in a struct with the target's order.
#[test]
fn diagnostics_storage_order_addresses_gcc_allows() {
    expect_clean(
        "sso_addr_allowed",
        &format!(
            "{BE}struct B *a(void) {{ return &g; }}\n\
             struct N *b(void) {{ return &g.nested; }}\n\
             int *c(void) {{ return &g.nested.n; }}\n\
             void **d(void) {{ return &g.p; }}\n\
             struct __attribute__((scalar_storage_order(\"little-endian\"))) L {{ int v; }} l;\n\
             int *e(void) {{ return &l.v; }}\n"
        ),
    );
}

/// An array of reversed scalars may have its address taken, for a block
/// copy, with gcc's warning, which `-Wno-scalar-storage-order` silences.
#[test]
fn diagnostics_storage_order_address_of_a_reversed_array_warns() {
    let src = format!("{BE}void *f(void) {{ return &g.arr; }}\n");
    expect_warning(
        "sso_addr_array",
        &src,
        "address of array with reverse scalar storage order requested",
    );
    let stderr = compile_accepted(
        "sso_addr_array_quiet",
        &src,
        &[LINUX, "-Wno-scalar-storage-order"],
    );
    assert!(!stderr.contains("warning"), "stderr:\n{stderr}");
}

/// The attribute's argument must name an order, and there must be one.
#[test]
fn diagnostics_storage_order_attribute_argument() {
    const ONE_OF: &str =
        "attribute 'scalar_storage_order' argument must be one of 'big-endian' or 'little-endian'";
    expect_error(
        "sso_arg_word",
        "struct __attribute__((scalar_storage_order(\"middle\"))) A { int x; };\n",
        ONE_OF,
    );
    expect_error(
        "sso_arg_int",
        "struct __attribute__((scalar_storage_order(1))) A { int x; };\n",
        ONE_OF,
    );
    expect_error(
        "sso_arg_none",
        "struct __attribute__((scalar_storage_order)) A { int x; };\n",
        "wrong number of arguments specified for 'scalar_storage_order' attribute",
    );
}

/// Anywhere but a struct or union specifier the attribute is ignored, with
/// gcc's warning -- ahead of the `struct` keyword included, where it belongs
/// to the declaration rather than the type.
#[test]
fn diagnostics_storage_order_attribute_ignored_off_a_specifier() {
    const MSG: &str = "'scalar_storage_order' attribute ignored";
    expect_warning(
        "sso_ignored_variable",
        "int v __attribute__((scalar_storage_order(\"big-endian\")));\n",
        MSG,
    );
    expect_warning(
        "sso_ignored_typedef",
        "typedef int T __attribute__((scalar_storage_order(\"big-endian\")));\n",
        MSG,
    );
    expect_warning(
        "sso_ignored_leading",
        "__attribute__((scalar_storage_order(\"big-endian\"))) struct S { int x; } s;\n\
         int *f(void) { return &s.x; }\n",
        MSG,
    );
}

/// The pragma names an order or `default`, and warns without one.
#[test]
fn diagnostics_storage_order_pragma_argument() {
    expect_warning(
        "sso_pragma_unknown",
        "#pragma scalar_storage_order sideways\nint x;\n",
        "expected 'big-endian', 'little-endian', or 'default' after '#pragma scalar_storage_order'",
    );
    expect_warning(
        "sso_pragma_missing",
        "#pragma scalar_storage_order\nint x;\n",
        "missing 'big-endian', 'little-endian', or 'default' after '#pragma scalar_storage_order'",
    );
}

/// x86-64's `long double` is x87's 80-bit format, which gcc cannot store in
/// reverse order and refuses where it is accessed.
#[test]
fn diagnostics_storage_order_x87_long_double_is_unimplemented() {
    expect_error(
        "sso_x87",
        "struct __attribute__((scalar_storage_order(\"big-endian\"))) X { long double d; };\n\
         long double f(struct X *x) { return x->d; }\n",
        "sorry, unimplemented: reverse storage order for XFmode",
    );
}

/// A struct and its variant in the other order are different types, so one
/// does not assign to the other.
#[test]
fn diagnostics_storage_order_variants_are_incompatible() {
    expect_error(
        "sso_variant_assign",
        "struct S { int i; };\n\
         typedef struct S __attribute__((scalar_storage_order(\"big-endian\"))) S1;\n\
         void f(struct S *s, S1 *s1) { *s = *s1; }\n",
        "incompatible types when assigning to type 'struct S' from type 'struct S'",
    );
    expect_clean(
        "sso_variant_native",
        "struct S { int i; };\n\
         typedef struct S __attribute__((scalar_storage_order(\"little-endian\"))) S2;\n\
         void f(struct S *s, S2 *s2) { *s = *s2; }\n",
    );
}

/// gcc.dg/sso-1.c: a static initializer cannot put an address -- of an
/// object, a function, a string literal, with or without an offset -- into
/// a struct or union stored in reverse order, though the pointer member
/// itself is stored natively. gcc says the element is not constant, and
/// says it of an address converted to an integer member as well. A struct
/// of the target's order nested inside keeps its own order, and is fine.
#[test]
fn diagnostics_storage_order_address_constant_in_a_reversed_aggregate() {
    const MSG: &str = "initializer element is not constant";
    const REC: &str = "int i;\nvoid fn(void);\n\
                       struct __attribute__((scalar_storage_order(\"big-endian\"))) Rec {\n\
                       int *p;\n\
                       };\n";
    let cases = [
        ("sso_init_object", format!("{REC}struct Rec r = {{ &i }};\n")),
        (
            "sso_init_static_block",
            format!("{REC}void f(void) {{ static struct Rec r = {{ &i }}; (void)r; }}\n"),
        ),
        (
            "sso_init_offset",
            format!("{REC}struct Rec r = {{ .p = &i + 1 }};\n"),
        ),
        (
            "sso_init_array_of",
            format!("{REC}struct Rec r[2] = {{ {{ &i }}, {{ 0 }} }};\n"),
        ),
        (
            "sso_init_function",
            "void g(void);\n\
             struct __attribute__((scalar_storage_order(\"big-endian\"))) F { void (*fp)(void); };\n\
             struct F r = { g };\n"
                .to_string(),
        ),
        (
            "sso_init_string",
            "struct __attribute__((scalar_storage_order(\"big-endian\"))) S { const char *s; };\n\
             struct S r = { \"hi\" };\n"
                .to_string(),
        ),
        (
            "sso_init_pointer_array",
            "int i;\n\
             struct __attribute__((scalar_storage_order(\"big-endian\"))) A { int *p[2]; };\n\
             struct A r = { { &i, 0 } };\n"
                .to_string(),
        ),
        (
            "sso_init_union",
            "int i;\n\
             union __attribute__((scalar_storage_order(\"big-endian\"))) U { int *p; long l; };\n\
             union U u = { &i };\n"
                .to_string(),
        ),
        (
            "sso_init_integer",
            "int i;\n\
             struct __attribute__((scalar_storage_order(\"big-endian\"))) L { long l; };\n\
             struct L r = { (long)&i };\n"
                .to_string(),
        ),
        (
            "sso_init_pragma",
            "int i;\n#pragma scalar_storage_order big-endian\n\
             struct P { int *p; };\n#pragma scalar_storage_order default\n\
             struct P r = { &i };\n"
                .to_string(),
        ),
        (
            "sso_init_typedef_variant",
            "int i;\nstruct T { int *p; };\n\
             typedef struct T __attribute__((scalar_storage_order(\"big-endian\"))) BT;\n\
             BT r = { &i };\n"
                .to_string(),
        ),
    ];
    for (name, src) in &cases {
        expect_error(name, src, MSG);
    }
}

/// What gcc still accepts: a null or integer-valued pointer, an automatic
/// object (initialized by stores, not by the linker), a native-order struct
/// nested inside a reversed one, a struct of the target's own order, and
/// the address *of* a reversed struct.
#[test]
fn diagnostics_storage_order_reversed_aggregate_initializers_gcc_allows() {
    expect_clean(
        "sso_init_allowed",
        "int i;\n\
         struct __attribute__((scalar_storage_order(\"big-endian\"))) Rec { int x; int *p; \
         struct In { int *q; } in; };\n\
         struct Rec a = { 1, 0, { &i } };\n\
         struct Rec b = { 2, (int *)16, { 0 } };\n\
         struct Rec *pa = &a;\n\
         struct __attribute__((scalar_storage_order(\"little-endian\"))) Le { int *p; } le = { &i };\n\
         int f(void) { struct Rec c = { 3, &i, { &i } }; return *c.p + *c.in.q; }\n",
    );
}

/// An `_Atomic` member of a reverse-storage-order struct cannot be accessed:
/// gcc rejects every read, write and compound assignment, as each needs the
/// member's address. c17 read and wrote it in native order.
#[test]
fn storage_order_atomic_member_is_refused() {
    for (name, body) in [
        ("sso_atomic_read", "int rd(void) { return g.a; }"),
        ("sso_atomic_write", "void wr(void) { g.a = 5; }"),
        ("sso_atomic_rmw", "void inc(void) { g.a += 1; }"),
    ] {
        let src = format!(
            "struct __attribute__((scalar_storage_order(\"big-endian\"))) S {{ _Atomic int a; int b; }};\n\
             struct S g;\n{body}\n"
        );
        crate::test_compile::compile_expect_error(
            name,
            &src,
            "cannot take address of scalar with reverse storage order",
        );
    }
}

/// Written on a variable declaration of an existing tag, the attribute is
/// ignored with a warning, as gcc ignores it: `x` and `y` are the plain
/// `struct S`, and one assigns to the other. c17 made each use a fresh,
/// incompatible type.
#[test]
fn storage_order_on_a_variable_declaration_is_ignored() {
    let src = "struct S { int a; };\n\
               struct S __attribute__((scalar_storage_order(\"big-endian\"))) x, y;\n\
               struct S z;\n\
               void f(void) { x = y; z = x; }\n";
    let out = crate::test_compile::compile_accepted("sso_var_decl", src, &[]);
    assert!(
        out.contains("'scalar_storage_order' attribute ignored"),
        "{out}"
    );
}

/// gcc refuses a reversed `_Atomic` member wherever it is an operand --
/// even an unevaluated one, `sizeof (g.a + 1)`, or an arm a constant
/// condition skips -- and allows it only as the whole operand of `sizeof`,
/// `_Alignof`, `typeof` or a `_Generic` controlling expression, and in an
/// association or `__builtin_choose_expr` arm not selected. An element of
/// an `_Atomic` array member is an ordinary reversed scalar to gcc.
#[test]
fn storage_order_atomic_member_unevaluated_operands() {
    const DECLS: &str =
        "struct __attribute__((scalar_storage_order(\"big-endian\"))) S { _Atomic int a; int b; _Atomic int arr[2]; };\n\
         struct S g;\n";
    const REFUSED: &str = "cannot take address of scalar with reverse storage order";
    for (name, body) in [
        (
            "sso_at_sizeof",
            "int f(void) { return sizeof g.a + sizeof (g.a); }",
        ),
        ("sso_at_alignof", "int f(void) { return _Alignof(g.a); }"),
        (
            "sso_at_typeof",
            "int f(void) { __typeof__(g.a) t = 0; return t + sizeof(__typeof__(g.a)); }",
        ),
        (
            "sso_at_generic_ctl",
            "int f(void) { return _Generic(g.a, int: 1, default: 2); }",
        ),
        (
            "sso_at_generic_arm",
            "int f(void) { return _Generic(0, int: 1, default: g.a); }",
        ),
        (
            "sso_at_choose",
            "int f(void) { return __builtin_choose_expr(1, 0, g.a); }",
        ),
        (
            "sso_at_array",
            "int f(void) { g.arr[1] = 2; return g.arr[1]; }",
        ),
        (
            "sso_at_other",
            "int f(void) { struct S h = { 1, 2 }; return g.b + h.b; }",
        ),
    ] {
        let stderr = compile_accepted(name, &format!("{DECLS}{body}\n"), &[LINUX]);
        assert!(!stderr.contains(REFUSED), "{name}:\n{stderr}");
    }
    for (name, body) in [
        (
            "sso_at_sizeof_sum",
            "int f(void) { return sizeof(g.a + 1); }",
        ),
        ("sso_at_dead_arm", "int f(void) { return 1 ? 0 : g.a; }"),
        (
            "sso_at_generic_sel",
            "int f(void) { return _Generic(0, int: g.a); }",
        ),
        (
            "sso_at_choose_sel",
            "int f(void) { return __builtin_choose_expr(0, 0, g.a); }",
        ),
        (
            "sso_at_constant_p",
            "int f(void) { return __builtin_constant_p(g.a); }",
        ),
        ("sso_at_arrow", "int f(struct S *p) { return p->a; }"),
        ("sso_at_discard", "void f(void) { (void)g.a; }"),
    ] {
        expect_error(name, &format!("{DECLS}{body}\n"), REFUSED);
    }
    // `&` refuses it once, in its own words.
    let stderr = compile_rejected_with(
        "sso_at_addr",
        &format!("{DECLS}int *f(void) {{ return (int *)&g.a; }}\n"),
        &[LINUX],
    );
    assert_eq!(stderr.matches(REFUSED).count(), 1, "{stderr}");
}

/// Written on a reference to an existing tag, the attribute makes a variant
/// only for a typedef or a type-name that derives nothing -- the type-name
/// with gcc's "after definition" warning -- and each such typedef is a type
/// of its own. A parameter, a member, a function, a pointer typedef and a
/// cast to a pointer all ignore it, with a warning.
#[test]
fn storage_order_written_on_a_reference() {
    const SSO: &str = "__attribute__((scalar_storage_order(\"big-endian\")))";
    const IGNORED: &str = "'scalar_storage_order' attribute ignored";
    for (name, decl) in [
        (
            "sso_ref_param",
            "int f(struct S SSO p) { struct S q = p; return q.a; }",
        ),
        (
            "sso_ref_member",
            "struct U { struct S SSO m; } u; struct S z; void f(void) { z = u.m; }",
        ),
        (
            "sso_ref_return",
            "struct S SSO f(void) { struct S s = { 1 }; return s; }",
        ),
        (
            "sso_ref_ptr_typedef",
            "typedef struct S SSO *P; struct S z; P p = &z;",
        ),
        ("sso_ref_extern", "extern struct S SSO e; struct S e;"),
        (
            "sso_ref_array",
            "struct S SSO arr[2]; struct S z; void f(void) { z = arr[0]; }",
        ),
        (
            "sso_ref_cast",
            "struct S z; int f(void) { return ((struct S SSO *)&z)->a; }",
        ),
    ] {
        let src = format!("struct S {{ int a; }};\n{}\n", decl.replace("SSO", SSO));
        expect_warning(name, &src, IGNORED);
    }
    expect_warning(
        "sso_ref_type_name",
        &format!("struct S {{ int a; }};\nint n = sizeof(struct S {SSO});\n"),
        "ignoring attributes applied to 'struct S' after definition",
    );
    expect_error(
        "sso_ref_typedefs_differ",
        &format!(
            "struct S {{ int a; }};\ntypedef struct S {SSO} A, B;\n\
             void f(void) {{ A a = {{ 1 }}; B b; b = a; }}\n"
        ),
        "incompatible types",
    );
}
