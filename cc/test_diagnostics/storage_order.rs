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
