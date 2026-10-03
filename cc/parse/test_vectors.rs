//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for GNU `vector_size` values: their result types, and the
// operand rules gcc applies to them.
//

use super::test_parser::{parse_expr_for, parse_tu_for};
use crate::target::{Arch, Os, Target};

const TYPES: &str = "typedef int v4si __attribute__((vector_size(16)));\
    typedef unsigned v4su __attribute__((vector_size(16)));\
    typedef float v4sf __attribute__((vector_size(16)));\
    typedef double v2df __attribute__((vector_size(16)));\
    typedef char v16qi __attribute__((vector_size(16)));\
    typedef int v2si __attribute__((vector_size(8)));\
    typedef long v2di __attribute__((vector_size(16)));";

fn x86_linux() -> Target {
    Target::new(Arch::X86_64, Os::Linux)
}

fn rejected(body: &str) -> bool {
    let src = format!(
        "{TYPES} void g(long l, int i, float fl, double d, int *p, int c) {{ \
         v4si a = {{0}}, b = {{0}}; v4su u = {{0}}; v4sf f = {{0}}; v2df d2 = {{0}}; \
         v16qi c16 = {{0}}; v2si l2 = {{0}}; const v4si ca = {{0}}; {body}; }}"
    );
    let before = crate::diag::error_count();
    parse_tu_for(&src, &x86_linux()).is_err() || crate::diag::error_count() > before
}

#[test]
fn test_vector_operations_type_as_gcc_types_them() {
    let src = format!(
        "{TYPES} v4si a; v4su u; v4sf f; v2df d2; v2si l2;\
         _Static_assert(sizeof(a + u) == 16, \"\");\
         _Static_assert(_Generic(a + u, v4si: 1, default: 0), \"left type\");\
         _Static_assert(_Generic(u + a, v4su: 1, default: 0), \"left type\");\
         _Static_assert(_Generic(a < a, v4si: 1, default: 0), \"int mask\");\
         _Static_assert(_Generic(f < f, v4si: 1, default: 0), \"float mask\");\
         _Static_assert(_Generic(d2 == d2, v2di: 1, default: 0), \"long mask\");\
         _Static_assert(_Generic(a + 1, v4si: 1, default: 0), \"splat\");\
         _Static_assert(_Generic(1 << a, v4si: 1, default: 0), \"scalar shift\");\
         _Static_assert(_Generic(-f, v4sf: 1, default: 0), \"neg\");\
         _Static_assert(_Generic((v4sf)a, v4sf: 1, default: 0), \"bitcast\");\
         _Static_assert(_Generic((long)l2, long: 1, default: 0), \"to integer\");\
         _Static_assert(_Generic(a, v4si: 1, default: 0), \"no decay\");"
    );
    parse_tu_for(&src, &x86_linux()).unwrap();
}

#[test]
fn test_vector_operand_rules() {
    for body in [
        "a + b; a + u; f + 1.5; a + i; a + 100; u + -1; a << l; a < b; -a; ~a; ++a; a++",
        "c ? a : b; a = b; a += 1; a <<= 1; a = (v4si)u; (v4si)f; (v2si)l; (long)l2",
        "a[1] = 3; (void)sizeof(-a); (void)&a",
    ] {
        assert!(!rejected(body), "{body}");
    }
    for body in [
        "a + f",
        "a + 1.5",
        "a + l",
        "f + i",
        "f + d",
        "c16 + 300",
        "f + 1.1",
        "a % f",
        "f % f",
        "f << 1",
        "l << a",
        "a + p",
        "~f",
        "!a",
        "*a",
        "a && b",
        "c ? a : u",
        "c ? a : 1",
        "if (a) ;",
        "a = f",
        "a = u",
        "a = 1",
        "(v2si)i",
        "(v2si)d",
        "(double)l2",
        "(int *)l2",
        "ca[1] = 3",
        "ca = a",
    ] {
        assert!(rejected(body), "{body}");
    }
}

#[test]
fn test_vector_type_spelling() {
    let target = x86_linux();
    let (e, types, _, _) =
        parse_expr_for("(__attribute__((vector_size(16))) int){1}", &target).unwrap();
    assert_eq!(types.format_type(e.typ.unwrap(), None), "__vector(4) int");
}

/// A comparison's mask is the signed vector of its shape, and assigns to any
/// vector of integer lanes of that shape -- unsigned too -- as gcc's
/// "opaque" result does. A type written `vector_size` before the type name
/// makes every declarator a vector, and a function's return type one.
#[test]
fn test_vector_masks_and_specifier_attributes() {
    let src = format!(
        "{TYPES} v4si a; v4su u; v4sf f;\
         void g(void) {{ u = a < a; u = (u > 1) & u; v4si m = f < f; (void)m; }}\
         _Static_assert(_Generic((u > 1) & u, v4su: 1, default: 0), \"mask yields\");\
         __attribute__((vector_size(8))) signed char p, q;\
         _Static_assert(sizeof q == 8, \"every declarator\");\
         __attribute__((vector_size(16))) float h(void);\
         _Static_assert(sizeof(h()) == 16, \"return type\");"
    );
    parse_tu_for(&src, &x86_linux()).unwrap();
    assert!(rejected("u = a"));
    assert!(rejected("v2si x = a < b"));
}

/// A vector parameter is passed by value: it is no array, and C17
/// 6.7.6.3p7's adjustment to a pointer does not reach it. A one-lane `float`
/// vector on aarch64 is the one shape c17 cannot pass; x86-64 passes it in
/// memory.
#[test]
fn test_vector_parameters_are_not_adjusted() {
    let src = format!(
        "{TYPES} v4si f(v4si a, v2si b);\
         _Static_assert(_Generic(f, v4si (*)(v4si, v2si): 1, default: 0), \"by value\");\
         v4si g(v4si a) {{ return a + 1; }}"
    );
    parse_tu_for(&src, &x86_linux()).unwrap();
    let small = "typedef float v1sf __attribute__((vector_size(4))); v1sf h(v1sf a);";
    parse_tu_for(small, &x86_linux()).unwrap();
    let before = crate::diag::error_count();
    let aarch64 = Target::new(Arch::Aarch64, Os::Linux);
    let rejected = parse_tu_for(small, &aarch64).is_err() || crate::diag::error_count() > before;
    assert!(rejected, "a vector of one float on aarch64");
}
