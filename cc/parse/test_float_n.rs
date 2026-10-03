//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser and type-table tests for C23's _FloatN and _FloatNx types
// (TS 18661-3): distinct types in the formats of the standard ones.
//

use super::test_parser::{parse_expr_for, parse_tu_for};
use crate::target::{Arch, Os, Target};
use crate::types::{FloatClass, TypeKind, TypeTable};

fn x86_linux() -> Target {
    Target::new(Arch::X86_64, Os::Linux)
}

/// The type of `expr`, spelled as diagnostics spell it.
fn type_of(expr: &str, target: &Target) -> String {
    let (e, types, _, _) = parse_expr_for(expr, target).unwrap();
    types.format_type(e.typ.unwrap(), None)
}

#[test]
fn test_float_n_types_are_distinct_but_share_a_format() {
    let types = TypeTable::new(&x86_linux());
    let f32_ = types.floating(TypeKind::Float, FloatClass::Interchange);
    let f64_ = types.floating(TypeKind::Double, FloatClass::Interchange);
    let f32x = types.floating(TypeKind::Double, FloatClass::Extended);
    let f64x = types.floating(TypeKind::LongDouble, FloatClass::Extended);
    for (named, standard) in [
        (f32_, types.float_id),
        (f64_, types.double_id),
        (f32x, types.double_id),
        (f64x, types.longdouble_id),
    ] {
        assert!(!types.types_compatible(named, standard));
        assert_eq!(types.fp_format(named), types.fp_format(standard));
        assert_eq!(types.size_bits(named), types.size_bits(standard));
    }
    assert!(!types.types_compatible(f64_, f32x));
    assert_eq!(types.format_type(f32x, None), "_Float32x");
    assert_eq!(types.format_type(f64x, None), "_Float64x");
}

/// gcc's rule for two names of one format: interchange over standard over
/// extended -- and no default argument promotion for `_Float32`.
#[test]
fn test_float_n_usual_arithmetic_conversions() {
    let target = x86_linux();
    for (expr, want) in [
        ("(_Float32)1 + 1.0f", "_Float32"),
        ("1.0f + (_Float32)1", "_Float32"),
        ("(_Float64)1 + 1.0", "_Float64"),
        ("(_Float32x)1 + 1.0", "double"),
        ("(_Float32x)1 + (_Float64)1", "_Float64"),
        ("(_Float64x)1 + 1.0L", "long double"),
        ("(_Float32)1 + 1", "_Float32"),
        ("(_Float32)1 + 1.0", "double"),
        ("(_Float32x)1 * (_Float32x)1", "_Float32x"),
        ("1.0f32x", "_Float32x"),
        ("1.0F64x", "_Float64x"),
        ("1.0f32", "_Float32"),
        ("(_Complex _Float32)1 + 1.0f", "_Float32 _Complex"),
    ] {
        assert_eq!(type_of(expr, &target), want, "{expr}");
    }
    let types = TypeTable::new(&target);
    let f32_ = types.floating(TypeKind::Float, FloatClass::Interchange);
    assert_eq!(types.default_argument_promote(f32_), f32_);
    assert_eq!(
        types.default_argument_promote(types.float_id),
        types.double_id
    );
}

#[test]
fn test_float_n_specifiers() {
    let target = x86_linux();
    parse_tu_for(
        "_Float32 a; _Float64 b; _Float32x c; _Float64x d; _Complex _Float32x e;\
         int n = _Generic(a, float: 1, _Float32: 2) + _Generic(d, long double: 1, _Float64x: 2);",
        &target,
    )
    .unwrap();
    for src in [
        "long _Float64 x;",
        "unsigned _Float32 x;",
        "_Float32 float x;",
    ] {
        let before = crate::diag::error_count();
        let rejected = parse_tu_for(src, &target).is_err();
        assert!(rejected || crate::diag::error_count() > before, "{src}");
    }
    // Apple's arm64 has nothing wider than `double` to be `_Float64x`.
    let apple = Target::new(Arch::Aarch64, Os::MacOS);
    assert!(parse_tu_for("_Float64x d;", &apple).is_err());
    assert!(parse_expr_for("1.0f64x", &apple).is_err());
    parse_tu_for("_Float32x c;", &apple).unwrap();
}

#[test]
fn test_float_nx_builtins() {
    let target = x86_linux();
    for (expr, want) in [
        ("__builtin_inff32x()", "_Float32x"),
        ("__builtin_huge_valf64x()", "_Float64x"),
        ("__builtin_nanf32x(\"\")", "_Float32x"),
        ("__builtin_nansf64x(\"\")", "_Float64x"),
        ("__builtin_fabsf32x(-1.0f32x)", "_Float32x"),
        ("__builtin_copysignf64x(1, -1)", "_Float64x"),
        ("__builtin_fabsf32(1)", "_Float32"),
        ("__builtin_inff64()", "_Float64"),
    ] {
        assert_eq!(type_of(expr, &target), want, "{expr}");
    }
}
