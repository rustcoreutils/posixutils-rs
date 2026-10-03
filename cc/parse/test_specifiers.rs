//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for the types of `__func__`, `_Complex` combinations and the
// named machine modes.
//

use super::test_parser::{parse_tu, with_statement_expr};
use crate::symbol::Namespace;
use crate::types::{TypeKind, TypeModifiers};

/// `__func__` is `const char[N]` for the enclosing function's name: the
/// helper's function is `t`, so N is 2.
#[test]
fn test_func_name_is_a_const_char_array() {
    for spelled in ["__func__", "__FUNCTION__", "__PRETTY_FUNCTION__"] {
        with_statement_expr("", spelled, |p, e| {
            let typ = e.typ.expect("typed");
            assert_eq!(p.types.kind(typ), TypeKind::Array, "{spelled}");
            assert_eq!(p.types.size_bytes(typ), 2, "{spelled}");
            let elem = p.types.base_type(typ).expect("element");
            assert!(p.types.modifiers(elem).contains(TypeModifiers::CONST));
        });
        with_statement_expr("", &format!("sizeof {spelled}"), |p, e| {
            assert_eq!(p.eval_const_expr(e), Some(2), "{spelled}");
        });
    }
}

/// `_Complex` with a base that is not arithmetic is reported, not dropped.
#[test]
fn test_complex_needs_an_arithmetic_base() {
    for src in [
        "typedef double ty; ty _Complex z;",
        "__typeof__(1.0f) _Complex z;",
        "_Complex _Bool b;",
        "_Complex void *p;",
        "struct S { int a; } _Complex v;",
    ] {
        let before = crate::diag::error_count();
        let _ = parse_tu(src);
        assert!(crate::diag::error_count() > before, "{src}: accepted");
    }
}

/// `byte` is one byte; `unwind_word` is eight.
#[test]
fn test_named_machine_modes() {
    let (_, types, strings, symbols) = parse_tu(
        "typedef int b __attribute__((mode(byte)));\n\
         typedef unsigned uw __attribute__((__mode__(__unwind_word__)));",
    )
    .unwrap();
    let size = |name: &str| {
        let id = strings.lookup(name).expect("interned");
        let typ = symbols
            .lookup(id, Namespace::Ordinary)
            .expect("declared")
            .typ;
        (types.size_bytes(typ), types.is_unsigned(typ))
    };
    assert_eq!(size("b"), (1, false));
    assert_eq!(size("uw"), (8, true));
}
