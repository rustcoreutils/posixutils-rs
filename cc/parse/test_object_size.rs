//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// What the parser answers for `__builtin_object_size` of a member reached
// through a pointer, and what it defers to the optimizer.
//

use super::ast::{BlockItem, ExprKind, ExternalDecl, Stmt};
use super::parser::Parser;
use super::LibraryCallPolicy;
use crate::strings::StringTable;
use crate::symbol::SymbolTable;
use crate::target::Target;
use crate::token::lexer::Tokenizer;
use crate::types::TypeTable;

const DECLS: &str = "struct in { char x[6]; int y; };\n\
     struct sb { int a; char name[16]; struct in in; int b; char tail[8]; };\n\
     struct fl { int n; char data[]; };\n";

/// What the parser made of `__builtin_object_size(operand, otype)` returned
/// from a function whose parameters are `struct sb *p` and `struct fl *q`:
/// `Some(answer)`, or `None` where it deferred.
fn answer(operand: &str, otype: u32, optimizing: bool) -> Option<i64> {
    let src = format!(
        "{DECLS}unsigned long f(struct sb *p, struct fl *q)\n\
         {{ return __builtin_object_size({operand}, {otype}); }}\n"
    );
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(src.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&Target::host());
    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
    parser.set_library_call_policy(LibraryCallPolicy {
        optimizing,
        math_errno: true,
    });
    let tu = parser.parse_translation_unit().expect("parses");
    let Some(ExternalDecl::FunctionDef(f)) = tu.items.last() else {
        panic!("no function");
    };
    let Stmt::Block(items) = &f.body else {
        panic!("no body");
    };
    let Some(BlockItem::Statement(stmt)) = items.last() else {
        panic!("no return");
    };
    let Stmt::Return(Some(mut expr)) = (**stmt).clone() else {
        panic!("no return value");
    };
    while let ExprKind::Cast { expr: inner, .. } = expr.kind {
        expr = *inner;
    }
    match expr.kind {
        ExprKind::IntLit(v) => Some(v),
        ExprKind::ObjectSize { .. } => None,
        other => panic!("{operand}: {other:?}"),
    }
}

/// Optimizing, a member reached through a pointer is its declared size for
/// types 1 and 3 -- what is left of it for a pointer inside it -- however
/// the access is spelled, as gcc answers.
#[test]
fn a_member_through_a_pointer_is_its_declared_size() {
    for (operand, size) in [
        ("p->name", 16),
        ("&p->name[2]", 14),
        ("p->name + 3", 13),
        ("&p->name", 16),
        ("p->in.x", 6),
        ("&p->in.x[1]", 5),
        ("&p->in", 12),
        ("&p->in.y", 4),
        ("&p->a", 4),
        ("(*p).name", 16),
        ("p[1].name", 16),
    ] {
        for otype in [1, 3] {
            assert_eq!(
                answer(operand, otype, true),
                Some(size),
                "{operand}, {otype}"
            );
        }
    }
}

/// The whole object behind the pointer is unknown, so types 0 and 2 wait for
/// the optimizer; so does a trailing array, which may run past its declared
/// size, and a flexible array member. Without optimization gcc knows none
/// of it, and neither does c17.
#[test]
fn what_a_pointer_does_not_say_is_left_unknown() {
    for operand in ["p->name", "&p->in.y"] {
        for otype in [0, 2] {
            assert_eq!(answer(operand, otype, true), None, "{operand}, {otype}");
        }
    }
    for operand in ["p->tail", "&p->tail[2]", "q->data", "p"] {
        for otype in 0..4 {
            assert_eq!(answer(operand, otype, true), None, "{operand}, {otype}");
        }
    }
    for (otype, unknown) in [(0, -1), (1, -1), (2, 0), (3, 0)] {
        assert_eq!(
            answer("p->name", otype, false),
            Some(unknown),
            "-O0 {otype}"
        );
    }
}
