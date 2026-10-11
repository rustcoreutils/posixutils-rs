//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for the GNU attributes that reach a declaration's symbol or
// function attributes: `error`, `warning`, `tls_model`, `retain`,
// `flatten`, `noipa` and the per-function code-generation attributes.
//

use crate::parse::ast::{ExternalDecl, FunctionAttrs, SymbolAttrs};
use crate::parse::Parser;
use crate::strings::StringTable;
use crate::symbol::SymbolTable;
use crate::target::Target;
use crate::token::lexer::Tokenizer;
use crate::token::preprocess::{extract_pragma_directives, preprocess_collecting};
use crate::token::PreprocessConfig;
use crate::types::TypeTable;

/// What one file-scope declarator or definition of `src` carries: its
/// name, its symbol attributes, and a definition's function attributes.
struct Declared {
    name: String,
    symbol: SymbolAttrs,
    function: Option<FunctionAttrs>,
}

/// Every file-scope declarator and function definition in `src`, in order.
fn declared(src: &str) -> Vec<Declared> {
    let target = Target::host();
    let mut strings = StringTable::new();
    let tokens = Tokenizer::new(src.as_bytes(), 0, &mut strings).tokenize();
    let (mut tokens, _) = preprocess_collecting(
        tokens,
        &target,
        &mut strings,
        "<test>",
        &PreprocessConfig::default(),
    );
    let pragmas = extract_pragma_directives(&mut tokens);
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&target);
    let tu = Parser::new(&tokens, &strings, &mut symbols, &mut types, pragmas)
        .parse_translation_unit()
        .expect("parse");
    let mut out = Vec::new();
    for item in &tu.items {
        match item {
            ExternalDecl::FunctionDef(f) => out.push(Declared {
                name: strings.get(f.name).to_string(),
                symbol: f.attrs.symbol.clone(),
                function: Some(f.attrs.clone()),
            }),
            ExternalDecl::Declaration(d) => {
                for decl in &d.declarators {
                    out.push(Declared {
                        name: strings.get(symbols.get(decl.symbol).name).to_string(),
                        symbol: decl.symbol_attrs.clone(),
                        function: None,
                    });
                }
            }
            ExternalDecl::Asm { .. } => {}
        }
    }
    out
}

/// The value each file-scope object of `src` named by one letter is
/// initialized with, which must be an integer literal.
fn initial_values(src: &str) -> Vec<(String, i64)> {
    let target = Target::host();
    let mut strings = StringTable::new();
    let tokens = Tokenizer::new(src.as_bytes(), 0, &mut strings).tokenize();
    let (mut tokens, _) = preprocess_collecting(
        tokens,
        &target,
        &mut strings,
        "<test>",
        &PreprocessConfig::default(),
    );
    let pragmas = extract_pragma_directives(&mut tokens);
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&target);
    let tu = Parser::new(&tokens, &strings, &mut symbols, &mut types, pragmas)
        .parse_translation_unit()
        .expect("parse");
    let mut out = Vec::new();
    for item in &tu.items {
        let ExternalDecl::Declaration(d) = item else {
            continue;
        };
        for decl in &d.declarators {
            let Some(init) = &decl.init else {
                continue;
            };
            let crate::parse::ast::ExprKind::IntLit(v) = init.kind else {
                panic!("not a literal: {init:?}");
            };
            let name = strings.get(symbols.get(decl.symbol).name).to_string();
            // The answers; the objects asked about have longer names.
            if name.len() == 1 {
                out.push((name, v));
            }
        }
    }
    out
}

/// `__builtin_has_attribute` folds to 1 or 0 from what was written on the
/// declaration -- any declaration of the name, in either spelling, the
/// arguments compared where gcc compares them -- or, for a type or an
/// expression naming no declaration, from the type.
#[test]
fn test_builtin_has_attribute_folds_to_a_constant() {
    let got = initial_values(
        "extern void f1(void) __attribute__((noreturn, cold));\n\
         void f1(void);\n\
         void late(void);\n\
         void late(void) __attribute__((__noinline__));\n\
         int v1 __attribute__((aligned(16), used));\n\
         static int v2 __attribute__((section(\"xx\"))) = 3;\n\
         typedef int T1 __attribute__((aligned(32)));\n\
         struct __attribute__((packed)) P { char c; int i; };\n\
         extern int pf(const char *, ...) __attribute__((nonnull(1)));\n\
         _Noreturn void f2(void);\n\
         int plain;\n\
         int a = __builtin_has_attribute(f1, noreturn);\n\
         int b = __builtin_has_attribute(f1, hot);\n\
         int c = __builtin_has_attribute(late, noinline);\n\
         int d = __builtin_has_attribute(v1, aligned(16));\n\
         int e = __builtin_has_attribute(v1, aligned(8));\n\
         int f = __builtin_has_attribute(v2, section(\"xx\"));\n\
         int g = __builtin_has_attribute(v2, section(\"yy\"));\n\
         int h = __builtin_has_attribute(T1, aligned(32));\n\
         int i = __builtin_has_attribute(int, aligned);\n\
         int j = __builtin_has_attribute(struct P, packed);\n\
         int k = __builtin_has_attribute(pf, nonnull(1));\n\
         int l = __builtin_has_attribute(pf, nonnull(2));\n\
         int m = __builtin_has_attribute(f2, noreturn);\n\
         int n = __builtin_has_attribute(plain + 1, aligned);\n\
         int o = __builtin_has_attribute(*&v1, aligned);\n",
    );
    let want = [
        ("a", 1),
        ("b", 0),
        ("c", 1),
        ("d", 1),
        ("e", 0),
        ("f", 1),
        ("g", 0),
        ("h", 1),
        ("i", 0),
        ("j", 1),
        ("k", 1),
        ("l", 0),
        ("m", 1),
        ("n", 0),
        ("o", 1),
    ];
    let want: Vec<(String, i64)> = want.iter().map(|(n, v)| (n.to_string(), *v)).collect();
    assert_eq!(got, want);
}

/// `error("msg")` and `warning("msg")`, in both spellings, give the
/// function its call diagnostics; a definition takes those of the
/// declarations before it; and on an object they are dropped.
#[test]
fn test_error_and_warning_attributes_reach_the_function() {
    let got = declared(
        "void e(void) __attribute__((error(\"E\")));\n\
         void w(void) __attribute__((__warning__(\"W\")));\n\
         void b(void) __attribute__((__error__(\"E2\"), warning(\"W2\")));\n\
         void b(void) {}\n\
         int v __attribute__((warning(\"V\")));\n",
    );
    let diags = |i: usize| {
        let d = &got[i].symbol.call_diagnostics;
        (
            got[i].name.as_str(),
            d.error.as_deref(),
            d.warning.as_deref(),
        )
    };
    assert_eq!(diags(0), ("e", Some("E"), None));
    assert_eq!(diags(1), ("w", None, Some("W")));
    assert_eq!(diags(2), ("b", Some("E2"), Some("W2")));
    assert_eq!(diags(3), ("b", Some("E2"), Some("W2")));
    assert!(got[3].function.is_some());
    assert_eq!(diags(4), ("v", None, None));
}

/// `alloc_size` and a sized `access` reach the function, their parameter
/// numbers counted from 0; a definition takes those of the declarations
/// before it; an `access` with no size, one of an unknown mode, and either
/// attribute on an object say nothing.
#[test]
fn test_alloc_size_and_access_reach_the_function() {
    use crate::parse::ast::{AccessSize, AllocSize};
    let got = declared(
        "void *m(unsigned long) __attribute__((alloc_size(1)));\n\
         void *c(int, unsigned long, unsigned long) __attribute__((__alloc_size__(2, 3)));\n\
         int f(const char *, unsigned long, char *) __attribute__((access(read_only, 1, 2), access(write_only, 3)));\n\
         int f(const char *p, unsigned long n, char *q) { return 0; }\n\
         int g(char *, int) __attribute__((access(bogus, 1, 2)));\n\
         int v __attribute__((alloc_size(1)));\n",
    );
    let attrs = |i: usize| (got[i].symbol.alloc_size, got[i].symbol.access.clone());
    let one = |size, count| Some(AllocSize { size, count });
    assert_eq!(attrs(0), (one(0, None), vec![]));
    assert_eq!(attrs(1), (one(1, Some(2)), vec![]));
    let sized = vec![AccessSize { ptr: 0, size: 1 }];
    assert_eq!(attrs(2), (None, sized.clone()));
    assert_eq!(attrs(3), (None, sized));
    assert!(got[3].function.is_some());
    assert_eq!(attrs(4), (None, vec![]));
    assert_eq!(attrs(5), (None, vec![]));
}
