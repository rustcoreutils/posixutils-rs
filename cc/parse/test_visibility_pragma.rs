//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for `#pragma GCC visibility push(...)` / `pop`: which
// declarations take the visibility in force, as gcc 13 gives it.
//

use crate::parse::ast::ExternalDecl;
use crate::parse::Parser;
use crate::strings::StringTable;
use crate::symbol::SymbolTable;
use crate::target::Target;
use crate::token::lexer::Tokenizer;
use crate::token::preprocess::{extract_pragma_directives, preprocess_collecting};
use crate::token::PreprocessConfig;
use crate::types::TypeTable;

/// Every file-scope declarator and function definition in `src`, in order,
/// with the visibility it carries.
fn visibilities(src: &str) -> Vec<(String, Option<String>)> {
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
            ExternalDecl::FunctionDef(f) => out.push((
                strings.get(f.name).to_string(),
                f.attrs.symbol.visibility.clone(),
            )),
            ExternalDecl::Declaration(d) => {
                for decl in &d.declarators {
                    let name = symbols.get(decl.symbol).name;
                    out.push((
                        strings.get(name).to_string(),
                        decl.symbol_attrs.visibility.clone(),
                    ));
                }
            }
            ExternalDecl::Asm { .. } => {}
        }
    }
    out
}

fn want(list: &[(&str, Option<&str>)]) -> Vec<(String, Option<String>)> {
    list.iter()
        .map(|(n, v)| (n.to_string(), v.map(str::to_string)))
        .collect()
}

/// Between `push` and `pop`, every declaration with external linkage takes
/// the visibility -- an `extern` declaration as well as a definition, a
/// function as well as an object -- and nothing with internal or no linkage
/// does. After the `pop`, nothing does.
#[test]
fn test_visibility_pragma_reaches_external_declarations() {
    let got = visibilities(
        "#pragma GCC visibility push(hidden)\n\
         extern int x;\n\
         int f(void);\n\
         int d = 1;\n\
         static int s;\n\
         typedef int T;\n\
         int g(void) { return x + s; }\n\
         static int sf(void) { return 0; }\n\
         #pragma GCC visibility pop\n\
         extern int z;\n\
         int h(void) { return z; }\n",
    );
    assert_eq!(
        got,
        want(&[
            ("x", Some("hidden")),
            ("f", Some("hidden")),
            ("d", Some("hidden")),
            ("s", None),
            ("T", None),
            ("g", Some("hidden")),
            ("sf", None),
            ("z", None),
            ("h", None),
        ])
    );
}

/// A visibility attribute on the declaration, or on an earlier declaration
/// of the same name, wins over the pragma; a declaration that had none
/// before the pragma takes it.
#[test]
fn test_visibility_attribute_wins_over_the_pragma() {
    let got = visibilities(
        "extern int early __attribute__((visibility(\"default\")));\n\
         void pf(void) __attribute__((visibility(\"protected\")));\n\
         void later(void);\n\
         int tent;\n\
         #pragma GCC visibility push(hidden)\n\
         extern int y __attribute__((visibility(\"default\")));\n\
         extern int early;\n\
         void pf(void);\n\
         void later(void) {}\n\
         extern int tent;\n\
         #pragma GCC visibility pop\n",
    );
    assert_eq!(
        got,
        want(&[
            ("early", Some("default")),
            ("pf", Some("protected")),
            ("later", None),
            ("tent", None),
            ("y", Some("default")),
            ("early", None),
            ("pf", None),
            ("later", Some("hidden")),
            ("tent", Some("hidden")),
        ])
    );
}

/// The pushes nest: each `pop` restores the visibility the `push` it matches
/// replaced, and `_Pragma` is the same directive.
#[test]
fn test_visibility_pragma_nests() {
    let got = visibilities(
        "#pragma GCC visibility push(hidden)\n\
         extern int a;\n\
         _Pragma(\"GCC visibility push(protected)\")\n\
         extern int b;\n\
         #pragma GCC visibility push(default)\n\
         extern int c;\n\
         #pragma GCC visibility pop\n\
         extern int d;\n\
         #pragma GCC visibility pop\n\
         extern int e;\n\
         #pragma GCC visibility push(internal)\n\
         extern int f;\n\
         #pragma GCC visibility pop\n\
         #pragma GCC visibility pop\n\
         extern int g;\n",
    );
    assert_eq!(
        got,
        want(&[
            ("a", Some("hidden")),
            ("b", Some("protected")),
            ("c", Some("default")),
            ("d", Some("protected")),
            ("e", Some("hidden")),
            ("f", Some("internal")),
            ("g", None),
        ])
    );
}
