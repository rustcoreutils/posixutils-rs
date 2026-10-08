//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `-aux-info FILE`: one line per function the translation unit
// declares or defines at file scope, in gcc's format:
//
//     /* <stdin>:14:NC */ extern int is_selinux_enabled (void);
//     /* f.c:3:NF */ static int helper (int x); /* (x) int x; */
//
// `N`/`O` says whether the declaration is a prototype, `C`/`F` whether it is
// a declaration or a definition. libselinux builds its Python binding's SWIG
// exception list from this record, so the Debian build fails without it.
//
// One difference from gcc: c17's types do not remember which typedef name a
// declaration used, so a type is spelled as what it stands for.
//

use crate::diag;
use crate::parse::ast::{ExternalDecl, FunctionDef, InitDeclarator, ParamStyle, TranslationUnit};
use crate::strings::StringTable;
use crate::symbol::{Linkage, Namespace, SymbolTable};
use crate::types::{TypeKind, TypeModifiers, TypeTable};

/// The `-aux-info` record of `ast`.
pub fn records(
    ast: &TranslationUnit,
    strings: &StringTable,
    types: &TypeTable,
    symbols: &SymbolTable,
) -> String {
    let mut out = String::from("/* compiled from: . */\n");
    for item in &ast.items {
        match item {
            ExternalDecl::Declaration(decl) => {
                for d in &decl.declarators {
                    if let Some(line) = declaration_record(d, strings, types, symbols) {
                        out.push_str(&line);
                    }
                }
            }
            ExternalDecl::FunctionDef(func) => {
                out.push_str(&definition_record(func, strings, types, symbols));
            }
            ExternalDecl::Asm { .. } => {}
        }
    }
    out
}

/// `/* FILE:LINE:XY */`, where the position is the declaration's.
fn location(pos: diag::Position, prototype: bool, definition: bool) -> String {
    let (file, line, _) = diag::effective_position(pos);
    let style = if prototype { 'N' } else { 'O' };
    let kind = if definition { 'F' } else { 'C' };
    format!("/* {file}:{line}:{style}{kind} */")
}

fn storage(internal: bool) -> &'static str {
    if internal {
        "static"
    } else {
        "extern"
    }
}

/// The record of a file-scope declarator, if it declares a function.
fn declaration_record(
    d: &InitDeclarator,
    strings: &StringTable,
    types: &TypeTable,
    symbols: &SymbolTable,
) -> Option<String> {
    if types.kind(d.typ) != TypeKind::Function || d.storage_class.contains(TypeModifiers::TYPEDEF) {
        return None;
    }
    let sym = symbols.get(d.symbol);
    let name = strings.get(sym.name);
    let prototype = types.get(d.typ).params.is_some();
    Some(format!(
        "{} {} {};\n",
        location(d.pos, prototype, false),
        storage(sym.linkage == Linkage::Internal),
        types.format_gcc_declaration(d.typ, name, Some(strings))
    ))
}

/// The record of a function definition: its parameters with their names,
/// and gcc's trailing comment listing them again.
fn definition_record(
    func: &FunctionDef,
    strings: &StringTable,
    types: &TypeTable,
    symbols: &SymbolTable,
) -> String {
    let name = strings.get(func.name);
    let mut names = Vec::new();
    let mut params = Vec::new();
    for p in &func.params {
        let pname = p.symbol.map_or("", |s| strings.get(symbols.get(s).name));
        names.push(pname);
        params.push(types.format_gcc_declaration(p.typ, pname, Some(strings)));
    }
    let variadic = symbols
        .lookup(func.name, Namespace::Ordinary)
        .is_some_and(|s| types.get(s.typ).variadic);
    if variadic {
        params.push("...".to_string());
    }
    let list = if params.is_empty() {
        "void".to_string()
    } else {
        params.join(", ")
    };
    let declarator = format!("{name} ({list})");
    let comment: String = params
        .iter()
        .take(names.len())
        .map(|p| format!(" {p};"))
        .collect();
    format!(
        "{} {} {}; /* ({}){} */\n",
        location(func.pos, func.param_style == ParamStyle::Prototype, true),
        storage(func.is_static),
        types.format_gcc_declaration(func.return_type, &declarator, Some(strings)),
        names.join(", "),
        comment
    )
}
