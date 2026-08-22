//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parse module - AST and parser
//

mod aggregate;
pub mod ast;
mod builtin_expr;
mod declaration;
mod declarator;
mod expr_check;
mod expression;
pub mod parser;
mod toplevel;
mod typename;

#[cfg(test)]
mod test_parser;

// Re-export parser used by main.rs
pub use parser::Parser;

/// Check if a StringId is a C11 nullability qualifier.
/// Single source of truth — used by all qualifier-parsing paths.
pub(crate) fn is_nullability_qualifier(id: crate::strings::StringId) -> bool {
    crate::kw::has_tag(id, crate::kw::NULLABILITY)
}
