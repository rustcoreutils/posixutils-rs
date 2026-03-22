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

pub mod ast;
mod expression;
pub mod parser;

#[cfg(test)]
mod test_parser;

// Re-export parser used by main.rs
pub use parser::Parser;

/// Check if a name is a C11 nullability qualifier.
/// Single source of truth — used by all qualifier-parsing paths.
pub(crate) fn is_nullability_qualifier(name: &str) -> bool {
    matches!(
        name,
        "_Nonnull"
            | "__nonnull"
            | "_Nullable"
            | "__nullable"
            | "_Null_unspecified"
            | "__null_unspecified"
    )
}
