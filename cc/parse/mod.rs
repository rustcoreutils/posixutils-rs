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

/// The type qualifier this identifier spells, in *any* of its spellings, or
/// `None` if it is not one.
///
/// C17 6.7.3's `const`, `volatile` and `restrict`, each of which GNU also
/// spells `__const__`/`__const` and so on -- spellings glibc's headers use
/// throughout. `_Atomic` is deliberately absent: it is a qualifier *and* a
/// type specifier (`_Atomic(T)`), so each site has to tell those apart for
/// itself. The
/// same match was written out at five places and four of them listed only the
/// standard spelling -- so `__const` was an unknown identifier inside a
/// declarator, and glibc's `memcpy` prototype -- which spells both
/// `__restrict` and `__const` -- failed to parse. One answer, so the five
/// cannot drift again.
pub(crate) fn cv_qualifier_modifier(
    id: crate::strings::StringId,
) -> Option<crate::types::TypeModifiers> {
    use crate::types::TypeModifiers as M;
    match id {
        crate::kw::CONST | crate::kw::GNU_CONST | crate::kw::GNU_CONST2 => Some(M::CONST),
        crate::kw::VOLATILE | crate::kw::GNU_VOLATILE | crate::kw::GNU_VOLATILE2 => {
            Some(M::VOLATILE)
        }
        crate::kw::RESTRICT | crate::kw::GNU_RESTRICT | crate::kw::GNU_RESTRICT2 => {
            Some(M::RESTRICT)
        }
        _ => None,
    }
}
