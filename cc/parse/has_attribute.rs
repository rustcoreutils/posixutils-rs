//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_has_attribute(type-or-expression, attribute)`: whether a
// declaration or a type carries an attribute, answered at compile time from
// what the program wrote.
//

use super::ast::{Expr, ExprKind, UnaryOp};
use super::attribute::{Attribute, AttributeArg};
use super::parser::{ParseResult, Parser};
use crate::diag::{self, Position};
use crate::strings::StringId;
use crate::symbol::{Linkage, SymbolId};
use crate::types::{TypeId, TypeKind};

/// What a declaration's attributes are recorded against: every declaration
/// of a name with linkage is one entity, however many symbols the parser
/// binds for it -- a definition binds a fresh one -- and anything else is
/// its own symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum WrittenOn {
    /// A function or object with linkage, by name.
    Linked(StringId),
    /// A declaration with no linkage: a local, a typedef.
    Unlinked(SymbolId),
}

/// What `__builtin_has_attribute` is asked about.
enum Subject {
    /// A declaration, named by its first argument.
    Declaration(SymbolId),
    /// A type: a type-name, or the type of an expression that names no
    /// declaration.
    Type(TypeId),
}

impl Parser<'_> {
    /// Record the attributes written on the declaration just bound to
    /// `on`, taking them from the pending slot.
    pub(super) fn record_written_attrs(&mut self, on: WrittenOn) {
        let written = std::mem::take(&mut self.pending_written_attrs);
        if !written.is_empty() {
            self.written_attrs.entry(on).or_default().extend(written);
        }
    }

    /// What the attributes on a declaration of `symbol` are recorded
    /// against; see [`WrittenOn`].
    pub(super) fn written_on(&self, symbol: SymbolId) -> WrittenOn {
        let s = self.symbols.get(symbol);
        if s.linkage == Linkage::None {
            WrittenOn::Unlinked(symbol)
        } else {
            WrittenOn::Linked(s.name)
        }
    }

    /// `__builtin_has_attribute(T-or-E, attribute)`, the name already
    /// consumed: an integer constant, 1 when the declaration `E` names --
    /// or, for a type-name or an expression naming none, the type -- has
    /// the attribute, else 0. The expression is not evaluated.
    ///
    /// A declaration has an attribute written on any of its declarations,
    /// in either spelling; an attribute asked with arguments must have them
    /// too where gcc compares them -- an alignment, a section, a visibility,
    /// a message, a vector size, `nonnull`'s positions -- and otherwise its
    /// presence is the answer, as gcc's is ([`arguments_match`]). A type
    /// has the attributes c17 records on types: `aligned`, `packed`,
    /// `vector_size`, `transparent_union`, `scalar_storage_order` and, on a
    /// function type, `noreturn`. An attribute c17 does not know is gcc's
    /// error.
    pub(super) fn parse_has_attribute(&mut self, pos: Position) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let subject = if self.starts_type_name() {
            Subject::Type(self.parse_type_name()?)
        } else {
            let expr = self.parse_assignment_expr()?;
            match Self::declaration_named(&expr) {
                Some(symbol) => Subject::Declaration(symbol),
                None => Subject::Type(expr.typ.unwrap_or(self.types.int_id)),
            }
        };
        self.expect_special(b',')?;
        let query = self.parse_queried_attribute();
        self.expect_special(b')')?;
        let answer = query.is_some_and(|query| match subject {
            Subject::Declaration(symbol) => self.declaration_has(symbol, &query),
            Subject::Type(typ) => self.type_has(typ, &query),
        });
        Ok(Self::typed_expr(
            ExprKind::IntLit(i64::from(answer)),
            self.types.int_id,
            pos,
        ))
    }

    /// The declaration `expr` names: an identifier, through any `*&`.
    fn declaration_named(expr: &Expr) -> Option<SymbolId> {
        match &expr.kind {
            ExprKind::Ident(symbol) => Some(*symbol),
            ExprKind::Unary {
                op: UnaryOp::Deref,
                operand,
            } => match &operand.kind {
                ExprKind::Unary {
                    op: UnaryOp::AddrOf,
                    operand,
                } => Self::declaration_named(operand),
                _ => None,
            },
            _ => None,
        }
    }

    /// The attribute `__builtin_has_attribute` asks about
    /// ([`Parser::parse_attribute_query`]). `None` after gcc's error for a
    /// name it does not know, or an argument that was diagnosed.
    fn parse_queried_attribute(&mut self) -> Option<Attribute> {
        let pos = self.current_pos();
        let arch = self.types.target().arch;
        let known = self
            .current_ident()
            .is_some_and(|id| crate::kw::attribute_supported(id, arch));
        if !known {
            let name = self.get_ident_name(self.current()).unwrap_or_default();
            diag::error_args(pos, "unknown attribute '{0}'", &[&name]);
            self.skip_to_closing_paren();
            return None;
        }
        self.parse_attribute_query()
    }

    /// Step over what is left of the argument list up to the `)` that
    /// closes it, leaving that `)` current.
    fn skip_to_closing_paren(&mut self) {
        let mut depth = 0usize;
        while !self.is_eof() {
            if self.is_special(b'(') {
                depth += 1;
            } else if self.is_special(b')') {
                if depth == 0 {
                    return;
                }
                depth -= 1;
            }
            self.advance();
        }
    }

    /// Whether the declaration `symbol` has the attribute `query`.
    fn declaration_has(&self, symbol: SymbolId, query: &Attribute) -> bool {
        let bare = query.name.trim_matches('_');
        let written = self.written_attrs.get(&self.written_on(symbol));
        let biggest = self.types.biggest_alignment();
        written
            .into_iter()
            .flatten()
            .filter(|a| a.is_named(bare))
            .any(|a| arguments_match(bare, query, a, biggest))
            || self.type_has(self.symbols.get(symbol).typ, query)
    }

    /// Whether the type `typ` has the attribute `query`, as far as c17
    /// records attributes on types.
    fn type_has(&self, typ: TypeId, query: &Attribute) -> bool {
        let t = self.types.get(typ);
        let wanted = |value: u64| match query.args.first() {
            Some(AttributeArg::Int(n)) => *n == i128::from(value),
            _ => true,
        };
        match query.name.trim_matches('_') {
            "aligned" => t.explicit_align.is_some_and(|a| wanted(u64::from(a))),
            "packed" => t
                .composite
                .as_ref()
                .is_some_and(|c| !c.members.is_empty() && c.members.iter().all(|m| m.align.packed)),
            "vector_size" => self.types.is_vector(typ) && wanted(self.types.size_bytes(typ) as u64),
            "transparent_union" => t.composite.as_ref().is_some_and(|c| c.transparent),
            "scalar_storage_order" => t.composite.as_ref().is_some_and(|c| c.reverse_order),
            "noreturn" => {
                let func = match t.kind {
                    TypeKind::Pointer => t.base,
                    _ => Some(typ),
                };
                func.is_some_and(|f| self.types.get(f).noreturn)
            }
            _ => false,
        }
    }
}

/// Whether `written`, an attribute `bare` on a declaration, answers `query`:
/// any one does when the query gives no arguments; otherwise gcc compares
/// the arguments of the attributes whose value is their point -- the
/// alignment, which with none written is `biggest`, a section, a
/// visibility, a message, an alias target, a vector size -- and not those
/// of the rest, where the name alone decides.
fn arguments_match(bare: &str, query: &Attribute, written: &Attribute, biggest: u32) -> bool {
    // `nonnull` without arguments is every pointer parameter, which answers
    // any query; one with arguments answers a query naming some of them --
    // and, as in gcc, not a query naming none.
    if bare == "nonnull" {
        return written.args.is_empty()
            || (!query.args.is_empty() && query.args.iter().all(|a| written.args.contains(a)));
    }
    if query.args.is_empty() {
        return true;
    }
    match bare {
        "aligned" => {
            let alignment = match written.args.first() {
                Some(AttributeArg::Int(n)) => *n,
                _ => i128::from(biggest),
            };
            query.args.first() == Some(&AttributeArg::Int(alignment))
        }
        "section" | "visibility" | "warning" | "error" | "alias" | "ifunc" | "vector_size" => {
            query.args == written.args
        }
        _ => true,
    }
}
