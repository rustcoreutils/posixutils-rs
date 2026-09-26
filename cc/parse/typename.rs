//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Type names (C17 6.7.7), as they appear in casts, sizeof, _Alignof,
// compound literals and _Generic. Their specifier-qualifier list is read by
// the one declaration-specifier loop, in declaration.rs.
//

use super::ast::Expr;
use super::declaration::SpecContext;
use super::parser::{DeclaratorContext, ParseError, ParseResult, ParsedDeclarator, Parser};
use crate::strings::StringId;
use crate::token::lexer::TokenType;
use crate::types::TypeId;

impl Parser<'_> {
    /// Check if identifier is a type-starting keyword (for cast/sizeof
    /// disambiguation).
    ///
    /// Any type *qualifier* can begin a type name -- C17 6.7.7 makes a
    /// type-name a specifier-qualifier-list, in either order -- and a
    /// qualifier at the head of a parenthesised construct cannot be anything
    /// else, so admitting them here does not make a cast ambiguous with an
    /// expression. Only `const`, `volatile` and `_Atomic` carried
    /// `TYPE_KEYWORD` in the table, so `sizeof(__const int)` and
    /// `(__const int *)p` were rejected while `sizeof(const int)` was fine.
    pub(crate) fn is_type_keyword(id: crate::strings::StringId) -> bool {
        crate::kw::has_tag(id, crate::kw::TYPE_KEYWORD)
            || crate::kw::has_tag(id, crate::kw::QUALIFIER)
    }

    /// Parse a type name (required, returns error if not a type)
    pub(super) fn parse_type_name(&mut self) -> ParseResult<TypeId> {
        self.try_parse_type_name()
            .ok_or_else(|| ParseError::new("expected type name".to_string(), self.current_pos()))
    }

    /// Parse a type-name: a specifier-qualifier list followed by an optional
    /// abstract declarator (C17 6.7.7).
    ///
    /// Speculative at the first token only. The caller uses it to tell
    /// `(type)expr` from `(expr)`, and that one token settles it
    /// ([`Self::starts_type_name`]): `None` means no type-name begins here and
    /// the cursor has not moved. Past it the type-name is committed, and a
    /// fault inside it is reported rather than answered with `None` -- a
    /// caller that re-read the tokens as an expression from wherever the parse
    /// had stopped diagnosed neither problem.
    ///
    /// The abstract declarator goes through `parse_declarator`, the same
    /// parser every other declarator uses: an abstract declarator is just a
    /// declarator whose identifier is absent, which it already represents as
    /// `StringId::EMPTY`.
    pub(crate) fn try_parse_type_name(&mut self) -> Option<TypeId> {
        self.try_parse_type_name_vm().map(|(typ, _dims)| typ)
    }

    /// A type-name together with the size expressions of its variably-modified
    /// array levels, outermost-first.
    ///
    /// Only `sizeof` and `typeof` need the expressions. C17 6.5.3.4p2
    /// evaluates the operand of `sizeof` when the type is a variable length
    /// array, and the size cannot be recovered afterwards: `int[n]`, `int[m]`
    /// and `int[]` all intern to one `TypeId`. Every other caller wants the
    /// type alone and uses [`Self::try_parse_type_name`].
    pub(crate) fn try_parse_type_name_vm(&mut self) -> Option<(TypeId, Vec<Expr>)> {
        if !self.starts_type_name() {
            return None;
        }
        // The attributes a type-name carries are its own; the declaration it
        // may sit inside neither lends it any nor receives any back.
        let outer = self.take_pending_decl_attrs();
        let result = self.parse_type_name_parts();
        self.restore_pending_decl_attrs(outer);
        Some(result)
    }

    /// Whether the current token begins a type-name, decided from that token
    /// alone (and, for `typeof`, the `(` after it).
    ///
    /// A type keyword or qualifier, an attribute and a typedef name each can
    /// begin nothing else in an expression position. Neither can a storage
    /// class, a function specifier or `_Alignas`, which a type-name may not
    /// contain: admitting them commits to the type-name reading, so they draw
    /// the type-name's own diagnostic instead of "undeclared identifier".
    /// `typeof` needs its `(`, since gcc lets a C17 program use the word as an
    /// ordinary identifier.
    pub(crate) fn starts_type_name(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }
        let Some(id) = self.get_ident_id(self.current()) else {
            return false;
        };
        if matches!(
            id,
            crate::kw::TYPEOF | crate::kw::GNU_TYPEOF | crate::kw::GNU_TYPEOF2
        ) {
            return self.next_token_is_open_paren();
        }
        const NEVER_AN_EXPRESSION: u32 = crate::kw::STORAGE
            | crate::kw::INLINE_KW
            | crate::kw::NORETURN_KW
            | crate::kw::ALIGNAS_KW
            | crate::kw::ATTR_KW;
        Self::is_type_keyword(id)
            || crate::kw::has_tag(id, NEVER_AN_EXPRESSION)
            || self.symbols.lookup_typedef(id).is_some()
    }

    /// [`Self::try_parse_type_name_vm`] past its first token, with the
    /// enclosing declaration's attribute slots already set aside.
    fn parse_type_name_parts(&mut self) -> (TypeId, Vec<Expr>) {
        let specs = match self.parse_declaration_specifiers(SpecContext::TypeName) {
            Ok(specs) => specs,
            Err(e) => {
                crate::diag::error(e.pos, &e.message);
                self.resync_to_enclosing_paren();
                return (self.types.int_id, Vec::new());
            }
        };
        let base = match specs.id {
            Some(id) => id,
            None => self.intern_type_with_tag(&specs.ty),
        };

        let declarator_start = self.pos;
        match self.parse_declarator(base, DeclaratorContext::TypeName) {
            Ok(ParsedDeclarator { name, typ, vla, .. }) => {
                // An abstract declarator names nothing. gcc's wording, and the
                // type-name is kept: one fault, one diagnostic.
                if name != StringId::EMPTY {
                    let at = (declarator_start..self.pos)
                        .map(|i| &self.tokens[i])
                        .find(|t| self.get_ident_id(t) == Some(name))
                        .map_or_else(|| self.current_pos(), |t| t.pos);
                    crate::diag::error_args(at, "expected ')' before '{0}'", &[self.str(name)]);
                    self.resync_to_enclosing_paren();
                }
                // Declarator levels are outermost, specifier levels innermost:
                // in `typeof(int[n])[3]` the constant 3 is the outer extent and
                // `n` the inner one. Concatenated the other way round,
                // `int[3][n]` and `int[n][3]` would come out the same size --
                // right for one shape and wrong for another.
                let mut dims = vla;
                dims.extend(specs.vm_dims);
                (self.apply_type_name_attrs(typ), dims)
            }
            // The declarator after a committed specifier-qualifier list is
            // simply invalid. Report its own error: rewinding discarded it and
            // let the caller re-read the tokens as an expression, which
            // diagnosed neither problem -- `sizeof(char[-1])` drew "undeclared
            // identifier 'char'" where gcc says "size of unnamed array is
            // negative".
            Err(e) => {
                crate::diag::error(e.pos, &e.message);
                self.resync_to_enclosing_paren();
                (self.types.int_id, Vec::new())
            }
        }
    }

    /// Apply the attributes a type-name collected to the type it names.
    ///
    /// They are applied as a declaration applies its own, through the same
    /// functions: `mode` and `vector_size` replace the type, and `aligned`
    /// aligns it as it would a typedef of that type. That puts the alignment
    /// on the *whole* type -- gcc answers 16 for both
    /// `_Alignof(int __attribute__((aligned(16))) *)` and
    /// `_Alignof(int * __attribute__((aligned(16))))`, just as it aligns the
    /// pointer `p` in either spelling of the declaration.
    fn apply_type_name_attrs(&mut self, typ: TypeId) -> TypeId {
        let typ = self.apply_pending_type_attrs(typ);
        let align = self.pending_alignas.take();
        self.align_typedef_type(typ, align)
    }

    /// Recover from an error inside the tag specifier that began at `start`.
    ///
    /// When the error arose inside the braces, skip to just past the `}` that
    /// closes them and let the declarator after it parse as usual: in
    /// `*(struct { char x[n]; } *)p` the cast is still a cast to a pointer,
    /// and giving up on the whole type-name instead made the `*` in front
    /// report a second, spurious error. Anywhere else there is no body to
    /// step over, and the type-name is abandoned up to its `)`.
    pub(super) fn skip_failed_tag_specifier(&mut self, start: usize) {
        let failed_at = self.pos;
        self.pos = start;
        while self.pos < failed_at && !self.is_special(b'{') {
            self.advance();
        }
        if self.pos == failed_at {
            self.resync_to_enclosing_paren();
            return;
        }
        let mut depth = 0u32;
        while !self.is_eof() {
            if self.is_special(b'{') {
                depth += 1;
            } else if self.is_special(b'}') {
                depth -= 1;
                if depth == 0 {
                    self.advance();
                    return;
                }
            }
            self.advance();
        }
    }

    /// After a committed type-name error, skip to the `)` that closes the
    /// construct the caller opened, so one bad declarator draws one
    /// diagnostic rather than cascading into "expected ')'".
    ///
    /// Every caller of `try_parse_type_name_vm` is positioned just inside a
    /// `(` -- `sizeof(`, `_Alignof(`, `_Atomic(`, `typeof(`, a cast, a
    /// compound literal -- so the token that ends the construct is the first
    /// `)` not nested inside a bracket or paren opened after this point.
    ///
    /// The cursor usually sits *inside* an unclosed `[` when this is called,
    /// the array bound being where the declarator failed, so a closer with no
    /// opener is one of the caller's and must not drive the depth negative.
    fn resync_to_enclosing_paren(&mut self) {
        let mut depth = 0i32;
        while !self.is_eof() {
            if self.is_special(b'(') || self.is_special(b'[') {
                depth += 1;
            } else if self.is_special(b']') {
                depth = (depth - 1).max(0);
            } else if self.is_special(b')') {
                if depth == 0 {
                    return;
                }
                depth -= 1;
            }
            self.advance();
        }
    }
}
