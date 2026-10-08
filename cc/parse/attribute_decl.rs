//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Attribute declarations: a GNU attribute-specifier list standing alone
// before `;`, at file scope or as a statement
//

use super::ast::{Declaration, ExternalDecl, Stmt};
use super::attribute::{AttributeList, ATTRIBUTE_WARNING};
use super::parser::{ParseResult, Parser};
use crate::diag;
use crate::token::lexer::{Position, TokenValue};
use gettextrs::gettext;

/// The one attribute an attribute statement gives a meaning to.
const FALLTHROUGH: &str = "fallthrough";

impl Parser<'_> {
    /// Whether an attribute declaration starts here: one or more
    /// `__attribute__((...))` and then `;`, with nothing to declare.
    ///
    /// gcc reads this as an "attribute declaration" -- the GNU spelling of
    /// C23's `[[attr]];` -- rather than as a declaration that declares
    /// nothing. Decided by looking ahead, so the attributes are parsed (and
    /// diagnosed) once, by whichever path takes them.
    pub(super) fn at_attribute_declaration(&self) -> bool {
        let mut i = self.pos;
        let mut seen = false;
        while let Some(tok) = self.tokens.get(i) {
            let is_attr_kw = self
                .get_ident_id(tok)
                .is_some_and(|id| crate::kw::has_tag(id, crate::kw::ATTR_KW));
            if !is_attr_kw {
                return seen && self.token_is_special(i, b';');
            }
            match self.past_balanced_parens(i + 1) {
                Some(next) => i = next,
                None => return false,
            }
            seen = true;
        }
        false
    }

    /// Whether the token at index `i` is the special `c`.
    fn token_is_special(&self, i: usize, c: u8) -> bool {
        self.tokens
            .get(i)
            .is_some_and(|t| matches!(t.value, TokenValue::Special(v) if v == c as u32))
    }

    /// The index just past the parenthesized group opening at `i`, or `None`
    /// when no `(` is there or it never closes.
    fn past_balanced_parens(&self, mut i: usize) -> Option<usize> {
        if !self.token_is_special(i, b'(') {
            return None;
        }
        let mut depth = 0usize;
        while i < self.tokens.len() {
            if self.token_is_special(i, b'(') {
                depth += 1;
            } else if self.token_is_special(i, b')') {
                depth -= 1;
                if depth == 0 {
                    return Some(i + 1);
                }
            }
            i += 1;
        }
        None
    }

    /// GNU label attributes, `L: __attribute__((unused)) stmt`: consume and
    /// discard attributes after a label's colon when what follows them is a
    /// statement. They say nothing c17 acts on -- gcc's label attributes are
    /// `unused`, `hot` and `cold`, hints only.
    ///
    /// Attributes followed by `;` are an attribute statement, and attributes
    /// followed by a declaration belong to that declaration; both are left
    /// where they are.
    ///
    /// binutils needs it: gas/read.c writes `just_record_alignment:
    /// ATTRIBUTE_UNUSED_LABEL` straight before an `if`.
    pub(super) fn parse_label_attributes(&mut self) {
        let mut end = self.pos;
        while self
            .tokens
            .get(end)
            .and_then(|tok| self.get_ident_id(tok))
            .is_some_and(|id| crate::kw::has_tag(id, crate::kw::ATTR_KW))
        {
            match self.past_balanced_parens(end + 1) {
                Some(next) => end = next,
                None => return,
            }
        }
        if end == self.pos {
            return;
        }
        let start = self.pos;
        self.pos = end;
        let statement_follows =
            !self.is_special(b';') && !self.is_special(b'}') && !self.is_declaration_start();
        self.pos = start;
        if statement_follows {
            let saved = self.take_pending_decl_attrs();
            self.parse_attributes();
            self.restore_pending_decl_attrs(saved);
        }
    }

    /// Parse an attribute declaration through its `;`.
    ///
    /// The attributes apply to nothing, so whatever they would have left
    /// pending for a declarator -- an alignment, a mode -- is discarded.
    fn parse_attribute_declaration(&mut self) -> ParseResult<(Position, AttributeList)> {
        let pos = self.current_pos();
        let saved = self.take_pending_decl_attrs();
        let attrs = self.parse_attributes();
        self.restore_pending_decl_attrs(saved);
        self.expect_special(b';')?;
        Ok((pos, attrs))
    }

    /// An attribute declaration at file scope, which declares nothing.
    pub(super) fn parse_file_attribute_declaration(&mut self) -> ParseResult<ExternalDecl> {
        let (pos, attrs) = self.parse_attribute_declaration()?;
        if attrs.find(FALLTHROUGH).is_none() {
            diag::pedwarn_default(pos, &gettext("empty declaration"));
        } else {
            diag::group_pedwarn_default(
                ATTRIBUTE_WARNING,
                pos,
                &gettext("'fallthrough' attribute at top level"),
            );
        }
        Ok(ExternalDecl::Declaration(Declaration {
            declarators: vec![],
        }))
    }

    /// An attribute declaration as a statement: with `fallthrough`, the null
    /// statement marking a deliberate fall into the next `case`; without it,
    /// an empty declaration. Either way it does nothing at run time.
    pub(super) fn parse_attribute_statement(&mut self) -> ParseResult<Stmt> {
        let (pos, attrs) = self.parse_attribute_declaration()?;
        if attrs.find(FALLTHROUGH).is_some() {
            self.check_fallthrough_statement(pos, &attrs);
        } else {
            diag::pedwarn_default(pos, &gettext("empty declaration"));
        }
        Ok(Stmt::Empty)
    }

    /// Diagnose a `fallthrough` statement as gcc does, as far as the next
    /// token can tell: outside every `switch` it is an error, and anywhere
    /// but just before a label it falls through to nothing.
    fn check_fallthrough_statement(&self, pos: Position, attrs: &AttributeList) {
        self.warn_ignored_beside_fallthrough(pos, attrs);
        if self.switch_depth == 0 {
            diag::error(pos, &gettext("invalid use of attribute 'fallthrough'"));
        } else if !self.may_reach_label() {
            diag::pedwarn_default(
                pos,
                &gettext("attribute 'fallthrough' not preceding a case label or default label"),
            );
        }
    }

    /// The rest of a `fallthrough` statement's attributes, which apply to
    /// nothing. An unrecognised one has already been warned about as such.
    fn warn_ignored_beside_fallthrough(&self, pos: Position, attrs: &AttributeList) {
        for attr in &attrs.attrs {
            if attr.is_named(FALLTHROUGH) {
                if !attr.args.is_empty() {
                    diag::group_warning(
                        ATTRIBUTE_WARNING,
                        pos,
                        &gettext("'fallthrough' attribute specified with a parameter"),
                    );
                }
            } else if self.is_supported_attribute(&attr.name) {
                diag::group_warning_args(
                    ATTRIBUTE_WARNING,
                    pos,
                    "'{0}' attribute ignored",
                    &[&attr.name],
                );
            }
        }
    }

    fn is_supported_attribute(&self, name: &str) -> bool {
        self.idents
            .lookup(name)
            .is_some_and(|id| crate::kw::has_tag(id, crate::kw::SUPPORTED_ATTR))
    }

    /// Whether what comes next may lead to a label: a label itself, or a
    /// brace.
    ///
    /// gcc decides "preceding a label" by control flow, which a brace leaves
    /// open: falling out of `{ ... }`, or into `{ case 2: ... }`, reaches a
    /// `case` just the same. So a brace is given the benefit of the doubt,
    /// and only a statement that plainly follows the attribute is reported.
    fn may_reach_label(&self) -> bool {
        self.is_keyword(crate::kw::CASE)
            || self.is_keyword(crate::kw::DEFAULT)
            || self.is_special(b'{')
            || self.is_special(b'}')
            || (self.current_ident().is_some() && self.next_token_is_special(b':'))
    }
}
