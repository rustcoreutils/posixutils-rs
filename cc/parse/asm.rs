//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GCC asm labels on declarations and extended asm statements
//

use super::ast::{AsmOperand, ExternalDecl, LabelId, Stmt};
use super::parser::{ParseError, ParseResult, Parser};
use crate::strings::StringId;
use crate::symbol::Namespace;
use crate::token::lexer::{payload_text, TokenType, TokenValue};
use crate::token::literal;
use crate::types::TypeModifiers;

impl Parser<'_> {
    /// Check if current token is __asm or __asm__
    pub(super) fn is_asm_keyword(&self) -> bool {
        self.current_ident()
            .is_some_and(|id| crate::kw::has_tag(id, crate::kw::ASM_KW))
    }

    /// Parse `__asm("name")` / `__asm__("name")` on a declaration: a GCC asm
    /// label, which renames the symbol the declaration refers to.
    ///
    /// `extern int myfn(int) __asm__("realfn");` still declares `myfn` for the
    /// source to use, but every emitted reference names `realfn`. The label is
    /// left in [`Parser::pending_asm_label`] for the declarator to claim.
    ///
    /// The label is a string *sequence*, not a single literal: glibc spells it
    /// `__ASMNAME(cname)`, which expands to
    /// `__STRING(__USER_LABEL_PREFIX__) cname` — two adjacent literals, `""`
    /// and `"stpncpy"` on ELF. They concatenate as in any other C string
    /// context.
    pub(super) fn parse_asm_label(&mut self) {
        while self.is_asm_keyword() {
            self.advance(); // consume __asm/__asm__

            // Expect '('
            if !self.is_special(b'(') {
                return;
            }
            self.advance(); // consume '('

            // Collect the string literals, and skip anything else so that a
            // shape we do not model still parses.
            let mut label = String::new();
            let mut depth = 1;
            while depth > 0 && !self.is_eof() {
                if self.is_special(b'(') {
                    depth += 1;
                } else if self.is_special(b')') {
                    depth -= 1;
                    if depth == 0 {
                        self.advance();
                        break;
                    }
                } else if depth == 1 {
                    if let TokenValue::String(s) = &self.current().value {
                        label.push_str(&payload_text(s));
                    }
                }
                self.advance();
            }

            // An empty label is not a rename. `__asm__("")` would ask for a
            // nameless symbol, which is not something GCC accepts either.
            if !label.is_empty() {
                self.pending_asm_label = Some(label);
            }
        }
    }

    /// Record, on whichever symbol is now bound to `name`, the facts about it
    /// that accumulate across every declaration in the translation unit.
    ///
    /// Two of them so far -- the GCC asm label and whether anything said
    /// `extern` -- and they share this helper because they share the problem
    /// that makes them awkward: a redeclaration, and in particular a
    /// definition, binds a *fresh* symbol that would not otherwise inherit
    /// what earlier declarations established, and the declaration that settles
    /// the question is allowed to come afterwards.
    ///
    /// Consuming the pending asm label means a declaration list gives each
    /// declarator only the label written on it: in `int a __asm__("x"), b;`
    /// only `a` is renamed.
    pub(super) fn settle_declaration_facts(
        &mut self,
        name: StringId,
        storage_class: TypeModifiers,
    ) {
        // C99 6.7.4p6 asks whether *any* declaration of this name says
        // `extern`, including ones not yet parsed, so accumulate rather than
        // overwrite. See `Symbol::has_extern_decl`.
        if storage_class.contains(TypeModifiers::EXTERN) {
            self.declared_extern_fns.insert(name);
        }
        if !storage_class.contains(TypeModifiers::INLINE) {
            self.declared_non_inline_fns.insert(name);
        }
        if let Some(id) = self.symbols.lookup_id(name, Namespace::Ordinary) {
            let sym = self.symbols.get_mut(id);
            sym.has_extern_decl |= self.declared_extern_fns.contains(&name);
            sym.has_non_inline_decl |= self.declared_non_inline_fns.contains(&name);
        }
        self.settle_asm_label(name);
    }

    /// Give the symbol now bound to `name` its asm label: the one this
    /// declaration wrote, which is then recorded for every later declaration
    /// of the name, or else the one an earlier declaration wrote.
    ///
    /// Asked of every declaration with linkage, at block scope too. C99
    /// 6.7.4p7 asks the *file-scope* declarations whether a function is
    /// `inline` or `extern`, so a block-scope one leaves those facts alone --
    /// but it names the same object or function, under the same assembler
    /// name: `extern int x __asm__("y");` inside a function reads `y`, and a
    /// block-scope redeclaration of a labelled function still calls the
    /// label.
    pub(super) fn settle_asm_label(&mut self, name: StringId) {
        let label = match self.pending_asm_label.take() {
            Some(label) => {
                self.declared_asm_labels.insert(name, label.clone());
                Some(label)
            }
            None => self.declared_asm_labels.get(&name).cloned(),
        };
        if let (Some(label), Some(id)) = (label, self.symbols.lookup_id(name, Namespace::Ordinary))
        {
            self.symbols.get_mut(id).asm_label = Some(label);
        }
    }

    /// Parse GCC extended inline assembly statement
    /// Format: __asm__ [volatile] [goto] ( "template" [: outputs [: inputs [: clobbers [: goto_labels]]]] );
    pub(super) fn parse_asm_statement(&mut self) -> ParseResult<Stmt> {
        let pos = self.current_pos();
        self.advance(); // consume __asm/__asm__

        // Parse optional qualifiers: 'volatile', '__volatile__', 'inline', '__inline__', 'goto'
        let mut is_volatile = false;
        let mut _is_goto = false;
        while let Some(name_id) = self.current_ident() {
            match name_id {
                crate::kw::VOLATILE | crate::kw::GNU_VOLATILE => {
                    is_volatile = true;
                    self.advance();
                }
                crate::kw::INLINE | crate::kw::GNU_INLINE => {
                    self.advance();
                }
                crate::kw::GOTO => {
                    _is_goto = true;
                    self.advance();
                }
                _ => break,
            }
        }

        self.expect_special(b'(')?;

        // Parse template string (may be multiple concatenated strings)
        let mut template = self.parse_asm_string_literal()?;

        // Basic asm, with no colon, is emitted as written: gcc substitutes
        // nothing into it, so `%eax` and `%%` both reach the assembler
        // unchanged. Escaping every `%` lets it share the extended-asm path.
        if !self.is_special(b':') {
            template = template.replace('%', "%%");
        }

        // Parse outputs (after first ':')
        let outputs = if self.is_special(b':') {
            self.advance();
            self.parse_asm_operands()?
        } else {
            vec![]
        };

        // Parse inputs (after second ':')
        let inputs = if self.is_special(b':') {
            self.advance();
            self.parse_asm_operands()?
        } else {
            vec![]
        };

        // Parse clobbers (after third ':')
        let clobbers = if self.is_special(b':') {
            self.advance();
            self.parse_asm_clobbers()?
        } else {
            vec![]
        };

        // Parse goto labels (after fourth ':')
        let goto_labels = if self.is_special(b':') {
            self.advance();
            self.parse_asm_goto_labels()?
        } else {
            vec![]
        };

        self.expect_special(b')')?;
        self.expect_special(b';')?;

        // Every asm is already treated as volatile: `Opcode::Asm` is a memory
        // access, so DCE never deletes one and no pass reorders it past
        // another memory operation. The qualifier has nothing left to add.
        let _ = is_volatile;

        Ok(Stmt::Asm {
            pos,
            template,
            outputs,
            inputs,
            clobbers,
            goto_labels,
        })
    }

    /// Parse GNU basic asm at file scope: `asm ( string-literal ) ;`, the
    /// `__asm` and `__asm__` spellings alike.
    ///
    /// Only the basic form exists here, as in gcc: no qualifier -- `volatile`,
    /// `inline` and `goto` are errors where the `(` belongs -- and no operands,
    /// whose first `:` gcc reads as a missing `)`. The text is the assembler's
    /// verbatim; nothing is substituted into it, so `%` stays as written.
    pub(super) fn parse_file_scope_asm(&mut self) -> ParseResult<ExternalDecl> {
        let pos = self.current_pos();
        self.advance(); // consume asm / __asm / __asm__
        if !self.is_special(b'(') {
            return Err(ParseError::new(
                format!("expected '(' before {}", self.describe_current()),
                self.current_pos(),
            ));
        }
        self.advance(); // consume '('
        match self.peek() {
            TokenType::String => {}
            TokenType::WideString | TokenType::Utf16String | TokenType::Utf32String => {
                return Err(ParseError::new(
                    "a wide string is invalid in this context",
                    self.current_pos(),
                ));
            }
            _ => {
                return Err(ParseError::new(
                    format!("expected string literal before {}", self.describe_current()),
                    self.current_pos(),
                ));
            }
        }
        let text = self.parse_asm_string_literal()?;
        if !self.is_special(b')') {
            return Err(ParseError::new(
                format!("expected ')' before {}", self.describe_current()),
                self.current_pos(),
            ));
        }
        self.advance(); // consume ')'
        self.expect_special(b';')?;
        Ok(ExternalDecl::Asm { pos, text })
    }

    /// The current token as gcc names it after "before": `'volatile'`,
    /// `':' token`, `numeric constant`.
    fn describe_current(&self) -> String {
        match &self.current().value {
            TokenValue::Ident(id) => format!("'{}'", self.idents.get_opt(*id).unwrap_or("?")),
            TokenValue::Special(v) => {
                format!("'{}' token", crate::token::lexer::show_special(*v))
            }
            _ => match self.peek() {
                TokenType::Number => "numeric constant".to_string(),
                TokenType::Char
                | TokenType::WideChar
                | TokenType::Utf16Char
                | TokenType::Utf32Char => "character constant".to_string(),
                TokenType::StreamEnd => "end of input".to_string(),
                _ => "string constant".to_string(),
            },
        }
    }

    /// Parse an asm template string (handles string concatenation)
    fn parse_asm_string_literal(&mut self) -> ParseResult<String> {
        let mut result = String::new();

        if self.peek() != TokenType::String {
            return Err(ParseError::new(
                "expected string literal in asm template",
                self.current_pos(),
            ));
        }

        // The first string, then any adjacent ones it concatenates with.
        loop {
            let token = self.consume();
            if let TokenValue::String(s) = &token.value {
                let elements = literal::parse_string_literal(s);
                literal::check_elements(&elements, literal::CHAR_UNIT_BITS, token.pos);
                result.push_str(&literal::literal_bytes(&elements));
            }
            if self.peek() != TokenType::String {
                break;
            }
        }

        // The literal's value is a payload, one `char` per byte; the
        // assembler is handed text, decoded from those bytes.
        Ok(payload_text(&result))
    }

    /// Parse asm operand list: [name] "constraint" (expr), ...
    fn parse_asm_operands(&mut self) -> ParseResult<Vec<AsmOperand>> {
        let mut operands = Vec::new();

        // Allow empty operand list
        if self.is_special(b':') || self.is_special(b')') {
            return Ok(operands);
        }

        loop {
            // Parse optional symbolic name: [name]
            let name = if self.is_special(b'[') {
                self.advance(); // consume '['
                let name = self.expect_identifier()?;
                self.expect_special(b']')?;
                Some(name)
            } else {
                None
            };

            // Parse constraint string
            if self.peek() != TokenType::String {
                return Err(ParseError::new(
                    "expected constraint string in asm operand",
                    self.current_pos(),
                ));
            }
            let constraint = self.parse_asm_string_literal()?;

            // Parse expression in parentheses
            self.expect_special(b'(')?;
            let expr = self.parse_expression()?;
            self.expect_special(b')')?;

            operands.push(AsmOperand {
                name,
                constraint,
                expr,
            });

            // Check for more operands
            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

        Ok(operands)
    }

    /// Parse asm clobber list: "clobber", ...
    fn parse_asm_clobbers(&mut self) -> ParseResult<Vec<String>> {
        let mut clobbers = Vec::new();

        // Allow empty clobber list
        if self.is_special(b':') || self.is_special(b')') {
            return Ok(clobbers);
        }

        loop {
            if self.peek() != TokenType::String {
                return Err(ParseError::new(
                    "expected clobber string in asm statement",
                    self.current_pos(),
                ));
            }
            let clobber = self.parse_asm_string_literal()?;
            clobbers.push(clobber);

            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

        Ok(clobbers)
    }

    /// Parse asm goto label list: label1, label2, ...
    fn parse_asm_goto_labels(&mut self) -> ParseResult<Vec<LabelId>> {
        let mut labels = Vec::new();

        // Allow empty label list
        if self.is_special(b')') {
            return Ok(labels);
        }

        loop {
            if self.peek() != TokenType::Ident {
                return Err(ParseError::new(
                    "expected label identifier in asm goto",
                    self.current_pos(),
                ));
            }
            let token = self.consume();
            if let TokenValue::Ident(label_id) = token.value {
                labels.push(self.resolve_label(label_id));
            }

            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

        Ok(labels)
    }
}
