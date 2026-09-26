//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Statement parsing (C17 6.8) and GNU statement expressions
//

use super::ast::{BlockItem, Expr, ExprKind, ForInit, Stmt};
use super::parser::{ParseError, ParseResult, Parser};
use crate::diag;
use crate::token::lexer::{Position, SpecialToken, TokenType};
use gettextrs::gettext;

impl Parser<'_> {
    pub fn parse_statement(&mut self) -> ParseResult<Stmt> {
        // Check for keywords
        if self.peek() == TokenType::Ident {
            if let Some(name_id) = self.get_ident_id(self.current()) {
                match name_id {
                    crate::kw::IF => return self.parse_if_stmt(),
                    crate::kw::WHILE => return self.parse_while_stmt(),
                    crate::kw::DO => return self.parse_do_while_stmt(),
                    crate::kw::FOR => return self.parse_for_stmt(),
                    crate::kw::RETURN => return self.parse_return_stmt(),
                    crate::kw::BREAK => {
                        let pos = self.current_pos();
                        self.advance();
                        self.expect_special(b';')?;
                        return Ok(Stmt::Break(pos));
                    }
                    crate::kw::CONTINUE => {
                        let pos = self.current_pos();
                        self.advance();
                        self.expect_special(b';')?;
                        return Ok(Stmt::Continue(pos));
                    }
                    crate::kw::GOTO => {
                        let pos = self.current_pos();
                        self.advance();
                        // GNU computed goto: `goto *expr;`
                        if self.is_special(b'*') {
                            self.advance();
                            let target = self.parse_expression()?;
                            self.expect_special(b';')?;
                            return Ok(Stmt::GotoIndirect { target, pos });
                        }
                        let name = self.expect_identifier()?;
                        self.expect_special(b';')?;
                        return Ok(Stmt::Goto { name, pos });
                    }
                    crate::kw::SWITCH => return self.parse_switch_stmt(),
                    crate::kw::CASE => return self.parse_case_label(),
                    crate::kw::DEFAULT => return self.parse_default_label(),
                    // GCC extended inline assembly
                    crate::kw::ASM | crate::kw::GNU_ASM | crate::kw::GNU_ASM2 => {
                        return self.parse_asm_statement();
                    }
                    _ => {}
                }
            }
        }

        // Check for compound statement
        if self.is_special(b'{') {
            return self.parse_block_stmt();
        }

        // Check for empty statement
        if self.is_special(b';') {
            self.advance();
            return Ok(Stmt::Empty);
        }

        // Check for labeled statement
        if self.peek() == TokenType::Ident {
            // Save position for potential backtrack
            let saved_pos = self.pos;
            let pos = self.current_pos();
            let name = self.expect_identifier()?;
            if self.is_special(b':') {
                self.advance();
                // C17 6.8.1 requires a statement after the label, and a label
                // at the end of a block therefore needs the empty one written
                // out. gcc and clang both accept it without, C23 made it
                // legal, and the idiom is common enough in code that jumps to
                // a cleanup label at the end of a function -- `asm goto`'s own
                // torture test is written that way. Accepted with a warning
                // rather than invented silently.
                let stmt = self.parse_labeled_statement()?;
                return Ok(Stmt::Label {
                    name,
                    stmt: Box::new(stmt),
                    pos,
                });
            }
            // Not a label, backtrack
            self.pos = saved_pos;
        }

        // Expression statement
        let expr = self.parse_expression()?;
        self.expect_special(b';')?;
        Ok(Stmt::Expr(expr))
    }

    /// Parse a selection or iteration statement as the block C17 makes it.
    ///
    /// 6.8.4p3 and 6.8.5p5: the whole statement is a block, and so is each of
    /// its substatements ([`Self::parse_substatement`]). The only things an
    /// expression can declare are a tag and its enumeration constants --
    /// `if (sizeof(struct T { int a; }))` -- and without the block they
    /// leaked into the enclosing scope, where gcc rightly reports `struct T`
    /// as incomplete.
    fn parse_in_block(
        &mut self,
        parse: impl FnOnce(&mut Self) -> ParseResult<Stmt>,
    ) -> ParseResult<Stmt> {
        self.symbols.enter_scope();
        let stmt = parse(self);
        self.symbols.leave_scope();
        stmt
    }

    /// The substatement of a selection or iteration statement, which is a
    /// block of its own. See [`Self::parse_in_block`].
    fn parse_substatement(&mut self) -> ParseResult<Stmt> {
        self.parse_in_block(Self::parse_statement)
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
        self.parse_in_block(Self::parse_if_stmt_in_block)
    }

    fn parse_if_stmt_in_block(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'if'
        self.expect_special(b'(')?;
        let cond = self.parse_expression()?;
        self.expect_special(b')')?;
        let then_stmt = self.parse_substatement()?;

        let else_stmt = if self.peek() == TokenType::Ident {
            if let Some(name_id) = self.get_ident_id(self.current()) {
                if name_id == crate::kw::ELSE {
                    self.advance();
                    Some(Box::new(self.parse_substatement()?))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        Ok(Stmt::If {
            cond,
            then_stmt: Box::new(then_stmt),
            else_stmt,
        })
    }

    fn parse_while_stmt(&mut self) -> ParseResult<Stmt> {
        self.parse_in_block(Self::parse_while_stmt_in_block)
    }

    fn parse_while_stmt_in_block(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'while'
        self.expect_special(b'(')?;
        let cond = self.parse_expression()?;
        self.expect_special(b')')?;
        let body = self.parse_substatement()?;

        Ok(Stmt::While {
            cond,
            body: Box::new(body),
        })
    }

    fn parse_do_while_stmt(&mut self) -> ParseResult<Stmt> {
        self.parse_in_block(Self::parse_do_while_stmt_in_block)
    }

    fn parse_do_while_stmt_in_block(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'do'
        let body = self.parse_substatement()?;

        // Expect 'while'
        if self.peek() != TokenType::Ident {
            return Err(ParseError::new("expected 'while'", self.current_pos()));
        }
        if let Some(name) = self.get_ident_name(self.current()) {
            if name != "while" {
                return Err(ParseError::new("expected 'while'", self.current_pos()));
            }
        }
        self.advance();

        self.expect_special(b'(')?;
        let cond = self.parse_expression()?;
        self.expect_special(b')')?;
        self.expect_special(b';')?;

        Ok(Stmt::DoWhile {
            body: Box::new(body),
            cond,
        })
    }

    /// Parse a for statement
    ///
    /// C99 allows declarations in for-init: `for (int i = 0; i < n; i++)`
    /// These declarations are scoped to the for loop (including body).
    fn parse_for_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'for'
        self.expect_special(b'(')?;

        // Enter scope for for-loop declarations (C99)
        // This scope includes init declaration + body
        self.symbols.enter_scope();

        // Parse init (can be declaration or expression)
        let init = if self.is_special(b';') {
            self.advance();
            None
        } else if self.is_declaration_start() {
            // C99: declaration in for-init, bind to for-scope
            // Note: storage class specifiers (static, extern) are forbidden here
            let decl = self.parse_for_init_declaration_and_bind()?;
            // Declaration already consumed the semicolon
            Some(ForInit::Declaration(decl))
        } else {
            let expr = self.parse_expression()?;
            self.expect_special(b';')?;
            Some(ForInit::Expression(expr))
        };

        // Parse condition
        let cond = if self.is_special(b';') {
            self.advance();
            None
        } else {
            let expr = self.parse_expression()?;
            self.expect_special(b';')?;
            Some(expr)
        };

        // Parse post
        let post = if self.is_special(b')') {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.expect_special(b')')?;
        let body = self.parse_substatement()?;

        // Leave for-scope
        self.symbols.leave_scope();

        Ok(Stmt::For {
            init,
            cond,
            post,
            body: Box::new(body),
        })
    }

    fn parse_return_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'return'

        if self.is_special(b';') {
            self.advance();
            return Ok(Stmt::Return(None));
        }

        let expr = self.parse_expression()?;
        self.check_not_vector_value(expr.typ, expr.pos);
        self.expect_special(b';')?;
        Ok(Stmt::Return(Some(expr)))
    }

    fn parse_switch_stmt(&mut self) -> ParseResult<Stmt> {
        self.parse_in_block(Self::parse_switch_stmt_in_block)
    }

    fn parse_switch_stmt_in_block(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'switch'
        self.expect_special(b'(')?;
        let expr = self.parse_expression()?;
        self.expect_special(b')')?;
        let body = self.parse_switch_body()?;
        Ok(Stmt::Switch {
            expr,
            body: Box::new(body),
        })
    }

    /// Parse a `switch` body, which C17 6.8.4 says is one statement.
    ///
    /// `case E : statement` is a single *labeled statement* in the grammar, but
    /// the AST flattens the label into a sibling marker -- `Stmt::Case` carries
    /// the value, not the statement it labels -- which is only sound inside a
    /// block, where the two stay adjacent items of one list. So a labeled
    /// non-compound body gains the block that flattening assumes; a compound
    /// or unlabeled body is returned unchanged.
    fn parse_switch_body(&mut self) -> ParseResult<Stmt> {
        // One statement, as C17 6.8.4 says. This used to re-block a run of
        // labels into a synthetic compound statement, because a label was a
        // flat sibling marker and the statement it prefixed was not part of
        // it. It only ever reached the labels at the top of the body, which
        // is why a `case` nested inside an unbraced `if` escaped the switch.
        self.parse_substatement()
    }

    /// Parse a case label, including the GNU range form `case lo ... hi:`.
    ///
    /// GCC requires whitespace around the `...`: `case 1...9:` lexes as one
    /// pp-number and is rejected there too ("too many decimal points in
    /// number"), so only the spaced form is accepted here as well.
    fn parse_case_label(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'case'
        let expr = self.parse_conditional_expr()?;
        let high = if self.is_special_token(SpecialToken::Ellipsis) {
            self.advance();
            Some(self.parse_conditional_expr()?)
        } else {
            None
        };
        self.expect_special(b':')?;
        let stmt = self.parse_labeled_statement()?;
        Ok(Stmt::Case(expr, high, Box::new(stmt)))
    }

    /// The statement a `case`, `default` or goto label prefixes.
    ///
    /// C17 6.8.1 requires one. A label at the end of a compound statement is
    /// accepted with a warning -- gcc and clang both take it, and C23 made it
    /// legal -- rather than failing on the `}`.
    fn parse_labeled_statement(&mut self) -> ParseResult<Stmt> {
        if self.is_special(b'}') {
            diag::warning(
                self.current_pos(),
                &gettext("a label at the end of a compound statement needs a statement in C17"),
            );
            return Ok(Stmt::Empty);
        }
        self.parse_statement()
    }

    fn parse_default_label(&mut self) -> ParseResult<Stmt> {
        let pos = self.current_pos();
        self.advance(); // consume 'default'
        self.expect_special(b':')?;
        let stmt = self.parse_labeled_statement()?;
        Ok(Stmt::Default(pos, Box::new(stmt)))
    }

    /// Parse block items (declarations and statements) until closing brace
    fn parse_block_items(&mut self) -> ParseResult<Vec<BlockItem>> {
        let mut items = Vec::new();
        while !self.is_special(b'}') && !self.is_eof() {
            if self.is_declaration_start() {
                let decl = self.parse_declaration_and_bind()?;
                items.push(BlockItem::Declaration(decl));
            } else {
                let stmt = self.parse_statement()?;
                items.push(BlockItem::Statement(Box::new(stmt)));
            }
        }
        Ok(items)
    }

    fn parse_block_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_special(b'{')?;

        // Enter block scope
        self.symbols.enter_scope();

        let items = self.parse_block_items()?;

        // Leave block scope
        self.symbols.leave_scope();

        self.expect_special(b'}')?;
        Ok(Stmt::Block(items))
    }

    /// Parse a compound statement without entering a new scope
    ///
    /// Used by function definitions where the scope is already entered
    /// by the function parsing code (to include parameters in scope).
    pub(super) fn parse_block_stmt_no_scope(&mut self) -> ParseResult<Stmt> {
        self.expect_special(b'{')?;
        let items = self.parse_block_items()?;
        self.expect_special(b'}')?;
        Ok(Stmt::Block(items))
    }

    /// Parse a statement expression: ({ stmt; stmt; expr; })
    /// This is a GNU extension that allows a compound statement to be used as an expression.
    /// The value is the result of the last expression in the block.
    pub(crate) fn parse_stmt_expr(&mut self, paren_pos: Position) -> ParseResult<Expr> {
        self.expect_special(b'{')?;

        // Enter block scope for the statement expression
        self.symbols.enter_scope();

        let mut items = self.parse_block_items()?;

        self.expect_special(b'}')?;
        self.expect_special(b')')?;

        // Leave block scope
        self.symbols.leave_scope();

        // The result of a statement expression is the last expression statement.
        // If there are no statements or the last isn't an expression, result is void.
        let (stmts, result, result_type) = if items.is_empty() {
            // Empty statement expression: ({ }) has type void
            (
                Vec::new(),
                Expr::typed(ExprKind::IntLit(0), self.types.void_id, paren_pos),
                self.types.void_id,
            )
        } else {
            // Check if the last item is an expression statement, possibly labeled
            let last = items.pop().unwrap();
            match last {
                BlockItem::Statement(stmt) if Self::ends_in_expr_stmt(&stmt) => {
                    let expr = Self::split_labeled_expr_stmt(*stmt, &mut items);
                    let typ = expr.typ.unwrap_or(self.types.int_id);
                    (items, expr, typ)
                }
                _ => {
                    // Last item is not an expression statement (e.g. if, while, for)
                    // Following sparse: the type becomes void (evaluate.c handles this
                    // by returning NULL which becomes void_ctype)
                    items.push(last);
                    (
                        items,
                        Expr::typed(ExprKind::IntLit(0), self.types.void_id, paren_pos),
                        self.types.void_id,
                    )
                }
            }
        };

        Ok(Self::typed_expr(
            ExprKind::StmtExpr {
                stmts,
                result: Box::new(result),
            },
            result_type,
            paren_pos,
        ))
    }

    /// Whether a statement expression's final statement gives it a value: an
    /// expression statement, or one under any number of labels. GCC takes
    /// `({ a: 1; })` as 1 (compile/pr17913). A `case` or `default` label
    /// cannot end a statement expression in a valid program -- its switch
    /// would jump into the statement expression -- but it is still a label,
    /// and giving the expression a value leaves "switch jumps into statement
    /// expression" as the one error, where calling it `void` added another.
    fn ends_in_expr_stmt(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Expr(_) => true,
            Stmt::Label { stmt, .. } | Stmt::Case(_, _, stmt) | Stmt::Default(_, stmt) => {
                Self::ends_in_expr_stmt(stmt)
            }
            _ => false,
        }
    }

    /// Split a final statement accepted by [`Self::ends_in_expr_stmt`] into
    /// its value and the labels in front of it. `L: e;` is `L: ; e;`, so each
    /// label is pushed onto `items` labelling an empty statement, and the
    /// value is evaluated after them exactly where the labels were.
    fn split_labeled_expr_stmt(stmt: Stmt, items: &mut Vec<BlockItem>) -> Expr {
        match stmt {
            Stmt::Expr(expr) => expr,
            Stmt::Label { name, stmt, pos } => {
                items.push(BlockItem::Statement(Box::new(Stmt::Label {
                    name,
                    stmt: Box::new(Stmt::Empty),
                    pos,
                })));
                Self::split_labeled_expr_stmt(*stmt, items)
            }
            Stmt::Case(low, high, stmt) => {
                let label = Stmt::Case(low, high, Box::new(Stmt::Empty));
                items.push(BlockItem::Statement(Box::new(label)));
                Self::split_labeled_expr_stmt(*stmt, items)
            }
            Stmt::Default(pos, stmt) => {
                let label = Stmt::Default(pos, Box::new(Stmt::Empty));
                items.push(BlockItem::Statement(Box::new(label)));
                Self::split_labeled_expr_stmt(*stmt, items)
            }
            _ => unreachable!("checked by ends_in_expr_stmt"),
        }
    }
}
