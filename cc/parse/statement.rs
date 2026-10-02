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

use super::ast::{BlockItem, Expr, ExprKind, ForInit, Label, Stmt};
use super::parser::{ParseError, ParseResult, Parser};
use crate::diag;
use crate::token::lexer::{Position, SpecialToken, TokenType};
use gettextrs::gettext;

impl Parser<'_> {
    /// Parse a statement, gathering every label in front of it into one
    /// [`Stmt::Labeled`].
    ///
    /// The labels are read in a loop rather than by recursing into the
    /// statement each one prefixes: a generated `switch` can carry tens of
    /// thousands of consecutive `case` labels, and one recursion per label
    /// overflowed the compiler's stack.
    pub fn parse_statement(&mut self) -> ParseResult<Stmt> {
        let mut labels = Vec::new();
        while let Some(label) = self.parse_label()? {
            labels.push(label);
        }
        if labels.is_empty() {
            return self.parse_unlabeled_statement();
        }
        // C17 6.8.1 requires a statement after the label, and a label at the
        // end of a block therefore needs the empty one written out. gcc and
        // clang both accept it without, C23 made it legal, and the idiom is
        // common enough in code that jumps to a cleanup label at the end of a
        // function -- `asm goto`'s own torture test is written that way.
        // Accepted with a warning rather than invented silently.
        let stmt = if self.is_special(b'}') {
            diag::warning(
                self.current_pos(),
                &gettext("a label at the end of a compound statement needs a statement in C17"),
            );
            Stmt::Empty
        } else {
            self.parse_unlabeled_statement()?
        };
        Ok(Stmt::Labeled {
            labels,
            stmt: Box::new(stmt),
        })
    }

    /// Parse one label and its colon, or nothing if the statement does not
    /// start with one.
    ///
    /// A statement keyword is never a goto label: `else:` and `break:` are
    /// errors, not labels named after them.
    fn parse_label(&mut self) -> ParseResult<Option<Label>> {
        match self.current_ident() {
            Some(crate::kw::CASE) => return self.parse_case_label().map(Some),
            Some(crate::kw::DEFAULT) => return self.parse_default_label().map(Some),
            Some(id) if crate::kw::has_tag(id, crate::kw::STMT_KW | crate::kw::ASM_KW) => {
                return Ok(None);
            }
            _ => {}
        }
        if self.peek() != TokenType::Ident {
            return Ok(None);
        }
        // Save position for potential backtrack
        let saved_pos = self.pos;
        let pos = self.current_pos();
        let name = self.expect_identifier()?;
        if self.is_special(b':') {
            self.advance();
            return Ok(Some(Label::Named { name, pos }));
        }
        // Not a label, backtrack
        self.pos = saved_pos;
        Ok(None)
    }

    /// A statement with no label in front of it, which [`Self::parse_label`]
    /// has already ruled out.
    fn parse_unlabeled_statement(&mut self) -> ParseResult<Stmt> {
        if self.at_attribute_declaration() {
            return self.parse_attribute_statement();
        }
        // Check for keywords
        if let Some(name_id) = self.current_ident() {
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
                // GCC extended inline assembly
                crate::kw::ASM | crate::kw::GNU_ASM | crate::kw::GNU_ASM2 => {
                    return self.parse_asm_statement();
                }
                _ => {}
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

    /// The controlling expression of `if`, `while`, `do` or `for`, which
    /// shall have scalar type (C17 6.8.4.1p1, 6.8.5p2).
    fn parse_controlling_expression(&mut self) -> ParseResult<Expr> {
        let cond = self.parse_expression()?;
        self.check_truth_value(&cond);
        Ok(cond)
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
        self.parse_in_block(Self::parse_if_stmt_in_block)
    }

    fn parse_if_stmt_in_block(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'if'
        self.expect_special(b'(')?;
        let cond = self.parse_controlling_expression()?;
        self.expect_special(b')')?;
        let then_stmt = self.parse_substatement()?;

        let else_stmt = if self.is_keyword(crate::kw::ELSE) {
            self.advance();
            Some(Box::new(self.parse_substatement()?))
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
        let cond = self.parse_controlling_expression()?;
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
        if !self.is_keyword(crate::kw::WHILE) {
            return Err(ParseError::new("expected 'while'", self.current_pos()));
        }
        self.advance();

        self.expect_special(b'(')?;
        let cond = self.parse_controlling_expression()?;
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
            let expr = self.parse_controlling_expression()?;
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
    fn parse_switch_body(&mut self) -> ParseResult<Stmt> {
        // One statement, as C17 6.8.4 says. This used to re-block a run of
        // labels into a synthetic compound statement, because a label was a
        // flat sibling marker and the statement it prefixed was not part of
        // it. It only ever reached the labels at the top of the body, which
        // is why a `case` nested inside an unbraced `if` escaped the switch.
        self.switch_depth += 1;
        let body = self.parse_substatement();
        self.switch_depth -= 1;
        body
    }

    /// Parse a case label, including the GNU range form `case lo ... hi:`.
    ///
    /// GCC requires whitespace around the `...`: `case 1...9:` lexes as one
    /// pp-number and is rejected there too ("too many decimal points in
    /// number"), so only the spaced form is accepted here as well.
    fn parse_case_label(&mut self) -> ParseResult<Label> {
        self.advance(); // consume 'case'
        let expr = self.parse_conditional_expr()?;
        let high = if self.is_special_token(SpecialToken::Ellipsis) {
            self.advance();
            Some(self.parse_conditional_expr()?)
        } else {
            None
        };
        self.expect_special(b':')?;
        Ok(Label::Case(expr, high))
    }

    fn parse_default_label(&mut self) -> ParseResult<Label> {
        let pos = self.current_pos();
        self.advance(); // consume 'default'
        self.expect_special(b':')?;
        Ok(Label::Default(pos))
    }

    /// Parse block items (declarations and statements) until closing brace
    fn parse_block_items(&mut self) -> ParseResult<Vec<BlockItem>> {
        let mut items = Vec::new();
        while !self.is_special(b'}') && !self.is_eof() {
            // An attribute declaration starts like a declaration but is a
            // statement: `__attribute__((fallthrough));`.
            if self.is_declaration_start() && !self.at_attribute_declaration() {
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
                    // Last item is not an expression statement (e.g. if, while,
                    // for): the statement expression yields no value, so its
                    // type is `void`, as in gcc.
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
    /// expression statement, labeled or not. GCC takes `({ a: 1; })` as 1
    /// (compile/pr17913). A `case` or `default` label cannot end a statement
    /// expression in a valid program -- its switch would jump into the
    /// statement expression -- but it is still a label, and giving the
    /// expression a value leaves "switch jumps into statement expression" as
    /// the one error, where calling it `void` added another.
    fn ends_in_expr_stmt(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Labeled { stmt, .. } => matches!(**stmt, Stmt::Expr(_)),
            stmt => matches!(stmt, Stmt::Expr(_)),
        }
    }

    /// Split a final statement accepted by [`Self::ends_in_expr_stmt`] into
    /// its value and the labels in front of it. `L: e;` is `L: ; e;`, so the
    /// labels are pushed onto `items` labelling an empty statement, and the
    /// value is evaluated after them exactly where the labels were.
    fn split_labeled_expr_stmt(stmt: Stmt, items: &mut Vec<BlockItem>) -> Expr {
        match stmt {
            Stmt::Expr(expr) => expr,
            Stmt::Labeled { labels, stmt } => {
                items.push(BlockItem::Statement(Box::new(Stmt::Labeled {
                    labels,
                    stmt: Box::new(Stmt::Empty),
                })));
                Self::split_labeled_expr_stmt(*stmt, items)
            }
            _ => unreachable!("checked by ends_in_expr_stmt"),
        }
    }
}
