//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Expression parsing for pcc C99 compiler
//

use super::ast::{
    AssignOp, BinaryOp, Designator, Expr, ExprKind, InitElement, OffsetOfPath, UnaryOp,
};
use super::parser::{ParseError, ParseResult, Parser};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol};
use crate::token::lexer::{Position, SpecialToken, TokenType, TokenValue};
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};

const DEFAULT_ARG_LIST_CAPACITY: usize = 8;
const DEFAULT_INIT_CAPACITY: usize = 8;

impl<'a> Parser<'a> {
    // ========================================================================
    // Expression Parsing - Precedence Chain
    //
    // From lowest to highest precedence:
    // 1. comma (left-to-right)
    // 2. assignment (right-to-left)
    // 3. ternary/conditional (right-to-left)
    // 4. logical-or (left-to-right)
    // 5. logical-and (left-to-right)
    // 6. bitwise-or (left-to-right)
    // 7. bitwise-xor (left-to-right)
    // 8. bitwise-and (left-to-right)
    // 9. equality (left-to-right)
    // 10. relational (left-to-right)
    // 11. shift (left-to-right)
    // 12. additive (left-to-right)
    // 13. multiplicative (left-to-right)
    // 14. unary (right-to-left)
    // 15. postfix (left-to-right)
    // 16. primary
    // ========================================================================

    /// Parse an expression (comma expression, lowest precedence)
    pub fn parse_expression(&mut self) -> ParseResult<Expr> {
        self.parse_comma_expr()
    }

    /// Parse a comma expression: expr, expr, ...
    /// Result type is the type of the rightmost expression
    fn parse_comma_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_assignment_expr()?;

        while self.is_special(b',') {
            self.advance();
            let right = self.parse_assignment_expr()?;
            // Comma expression type is the type of the rightmost expression
            let result_typ = right.typ;

            // Build comma expression
            let Expr { kind, typ, pos } = expr;
            expr = match kind {
                ExprKind::Comma(mut exprs) => {
                    exprs.push(right);
                    Expr {
                        kind: ExprKind::Comma(exprs),
                        typ: result_typ,
                        pos,
                    }
                }
                other => Expr {
                    kind: ExprKind::Comma(vec![
                        Expr {
                            kind: other,
                            typ,
                            pos,
                        },
                        right,
                    ]),
                    typ: result_typ,
                    pos,
                },
            };
        }

        Ok(expr)
    }

    /// Parse an assignment expression (right-to-left associative)
    pub(crate) fn parse_assignment_expr(&mut self) -> ParseResult<Expr> {
        // Parse left side (could be lvalue for assignment)
        let left = self.parse_conditional_expr()?;

        // Check for assignment operators
        let op = match self.peek_special() {
            Some(v) if v == b'=' as u32 => Some(AssignOp::Assign),
            Some(v) if v == SpecialToken::AddAssign as u32 => Some(AssignOp::AddAssign),
            Some(v) if v == SpecialToken::SubAssign as u32 => Some(AssignOp::SubAssign),
            Some(v) if v == SpecialToken::MulAssign as u32 => Some(AssignOp::MulAssign),
            Some(v) if v == SpecialToken::DivAssign as u32 => Some(AssignOp::DivAssign),
            Some(v) if v == SpecialToken::ModAssign as u32 => Some(AssignOp::ModAssign),
            Some(v) if v == SpecialToken::AndAssign as u32 => Some(AssignOp::AndAssign),
            Some(v) if v == SpecialToken::OrAssign as u32 => Some(AssignOp::OrAssign),
            Some(v) if v == SpecialToken::XorAssign as u32 => Some(AssignOp::XorAssign),
            Some(v) if v == SpecialToken::ShlAssign as u32 => Some(AssignOp::ShlAssign),
            Some(v) if v == SpecialToken::ShrAssign as u32 => Some(AssignOp::ShrAssign),
            _ => None,
        };

        if let Some(assign_op) = op {
            let assign_pos = self.current_pos();
            self.advance();

            // Check if target is const (assignment to const is an error)
            self.check_const_assignment(&left, assign_pos);

            // Right-to-left associativity: parse the right side as another assignment
            let right = self.parse_assignment_expr()?;
            // In C, assignment expression type is the type of the left operand
            let assign_type = left.typ.unwrap_or(self.types.int_id);
            Ok(Self::typed_expr(
                ExprKind::Assign {
                    op: assign_op,
                    target: Box::new(left),
                    value: Box::new(right),
                },
                assign_type,
                assign_pos,
            ))
        } else {
            Ok(left)
        }
    }

    /// Parse an initializer (C99 6.7.8)
    /// Can be either:
    /// - assignment-expression
    /// - { initializer-list }
    /// - { initializer-list , }
    pub(crate) fn parse_initializer(&mut self) -> ParseResult<Expr> {
        if self.is_special(b'{') {
            self.parse_initializer_list()
        } else {
            self.parse_assignment_expr()
        }
    }

    /// Parse a brace-enclosed initializer list
    fn parse_initializer_list(&mut self) -> ParseResult<Expr> {
        let list_pos = self.current_pos();
        self.expect_special(b'{')?;

        let mut elements = Vec::with_capacity(DEFAULT_INIT_CAPACITY);

        // Handle empty initializer list: {}
        if self.is_special(b'}') {
            self.advance();
            return Ok(Expr::new(ExprKind::InitList { elements }, list_pos));
        }

        loop {
            // Parse one initializer element (with optional designators)
            let element = self.parse_init_element()?;
            elements.push(element);

            // Check for comma or end
            if self.is_special(b',') {
                self.advance();
                // Trailing comma is allowed
                if self.is_special(b'}') {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect_special(b'}')?;
        Ok(Expr::new(ExprKind::InitList { elements }, list_pos))
    }

    /// Parse a single element of an initializer list
    /// Can have designators: .field = value, [index] = value, or just value
    fn parse_init_element(&mut self) -> ParseResult<InitElement> {
        let mut designators = Vec::new();

        // Parse designator chain: .field, [index], can be chained like .x[0].y
        loop {
            if self.is_special(b'.') {
                // Field designator: .fieldname
                self.advance();
                let name = self.expect_identifier()?;
                designators.push(Designator::Field(name));
            } else if self.is_special(b'[') {
                // Array index designator: [constant-expression]
                self.advance();
                let index_expr = self.parse_conditional_expr()?;
                self.expect_special(b']')?;

                // Evaluate to constant
                let index = self.eval_const_expr(&index_expr).ok_or_else(|| {
                    ParseError::new(
                        "array designator index must be constant",
                        self.current_pos(),
                    )
                })?;
                designators.push(Designator::Index(index));
            } else {
                break;
            }
        }

        // If we had designators, expect '='
        if !designators.is_empty() {
            self.expect_special(b'=')?;
        }

        // Parse the initializer value (can be nested initializer list)
        let value = self.parse_initializer()?;

        Ok(InitElement {
            designators,
            value: Box::new(value),
        })
    }

    /// Parse a conditional (ternary) expression: cond ? then : else
    pub(crate) fn parse_conditional_expr(&mut self) -> ParseResult<Expr> {
        let cond = self.parse_logical_or_expr()?;

        if self.is_special(b'?') {
            self.advance();
            let then_expr = self.parse_expression()?;
            self.expect_special(b':')?;
            // Right-to-left: parse else as another conditional
            let else_expr = self.parse_conditional_expr()?;

            // The result type is the common type of then and else branches
            // Apply array-to-pointer and function-to-pointer decay (C99 6.3.2.1)
            let then_typ = then_expr.typ.unwrap_or(self.types.int_id);
            let else_typ = else_expr.typ.unwrap_or(self.types.int_id);

            // Decay arrays to pointers
            let then_decayed = if self.types.kind(then_typ) == TypeKind::Array {
                let elem = self.types.base_type(then_typ).unwrap_or(self.types.char_id);
                self.types.pointer_to(elem)
            } else if self.types.kind(then_typ) == TypeKind::Function {
                self.types.pointer_to(then_typ)
            } else {
                then_typ
            };
            let _else_decayed = if self.types.kind(else_typ) == TypeKind::Array {
                let elem = self.types.base_type(else_typ).unwrap_or(self.types.char_id);
                self.types.pointer_to(elem)
            } else if self.types.kind(else_typ) == TypeKind::Function {
                self.types.pointer_to(else_typ)
            } else {
                else_typ
            };

            // Use decayed then type (for now; proper impl would compute common type)
            let typ = then_decayed;

            let pos = cond.pos;
            Ok(Self::typed_expr(
                ExprKind::Conditional {
                    cond: Box::new(cond),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                },
                typ,
                pos,
            ))
        } else {
            Ok(cond)
        }
    }

    /// Parse logical-or: expr || expr
    fn parse_logical_or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_logical_and_expr()?;

        while self.is_special_token(SpecialToken::LogicalOr) {
            self.advance();
            let right = self.parse_logical_and_expr()?;
            left = self.make_binary(BinaryOp::LogOr, left, right);
        }

        Ok(left)
    }

    /// Parse logical-and: expr && expr
    fn parse_logical_and_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_bitwise_or_expr()?;

        while self.is_special_token(SpecialToken::LogicalAnd) {
            self.advance();
            let right = self.parse_bitwise_or_expr()?;
            left = self.make_binary(BinaryOp::LogAnd, left, right);
        }

        Ok(left)
    }

    /// Parse bitwise-or: expr | expr
    fn parse_bitwise_or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_bitwise_xor_expr()?;

        // | but not ||
        while self.is_special(b'|') && !self.is_special_token(SpecialToken::LogicalOr) {
            self.advance();
            let right = self.parse_bitwise_xor_expr()?;
            left = self.make_binary(BinaryOp::BitOr, left, right);
        }

        Ok(left)
    }

    /// Parse bitwise-xor: expr ^ expr
    fn parse_bitwise_xor_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_bitwise_and_expr()?;

        while self.is_special(b'^') && !self.is_special_token(SpecialToken::XorAssign) {
            self.advance();
            let right = self.parse_bitwise_and_expr()?;
            left = self.make_binary(BinaryOp::BitXor, left, right);
        }

        Ok(left)
    }

    /// Parse bitwise-and: expr & expr
    fn parse_bitwise_and_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_equality_expr()?;

        // & but not &&
        while self.is_special(b'&') && !self.is_special_token(SpecialToken::LogicalAnd) {
            self.advance();
            let right = self.parse_equality_expr()?;
            left = self.make_binary(BinaryOp::BitAnd, left, right);
        }

        Ok(left)
    }

    /// Parse equality: expr == expr, expr != expr
    fn parse_equality_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_relational_expr()?;

        loop {
            let op = if self.is_special_token(SpecialToken::Equal) {
                Some(BinaryOp::Eq)
            } else if self.is_special_token(SpecialToken::NotEqual) {
                Some(BinaryOp::Ne)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_relational_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse relational: expr < expr, expr > expr, expr <= expr, expr >= expr
    fn parse_relational_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_shift_expr()?;

        loop {
            let op = if self.is_special_token(SpecialToken::Lte) {
                Some(BinaryOp::Le)
            } else if self.is_special_token(SpecialToken::Gte) {
                Some(BinaryOp::Ge)
            } else if self.is_special(b'<') && !self.is_special_token(SpecialToken::LeftShift) {
                Some(BinaryOp::Lt)
            } else if self.is_special(b'>') && !self.is_special_token(SpecialToken::RightShift) {
                Some(BinaryOp::Gt)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_shift_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse shift: expr << expr, expr >> expr
    fn parse_shift_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_additive_expr()?;

        loop {
            let op = if self.is_special_token(SpecialToken::LeftShift) {
                Some(BinaryOp::Shl)
            } else if self.is_special_token(SpecialToken::RightShift) {
                Some(BinaryOp::Shr)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_additive_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse additive: expr + expr, expr - expr
    fn parse_additive_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_multiplicative_expr()?;

        loop {
            let op = if self.is_special(b'+') && !self.is_special_token(SpecialToken::Increment) {
                Some(BinaryOp::Add)
            } else if self.is_special(b'-') && !self.is_special_token(SpecialToken::Decrement) {
                Some(BinaryOp::Sub)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_multiplicative_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse multiplicative: expr * expr, expr / expr, expr % expr
    fn parse_multiplicative_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_unary_expr()?;

        loop {
            let op = if self.is_special(b'*') && !self.is_special_token(SpecialToken::MulAssign) {
                Some(BinaryOp::Mul)
            } else if self.is_special(b'/') && !self.is_special_token(SpecialToken::DivAssign) {
                Some(BinaryOp::Div)
            } else if self.is_special(b'%') && !self.is_special_token(SpecialToken::ModAssign) {
                Some(BinaryOp::Mod)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_unary_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse unary expression: ++x, --x, &x, *x, +x, -x, ~x, !x, sizeof
    fn parse_unary_expr(&mut self) -> ParseResult<Expr> {
        // Check for prefix operators
        if self.is_special_token(SpecialToken::Increment) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Check for const modification
            self.check_const_assignment(&operand, op_pos);
            // PreInc has same type as operand
            let typ = operand.typ.unwrap_or(self.types.int_id);
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::PreInc,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special_token(SpecialToken::Decrement) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Check for const modification
            self.check_const_assignment(&operand, op_pos);
            // PreDec has same type as operand
            let typ = operand.typ.unwrap_or(self.types.int_id);
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::PreDec,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special(b'&') && !self.is_special_token(SpecialToken::LogicalAnd) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // AddrOf produces pointer to operand's type
            let base_type = operand.typ.unwrap_or(self.types.int_id);
            let ptr_type = self.types.intern(Type::pointer(base_type));
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::AddrOf,
                    operand: Box::new(operand),
                },
                ptr_type,
                op_pos,
            ));
        }

        if self.is_special(b'*') {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Deref produces the base type of the pointer
            let typ = operand
                .typ
                .and_then(|t| self.types.base_type(t))
                .unwrap_or(self.types.int_id);
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::Deref,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special(b'+') && !self.is_special_token(SpecialToken::Increment) {
            self.advance();
            // Unary + is a no-op for numeric types, but we need to parse it
            return self.parse_unary_expr();
        }

        if self.is_special(b'-') && !self.is_special_token(SpecialToken::Decrement) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Neg has same type as operand
            let typ = operand.typ.unwrap_or(self.types.int_id);
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::Neg,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special(b'~') {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // BitNot: C99 integer promotion - types smaller than int promote to int
            let op_typ = operand.typ.unwrap_or(self.types.int_id);
            // Apply integer promotion: _Bool, char, short -> int
            let typ = {
                let kind = self.types.kind(op_typ);
                if matches!(kind, TypeKind::Bool | TypeKind::Char | TypeKind::Short) {
                    self.types.int_id
                } else {
                    op_typ
                }
            };
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::BitNot,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special(b'!') {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Logical not always produces int (0 or 1)
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::Not,
                    operand: Box::new(operand),
                },
                self.types.int_id,
                op_pos,
            ));
        }

        // sizeof and _Alignof
        if self.peek() == TokenType::Ident {
            if let Some(name) = self.get_ident_name(self.current()) {
                if name == "sizeof" {
                    self.advance();
                    return self.parse_sizeof();
                }
                if name == "_Alignof"
                    || name == "__alignof__"
                    || name == "__alignof"
                    || name == "alignof"
                {
                    self.advance();
                    return self.parse_alignof();
                }
            }
        }

        // No unary operator, parse postfix
        self.parse_postfix_expr()
    }

    /// Parse sizeof expression
    fn parse_sizeof(&mut self) -> ParseResult<Expr> {
        let sizeof_pos = self.current_pos();
        // sizeof returns size_t, which is unsigned long in our implementation
        let size_t = self.types.ulong_id;

        if self.is_special(b'(') {
            // Could be sizeof(type) or sizeof(expr)
            // For now, try to detect if it's a type
            // This is a simplified check - full implementation needs type lookahead
            self.advance(); // consume '('

            // Try to parse as type first
            if let Some(typ) = self.try_parse_type_name() {
                self.expect_special(b')')?;
                return Ok(Expr::typed(ExprKind::SizeofType(typ), size_t, sizeof_pos));
            }

            // Not a type, parse as expression
            let expr = self.parse_expression()?;
            self.expect_special(b')')?;
            Ok(Expr::typed(
                ExprKind::SizeofExpr(Box::new(expr)),
                size_t,
                sizeof_pos,
            ))
        } else {
            // sizeof without parens - must be expression
            let expr = self.parse_unary_expr()?;
            Ok(Expr::typed(
                ExprKind::SizeofExpr(Box::new(expr)),
                size_t,
                sizeof_pos,
            ))
        }
    }

    /// Parse _Alignof expression (C11)
    fn parse_alignof(&mut self) -> ParseResult<Expr> {
        let alignof_pos = self.current_pos();
        // _Alignof returns size_t
        let size_t = self.types.ulong_id;

        if self.is_special(b'(') {
            // Could be _Alignof(type) or _Alignof(expr)
            self.advance(); // consume '('

            // Try to parse as type first
            if let Some(typ) = self.try_parse_type_name() {
                self.expect_special(b')')?;
                return Ok(Expr::typed(ExprKind::AlignofType(typ), size_t, alignof_pos));
            }

            // Not a type, parse as expression
            let expr = self.parse_expression()?;
            self.expect_special(b')')?;
            Ok(Expr::typed(
                ExprKind::AlignofExpr(Box::new(expr)),
                size_t,
                alignof_pos,
            ))
        } else {
            // _Alignof without parens - must be expression
            let expr = self.parse_unary_expr()?;
            Ok(Expr::typed(
                ExprKind::AlignofExpr(Box::new(expr)),
                size_t,
                alignof_pos,
            ))
        }
    }

    /// Check if identifier is a type-starting keyword (for cast/sizeof disambiguation)
    pub(crate) fn is_type_keyword(name: &str) -> bool {
        matches!(
            name,
            "void"
                | "_Bool"
                | "_Complex"
                | "_Atomic"
                | "char"
                | "short"
                | "int"
                | "long"
                | "float"
                | "double"
                | "_Float16"
                | "_Float32"
                | "_Float64"
                | "signed"
                | "unsigned"
                | "const"
                | "volatile"
                | "struct"
                | "union"
                | "enum"
                | "__builtin_va_list"
                | "typeof"
                | "__typeof__"
                | "__typeof"
        )
    }

    /// Consume type qualifiers (const, volatile, restrict)
    /// Used for qualifiers after '*' in pointers or after struct/union/enum types
    /// Returns the modifiers that were consumed
    pub(crate) fn consume_type_qualifiers(&mut self) -> TypeModifiers {
        let mut mods = TypeModifiers::empty();
        while self.peek() == TokenType::Ident {
            let name_id = match self.get_ident_id(self.current()) {
                Some(id) => id,
                None => break,
            };
            let name = self.str(name_id);
            match name {
                "const" | "__const" | "__const__" => {
                    self.advance();
                    mods |= TypeModifiers::CONST;
                }
                "volatile" | "__volatile" | "__volatile__" => {
                    self.advance();
                    mods |= TypeModifiers::VOLATILE;
                }
                "restrict" | "__restrict" | "__restrict__" => {
                    self.advance();
                    mods |= TypeModifiers::RESTRICT;
                }
                "_Atomic" => {
                    self.advance();
                    mods |= TypeModifiers::ATOMIC;
                }
                _ => break,
            }
        }
        mods
    }

    /// Parse a chain of pointer declarators with optional qualifiers
    /// e.g., `* const * volatile *` returns the final pointer type
    fn parse_pointer_chain(&mut self, mut base_type: TypeId) -> TypeId {
        while self.is_special(b'*') {
            self.advance();
            let ptr_mods = self.consume_type_qualifiers();
            let mut ptr_type = Type::pointer(base_type);
            ptr_type.modifiers |= ptr_mods;
            base_type = self.types.intern(ptr_type);
        }
        base_type
    }

    /// Apply trailing qualifiers to a type and return the qualified type id
    /// Used for patterns like "struct foo const *" where const comes after the struct
    fn apply_trailing_qualifiers(&mut self, base_type: TypeId) -> TypeId {
        let trailing_mods = self.consume_type_qualifiers();
        if trailing_mods.is_empty() {
            base_type
        } else {
            let mut qualified_type = self.types.get(base_type).clone();
            qualified_type.modifiers |= trailing_mods;
            self.types.intern(qualified_type)
        }
    }

    /// Parse a type name (required, returns error if not a type)
    fn parse_type_name(&mut self) -> ParseResult<TypeId> {
        self.try_parse_type_name()
            .ok_or_else(|| ParseError::new("expected type name".to_string(), self.current_pos()))
    }

    /// Try to parse a type name for casts and sizeof
    /// Supports compound types like `unsigned char`, `long long`, pointers, etc.
    pub(crate) fn try_parse_type_name(&mut self) -> Option<TypeId> {
        if self.peek() != TokenType::Ident {
            return None;
        }

        // Check if this looks like a type name (keyword or typedef)
        let name_id = self.get_ident_id(self.current())?;
        let name = self.str(name_id);
        if !Self::is_type_keyword(name) && self.symbols.lookup_typedef(name_id).is_none() {
            // Not a type keyword and not a typedef
            return None;
        }

        // Parse type specifiers (similar to parse_type_specifier)
        let mut modifiers = TypeModifiers::empty();
        let mut base_kind: Option<TypeKind> = None;
        let mut parsed_something = false;
        // Track typedef type separately - we continue parsing after a typedef
        // to collect trailing qualifiers like "z_word_t const"
        let mut typedef_base: Option<TypeId> = None;

        loop {
            if self.peek() != TokenType::Ident {
                break;
            }

            let name_id = match self.get_ident_id(self.current()) {
                Some(id) => id,
                None => break,
            };
            let name = self.str(name_id);

            match name {
                "const" => {
                    self.advance();
                    modifiers |= TypeModifiers::CONST;
                    parsed_something = true;
                }
                "volatile" => {
                    self.advance();
                    modifiers |= TypeModifiers::VOLATILE;
                    parsed_something = true;
                }
                "signed" => {
                    self.advance();
                    modifiers |= TypeModifiers::SIGNED;
                    parsed_something = true;
                }
                "unsigned" => {
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                    parsed_something = true;
                }
                "_Complex" => {
                    self.advance();
                    modifiers |= TypeModifiers::COMPLEX;
                    parsed_something = true;
                }
                "_Atomic" => {
                    self.advance();
                    // _Atomic can be:
                    // 1. Type specifier: _Atomic(type-name)
                    // 2. Type qualifier: _Atomic (without parens)
                    if self.is_special(b'(') {
                        // Type specifier form: _Atomic(type-name)
                        self.advance(); // consume '('
                        if let Some(inner_type) = self.try_parse_type_name() {
                            if !self.is_special(b')') {
                                return None;
                            }
                            self.advance(); // consume ')'
                            let inner = self.types.get(inner_type).clone();
                            let result = Type {
                                modifiers: modifiers | inner.modifiers | TypeModifiers::ATOMIC,
                                ..inner
                            };
                            let result_id = self.types.intern(result);
                            return Some(self.parse_pointer_chain(result_id));
                        } else {
                            return None;
                        }
                    } else {
                        // Qualifier form: just _Atomic
                        modifiers |= TypeModifiers::ATOMIC;
                    }
                    parsed_something = true;
                }
                "short" => {
                    self.advance();
                    modifiers |= TypeModifiers::SHORT;
                    if base_kind.is_none() {
                        base_kind = Some(TypeKind::Short);
                    }
                    parsed_something = true;
                }
                "long" => {
                    self.advance();
                    if modifiers.contains(TypeModifiers::LONG) {
                        modifiers |= TypeModifiers::LONGLONG;
                        base_kind = Some(TypeKind::LongLong);
                    } else {
                        modifiers |= TypeModifiers::LONG;
                        // long double case
                        if base_kind == Some(TypeKind::Double) {
                            base_kind = Some(TypeKind::LongDouble);
                        } else if base_kind.is_none() {
                            base_kind = Some(TypeKind::Long);
                        }
                    }
                    parsed_something = true;
                }
                "void" => {
                    self.advance();
                    base_kind = Some(TypeKind::Void);
                    parsed_something = true;
                }
                "char" => {
                    self.advance();
                    base_kind = Some(TypeKind::Char);
                    parsed_something = true;
                }
                "int" => {
                    self.advance();
                    if base_kind.is_none()
                        || !matches!(
                            base_kind,
                            Some(TypeKind::Short) | Some(TypeKind::Long) | Some(TypeKind::LongLong)
                        )
                    {
                        base_kind = Some(TypeKind::Int);
                    }
                    parsed_something = true;
                }
                "float" => {
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                    parsed_something = true;
                }
                "double" => {
                    self.advance();
                    // Handle long double
                    if modifiers.contains(TypeModifiers::LONG) {
                        base_kind = Some(TypeKind::LongDouble);
                    } else {
                        base_kind = Some(TypeKind::Double);
                    }
                    parsed_something = true;
                }
                "_Float16" => {
                    self.advance();
                    base_kind = Some(TypeKind::Float16);
                    parsed_something = true;
                }
                "_Float32" => {
                    // _Float32 is an alias for float (TS 18661-3 / C23)
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                    parsed_something = true;
                }
                "_Float64" => {
                    // _Float64 is an alias for double (TS 18661-3 / C23)
                    self.advance();
                    base_kind = Some(TypeKind::Double);
                    parsed_something = true;
                }
                "_Bool" => {
                    self.advance();
                    base_kind = Some(TypeKind::Bool);
                    parsed_something = true;
                }
                "__builtin_va_list" => {
                    self.advance();
                    base_kind = Some(TypeKind::VaList);
                    parsed_something = true;
                }
                "typeof" | "__typeof__" | "__typeof" => {
                    self.advance(); // consume typeof
                    if !self.is_special(b'(') {
                        return None;
                    }
                    self.advance(); // consume '('

                    // typeof can take either a type name or an expression
                    // Try type name first
                    if let Some(typ) = self.try_parse_type_name() {
                        if !self.is_special(b')') {
                            return None;
                        }
                        self.advance(); // consume ')'
                        return Some(self.parse_pointer_chain(typ));
                    }

                    // Not a type name, try expression
                    let expr = match self.parse_expression() {
                        Ok(e) => e,
                        Err(_) => return None,
                    };
                    if !self.is_special(b')') {
                        return None;
                    }
                    self.advance(); // consume ')'

                    let expr_type = expr.typ.unwrap_or(self.types.int_id);
                    return Some(self.parse_pointer_chain(expr_type));
                }
                "struct" => {
                    self.advance(); // consume 'struct'
                                    // For struct tag reference, look up directly in symbol table
                    if let Some(tag_name) = self.get_ident_id(self.current()) {
                        if !self.is_special(b'{') {
                            // This is a tag reference (e.g., "struct Point*")
                            self.advance(); // consume tag name
                            if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                                let result_id = self.apply_trailing_qualifiers(existing.typ);
                                let mut result_id = self.parse_pointer_chain(result_id);
                                // Handle array declarators
                                while self.is_special(b'[') {
                                    self.advance();
                                    if let Ok(size_expr) = self.parse_conditional_expr() {
                                        if let Some(size) = self.eval_const_expr(&size_expr) {
                                            result_id = self
                                                .types
                                                .intern(Type::array(result_id, size as usize));
                                        }
                                    }
                                    if !self.is_special(b']') {
                                        return None;
                                    }
                                    self.advance();
                                }
                                return Some(result_id);
                            }
                            // Tag not found - create incomplete struct type and register it
                            // This ensures that when the struct is later defined, we can update
                            // this same TypeId rather than creating a new one
                            let mut incomplete = Type::incomplete_struct(tag_name);
                            incomplete.modifiers |= self.consume_type_qualifiers();
                            let result_id = self.types.intern(incomplete);
                            let sym = Symbol::tag(tag_name, result_id, self.symbols.depth());
                            let _ = self.symbols.declare(sym);
                            return Some(self.parse_pointer_chain(result_id));
                        }
                    }
                    // Fall back to full struct parsing for definitions
                    self.pos -= 1;
                    if let Ok(struct_type) = self.parse_struct_or_union_specifier(false) {
                        let mut typ = struct_type;
                        typ.modifiers |= modifiers | self.consume_type_qualifiers();
                        let mut result_id = self.types.intern(typ);
                        result_id = self.parse_pointer_chain(result_id);
                        // Handle array declarators
                        while self.is_special(b'[') {
                            self.advance();
                            if let Ok(size_expr) = self.parse_conditional_expr() {
                                if let Some(size) = self.eval_const_expr(&size_expr) {
                                    result_id =
                                        self.types.intern(Type::array(result_id, size as usize));
                                }
                            }
                            if !self.is_special(b']') {
                                return None;
                            }
                            self.advance();
                        }
                        return Some(result_id);
                    }
                    return None;
                }
                "union" => {
                    self.advance(); // consume 'union'
                                    // For union tag reference, look up directly in symbol table
                    if let Some(tag_name) = self.get_ident_id(self.current()) {
                        if !self.is_special(b'{') {
                            // This is a tag reference
                            self.advance(); // consume tag name
                            if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                                let result_id = self.apply_trailing_qualifiers(existing.typ);
                                let mut result_id = self.parse_pointer_chain(result_id);
                                // Handle array declarators
                                while self.is_special(b'[') {
                                    self.advance();
                                    if let Ok(size_expr) = self.parse_conditional_expr() {
                                        if let Some(size) = self.eval_const_expr(&size_expr) {
                                            result_id = self
                                                .types
                                                .intern(Type::array(result_id, size as usize));
                                        }
                                    }
                                    if !self.is_special(b']') {
                                        return None;
                                    }
                                    self.advance();
                                }
                                return Some(result_id);
                            }
                            // Tag not found - create incomplete union type and register it
                            // This ensures that when the union is later defined, we can update
                            // this same TypeId rather than creating a new one
                            let mut incomplete = Type::incomplete_union(tag_name);
                            incomplete.modifiers |= self.consume_type_qualifiers();
                            let result_id = self.types.intern(incomplete);
                            let sym = Symbol::tag(tag_name, result_id, self.symbols.depth());
                            let _ = self.symbols.declare(sym);
                            return Some(self.parse_pointer_chain(result_id));
                        }
                    }
                    // Fall back to full union parsing for definitions
                    self.pos -= 1;
                    if let Ok(union_type) = self.parse_struct_or_union_specifier(true) {
                        let mut typ = union_type;
                        typ.modifiers |= modifiers | self.consume_type_qualifiers();
                        let mut result_id = self.types.intern(typ);
                        result_id = self.parse_pointer_chain(result_id);
                        // Handle array declarators
                        while self.is_special(b'[') {
                            self.advance();
                            if let Ok(size_expr) = self.parse_conditional_expr() {
                                if let Some(size) = self.eval_const_expr(&size_expr) {
                                    result_id =
                                        self.types.intern(Type::array(result_id, size as usize));
                                }
                            }
                            if !self.is_special(b']') {
                                return None;
                            }
                            self.advance();
                        }
                        return Some(result_id);
                    }
                    return None;
                }
                "enum" => {
                    if let Ok(enum_type) = self.parse_enum_specifier() {
                        let mut typ = enum_type;
                        typ.modifiers |= modifiers | self.consume_type_qualifiers();
                        let mut result_id = self.types.intern(typ);
                        result_id = self.parse_pointer_chain(result_id);
                        // Handle array declarators
                        while self.is_special(b'[') {
                            self.advance();
                            if let Ok(size_expr) = self.parse_conditional_expr() {
                                if let Some(size) = self.eval_const_expr(&size_expr) {
                                    result_id =
                                        self.types.intern(Type::array(result_id, size as usize));
                                }
                            }
                            if !self.is_special(b']') {
                                return None;
                            }
                            self.advance();
                        }
                        return Some(result_id);
                    }
                    return None;
                }
                _ => {
                    // Check if it's a typedef name
                    // Only consume if we haven't already seen a base type or typedef
                    if base_kind.is_none() && typedef_base.is_none() {
                        if let Some(typedef_type_id) = self.symbols.lookup_typedef(name_id) {
                            self.advance();
                            // Save the typedef type and continue looping to collect trailing
                            // qualifiers (e.g., "z_word_t const" where const comes after typedef)
                            typedef_base = Some(typedef_type_id);
                            parsed_something = true;
                            continue;
                        }
                    }
                    break;
                }
            }
        }

        if !parsed_something {
            return None;
        }

        // Get the base type - either from typedef or from built-in type specifiers
        let mut result_id = if let Some(typedef_type_id) = typedef_base {
            // Apply trailing modifiers to the typedef type
            if !modifiers.is_empty() {
                let typedef_type = self.types.get(typedef_type_id);
                let mut result = typedef_type.clone();
                result.modifiers &= !TypeModifiers::TYPEDEF;
                result.modifiers |= modifiers;
                self.types.intern(result)
            } else {
                typedef_type_id
            }
        } else {
            // If we only have modifiers like `unsigned` without a base type, default to int
            let kind = base_kind.unwrap_or(TypeKind::Int);
            let typ = Type::with_modifiers(kind, modifiers);
            self.types.intern(typ)
        };

        // Handle pointer declarators
        result_id = self.parse_pointer_chain(result_id);

        // Handle function pointer declarators: void (*)(int), int (*)(void), etc.
        // Syntax: return_type (*)(param_types)
        if self.is_special(b'(') {
            // Look ahead to check for (*) pattern indicating function pointer
            let saved_pos = self.pos;
            self.advance(); // consume '('
            if self.is_special(b'*') {
                self.advance(); // consume '*'
                if self.is_special(b')') {
                    self.advance(); // consume ')'
                                    // Now expect parameter list
                    if self.is_special(b'(') {
                        self.advance(); // consume '('
                                        // Parse parameter types properly
                        if let Ok((raw_params, variadic)) = self.parse_parameter_list() {
                            if !self.is_special(b')') {
                                return None;
                            }
                            self.advance(); // consume final ')'
                                            // Create function pointer type with actual parameter types
                            let param_type_ids: Vec<TypeId> =
                                raw_params.iter().map(|(_, typ)| *typ).collect();
                            let fn_type =
                                Type::function(result_id, param_type_ids, variadic, false);
                            let fn_type_id = self.types.intern(fn_type);
                            result_id = self.types.intern(Type::pointer(fn_type_id));
                            return Some(result_id);
                        }
                    }
                }
            }
            // Not a function pointer, backtrack
            self.pos = saved_pos;
        }

        // Handle array declarators: int[10], char[20], int[], etc.
        while self.is_special(b'[') {
            self.advance();
            // Check for empty brackets [] (incomplete array type for compound literals)
            if self.is_special(b']') {
                // Empty brackets - create array with size 0 (size to be determined from initializer)
                result_id = self.types.intern(Type::array(result_id, 0));
            } else if let Ok(size_expr) = self.parse_conditional_expr() {
                if let Some(size) = self.eval_const_expr(&size_expr) {
                    result_id = self.types.intern(Type::array(result_id, size as usize));
                } else {
                    // Non-constant size (VLA in type name) - create array with size 0
                    result_id = self.types.intern(Type::array(result_id, 0));
                }
            }
            if !self.is_special(b']') {
                return None;
            }
            self.advance();
        }

        Some(result_id)
    }

    /// Parse postfix expression: x++, x--, x[i], x.member, x->member, x(args)
    fn parse_postfix_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary_expr()?;

        loop {
            // Preserve the position of the base expression for all postfix ops
            let base_pos = expr.pos;

            if self.is_special_token(SpecialToken::Increment) {
                let op_pos = self.current_pos();
                self.advance();
                // Check for const modification
                self.check_const_assignment(&expr, op_pos);
                // PostInc has same type as operand
                let typ = expr.typ.unwrap_or(self.types.int_id);
                expr = Self::typed_expr(ExprKind::PostInc(Box::new(expr)), typ, base_pos);
            } else if self.is_special_token(SpecialToken::Decrement) {
                let op_pos = self.current_pos();
                self.advance();
                // Check for const modification
                self.check_const_assignment(&expr, op_pos);
                // PostDec has same type as operand
                let typ = expr.typ.unwrap_or(self.types.int_id);
                expr = Self::typed_expr(ExprKind::PostDec(Box::new(expr)), typ, base_pos);
            } else if self.is_special(b'[') {
                // Array subscript
                self.advance();
                let index = self.parse_expression()?;
                self.expect_special(b']')?;
                // Get element type from array/pointer type
                let elem_type = expr
                    .typ
                    .and_then(|t| self.types.base_type(t))
                    .unwrap_or(self.types.int_id);
                expr = Self::typed_expr(
                    ExprKind::Index {
                        array: Box::new(expr),
                        index: Box::new(index),
                    },
                    elem_type,
                    base_pos,
                );
            } else if self.is_special(b'.') {
                // Member access
                let dot_pos = self.current_pos();
                self.advance();
                let member = self.expect_identifier()?;
                // Get member type from struct type, resolving incomplete types first
                let member_type = if let Some(t) = expr.typ {
                    let resolved = self.resolve_struct_type(t);
                    let kind = self.types.kind(resolved);
                    if kind != TypeKind::Struct && kind != TypeKind::Union {
                        diag::error(
                            dot_pos,
                            "request for member in something not a structure or union",
                        );
                        self.types.int_id
                    } else if let Some(info) = self.types.find_member(resolved, member) {
                        info.typ
                    } else {
                        let member_name = self.idents.get_opt(member).unwrap_or("<unknown>");
                        diag::error(dot_pos, &format!("has no member named '{}'", member_name));
                        self.types.int_id
                    }
                } else {
                    self.types.int_id
                };
                expr = Self::typed_expr(
                    ExprKind::Member {
                        expr: Box::new(expr),
                        member,
                    },
                    member_type,
                    base_pos,
                );
            } else if self.is_special_token(SpecialToken::Arrow) {
                // Pointer member access
                let arrow_pos = self.current_pos();
                self.advance();
                let member = self.expect_identifier()?;
                // Get member type: dereference pointer to get struct, resolve if incomplete, then find member
                let member_type = if let Some(t) = expr.typ {
                    if let Some(struct_type) = self.types.base_type(t) {
                        let resolved = self.resolve_struct_type(struct_type);
                        let kind = self.types.kind(resolved);
                        if kind != TypeKind::Struct && kind != TypeKind::Union {
                            diag::error(
                                arrow_pos,
                                "request for member in something not a structure or union",
                            );
                            self.types.int_id
                        } else if let Some(info) = self.types.find_member(resolved, member) {
                            info.typ
                        } else {
                            let member_name = self.idents.get_opt(member).unwrap_or("<unknown>");
                            diag::error(
                                arrow_pos,
                                &format!("has no member named '{}'", member_name),
                            );
                            self.types.int_id
                        }
                    } else {
                        self.types.int_id
                    }
                } else {
                    self.types.int_id
                };
                expr = Self::typed_expr(
                    ExprKind::Arrow {
                        expr: Box::new(expr),
                        member,
                    },
                    member_type,
                    base_pos,
                );
            } else if self.is_special(b'(') {
                // Function call
                self.advance();
                let args = self.parse_argument_list()?;
                self.expect_special(b')')?;

                // Get the return type from the function type
                // The func expression should have type TypeKind::Function
                // and the return type is stored in base.
                // For function pointers (TypeKind::Pointer to Function),
                // we need to dereference first to get the function type.
                let return_type = expr
                    .typ
                    .and_then(|t| {
                        let kind = self.types.kind(t);
                        if kind == TypeKind::Function {
                            // Direct function call
                            self.types.base_type(t)
                        } else if kind == TypeKind::Pointer {
                            // Function pointer call - get the pointee (function type)
                            self.types.base_type(t).and_then(|func_type| {
                                if self.types.kind(func_type) == TypeKind::Function {
                                    // Get return type from function type
                                    self.types.base_type(func_type)
                                } else {
                                    None
                                }
                            })
                        } else {
                            None
                        }
                    })
                    .unwrap_or(self.types.int_id); // Default to int

                expr = Self::typed_expr(
                    ExprKind::Call {
                        func: Box::new(expr),
                        args,
                    },
                    return_type,
                    base_pos,
                );
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse function argument list
    fn parse_argument_list(&mut self) -> ParseResult<Vec<Expr>> {
        let mut args = Vec::with_capacity(DEFAULT_ARG_LIST_CAPACITY);

        if self.is_special(b')') {
            return Ok(args);
        }

        loop {
            // Parse assignment expression (not comma, as comma separates args)
            args.push(self.parse_assignment_expr()?);

            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

        Ok(args)
    }

    /// Expect and consume an identifier, returning its StringId
    pub(crate) fn expect_identifier(&mut self) -> ParseResult<StringId> {
        if self.peek() != TokenType::Ident {
            return Err(ParseError::new("expected identifier", self.current_pos()));
        }

        let id = self
            .get_ident_id(self.current())
            .ok_or_else(|| ParseError::new("invalid identifier", self.current_pos()))?;

        self.advance();
        Ok(id)
    }

    /// Check if an expression is const and report error if assigning to it
    fn check_const_assignment(&self, target: &Expr, pos: Position) {
        // Check for assignment through pointer to const first: *p where p is const T*
        if let ExprKind::Unary {
            op: UnaryOp::Deref,
            operand,
        } = &target.kind
        {
            if let Some(ptr_type_id) = operand.typ {
                if let Some(base_type_id) = self.types.base_type(ptr_type_id) {
                    if self
                        .types
                        .modifiers(base_type_id)
                        .contains(TypeModifiers::CONST)
                    {
                        diag::error(pos, "assignment of read-only location");
                        return; // Don't duplicate with the general const check
                    }
                }
            }
        }

        // Check if target type has CONST modifier (direct const variable)
        if let Some(typ_id) = target.typ {
            if self.types.modifiers(typ_id).contains(TypeModifiers::CONST) {
                // Get variable name if it's an identifier
                let var_name = match &target.kind {
                    ExprKind::Ident(symbol_id) => {
                        let name = self.symbols.get(*symbol_id).name;
                        format!(" '{}'", self.str(name))
                    }
                    _ => String::new(),
                };
                diag::error(
                    pos,
                    &format!("assignment of read-only variable{}", var_name),
                );
            }
        }
    }

    /// Parse primary expression: literals, identifiers, parenthesized expressions
    /// Create a typed expression with position
    pub(crate) fn typed_expr(kind: ExprKind, typ: TypeId, pos: Position) -> Expr {
        Expr {
            kind,
            typ: Some(typ),
            pos,
        }
    }

    /// Create a typed binary expression, computing result type from operands
    fn make_binary(&mut self, op: BinaryOp, left: Expr, right: Expr) -> Expr {
        // Compute result type based on operator and operand types
        let left_type = left.typ.unwrap_or(self.types.int_id);
        let right_type = right.typ.unwrap_or(self.types.int_id);

        let result_type = match op {
            // Comparison and logical operators always return int
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Ge
            | BinaryOp::LogAnd
            | BinaryOp::LogOr => self.types.int_id,

            // Arithmetic operators use usual arithmetic conversions
            // But Add/Sub with pointers/arrays need special handling
            BinaryOp::Add | BinaryOp::Sub => {
                let left_kind = self.types.kind(left_type);
                let right_kind = self.types.kind(right_type);
                let left_is_ptr_or_arr =
                    left_kind == TypeKind::Pointer || left_kind == TypeKind::Array;
                let right_is_ptr_or_arr =
                    right_kind == TypeKind::Pointer || right_kind == TypeKind::Array;

                if left_is_ptr_or_arr && self.types.is_integer(right_type) {
                    // ptr + int or arr + int -> pointer to element type
                    if left_kind == TypeKind::Array {
                        // Array decays to pointer
                        let elem_type =
                            self.types.base_type(left_type).unwrap_or(self.types.int_id);
                        self.types.intern(Type::pointer(elem_type))
                    } else {
                        left_type
                    }
                } else if self.types.is_integer(left_type)
                    && right_is_ptr_or_arr
                    && op == BinaryOp::Add
                {
                    // int + ptr or int + arr -> pointer to element type
                    if right_kind == TypeKind::Array {
                        let elem_type = self
                            .types
                            .base_type(right_type)
                            .unwrap_or(self.types.int_id);
                        self.types.intern(Type::pointer(elem_type))
                    } else {
                        right_type
                    }
                } else if left_is_ptr_or_arr && right_is_ptr_or_arr && op == BinaryOp::Sub {
                    // ptr - ptr -> ptrdiff_t (long)
                    self.types.long_id
                } else {
                    self.usual_arithmetic_conversions(left_type, right_type)
                }
            }
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                self.usual_arithmetic_conversions(left_type, right_type)
            }

            // Bitwise and shift operators use integer promotions
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Shl
            | BinaryOp::Shr => self.usual_arithmetic_conversions(left_type, right_type),
        };

        let pos = left.pos;
        Self::typed_expr(
            ExprKind::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
            result_type,
            pos,
        )
    }

    /// Compute usual arithmetic conversions (C99 6.3.1.8)
    fn usual_arithmetic_conversions(&mut self, left: TypeId, right: TypeId) -> TypeId {
        // C99 6.3.1.8: Usual arithmetic conversions
        // For complex types: if either operand is complex, result is complex
        // The underlying type follows the same rules as real types

        let left_kind = self.types.kind(left);
        let right_kind = self.types.kind(right);
        let left_complex = self.types.is_complex(left);
        let right_complex = self.types.is_complex(right);
        let is_complex = left_complex || right_complex;

        // Determine the underlying floating-point type
        // 1. If either is long double, result is long double
        // 2. If either is double, result is double
        // 3. If either is float, result is float
        // 4. Otherwise, integer promotions apply

        if left_kind == TypeKind::LongDouble || right_kind == TypeKind::LongDouble {
            if is_complex {
                self.types.complex_longdouble_id
            } else {
                self.types.longdouble_id
            }
        } else if left_kind == TypeKind::Double || right_kind == TypeKind::Double {
            if is_complex {
                self.types.complex_double_id
            } else {
                self.types.double_id
            }
        } else if left_kind == TypeKind::Float || right_kind == TypeKind::Float {
            if is_complex {
                self.types.complex_float_id
            } else {
                self.types.float_id
            }
        } else if left_kind == TypeKind::Float16 || right_kind == TypeKind::Float16 {
            // C23 _Float16: stays as _Float16 for arithmetic
            if is_complex {
                self.types.complex_float16_id
            } else {
                self.types.float16_id
            }
        } else if left_kind == TypeKind::LongLong || right_kind == TypeKind::LongLong {
            // If either is unsigned long long, result is unsigned long long
            if self.types.is_unsigned(left) || self.types.is_unsigned(right) {
                self.types.ulonglong_id
            } else {
                self.types.longlong_id
            }
        } else if left_kind == TypeKind::Long || right_kind == TypeKind::Long {
            // If either is unsigned long, result is unsigned long
            if self.types.is_unsigned(left) || self.types.is_unsigned(right) {
                self.types.ulong_id
            } else {
                self.types.long_id
            }
        } else if self.types.is_unsigned(left) || self.types.is_unsigned(right) {
            self.types.uint_id
        } else {
            self.types.int_id
        }
    }

    fn parse_primary_expr(&mut self) -> ParseResult<Expr> {
        match self.peek() {
            TokenType::Number => {
                let token = self.consume();
                if let TokenValue::Number(s) = &token.value {
                    // Parse the number literal (returns typed expression)
                    self.parse_number_literal(s, token.pos)
                } else {
                    Err(ParseError::new("invalid number token", token.pos))
                }
            }

            TokenType::Ident => {
                let token = self.consume();
                let token_pos = token.pos;
                if let TokenValue::Ident(id) = &token.value {
                    let name_id = *id;
                    let name_str = self.idents.get_opt(name_id).unwrap_or("");

                    // Check for varargs builtins that need special parsing
                    match name_str {
                        "__builtin_va_start" => {
                            // __builtin_va_start(ap, last_param)
                            self.expect_special(b'(')?;
                            let ap = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            // Second arg is a parameter name
                            let last_param = self.expect_identifier()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::VaStart {
                                    ap: Box::new(ap),
                                    last_param,
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_va_arg" => {
                            // __builtin_va_arg(ap, type)
                            self.expect_special(b'(')?;
                            let ap = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            // Second arg is a type
                            let arg_type = self.parse_type_name()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::VaArg {
                                    ap: Box::new(ap),
                                    arg_type,
                                },
                                arg_type,
                                token_pos,
                            ));
                        }
                        "__builtin_va_end" => {
                            // __builtin_va_end(ap)
                            self.expect_special(b'(')?;
                            let ap = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::VaEnd { ap: Box::new(ap) },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_va_copy" => {
                            // __builtin_va_copy(dest, src)
                            self.expect_special(b'(')?;
                            let dest = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let src = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::VaCopy {
                                    dest: Box::new(dest),
                                    src: Box::new(src),
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_bswap16" => {
                            // __builtin_bswap16(x) - returns uint16_t
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Bswap16 { arg: Box::new(arg) },
                                self.types.ushort_id,
                                token_pos,
                            ));
                        }
                        "__builtin_bswap32" => {
                            // __builtin_bswap32(x) - returns uint32_t
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Bswap32 { arg: Box::new(arg) },
                                self.types.uint_id,
                                token_pos,
                            ));
                        }
                        "__builtin_bswap64" => {
                            // __builtin_bswap64(x) - returns uint64_t
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Bswap64 { arg: Box::new(arg) },
                                self.types.ulonglong_id,
                                token_pos,
                            ));
                        }
                        "__builtin_ctz" => {
                            // __builtin_ctz(x) - returns int, counts trailing zeros in unsigned int
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Ctz { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_ctzl" => {
                            // __builtin_ctzl(x) - returns int, counts trailing zeros in unsigned long
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Ctzl { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_ctzll" => {
                            // __builtin_ctzll(x) - returns int, counts trailing zeros in unsigned long long
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Ctzll { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_clz" => {
                            // __builtin_clz(x) - returns int, counts leading zeros in unsigned int
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Clz { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_clzl" => {
                            // __builtin_clzl(x) - returns int, counts leading zeros in unsigned long
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Clzl { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_clzll" => {
                            // __builtin_clzll(x) - returns int, counts leading zeros in unsigned long long
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Clzll { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_popcount" => {
                            // __builtin_popcount(x) - returns int, counts set bits in unsigned int
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Popcount { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_popcountl" => {
                            // __builtin_popcountl(x) - returns int, counts set bits in unsigned long
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Popcountl { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_popcountll" => {
                            // __builtin_popcountll(x) - returns int, counts set bits in unsigned long long
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Popcountll { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_alloca" => {
                            // __builtin_alloca(size) - returns void*
                            self.expect_special(b'(')?;
                            let size = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Alloca {
                                    size: Box::new(size),
                                },
                                self.types.void_ptr_id,
                                token_pos,
                            ));
                        }
                        // Memory builtins - generate calls to C library functions
                        "__builtin_memset" => {
                            // __builtin_memset(dest, c, n) - returns void*
                            self.expect_special(b'(')?;
                            let dest = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let c = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let n = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Memset {
                                    dest: Box::new(dest),
                                    c: Box::new(c),
                                    n: Box::new(n),
                                },
                                self.types.void_ptr_id,
                                token_pos,
                            ));
                        }
                        "__builtin_memcpy" => {
                            // __builtin_memcpy(dest, src, n) - returns void*
                            self.expect_special(b'(')?;
                            let dest = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let src = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let n = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Memcpy {
                                    dest: Box::new(dest),
                                    src: Box::new(src),
                                    n: Box::new(n),
                                },
                                self.types.void_ptr_id,
                                token_pos,
                            ));
                        }
                        "__builtin_memmove" => {
                            // __builtin_memmove(dest, src, n) - returns void*
                            self.expect_special(b'(')?;
                            let dest = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let src = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let n = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Memmove {
                                    dest: Box::new(dest),
                                    src: Box::new(src),
                                    n: Box::new(n),
                                },
                                self.types.void_ptr_id,
                                token_pos,
                            ));
                        }
                        // Infinity builtins - return float constants
                        "__builtin_inf" | "__builtin_huge_val" => {
                            self.expect_special(b'(')?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::FloatLit(f64::INFINITY),
                                self.types.double_id,
                                token_pos,
                            ));
                        }
                        "__builtin_inff" | "__builtin_huge_valf" => {
                            self.expect_special(b'(')?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::FloatLit(f64::INFINITY),
                                self.types.float_id,
                                token_pos,
                            ));
                        }
                        "__builtin_infl" | "__builtin_huge_vall" => {
                            self.expect_special(b'(')?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::FloatLit(f64::INFINITY),
                                self.types.longdouble_id,
                                token_pos,
                            ));
                        }
                        // NaN builtins - returns quiet NaN
                        // The string argument is typically empty "" for quiet NaN
                        "__builtin_nan" | "__builtin_nans" => {
                            self.expect_special(b'(')?;
                            let _arg = self.parse_assignment_expr()?; // string argument (ignored)
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::FloatLit(f64::NAN),
                                self.types.double_id,
                                token_pos,
                            ));
                        }
                        "__builtin_nanf" | "__builtin_nansf" => {
                            self.expect_special(b'(')?;
                            let _arg = self.parse_assignment_expr()?; // string argument (ignored)
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::FloatLit(f64::NAN),
                                self.types.float_id,
                                token_pos,
                            ));
                        }
                        "__builtin_nanl" | "__builtin_nansl" => {
                            self.expect_special(b'(')?;
                            let _arg = self.parse_assignment_expr()?; // string argument (ignored)
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::FloatLit(f64::NAN),
                                self.types.longdouble_id,
                                token_pos,
                            ));
                        }
                        // FLT_ROUNDS - returns current rounding mode (1 = to nearest)
                        "__builtin_flt_rounds" => {
                            self.expect_special(b'(')?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::IntLit(1), // IEEE 754 default: round to nearest
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        // Fabs builtins - absolute value for floats
                        "__builtin_fabs" => {
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Fabs { arg: Box::new(arg) },
                                self.types.double_id,
                                token_pos,
                            ));
                        }
                        "__builtin_fabsf" => {
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Fabsf { arg: Box::new(arg) },
                                self.types.float_id,
                                token_pos,
                            ));
                        }
                        "__builtin_fabsl" => {
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Fabsl { arg: Box::new(arg) },
                                self.types.longdouble_id,
                                token_pos,
                            ));
                        }
                        // Signbit builtins - test sign bit of floats
                        "__builtin_signbit" => {
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Signbit { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_signbitf" => {
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Signbitf { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_signbitl" => {
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Signbitl { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_unreachable" => {
                            // __builtin_unreachable() - marks code as unreachable
                            // Takes no arguments, returns void
                            // Behavior is undefined if actually reached at runtime
                            self.expect_special(b'(')?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Unreachable,
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_constant_p" => {
                            // __builtin_constant_p(expr) - returns 1 if expr is a constant, 0 otherwise
                            // This is evaluated at compile time, not runtime
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Check if the argument is a constant expression
                            let is_constant = self.eval_const_expr(&arg).is_some();
                            return Ok(Self::typed_expr(
                                ExprKind::IntLit(if is_constant { 1 } else { 0 }),
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_expect" => {
                            // __builtin_expect(expr, c) - branch prediction hint
                            // Returns expr, the second argument is the expected value (for optimization hints)
                            // We just return expr since we don't do branch prediction optimization
                            self.expect_special(b'(')?;
                            let expr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let _expected = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(expr);
                        }
                        "__builtin_assume_aligned" => {
                            // __builtin_assume_aligned(ptr, align) or
                            // __builtin_assume_aligned(ptr, align, offset)
                            // Returns ptr, hints that ptr is aligned to align bytes
                            // We just return ptr since we don't do alignment optimization
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let _align = self.parse_assignment_expr()?;
                            // Optional third argument (offset)
                            if self.peek_special() == Some(b',' as u32) {
                                self.expect_special(b',')?;
                                let _offset = self.parse_assignment_expr()?;
                            }
                            self.expect_special(b')')?;
                            return Ok(ptr);
                        }
                        "__builtin_prefetch" => {
                            // __builtin_prefetch(addr) or
                            // __builtin_prefetch(addr, rw) or
                            // __builtin_prefetch(addr, rw, locality)
                            // Prefetch data at addr into cache - no-op for correctness
                            self.expect_special(b'(')?;
                            let _addr = self.parse_assignment_expr()?;
                            // Optional rw argument (0=read, 1=write)
                            if self.peek_special() == Some(b',' as u32) {
                                self.expect_special(b',')?;
                                let _rw = self.parse_assignment_expr()?;
                                // Optional locality argument (0-3)
                                if self.peek_special() == Some(b',' as u32) {
                                    self.expect_special(b',')?;
                                    let _locality = self.parse_assignment_expr()?;
                                }
                            }
                            self.expect_special(b')')?;
                            // Returns void - just return a void expression
                            return Ok(Self::typed_expr(
                                ExprKind::IntLit(0),
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_types_compatible_p" => {
                            // __builtin_types_compatible_p(type1, type2) - returns 1 if types are compatible
                            // This is evaluated at compile time, ignoring top-level qualifiers
                            self.expect_special(b'(')?;
                            let type1 = self.parse_type_name()?;
                            self.expect_special(b',')?;
                            let type2 = self.parse_type_name()?;
                            self.expect_special(b')')?;
                            // Check type compatibility (ignoring qualifiers)
                            let compatible = self.types.types_compatible(type1, type2);
                            return Ok(Self::typed_expr(
                                ExprKind::IntLit(if compatible { 1 } else { 0 }),
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_frame_address" => {
                            // __builtin_frame_address(level) - returns void*, address of frame at level
                            // Level 0 is the current frame, 1 is the caller's frame, etc.
                            // Returns NULL for invalid levels (beyond stack bounds)
                            self.expect_special(b'(')?;
                            let level = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::FrameAddress {
                                    level: Box::new(level),
                                },
                                self.types.void_ptr_id,
                                token_pos,
                            ));
                        }
                        "__builtin_return_address" => {
                            // __builtin_return_address(level) - returns void*, return address at level
                            // Level 0 is the current function's return address
                            // Returns NULL for invalid levels (beyond stack bounds)
                            self.expect_special(b'(')?;
                            let level = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::ReturnAddress {
                                    level: Box::new(level),
                                },
                                self.types.void_ptr_id,
                                token_pos,
                            ));
                        }
                        "setjmp" | "_setjmp" => {
                            // setjmp(env) - saves execution context, returns int
                            // Returns 0 on direct call, non-zero when returning via longjmp
                            self.expect_special(b'(')?;
                            let env = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Setjmp { env: Box::new(env) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "longjmp" | "_longjmp" => {
                            // longjmp(env, val) - restores execution context (never returns)
                            // Causes corresponding setjmp to return val (or 1 if val == 0)
                            self.expect_special(b'(')?;
                            let env = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Longjmp {
                                    env: Box::new(env),
                                    val: Box::new(val),
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_offsetof" | "offsetof" => {
                            // __builtin_offsetof(type, member-designator)
                            // Returns the byte offset of a member within a struct/union
                            // member-designator can be .field or [index] chains
                            self.expect_special(b'(')?;

                            // Parse the type name
                            let type_id = self.parse_type_name()?;

                            self.expect_special(b',')?;

                            // Parse member-designator starting with field name (no dot prefix for first field)
                            // Subsequent components use .field or [index] syntax
                            let mut path = Vec::new();

                            // Expect identifier for first member
                            let first_field = self.expect_identifier()?;
                            path.push(OffsetOfPath::Field(first_field));

                            // Parse subsequent designators
                            loop {
                                if self.is_special(b'.') {
                                    self.advance();
                                    let field = self.expect_identifier()?;
                                    path.push(OffsetOfPath::Field(field));
                                } else if self.is_special(b'[') {
                                    self.advance();
                                    // Parse constant expression for index
                                    let index_expr = self.parse_conditional_expr()?;
                                    let index_pos = index_expr.pos;
                                    self.expect_special(b']')?;
                                    // Evaluate as constant - offsetof requires compile-time constant
                                    let index_val =
                                        self.eval_const_expr(&index_expr).ok_or_else(|| {
                                            ParseError::new(
                                            "array index in offsetof must be a constant expression",
                                            index_pos,
                                        )
                                        })?;
                                    path.push(OffsetOfPath::Index(index_val));
                                } else {
                                    break;
                                }
                            }

                            self.expect_special(b')')?;

                            return Ok(Self::typed_expr(
                                ExprKind::OffsetOf { type_id, path },
                                self.types.ulong_id, // size_t is typically unsigned long
                                token_pos,
                            ));
                        }
                        // ================================================================
                        // Atomic builtins (Clang __c11_atomic_* for C11 stdatomic.h)
                        // ================================================================
                        "__c11_atomic_init" => {
                            // __c11_atomic_init(ptr, val) - initialize atomic (no ordering)
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicInit {
                                    ptr: Box::new(ptr),
                                    val: Box::new(val),
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_load" => {
                            // __c11_atomic_load(ptr, order) - returns *ptr atomically
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Result type is the pointed-to type
                            let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                            let result_type =
                                self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicLoad {
                                    ptr: Box::new(ptr),
                                    order: Box::new(order),
                                },
                                result_type,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_store" => {
                            // __c11_atomic_store(ptr, val, order) - *ptr = val atomically
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicStore {
                                    ptr: Box::new(ptr),
                                    val: Box::new(val),
                                    order: Box::new(order),
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_exchange" => {
                            // __c11_atomic_exchange(ptr, val, order) - swap and return old
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Result type is the pointed-to type
                            let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                            let result_type =
                                self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicExchange {
                                    ptr: Box::new(ptr),
                                    val: Box::new(val),
                                    order: Box::new(order),
                                },
                                result_type,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_compare_exchange_strong" => {
                            // __c11_atomic_compare_exchange_strong(ptr, expected, desired, succ, fail)
                            // Note: fail_order is parsed but ignored (we use succ_order for both)
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let expected = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let desired = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let succ_order = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let _fail_order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Returns bool (_Bool)
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicCompareExchangeStrong {
                                    ptr: Box::new(ptr),
                                    expected: Box::new(expected),
                                    desired: Box::new(desired),
                                    succ_order: Box::new(succ_order),
                                },
                                self.types.bool_id,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_compare_exchange_weak" => {
                            // __c11_atomic_compare_exchange_weak(ptr, expected, desired, succ, fail)
                            // Note: Implemented as strong (no spurious failures)
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let expected = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let desired = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let succ_order = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let _fail_order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Returns bool (_Bool)
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicCompareExchangeWeak {
                                    ptr: Box::new(ptr),
                                    expected: Box::new(expected),
                                    desired: Box::new(desired),
                                    succ_order: Box::new(succ_order),
                                },
                                self.types.bool_id,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_fetch_add" => {
                            // __c11_atomic_fetch_add(ptr, val, order) - add and return old
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Result type is the pointed-to type
                            let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                            let result_type =
                                self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicFetchAdd {
                                    ptr: Box::new(ptr),
                                    val: Box::new(val),
                                    order: Box::new(order),
                                },
                                result_type,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_fetch_sub" => {
                            // __c11_atomic_fetch_sub(ptr, val, order) - subtract and return old
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Result type is the pointed-to type
                            let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                            let result_type =
                                self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicFetchSub {
                                    ptr: Box::new(ptr),
                                    val: Box::new(val),
                                    order: Box::new(order),
                                },
                                result_type,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_fetch_and" => {
                            // __c11_atomic_fetch_and(ptr, val, order) - AND and return old
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Result type is the pointed-to type
                            let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                            let result_type =
                                self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicFetchAnd {
                                    ptr: Box::new(ptr),
                                    val: Box::new(val),
                                    order: Box::new(order),
                                },
                                result_type,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_fetch_or" => {
                            // __c11_atomic_fetch_or(ptr, val, order) - OR and return old
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Result type is the pointed-to type
                            let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                            let result_type =
                                self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicFetchOr {
                                    ptr: Box::new(ptr),
                                    val: Box::new(val),
                                    order: Box::new(order),
                                },
                                result_type,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_fetch_xor" => {
                            // __c11_atomic_fetch_xor(ptr, val, order) - XOR and return old
                            self.expect_special(b'(')?;
                            let ptr = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Result type is the pointed-to type
                            let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                            let result_type =
                                self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicFetchXor {
                                    ptr: Box::new(ptr),
                                    val: Box::new(val),
                                    order: Box::new(order),
                                },
                                result_type,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_thread_fence" => {
                            // __c11_atomic_thread_fence(order) - memory fence
                            self.expect_special(b'(')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicThreadFence {
                                    order: Box::new(order),
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__c11_atomic_signal_fence" => {
                            // __c11_atomic_signal_fence(order) - compiler barrier
                            self.expect_special(b'(')?;
                            let order = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::C11AtomicSignalFence {
                                    order: Box::new(order),
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        _ => {}
                    }

                    // Look up symbol to get type (during parsing, symbol is in scope)
                    // C99 6.4.2.2: __func__ is a predefined identifier with type const char[]
                    // GCC extensions: __FUNCTION__ and __PRETTY_FUNCTION__ behave similarly
                    if name_str == "__func__"
                        || name_str == "__FUNCTION__"
                        || name_str == "__PRETTY_FUNCTION__"
                    {
                        // These behave like a string literal (const char[])
                        // Linearization handles mapping to __func__ behavior
                        return Ok(Self::typed_expr(
                            ExprKind::FuncName,
                            self.types.char_ptr_id,
                            token_pos,
                        ));
                    }

                    // Check if this is an enum constant - if so, return IntLit
                    if let Some(sym) = self.symbols.lookup_enum_constant(name_id) {
                        if let Some(value) = sym.enum_value {
                            return Ok(Self::typed_expr(
                                ExprKind::IntLit(value),
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                    }

                    // Regular variable/function - look up the symbol
                    // The symbol must exist (error if undeclared)
                    if let Some(symbol_id) = self.symbols.lookup_id(name_id, Namespace::Ordinary) {
                        let typ = self.symbols.get(symbol_id).typ;
                        Ok(Self::typed_expr(ExprKind::Ident(symbol_id), typ, token_pos))
                    } else {
                        // C99 6.5.1: Undeclared identifier is an error
                        // (implicit int was removed in C99)
                        diag::error(token_pos, &format!("undeclared identifier '{}'", name_str));
                        // Return a dummy expression to continue parsing
                        Ok(Self::typed_expr(
                            ExprKind::IntLit(0),
                            self.types.int_id,
                            token_pos,
                        ))
                    }
                } else {
                    Err(ParseError::new("invalid identifier token", token.pos))
                }
            }

            TokenType::Char => {
                let token = self.consume();
                let token_pos = token.pos;
                if let TokenValue::Char(s) = &token.value {
                    // Parse character literal - type is int (C promotes char to int)
                    let c = self.parse_char_literal(s);
                    Ok(Self::typed_expr(
                        ExprKind::CharLit(c),
                        self.types.int_id,
                        token_pos,
                    ))
                } else {
                    Err(ParseError::new("invalid char token", token.pos))
                }
            }

            TokenType::WideChar => {
                let token = self.consume();
                let token_pos = token.pos;
                if let TokenValue::WideChar(s) = &token.value {
                    // Parse wide character literal - type is wchar_t (int on most platforms)
                    let c = self.parse_char_literal(s);
                    // wchar_t is defined as int on macOS/most Unix
                    Ok(Self::typed_expr(
                        ExprKind::CharLit(c),
                        self.types.int_id,
                        token_pos,
                    ))
                } else {
                    Err(ParseError::new("invalid wide char token", token.pos))
                }
            }

            TokenType::String => {
                let token = self.consume();
                let token_pos = token.pos;
                if let TokenValue::String(s) = &token.value {
                    // Parse string literal - convert escape sequences
                    let mut parsed = Self::parse_string_literal(s);

                    // Handle adjacent string literal concatenation (C99 6.4.5)
                    // "hello" "world" -> "helloworld"
                    while self.peek() == TokenType::String {
                        let next_token = self.consume();
                        if let TokenValue::String(next_s) = &next_token.value {
                            parsed.push_str(&Self::parse_string_literal(next_s));
                        }
                    }

                    // String literal type is char[N] where N includes the null terminator
                    // (C11 6.4.5: "type is array of char" not "pointer to char")
                    // The array-to-pointer decay happens when used in most contexts,
                    // but sizeof("hello") needs to return 6, not sizeof(char*).
                    let array_size = parsed.len() + 1; // +1 for null terminator
                    let str_type = self
                        .types
                        .intern(Type::array(self.types.char_id, array_size));
                    Ok(Self::typed_expr(
                        ExprKind::StringLit(parsed),
                        str_type,
                        token_pos,
                    ))
                } else {
                    Err(ParseError::new("invalid string token", token.pos))
                }
            }

            TokenType::WideString => {
                let token = self.consume();
                let token_pos = token.pos;
                if let TokenValue::WideString(s) = &token.value {
                    // Parse wide string literal - convert escape sequences
                    let mut parsed = Self::parse_string_literal(s);

                    // Handle adjacent wide string literal concatenation
                    while self.peek() == TokenType::WideString {
                        let next_token = self.consume();
                        if let TokenValue::WideString(next_s) = &next_token.value {
                            parsed.push_str(&Self::parse_string_literal(next_s));
                        }
                    }

                    // Wide string literal type is wchar_t[N] (wchar_t is int on this platform)
                    // The array size includes the null terminator
                    let array_size = parsed.chars().count() + 1; // +1 for null terminator
                    let wstr_type = self
                        .types
                        .intern(Type::array(self.types.int_id, array_size));
                    Ok(Self::typed_expr(
                        ExprKind::WideStringLit(parsed),
                        wstr_type,
                        token_pos,
                    ))
                } else {
                    Err(ParseError::new("invalid wide string token", token.pos))
                }
            }

            TokenType::Special => {
                if self.is_special(b'(') {
                    // Parenthesized expression or cast
                    let paren_pos = self.current_pos();
                    self.advance();

                    // Check for statement expression: ({ ... })
                    // GNU extension allowing compound statements as expressions
                    if self.is_special(b'{') {
                        return self.parse_stmt_expr(paren_pos);
                    }

                    // Try to detect cast (type) or compound literal (type){...}
                    if let Some(typ) = self.try_parse_type_name() {
                        self.expect_special(b')')?;

                        // Check for compound literal: (type){ ... }
                        if self.is_special(b'{') {
                            let init_list = self.parse_initializer_list()?;
                            let elements = match init_list.kind {
                                ExprKind::InitList { elements } => elements,
                                _ => unreachable!(),
                            };

                            // For incomplete array types (size 0), determine size from initializer
                            let final_typ = if self.types.kind(typ) == TypeKind::Array
                                && self.types.get(typ).array_size == Some(0)
                            {
                                let elem_type =
                                    self.types.base_type(typ).unwrap_or(self.types.int_id);
                                let array_size = self.array_size_from_elements(&elements);
                                self.types.intern(Type::array(elem_type, array_size))
                            } else {
                                typ
                            };

                            return Ok(Self::typed_expr(
                                ExprKind::CompoundLiteral {
                                    typ: final_typ,
                                    elements,
                                },
                                final_typ,
                                paren_pos,
                            ));
                        }

                        // Regular cast expression
                        let expr = self.parse_unary_expr()?;
                        return Ok(Self::typed_expr(
                            ExprKind::Cast {
                                cast_type: typ,
                                expr: Box::new(expr),
                            },
                            typ,
                            paren_pos,
                        ));
                    }

                    // Regular parenthesized expression
                    let expr = self.parse_expression()?;
                    self.expect_special(b')')?;
                    Ok(expr)
                } else {
                    Err(ParseError::new(
                        "unexpected token in expression".to_string(),
                        self.current_pos(),
                    ))
                }
            }

            _ => Err(ParseError::new(
                format!("unexpected token {:?}", self.peek()),
                self.current_pos(),
            )),
        }
    }

    /// Parse a number literal string into an expression
    fn parse_number_literal(&self, s: &str, pos: Position) -> ParseResult<Expr> {
        let s_lower = s.to_lowercase();

        // Check if it's a hex number (must check before suffix trimming)
        let is_hex = s_lower.starts_with("0x");

        // Check if it's a floating point number
        let is_float = s_lower.contains('.')
            || (s_lower.contains('e') && !is_hex)
            || (s_lower.contains('p') && is_hex);

        // Detect C23 _Float* suffixes (f16, F16, f32, F32, f64, F64)
        // Only for non-hex numbers since f16/f32/f64 are valid hex digit sequences
        let is_float16_suffix = !is_hex && s_lower.ends_with("f16");
        let is_float32_suffix = !is_hex && s_lower.ends_with("f32");
        let is_float64_suffix = !is_hex && s_lower.ends_with("f64");

        // Remove suffixes - but for hex numbers, don't strip a-f as they're digits
        let num_str = if is_hex {
            // For hex, only strip u/l suffixes (not f which is a hex digit)
            s_lower.trim_end_matches(['u', 'l'])
        } else if is_float16_suffix {
            s_lower.trim_end_matches("f16")
        } else if is_float32_suffix {
            s_lower.trim_end_matches("f32")
        } else if is_float64_suffix {
            s_lower.trim_end_matches("f64")
        } else {
            // For decimal/octal, strip u/l/f suffixes
            s_lower.trim_end_matches(['u', 'l', 'f'])
        };

        if is_float || is_float16_suffix || is_float32_suffix || is_float64_suffix {
            // Float - type depends on suffix:
            // - no suffix = double
            // - f/F = float
            // - l/L = long double
            // - f16/F16 = _Float16
            // - f32/F32 = float (alias)
            // - f64/F64 = double (alias)
            let is_float_suffix = !is_float16_suffix
                && !is_float32_suffix
                && !is_float64_suffix
                && s_lower.ends_with('f');
            let is_longdouble_suffix = !is_float16_suffix
                && !is_float32_suffix
                && !is_float64_suffix
                && s_lower.ends_with('l');
            let value: f64 = if is_hex {
                // Hex float parsing: 0x[hex-digits].[hex-digits]p[±exponent]
                // Value = significand × 2^exponent
                Self::parse_hex_float(num_str).map_err(|_| {
                    ParseError::new(format!("invalid hex float literal: {}", s), pos)
                })?
            } else {
                num_str
                    .parse()
                    .map_err(|_| ParseError::new(format!("invalid float literal: {}", s), pos))?
            };
            let typ = if is_float16_suffix {
                self.types.float16_id
            } else if is_float32_suffix {
                self.types.float_id // f32 is alias for float
            } else if is_float64_suffix {
                self.types.double_id // f64 is alias for double
            } else if is_float_suffix {
                self.types.float_id
            } else if is_longdouble_suffix {
                self.types.longdouble_id
            } else {
                self.types.double_id
            };
            Ok(Self::typed_expr(ExprKind::FloatLit(value), typ, pos))
        } else {
            // Integer - determine type from suffix
            // Check for long long first (ll, ull, llu) before checking for long (l, ul, lu)
            let is_longlong =
                s_lower.ends_with("ll") || s_lower.ends_with("ull") || s_lower.ends_with("llu");
            let is_long = !is_longlong
                && (s_lower.ends_with('l') || s_lower.ends_with("ul") || s_lower.ends_with("lu"));
            let is_unsigned = s_lower.contains('u');

            // Parse as u64 first to handle large unsigned values, then reinterpret as i64
            let value_u64: u64 = if is_hex {
                // Strip 0x or 0X prefix
                let hex_part = num_str
                    .strip_prefix("0x")
                    .or_else(|| num_str.strip_prefix("0X"))
                    .unwrap_or(num_str);
                u64::from_str_radix(hex_part, 16)
            } else if let Some(bin_part) = num_str.strip_prefix("0b") {
                u64::from_str_radix(bin_part, 2)
            } else if num_str.starts_with('0') && num_str.len() > 1 {
                u64::from_str_radix(num_str, 8)
            } else {
                num_str.parse()
            }
            .map_err(|_| ParseError::new(format!("invalid integer literal: {}", s), pos))?;

            // Reinterpret bits as i64 (preserves bit pattern for unsigned values)
            let value = value_u64 as i64;

            // Determine type according to C99 6.4.4.1:
            // - Decimal constants: int, long int, long long int (signed only)
            // - Hex/Octal constants: int, unsigned int, long int, unsigned long int,
            //   long long int, unsigned long long int (both signed and unsigned)
            // The type is the first in the list that can represent the value.
            let is_octal = !is_hex && num_str.starts_with('0') && num_str.len() > 1;
            let typ = if is_unsigned {
                // Explicit U suffix
                match (is_longlong, is_long) {
                    (true, _) => self.types.ulonglong_id,
                    (false, true) => self.types.ulong_id,
                    (false, false) => self.types.uint_id,
                }
            } else if is_hex || is_octal {
                // Hex/octal without U suffix - use first type that fits (C99 6.4.4.1)
                match (is_longlong, is_long) {
                    (true, _) => {
                        // long long or unsigned long long
                        if value_u64 <= i64::MAX as u64 {
                            self.types.longlong_id
                        } else {
                            self.types.ulonglong_id
                        }
                    }
                    (false, true) => {
                        // long or unsigned long
                        if value_u64 <= i64::MAX as u64 {
                            self.types.long_id
                        } else {
                            self.types.ulong_id
                        }
                    }
                    (false, false) => {
                        // int, unsigned int, long, unsigned long, long long, unsigned long long
                        if value_u64 <= i32::MAX as u64 {
                            self.types.int_id
                        } else if value_u64 <= u32::MAX as u64 {
                            self.types.uint_id
                        } else if value_u64 <= i64::MAX as u64 {
                            self.types.long_id
                        } else {
                            self.types.ulong_id
                        }
                    }
                }
            } else {
                // Decimal without U suffix - signed types only
                match (is_longlong, is_long) {
                    (true, _) => self.types.longlong_id,
                    (false, true) => self.types.long_id,
                    (false, false) => {
                        // int, long, long long
                        if value_u64 <= i32::MAX as u64 {
                            self.types.int_id
                        } else if value_u64 <= i64::MAX as u64 {
                            self.types.long_id
                        } else {
                            self.types.longlong_id
                        }
                    }
                }
            };
            Ok(Self::typed_expr(ExprKind::IntLit(value), typ, pos))
        }
    }

    /// Parse a hexadecimal floating-point literal (C99 feature)
    /// Format: 0x[hex-mantissa]p[±exponent] where mantissa can have decimal point
    /// Value = significand × 2^exponent
    fn parse_hex_float(s: &str) -> Result<f64, ()> {
        // Strip 0x prefix
        let s = s
            .strip_prefix("0x")
            .or_else(|| s.strip_prefix("0X"))
            .ok_or(())?;

        // Find 'p' or 'P' separator for exponent
        let p_pos = s.find(['p', 'P']).ok_or(())?;
        let (mantissa_str, exp_str) = s.split_at(p_pos);
        let exp_str = &exp_str[1..]; // Skip the 'p'/'P'

        // Parse mantissa (may have decimal point)
        let (int_part, frac_part) = if let Some(dot_pos) = mantissa_str.find('.') {
            (&mantissa_str[..dot_pos], &mantissa_str[dot_pos + 1..])
        } else {
            (mantissa_str, "")
        };

        // Convert hex mantissa to f64
        let int_val = if int_part.is_empty() {
            0u64
        } else {
            u64::from_str_radix(int_part, 16).map_err(|_| ())?
        };

        let mut mantissa = int_val as f64;

        // Add fractional part: each hex digit after dot is worth 1/16, 1/256, etc.
        if !frac_part.is_empty() {
            let frac_val = u64::from_str_radix(frac_part, 16).map_err(|_| ())?;
            let frac_bits = frac_part.len() * 4; // 4 bits per hex digit
            mantissa += frac_val as f64 / (1u64 << frac_bits) as f64;
        }

        // Parse exponent (base 10 number representing power of 2)
        let exponent: i32 = exp_str.parse().map_err(|_| ())?;

        // Calculate final value: mantissa × 2^exponent
        Ok(mantissa * (2.0_f64).powi(exponent))
    }

    /// Parse an escape sequence starting at position i (after the backslash).
    /// Returns (unescaped_char, number_of_chars_consumed_after_backslash).
    fn parse_escape_sequence(chars: &[char], i: usize) -> (char, usize) {
        if i >= chars.len() {
            return ('\\', 0);
        }

        match chars[i] {
            'n' => ('\n', 1),
            't' => ('\t', 1),
            'r' => ('\r', 1),
            '\\' => ('\\', 1),
            '\'' => ('\'', 1),
            '"' => ('"', 1),
            'a' => ('\x07', 1), // bell
            'b' => ('\x08', 1), // backspace
            'f' => ('\x0C', 1), // form feed
            'v' => ('\x0B', 1), // vertical tab
            'x' => {
                // Hex escape \xHH - consume all hex digits
                let mut hex_chars = 0;
                while i + 1 + hex_chars < chars.len()
                    && chars[i + 1 + hex_chars].is_ascii_hexdigit()
                {
                    hex_chars += 1;
                }
                if hex_chars > 0 {
                    let hex: String = chars[i + 1..i + 1 + hex_chars].iter().collect();
                    // C allows arbitrary-length hex escapes, but only low 8 bits matter
                    let val = u64::from_str_radix(&hex, 16).unwrap_or(0) as u8;
                    (val as char, 1 + hex_chars)
                } else {
                    ('x', 1) // \x with no hex digits - just 'x'
                }
            }
            'u' => {
                // UCN \uXXXX - exactly 4 hex digits (C99 6.4.3)
                if i + 4 < chars.len() && chars[i + 1..i + 5].iter().all(|c| c.is_ascii_hexdigit())
                {
                    let hex: String = chars[i + 1..i + 5].iter().collect();
                    let val = u32::from_str_radix(&hex, 16).unwrap_or(0);
                    if let Some(c) = char::from_u32(val) {
                        (c, 5)
                    } else {
                        ('u', 1) // Invalid code point
                    }
                } else {
                    ('u', 1) // Not enough hex digits
                }
            }
            'U' => {
                // UCN \UXXXXXXXX - exactly 8 hex digits (C99 6.4.3)
                if i + 8 < chars.len() && chars[i + 1..i + 9].iter().all(|c| c.is_ascii_hexdigit())
                {
                    let hex: String = chars[i + 1..i + 9].iter().collect();
                    let val = u32::from_str_radix(&hex, 16).unwrap_or(0);
                    if let Some(c) = char::from_u32(val) {
                        (c, 9)
                    } else {
                        ('U', 1) // Invalid code point
                    }
                } else {
                    ('U', 1) // Not enough hex digits
                }
            }
            c if c.is_ascii_digit() && c != '8' && c != '9' => {
                // Octal escape \NNN (up to 3 digits)
                let mut oct_chars = 1;
                while oct_chars < 3
                    && i + oct_chars < chars.len()
                    && chars[i + oct_chars].is_ascii_digit()
                    && chars[i + oct_chars] != '8'
                    && chars[i + oct_chars] != '9'
                {
                    oct_chars += 1;
                }
                let oct: String = chars[i..i + oct_chars].iter().collect();
                let val = u8::from_str_radix(&oct, 8).unwrap_or(0);
                (val as char, oct_chars)
            }
            c => (c, 1), // Unknown escape - just return the character
        }
    }

    /// Parse a character literal string into a char
    fn parse_char_literal(&self, s: &str) -> char {
        if s.is_empty() {
            return '\0';
        }

        let chars: Vec<char> = s.chars().collect();
        if chars[0] == '\\' && chars.len() > 1 {
            let (c, _) = Self::parse_escape_sequence(&chars, 1);
            c
        } else {
            chars[0]
        }
    }

    /// Parse a string literal, converting escape sequences to their actual values.
    /// This implements C99 translation phase 5 for string literals.
    pub(crate) fn parse_string_literal(s: &str) -> String {
        let chars: Vec<char> = s.chars().collect();
        let mut result = String::new();
        let mut i = 0;

        while i < chars.len() {
            if chars[i] == '\\' && i + 1 < chars.len() {
                let (c, consumed) = Self::parse_escape_sequence(&chars, i + 1);
                result.push(c);
                i += 1 + consumed;
            } else {
                result.push(chars[i]);
                i += 1;
            }
        }

        result
    }
}
