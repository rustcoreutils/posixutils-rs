//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Expression parsing for c17 C17 compiler
//

use super::ast::{
    AssignOp, BinaryOp, Designator, Expr, ExprKind, FpTest, InitElement, OffsetOfPath, UnaryOp,
};
use super::parser::{DeclaratorName, ParseError, ParseResult, Parser};
use crate::diag;
use crate::float::FloatVal;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId};
use crate::token::lexer::{Position, SpecialToken, TokenType, TokenValue};
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;

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
                designators.push(Designator::Index(index as i64));
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

    /// Compute common type for ternary operator branches (C99 6.5.15, 6.3.1.8)
    fn ternary_common_type(&mut self, then_typ: TypeId, else_typ: TypeId) -> TypeId {
        let then_kind = self.types.kind(then_typ);
        let else_kind = self.types.kind(else_typ);

        // If either is a pointer, use pointer type
        if then_kind == TypeKind::Pointer {
            return then_typ;
        }
        if else_kind == TypeKind::Pointer {
            return else_typ;
        }

        // If either is void, result is void
        if then_kind == TypeKind::Void || else_kind == TypeKind::Void {
            return self.types.void_id;
        }

        // Both arithmetic: apply usual arithmetic conversions
        // Float types take precedence
        if self.types.is_float(then_typ) || self.types.is_float(else_typ) {
            if then_kind == TypeKind::Float128 || else_kind == TypeKind::Float128 {
                return self.types.float128_id;
            }
            if then_kind == TypeKind::LongDouble || else_kind == TypeKind::LongDouble {
                return self.types.longdouble_id;
            }
            if then_kind == TypeKind::Double || else_kind == TypeKind::Double {
                return self.types.double_id;
            }
            return self.types.float_id;
        }

        // Integer types: promote both, then pick the wider/unsigned
        let then_size = self.types.size_bits(then_typ).max(32); // integer promotion
        let else_size = self.types.size_bits(else_typ).max(32);

        // Pick the wider type, or int if both are narrow
        if then_size >= else_size && then_size >= 32 {
            if then_size == 32 {
                return self.types.int_id;
            }
            return then_typ;
        }
        if else_size >= then_size && else_size >= 32 {
            if else_size == 32 {
                return self.types.int_id;
            }
            return else_typ;
        }
        self.types.int_id
    }

    /// Apply the array-to-pointer and function-to-pointer decays of C17
    /// 6.3.2.1p3-4. Qualifiers are left alone.
    pub(crate) fn decayed_type(&mut self, typ: TypeId) -> TypeId {
        match self.types.kind(typ) {
            TypeKind::Array => {
                let elem = self.types.base_type(typ).unwrap_or(self.types.char_id);
                self.types.intern(Type::pointer(elem))
            }
            TypeKind::Function => self.types.intern(Type::pointer(typ)),
            _ => typ,
        }
    }

    /// The type an lvalue expression has after lvalue conversion: decayed as
    /// above, then stripped of every top-level qualifier.
    ///
    /// This is what C17 6.5.1.1p2 requires of a `_Generic` controlling
    /// expression, and it is why `_Generic(x, int: ..., const int: ...)` can
    /// never select the `const int` association.
    pub(crate) fn lvalue_converted_type(&mut self, typ: TypeId) -> TypeId {
        let decayed = self.decayed_type(typ);

        const QUALIFIERS: TypeModifiers = TypeModifiers::CONST
            .union(TypeModifiers::VOLATILE)
            .union(TypeModifiers::RESTRICT)
            .union(TypeModifiers::ATOMIC);

        let ty = self.types.get(decayed);
        if !ty.modifiers.intersects(QUALIFIERS) {
            return decayed;
        }

        let mut unqualified = ty.clone();
        unqualified.modifiers.remove(QUALIFIERS);
        self.types.intern(unqualified)
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

            // Decay arrays to pointers, functions to pointer-to-function
            let then_decayed = self.decayed_type(then_typ);
            let else_decayed = self.decayed_type(else_typ);

            // Compute common type of then and else branches (C99 6.5.15)
            let typ = self.ternary_common_type(then_decayed, else_decayed);

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
            // Deref produces the base type of the pointer.
            // Function types: *func is a no-op in C (6.5.3.2, 6.3.2.1).
            // A function identifier has function type which decays to pointer-
            // to-function in expression context; base_type of pointer-to-function
            // is the function type.  But if the operand already has function type
            // (not yet decayed), base_type would give the return type — wrong.
            // In that case, keep the function type as-is.
            let typ = operand
                .typ
                .map(|t| {
                    if self.types.kind(t) == crate::types::TypeKind::Function {
                        t // *func_name is a no-op; result is still function type
                    } else {
                        self.types.base_type(t).unwrap_or(self.types.int_id)
                    }
                })
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
            // C99 6.3.1.1: integer promotion — types smaller than int promote to int
            let op_typ = operand.typ.unwrap_or(self.types.int_id);
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
            if let Some(name_id) = self.get_ident_id(self.current()) {
                if name_id == crate::kw::SIZEOF {
                    self.advance();
                    return self.parse_sizeof();
                }
                if matches!(
                    name_id,
                    crate::kw::ALIGNOF
                        | crate::kw::GNU_ALIGNOF
                        | crate::kw::GNU_ALIGNOF2
                        | crate::kw::ALIGNOF_C23
                ) && !self.builtin_is_shadowed(name_id)
                {
                    self.advance();
                    return self.parse_alignof();
                }
                // GCC's `__real__` / `__imag__`. The result type is the
                // operand's base type when it is complex, and the operand's own
                // type otherwise -- gcc accepts both, and `__real__` of a real
                // value is that value.
                if matches!(
                    name_id,
                    crate::kw::REAL_KW
                        | crate::kw::REAL_KW_SHORT
                        | crate::kw::IMAG_KW
                        | crate::kw::IMAG_KW_SHORT
                ) {
                    let is_real = matches!(name_id, crate::kw::REAL_KW | crate::kw::REAL_KW_SHORT);
                    let op_pos = self.current_pos();
                    self.advance();
                    let operand = self.parse_unary_expr()?;
                    let op_typ = operand.typ.unwrap_or(self.types.double_id);
                    let result_typ = if self.types.is_complex(op_typ) {
                        self.types.complex_base(op_typ)
                    } else {
                        op_typ
                    };
                    return Ok(Expr::typed(
                        ExprKind::Unary {
                            op: if is_real {
                                UnaryOp::Real
                            } else {
                                UnaryOp::Imag
                            },
                            operand: Box::new(operand),
                        },
                        result_typ,
                        op_pos,
                    ));
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
    pub(crate) fn is_type_keyword(id: crate::strings::StringId) -> bool {
        crate::kw::has_tag(id, crate::kw::TYPE_KEYWORD)
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
            match name_id {
                crate::kw::CONST | crate::kw::GNU_CONST2 | crate::kw::GNU_CONST => {
                    self.advance();
                    mods |= TypeModifiers::CONST;
                }
                crate::kw::VOLATILE | crate::kw::GNU_VOLATILE2 | crate::kw::GNU_VOLATILE => {
                    self.advance();
                    mods |= TypeModifiers::VOLATILE;
                }
                crate::kw::RESTRICT | crate::kw::GNU_RESTRICT2 | crate::kw::GNU_RESTRICT => {
                    self.advance();
                    mods |= TypeModifiers::RESTRICT;
                }
                crate::kw::ATOMIC => {
                    self.advance();
                    mods |= TypeModifiers::ATOMIC;
                }
                _ if super::is_nullability_qualifier(name_id) => {
                    self.advance();
                }
                _ => break,
            }
        }
        mods
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
    /// Parse a type-name: a specifier-qualifier list followed by an optional
    /// abstract declarator (C17 6.7.7). Speculative -- the caller uses it to
    /// tell `(type)expr` from `(expr)`, so a failure rewinds and answers
    /// `None` rather than reporting anything.
    ///
    /// The abstract declarator goes through `parse_declarator`, the same
    /// parser every other declarator uses. This file used to carry its own,
    /// which recognised exactly one shape -- `(*)(params)` -- and backtracked
    /// on everything else, so `int (*)[3]`, `int (**)(void)` and
    /// `int (*[4])(void)` were not type-names at all: `sizeof(int (*)[3])` and
    /// the cast `(int(*)[3])0` were parse errors. An abstract declarator is
    /// just a declarator whose identifier is absent, which `parse_declarator`
    /// already represents as `StringId::EMPTY`, so there was never a reason
    /// for a second implementation.
    pub(crate) fn try_parse_type_name(&mut self) -> Option<TypeId> {
        let saved_pos = self.pos;
        let base = self.try_parse_specifier_qualifier_list()?;

        match self.parse_declarator(base, DeclaratorName::Optional) {
            // An abstract declarator names nothing. A name here means this was
            // never a type-name, so let the caller try it as an expression.
            Ok((name, typ, _vla, _params)) if name == StringId::EMPTY => Some(typ),
            _ => {
                self.pos = saved_pos;
                None
            }
        }
    }

    /// The specifier-qualifier list of a type-name, without its declarator.
    fn try_parse_specifier_qualifier_list(&mut self) -> Option<TypeId> {
        if self.peek() != TokenType::Ident {
            return None;
        }

        // Check if this looks like a type name (keyword or typedef)
        let name_id = self.get_ident_id(self.current())?;
        if !Self::is_type_keyword(name_id) && self.symbols.lookup_typedef(name_id).is_none() {
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
            match name_id {
                crate::kw::CONST => {
                    self.advance();
                    modifiers |= TypeModifiers::CONST;
                    parsed_something = true;
                }
                crate::kw::VOLATILE => {
                    self.advance();
                    modifiers |= TypeModifiers::VOLATILE;
                    parsed_something = true;
                }
                crate::kw::SIGNED => {
                    self.advance();
                    modifiers |= TypeModifiers::SIGNED;
                    parsed_something = true;
                }
                crate::kw::UNSIGNED => {
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                    parsed_something = true;
                }
                crate::kw::COMPLEX => {
                    self.advance();
                    modifiers |= TypeModifiers::COMPLEX;
                    parsed_something = true;
                }
                crate::kw::ATOMIC => {
                    self.advance();
                    // _Atomic can be:
                    // 1. Type specifier: _Atomic(type-name)
                    // 2. Type qualifier: _Atomic (without parens)
                    if self.is_special(b'(') {
                        // Type specifier form: _Atomic(type-name)
                        self.advance(); // consume '('
                        let inner_type = self.try_parse_type_name()?;
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
                        return Some(result_id);
                    } else {
                        // Qualifier form: just _Atomic
                        modifiers |= TypeModifiers::ATOMIC;
                    }
                    parsed_something = true;
                }
                crate::kw::SHORT => {
                    self.advance();
                    modifiers |= TypeModifiers::SHORT;
                    if base_kind.is_none() {
                        base_kind = Some(TypeKind::Short);
                    }
                    parsed_something = true;
                }
                crate::kw::LONG => {
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
                crate::kw::VOID => {
                    self.advance();
                    base_kind = Some(TypeKind::Void);
                    parsed_something = true;
                }
                crate::kw::CHAR => {
                    self.advance();
                    base_kind = Some(TypeKind::Char);
                    parsed_something = true;
                }
                crate::kw::INT => {
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
                crate::kw::FLOAT => {
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                    parsed_something = true;
                }
                crate::kw::DOUBLE => {
                    self.advance();
                    // Handle long double
                    if modifiers.contains(TypeModifiers::LONG) {
                        base_kind = Some(TypeKind::LongDouble);
                    } else {
                        base_kind = Some(TypeKind::Double);
                    }
                    parsed_something = true;
                }
                crate::kw::FLOAT16 => {
                    self.advance();
                    base_kind = Some(TypeKind::Float16);
                    parsed_something = true;
                }
                crate::kw::FLOAT32 => {
                    // _Float32 is an alias for float (TS 18661-3 / C23)
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                    parsed_something = true;
                }
                crate::kw::FLOAT64 => {
                    // _Float64 is an alias for double (TS 18661-3 / C23)
                    self.advance();
                    base_kind = Some(TypeKind::Double);
                    parsed_something = true;
                }
                crate::kw::FLOAT128 | crate::kw::FLOAT128_ALIAS => {
                    // IEEE binary128; see the declaration parser's arm.
                    if !self.types.has_float128() {
                        break;
                    }
                    self.advance();
                    base_kind = Some(TypeKind::Float128);
                    parsed_something = true;
                }
                crate::kw::BOOL => {
                    self.advance();
                    base_kind = Some(TypeKind::Bool);
                    parsed_something = true;
                }
                crate::kw::INT128 => {
                    self.advance();
                    base_kind = Some(TypeKind::Int128);
                    parsed_something = true;
                }
                crate::kw::INT128_T => {
                    self.advance();
                    base_kind = Some(TypeKind::Int128);
                    parsed_something = true;
                }
                crate::kw::UINT128_T => {
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                    base_kind = Some(TypeKind::Int128);
                    parsed_something = true;
                }
                crate::kw::BUILTIN_VA_LIST => {
                    self.advance();
                    base_kind = Some(TypeKind::VaList);
                    parsed_something = true;
                }
                crate::kw::TYPEOF | crate::kw::GNU_TYPEOF | crate::kw::GNU_TYPEOF2 => {
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
                        return Some(typ);
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
                    return Some(expr_type);
                }
                crate::kw::STRUCT => {
                    self.advance(); // consume 'struct'
                                    // For struct tag reference, look up directly in symbol table
                    if let Some(tag_name) = self.get_ident_id(self.current()) {
                        if !self.is_special(b'{') {
                            // This is a tag reference (e.g., "struct Point*")
                            self.advance(); // consume tag name
                            if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                                return Some(self.apply_trailing_qualifiers(existing.typ));
                            }
                            // Tag not found - create incomplete struct type and register it
                            // This ensures that when the struct is later defined, we can update
                            // this same TypeId rather than creating a new one
                            let mut incomplete = Type::incomplete_struct(tag_name);
                            incomplete.modifiers |= self.consume_type_qualifiers();
                            let result_id = self.types.intern(incomplete);
                            let sym = Symbol::tag(tag_name, result_id, self.symbols.depth());
                            let _ = self.symbols.declare(sym);
                            return Some(result_id);
                        }
                    }
                    // Fall back to full struct parsing for definitions
                    self.pos -= 1;
                    if let Ok(struct_type) = self.parse_struct_or_union_specifier(false) {
                        let mut typ = struct_type;
                        typ.modifiers |= modifiers | self.consume_type_qualifiers();
                        return Some(self.types.intern(typ));
                    }
                    return None;
                }
                crate::kw::UNION => {
                    self.advance(); // consume 'union'
                                    // For union tag reference, look up directly in symbol table
                    if let Some(tag_name) = self.get_ident_id(self.current()) {
                        if !self.is_special(b'{') {
                            // This is a tag reference
                            self.advance(); // consume tag name
                            if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                                return Some(self.apply_trailing_qualifiers(existing.typ));
                            }
                            // Tag not found - create incomplete union type and register it
                            // This ensures that when the union is later defined, we can update
                            // this same TypeId rather than creating a new one
                            let mut incomplete = Type::incomplete_union(tag_name);
                            incomplete.modifiers |= self.consume_type_qualifiers();
                            let result_id = self.types.intern(incomplete);
                            let sym = Symbol::tag(tag_name, result_id, self.symbols.depth());
                            let _ = self.symbols.declare(sym);
                            return Some(result_id);
                        }
                    }
                    // Fall back to full union parsing for definitions
                    self.pos -= 1;
                    if let Ok(union_type) = self.parse_struct_or_union_specifier(true) {
                        let mut typ = union_type;
                        typ.modifiers |= modifiers | self.consume_type_qualifiers();
                        return Some(self.types.intern(typ));
                    }
                    return None;
                }
                crate::kw::ENUM => {
                    if let Ok(enum_type) = self.parse_enum_specifier() {
                        let mut typ = enum_type;
                        typ.modifiers |= modifiers | self.consume_type_qualifiers();
                        return Some(self.types.intern(typ));
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
        let result_id = if let Some(typedef_type_id) = typedef_base {
            // Drop the TYPEDEF bit either way. It records how the name was
            // *declared*, not anything about the type, and leaving it on made a
            // typedef's type differ from the type it aliases -- so
            // `__builtin_types_compatible_p(int, MyInt)` answered 0, and
            // `_Generic` could neither match nor reject a typedef'd
            // association. The trailing-modifier path already cleared it; the
            // bare path did not.
            let typedef_type = self.types.get(typedef_type_id);
            if !modifiers.is_empty() || typedef_type.modifiers.contains(TypeModifiers::TYPEDEF) {
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
                            &gettext("request for member in something not a structure or union"),
                        );
                        self.types.int_id
                    } else if let Some(info) = self.types.find_member(resolved, member) {
                        info.typ
                    } else {
                        let member_name = self.idents.get_opt(member).unwrap_or("<unknown>");
                        diag::error_args(dot_pos, "has no member named '{0}'", &[member_name]);
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
                                &gettext(
                                    "request for member in something not a structure or union",
                                ),
                            );
                            self.types.int_id
                        } else if let Some(info) = self.types.find_member(resolved, member) {
                            info.typ
                        } else {
                            let member_name = self.idents.get_opt(member).unwrap_or("<unknown>");
                            diag::error_args(
                                arrow_pos,
                                "has no member named '{0}'",
                                &[member_name],
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
                let call_pos = self.current_pos();
                self.advance();
                let args = self.parse_argument_list()?;
                self.expect_special(b')')?;
                self.check_call_arity(&expr, &args, call_pos);

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
    /// Parse a run of adjacent string literals into one expression.
    ///
    /// C11 6.4.5p5: if any literal in the run has an encoding prefix, the
    /// result takes that encoding; a run mixing two *different* prefixes is a
    /// constraint violation (6.4.5p2).
    fn parse_string_literal_run(&mut self) -> ParseResult<Expr> {
        let start_pos = self.current_pos();
        // Bytes of the concatenated literal. `parse_string_literal` yields one
        // `char` per byte, so this stays byte-exact for the narrow case.
        let mut bytes = String::new();
        let mut encoding: Option<TokenType> = None;
        let mut mixed_reported = false;

        loop {
            let kind = self.peek();
            let piece = match kind {
                TokenType::String
                | TokenType::WideString
                | TokenType::Utf16String
                | TokenType::Utf32String => {
                    let token = self.consume();
                    match &token.value {
                        TokenValue::String(s)
                        | TokenValue::WideString(s)
                        | TokenValue::Utf16String(s)
                        | TokenValue::Utf32String(s) => Self::parse_string_literal(s),
                        _ => return Err(ParseError::new("invalid string token", token.pos)),
                    }
                }
                _ => break,
            };
            bytes.push_str(&piece);

            if kind != TokenType::String {
                match encoding {
                    None => encoding = Some(kind),
                    Some(prev) if prev != kind && !mixed_reported => {
                        diag::error(
                            start_pos,
                            &gettext(
                                "concatenation of string literals with different encoding prefixes",
                            ),
                        );
                        mixed_reported = true;
                    }
                    _ => {}
                }
            }
        }

        match encoding {
            // char[N]. Each element is a byte, and `bytes` already holds one
            // char per byte, so the count is exact for non-ASCII too.
            None => {
                let array_size = bytes.chars().count() + 1;
                let str_type = self
                    .types
                    .intern(Type::array(self.types.char_id, array_size));
                Ok(Self::typed_expr(
                    ExprKind::StringLit(bytes),
                    str_type,
                    start_pos,
                ))
            }
            // wchar_t[N] — int on the targets here.
            Some(TokenType::WideString) => {
                let array_size = bytes.chars().count() + 1;
                let wstr_type = self
                    .types
                    .intern(Type::array(self.types.int_id, array_size));
                Ok(Self::typed_expr(
                    ExprKind::WideStringLit(bytes),
                    wstr_type,
                    start_pos,
                ))
            }
            // char16_t[N] / char32_t[N]. These carry real code units rather
            // than bytes, so the UTF-8 the lexer preserved is decoded here; a
            // code point outside the BMP becomes a surrogate pair in the
            // char16_t case.
            Some(kind @ (TokenType::Utf16String | TokenType::Utf32String)) => {
                let text = Self::literal_bytes_as_text(&bytes);
                if kind == TokenType::Utf16String {
                    let units: Vec<u16> = text.encode_utf16().collect();
                    let t = self
                        .types
                        .intern(Type::array(self.types.ushort_id, units.len() + 1));
                    Ok(Self::typed_expr(
                        ExprKind::Utf16StringLit(units),
                        t,
                        start_pos,
                    ))
                } else {
                    let units: Vec<u32> = text.chars().map(|c| c as u32).collect();
                    let t = self
                        .types
                        .intern(Type::array(self.types.uint_id, units.len() + 1));
                    Ok(Self::typed_expr(
                        ExprKind::Utf32StringLit(units),
                        t,
                        start_pos,
                    ))
                }
            }
            Some(_) => unreachable!("only string token types reach here"),
        }
    }

    /// Reinterpret a parsed literal's per-byte `char`s as the UTF-8 they came
    /// from, so `u"..."` and `U"..."` see code points rather than bytes.
    fn literal_bytes_as_text(bytes: &str) -> String {
        let raw: Vec<u8> = bytes.chars().map(|c| c as u32 as u8).collect();
        String::from_utf8_lossy(&raw).into_owned()
    }

    /// Check a call against the callee's prototype (C99 6.5.2.2p2).
    ///
    /// Done at parse time rather than in the linearizer: the same `TypeId` is
    /// available here, but the positions are far better — `call_pos` and every
    /// argument's own position are live, whereas by linearization the only
    /// position left points at whichever sub-expression was lowered last.
    fn check_call_arity(&self, callee: &Expr, args: &[Expr], call_pos: Position) {
        // Resolve through a function pointer, as the return-type logic does.
        let func_type = callee.typ.and_then(|t| match self.types.kind(t) {
            TypeKind::Function => Some(t),
            TypeKind::Pointer => self
                .types
                .base_type(t)
                .filter(|&b| self.types.kind(b) == TypeKind::Function),
            _ => None,
        });
        let Some(func_type) = func_type else { return };

        // `params == None` means no prototype is visible: either an
        // unprototyped K&R declaration, where no check is permitted, or an
        // undeclared callee, which already produced its own diagnostic and
        // carries a dummy `int` type.
        let ft = self.types.get(func_type);
        let Some(params) = ft.params.as_ref() else {
            return;
        };
        let required = params.len();

        let variadic = ft.variadic;

        // `int f(void)` and `int f()` both intern as an empty parameter list,
        // but they mean opposite things: the first takes no arguments, the
        // second leaves them unspecified and accepts any number. Telling them
        // apart needs a "no prototype" marker on the function type, which the
        // ABI, inliner and linearizer all read — too much reach for one
        // diagnostic. So the zero-parameter case is skipped, and
        // `int f(void); f(1);` goes undiagnosed. Every prototype with at least
        // one parameter is checked.
        if required == 0 && !variadic {
            return;
        }
        let wrong = if variadic {
            args.len() < required
        } else {
            args.len() != required
        };
        if !wrong {
            return;
        }

        let expected = if variadic {
            format!("at least {}", required)
        } else {
            required.to_string()
        };
        // The singular/plural split is baked into the English sentence, so no
        // amount of substitution fixes it from outside -- both forms have to be
        // msgids.
        diag::error_plural(
            call_pos,
            "call expects {0} argument, but {1} given",
            "call expects {0} arguments, but {1} given",
            required,
            &[&expected, &args.len().to_string()],
        );
    }

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
                        diag::error(pos, &gettext("assignment of read-only location"));
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
                diag::error_args(
                    pos,
                    "assignment of read-only variable{0}",
                    &[&var_name.to_string()],
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

        if left_kind == TypeKind::Float128 || right_kind == TypeKind::Float128 {
            // binary128 outranks every other real type: it is wider than x87
            // extended in the significand, and equal to it in range.
            if is_complex {
                self.types.complex_float128_id
            } else {
                self.types.float128_id
            }
        } else if left_kind == TypeKind::LongDouble || right_kind == TypeKind::LongDouble {
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
        } else if left_kind == TypeKind::Int128 || right_kind == TypeKind::Int128 {
            if self.types.is_unsigned(left) || self.types.is_unsigned(right) {
                self.types.uint128_id
            } else {
                self.types.int128_id
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

    /// Parse a C11 generic selection (C17 6.5.1.1):
    ///
    /// ```text
    /// generic-selection:
    ///     _Generic ( assignment-expression , generic-assoc-list )
    /// generic-association:
    ///     type-name : assignment-expression
    ///     default : assignment-expression
    /// ```
    ///
    /// The selection is resolved here, at parse time, and the chosen
    /// association's expression is returned directly. No AST node is
    /// introduced. This follows `__builtin_types_compatible_p`, which likewise
    /// folds during parsing, and it is possible because this parser resolves
    /// types as it goes: the controlling expression already carries a `TypeId`
    /// by the time the associations are read.
    ///
    /// Folding also means a `_Generic` whose selected arm is an integer
    /// constant expression *is* one, which `_Static_assert` and `case` need,
    /// and it keeps `cflow`/`cxref` -- whose visitors have catch-all arms --
    /// seeing the real expression rather than silently skipping a node they do
    /// not know.
    ///
    /// The controlling expression is parsed but never evaluated (6.5.1.1p2);
    /// returning only the selected arm is what makes that true of the
    /// unselected arms as well.
    fn parse_generic_selection(&mut self, token_pos: Position) -> ParseResult<Expr> {
        self.expect_special(b'(')?;

        // The controlling expression contributes only its type, after lvalue
        // conversion: array-to-pointer, function-to-pointer, and every
        // top-level qualifier removed.
        let controlling = self.parse_assignment_expr()?;
        let controlling_typ = controlling.typ.unwrap_or(self.types.int_id);
        let selector = self.lvalue_converted_type(controlling_typ);

        self.expect_special(b',')?;

        let mut selected: Option<Expr> = None;
        let mut default_expr: Option<Expr> = None;
        let mut default_pos: Option<Position> = None;
        // Association types seen so far, for the "no two compatible" check.
        let mut seen: Vec<(TypeId, Position)> = Vec::new();

        loop {
            let assoc_pos = self.current_pos();

            let is_default = self.peek() == TokenType::Ident
                && self.get_ident_id(self.current()) == Some(crate::kw::DEFAULT);

            if is_default {
                self.advance();
                self.expect_special(b':')?;
                let expr = self.parse_assignment_expr()?;

                if default_pos.is_some() {
                    diag::error(
                        assoc_pos,
                        &gettext("_Generic selection has more than one 'default' association"),
                    );
                } else {
                    default_pos = Some(assoc_pos);
                    default_expr = Some(expr);
                }
            } else {
                let assoc_typ = self.parse_type_name()?;
                self.expect_special(b':')?;
                let expr = self.parse_assignment_expr()?;

                // 6.5.1.1p2: no two associations may name compatible types.
                // The comparison is qualifier-sensitive, so `int` and
                // `const int` may coexist -- they are not compatible types.
                if let Some((_, prev)) = seen.iter().find(|(seen_typ, _)| {
                    self.types.types_compatible_qualified(*seen_typ, assoc_typ)
                }) {
                    let _ = prev;
                    diag::error_args(
                        assoc_pos,
                        "_Generic selection has two associations with compatible type '{0}'",
                        &[&self.types.get(assoc_typ).to_string()],
                    );
                } else {
                    seen.push((assoc_typ, assoc_pos));
                }

                if self.types.types_compatible_qualified(selector, assoc_typ) && selected.is_none()
                {
                    selected = Some(expr);
                }
            }

            if self.is_special(b',') {
                self.advance();
                continue;
            }
            break;
        }

        self.expect_special(b')')?;

        match selected.or(default_expr) {
            Some(expr) => Ok(expr),
            None => {
                diag::error_args(
                    token_pos,
                    "_Generic selector of type '{0}' is not compatible with any association",
                    &[&self.types.get(selector).to_string()],
                );
                // Recover with a typed zero so one bad selection does not
                // cascade through the rest of the expression.
                Ok(Self::typed_expr(
                    ExprKind::IntLit(0),
                    self.types.int_id,
                    token_pos,
                ))
            }
        }
    }

    /// Whether a declaration in scope displaces the builtin meaning the parser
    /// would otherwise give `name_id`.
    ///
    /// `offsetof`, `alignof`, `setjmp` and `longjmp` are not C17 keywords --
    /// the first is a macro, the second a C23 spelling, the last two ordinary
    /// library functions -- so a program may use any of them as an identifier,
    /// and gcc accepts that. Recognising them ahead of ordinary lookup made
    /// them impossible to *use*: `int offsetof; offsetof = 1;` declared fine
    /// and then reported "expected '('".
    ///
    /// `setjmp` and `longjmp` are the exception, and only against a *function*
    /// declaration: `<setjmp.h>` declares exactly those, and they need code
    /// generation an ordinary call cannot produce. A declaration of any other
    /// kind -- a variable, a parameter, a typedef -- is unambiguously not that
    /// function.
    ///
    /// The reserved spellings (`__builtin_*`, `_Alignof`, `__alignof__`) are
    /// never displaced: C17 7.1.3 reserves them to the implementation in every
    /// scope, so a program that declares one has no claim on the name.
    fn builtin_is_shadowed(&self, name_id: StringId) -> bool {
        let shadowed_by_any_decl = matches!(name_id, crate::kw::OFFSETOF | crate::kw::ALIGNOF_C23);
        let shadowable = shadowed_by_any_decl
            || matches!(
                name_id,
                crate::kw::SETJMP | crate::kw::SETJMP2 | crate::kw::LONGJMP | crate::kw::LONGJMP2
            );
        if !shadowable {
            return false;
        }

        let Some(symbol_id) = self.symbols.lookup_id(name_id, Namespace::Ordinary) else {
            return false;
        };
        shadowed_by_any_decl
            || self.types.kind(self.symbols.get(symbol_id).typ) != TypeKind::Function
    }

    /// Try to parse a builtin function expression.
    /// Returns `Some(result)` if `name_id` is a recognized builtin, `None` otherwise.
    fn parse_builtin_expr(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            crate::kw::BUILTIN_VA_START => Some((|| {
                // __builtin_va_start(ap, last_param)
                self.expect_special(b'(')?;
                let ap = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                // Second arg is a parameter name
                let last_param = self.expect_identifier()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::VaStart {
                        ap: Box::new(ap),
                        last_param,
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::GENERIC => Some(self.parse_generic_selection(token_pos)),
            crate::kw::BUILTIN_VA_ARG => Some((|| {
                // __builtin_va_arg(ap, type)
                self.expect_special(b'(')?;
                let ap = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                // Second arg is a type
                let arg_type = self.parse_type_name()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::VaArg {
                        ap: Box::new(ap),
                        arg_type,
                    },
                    arg_type,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_VA_END => Some((|| {
                // __builtin_va_end(ap)
                self.expect_special(b'(')?;
                let ap = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::VaEnd { ap: Box::new(ap) },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_VA_COPY => Some((|| {
                // __builtin_va_copy(dest, src)
                self.expect_special(b'(')?;
                let dest = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let src = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::VaCopy {
                        dest: Box::new(dest),
                        src: Box::new(src),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_BSWAP16 => Some((|| {
                // __builtin_bswap16(x) - returns uint16_t
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Bswap16 { arg: Box::new(arg) },
                    self.types.ushort_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_BSWAP32 => Some((|| {
                // __builtin_bswap32(x) - returns uint32_t
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Bswap32 { arg: Box::new(arg) },
                    self.types.uint_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_BSWAP64 => Some((|| {
                // __builtin_bswap64(x) - returns uint64_t
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Bswap64 { arg: Box::new(arg) },
                    self.types.ulonglong_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CTZ => Some((|| {
                // __builtin_ctz(x) - returns int, counts trailing zeros in unsigned int
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Ctz { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CTZL => Some((|| {
                // __builtin_ctzl(x) - returns int, counts trailing zeros in unsigned long
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Ctzl { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CTZLL => Some((|| {
                // __builtin_ctzll(x) - returns int, counts trailing zeros in unsigned long long
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Ctzll { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLZ => Some((|| {
                // __builtin_clz(x) - returns int, counts leading zeros in unsigned int
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Clz { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLZL => Some((|| {
                // __builtin_clzl(x) - returns int, counts leading zeros in unsigned long
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Clzl { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLZLL => Some((|| {
                // __builtin_clzll(x) - returns int, counts leading zeros in unsigned long long
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Clzll { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_POPCOUNT => Some((|| {
                // __builtin_popcount(x) - returns int, counts set bits in unsigned int
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Popcount { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_POPCOUNTL => Some((|| {
                // __builtin_popcountl(x) - returns int, counts set bits in unsigned long
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Popcountl { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_POPCOUNTLL => Some((|| {
                // __builtin_popcountll(x) - returns int, counts set bits in unsigned long long
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Popcountll { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_ALLOCA => Some((|| {
                // __builtin_alloca(size) - returns void*
                self.expect_special(b'(')?;
                let size = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Alloca {
                        size: Box::new(size),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            // Memory builtins - generate calls to C library functions
            crate::kw::BUILTIN_MEMSET => Some((|| {
                // __builtin_memset(dest, c, n) - returns void*
                self.expect_special(b'(')?;
                let dest = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let c = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let n = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Memset {
                        dest: Box::new(dest),
                        c: Box::new(c),
                        n: Box::new(n),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_MEMCPY => Some((|| {
                // __builtin_memcpy(dest, src, n) - returns void*
                self.expect_special(b'(')?;
                let dest = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let src = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let n = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Memcpy {
                        dest: Box::new(dest),
                        src: Box::new(src),
                        n: Box::new(n),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_MEMMOVE => Some((|| {
                // __builtin_memmove(dest, src, n) - returns void*
                self.expect_special(b'(')?;
                let dest = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let src = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let n = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Memmove {
                        dest: Box::new(dest),
                        src: Box::new(src),
                        n: Box::new(n),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            // Infinity builtins - return float constants
            crate::kw::BUILTIN_INF | crate::kw::BUILTIN_HUGE_VAL => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::infinity(false)),
                    self.types.double_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_INFF | crate::kw::BUILTIN_HUGE_VALF => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::infinity(false)),
                    self.types.float_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_INFL | crate::kw::BUILTIN_HUGE_VALL => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::infinity(false)),
                    self.types.longdouble_id,
                    token_pos,
                ))
            })()),
            // NaN builtins - returns quiet NaN
            // The string argument is typically empty "" for quiet NaN
            crate::kw::BUILTIN_NAN | crate::kw::BUILTIN_NANS => Some((|| {
                self.expect_special(b'(')?;
                let _arg = self.parse_assignment_expr()?; // string argument (ignored)
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::nan()),
                    self.types.double_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_NANF | crate::kw::BUILTIN_NANSF => Some((|| {
                self.expect_special(b'(')?;
                let _arg = self.parse_assignment_expr()?; // string argument (ignored)
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::nan()),
                    self.types.float_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_NANL | crate::kw::BUILTIN_NANSL => Some((|| {
                self.expect_special(b'(')?;
                let _arg = self.parse_assignment_expr()?; // string argument (ignored)
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::nan()),
                    self.types.longdouble_id,
                    token_pos,
                ))
            })()),
            // FLT_ROUNDS - returns current rounding mode (1 = to nearest)
            crate::kw::BUILTIN_FLT_ROUNDS => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::IntLit(1), // IEEE 754 default: round to nearest
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            // Fabs builtins - absolute value for floats
            crate::kw::BUILTIN_FABS => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Fabs { arg: Box::new(arg) },
                    self.types.double_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_FABSF => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Fabsf { arg: Box::new(arg) },
                    self.types.float_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_FABSL => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Fabsl { arg: Box::new(arg) },
                    self.types.longdouble_id,
                    token_pos,
                ))
            })()),
            // Signbit builtins - test sign bit of floats
            crate::kw::BUILTIN_ISNAN
            | crate::kw::BUILTIN_ISINF
            | crate::kw::BUILTIN_ISINF_SIGN
            | crate::kw::BUILTIN_ISFINITE
            | crate::kw::BUILTIN_ISNORMAL => Some((|| {
                let test = match name_id {
                    crate::kw::BUILTIN_ISNAN => FpTest::IsNan,
                    crate::kw::BUILTIN_ISINF => FpTest::IsInf,
                    crate::kw::BUILTIN_ISINF_SIGN => FpTest::IsInfSign,
                    crate::kw::BUILTIN_ISFINITE => FpTest::IsFinite,
                    _ => FpTest::IsNormal,
                };
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FpTest {
                        test,
                        arg: Box::new(arg),
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_FPCLASSIFY => Some((|| {
                // __builtin_fpclassify(nan, inf, normal, subnormal, zero, x)
                self.expect_special(b'(')?;
                let mut args = Vec::with_capacity(6);
                args.push(self.parse_assignment_expr()?);
                while self.is_special(b',') {
                    self.advance();
                    args.push(self.parse_assignment_expr()?);
                }
                self.expect_special(b')')?;
                if args.len() != 6 {
                    return Err(ParseError::new(
                        "__builtin_fpclassify expects five class codes and a value",
                        token_pos,
                    ));
                }
                let arg = args.pop().expect("checked length");
                Ok(Self::typed_expr(
                    ExprKind::FpClassify {
                        classes: args,
                        arg: Box::new(arg),
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_SIGNBIT => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Signbit { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_SIGNBITF => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Signbitf { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_SIGNBITL => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Signbitl { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_COMPLEX => Some((|| {
                // __builtin_complex(real, imag) - construct complex value
                self.expect_special(b'(')?;
                let real = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let imag = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Determine complex type from argument types
                let real_typ = real.typ.unwrap_or(self.types.double_id);
                let complex_typ = self.types.make_complex(real_typ);
                Ok(Self::typed_expr(
                    ExprKind::BuiltinComplex {
                        real: Box::new(real),
                        imag: Box::new(imag),
                    },
                    complex_typ,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_UNREACHABLE => Some((|| {
                // __builtin_unreachable() - marks code as unreachable
                // Takes no arguments, returns void
                // Behavior is undefined if actually reached at runtime
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Unreachable,
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CONSTANT_P => Some((|| {
                // __builtin_constant_p(expr) - returns 1 if expr is a constant, 0 otherwise
                // This is evaluated at compile time, not runtime
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Check if the argument is a constant expression
                let is_constant = self.eval_const_expr(&arg).is_some();
                Ok(Self::typed_expr(
                    ExprKind::IntLit(if is_constant { 1 } else { 0 }),
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_EXPECT => Some((|| {
                // __builtin_expect(expr, c) - branch prediction hint
                // Returns expr, the second argument is the expected value (for optimization hints)
                // We just return expr since we don't do branch prediction optimization
                self.expect_special(b'(')?;
                let expr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let _expected = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(expr)
            })()),
            crate::kw::BUILTIN_ASSUME_ALIGNED => Some((|| {
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
                Ok(ptr)
            })()),
            crate::kw::BUILTIN_PREFETCH => Some((|| {
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
                Ok(Self::typed_expr(
                    ExprKind::IntLit(0),
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_TYPES_COMPATIBLE_P => Some((|| {
                // __builtin_types_compatible_p(type1, type2) - returns 1 if types are compatible
                // This is evaluated at compile time, ignoring top-level qualifiers
                self.expect_special(b'(')?;
                let type1 = self.parse_type_name()?;
                self.expect_special(b',')?;
                let type2 = self.parse_type_name()?;
                self.expect_special(b')')?;
                // Check type compatibility (ignoring qualifiers)
                let compatible = self.types.types_compatible(type1, type2);
                Ok(Self::typed_expr(
                    ExprKind::IntLit(if compatible { 1 } else { 0 }),
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_FRAME_ADDRESS => Some((|| {
                // __builtin_frame_address(level) - returns void*, address of frame at level
                // Level 0 is the current frame, 1 is the caller's frame, etc.
                // Returns NULL for invalid levels (beyond stack bounds)
                self.expect_special(b'(')?;
                let level = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FrameAddress {
                        level: Box::new(level),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_RETURN_ADDRESS => Some((|| {
                // __builtin_return_address(level) - returns void*, return address at level
                // Level 0 is the current function's return address
                // Returns NULL for invalid levels (beyond stack bounds)
                self.expect_special(b'(')?;
                let level = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::ReturnAddress {
                        level: Box::new(level),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::SETJMP | crate::kw::SETJMP2 => Some((|| {
                // setjmp(env) - saves execution context, returns int
                // Returns 0 on direct call, non-zero when returning via longjmp
                self.expect_special(b'(')?;
                let env = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Setjmp { env: Box::new(env) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::LONGJMP | crate::kw::LONGJMP2 => Some((|| {
                // longjmp(env, val) - restores execution context (never returns)
                // Causes corresponding setjmp to return val (or 1 if val == 0)
                self.expect_special(b'(')?;
                let env = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Longjmp {
                        env: Box::new(env),
                        val: Box::new(val),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_OFFSETOF | crate::kw::OFFSETOF => Some((|| {
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
                        let index_val = self.eval_const_expr(&index_expr).ok_or_else(|| {
                            ParseError::new(
                                "array index in offsetof must be a constant expression",
                                index_pos,
                            )
                        })?;
                        path.push(OffsetOfPath::Index(index_val as i64));
                    } else {
                        break;
                    }
                }
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::OffsetOf { type_id, path },
                    self.types.ulong_id, // size_t is typically unsigned long
                    token_pos,
                ))
            })()),
            // ================================================================
            // Atomic builtins (Clang __c11_atomic_* for C11 stdatomic.h)
            // ================================================================
            crate::kw::C11_ATOMIC_INIT => Some((|| {
                // __c11_atomic_init(ptr, val) - initialize atomic (no ordering)
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicInit {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_LOAD => Some((|| {
                // __c11_atomic_load(ptr, order) - returns *ptr atomically
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Result type is the pointed-to type
                let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicLoad {
                        ptr: Box::new(ptr),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_STORE => Some((|| {
                // __c11_atomic_store(ptr, val, order) - *ptr = val atomically
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicStore {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_EXCHANGE => Some((|| {
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
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicExchange {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_COMPARE_EXCHANGE_STRONG => Some((|| {
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
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicCompareExchangeStrong {
                        ptr: Box::new(ptr),
                        expected: Box::new(expected),
                        desired: Box::new(desired),
                        succ_order: Box::new(succ_order),
                    },
                    self.types.bool_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_COMPARE_EXCHANGE_WEAK => Some((|| {
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
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicCompareExchangeWeak {
                        ptr: Box::new(ptr),
                        expected: Box::new(expected),
                        desired: Box::new(desired),
                        succ_order: Box::new(succ_order),
                    },
                    self.types.bool_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_ADD => Some((|| {
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
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicFetchAdd {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_SUB => Some((|| {
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
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicFetchSub {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_AND => Some((|| {
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
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicFetchAnd {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_OR => Some((|| {
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
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicFetchOr {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_XOR => Some((|| {
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
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicFetchXor {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_THREAD_FENCE => Some((|| {
                // __c11_atomic_thread_fence(order) - memory fence
                self.expect_special(b'(')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicThreadFence {
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_SIGNAL_FENCE => Some((|| {
                // __c11_atomic_signal_fence(order) - compiler barrier
                self.expect_special(b'(')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicSignalFence {
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_OBJECT_SIZE => Some((|| {
                // __builtin_object_size(ptr, type): how many bytes remain in
                // the object `ptr` points into, when that is known statically.
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let otype = self.parse_assignment_expr()?;
                self.expect_special(b')')?;

                // The type argument must be an integer constant 0..3. Bit 0
                // selects the closest surrounding subobject over the whole
                // object; bit 1 asks for a minimum rather than a maximum.
                let otype = self.eval_const_expr(&otype).unwrap_or(0).clamp(0, 3) as u32;

                let size = match self.object_extent(&ptr) {
                    // A statically known object has the same minimum and
                    // maximum size, so bit 1 does not change the answer.
                    Some(extent) => extent.remaining(otype & 1 != 0),
                    // Unknown: the documented answers are the ones that make a
                    // `_FORTIFY_SOURCE` check pass rather than fire, which is
                    // the largest value for a maximum and zero for a minimum.
                    None if otype & 2 != 0 => 0,
                    None => u64::MAX,
                };

                Ok(Self::typed_expr(
                    ExprKind::IntLit(size as i64),
                    self.types.ulong_id,
                    token_pos,
                ))
            })()),
            _ => {
                let name_str = self.idents.get_opt(name_id).unwrap_or("");
                if name_str.starts_with("__builtin___") {
                    Some((|| {
                        // Fortified builtins: __builtin___snprintf_chk etc.
                        // Strip __builtin_ prefix → __snprintf_chk, which is a
                        // real libc function (declared by macOS/glibc headers).
                        let real_name = &name_str["__builtin_".len()..];
                        // Parse arguments first (must consume tokens regardless)
                        self.expect_special(b'(')?;
                        let mut args = Vec::new();
                        if !self.is_special(b')') {
                            args.push(self.parse_assignment_expr()?);
                            while self.is_special(b',') {
                                self.advance();
                                args.push(self.parse_assignment_expr()?);
                            }
                        }
                        self.expect_special(b')')?;
                        // Look up the real function by its de-prefixed name
                        let real_name_id = self.idents.lookup(real_name);
                        let symbol_id = real_name_id.and_then(|id| {
                            self.symbols
                                .lookup_id(id, crate::symbol::Namespace::Ordinary)
                        });
                        if let Some(symbol_id) = symbol_id {
                            let func_type = self.symbols.get(symbol_id).typ;
                            let ret_type =
                                self.types.base_type(func_type).unwrap_or(self.types.int_id);
                            let func_expr =
                                Self::typed_expr(ExprKind::Ident(symbol_id), func_type, token_pos);
                            return Ok(Self::typed_expr(
                                ExprKind::Call {
                                    func: Box::new(func_expr),
                                    args,
                                },
                                ret_type,
                                token_pos,
                            ));
                        }
                        // Not declared. gcc knows these intrinsically and
                        // glibc relies on that: `bits/string_fortified.h`
                        // calls `__builtin___memcpy_chk` without ever
                        // declaring `__memcpy_chk`. Synthesize the
                        // declaration rather than failing.
                        if let Some(symbol_id) = self
                            .chk_builtin_return_type(real_name)
                            .and_then(|ret| self.declare_chk_builtin(real_name, ret))
                        {
                            let ret_type = self
                                .types
                                .base_type(self.symbols.get(symbol_id).typ)
                                .unwrap_or(self.types.int_id);
                            let func_type = self.symbols.get(symbol_id).typ;
                            let func_expr =
                                Self::typed_expr(ExprKind::Ident(symbol_id), func_type, token_pos);
                            return Ok(Self::typed_expr(
                                ExprKind::Call {
                                    func: Box::new(func_expr),
                                    args,
                                },
                                ret_type,
                                token_pos,
                            ));
                        }
                        diag::error_args(token_pos, "undeclared function '{0}'", &[real_name]);
                        Ok(Self::typed_expr(
                            ExprKind::IntLit(0),
                            self.types.int_id,
                            token_pos,
                        ))
                    })())
                } else {
                    None
                }
            }
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

                    // Try builtin dispatch first, unless a declaration in scope
                    // has claimed the name (see `builtin_is_shadowed`).
                    if !self.builtin_is_shadowed(name_id) {
                        if let Some(result) = self.parse_builtin_expr(name_id, token_pos) {
                            return result;
                        }
                    }

                    // Look up symbol to get type (during parsing, symbol is in scope)
                    // C99 6.4.2.2: __func__ is a predefined identifier with type const char[]
                    // GCC extensions: __FUNCTION__ and __PRETTY_FUNCTION__ behave similarly
                    if name_id == crate::kw::FUNC
                        || name_id == crate::kw::FUNCTION
                        || name_id == crate::kw::PRETTY_FUNCTION
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
                        let name_str = self.idents.get_opt(name_id).unwrap_or("");
                        diag::error_args(token_pos, "undeclared identifier '{0}'", &[name_str]);
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

            // A prefixed character constant differs from a narrow one only in
            // its type: wchar_t, char16_t or char32_t. The value is the code
            // point either way.
            TokenType::WideChar | TokenType::Utf16Char | TokenType::Utf32Char => {
                let kind = self.peek();
                let token = self.consume();
                let token_pos = token.pos;
                match &token.value {
                    TokenValue::WideChar(s)
                    | TokenValue::Utf16Char(s)
                    | TokenValue::Utf32Char(s) => {
                        let c = self.parse_char_literal(s);
                        let typ = match kind {
                            // wchar_t is int on the targets here.
                            TokenType::WideChar => self.types.int_id,
                            TokenType::Utf16Char => self.types.ushort_id,
                            _ => self.types.uint_id,
                        };
                        Ok(Self::typed_expr(ExprKind::CharLit(c), typ, token_pos))
                    }
                    _ => Err(ParseError::new("invalid character token", token.pos)),
                }
            }

            // All string literal encodings share one arm, because adjacent
            // literals of *different* encodings concatenate (C11 6.4.5p5) and
            // the two separate loops this replaces could each only see their
            // own kind — so `"a" L"b"` left `L"b"` unconsumed and became a
            // syntax error with no useful diagnostic.
            TokenType::String
            | TokenType::WideString
            | TokenType::Utf16String
            | TokenType::Utf32String => self.parse_string_literal_run(),

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

                            // An incomplete array type takes its size from the
                            // initializer. `parse_declarator` spells "no size
                            // given" as `None`, which is what `int a[]` means;
                            // the type-name parser this replaced spelled it
                            // `Some(0)`, conflating it with the GNU zero-length
                            // array. Accept both, since the declaration path
                            // (`infer_array_size_from_init`) also does.
                            let final_typ = if self.types.kind(typ) == TypeKind::Array
                                && matches!(self.types.get(typ).array_size, None | Some(0))
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

                        // Fold cast-to-Int128 of constant expressions into Int128Lit
                        if self.types.kind(typ) == TypeKind::Int128 {
                            if let Some(val) = self.eval_const_expr(&expr) {
                                return Ok(Self::typed_expr(
                                    ExprKind::Int128Lit(val),
                                    typ,
                                    paren_pos,
                                ));
                            }
                        }

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
        // `q` is GCC's binary128 suffix, and the one glibc's `__f128(x)` pastes
        // on. Unlike `f16`/`f32`/`f64` it is safe on a hex literal too, since
        // `q` is not a hex digit. It is a *floating* suffix: gcc rejects
        // `1q` with "invalid suffix on integer constant", and accepting it
        // silently reinterpreted an integer as a binary128.
        let is_quad_suffix = is_float && s_lower.ends_with('q');
        // The `f128` spelling, which is valid on a hex literal too -- after a
        // `p` exponent it cannot be mistaken for hex digits. Like `q` it is a
        // *floating* suffix: `1f128` is an integer constant with a bad suffix,
        // and `0x1f128` is a hex integer whose last five digits merely spell
        // one.
        let is_float128_suffix = is_float && s_lower.ends_with("f128");

        // Remove suffixes - but for hex numbers, don't strip a-f as they're digits
        let num_str = if is_hex && is_float && is_float128_suffix {
            s_lower.trim_end_matches("f128")
        } else if is_hex && is_float {
            // Hex float: strip f/l/q suffixes (they appear after p-exponent, not as hex digits)
            s_lower.trim_end_matches(['u', 'l', 'f', 'q'])
        } else if is_hex {
            // Hex integer: only strip u/l (f is a hex digit, and `q` is a
            // floating suffix -- `0x1q` is not a number)
            s_lower.trim_end_matches(['u', 'l'])
        } else if is_float128_suffix {
            s_lower.trim_end_matches("f128")
        } else if is_float16_suffix {
            s_lower.trim_end_matches("f16")
        } else if is_float32_suffix {
            s_lower.trim_end_matches("f32")
        } else if is_float64_suffix {
            s_lower.trim_end_matches("f64")
        } else if is_quad_suffix {
            s_lower.trim_end_matches('q')
        } else {
            // For decimal/octal, strip u/l/f suffixes. Not `q`: it is a
            // floating suffix, so `1q` must fail to parse rather than
            // quietly becoming the integer 1.
            s_lower.trim_end_matches(['u', 'l', 'f'])
        };

        if is_float
            || is_float16_suffix
            || is_float32_suffix
            || is_float64_suffix
            || is_float128_suffix
            || is_quad_suffix
        {
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
                && !is_float128_suffix
                && s_lower.ends_with('f');
            let is_longdouble_suffix = !is_float16_suffix
                && !is_float32_suffix
                && !is_float64_suffix
                && !is_float128_suffix
                && s_lower.ends_with('l');
            let value: FloatVal = if is_hex {
                // Hex float parsing: 0x[hex-digits].[hex-digits]p[±exponent]
                // Value = significand × 2^exponent.
                // `parse_hex_float_parts` is exact, so the literal reaches the
                // target format without passing through `f64` -- which would
                // flush `0x1p-16382L` to zero before its type is even known.
                let (mantissa, exp2) = Self::parse_hex_float_parts(num_str).map_err(|_| {
                    ParseError::new(format!("invalid hex float literal: {}", s), pos)
                })?;
                FloatVal::from_parts(false, mantissa, exp2)
            } else {
                // Exact, like the hex path: the digits are scaled by a power
                // of ten in full precision and only rounded once, at the
                // target's width. Going through `f64` cost a `long double`
                // eleven of its significand bits and flushed anything outside
                // double's range before the type was even known.
                let (mantissa, exp2) = crate::float::parse_decimal_float_parts(num_str)
                    .map_err(|_| ParseError::new(format!("invalid float literal: {}", s), pos))?;
                FloatVal::from_parts(false, mantissa, exp2)
            };
            let typ = if is_float128_suffix || is_quad_suffix {
                if !self.types.has_float128() {
                    return Err(ParseError::new(
                        format!("__float128 is not supported on this target: {}", s),
                        pos,
                    ));
                }
                self.types.float128_id
            } else if is_float16_suffix {
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
    /// Decompose a hex float literal into an exact `(mantissa, exp2)` pair,
    /// where the value is `mantissa * 2^exp2` with `mantissa` an integer.
    ///
    /// C99 6.4.4.2 hex floats name a binary value directly, so this is exact
    /// -- no decimal rounding is involved -- for any literal whose significand
    /// fits 128 bits. Beyond that the tail is folded into a sticky low bit,
    /// which is enough to round correctly at every width we support.
    ///
    /// Returned separately from [`parse_hex_float`] so a wider target format
    /// can use the full significand rather than whatever survived an `f64`.
    fn parse_hex_float_parts(s: &str) -> Result<(u128, i32), ()> {
        let s = s
            .strip_prefix("0x")
            .or_else(|| s.strip_prefix("0X"))
            .ok_or(())?;

        let p_pos = s.find(['p', 'P']).ok_or(())?;
        let (mantissa_str, exp_str) = s.split_at(p_pos);
        let exponent: i32 = exp_str[1..].parse().map_err(|_| ())?;

        let (int_part, frac_part) = match mantissa_str.find('.') {
            Some(dot) => (&mantissa_str[..dot], &mantissa_str[dot + 1..]),
            None => (mantissa_str, ""),
        };
        if int_part.is_empty() && frac_part.is_empty() {
            return Err(());
        }

        // Accumulate the significand as an integer, remembering how far the
        // radix point moved. A u128 holds 32 hex digits; the previous code
        // used a u64 and shifted by `4 * digits`, so a 16-digit fraction
        // shifted by 64 -- which wraps to a shift of 0 in release, turning
        // `0x1.0000000000000002p0` into 3.0 rather than a value near 1.
        let mut mantissa: u128 = 0;
        let mut exp2 = exponent;
        let mut sticky = false;
        let mut seen_digit = false;

        for (i, c) in int_part.chars().chain(frac_part.chars()).enumerate() {
            let d = c.to_digit(16).ok_or(())? as u128;
            let in_fraction = i >= int_part.chars().count();

            if mantissa.leading_zeros() >= 4 {
                mantissa = (mantissa << 4) | d;
                if in_fraction {
                    exp2 -= 4;
                }
            } else {
                // No room left: the digit only contributes to rounding. An
                // integer digit still scales the value.
                sticky |= d != 0;
                if !in_fraction {
                    exp2 += 4;
                }
            }
            seen_digit = true;
        }
        if !seen_digit {
            return Err(());
        }

        if sticky {
            mantissa |= 1;
        }
        Ok((mantissa, exp2))
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

    /// What is statically known about the object a pointer expression
    /// designates, for `__builtin_object_size`.
    ///
    /// Tracks both the whole object and the innermost aggregate containing the
    /// designated byte, because the builtin's type argument selects between
    /// them: `__builtin_object_size(s.arr, 0)` is everything left in `s`,
    /// while type 1 is everything left in `arr`.
    fn object_extent(&self, expr: &Expr) -> Option<ObjectExtent> {
        match &expr.kind {
            // An array name decays to a pointer to its first element, so it
            // designates the array itself. A pointer *variable* designates
            // whatever it was assigned, which is exactly what is not known.
            ExprKind::Ident(id) => {
                let typ = self.symbols.get(*id).typ;
                if self.types.kind(typ) != TypeKind::Array {
                    return None;
                }
                ObjectExtent::whole_of(self.byte_size(typ)?)
            }

            // A string literal is an array of its bytes plus the terminator.
            ExprKind::StringLit(s) => ObjectExtent::whole_of(s.chars().count() as u64 + 1),

            // `&lvalue` designates the lvalue, which may be a subobject.
            ExprKind::Unary {
                op: UnaryOp::AddrOf,
                operand,
            } => self.lvalue_extent(operand),

            // A cast does not move the pointer.
            ExprKind::Cast { expr, .. } => self.object_extent(expr),

            // Pointer arithmetic with a constant displacement.
            ExprKind::Binary {
                op: op @ (BinaryOp::Add | BinaryOp::Sub),
                left,
                right,
            } => {
                let base = self.object_extent(left)?;
                let elem = self.pointee_size(left)?;
                let n = self.eval_const_expr(right)?;
                let bytes = i128::from(elem).checked_mul(n)?;
                base.advance(if *op == BinaryOp::Sub { -bytes } else { bytes })
            }

            // Everything else -- a call, a dereference, an unknown pointer.
            _ => self.object_extent_of_lvalue_forms(expr),
        }
    }

    /// The extent of an lvalue: the object it names, and where inside its
    /// enclosing object it sits.
    fn lvalue_extent(&self, expr: &Expr) -> Option<ObjectExtent> {
        match &expr.kind {
            ExprKind::Ident(id) => {
                ObjectExtent::whole_of(self.byte_size(self.symbols.get(*id).typ)?)
            }

            ExprKind::Member { expr: base, member } => {
                let base_typ = self.lvalue_type(base)?;
                let info = self.types.find_member(base_typ, *member)?;
                let outer = self.lvalue_extent(base)?;
                outer.narrow(info.offset as u64, self.byte_size(info.typ)?)
            }

            // `a[i]` with a constant index, where `a` is an array we can see.
            ExprKind::Index { array, index } => {
                let n = self.eval_const_expr(index)?;
                let outer = self.object_extent(array)?;
                let elem = self.pointee_size(array)?;
                outer.advance(i128::from(elem).checked_mul(n)?)
            }

            _ => None,
        }
    }

    /// `Member` and `Index` reached without an `&`, i.e. an array-typed
    /// subobject that decayed to a pointer.
    fn object_extent_of_lvalue_forms(&self, expr: &Expr) -> Option<ObjectExtent> {
        match &expr.kind {
            ExprKind::Member { .. } | ExprKind::Index { .. } => {
                let typ = self.lvalue_type(expr)?;
                if self.types.kind(typ) != TypeKind::Array {
                    return None;
                }
                self.lvalue_extent(expr)
            }
            _ => None,
        }
    }

    /// The declared type of an lvalue expression, as far as it can be resolved
    /// from symbols and member lookups alone.
    fn lvalue_type(&self, expr: &Expr) -> Option<TypeId> {
        match &expr.kind {
            ExprKind::Ident(id) => Some(self.symbols.get(*id).typ),
            ExprKind::Member { expr, member } => {
                let base = self.lvalue_type(expr)?;
                Some(self.types.find_member(base, *member)?.typ)
            }
            ExprKind::Index { array, .. } => {
                let base = self.lvalue_type(array)?;
                self.types.get(base).base
            }
            ExprKind::Cast { cast_type, .. } => Some(*cast_type),
            _ => expr.typ,
        }
    }

    /// Size in bytes of a type whose size is known and non-zero.
    fn byte_size(&self, typ: TypeId) -> Option<u64> {
        let bits = self.types.size_bits(typ);
        if bits == 0 {
            return None;
        }
        Some(u64::from(bits) / 8)
    }

    /// Size of what `expr` points at, for scaling pointer arithmetic.
    fn pointee_size(&self, expr: &Expr) -> Option<u64> {
        let typ = self.lvalue_type(expr)?;
        let elem = self.types.get(typ).base?;
        self.byte_size(elem)
    }
    /// The return type of a `_chk` fortified libc entry point, if `name` is
    /// one c17 knows.
    ///
    /// The type matters more than it looks: these mostly return a pointer, and
    /// declaring one of them `int` truncates the returned address to 32 bits.
    /// `None` means "not a known `_chk` function", which stays an error.
    fn chk_builtin_return_type(&mut self, name: &str) -> Option<TypeId> {
        // The string family returns `char *`; the memory family returns
        // `void *`; the printf family returns `int`.
        match name {
            "__memcpy_chk" | "__memmove_chk" | "__mempcpy_chk" | "__memset_chk" => {
                Some(self.types.void_ptr_id)
            }
            "__strcpy_chk" | "__stpcpy_chk" | "__strncpy_chk" | "__stpncpy_chk"
            | "__strcat_chk" | "__strncat_chk" => {
                let char_id = self.types.char_id;
                Some(self.types.intern(Type {
                    kind: TypeKind::Pointer,
                    base: Some(char_id),
                    ..Default::default()
                }))
            }
            "__sprintf_chk" | "__snprintf_chk" | "__printf_chk" | "__fprintf_chk"
            | "__vsprintf_chk" | "__vsnprintf_chk" | "__vprintf_chk" | "__vfprintf_chk" => {
                Some(self.types.int_id)
            }
            _ => None,
        }
    }

    /// Declare a `_chk` entry point, so the call type-checks and returns a
    /// value of the right width.
    ///
    /// The fixed parameters are counted even though their types are not
    /// modelled: where an argument stops being fixed and starts being variadic
    /// is an ABI question, not only a type-checking one. Apple's arm64 passes
    /// every variadic argument on the stack while fixed ones stay in
    /// registers, so declaring these as variadic-from-argument-zero -- which
    /// is what an empty parameter list means -- put `__snprintf_chk`'s buffer,
    /// length, flag and size on the stack, where it does not look for them.
    fn declare_chk_builtin(&mut self, name: &str, ret_type: TypeId) -> Option<SymbolId> {
        // Pre-interned in `cc/kw.rs`; the identifier table is read-only here.
        let name_id = self.idents.lookup(name)?;

        // (fixed parameters before the `...`, whether a `...` follows). The
        // `v` forms take a `va_list` and are not variadic; the memory ones
        // take a fixed argument list outright.
        let (fixed, variadic) = match name {
            "__printf_chk" => (2, true),
            "__fprintf_chk" | "__sprintf_chk" => (3, true),
            "__snprintf_chk" => (5, true),
            "__vprintf_chk" => (3, false),
            "__vfprintf_chk" | "__vsprintf_chk" => (4, false),
            "__vsnprintf_chk" => (6, false),
            "__memset_chk" | "__strcpy_chk" | "__stpcpy_chk" | "__strcat_chk" => (3, false),
            "__memcpy_chk" | "__memmove_chk" | "__mempcpy_chk" | "__strncpy_chk"
            | "__stpncpy_chk" | "__strncat_chk" => (4, false),
            // An entry point this does not know is left as it was: variadic,
            // with nothing fixed.
            _ => (0, true),
        };
        // The types are not modelled -- only how many arguments are fixed --
        // so each is spelled as the widest integer the ABI passes in one
        // register, which a pointer, a size and a flag all classify as.
        let params = vec![self.types.ulong_id; fixed];

        let func_type = self.types.intern(Type {
            kind: TypeKind::Function,
            base: Some(ret_type),
            variadic,
            params: Some(params),
            ..Default::default()
        });
        let symbol = Symbol::function(name_id, func_type, self.symbols.depth());
        // A redeclaration can only mean the header did declare it after all,
        // in which case the existing symbol is the one to use.
        Some(self.symbols.declare(symbol).unwrap_or_else(|_| {
            self.symbols
                .lookup_id(name_id, Namespace::Ordinary)
                .expect("declare failed but no existing symbol")
        }))
    }
}

/// A statically known object, and where inside it a pointer points.
///
/// `whole` / `offset` describe the complete object; `sub` / `sub_offset`
/// describe the innermost aggregate containing the designated byte. For a
/// plain array the two coincide.
#[derive(Clone, Copy)]
pub(crate) struct ObjectExtent {
    whole: u64,
    offset: u64,
    sub: u64,
    sub_offset: u64,
}

impl ObjectExtent {
    /// A pointer to the start of an object of `size` bytes.
    fn whole_of(size: u64) -> Option<Self> {
        Some(ObjectExtent {
            whole: size,
            offset: 0,
            sub: size,
            sub_offset: 0,
        })
    }

    /// Move the pointer by `bytes`, staying inside both objects.
    fn advance(self, bytes: i128) -> Option<Self> {
        let offset = i128::from(self.offset).checked_add(bytes)?;
        let sub_offset = i128::from(self.sub_offset).checked_add(bytes)?;
        // Out of bounds either way: gcc gives up rather than reporting a size
        // that would licence an overrun.
        if offset < 0 || offset > i128::from(self.whole) {
            return None;
        }
        if sub_offset < 0 || sub_offset > i128::from(self.sub) {
            return None;
        }
        Some(ObjectExtent {
            offset: offset as u64,
            sub_offset: sub_offset as u64,
            ..self
        })
    }

    /// Step into a member at `offset` bytes with size `size`, which becomes
    /// the new innermost subobject.
    fn narrow(self, offset: u64, size: u64) -> Option<Self> {
        Some(ObjectExtent {
            whole: self.whole,
            offset: self.offset.checked_add(offset)?,
            sub: size,
            sub_offset: 0,
        })
    }

    /// Bytes from the pointer to the end of the selected object.
    fn remaining(self, closest_subobject: bool) -> u64 {
        if closest_subobject {
            self.sub.saturating_sub(self.sub_offset)
        } else {
            self.whole.saturating_sub(self.offset)
        }
    }
}
