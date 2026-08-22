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

use super::ast::{AssignOp, BinaryOp, Designator, Expr, ExprKind, InitElement, UnaryOp};
use super::parser::{ParseError, ParseResult, Parser};
use crate::diag;
use crate::float::FloatVal;
use crate::strings::StringId;
use crate::symbol::Namespace;
use crate::token::lexer::{Position, SpecialToken, TokenType, TokenValue};
use crate::token::literal;
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;

const DEFAULT_ARG_LIST_CAPACITY: usize = 8;
const DEFAULT_INIT_CAPACITY: usize = 8;

impl<'a> Parser<'a> {
    // Expression parsing, one function per precedence level: the chain below
    // runs from lowest (comma) to highest (primary), each level delegating to
    // the next.

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
            self.check_modifiable_lvalue(&left, "left operand of assignment", assign_pos);
            self.check_const_assignment(&left, assign_pos);

            // Right-to-left associativity: parse the right side as another assignment
            let right = self.parse_assignment_expr()?;
            if assign_op == AssignOp::Assign {
                self.check_assignment_types(&left, &right, assign_pos);
            }
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
                // Array index designator: `[constant-expression]`, or the GNU
                // range `[lo ... hi]`. As with a case range, GCC requires the
                // spaces: `[0...3]` lexes as one pp-number and it rejects that
                // too.
                self.advance();
                let index_expr = self.parse_conditional_expr()?;
                let index = self.eval_const_expr(&index_expr).ok_or_else(|| {
                    ParseError::new(
                        "array designator index must be constant",
                        self.current_pos(),
                    )
                })?;
                let high = if self.is_special_token(SpecialToken::Ellipsis) {
                    let pos = self.current_pos();
                    self.advance();
                    let high_expr = self.parse_conditional_expr()?;
                    let high = self.eval_const_expr(&high_expr).ok_or_else(|| {
                        ParseError::new("array designator index must be constant", pos)
                    })?;
                    Some(high as i64)
                } else {
                    None
                };
                self.expect_special(b']')?;

                let index = index as i64;
                if index < 0 {
                    return Err(ParseError::new(
                        "array index in initializer is negative",
                        self.current_pos(),
                    ));
                }
                if let Some(high) = high {
                    // GCC: "empty index range in initializer".
                    if high < index {
                        return Err(ParseError::new(
                            "empty index range in initializer",
                            self.current_pos(),
                        ));
                    }
                }
                // A range that follows a field designator -- `.m[0 ... 3] = v`
                // -- resolves through `resolve_designator_chain`, which yields
                // one offset where a range names many. The nested spelling
                // `.m = {[0 ... 3] = v}` does the same job and works.
                if high.is_some() && !designators.is_empty() {
                    return Err(ParseError::new(
                        "an index range is not supported after a field designator;                          write '.field = { [lo ... hi] = value }'",
                        self.current_pos(),
                    ));
                }
                designators.push(match high {
                    None => Designator::Index(index),
                    Some(high) => Designator::IndexRange(index, high),
                });
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

        // Both arithmetic: the usual arithmetic conversions, which C17 6.5.15p5
        // sends the arms through -- the same ones the binary operators use.
        // Delegated rather than restated, so the answer follows conversion
        // *rank* and not bit width, and does not depend on which arm was
        // written first.
        //
        // Complex is held back deliberately. The shared version answers it
        // correctly -- `c ? z : 1.0` really is `double _Complex` -- but a
        // complex conditional does not survive codegen today: the arms carry a
        // two-register value while the merge dereferences the pseudo as an
        // address, which segfaults. Widening the type here would turn "quietly
        // drops the imaginary part" into "crashes", so complex keeps its own
        // answer until that is fixed.
        let complex = self.types.is_complex(then_typ) || self.types.is_complex(else_typ);
        if !complex && self.types.is_arithmetic(then_typ) && self.types.is_arithmetic(else_typ) {
            return self.usual_arithmetic_conversions(then_typ, else_typ);
        }

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

        // Neither arithmetic nor a pointer: a struct or union, where C17
        // 6.5.15p3 has already required both arms to have the same type and
        // there is nothing to convert.
        then_typ
    }

    /// Apply the array-to-pointer and function-to-pointer decays of C17
    /// 6.3.2.1p3-4. Qualifiers are left alone.
    pub(crate) fn decayed_type(&mut self, typ: TypeId) -> TypeId {
        self.types.decayed(typ)
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

            // GNU `a ?: b`: the middle operand may be omitted, and then the
            // condition is also the value when it is true. Kept as its own
            // node rather than rewritten to `a ? a : b`, because 6.5.15 would
            // then evaluate `a` twice -- `f() ?: 0` must call `f` once.
            if self.is_special(b':') {
                self.advance();
                let else_expr = self.parse_conditional_expr()?;

                let cond_typ = cond.typ.unwrap_or(self.types.int_id);
                let else_typ = else_expr.typ.unwrap_or(self.types.int_id);
                let cond_decayed = self.decayed_type(cond_typ);
                let else_decayed = self.decayed_type(else_typ);
                let typ = self.ternary_common_type(cond_decayed, else_decayed);

                let pos = cond.pos;
                return Ok(Self::typed_expr(
                    ExprKind::CondElvis {
                        cond: Box::new(cond),
                        else_expr: Box::new(else_expr),
                    },
                    typ,
                    pos,
                ));
            }

            let then_expr = self.parse_expression()?;
            self.expect_special(b':')?;
            // Right-to-left: parse else as another conditional
            let else_expr = self.parse_conditional_expr()?;

            // The result type is the common type of then and else branches
            // Apply array-to-pointer and function-to-pointer decay (C99 6.3.2.1)
            let then_typ = then_expr.typ.unwrap_or(self.types.int_id);
            let else_typ = else_expr.typ.unwrap_or(self.types.int_id);

            // C17 6.5.15p3: either both arms have type void, or neither does.
            // A mismatch means one arm has no value for the expression to
            // take.
            let then_void = self.types.kind(then_typ) == TypeKind::Void;
            let else_void = self.types.kind(else_typ) == TypeKind::Void;
            if then_void != else_void {
                let culprit = if then_void { &then_expr } else { &else_expr };
                self.check_not_void(culprit, culprit.pos);
            }

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

    fn parse_logical_or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_logical_and_expr()?;

        while self.is_special_token(SpecialToken::LogicalOr) {
            self.advance();
            let right = self.parse_logical_and_expr()?;
            left = self.make_binary(BinaryOp::LogOr, left, right);
        }

        Ok(left)
    }

    fn parse_logical_and_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_bitwise_or_expr()?;

        while self.is_special_token(SpecialToken::LogicalAnd) {
            self.advance();
            let right = self.parse_bitwise_or_expr()?;
            left = self.make_binary(BinaryOp::LogAnd, left, right);
        }

        Ok(left)
    }

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

    fn parse_bitwise_xor_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_bitwise_and_expr()?;

        while self.is_special(b'^') && !self.is_special_token(SpecialToken::XorAssign) {
            self.advance();
            let right = self.parse_bitwise_and_expr()?;
            left = self.make_binary(BinaryOp::BitXor, left, right);
        }

        Ok(left)
    }

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
            self.check_modifiable_lvalue(&operand, "increment operand", op_pos);
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
            self.check_modifiable_lvalue(&operand, "decrement operand", op_pos);
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

        // GNU label address: `&&label`, of type `void *`. `&&` is one token,
        // so this has to be tested before the unary `&` below, which
        // deliberately excludes it.
        if self.is_special_token(SpecialToken::LogicalAnd) {
            let op_pos = self.current_pos();
            self.advance();
            let name = self.expect_identifier()?;
            return Ok(Self::typed_expr(
                ExprKind::LabelAddr(name),
                self.types.void_ptr_id,
                op_pos,
            ));
        }

        if self.is_special(b'&') && !self.is_special_token(SpecialToken::LogicalAnd) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            self.check_addressable(&operand, op_pos);
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
            self.check_dereferenceable(&operand, op_pos);
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

    /// The operand of a `sizeof (typeof ( E ))`, when `E` is an *expression*.
    ///
    /// Returns `None` -- having restored the position -- for `typeof` of a
    /// type-name, which the ordinary type-name path handles and which carries
    /// its own extents, and for anything that is not a `typeof` at all.
    ///
    /// The caller has already consumed `sizeof`'s own `(`.
    fn try_parse_sizeof_typeof_operand(&mut self) -> ParseResult<Option<Expr>> {
        let saved = self.pos;
        let is_typeof = self.get_ident_id(self.current()).is_some_and(|id| {
            matches!(
                id,
                crate::kw::TYPEOF | crate::kw::GNU_TYPEOF | crate::kw::GNU_TYPEOF2
            )
        });
        if !is_typeof {
            return Ok(None);
        }
        self.advance(); // consume `typeof`
        if !self.is_special(b'(') {
            self.pos = saved;
            return Ok(None);
        }
        self.advance(); // consume typeof's `(`

        // A type-name operand belongs to the other path.
        if self.try_parse_type_name_vm().is_some() {
            self.pos = saved;
            return Ok(None);
        }

        let Ok(expr) = self.parse_expression() else {
            self.pos = saved;
            return Ok(None);
        };
        if !self.is_special(b')') {
            self.pos = saved;
            return Ok(None);
        }
        self.advance(); // consume typeof's `)`
        Ok(Some(expr))
    }

    fn parse_sizeof(&mut self) -> ParseResult<Expr> {
        let sizeof_pos = self.current_pos();
        // sizeof returns size_t, which is unsigned long in our implementation
        let size_t = self.types.ulong_id;

        if self.is_special(b'(') {
            // Could be sizeof(type) or sizeof(expr)
            // For now, try to detect if it's a type
            // This is a simplified check - full implementation needs type lookahead
            self.advance(); // consume '('

            // `sizeof(typeof(E))` is `sizeof(E)`. `sizeof` does not
            // lvalue-convert, so no array decays and no qualifier matters, and
            // neither compiler evaluates through `typeof` -- but the answer for
            // a variably modified `E` lives in the *declaration* of the object,
            // which the linearizer already recorded and a bare `TypeId` cannot
            // carry. Routing it to `SizeofExpr` reaches that record.
            if let Some(inner) = self.try_parse_sizeof_typeof_operand()? {
                self.expect_special(b')')?;
                self.check_sizeof_expr_operand(&inner, sizeof_pos);
                return Ok(Expr::typed(
                    ExprKind::SizeofExpr(Box::new(inner)),
                    size_t,
                    sizeof_pos,
                ));
            }

            // Try to parse as type. The size expressions of any
            // variably-modified array level ride on the node: 6.5.3.4p2 says
            // the operand is evaluated and its size computed at run time, and
            // the interned type cannot carry either. `typeof(type-name)`
            // carries its own extents out, so the completeness check needs no
            // exemption for it.
            if let Some((typ, dims)) = self.try_parse_type_name_vm() {
                self.expect_special(b')')?;
                self.check_sizeof_operand_is_complete(typ, &dims, sizeof_pos);
                return Ok(Expr::typed(
                    ExprKind::SizeofType(typ, dims),
                    size_t,
                    sizeof_pos,
                ));
            }

            // Not a type, parse as expression
            let expr = self.parse_expression()?;
            self.expect_special(b')')?;
            self.check_sizeof_expr_operand(&expr, sizeof_pos);
            Ok(Expr::typed(
                ExprKind::SizeofExpr(Box::new(expr)),
                size_t,
                sizeof_pos,
            ))
        } else {
            // sizeof without parens - must be expression
            let expr = self.parse_unary_expr()?;
            self.check_sizeof_expr_operand(&expr, sizeof_pos);
            Ok(Expr::typed(
                ExprKind::SizeofExpr(Box::new(expr)),
                size_t,
                sizeof_pos,
            ))
        }
    }

    /// 6.5.3.4p1 for the *expression* form of `sizeof`.
    ///
    /// `check_sizeof_operand_is_complete` answers for a type-name, where the
    /// extents ride on the node. An expression has only its type, and the type
    /// cannot tell an incomplete array from a variably modified one -- `int[]`,
    /// `int[n]` and `int[m]` all intern to one `TypeId`. So the question is put
    /// to the *declaration*: `Symbol::array_is_variably_modified` records
    /// whether the declarator carried size expressions. Without it `extern int
    /// a[]; sizeof a` answered 0 where gcc rejects it, while a local VLA's
    /// `sizeof` had to keep working.
    ///
    /// Only an identifier is examined. A subscript or a member reaches an
    /// element whose type is complete by construction, and a call cannot
    /// return an array.
    fn check_sizeof_expr_operand(&self, expr: &Expr, pos: Position) {
        let ExprKind::Ident(symbol_id) = expr.kind else {
            return;
        };
        let Some(typ) = expr.typ else {
            return;
        };
        if self.types.kind(typ) != TypeKind::Array
            || self.types.unsized_array_levels(typ) == 0
            || self.symbols.get(symbol_id).array_is_variably_modified
        {
            return;
        }
        let named = self.types.format_type(typ, Some(self.idents));
        diag::error_args(
            pos,
            "invalid application of 'sizeof' to incomplete type '{0}'",
            &[&named],
        );
    }

    fn check_sizeof_operand_is_complete(&self, typ: TypeId, dims: &[Expr], pos: Position) {
        let incomplete = match self.types.kind(typ) {
            TypeKind::Array => self.types.unsized_array_levels(typ) > dims.len(),
            TypeKind::Struct | TypeKind::Union => !self.types.is_composite_complete(typ),
            _ => false,
        };
        if incomplete {
            crate::diag::error(
                pos,
                &format!(
                    "invalid application of 'sizeof' to incomplete type '{}'",
                    self.types.format_type(typ, Some(self.idents))
                ),
            );
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

            // Try to parse as type first.
            //
            // Deliberately NOT the variably-modified form that `sizeof` two
            // functions above uses. C17 6.5.3.4p3 makes the result of
            // `_Alignof` an integer constant and does not evaluate the
            // operand, and the alignment of `int[n]` is the alignment of
            // `int`, which `TypeTable::alignment` already computes without
            // ever reading an extent. Collecting the expressions here would
            // either be dead weight in the AST or an evaluation the standard
            // forbids.
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
                self.check_modifiable_lvalue(&expr, "increment operand", op_pos);
                self.check_const_assignment(&expr, op_pos);
                // PostInc has same type as operand
                let typ = expr.typ.unwrap_or(self.types.int_id);
                expr = Self::typed_expr(ExprKind::PostInc(Box::new(expr)), typ, base_pos);
            } else if self.is_special_token(SpecialToken::Decrement) {
                let op_pos = self.current_pos();
                self.advance();
                // Check for const modification
                self.check_modifiable_lvalue(&expr, "decrement operand", op_pos);
                self.check_const_assignment(&expr, op_pos);
                // PostDec has same type as operand
                let typ = expr.typ.unwrap_or(self.types.int_id);
                expr = Self::typed_expr(ExprKind::PostDec(Box::new(expr)), typ, base_pos);
            } else if self.is_special(b'[') {
                // Array subscript
                self.advance();
                let index = self.parse_expression()?;
                self.expect_special(b']')?;
                self.check_subscript(&expr, &index, base_pos);
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
                if let Some(t) = expr.typ {
                    self.warn_atomic_member_access(t, member, dot_pos);
                }
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
                // `p->m` names the object `*p`, so the atomicity that matters
                // is the pointee's.
                if let Some(pointee) = expr.typ.and_then(|t| self.types.base_type(t)) {
                    self.warn_atomic_member_access(pointee, member, arrow_pos);
                }
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
                self.check_callable(&expr, call_pos);
                self.check_call_arity(&expr, &args, call_pos);
                self.check_argument_types(&expr, &args);

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

    /// Parse a run of adjacent string literals into one expression.
    ///
    /// C11 6.4.5p5: if any literal in the run has an encoding prefix, the
    /// result takes that encoding; a run mixing two *different* prefixes is a
    /// constraint violation (6.4.5p2).
    fn parse_string_literal_run(&mut self) -> ParseResult<Expr> {
        let start_pos = self.current_pos();
        // Elements of the concatenated literal, still distinguishing a byte
        // from a named character so each encoding can ask for what it needs.
        let mut elements: Vec<literal::Escaped> = Vec::new();
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
                        | TokenValue::Utf32String(s) => {
                            let piece = literal::parse_string_literal(s);
                            // `parse_string_literal` has no position to report
                            // from, so the constraint is raised here, where the
                            // token still does.
                            for e in &piece {
                                if let literal::Escaped::ForbiddenUcn(val) = e {
                                    self.report_forbidden_ucn_at(token.pos, *val);
                                }
                            }
                            piece
                        }
                        _ => return Err(ParseError::new("invalid string token", token.pos)),
                    }
                }
                _ => break,
            };
            elements.extend(piece);

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
                let bytes = literal::literal_bytes(&elements);
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
            // wchar_t[N] — int on the targets here. Like char16_t/char32_t
            // below, its elements are code points rather than bytes, so the
            // UTF-8 the lexer preserved is decoded here. Taking `bytes`
            // straight through instead gave `L"café"` five elements, the first
            // two being the halves of the UTF-8 pair.
            Some(TokenType::WideString) => {
                let units = literal::literal_wide_chars(&elements);
                let wstr_type = self
                    .types
                    .intern(Type::array(self.types.int_id, units.len() + 1));
                // `WideStringLit` carries one `char` per element.
                let text: String = units
                    .iter()
                    .map(|&u| char::from_u32(u).unwrap_or('\u{fffd}'))
                    .collect();
                Ok(Self::typed_expr(
                    ExprKind::WideStringLit(text),
                    wstr_type,
                    start_pos,
                ))
            }
            // char16_t[N] / char32_t[N]. These carry real code units rather
            // than bytes, so the UTF-8 the lexer preserved is decoded here; a
            // code point outside the BMP becomes a surrogate pair in the
            // char16_t case.
            Some(kind @ (TokenType::Utf16String | TokenType::Utf32String)) => {
                let text: String = literal::literal_wide_chars(&elements)
                    .into_iter()
                    .map(|u| char::from_u32(u).unwrap_or('\u{fffd}'))
                    .collect();
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
        // C17 6.5.5-6.5.14 require operands with a value. `f() + 1` where `f`
        // returns void has none.
        self.check_not_void(&left, left.pos);
        self.check_not_void(&right, right.pos);

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

            // The bitwise operators take the usual arithmetic conversions.
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                self.usual_arithmetic_conversions(left_type, right_type)
            }

            // A shift does not. C17 6.5.7p3: the integer promotions are
            // performed on *each* operand and "the type of the result is that
            // of the promoted left operand" -- the right operand's type never
            // reaches the result. Taking the usual arithmetic conversions here
            // let it through, so `1 << 1L` came out `long` and
            // `sizeof(1 << 1L)` answered 8 where gcc answers 4.
            BinaryOp::Shl | BinaryOp::Shr => {
                let promoted = self.types.integer_promote(left_type);
                self.check_shift_count(op, promoted, &right);
                promoted
            }
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

    /// Warn when a shift's constant count cannot name a bit of the value
    /// being shifted.
    ///
    /// C17 6.5.7p3 makes a count that is negative, or not less than the width
    /// of the promoted left operand, undefined. c17 folds such a shift by
    /// masking the count the way the hardware does, and keeps the run-time
    /// path in agreement, so only the diagnostic is owed.
    ///
    /// This sits with the type check rather than in the constant folder
    /// because the folder is called speculatively -- by `__builtin_constant_p`
    /// merely asking whether an expression folds, and by the backtracking
    /// type-name parse -- and would report on expressions that are never
    /// evaluated, sometimes twice. A shift's type is computed exactly once.
    ///
    /// Only the *count* need be constant, as in gcc: `x << 64` warns.
    fn check_shift_count(&mut self, op: BinaryOp, promoted_left: TypeId, right: &Expr) {
        let Some(count) = self.eval_const_expr(right) else {
            return;
        };
        let width = self.types.size_bits(promoted_left) as i128;
        let side = if op == BinaryOp::Shl { "left" } else { "right" };

        // gcc spells these as two groups, and so does `-Wno-`.
        if count < 0 {
            if crate::diag::warning_group_enabled("shift-count-negative") {
                crate::diag::warning(right.pos, &format!("{} shift count is negative", side));
            }
        } else if count >= width && crate::diag::warning_group_enabled("shift-count-overflow") {
            crate::diag::warning(right.pos, &format!("{} shift count >= width of type", side));
        }
    }

    /// The usual arithmetic conversions (C17 6.3.1.8).
    ///
    /// The rules themselves live on the type table, because the linearizer and
    /// the constant folder need the same answer and reach nothing else. This
    /// was a second implementation of them, and the two had drifted: the table
    /// compared widths where this one ranked by kind, so they disagreed about
    /// `long` against `long long`. One of them had to go.
    fn usual_arithmetic_conversions(&mut self, left: TypeId, right: TypeId) -> TypeId {
        self.types.common_type(left, right)
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
    pub(super) fn parse_generic_selection(&mut self, token_pos: Position) -> ParseResult<Expr> {
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
                            // The constant carries its enumeration's type, not
                            // a fixed `int`: when a member does not fit in
                            // `int` the whole enumeration widens, and reading
                            // the constant back as `int` would undo that.
                            let typ = sym.typ;
                            let kind = match i64::try_from(value) {
                                Ok(v) => ExprKind::IntLit(v),
                                // Only an `unsigned long` enumeration above
                                // `LONG_MAX` lands here.
                                Err(_) => ExprKind::Int128Lit(value),
                            };
                            return Ok(Self::typed_expr(kind, typ, token_pos));
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
                    // C17 6.4.4.4p10: an unprefixed character constant has
                    // type `int` and the value of a `char` object holding the
                    // character, converted to `int` -- so its signedness is
                    // plain `char`'s, which is the target's. `'\x80'` is -128
                    // where `char` is signed and 128 where it is not.
                    let (v, is_code_point) =
                        literal::char_literal_value(s, false, self.current_pos());
                    let value = if is_code_point {
                        // Not a byte, so plain `char`'s signedness does not
                        // reach it.
                        v as i64
                    } else if self.types.is_unsigned(self.types.char_id) {
                        v as u8 as i64
                    } else {
                        v as u8 as i8 as i64
                    };
                    Ok(Self::typed_expr(
                        ExprKind::CharLit(value),
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
                        // A prefixed constant takes the code point in its own
                        // type, with no reference to plain `char`'s
                        // signedness: `L'\x80'` is 128, not -128.
                        let (code_point, _) =
                            literal::char_literal_value(s, true, self.current_pos());
                        let (typ, value) = match kind {
                            // wchar_t is int on the targets here.
                            TokenType::WideChar => (self.types.int_id, code_point as i32 as i64),
                            TokenType::Utf16Char => {
                                (self.types.ushort_id, code_point as u16 as i64)
                            }
                            _ => (self.types.uint_id, code_point as i64),
                        };
                        Ok(Self::typed_expr(ExprKind::CharLit(value), typ, token_pos))
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
                                // `(char[]){"hi"}` is the string in braces
                                // (C17 6.7.9p14), three characters, not an
                                // array of one element.
                                let array_size =
                                    match self.braced_string_initializer(elem_type, &elements) {
                                        Some(lit) => self.string_initializer_len(lit),
                                        None => Some(
                                            self.array_size_from_elements(&elements, elem_type),
                                        ),
                                    }
                                    .unwrap_or_else(|| {
                                        self.array_size_from_elements(&elements, elem_type)
                                    });
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
                // Explicit U suffix. 6.4.4.1p5 still picks the *first* of
                // `unsigned int`, `unsigned long`, `unsigned long long` that
                // can represent the value; taking `unsigned int` regardless of
                // magnitude gave `0xaaaaaaaaaaaaaaabu` a four-byte type, and
                // once constants folded at their own width that truncated it.
                match (is_longlong, is_long) {
                    (true, _) => self.types.ulonglong_id,
                    (false, true) => self.types.ulong_id,
                    (false, false) if value_u64 <= u32::MAX as u64 => self.types.uint_id,
                    (false, false) => self.types.ulong_id,
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

    /// C17 6.4.3p2: a universal character name may not name a character below
    /// 00A0 other than `$`, `@` and `` ` ``, nor a UTF-16 surrogate.
    ///
    /// The first half stops a UCN spelling a character that already has a
    /// spelling, which would let `\u0041` smuggle an `A` past anything that
    /// reads the source as text. Both were accepted silently -- a surrogate
    /// even degraded to the letter `u`, because `char::from_u32` rejects it
    /// and the caller took that for "not an escape".
    fn report_forbidden_ucn_at(&self, pos: Position, val: u32) {
        crate::token::lexer::report_forbidden_ucn(pos, val);
    }
}
