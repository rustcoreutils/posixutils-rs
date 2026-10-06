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
use super::operand_rule::UnaryOperator;
use super::parser::{ParseError, ParseResult, Parser};
use crate::diag;
use crate::float::FloatVal;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol};
use crate::token::lexer::{Position, SpecialToken, TokenType, TokenValue};
use crate::token::literal;
use crate::types::{FloatClass, Type, TypeId, TypeKind, TypeModifiers};
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
            // C17 6.5.17p2: the result is the *value* of the right operand,
            // so the lvalue conversion runs -- an array or a function
            // designator becomes a pointer. Taking `right.typ` raw made
            // `sizeof (1, foo)` answer for the function rather than for a
            // pointer to it, and `sizeof (1, arr)` for the whole array.
            let result_typ = right.typ.map(|t| self.lvalue_converted_type(t));

            // Build comma expression
            let Expr { kind, typ, pos, .. } = expr;
            expr = match kind {
                ExprKind::Comma(mut exprs) => {
                    exprs.push(right);
                    Expr {
                        kind: ExprKind::Comma(exprs),
                        typ: result_typ,
                        pos,
                        bitfield_bits: None,
                    }
                }
                other => Expr {
                    kind: ExprKind::Comma(vec![
                        Expr {
                            kind: other,
                            typ,
                            pos,
                            bitfield_bits: None,
                        },
                        right,
                    ]),
                    typ: result_typ,
                    pos,
                    bitfield_bits: None,
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
            match assign_op.binary_op() {
                Some(op) => self.check_compound_assignment(op, &left, &right, assign_pos),
                None => self.check_assignment_types(&left, &right, assign_pos),
            }
            let assign_type = self.modification_result_type(&left);
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
            } else if designators.is_empty()
                && self.peek() == TokenType::Ident
                && self.next_token_is_special(b':')
            {
                // GNU's obsolete field designator, `fieldname: value`, which
                // predates C99's `.fieldname = value`. gcc still accepts it
                // (with `-Wdeprecated`), and glibc-era sources use it:
                //
                //     union { double d; int i[2]; } u = { d: -0.25 };
                //     struct s s = { c: {1, 2, 3} };
                //
                // One token of lookahead settles it: inside an initializer
                // list an identifier followed by `:` cannot be anything else.
                // A conditional starts `x ?`, not `x :`.
                //
                // Only the leading, unchained form -- which is all gcc's own
                // grammar allows -- and the `:` stands in for the `=`, so the
                // loop must not go on to demand one.
                let name = self.expect_identifier()?;
                self.expect_special(b':')?;
                designators.push(Designator::Field(name));
                let value = self.parse_initializer()?;
                return Ok(InitElement {
                    designators,
                    value: Box::new(value),
                });
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

    /// The type of a conditional expression with arms `then_expr` and
    /// `else_expr` (C17 6.5.15p5-6), after checking that they are a pair
    /// 6.5.15p3 admits. `None` after an error, so the conditional is left
    /// untyped and what encloses it does not report the same mistake again.
    ///
    /// The result is a value, so it is unqualified whatever the arms were.
    /// gcc's warnings for pointers to incompatible types and for a pointer
    /// beside an integer leave the conditional typed, as gcc does: `void *`
    /// for the first, the pointer's type for the second.
    fn conditional_result_type(
        &mut self,
        then_expr: &Expr,
        else_expr: &Expr,
        pos: Position,
    ) -> Option<TypeId> {
        // An arm diagnosed already, or one with no value -- whose mismatch
        // with a valued arm `check_not_void` has reported.
        let (then_typ, else_typ) = (then_expr.typ?, else_expr.typ?);
        // GNU vectors: two of one type, and nothing else.
        if self.types.is_vector(then_typ) || self.types.is_vector(else_typ) {
            let then_typ = self.types.unqualified(then_typ);
            let else_typ = self.types.unqualified(else_typ);
            if self.types.is_vector(then_typ)
                && self.types.is_vector(else_typ)
                && self.types.types_compatible(then_typ, else_typ)
            {
                return Some(then_typ);
            }
            diag::error(pos, &gettext("type mismatch in conditional expression"));
            return None;
        }
        let then_typ = self.lvalue_converted_type(then_typ);
        let else_typ = self.lvalue_converted_type(else_typ);
        let kinds = (self.types.kind(then_typ), self.types.kind(else_typ));
        if kinds.0 == TypeKind::Void || kinds.1 == TypeKind::Void {
            return Some(self.types.void_id);
        }

        // Both arithmetic: the usual arithmetic conversions (6.5.15p5) --
        // the same ones the binary operators use, so the answer follows
        // conversion *rank* and not bit width, and does not depend on which
        // arm was written first. Complex included: `c ? 1 : z` is `double
        // _Complex`, and `linearize_complex_ternary` merges it by address.
        if self.types.is_arithmetic(then_typ) && self.types.is_arithmetic(else_typ) {
            return Some(self.usual_arithmetic_conversions(then_typ, else_typ));
        }

        let pointers = (kinds.0 == TypeKind::Pointer, kinds.1 == TypeKind::Pointer);
        match pointers {
            (true, true) => {
                // A null pointer constant takes the other arm's type, even
                // spelled `(void *)0`: that is what makes `c ? fp : NULL` a
                // function pointer and not a `void *`.
                if self.is_null_pointer_constant(else_expr) {
                    return Some(then_typ);
                }
                if self.is_null_pointer_constant(then_expr) {
                    return Some(else_typ);
                }
                Some(self.pointer_conditional_type(then_typ, else_typ, pos))
            }
            (true, false) | (false, true) => {
                let (ptr, other, other_typ) = if pointers.0 {
                    (then_typ, else_expr, else_typ)
                } else {
                    (else_typ, then_expr, then_typ)
                };
                if !self.types.is_integer(other_typ) {
                    return self.report_conditional_mismatch(pos);
                }
                if !self.is_null_pointer_constant(other) {
                    diag::warning(
                        pos,
                        &gettext("pointer/integer type mismatch in conditional expression"),
                    );
                }
                Some(ptr)
            }
            // The same structure or union; anything else, such as a
            // structure beside a number, is no pair at all.
            (false, false) if self.types.types_compatible(then_typ, else_typ) => Some(then_typ),
            (false, false) => self.report_conditional_mismatch(pos),
        }
    }

    /// gcc's "type mismatch in conditional expression", an error: these arms
    /// are no pair 6.5.15p3 admits, so the conditional has no type.
    fn report_conditional_mismatch(&self, pos: Position) -> Option<TypeId> {
        diag::error(pos, &gettext("type mismatch in conditional expression"));
        None
    }

    /// The type of a conditional whose arms are the pointers `then_typ` and
    /// `else_typ`, neither of them a null pointer constant (C17 6.5.15p6).
    ///
    /// A pointer to the composite type, or to `void` when either arm points
    /// to `void`, qualified with the qualifiers of *both* referenced types:
    /// `c ? (const int *)a : (volatile int *)b` is `const volatile int *`,
    /// whichever arm comes first. An array's qualifiers are its elements',
    /// as gcc reads them, so `const int (*)[]` meets `int (*)[3]` as
    /// `const int (*)[3]`.
    fn pointer_conditional_type(
        &mut self,
        then_typ: TypeId,
        else_typ: TypeId,
        pos: Position,
    ) -> TypeId {
        let (Some(tp), Some(ep)) = (
            self.types.base_type(then_typ),
            self.types.base_type(else_typ),
        ) else {
            return then_typ;
        };
        let quals =
            self.types.qualifiers_through_arrays(tp) | self.types.qualifiers_through_arrays(ep);
        let (tp, ep) = (
            self.types.unqualified_through_arrays(tp),
            self.types.unqualified_through_arrays(ep),
        );
        let (t_void, e_void) = (
            self.types.kind(tp) == TypeKind::Void,
            self.types.kind(ep) == TypeKind::Void,
        );
        let target = if t_void || e_void {
            // The `void *` carve-out is for pointers to objects. gcc objects
            // to a function pointer only under `-pedantic`.
            if self.types.pointees_pair_function_with_void(tp, ep) {
                diag::pedwarn(
                    pos,
                    &gettext(
                        "ISO C forbids conditional expr between 'void *' and function pointer",
                    ),
                );
            }
            self.types.void_id
        } else if self.types.types_compatible(tp, ep) {
            self.types.composite_type(tp, ep)
        } else {
            // gcc's answer, unqualified whatever the arms pointed to.
            diag::warning(
                pos,
                &gettext("pointer type mismatch in conditional expression"),
            );
            return self.types.void_ptr_id;
        };
        let target = self.types.qualified_with(target, quals);
        self.types.intern(Type::pointer(target))
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
        self.types.unqualified(decayed)
    }

    /// The type of an assignment, compound assignment, or prefix or postfix
    /// `++`/`--` of `target`: the type `target` has after lvalue conversion
    /// (C17 6.5.16p3; 6.5.2.4p2 and 6.5.3.1p2 by reference to it).
    ///
    /// Not the target's own type. `v = 1` for a `volatile int v` is an `int`
    /// value, and typing it `volatile int` let a qualifier onto an rvalue
    /// that `_Generic`, `typeof` and any "did this touch a volatile object"
    /// question downstream would all read.
    fn modification_result_type(&mut self, target: &Expr) -> TypeId {
        let typ = target.typ.unwrap_or(self.types.int_id);
        self.lvalue_converted_type(typ)
    }

    /// Parse a conditional (ternary) expression: cond ? then : else
    pub(crate) fn parse_conditional_expr(&mut self) -> ParseResult<Expr> {
        let cond = self.parse_logical_or_expr()?;

        if self.is_special(b'?') {
            self.advance();
            let cond_tested = self.check_truth_value(&cond);

            // GNU `a ?: b`: the middle operand may be omitted, and then the
            // condition is also the value when it is true. Kept as its own
            // node rather than rewritten to `a ? a : b`, because 6.5.15 would
            // then evaluate `a` twice -- `f() ?: 0` must call `f` once.
            let colon_pos = self.current_pos();
            if self.is_special(b':') {
                self.advance();
                let else_expr = self.parse_conditional_expr()?;

                // The condition is also an arm here, so one that cannot be
                // tested has been reported as the operand it is.
                let typ = if cond_tested {
                    self.conditional_result_type(&cond, &else_expr, colon_pos)
                } else {
                    None
                };

                let pos = cond.pos;
                let e = ExprKind::CondElvis {
                    cond: Box::new(cond),
                    else_expr: Box::new(else_expr),
                };
                return Ok(Expr {
                    typ,
                    ..Expr::new(e, pos)
                });
            }

            let then_expr = self.parse_expression()?;
            let colon_pos = self.current_pos();
            self.expect_special(b':')?;
            // Right-to-left: parse else as another conditional
            let else_expr = self.parse_conditional_expr()?;

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

            let typ = self.conditional_result_type(&then_expr, &else_expr, colon_pos);

            let pos = cond.pos;
            let e = ExprKind::Conditional {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            };
            Ok(Expr {
                typ,
                ..Expr::new(e, pos)
            })
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
            self.check_unary_operand(UnaryOperator::Increment, &operand, op_pos);
            let typ = self.modification_result_type(&operand);
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
            self.check_unary_operand(UnaryOperator::Decrement, &operand, op_pos);
            let typ = self.modification_result_type(&operand);
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
            let label = self.resolve_label(name);
            return Ok(Self::typed_expr(
                ExprKind::LabelAddr(label),
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
            match operand.typ.filter(|&t| self.types.is_vector(t)) {
                Some(t) => {
                    let named = self.types.format_type(t, Some(self.idents));
                    diag::error_args(
                        op_pos,
                        "invalid type argument of unary '*' (have '{0}')",
                        &[&named],
                    );
                }
                None => self.check_dereferenceable(&operand, op_pos),
            }
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
            // C17 6.5.3.3p2: the result is the value of the *promoted*
            // operand, with the promoted type -- and a value, not an lvalue.
            // Returning the operand itself made `+(signed char)0` a
            // `signed char` and `+x = 1` an assignment.
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            let valid = self.check_unary_operand(UnaryOperator::Plus, &operand, op_pos);
            let (operand, typ) = self.promote_unary_operand(operand);
            let e = Self::typed_expr(
                ExprKind::Cast {
                    cast_type: typ,
                    expr: Box::new(operand),
                },
                typ,
                op_pos,
            );
            return Ok(Self::typed_if(e, valid));
        }

        if self.is_special(b'-') && !self.is_special_token(SpecialToken::Decrement) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            let valid = self.check_unary_operand(UnaryOperator::Minus, &operand, op_pos);
            let (operand, typ) = self.promote_unary_operand(operand);
            let width = self.unary_bitfield_width(&operand, typ);
            let mut e = Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::Neg,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            );
            e.bitfield_bits = width;
            return Ok(Self::typed_if(e, valid));
        }

        if self.is_special(b'~') {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            let valid = self.check_unary_operand(UnaryOperator::Complement, &operand, op_pos);
            let (operand, typ) = self.promote_unary_operand(operand);
            let width = self.unary_bitfield_width(&operand, typ);
            let mut e = Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::BitNot,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            );
            e.bitfield_bits = width;
            return Ok(Self::typed_if(e, valid));
        }

        if self.is_special(b'!') {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            let valid = self.check_unary_operand(UnaryOperator::Not, &operand, op_pos);
            // Logical not always produces int (0 or 1)
            let e = Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::Not,
                    operand: Box::new(operand),
                },
                self.types.int_id,
                op_pos,
            );
            return Ok(Self::typed_if(e, valid));
        }

        // sizeof and _Alignof
        if let Some(name_id) = self.current_ident() {
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
        let is_typeof = self.current_ident().is_some_and(|id| {
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

        // A type-name operand belongs to the other path. Asked of the first
        // token only: parsing the type-name here and then rewinding would
        // parse it twice, reporting every fault in it twice.
        if self.starts_type_name() {
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

    /// `value`, whose type was written as a type-name with the size
    /// expressions `dims`, carrying them ([`ExprKind::VmTypeName`]) so that
    /// what is rooted in it finds its extents -- and they are evaluated.
    ///
    /// Only a pointer's pointee has extents to record: a cast to an array
    /// type is refused elsewhere, a compound literal may not be a VLA (C17
    /// 6.5.2.5p1), and `sizeof(int (*)[n])`-style uses never get here.
    pub(crate) fn with_type_name_extents(&mut self, dims: Vec<Expr>, value: Expr) -> Expr {
        let Some(typ) = value.typ.filter(|_| !dims.is_empty()) else {
            return value;
        };
        if self.types.kind(typ) != TypeKind::Pointer {
            return value;
        }
        let mut sym = Symbol::typedef(StringId::EMPTY, typ, self.symbols.depth())
            .with_variably_modified_array(true);
        // Unnamed, so never a redefinition of another one in this scope.
        sym.defined = false;
        let Ok(symbol) = self.symbols.declare(sym) else {
            return value;
        };
        let pos = value.pos;
        Self::typed_expr(
            ExprKind::VmTypeName {
                symbol,
                dims,
                expr: Box::new(value),
            },
            typ,
            pos,
        )
    }

    /// The `{ ... }` of a compound literal, with `typ` already parsed.
    ///
    /// Its own function because C99 6.5.2.5 makes a compound literal a
    /// *postfix* expression, so more than one production reaches it: a cast
    /// is not the only thing that can follow a parenthesised type name, and
    /// `sizeof (struct s){1, 2}` and `_Alignof` each committed to the type
    /// alone and left the braces behind.
    ///
    /// `dims` are the type-name's size expressions. A variable length array
    /// may not be a compound literal (C17 6.5.2.5p1) -- `(int[n]){0}` was
    /// sized by its one initializer instead -- but a pointer to one may, and
    /// carries its extents ([`Self::with_type_name_extents`]).
    pub(crate) fn parse_compound_literal_tail(
        &mut self,
        typ: TypeId,
        dims: Vec<Expr>,
        paren_pos: Position,
    ) -> ParseResult<Expr> {
        if super::ast::sizeof_type_is_runtime(self.types, typ, &dims) {
            diag::error(paren_pos, &gettext("compound literal has variable size"));
        }
        let literal = self.parse_compound_literal_body(typ, paren_pos)?;
        Ok(self.with_type_name_extents(dims, literal))
    }

    /// C17 6.5.4p2-p4 for a cast to anything but a union (gcc's extension)
    /// or an array or function type (reported by the caller): the target is
    /// `void` or a scalar type, the operand is a scalar, and neither side
    /// pairs a pointer with a floating type. Each of these compiled -- a
    /// structure reinterpreted as an integer, a `double`'s bits as an
    /// address -- where gcc rejects them in its words.
    fn check_cast_operand(&mut self, target: TypeId, expr: &Expr, pos: Position) {
        let target_kind = self.types.kind(target);
        if target_kind == TypeKind::Void {
            return;
        }
        let Some(from) = expr.typ.map(|t| self.decayed_type(t)) else {
            return;
        };
        // Vectors have their own rules, checked by the caller.
        if self.types.is_vector(target) || self.types.is_vector(from) {
            return;
        }
        // A cast to the operand's own structure type, qualifiers aside, is
        // gcc's extension and converts nothing; any other is not a scalar
        // conversion at all.
        if matches!(target_kind, TypeKind::Struct) {
            let target = self.types.unqualified(target);
            let from = self.lvalue_converted_type(from);
            if !self.types.types_compatible(from, target) {
                diag::error(pos, &gettext("conversion to non-scalar type requested"));
            } else if !self.types.is_composite_complete(target) {
                let named = self.types.format_type(target, Some(self.idents));
                diag::error_args(pos, "invalid use of undefined type '{0}'", &[&named]);
            }
            return;
        }
        let from_kind = self.types.kind(from);
        // A void expression has no value to convert (C17 6.3.2.2): `(int)g()`
        // for a `void g(void)` read whatever the return register held.
        if from_kind == TypeKind::Void {
            diag::error(pos, &gettext("invalid use of void expression"));
            return;
        }
        let wanted = if self.types.is_float(target) {
            "a floating-point"
        } else if target_kind == TypeKind::Pointer {
            "a pointer"
        } else {
            "an integer"
        };
        if matches!(from_kind, TypeKind::Struct | TypeKind::Union) {
            diag::error_args(
                pos,
                "aggregate value used where {0} was expected",
                &[wanted],
            );
        } else if from_kind == TypeKind::Pointer && self.types.is_complex(target) {
            diag::error(
                pos,
                &gettext("pointer value used where a complex was expected"),
            );
        } else if target_kind == TypeKind::Pointer && self.types.is_complex(from) {
            diag::error(pos, &gettext("cannot convert to a pointer type"));
        } else if from_kind == TypeKind::Pointer && self.types.is_float(target) {
            diag::error(
                pos,
                &gettext("pointer value used where a floating-point was expected"),
            );
        } else if target_kind == TypeKind::Pointer && self.types.is_float(from) {
            diag::error(pos, &gettext("cannot convert to a pointer type"));
        } else if target_kind == TypeKind::Pointer && from_kind == TypeKind::Pointer {
            self.check_function_object_pointer_cast(target, from, expr, pos);
        }
    }

    /// C17 6.3.2.3 converts a function pointer only to another function
    /// pointer; a cast between one and an object pointer -- `void *`
    /// included, which is why `(fp)dlsym(h, "x")` is outside the standard --
    /// is a conversion it does not define. gcc accepts both directions as an
    /// extension and objects only under `-pedantic`, except to a null pointer
    /// constant, which converts to any pointer.
    fn check_function_object_pointer_cast(
        &self,
        target: TypeId,
        from: TypeId,
        expr: &Expr,
        pos: Position,
    ) {
        let (Some(to), Some(of)) = (self.types.base_type(target), self.types.base_type(from))
        else {
            return;
        };
        let (to_fn, of_fn) = (
            self.types.kind(to) == TypeKind::Function,
            self.types.kind(of) == TypeKind::Function,
        );
        if of_fn && !to_fn {
            diag::pedwarn(
                pos,
                &gettext("ISO C forbids conversion of function pointer to object pointer type"),
            );
        } else if to_fn && !of_fn && !self.is_null_pointer_constant(expr) {
            diag::pedwarn(
                pos,
                &gettext("ISO C forbids conversion of object pointer to function pointer type"),
            );
        }
    }

    /// A GNU cast to union type, `(union U)expr`.
    ///
    /// The operand must have the type of one of the members -- see
    /// [`crate::types::TypeTable::union_member_for_cast`] -- and the result is
    /// the union with that member initialized: `(union U){ .member = expr }`,
    /// every other byte zero. It is built as exactly that compound literal,
    /// so the initializer, constant and aggregate-value paths all treat it as
    /// one, and wrapped in a cast to its own type, which converts nothing but
    /// keeps the result from being an lvalue as a compound literal is.
    ///
    /// An operand of the union's own type is an ordinary no-op cast. One that
    /// matches no member is diagnosed as gcc does.
    fn cast_to_union(&mut self, union_typ: TypeId, operand: Expr, pos: Position) -> Expr {
        let union_typ = self.types.unqualified(union_typ);
        let value = match operand.typ {
            Some(t) => {
                let t = self.lvalue_converted_type(t);
                if self.types.types_compatible(t, union_typ) {
                    operand
                } else if let Some(name) = self.types.union_member_for_cast(union_typ, t) {
                    let elements = vec![InitElement {
                        designators: vec![Designator::Field(name)],
                        value: Box::new(operand),
                    }];
                    Self::typed_expr(
                        ExprKind::CompoundLiteral {
                            typ: union_typ,
                            elements,
                        },
                        union_typ,
                        pos,
                    )
                } else {
                    diag::error(
                        pos,
                        &gettext("cast to union type from type not present in union"),
                    );
                    operand
                }
            }
            None => operand,
        };
        Self::typed_expr(
            ExprKind::Cast {
                cast_type: union_typ,
                expr: Box::new(value),
            },
            union_typ,
            pos,
        )
    }

    /// [`Self::parse_compound_literal_tail`] from its `{`.
    fn parse_compound_literal_body(
        &mut self,
        typ: TypeId,
        paren_pos: Position,
    ) -> ParseResult<Expr> {
        let init_list = self.parse_initializer_list()?;
        let mut elements = match init_list.kind {
            ExprKind::InitList { elements } => elements,
            _ => unreachable!("parse_initializer_list returns an InitList"),
        };

        // An incomplete array type takes its size from the initializer.
        // `parse_declarator` spells "no size given" as `None`, which is what
        // `int a[]` means; the type-name parser this replaced spelled it
        // `Some(0)`, conflating it with the GNU zero-length array. Accept
        // both, since the declaration path (`infer_array_size_from_init`)
        // also does.
        self.walk_initializer_elements(typ, &mut elements);
        let final_typ = if self.types.kind(typ) == TypeKind::Array
            && matches!(self.types.get(typ).array_size, None | Some(0))
        {
            let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
            // `(char[]){"hi"}` is the string in braces (C17 6.7.9p14), three
            // characters, not an array of one element.
            let array_size = match self.braced_string_initializer(elem_type, &elements) {
                Some(lit) => self.string_initializer_len(lit),
                None => Some(self.array_size_from_elements(&elements, elem_type)),
            }
            .unwrap_or_else(|| self.array_size_from_elements(&elements, elem_type));
            self.types.intern(Type::array(elem_type, array_size))
        } else {
            typ
        };

        // A compound literal inside a function has automatic storage duration
        // (C17 6.5.2.5p5), so it gets a frame slot and the same bound applies.
        // It is not a declaration, so the declarator check above never sees it;
        // at file scope the literal is static and is left alone.
        if self.symbols.depth() > 0 {
            self.check_stack_object_size(final_typ, paren_pos, "a compound literal")?;
        }

        Ok(Self::typed_expr(
            ExprKind::CompoundLiteral {
                typ: final_typ,
                elements,
            },
            final_typ,
            paren_pos,
        ))
    }

    fn parse_sizeof(&mut self) -> ParseResult<Expr> {
        let sizeof_pos = self.current_pos();
        // sizeof returns size_t, which is unsigned long in our implementation
        let size_t = self.types.ulong_id;

        if self.is_special(b'(') {
            // `sizeof (type-name)`, or a unary expression that begins with a
            // parenthesized primary: a type name is tried first, and an
            // expression parsed if it is not one.
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
                // `sizeof (struct s){1, 2}` is `sizeof` of a *compound
                // literal*, not of the type: C99 6.5.2.5 makes the literal a
                // postfix expression, and `sizeof` binds to the whole of one.
                // Committing to the type left the braces for whatever was
                // parsing the enclosing construct.
                if self.is_special(b'{') {
                    let literal = self.parse_compound_literal_tail(typ, dims, sizeof_pos)?;
                    let expr = self.parse_postfix_suffixes(literal)?;
                    self.check_sizeof_expr_operand(&expr, sizeof_pos);
                    return Ok(Expr::typed(
                        ExprKind::SizeofExpr(Box::new(expr)),
                        size_t,
                        sizeof_pos,
                    ));
                }
                self.check_sizeof_operand_is_complete(typ, &dims, sizeof_pos);
                return Ok(Expr::typed(
                    ExprKind::SizeofType(typ, dims),
                    size_t,
                    sizeof_pos,
                ));
            }

            // Not a type: the parenthesized expression is only the primary
            // of the operand, a unary expression -- `sizeof (a)[0]` measures
            // `a[0]`, and stopping at the `)` rejected the `[`.
            let expr = self.parse_parenthesized_operand()?;
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
    /// An incomplete structure, union or enumeration is incomplete whatever
    /// the expression -- `sizeof *p` for a `struct S *p` whose tag has no
    /// definition yet. For an array only an identifier is examined: a
    /// subscript or a member reaches an element whose type is complete by
    /// construction, and a call cannot return an array.
    fn check_sizeof_expr_operand(&self, expr: &Expr, pos: Position) {
        // C17 6.5.3.4p1: not a bit-field, which has no size in bytes.
        if self.bit_field_designated(expr).is_some() {
            diag::error(pos, &gettext("'sizeof' applied to a bit-field"));
            return;
        }
        let Some(typ) = expr.typ else {
            return;
        };
        if matches!(
            self.types.kind(typ),
            TypeKind::Struct | TypeKind::Union | TypeKind::Enum
        ) && self.type_name_is_incomplete(typ, 0)
        {
            let named = self.types.format_type(typ, Some(self.idents));
            diag::error_args(
                pos,
                "invalid application of 'sizeof' to incomplete type '{0}'",
                &[&named],
            );
            return;
        }
        let ExprKind::Ident(symbol_id) = expr.kind else {
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

    /// Whether the type named by a type-name is incomplete (C17 6.2.5p1):
    /// `void`, a declared but undefined structure, union or enumeration, or
    /// an array with an extent neither written nor supplied by one of the
    /// type-name's own size expressions (`extents`, which `int[n]` has and
    /// `int[]` does not).
    pub(crate) fn type_name_is_incomplete(&self, typ: TypeId, extents: usize) -> bool {
        match self.types.kind(typ) {
            TypeKind::Void => true,
            TypeKind::Array => self.types.unsized_array_levels(typ) > extents,
            TypeKind::Struct | TypeKind::Union | TypeKind::Enum => {
                !self.types.is_composite_complete(typ)
            }
            _ => false,
        }
    }

    /// `sizeof (void)` is gcc's extension (it is 1); every other incomplete
    /// type-name is an error.
    fn check_sizeof_operand_is_complete(&self, typ: TypeId, dims: &[Expr], pos: Position) {
        let incomplete =
            self.types.kind(typ) != TypeKind::Void && self.type_name_is_incomplete(typ, dims.len());
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
            // The size expressions are not the operand's: C17 6.5.3.4p3 makes
            // the result of `_Alignof` an integer constant and does not
            // evaluate the operand, and the alignment of `int[n]` is the
            // alignment of `int`, which `TypeTable::alignment` computes
            // without ever reading an extent. Only a compound literal, whose
            // type-name they complete, keeps them.
            if let Some((typ, dims)) = self.try_parse_type_name_vm() {
                self.expect_special(b')')?;
                // As in `sizeof`: a `{` here means the operand was a compound
                // literal, which is a postfix expression and not the type.
                if self.is_special(b'{') {
                    let literal = self.parse_compound_literal_tail(typ, dims, alignof_pos)?;
                    let expr = self.parse_postfix_suffixes(literal)?;
                    return Ok(self.alignof_expr(expr, size_t, alignof_pos));
                }
                // C17 6.5.3.4p1: not an incomplete type. gcc answers 1 for
                // `void`, as an extension, and so does c17. A variable length
                // array's extents are its own size expressions, so `int[n]`
                // is complete.
                if self.types.kind(typ) != TypeKind::Void
                    && self.type_name_is_incomplete(typ, dims.len())
                {
                    let named = self.types.format_type(typ, Some(self.idents));
                    diag::error_args(
                        alignof_pos,
                        "invalid application of '_Alignof' to incomplete type '{0}'",
                        &[&named],
                    );
                }
                return Ok(Expr::typed(ExprKind::AlignofType(typ), size_t, alignof_pos));
            }

            // Not a type, so an expression -- with its postfix operators, as
            // in `sizeof`.
            let expr = self.parse_parenthesized_operand()?;
            Ok(self.alignof_expr(expr, size_t, alignof_pos))
        } else {
            // _Alignof without parens - must be expression
            let expr = self.parse_unary_expr()?;
            Ok(self.alignof_expr(expr, size_t, alignof_pos))
        }
    }

    /// The rest of a `sizeof` or `_Alignof` operand that began with a `(` not
    /// starting a type-name, the `(` already consumed: a parenthesized
    /// expression and whatever postfix operators follow it.
    fn parse_parenthesized_operand(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_expression()?;
        self.expect_special(b')')?;
        self.parse_postfix_suffixes(expr)
    }

    /// `_Alignof` applied to an expression rather than a type name.
    ///
    /// C17 6.5.3.4p1 takes a parenthesised type name; applying the operator to
    /// an expression is the GNU `__alignof__` extension, and there gcc answers
    /// the *object's* declared alignment, not its type's. So an object carrying
    /// `_Alignas(64)` or `__attribute__((aligned(64)))` answers 64 even though
    /// its type is still plain `int`.
    ///
    /// Resolved here, where the symbol is in hand, so that the one rule has one
    /// implementation: the constant evaluator and the linearizer each computed
    /// this from `expr.typ` alone and so disagreed with gcc identically.
    fn alignof_expr(&mut self, expr: Expr, size_t: TypeId, pos: Position) -> Expr {
        if self.bit_field_designated(&expr).is_some() {
            diag::error(pos, &gettext("'_Alignof' applied to a bit-field"));
        }
        if let ExprKind::Ident(symbol_id) = &expr.kind {
            let symbol = self.symbols.get(*symbol_id);
            if let Some(align) = symbol.explicit_align {
                return Expr::typed(ExprKind::IntLit(align as i64), size_t, pos);
            }
            // A function's `aligned` is a function attribute, gathered across
            // every declaration of the name rather than held on one symbol.
            if self.types.kind(symbol.typ) == TypeKind::Function {
                let declared = self.declared_fn_attrs.get(&symbol.name);
                if let Some(align) = declared.and_then(|attrs| attrs.align) {
                    return Expr::typed(ExprKind::IntLit(align as i64), size_t, pos);
                }
            }
        }
        Expr::typed(ExprKind::AlignofExpr(Box::new(expr)), size_t, pos)
    }

    /// Parse postfix expression: x++, x--, x[i], x.member, x->member, x(args)
    fn parse_postfix_expr(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_primary_expr()?;
        self.parse_postfix_suffixes(expr)
    }

    /// The `[...]`, `.`, `->`, `(...)`, `++` and `--` that may follow a
    /// postfix expression, applied to one already parsed.
    ///
    /// Split out so a compound literal reached from somewhere other than
    /// `parse_primary_expr` -- `sizeof (int[2]){1, 2}[0]` -- takes the same
    /// suffixes as one reached the usual way.
    pub(crate) fn parse_postfix_suffixes(&mut self, expr: Expr) -> ParseResult<Expr> {
        let mut expr = expr;

        loop {
            // Preserve the position of the base expression for all postfix ops
            let base_pos = expr.pos;

            if self.is_special_token(SpecialToken::Increment) {
                let op_pos = self.current_pos();
                self.advance();
                // Check for const modification
                self.check_modifiable_lvalue(&expr, "increment operand", op_pos);
                self.check_const_assignment(&expr, op_pos);
                self.check_unary_operand(UnaryOperator::Increment, &expr, op_pos);
                let typ = self.modification_result_type(&expr);
                expr = Self::typed_expr(ExprKind::PostInc(Box::new(expr)), typ, base_pos);
            } else if self.is_special_token(SpecialToken::Decrement) {
                let op_pos = self.current_pos();
                self.advance();
                // Check for const modification
                self.check_modifiable_lvalue(&expr, "decrement operand", op_pos);
                self.check_const_assignment(&expr, op_pos);
                self.check_unary_operand(UnaryOperator::Decrement, &expr, op_pos);
                let typ = self.modification_result_type(&expr);
                expr = Self::typed_expr(ExprKind::PostDec(Box::new(expr)), typ, base_pos);
            } else if self.is_special(b'[') {
                // Array subscript
                self.advance();
                let index = self.parse_expression()?;
                self.expect_special(b']')?;
                self.check_subscript(&expr, &index, base_pos);
                // C17 6.5.2.1p2 defines `E1[E2]` as `(*((E1)+(E2)))`, so the
                // two operands are interchangeable: `N[p]` is `p[N]`. The
                // element type therefore comes from whichever operand is the
                // pointer, and taking it from the left one alone gave `N[p]`
                // the type `int` -- it read four bytes at a four-byte stride
                // from a `char` object, and a store through it landed
                // somewhere else entirely. The linearizer already swapped the
                // operands; only the type did not.
                let elem_type = expr
                    .typ
                    .and_then(|t| self.types.base_type(t))
                    .or_else(|| index.typ.and_then(|t| self.types.base_type(t)))
                    .unwrap_or(self.types.int_id);
                // A vector is qualified as a whole, and an element of a
                // `const` one is not to be written either.
                let elem_type = match expr.typ.filter(|&t| self.types.is_vector(t)) {
                    Some(v) => {
                        let quals = self.types.qualifiers(v);
                        self.types.qualified_with(elem_type, quals)
                    }
                    None => elem_type,
                };
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
                let member_type = if let Some(t) = expr.typ {
                    let kind = self.types.kind(t);
                    if kind != TypeKind::Struct && kind != TypeKind::Union {
                        diag::error(
                            dot_pos,
                            &gettext("request for member in something not a structure or union"),
                        );
                        self.types.int_id
                    } else if let Some(typ) = self.types.member_access_type(t, member) {
                        // C17 6.5.2.3p3: so-qualified by the object.
                        typ
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
                // Get member type: dereference the pointer, then find the member
                let member_type = if let Some(t) = expr.typ {
                    // C17 6.5.2.3p2: the operand of `->` is a pointer (an
                    // array decays to one). A structure there has a base type
                    // of nothing, and the member access was dropped silently.
                    let decayed = self.decayed_type(t);
                    if self.types.kind(decayed) != TypeKind::Pointer {
                        let have = self.types.format_type(t, Some(self.idents));
                        diag::error_args(
                            arrow_pos,
                            "invalid type argument of '->' (have '{0}')",
                            &[&have],
                        );
                        self.types.int_id
                    } else if let Some(struct_type) = self.types.base_type(decayed) {
                        let kind = self.types.kind(struct_type);
                        if kind != TypeKind::Struct && kind != TypeKind::Union {
                            diag::error(
                                arrow_pos,
                                &gettext(
                                    "request for member in something not a structure or union",
                                ),
                            );
                            self.types.int_id
                        } else if let Some(typ) = self.types.member_access_type(struct_type, member)
                        {
                            // C17 6.5.2.3p4: so-qualified by the *pointee*.
                            // `struct S *volatile p` qualifies `p`, not `*p`.
                            typ
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
                expr = self.checked_call(expr, args, call_pos, base_pos);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// The call `callee(args)`, checked as C17 6.5.2.2 asks: the callee is
    /// callable, the arguments agree with its prototype, and the value it
    /// returns is complete. `call_pos` is where diagnostics about the call
    /// point; `pos` is the expression's own position.
    ///
    /// Every call the source spells goes through here, and so does the call
    /// `__attribute__((cleanup(fn)))` stands for.
    pub(super) fn checked_call(
        &mut self,
        callee: Expr,
        args: Vec<Expr>,
        call_pos: Position,
        pos: Position,
    ) -> Expr {
        self.check_callable(&callee, call_pos);
        let func_type = self.resolved_function_type(&callee);
        let callee_name = self.callee_name(&callee);
        self.check_call(func_type, callee_name, &args, call_pos);

        // The return type, from the function type the call calls --
        // through a pointer for a call through one -- or `int` when
        // there is none. A call's value has the unqualified version
        // of it (C17 6.7.6.3p4 makes that the function's return type).
        let return_type = func_type
            .and_then(|f| self.types.base_type(f))
            .unwrap_or(self.types.int_id);
        let return_type = self.types.unqualified(return_type);
        // 6.5.2.2p1: a call returns `void` or a complete object type;
        // a prototype may name an incomplete one, but a call has a
        // value of it to make.
        if self.types.kind(return_type) != TypeKind::Void
            && self.type_name_is_incomplete(return_type, 0)
        {
            let named = self.types.format_type(return_type, Some(self.idents));
            diag::error_args(call_pos, "invalid use of undefined type '{0}'", &[&named]);
        }

        let known = self.known_callee(&callee);
        self.fold_zero_length_compare(Self::typed_expr(
            ExprKind::Call {
                func: Box::new(callee),
                args,
                binding: crate::parse::ast::CalleeBinding::Declared,
                known,
            },
            return_type,
            pos,
        ))
    }

    /// The type of `__func__`: `const char[N]` for the enclosing function's
    /// name. Outside a function gcc warns and gives it an empty name.
    fn func_name_type(&mut self, spelled: StringId, pos: Position) -> TypeId {
        let len = match self.enclosing_function.name {
            Some(name) => self.idents.get_opt(name).map_or(0, str::len),
            None => {
                let spelled = self.idents.get_opt(spelled).unwrap_or("__func__");
                diag::warning_args(
                    pos,
                    "'{0}' is not defined outside of function scope",
                    &[spelled],
                );
                0
            }
        };
        let const_char = self
            .types
            .qualified_with(self.types.char_id, TypeModifiers::CONST);
        self.types.intern(Type::array(const_char, len + 1))
    }

    /// Parse a run of adjacent string literals into one expression.
    ///
    /// C11 6.4.5p5: if any literal in the run has an encoding prefix, the
    /// result takes that encoding; a run mixing two *different* prefixes is a
    /// constraint violation (6.4.5p2).
    pub(super) fn parse_string_literal_run(&mut self) -> ParseResult<Expr> {
        let start_pos = self.current_pos();
        // Elements of the concatenated literal, still distinguishing a byte
        // from a named character so each encoding can ask for what it needs.
        let mut pieces: Vec<(Position, Vec<literal::Escaped>)> = Vec::new();
        let mut encoding: Option<TokenType> = None;
        let mut mixed_reported = false;
        // A `u8` literal folds into the narrow token type, so it is tracked
        // apart: it may join a plain literal but not a wide one (6.4.5p2).
        let mut saw_utf8 = false;

        loop {
            let kind = self.peek();
            let piece = match kind {
                TokenType::String
                | TokenType::WideString
                | TokenType::Utf16String
                | TokenType::Utf32String => {
                    let token = self.consume();
                    saw_utf8 |= token.encoding_prefix() == "u8";
                    match &token.value {
                        TokenValue::String(s)
                        | TokenValue::WideString(s)
                        | TokenValue::Utf16String(s)
                        | TokenValue::Utf32String(s) => {
                            (token.pos, literal::parse_string_literal(s))
                        }
                        _ => return Err(ParseError::new("invalid string token", token.pos)),
                    }
                }
                _ => break,
            };
            pieces.push(piece);

            // Two wide prefixes that differ, or a `u8` anywhere in a run that
            // has a wide one -- judged once this piece's own prefix is
            // recorded, so `u8"a" L"b"` is caught as `L"a" u8"b"` is.
            let mut mixed = false;
            if kind != TokenType::String {
                mixed = encoding.is_some_and(|prev| prev != kind);
                encoding.get_or_insert(kind);
            }
            mixed |= saw_utf8 && encoding.is_some();
            if mixed && !mixed_reported {
                diag::error(
                    start_pos,
                    &gettext("concatenation of string literals with different encoding prefixes"),
                );
                mixed_reported = true;
            }
        }

        // Each piece is checked against the run's element type, since a plain
        // piece takes the prefix of the run it is in (6.4.5p5) -- and from its
        // own token's position, which `parse_string_literal` has no way to
        // report from.
        let unit_bits = match encoding {
            None => literal::CHAR_UNIT_BITS,
            Some(TokenType::WideString) => self.types.size_bits(self.types.wchar_id),
            Some(TokenType::Utf16String) => self.types.size_bits(self.types.char16_id),
            Some(_) => self.types.size_bits(self.types.char32_id),
        };
        let mut elements: Vec<literal::Escaped> = Vec::new();
        for (pos, piece) in pieces {
            literal::check_elements(&piece, unit_bits, pos);
            elements.extend(piece);
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
            // wchar_t[N], char16_t[N] and char32_t[N]. Their elements are
            // code units rather than bytes, so the UTF-8 the lexer preserved
            // is decoded here -- taking the bytes straight through gave
            // `L"café"` five elements, the first two the halves of a UTF-8
            // pair -- while a unit an escape names is kept as the number it
            // is: `L"\xffffffff"` is one element, all ones. A character
            // beyond the BMP becomes a surrogate pair in a `u"..."` literal.
            Some(TokenType::WideString) => {
                let units = literal::literal_wide_chars(&elements);
                let t = self
                    .types
                    .intern(Type::array(self.types.wchar_id, units.len() + 1));
                Ok(Self::typed_expr(
                    ExprKind::WideStringLit(units),
                    t,
                    start_pos,
                ))
            }
            Some(TokenType::Utf16String) => {
                let units = literal::literal_utf16_units(&elements);
                let t = self
                    .types
                    .intern(Type::array(self.types.char16_id, units.len() + 1));
                Ok(Self::typed_expr(
                    ExprKind::Utf16StringLit(units),
                    t,
                    start_pos,
                ))
            }
            Some(TokenType::Utf32String) => {
                let units = literal::literal_wide_chars(&elements);
                let t = self
                    .types
                    .intern(Type::array(self.types.char32_id, units.len() + 1));
                Ok(Self::typed_expr(
                    ExprKind::Utf32StringLit(units),
                    t,
                    start_pos,
                ))
            }
            Some(_) => unreachable!("only string token types reach here"),
        }
    }

    pub(super) fn parse_argument_list(&mut self) -> ParseResult<Vec<Expr>> {
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
            .current_ident()
            .ok_or_else(|| ParseError::new("invalid identifier", self.current_pos()))?;

        self.advance();
        Ok(id)
    }

    /// `e` as built, or left untyped when its operands were diagnosed: an
    /// untyped result keeps whatever encloses it from reporting the same
    /// mistake again.
    fn typed_if(mut e: Expr, valid: bool) -> Expr {
        if !valid {
            e.typ = None;
        }
        e
    }

    /// Create a typed expression with position
    pub(crate) fn typed_expr(kind: ExprKind, typ: TypeId, pos: Position) -> Expr {
        Expr {
            kind,
            typ: Some(typ),
            pos,
            bitfield_bits: None,
        }
    }

    /// Create a typed binary expression, computing result type from operands
    fn make_binary(&mut self, op: BinaryOp, left: Expr, right: Expr) -> Expr {
        let valid = self.check_binary_operands(op, &left, &right, left.pos);

        // A bit-field operand promotes before anything else looks at it
        // (C17 6.3.1.1p2), and that promotion is not derivable from the
        // operand's type alone -- the width lives on the member, not the type.
        //
        // Made explicit in the tree rather than only in the result type,
        // because a comparison takes its signedness from its operands and not
        // from its own `int` result: with the promotion left implicit,
        // `b.u7 > -1` still compared unsigned and answered false.
        let left = self.promote_bitfield_operand(left);
        let right = self.promote_bitfield_operand(right);

        let left_type = left.typ.unwrap_or(self.types.int_id);
        let result_type = self.binary_result_type(op, left_type, &right);

        // C17 6.7.2.1p10: a bit-field has a type of exactly its declared width,
        // and 6.2.5p9 then reduces an unsigned result modulo 2^width. So
        // `x.b << 32` with `unsigned long long b : 40` holding 0x100 is zero --
        // every set bit shifts out of the 40-bit type.
        //
        // Which operators carry the width, and from where:
        //   - the arithmetic and bitwise ones take the wider operand's width;
        //   - a shift takes the **left** operand's alone (6.5.7p3 -- the right
        //     operand's type never reaches the result);
        //   - a comparison or a logical operator yields `int` and carries
        //     nothing, which falls out of not asking.
        let width = match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Mod
            | BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor => self.combined_bitfield_width(&left, &right),
            BinaryOp::Shl | BinaryOp::Shr => self.effective_bitfield_width(&left),
            _ => None,
        }
        .filter(|bits| *bits < self.types.size_bits(result_type))
        .filter(|_| !self.types.is_vector(result_type));

        let pos = left.pos;
        let mut e = Self::typed_expr(
            ExprKind::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
            result_type,
            pos,
        );
        e.bitfield_bits = width;
        Self::typed_if(e, valid)
    }

    /// The type of `left op right`, where `left` has type `left_type` and
    /// the operands satisfy the operator's constraints.
    pub(super) fn binary_result_type(
        &mut self,
        op: BinaryOp,
        left_type: TypeId,
        right: &Expr,
    ) -> TypeId {
        let right_type = right.typ.unwrap_or(self.types.int_id);
        // A vector operation yields the vector's type -- the left operand's
        // when both are vectors, unless that one is a comparison's mask --
        // and a comparison a vector of masks.
        let vector = [left_type, right_type]
            .into_iter()
            .filter(|&t| self.types.is_vector(t))
            .min_by_key(|&t| self.types.is_vector_mask(t));
        if let Some(vector) = vector {
            let vector = self.types.unqualified(vector);
            return if op.is_comparison() {
                self.types.vector_mask_type(vector)
            } else {
                vector
            };
        }
        match op {
            // Comparison and logical operators always return int
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Ge
            | BinaryOp::LogAnd
            | BinaryOp::LogOr => self.types.int_id,

            // The additive operators take their pointer forms on the decayed
            // operands (6.3.2.1p3-4): an array is a pointer to its element and
            // a function a pointer to itself, so `f - g` is a `ptrdiff_t` and
            // `f + 1` a pointer, as `fp - gp` and `fp + 1` are.
            BinaryOp::Add | BinaryOp::Sub => {
                let left_type = self.types.decayed(left_type);
                let right_type = self.types.decayed(right_type);
                let left_is_ptr = self.types.kind(left_type) == TypeKind::Pointer;
                let right_is_ptr = self.types.kind(right_type) == TypeKind::Pointer;

                if left_is_ptr && self.types.is_integer(right_type) {
                    left_type
                } else if self.types.is_integer(left_type) && right_is_ptr && op == BinaryOp::Add {
                    right_type
                } else if left_is_ptr && right_is_ptr && op == BinaryOp::Sub {
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
                // The promoted type of a *value*: unqualified, as every
                // arithmetic result is (6.3.2.1p2).
                let promoted = self.types.integer_promote(left_type);
                let promoted = self.types.unqualified(promoted);
                self.check_shift_count(op, promoted, right);
                promoted
            }
        }
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
    /// The width and declared type of the bit-field `e` names, if it names one.
    /// The bit-field width this expression's value is confined to, if any.
    ///
    /// Either it names a bit-field member directly, or it is the result of an
    /// operation on one and carries the width forward. Only widths *wider*
    /// than `int` matter here: a narrower field promotes to `int`, which the
    /// type already expresses, and `bitfield_promoted_type` handles it.
    fn effective_bitfield_width(&mut self, e: &Expr) -> Option<u32> {
        if let Some(bits) = e.bitfield_bits {
            return Some(bits);
        }
        let (bits, typ) = self.bitfield_of(e)?;
        (bits < self.types.size_bits(typ) && bits > self.types.size_bits(self.types.int_id))
            .then_some(bits)
    }

    /// The width an operation's result is confined to.
    ///
    /// gcc takes the **wider** of the two operands -- `u40 * u33` reduces
    /// modulo 2^40, and so does `u33 * u40` -- which also makes the operation
    /// commutative, as it has to be. A plain operand contributes nothing, so
    /// `u33 * 1ULL` stays 33 bits rather than widening to 64.
    fn combined_bitfield_width(&mut self, left: &Expr, right: &Expr) -> Option<u32> {
        match (
            self.effective_bitfield_width(left),
            self.effective_bitfield_width(right),
        ) {
            (Some(a), Some(b)) => Some(a.max(b)),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        }
    }

    fn bitfield_of(&mut self, e: &Expr) -> Option<(u32, TypeId)> {
        let (base_typ, member) = match &e.kind {
            ExprKind::Member { expr, member } => (expr.typ?, *member),
            ExprKind::Arrow { expr, member } => (self.types.base_type(expr.typ?)?, *member),
            _ => return None,
        };
        let info = self.types.find_member(base_typ, member)?;
        Some((info.bit_width?, info.typ))
    }

    /// The type an operand contributes to an arithmetic expression.
    ///
    /// For anything but a bit-field this is just its own type. C17 6.3.1.1p2
    /// promotes a bit-field the way it promotes a narrow integer: to `int` if
    /// `int` can represent all its values, otherwise to `unsigned int`. What
    /// makes it a separate question from `integer_promote` is that the width
    /// is a property of the *member*, not of the type -- an `unsigned int f:7`
    /// has type `unsigned int`, so asking the type alone answers "no change"
    /// and `b.f - 2` comes out a huge unsigned value instead of -1.
    ///
    /// A field at least as wide as `int` is left alone, which covers both an
    /// `unsigned int f:32` (which stays unsigned, since `int` cannot hold all
    /// of it) and a `long long b:40` (whose declared type is already wider).
    /// The integer promotions, for the operand of unary `-` or `~`.
    ///
    /// C17 6.5.3.3p3 and p4: both perform the integer promotions on their
    /// operand, and the result has the promoted type. `-` and `~` had each
    /// open-coded that as a `TypeKind` test for `_Bool`/`char`/`short`, which
    /// is right as far as it goes and misses bit-fields entirely -- the width
    /// is a property of the member, not of the type, so no test on `TypeKind`
    /// can see it. `-v.u7 < 0` was false where C and gcc say true.
    ///
    /// Returns the operand, wrapped in a conversion if it needed one, together
    /// with the promoted type. Going through `promote_bitfield_operand` keeps
    /// the one rule in one place: the binary operators already use it, and two
    /// copies of a promotion rule is how this went wrong to begin with.
    fn promote_unary_operand(&mut self, operand: Expr) -> (Expr, TypeId) {
        // A vector is operated on lane by lane, with no promotion.
        if let Some(t) = operand.typ.filter(|&t| self.types.is_vector(t)) {
            let typ = self.types.unqualified(t);
            return (operand, typ);
        }
        let operand = self.promote_bitfield_operand(operand);
        let op_typ = operand.typ.unwrap_or(self.types.int_id);
        // A GNU complex integer is left alone. `integer_promote` switches on
        // `kind()`, which answers a complex type's *base* kind, so
        // `_Complex short` looked like a `short` and promoted to `int` -- and
        // the conversion below then kept only the real half, so `+z` and `-z`
        // came out `(3, 0)` and `(-3, 0)`. gcc and clang leave the type as it
        // is, and `default_argument_promote` already draws the same line.
        //
        // The guard belongs here and not in `integer_promote`, which the usual
        // arithmetic conversions call precisely to reduce a complex integer to
        // its promoted base before `pick_complex` re-wraps it.
        let typ = if self.types.is_complex(op_typ) {
            op_typ
        } else {
            self.types.integer_promote(op_typ)
        };
        // The result is a value, which has the unqualified type (6.3.2.1p2):
        // `-v` is an `int` even where `v` is a `volatile int`.
        let typ = self.types.unqualified(typ);
        // The *value* is promoted, not just the type it is computed at. The
        // conversion used to be left out, on the reasoning that the operand
        // is already in a wider register -- but nothing in the IR then says
        // how the narrow value is widened, and the move that does it has no
        // sign. `-(signed char)200` came out -200 where C says 56, while a
        // variable operand was right, because loading one knows its type.
        let operand = self.convert_operand(operand, typ);
        (operand, typ)
    }

    /// `e` converted to `typ`, or `e` unchanged when it is already that type.
    pub(super) fn convert_operand(&mut self, e: Expr, typ: TypeId) -> Expr {
        if e.typ == Some(typ) {
            return e;
        }
        let pos = e.pos;
        Self::typed_expr(
            ExprKind::Cast {
                cast_type: typ,
                expr: Box::new(e),
            },
            typ,
            pos,
        )
    }

    /// The width `-x` or `~x` yields: the operand's own. `-` and `~` on a
    /// 40-bit field are computed and reduced at 40 bits, exactly as a binary
    /// operator on it would be.
    fn unary_bitfield_width(&mut self, operand: &Expr, result_typ: TypeId) -> Option<u32> {
        self.effective_bitfield_width(operand)
            .filter(|bits| *bits < self.types.size_bits(result_typ))
    }

    /// Wrap a bit-field operand in the conversion C17 6.3.1.1p2 calls for.
    ///
    /// A no-op for anything else, and for a field that does not promote.
    fn promote_bitfield_operand(&mut self, e: Expr) -> Expr {
        let declared = e.typ.unwrap_or(self.types.int_id);
        let promoted = self.bitfield_promoted_type(&e);
        if promoted == declared {
            return e;
        }
        let pos = e.pos;
        Self::typed_expr(
            ExprKind::Cast {
                cast_type: promoted,
                expr: Box::new(e),
            },
            promoted,
            pos,
        )
    }

    fn bitfield_promoted_type(&mut self, e: &Expr) -> TypeId {
        let declared = e.typ.unwrap_or(self.types.int_id);
        let Some((bit_width, field_typ)) = self.bitfield_of(e) else {
            return declared;
        };
        let int_bits = self.types.size_bits(self.types.int_id);
        if bit_width < int_bits {
            self.types.int_id
        } else if bit_width == int_bits && self.types.is_unsigned(field_typ) {
            self.types.uint_id
        } else if bit_width == int_bits {
            self.types.int_id
        } else {
            declared
        }
    }

    /// The type the usual arithmetic conversions (C17 6.3.1.8) bring two
    /// operands to, as an *rvalue* type.
    ///
    /// `common_type` answers with one of the operands' own `TypeId`s, so
    /// `volatile int + int` came out `volatile int` and a qualifier the object
    /// carried leaked into the type of a value. C17 6.3.2.1p2 drops the
    /// qualifiers when an lvalue is converted to a value, and nothing
    /// downstream may read an rvalue's type as "this expression touched a
    /// volatile object" -- now that a member of a `volatile` object is itself
    /// volatile (6.5.2.3p3), that leak would reach every `s.m + 1`.
    ///
    /// Stripping them here rather than in `common_type` keeps the latter a
    /// pure question about conversion rank, which is what lets the linearizer
    /// and the constant folder ask it through a `&TypeTable`: interning a
    /// stripped type needs `&mut`.
    pub(crate) fn usual_arithmetic_conversions(&mut self, left: TypeId, right: TypeId) -> TypeId {
        let common = self.types.common_type(left, right);
        self.types.unqualified(common)
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

            if self.is_keyword(crate::kw::DEFAULT) {
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
                let (assoc_typ, dims) = self.parse_type_name_vm()?;
                // 6.5.1.1p2: an association names no variably modified type,
                // whose compatibility would turn on a run-time extent.
                if !dims.is_empty() {
                    diag::error(
                        assoc_pos,
                        &gettext("'_Generic' association has variable length type"),
                    );
                }
                // ... and names a complete object type: a function type or
                // an incomplete one can never be the controlling
                // expression's.
                if self.types.kind(assoc_typ) == TypeKind::Function {
                    diag::error(
                        assoc_pos,
                        &gettext("'_Generic' association has function type"),
                    );
                } else if self.type_name_is_incomplete(assoc_typ, dims.len()) {
                    diag::error(
                        assoc_pos,
                        &gettext("'_Generic' association has incomplete type"),
                    );
                }
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

                    // `__label__` declares local labels at the head of a
                    // block and nowhere else; anywhere a statement or an
                    // expression is wanted, gcc takes it as neither.
                    if name_id == crate::kw::GNU_LABEL {
                        return Err(ParseError::new(
                            gettext("expected expression before '__label__'"),
                            token_pos,
                        ));
                    }

                    // Try builtin dispatch first, unless a declaration in scope
                    // has claimed the name (see `builtin_is_shadowed`).
                    if !self.builtin_is_shadowed(name_id)
                        && crate::kw::exists_on(name_id, self.types.target().arch)
                    {
                        if let Some(result) = self.parse_builtin_expr(name_id, token_pos) {
                            return result;
                        }
                    }

                    // C17 6.4.2.2p1: `__func__` is implicitly declared
                    // `static const char __func__[] = "function-name";`, so
                    // its type is `const char[N]`: `sizeof __func__` is the
                    // name's length plus one, and it decays like any array.
                    // gcc's `__FUNCTION__` and `__PRETTY_FUNCTION__` are the
                    // same in C.
                    if name_id == crate::kw::FUNC
                        || name_id == crate::kw::FUNCTION
                        || name_id == crate::kw::PRETTY_FUNCTION
                    {
                        let typ = self.func_name_type(name_id, token_pos);
                        return Ok(Self::typed_expr(ExprKind::FuncName, typ, token_pos));
                    }

                    // Check if this is an enum constant - if so, return IntLit
                    if let Some(sym) = self.symbols.lookup_enum_constant(name_id) {
                        if let Some(value) = sym.enum_value {
                            // C17 6.4.4.3p2: an enumeration constant has type
                            // `int`, which is the symbol's type while its list
                            // is parsed; `Parser::enumerator_type` gives it
                            // its final one when the enumeration completes.
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
                    } else if diag::permissive() && self.is_special(b'(') {
                        // `-fpermissive`: C89 6.3.2.2 let a call to an
                        // undeclared function declare it implicitly as
                        // `extern int f();` -- unprototyped, so no argument is
                        // checked or converted. C99 6.5.1p2 removed the rule.
                        //
                        // Only a name followed by `(` gets this. A bare
                        // undeclared identifier was never implicitly declared
                        // by any C standard and stays an error, which is what
                        // keeps a misspelled variable from silently becoming a
                        // function.
                        let name_str = self.idents.get_opt(name_id).unwrap_or("").to_string();
                        diag::warning_args(
                            token_pos,
                            "implicit declaration of function '{0}'",
                            &[&name_str],
                        );
                        // A name c17 knows as a library builtin gets that
                        // builtin's return type, not `int`. gcc does the same,
                        // and it has to: `x = alloca(n)` or `p = malloc(n)`
                        // without a declaration truncated the returned address
                        // to 32 bits and the program died on the first
                        // dereference -- which is exactly the pre-C99 code
                        // `-fpermissive` exists to compile.
                        //
                        // The declaration stays unprototyped either way, so no
                        // argument is checked or converted, as C89 6.3.2.2
                        // says.
                        let ret_id = self
                            .library_return_type(&name_str)
                            .unwrap_or(self.types.int_id);
                        let func_type = self.types.intern(Type {
                            kind: TypeKind::Function,
                            base: Some(ret_id),
                            params: None,
                            ..Default::default()
                        });
                        // An implicit declaration is `extern int f();` (C89
                        // 6.3.2.2), so it has external linkage.
                        let symbol = crate::symbol::Symbol::function(
                            name_id,
                            func_type,
                            self.symbols.depth(),
                        )
                        .with_linkage(crate::symbol::Linkage::External);
                        let symbol_id = self.symbols.declare(symbol).unwrap_or_else(|_| {
                            self.symbols
                                .lookup_id(name_id, crate::symbol::Namespace::Ordinary)
                                .expect("declare failed but no existing symbol")
                        });
                        Ok(Self::typed_expr(
                            ExprKind::Ident(symbol_id),
                            func_type,
                            token_pos,
                        ))
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
                    // `token_pos`, not `current_pos`: the token has been
                    // consumed, so the current position is the *next* one --
                    // a different line, where the terminator is on one of its
                    // own.
                    let (v, is_code_point) = literal::char_literal_value(s, None, token_pos);
                    let value = if is_code_point {
                        // Not a byte, so plain `char`'s signedness does not
                        // reach it.
                        v as i64
                    } else {
                        self.types.plain_char().byte_value(v as u8)
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
                        let typ = match kind {
                            TokenType::WideChar => self.types.wchar_id,
                            TokenType::Utf16Char => self.types.char16_id,
                            _ => self.types.char32_id,
                        };
                        let bits = self.types.size_bits(typ);
                        // The consumed token's position, as above.
                        let (code_point, _) = literal::char_literal_value(s, Some(bits), token_pos);
                        let value = literal::prefixed_char_value(
                            code_point,
                            bits,
                            !self.types.is_unsigned(typ),
                        );
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
                    if let Some((typ, dims)) = self.try_parse_type_name_vm() {
                        self.expect_special(b')')?;

                        // Check for compound literal: (type){ ... }
                        if self.is_special(b'{') {
                            return self.parse_compound_literal_tail(typ, dims, paren_pos);
                        }

                        // Regular cast expression
                        let expr = self.parse_unary_expr()?;
                        // C17 6.5.4p2: a cast names a scalar type or `void`.
                        // An array was converted as if it were its first
                        // element's address. (A union stays: gcc casts to
                        // one, and so does c17 -- see `cast_to_union`.)
                        let vector = self.types.is_vector(typ)
                            || expr.typ.is_some_and(|t| self.types.is_vector(t));
                        match self.types.kind(typ) {
                            TypeKind::Union => {
                                let cast = self.cast_to_union(typ, expr, paren_pos);
                                return Ok(self.with_type_name_extents(dims, cast));
                            }
                            // A vector's own rules, below.
                            _ if vector => {}
                            TypeKind::Array => {
                                diag::error(paren_pos, &gettext("cast specifies array type"))
                            }
                            TypeKind::Function => {
                                diag::error(paren_pos, &gettext("cast specifies function type"))
                            }
                            _ => self.check_cast_operand(typ, &expr, paren_pos),
                        }
                        // gcc reinterprets the bits between a vector and a
                        // same-sized integer or vector. A cast to `void`
                        // reads nothing -- `(void)v;` is how an unused vector
                        // is marked used.
                        if let Some(from) = expr.typ {
                            let from = self.lvalue_converted_type(from);
                            let vector = self.types.is_vector(from) || self.types.is_vector(typ);
                            if vector && self.types.kind(typ) != TypeKind::Void {
                                self.check_vector_cast(from, typ, paren_pos);
                            }
                        }

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

                        // A cast yields a value, so a cast to a qualified type
                        // is a cast to its unqualified version (C17 6.5.4p5,
                        // footnote 108).
                        let typ = self.types.unqualified(typ);
                        let cast = Self::typed_expr(
                            ExprKind::Cast {
                                cast_type: typ,
                                expr: Box::new(expr),
                            },
                            typ,
                            paren_pos,
                        );
                        return Ok(self.with_type_name_extents(dims, cast));
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

    /// Give a floating literal a zero real part, if it carried an imaginary
    /// marker.
    ///
    /// `__builtin_complex(0, v)` is exactly the value wanted and already
    /// exists, so no new expression node is needed -- and it is the node
    /// `<complex.h>` builds `I` and `CMPLX` from, so the constant folds the
    /// same way theirs do.
    fn imaginary_if(&self, lit: Expr, is_imaginary: bool, base_typ: TypeId, pos: Position) -> Expr {
        if !is_imaginary {
            return lit;
        }
        // The real part has to be a zero of the *base's* family: `2i` is a
        // `_Complex int`, and giving its real half a `FloatLit` made the
        // constant folder carry an integer complex as two `FloatVal`s.
        let zero_kind = if self.types.is_integer(base_typ) {
            ExprKind::IntLit(0)
        } else {
            ExprKind::FloatLit(FloatVal::ZERO)
        };
        let zero = Self::typed_expr(zero_kind, base_typ, pos);
        Self::typed_expr(
            ExprKind::BuiltinComplex {
                real: Box::new(zero),
                imag: Box::new(lit),
            },
            self.types.make_complex(base_typ),
            pos,
        )
    }

    /// Parse a number literal string into an expression
    fn parse_number_literal(&self, s: &str, pos: Position) -> ParseResult<Expr> {
        let spelling = NumberSpelling::split(s);
        let bad = || {
            let what = if spelling.is_float {
                "float"
            } else {
                "integer"
            };
            ParseError::new(format!("invalid {what} literal: {s}"), pos)
        };
        // A GNU imaginary constant: a number with an `i` or `j` in its suffix.
        //
        // C's own spelling of this is `_Imaginary`, which C17 6.4.1 reserves
        // and Annex G makes optional; c17 does not provide the type, and gcc
        // does not either. Both give the constant a *complex* type with a zero
        // real part, which is what `__builtin_complex(0, v)` already builds.
        let (suffix, is_imaginary) = strip_imaginary_marker(spelling.suffix).ok_or_else(bad)?;
        let body = spelling.body.to_ascii_lowercase();
        let is_hex = spelling.is_hex;

        if spelling.is_float {
            let float_suffix = FloatSuffix::parse(&suffix).ok_or_else(bad)?;
            let value: FloatVal = if is_hex {
                // Hex float parsing: 0x[hex-digits].[hex-digits]p[±exponent]
                // Value = significand × 2^exponent.
                // `parse_hex_float_parts` is exact, so the literal reaches the
                // target format without passing through `f64` -- which would
                // flush `0x1p-16382L` to zero before its type is even known.
                let (mantissa, exp2) = Self::parse_hex_float_parts(&body).map_err(|_| {
                    ParseError::new(format!("invalid hex float literal: {}", s), pos)
                })?;
                FloatVal::from_parts(false, mantissa, exp2)
            } else {
                // Exact, like the hex path: the digits are scaled by a power
                // of ten in full precision and only rounded once, at the
                // target's width. Going through `f64` cost a `long double`
                // eleven of its significand bits and flushed anything outside
                // double's range before the type was even known.
                let (mantissa, exp2) = crate::float::parse_decimal_float_parts(&body)
                    .map_err(|_| ParseError::new(format!("invalid float literal: {}", s), pos))?;
                FloatVal::from_parts(false, mantissa, exp2)
            };
            let typ = match float_suffix {
                FloatSuffix::None => self.types.double_id,
                FloatSuffix::F => self.types.float_id,
                FloatSuffix::L => self.types.longdouble_id,
                FloatSuffix::F32 => self
                    .types
                    .floating(TypeKind::Float, FloatClass::Interchange),
                FloatSuffix::F64 => self
                    .types
                    .floating(TypeKind::Double, FloatClass::Interchange),
                FloatSuffix::F32x => self.types.floating(TypeKind::Double, FloatClass::Extended),
                FloatSuffix::F64x => {
                    if !self.types.has_float64x() {
                        return Err(ParseError::new(
                            format!("_Float64x is not supported on this target: {}", s),
                            pos,
                        ));
                    }
                    self.types
                        .floating(TypeKind::LongDouble, FloatClass::Extended)
                }
                FloatSuffix::F16 => self.types.float16_id,
                FloatSuffix::F128 => {
                    if !self.types.has_float128() {
                        return Err(ParseError::new(
                            format!("__float128 is not supported on this target: {}", s),
                            pos,
                        ));
                    }
                    self.types.float128_id
                }
            };
            let lit = Self::typed_expr(ExprKind::FloatLit(value), typ, pos);
            Ok(self.imaginary_if(lit, is_imaginary, typ, pos))
        } else {
            let IntSuffix {
                unsigned: is_unsigned,
                long,
            } = IntSuffix::parse(&suffix).ok_or_else(bad)?;
            let is_longlong = long == IntLong::LongLong;
            let is_long = long == IntLong::Long;

            // Parse as u64 first to handle large unsigned values, then reinterpret as i64
            let value_u64: u64 = if is_hex {
                // Strip 0x or 0X prefix
                u64::from_str_radix(&body[2..], 16)
            } else if let Some(bin_part) = body.strip_prefix("0b") {
                u64::from_str_radix(bin_part, 2)
            } else if body.starts_with('0') && body.len() > 1 {
                u64::from_str_radix(&body, 8)
            } else {
                body.parse()
            }
            .map_err(|_| ParseError::new(format!("invalid integer literal: {}", s), pos))?;

            // Reinterpret bits as i64 (preserves bit pattern for unsigned values)
            let value = value_u64 as i64;

            // C17 6.4.4.1p6: a decimal constant without `u` that no signed
            // type of its list can hold has no type at all. gcc gives it
            // `__int128`, with a warning, and so does c17 -- it used to wrap
            // to a negative `long long`, so `18446744073709551615 > 0` was 0.
            let is_decimal = !is_hex && !body.starts_with('0') && !body.starts_with("0b");
            if is_decimal && !is_unsigned && value_u64 > i64::MAX as u64 {
                diag::warning(
                    pos,
                    &gettext("integer constant is so large that it is unsigned"),
                );
                let typ = self.types.int128_id;
                let lit = Self::typed_expr(ExprKind::Int128Lit(i128::from(value_u64)), typ, pos);
                return Ok(self.imaginary_if(lit, is_imaginary, typ, pos));
            }

            // Determine type according to C99 6.4.4.1:
            // - Decimal constants: int, long int, long long int (signed only)
            // - Hex/Octal constants: int, unsigned int, long int, unsigned long int,
            //   long long int, unsigned long long int (both signed and unsigned)
            // The type is the first in the list that can represent the value.
            let is_octal = !is_hex && body.starts_with('0') && body.len() > 1;
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
            // `2i` is a `_Complex int` in gcc: an integer imaginary constant,
            // whose real half is an integer zero.
            let lit = Self::typed_expr(ExprKind::IntLit(value), typ, pos);
            Ok(self.imaginary_if(lit, is_imaginary, typ, pos))
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
}

/// A numeric constant's spelling, split into the number and its suffix.
///
/// Where the suffix begins is a property of the number's form, not of which
/// letters happen to end it: before a hex constant's `p`, `a`-`f` are digits,
/// so `0x1f16` is an integer and `0x1p0f16` a `_Float16`; and a suffix such as
/// `f16` has digits of its own, so "the trailing run of letters" is not it.
/// Classifying the suffix by `ends_with` tests found neither boundary: it
/// rejected `0x1p0f16`, took `1f16` for a floating constant, and could not
/// see the imaginary marker in `2.0if16`.
struct NumberSpelling<'a> {
    /// The digits, radix point and exponent.
    body: &'a str,
    /// Everything after them.
    suffix: &'a str,
    is_hex: bool,
    /// A radix point or an exponent makes a floating constant (C17 6.4.4.2);
    /// the suffix does not.
    is_float: bool,
}

impl<'a> NumberSpelling<'a> {
    fn split(s: &'a str) -> Self {
        let b = s.as_bytes();
        let prefixed = |c: u8| b.len() > 1 && b[0] == b'0' && b[1].eq_ignore_ascii_case(&c);
        let is_hex = prefixed(b'x');
        // The digits of the number's radix, the letter that opens its
        // exponent, and where its digits start.
        let (is_digit, exponent, start): (fn(&u8) -> bool, Option<u8>, usize) = if is_hex {
            (u8::is_ascii_hexdigit, Some(b'p'), 2)
        } else if prefixed(b'b') {
            (u8::is_ascii_digit, None, 2)
        } else {
            (u8::is_ascii_digit, Some(b'e'), 0)
        };
        let mut end = start;
        while end < b.len() && (is_digit(&b[end]) || b[end] == b'.') {
            end += 1;
        }
        let mut is_float = s[start..end].contains('.');
        if exponent.is_some_and(|e| end < b.len() && b[end].eq_ignore_ascii_case(&e)) {
            // An exponent is the letter, an optional sign and at least one
            // decimal digit; anything less is left in the suffix, which then
            // fails to classify.
            let mut j = end + 1;
            if j < b.len() && matches!(b[j], b'+' | b'-') {
                j += 1;
            }
            if j < b.len() && b[j].is_ascii_digit() {
                while j < b.len() && b[j].is_ascii_digit() {
                    j += 1;
                }
                end = j;
                is_float = true;
            }
        }
        NumberSpelling {
            body: &s[..end],
            suffix: &s[end..],
            is_hex,
            is_float,
        }
    }
}

/// The suffixes of a floating constant (C17 6.4.4.2), with the `_FloatN`
/// and `_FloatNx` spellings of TS 18661-3 that c17 has types for and GNU's
/// `q`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum FloatSuffix {
    None,
    F,
    L,
    F16,
    F32,
    F64,
    F32x,
    F64x,
    /// `f128`, or GNU's `q`.
    F128,
}

impl FloatSuffix {
    fn parse(suffix: &str) -> Option<Self> {
        Some(match suffix.to_ascii_lowercase().as_str() {
            "" => FloatSuffix::None,
            "f" => FloatSuffix::F,
            "l" => FloatSuffix::L,
            "f16" => FloatSuffix::F16,
            "f32" => FloatSuffix::F32,
            "f64" => FloatSuffix::F64,
            "f32x" => FloatSuffix::F32x,
            "f64x" => FloatSuffix::F64x,
            "f128" | "q" => FloatSuffix::F128,
            _ => return None,
        })
    }
}

/// How many `l`s an integer suffix has.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum IntLong {
    None,
    Long,
    LongLong,
}

/// An integer constant's suffix (C17 6.4.4.1): an optional `u` before or
/// after an optional `l` or `ll`, whose two letters share a case.
struct IntSuffix {
    unsigned: bool,
    long: IntLong,
}

impl IntSuffix {
    fn parse(suffix: &str) -> Option<Self> {
        let (unsigned, rest) = match suffix
            .strip_prefix(['u', 'U'])
            .or_else(|| suffix.strip_suffix(['u', 'U']))
        {
            Some(rest) => (true, rest),
            None => (false, suffix),
        };
        let long = match rest {
            "" => IntLong::None,
            "l" | "L" => IntLong::Long,
            "ll" | "LL" => IntLong::LongLong,
            _ => return None,
        };
        Some(IntSuffix { unsigned, long })
    }
}

/// A suffix without its GNU imaginary marker (`i`, `I`, `j` or `J`), and
/// whether it had one; `None` if the marker is misplaced.
///
/// gcc takes the marker on either side of any other suffix and between an
/// integer's `u` and `l` -- `2.0if16`, `2.0f16i`, `1.0Li`, `1uil` -- but not
/// twice, and not inside another suffix: `2.0fi16`, `2.0f1i6` and `1lil` are
/// errors. So the marker must fall on a boundary between the tokens of the
/// suffix that remains without it.
fn strip_imaginary_marker(suffix: &str) -> Option<(String, bool)> {
    let is_marker = |c: char| matches!(c, 'i' | 'I' | 'j' | 'J');
    let mut markers = suffix.match_indices(is_marker);
    let Some((at, _)) = markers.next() else {
        return Some((suffix.to_string(), false));
    };
    if markers.next().is_some() {
        return None;
    }
    let rest = format!("{}{}", &suffix[..at], &suffix[at + 1..]);
    let on_boundary = at == 0
        || suffix_tokens(&rest)
            .iter()
            .scan(0, |end, t| {
                *end += t.len();
                Some(*end)
            })
            .any(|end| end == at);
    on_boundary.then_some((rest, true))
}

/// A constant's suffix cut into the tokens it is built from: `ll`/`LL`, a
/// letter with the digits (and `x`) that follow it, such as `f16` or `f32x`,
/// and any other single character.
fn suffix_tokens(suffix: &str) -> Vec<&str> {
    let b = suffix.as_bytes();
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < b.len() {
        let start = i;
        let c = b[i];
        if !c.is_ascii() {
            // Not a suffix anything accepts; keep it whole so that no slice
            // splits a character.
            tokens.push(&suffix[start..]);
            break;
        }
        i += 1;
        if matches!(c, b'l' | b'L') && b.get(i) == Some(&c) {
            i += 1;
        } else if c.is_ascii_alphabetic() {
            let digits = i;
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
            if i > digits && i < b.len() && b[i].eq_ignore_ascii_case(&b'x') {
                i += 1;
            }
        }
        tokens.push(&suffix[start..i]);
    }
    tokens
}
