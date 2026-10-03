//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The operand rules of GNU `vector_size` values: which operators take them,
// how a scalar operand is spread across the lanes, and which casts reinterpret
// one. gcc's rules, measured against gcc rather than read from its manual.
//

use super::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use super::operand_rule::UnaryOperator;
use super::parser::Parser;
use crate::diag;
use crate::token::lexer::Position;
use crate::types::TypeId;
use gettextrs::gettext;

/// Why a scalar cannot be spread across the lanes of a vector.
enum SplatFault {
    /// Not an arithmetic value at all: gcc's "invalid operands".
    NotArithmetic,
    /// A floating value for integer lanes.
    FloatToInteger,
    /// A value the lane type cannot hold: a constant out of its range, or a
    /// type with more precision than it.
    Truncation,
}

impl Parser<'_> {
    /// Check a binary operator with at least one vector operand, the operand
    /// types `lt` and `rt` being lvalue-converted. Answers whether it is
    /// valid.
    ///
    /// Two vectors must have as many lanes of the same width, and the same
    /// lane type unless both are integers -- gcc lets `int` and `unsigned`
    /// lanes mix, the result taking the left operand's type. A scalar beside
    /// a vector is spread across its lanes, if the lane type holds it (see
    /// [`Self::splat_fault`]); the count of a vector shift is exempt. `%`,
    /// the bitwise operators and the shifts want integer lanes, and `&&` and
    /// `||` take no vector at all, which [`Self::check_truth_value`] reports.
    pub(super) fn check_vector_operands(
        &mut self,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        (lt, rt): (TypeId, TypeId),
        pos: Position,
    ) -> bool {
        let integer_only = matches!(
            op,
            BinaryOp::Mod
                | BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::Shl
                | BinaryOp::Shr
        );
        let shift = matches!(op, BinaryOp::Shl | BinaryOp::Shr);
        let lanes = (self.types.vector_lanes(lt), self.types.vector_lanes(rt));
        let lane = match lanes {
            (Some((l, _)), _) | (None, Some((l, _))) => l,
            (None, None) => return true,
        };
        if integer_only && !self.types.is_integer(lane) {
            self.report_vector_operands(op, lt, rt, pos);
            return false;
        }
        let fault = match lanes {
            (Some((le, ln)), Some((re, rn))) => {
                let same_width = ln == rn && self.types.size_bits(le) == self.types.size_bits(re);
                let both_integer = self.types.is_integer(le) && self.types.is_integer(re);
                if same_width && (both_integer || self.types.types_compatible(le, re)) {
                    return true;
                }
                SplatFault::NotArithmetic
            }
            // A shift count may be any integer.
            (Some(_), None) if shift && self.types.is_integer(rt) => return true,
            (Some(_), None) => match self.splat_fault(right, rt, lane) {
                None => return true,
                Some(fault) => fault,
            },
            (None, _) => match self.splat_fault(left, lt, lane) {
                None => return true,
                Some(fault) => fault,
            },
        };
        let (vector, scalar) = if lanes.0.is_some() {
            (lt, rt)
        } else {
            (rt, lt)
        };
        self.report_splat_fault(fault, op, (lt, rt), (vector, scalar), pos);
        false
    }

    fn report_vector_operands(&mut self, op: BinaryOp, lt: TypeId, rt: TypeId, pos: Position) {
        let names = [lt, rt].map(|t| self.types.format_type(t, Some(self.idents)));
        diag::error_args(
            pos,
            "invalid operands to binary {0} (have '{1}' and '{2}')",
            &[op.spelling(), &names[0], &names[1]],
        );
    }

    fn report_splat_fault(
        &mut self,
        fault: SplatFault,
        op: BinaryOp,
        (lt, rt): (TypeId, TypeId),
        (vector, scalar): (TypeId, TypeId),
        pos: Position,
    ) {
        match fault {
            SplatFault::NotArithmetic => self.report_vector_operands(op, lt, rt, pos),
            SplatFault::FloatToInteger => {
                diag::error(pos, &gettext("cannot convert value to a vector"))
            }
            SplatFault::Truncation => {
                let [s, v] = [scalar, vector].map(|t| self.types.format_type(t, Some(self.idents)));
                diag::error_args(
                    pos,
                    "conversion of scalar '{0}' to vector '{1}' involves truncation",
                    &[&s, &v],
                );
            }
        }
    }

    /// Whether, and why not, the scalar `e` of type `typ` can be spread
    /// across lanes of type `lane`. gcc's test: a constant must be held
    /// exactly by the lane type; any other value must not have more
    /// precision than it, counting a floating lane's significand; and no
    /// floating value goes into integer lanes.
    fn splat_fault(&self, e: &Expr, typ: TypeId, lane: TypeId) -> Option<SplatFault> {
        let types = &*self.types;
        if !types.is_arithmetic(typ) || types.is_complex(typ) || types.is_vector(typ) {
            return Some(SplatFault::NotArithmetic);
        }
        let scalar_float = types.is_float(typ);
        let Some(lane_format) = types.fp_format(lane) else {
            // Integer lanes.
            if scalar_float {
                return Some(SplatFault::FloatToInteger);
            }
            let bits = types.size_bits(lane);
            let fits = match self.eval_const_expr(e) {
                // Either reading of the lane's bits will do: gcc spreads
                // `-1` across unsigned lanes.
                Some(c) => {
                    let lo = -(1i128 << (bits - 1));
                    let hi = (1i128 << bits) - 1;
                    (lo..=hi).contains(&c)
                }
                None => types.size_bits(typ) <= bits,
            };
            return (!fits).then_some(SplatFault::Truncation);
        };
        let precision = lane_format.precision();
        let fits = if scalar_float {
            match float_constant(e) {
                Some(v) => {
                    let rounded = v.round_to_format(lane_format);
                    rounded.cmp_value(v) == Some(std::cmp::Ordering::Equal)
                }
                None => types
                    .fp_format(typ)
                    .is_some_and(|f| f.precision() <= precision),
            }
        } else {
            match self.eval_const_expr(e) {
                Some(c) => {
                    let v = crate::float::FloatVal::from_i128(c);
                    let rounded = v.round_to_format(lane_format);
                    rounded.cmp_value(v) == Some(std::cmp::Ordering::Equal)
                }
                None => types.size_bits(typ) <= precision,
            }
        };
        (!fits).then_some(SplatFault::Truncation)
    }

    /// Check a cast between `from` and `to`, at least one a vector: gcc
    /// reinterprets the bits of another vector or of an integer of the same
    /// size, and converts nothing else. Answers whether it is valid.
    pub(super) fn check_vector_cast(&mut self, from: TypeId, to: TypeId, pos: Position) -> bool {
        let types = &*self.types;
        let same_size = types.size_bits(from) == types.size_bits(to);
        let (vector, other) = if types.is_vector(to) {
            (to, from)
        } else {
            (from, to)
        };
        let other_ok =
            types.is_vector(other) || (types.is_integer(other) && !types.is_complex(other));
        if other_ok && same_size {
            return true;
        }
        let [v, o] = [vector, other].map(|t| self.types.format_type(t, Some(self.idents)));
        if self.types.is_vector(to) {
            if other_ok {
                diag::error_args(
                    pos,
                    "cannot convert a value of type '{0}' to vector type '{1}' which has different size",
                    &[&o, &v],
                );
            } else {
                diag::error(pos, &gettext("cannot convert value to a vector"));
            }
        } else if self.types.is_float(to) {
            diag::error(
                pos,
                &gettext("aggregate value used where a floating-point was expected"),
            );
        } else if self.types.kind(to) == crate::types::TypeKind::Pointer {
            diag::error(pos, &gettext("cannot convert to a pointer type"));
        } else {
            diag::error_args(
                pos,
                "cannot convert a vector of type '{0}' to type '{1}' which has different size",
                &[&v, &o],
            );
        }
        false
    }

    /// Check unary `op` on a vector of type `typ`: `-`, `+`, `++` and `--`
    /// take any lanes, `~` integer lanes, and `!` none. Answers whether it is
    /// valid.
    pub(super) fn check_vector_unary(&self, op: UnaryOperator, typ: TypeId, pos: Position) -> bool {
        let Some((lane, _)) = self.types.vector_lanes(typ) else {
            return true;
        };
        let ok = match op {
            UnaryOperator::Complement => self.types.is_integer(lane),
            UnaryOperator::Not => false,
            _ => true,
        };
        if !ok {
            diag::error_args(pos, "wrong type argument to {0}", &[op.name()]);
        }
        ok
    }
}

/// The value of a floating constant, possibly negated, or `None` for any
/// other expression.
fn float_constant(e: &Expr) -> Option<crate::float::FloatVal> {
    match &e.kind {
        ExprKind::FloatLit(v) => Some(*v),
        ExprKind::Unary {
            op: UnaryOp::Neg,
            operand,
        } => float_constant(operand).map(|v| v.negated()),
        _ => None,
    }
}
