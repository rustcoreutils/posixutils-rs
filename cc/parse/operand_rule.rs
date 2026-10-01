//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The operand constraints of the C operators.
//
// Every unary and binary operator states what types its operands may have
// (C17 6.5.3.3p1, 6.5.5p2 through 6.5.15p2, 6.5.16.2p1-2). The rules are
// written here once, as answers about types, and the parser asks them from
// the one place each operator is built. Nothing here reports: the parser
// owns the wording and the severity.
//

use super::ast::BinaryOp;
use crate::types::{TypeId, TypeKind, TypeTable};

/// The kind of type an operator requires of an operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OperandClass {
    /// An integer type: `%`, the shifts and the bitwise operators.
    Integer,
    /// An integer or a complex type: `~`, which gcc gives a complex operand
    /// as its conjugate.
    IntegerOrComplex,
    /// A real type -- integer or real floating, not complex: the relational
    /// operators, since a complex value has no ordering.
    Real,
    /// An arithmetic type: unary `+` and `-`, `*`, `/`, and the arithmetic
    /// forms of `+`, `-`, `==` and `!=`.
    Arithmetic,
    /// An arithmetic or pointer type: `!`, `&&`, `||`, `++`, `--` and the
    /// first operand of `?:`.
    Scalar,
}

impl OperandClass {
    /// Does a value of type `typ`, already decayed, belong to this class?
    pub(crate) fn admits(self, types: &TypeTable, typ: TypeId) -> bool {
        let integer = is_real_integer(types, typ);
        match self {
            OperandClass::Integer => integer,
            OperandClass::IntegerOrComplex => integer || types.is_complex(typ),
            OperandClass::Real => integer || types.is_float(typ),
            OperandClass::Arithmetic => types.is_arithmetic(typ),
            OperandClass::Scalar => types.is_scalar(typ),
        }
    }
}

/// An integer type that is not a GNU complex integer. `is_integer` answers by
/// kind, and a `_Complex int` has kind `Int`.
fn is_real_integer(types: &TypeTable, typ: TypeId) -> bool {
    types.is_integer(typ) && !types.is_complex(typ)
}

/// The class a binary operator requires of both operands when its pointer
/// forms do not apply.
pub(crate) fn binary_operand_class(op: BinaryOp) -> OperandClass {
    match op {
        BinaryOp::Mod
        | BinaryOp::Shl
        | BinaryOp::Shr
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor => OperandClass::Integer,
        BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => OperandClass::Real,
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => OperandClass::Arithmetic,
        BinaryOp::Eq | BinaryOp::Ne => OperandClass::Arithmetic,
        BinaryOp::LogAnd | BinaryOp::LogOr => OperandClass::Scalar,
    }
}

/// One operand of a binary operator, as the constraints see it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Operand {
    /// The operand's type after array and function decay.
    pub typ: TypeId,
    /// Is the operand a null pointer constant (C17 6.3.2.3p3)?
    pub null_constant: bool,
}

/// What a binary operator makes of a pair of operands, in gcc's severities.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OperandVerdict {
    /// The operands satisfy the operator's constraints.
    Valid,
    /// A constraint violation gcc rejects: "invalid operands to binary".
    Invalid,
    /// A pointer compared with an integer that is not a null pointer
    /// constant (6.5.8p2, 6.5.9p2). gcc warns and compares the bits.
    PointerInteger,
    /// Two pointers compared whose referenced types are not compatible.
    /// gcc warns and compares the addresses.
    DistinctPointers,
}

/// Check a binary operator's operands against C17 6.5.5p2 through 6.5.14p2.
pub(crate) fn binary_operand_verdict(
    types: &TypeTable,
    op: BinaryOp,
    left: Operand,
    right: Operand,
) -> OperandVerdict {
    let class = binary_operand_class(op);
    if class.admits(types, left.typ) && class.admits(types, right.typ) {
        return OperandVerdict::Valid;
    }
    pointer_form_verdict(types, op, left, right).unwrap_or(OperandVerdict::Invalid)
}

/// The pointer forms of `+` (6.5.6p2), `-` (6.5.6p3), the relational
/// operators (6.5.8p2) and the equality operators (6.5.9p2); `None` when the
/// operands are not in one of those shapes.
fn pointer_form_verdict(
    types: &TypeTable,
    op: BinaryOp,
    left: Operand,
    right: Operand,
) -> Option<OperandVerdict> {
    let is_ptr = |o: Operand| types.kind(o.typ) == TypeKind::Pointer;
    let is_int = |o: Operand| is_real_integer(types, o.typ);
    let (lp, rp) = (is_ptr(left), is_ptr(right));
    let pointer_and_integer = (lp && is_int(right)) || (is_int(left) && rp);
    match op {
        BinaryOp::Add if pointer_and_integer => Some(OperandVerdict::Valid),
        BinaryOp::Sub if lp && is_int(right) => Some(OperandVerdict::Valid),
        BinaryOp::Sub if lp && rp => Some(if pointees_compatible(types, left, right) {
            OperandVerdict::Valid
        } else {
            OperandVerdict::Invalid
        }),
        _ if op.is_comparison() && lp && rp => {
            Some(if pointers_comparable(types, op, left, right) {
                OperandVerdict::Valid
            } else {
                OperandVerdict::DistinctPointers
            })
        }
        _ if op.is_comparison() && pointer_and_integer => {
            let integer = if lp { right } else { left };
            Some(if integer.null_constant {
                OperandVerdict::Valid
            } else {
                OperandVerdict::PointerInteger
            })
        }
        _ => None,
    }
}

/// Do two pointers point at compatible types, qualifiers aside?
fn pointees_compatible(types: &TypeTable, left: Operand, right: Operand) -> bool {
    match (types.base_type(left.typ), types.base_type(right.typ)) {
        (Some(l), Some(r)) => types.types_compatible(l, r),
        _ => false,
    }
}

/// May two pointers be compared by `op`? Equality also takes a pointer to
/// `void` against any other (6.5.9p2); ordering needs compatible types.
fn pointers_comparable(types: &TypeTable, op: BinaryOp, left: Operand, right: Operand) -> bool {
    let points_to_void = |o: Operand| {
        types
            .base_type(o.typ)
            .is_some_and(|b| types.kind(b) == TypeKind::Void)
    };
    pointees_compatible(types, left, right)
        || (matches!(op, BinaryOp::Eq | BinaryOp::Ne)
            && (points_to_void(left) || points_to_void(right)))
}

/// The unary operators, by the constraint each places on its operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnaryOperator {
    Plus,
    Minus,
    Complement,
    Not,
    Increment,
    Decrement,
}

impl UnaryOperator {
    /// C17 6.5.3.3p1 for `+`, `-`, `~` and `!`; 6.5.2.4p1 and 6.5.3.1p1 for
    /// `++` and `--`, which want a real or pointer type and to which gcc also
    /// admits a complex one.
    pub(crate) fn operand_class(self) -> OperandClass {
        match self {
            UnaryOperator::Plus | UnaryOperator::Minus => OperandClass::Arithmetic,
            UnaryOperator::Complement => OperandClass::IntegerOrComplex,
            UnaryOperator::Not | UnaryOperator::Increment | UnaryOperator::Decrement => {
                OperandClass::Scalar
            }
        }
    }

    /// gcc's name for the operator in "wrong type argument to ...".
    pub(crate) fn name(self) -> &'static str {
        match self {
            UnaryOperator::Plus => "unary plus",
            UnaryOperator::Minus => "unary minus",
            UnaryOperator::Complement => "bit-complement",
            UnaryOperator::Not => "unary exclamation mark",
            UnaryOperator::Increment => "increment",
            UnaryOperator::Decrement => "decrement",
        }
    }
}
