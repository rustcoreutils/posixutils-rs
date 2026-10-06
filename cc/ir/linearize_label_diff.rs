//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU label differences in static initializers
//

//! `&&a - &&b` as a constant.
//!
//! Neither label has an address before the function is assembled, but the
//! distance between two labels of one function is fixed once it is, so the
//! assembler can write it: `.long .La - .Lb`. glibc's vfprintf builds its
//! jump tables this way -- `static const int t[] = { &&x - &&base, ... }`,
//! dispatched by `goto *(&&base + t[i])` -- and gcc accepts the difference,
//! plus or minus an integer constant, stored to an integer of any width the
//! assembler has a directive for.
//!
//! Both labels are always in the same function: a label's scope is its
//! function, so `&&y` naming a label of another function is a label used but
//! not defined, diagnosed as any such label is.

use super::linearize::Linearizer;
use super::Initializer;
use crate::diag::error;
use crate::parse::ast::{BinaryOp, Expr, ExprKind, LabelId, UnaryOp};
use crate::types::{TypeId, TypeKind};

/// An integer-valued sum of label addresses, each counted with a sign, and a
/// byte constant.
#[derive(Debug, Default)]
struct LabelSum {
    /// Each label with its net coefficient. A label that cancels stays here
    /// with a zero coefficient, so it is still checked and kept alive.
    labels: Vec<(LabelId, i64)>,
    addend: i64,
}

impl LabelSum {
    fn constant(addend: i64) -> Self {
        Self {
            labels: Vec::new(),
            addend,
        }
    }

    fn label(label: LabelId) -> Self {
        Self {
            labels: vec![(label, 1)],
            addend: 0,
        }
    }

    fn has_labels(&self) -> bool {
        !self.labels.is_empty()
    }

    /// `self * factor`, or `None` on overflow.
    fn scaled(mut self, factor: i64) -> Option<Self> {
        for (_, coeff) in &mut self.labels {
            *coeff = coeff.checked_mul(factor)?;
        }
        self.addend = self.addend.checked_mul(factor)?;
        Some(self)
    }

    /// `self + other`, folding a label that appears in both.
    fn plus(mut self, other: Self) -> Option<Self> {
        for (label, coeff) in other.labels {
            match self.labels.iter_mut().find(|(l, _)| *l == label) {
                Some((_, c)) => *c = c.checked_add(coeff)?,
                None => self.labels.push((label, coeff)),
            }
        }
        self.addend = self.addend.checked_add(other.addend)?;
        Some(self)
    }

    /// The labels left after cancellation, as `(end, start)` of a
    /// difference: `Some(None)` when every label cancelled, `None` when what
    /// is left is not one label minus another.
    fn as_difference(&self) -> Option<Option<(LabelId, LabelId)>> {
        let live: Vec<_> = self.labels.iter().filter(|(_, c)| *c != 0).collect();
        match live.as_slice() {
            [] => Some(None),
            [(a, 1), (b, -1)] => Some(Some((*a, *b))),
            [(a, -1), (b, 1)] => Some(Some((*b, *a))),
            _ => None,
        }
    }
}

impl Linearizer<'_> {
    /// The initializer for `expr` when it is the difference of two label
    /// addresses, plus or minus a constant, initializing an object of type
    /// `typ`; `None` when `expr` is not shaped so, which leaves it to the
    /// ordinary initializer paths and their diagnostics.
    pub(crate) fn label_difference_init(
        &mut self,
        expr: &Expr,
        typ: TypeId,
    ) -> Option<Initializer> {
        if !self.mentions_label_address(expr) {
            return None;
        }
        let width = self.types.size_bytes(typ);
        let sum = self.label_sum(expr, width)?;
        let shape = sum.as_difference()?;
        // A difference is an integer, and only an integer the assembler has
        // a data directive for can hold one. gcc's wording, for `_Bool`, the
        // floating types and `__int128` alike.
        let fits = self.types.is_integer(typ)
            && self.types.kind(typ) != TypeKind::Bool
            && matches!(width, 1 | 2 | 4 | 8);
        if !fits {
            if self.types.kind(typ) == TypeKind::Pointer {
                // An integer is not an address: gcc rejects
                // `void *p = (void *)(&&b - &&a);` as not constant, and so
                // does the ordinary pointer path.
                return None;
            }
            error(
                self.expr_pos(expr),
                "initializer element is not computable at load time",
            );
            return Some(Initializer::None);
        }

        // Every label is checked to exist and kept alive, cancelled or not.
        // Outside a function there is none, which is diagnosed there.
        let mut symbols = Vec::with_capacity(sum.labels.len());
        for (label, _) in &sum.labels {
            let Some(sym) = self.take_label_address(*label, expr.pos) else {
                return Some(Initializer::None);
            };
            symbols.push((*label, sym));
        }
        let symbol_of = |label: LabelId| {
            symbols
                .iter()
                .find(|(l, _)| *l == label)
                .map(|(_, s)| s.clone())
                .expect("every label in the sum was taken")
        };
        let Some((end, start)) = shape else {
            // `&&a - &&a`: gcc folds it to the constant it is.
            return Some(Initializer::Int(i128::from(sum.addend)));
        };
        // The table names the function's blocks, so the function can no
        // longer be copied: see `Function::saves_label_in_static`.
        if let Some(func) = &mut self.current_func {
            func.saves_label_in_static = true;
        }
        Some(Initializer::LabelDiff {
            end: symbol_of(end),
            start: symbol_of(start),
            addend: sum.addend,
        })
    }

    /// Whether `expr` takes a label's address anywhere in the arithmetic
    /// [`Self::label_sum`] walks.
    fn mentions_label_address(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::LabelAddr(_) => true,
            ExprKind::Cast { expr: inner, .. }
            | ExprKind::Unary {
                op: UnaryOp::Neg,
                operand: inner,
            } => self.mentions_label_address(inner),
            ExprKind::Binary {
                op: BinaryOp::Add | BinaryOp::Sub,
                left,
                right,
            } => self.mentions_label_address(left) || self.mentions_label_address(right),
            _ => false,
        }
    }

    /// `expr` as a [`LabelSum`] in bytes, or `None` when it is anything but
    /// sums and differences of label addresses and integer constants.
    ///
    /// The arithmetic is modular, so a cast that truncates to no fewer than
    /// the `width` bytes being initialized cannot change them; a narrower one
    /// could, and is not followed.
    fn label_sum(&mut self, expr: &Expr, width: usize) -> Option<LabelSum> {
        match &expr.kind {
            ExprKind::LabelAddr(label) => Some(LabelSum::label(*label)),
            ExprKind::Cast {
                expr: inner,
                cast_type,
            } => {
                let kind = self.types.kind(*cast_type);
                let keeps_bytes = kind == TypeKind::Pointer
                    || (self.types.is_integer(*cast_type)
                        && kind != TypeKind::Bool
                        && self.types.size_bytes(*cast_type) >= width);
                if !keeps_bytes {
                    return None;
                }
                self.label_sum(inner, width)
            }
            ExprKind::Unary {
                op: UnaryOp::Neg,
                operand,
            } => self.label_sum(operand, width)?.scaled(-1),
            ExprKind::Binary {
                op: op @ (BinaryOp::Add | BinaryOp::Sub),
                left,
                right,
            } => self.label_sum_binary(*op, left, right, width),
            _ => {
                let v = self.eval_const_init_expr(expr)?;
                Some(LabelSum::constant(i64::try_from(v).ok()?))
            }
        }
    }

    /// [`Self::label_sum`] for `left op right`, with C's pointer arithmetic:
    /// an integer added to a pointer counts elements, and the difference of
    /// two pointers counts them too, so only a byte-sized element leaves a
    /// difference the assembler can write.
    fn label_sum_binary(
        &mut self,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        width: usize,
    ) -> Option<LabelSum> {
        let elem = |this: &Self, e: &Expr| -> Option<i64> {
            let t = e.typ?;
            if this.types.kind(t) != TypeKind::Pointer {
                return None;
            }
            let pointee = this.types.arithmetic_pointee(t)?;
            Some((this.types.size_bytes(pointee) as i64).max(1))
        };
        let (lelem, relem) = (elem(self, left), elem(self, right));
        let l = self.label_sum(left, width)?;
        let r = self.label_sum(right, width)?;
        let negate = op == BinaryOp::Sub;
        // pointer - pointer counts elements of the pointee, which divides
        // the byte distance unless an element is a byte.
        if lelem.is_some() && relem.is_some() && (!negate || lelem != Some(1)) {
            return None;
        }
        // pointer +/- integer, integer + pointer: the integer counts elements.
        let r = match (lelem, relem) {
            (Some(le), None) if !r.has_labels() || le == 1 => r.scaled(le)?,
            (Some(_), None) => return None,
            _ => r,
        };
        let l = match (lelem, relem) {
            (None, Some(re)) if !l.has_labels() || re == 1 => l.scaled(re)?,
            (None, Some(_)) => return None,
            _ => l,
        };
        l.plus(if negate { r.scaled(-1)? } else { r })
    }
}
