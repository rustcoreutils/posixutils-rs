//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU `vector_size` values, lowered lane by lane.
//
// A vector travels by address, as a complex value does: every expression of
// vector type linearizes to the address of storage holding its lanes -- the
// object itself for a name, a member or an element, a fresh frame temporary
// for anything computed. An operation reads each operand's lanes, computes
// each lane with the scalar opcode of the lane type, and stores the result
// into a temporary. Nothing below the linearizer sees a vector.
//

use super::linearize::{BlockVolatility, Linearizer};
use super::{Instruction, PseudoId};
use crate::float::FloatVal;
use crate::parse::ast::{AssignOp, BinaryOp, Expr, UnaryOp};
use crate::types::TypeId;

/// Where an operand's lane `i` comes from.
#[derive(Clone, Copy)]
enum Lanes {
    /// A vector: lane `i` is at `addr + i * size(lane)`, of type `lane`.
    Vector { addr: PseudoId, lane: TypeId },
    /// A scalar spread across every lane, converted once.
    Splat(PseudoId),
}

impl Linearizer<'_> {
    /// The lane type, lane count and lane size in bytes of vector `typ`.
    fn vector_shape(&self, typ: TypeId) -> (TypeId, usize, i64) {
        let (lane, count) = self
            .types
            .vector_lanes(typ)
            .expect("a vector expression has a vector type");
        (lane, count, self.types.size_bytes(lane) as i64)
    }

    /// The address of the lanes of vector expression `e`.
    pub(crate) fn vector_addr(&mut self, e: &Expr) -> PseudoId {
        self.linearize_expr(e)
    }

    /// Operand `e` as lanes of type `lane` -- a vector's own, or a scalar
    /// converted to `lane` once and spread.
    fn vector_lanes_of(&mut self, e: &Expr, lane: TypeId) -> Lanes {
        let typ = self.expr_type(e);
        if let Some((own, _)) = self.types.vector_lanes(typ) {
            return Lanes::Vector {
                addr: self.vector_addr(e),
                lane: own,
            };
        }
        let value = self.linearize_expr(e);
        Lanes::Splat(self.emit_convert(value, typ, lane))
    }

    /// Lane `i` of `lanes`, as a value of type `lane`.
    fn vector_lane(&mut self, lanes: Lanes, i: usize, lane: TypeId) -> PseudoId {
        match lanes {
            Lanes::Splat(value) => value,
            Lanes::Vector { addr, lane: own } => {
                let size = self.types.size_bits(own);
                let offset = i as i64 * (size / 8) as i64;
                let value = self.alloc_reg_pseudo();
                self.emit(Instruction::load(value, addr, offset, own, size));
                self.emit_convert(value, own, lane)
            }
        }
    }

    /// `left op right` with a vector operand, whose result has type
    /// `result_typ`: a vector of the operation's type, or of masks for a
    /// comparison. Two vectors of integer lanes differing in signedness
    /// compute at the left operand's lane type, which is the result's.
    pub(crate) fn linearize_vector_binary(
        &mut self,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        result_typ: TypeId,
    ) -> PseudoId {
        let left_typ = self.expr_type(left);
        let right_typ = self.expr_type(right);
        let vector_typ = if self.types.is_vector(left_typ) {
            left_typ
        } else {
            right_typ
        };
        let (lane, count, _) = self.vector_shape(vector_typ);
        let (result_lane, _, result_size) = self.vector_shape(result_typ);
        let l = self.vector_lanes_of(left, lane);
        let r = self.vector_lanes_of(right, lane);
        let result = self.frame_temp_addr("__vec", result_typ);
        let lane_bits = self.types.size_bits(result_lane);
        for i in 0..count {
            let a = self.vector_lane(l, i, lane);
            let b = self.vector_lane(r, i, lane);
            let value = if op.is_comparison() {
                // Each lane is all ones for true, as gcc's masks are.
                let int = self.types.int_id;
                let truth = self.emit_binary(op, a, b, int, lane);
                let zero = self.emit_const(0, int);
                let mask = self.emit_binary(BinaryOp::Sub, zero, truth, int, int);
                self.emit_convert(mask, int, result_lane)
            } else {
                self.emit_binary(op, a, b, lane, lane)
            };
            let offset = i as i64 * result_size;
            self.emit(Instruction::store(
                value,
                result,
                offset,
                result_lane,
                lane_bits,
            ));
        }
        result
    }

    /// `-v`, `+v` or `~v`, lane by lane.
    pub(crate) fn linearize_vector_unary(&mut self, op: UnaryOp, operand: &Expr) -> PseudoId {
        let typ = self.expr_type(operand);
        let (lane, count, size) = self.vector_shape(typ);
        let src = self.vector_lanes_of(operand, lane);
        if op != UnaryOp::Neg && op != UnaryOp::BitNot {
            // `+v` is the value itself, at a fresh place as any result is.
            return self.vector_copy(src, typ);
        }
        let result = self.frame_temp_addr("__vec", typ);
        let bits = self.types.size_bits(lane);
        for i in 0..count {
            let a = self.vector_lane(src, i, lane);
            let value = self.emit_unary(op, a, lane);
            self.emit(Instruction::store(
                value,
                result,
                i as i64 * size,
                lane,
                bits,
            ));
        }
        result
    }

    /// A fresh copy of the vector `src` of type `typ`.
    fn vector_copy(&mut self, src: Lanes, typ: TypeId) -> PseudoId {
        let result = self.frame_temp_addr("__vec", typ);
        if let Lanes::Vector { addr, .. } = src {
            let bytes = self.types.size_bytes(typ) as i64;
            self.emit_block_copy(result, addr, bytes, BlockVolatility::default());
        }
        result
    }

    /// A cast with a vector on either side, which reinterprets the bits of a
    /// value of the same size: a vector as another vector or as an integer,
    /// or an integer as a vector.
    pub(crate) fn linearize_vector_cast(&mut self, inner: &Expr, to: TypeId) -> PseudoId {
        let from = self.expr_type(inner);
        let bits = self.types.size_bits(to);
        if self.types.is_vector(from) {
            let addr = self.vector_addr(inner);
            if self.types.is_vector(to) {
                let result = self.frame_temp_addr("__vec", to);
                self.emit_block_copy(result, addr, (bits / 8) as i64, BlockVolatility::default());
                return result;
            }
            let value = self.alloc_reg_pseudo();
            self.emit(Instruction::load(value, addr, 0, to, bits));
            return value;
        }
        let value = self.linearize_expr(inner);
        let result = self.frame_temp_addr("__vec", to);
        self.emit(Instruction::store(value, result, 0, from, bits));
        result
    }

    /// An assignment to a vector object: `=`, or `op=` computed lane by lane
    /// with the target as the left operand. The target is evaluated once,
    /// and the value of the assignment is the object assigned to.
    pub(crate) fn emit_vector_assign(
        &mut self,
        op: AssignOp,
        target: &Expr,
        value: &Expr,
    ) -> PseudoId {
        let typ = self.expr_type(target);
        let bytes = self.types.size_bytes(typ) as i64;
        let target_addr = self.linearize_lvalue(target);
        let source = match op.binary_op() {
            None => self.vector_addr(value),
            Some(binop) => {
                let (lane, count, size) = self.vector_shape(typ);
                let l = Lanes::Vector {
                    addr: target_addr,
                    lane,
                };
                let r = self.vector_lanes_of(value, lane);
                let result = self.frame_temp_addr("__vec", typ);
                let bits = self.types.size_bits(lane);
                for i in 0..count {
                    let a = self.vector_lane(l, i, lane);
                    let b = self.vector_lane(r, i, lane);
                    let v = self.emit_binary(binop, a, b, lane, lane);
                    self.emit(Instruction::store(v, result, i as i64 * size, lane, bits));
                }
                result
            }
        };
        let vol = self.block_volatility(self.expr_type(target), self.expr_type(value));
        self.emit_block_copy(target_addr, source, bytes, vol);
        target_addr
    }

    /// `++v`, `--v`, `v++` or `v--`: every lane stepped by one. A prefix
    /// form is the object itself afterwards, a postfix one a copy of what it
    /// held before.
    pub(crate) fn emit_vector_step(&mut self, operand: &Expr, inc: bool, prefix: bool) -> PseudoId {
        let typ = self.expr_type(operand);
        let (lane, count, size) = self.vector_shape(typ);
        let addr = self.linearize_lvalue(operand);
        let src = Lanes::Vector { addr, lane };
        let old = (!prefix).then(|| self.vector_copy(src, typ));
        let one = if self.types.is_float(lane) {
            self.emit_fconst(FloatVal::from_f64(1.0), lane)
        } else {
            self.emit_const(1, lane)
        };
        let op = if inc { BinaryOp::Add } else { BinaryOp::Sub };
        let bits = self.types.size_bits(lane);
        for i in 0..count {
            let a = self.vector_lane(src, i, lane);
            let v = self.emit_binary(op, a, one, lane, lane);
            self.emit(Instruction::store(v, addr, i as i64 * size, lane, bits));
        }
        old.unwrap_or(addr)
    }
}
