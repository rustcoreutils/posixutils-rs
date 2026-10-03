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
use crate::abi::{get_abi_for_conv, CallingConv};
use crate::float::FloatVal;
use crate::parse::ast::{AssignOp, BinaryOp, Expr, ShuffleSelector, UnaryOp};
use crate::types::{TypeId, TypeKind};

/// Where an operand's lane `i` comes from.
#[derive(Clone, Copy)]
enum Lanes {
    /// A vector: lane `i` is at `addr + i * size(lane)`, of type `lane`.
    Vector { addr: PseudoId, lane: TypeId },
    /// A scalar spread across every lane, converted once.
    Splat(PseudoId),
}

impl Linearizer<'_> {
    /// The type the convention `conv` passes and returns vectors of type
    /// `vec` as (`Abi::vector_carrier`). The parser refuses a vector at a
    /// call boundary that has none, so a missing one is the vector itself,
    /// which every path below then refuses to take apart.
    pub(crate) fn vector_carrier(&self, vec: TypeId, conv: CallingConv) -> TypeId {
        get_abi_for_conv(conv, self.target)
            .vector_carrier(vec, self.types)
            .unwrap_or(vec)
    }

    /// `typ`, or the carrier of `typ` if it is a vector: what a parameter or
    /// argument of type `typ` travels as.
    pub(crate) fn abi_type(&self, typ: TypeId, conv: CallingConv) -> TypeId {
        if self.types.is_vector(typ) {
            self.vector_carrier(typ, conv)
        } else {
            typ
        }
    }

    /// `typ`, or what a vector of type `typ` is returned as
    /// (`Abi::vector_return_carrier`): what a return value of type `typ`
    /// travels as.
    pub(crate) fn abi_return_type(&self, typ: TypeId, conv: CallingConv) -> TypeId {
        if !self.types.is_vector(typ) {
            return typ;
        }
        get_abi_for_conv(conv, self.target)
            .vector_return_carrier(typ, self.types)
            .unwrap_or(typ)
    }

    /// The vector `vec` is widened to, lane by lane, to be returned under
    /// `conv` (`Abi::vector_return_widened`).
    fn vector_return_widened(&self, vec: TypeId, conv: CallingConv) -> Option<TypeId> {
        get_abi_for_conv(conv, self.target).vector_return_widened(vec, self.types)
    }

    /// The value a function returning the vector of type `vec` at `addr`
    /// returns, as its return `carrier`: the vector's bits, or those of the
    /// vector it is widened to.
    pub(crate) fn vector_return_value(
        &mut self,
        addr: PseudoId,
        vec: TypeId,
        carrier: TypeId,
        conv: CallingConv,
    ) -> PseudoId {
        let addr = match self.vector_return_widened(vec, conv) {
            Some(widened) => self.convert_vector_at(addr, vec, widened),
            None => addr,
        };
        self.vector_to_carrier(addr, carrier)
    }

    /// The vector of type `vec` a call under `conv` returned as `carrier` in
    /// `result` ([`Self::vector_call_result`]), narrowed back where it was
    /// returned widened.
    pub(crate) fn vector_returned(
        &mut self,
        result: PseudoId,
        carrier: TypeId,
        vec: TypeId,
        conv: CallingConv,
    ) -> PseudoId {
        match self.vector_return_widened(vec, conv) {
            Some(widened) => {
                let wide = self.vector_from_carrier(result, carrier, widened);
                self.convert_vector_at(wide, widened, vec)
            }
            None => self.vector_call_result(result, carrier, vec),
        }
    }

    /// Whether `carrier` travels as an aggregate, by address, rather than as
    /// a value in a register.
    fn carrier_is_aggregate(&self, carrier: TypeId) -> bool {
        matches!(self.types.kind(carrier), TypeKind::Struct | TypeKind::Union)
    }

    /// A vector argument, as the convention passes it: the value of its
    /// carrier, loaded from the vector's lanes, or -- for a carrier that is
    /// an aggregate -- the vector's address under the carrier's type, copied
    /// first where the convention passes it by reference.
    pub(crate) fn lower_vector_arg(&mut self, a: &Expr, conv: CallingConv) -> (PseudoId, TypeId) {
        let vec = self.expr_type(a);
        let carrier = self.vector_carrier(vec, conv);
        let addr = self.vector_addr(a);
        if !self.carrier_is_aggregate(carrier) {
            return (self.vector_to_carrier(addr, carrier), carrier);
        }
        let val = if self.passed_by_reference(carrier, conv) {
            let vol = self.block_volatility(carrier, vec);
            self.argument_copy(addr, carrier, true, vol)
        } else {
            addr
        };
        (val, carrier)
    }

    /// The bits of the vector at `addr`, as a value of the scalar `carrier`.
    pub(crate) fn vector_to_carrier(&mut self, addr: PseudoId, carrier: TypeId) -> PseudoId {
        let bits = self.types.size_bits(carrier);
        let value = self.alloc_reg_pseudo();
        self.emit(Instruction::load(value, addr, 0, carrier, bits));
        value
    }

    /// The vector of type `vec` whose bits are `value`, of the scalar
    /// `carrier`: stored to a fresh temporary, whose address is the vector.
    pub(crate) fn vector_from_carrier(
        &mut self,
        value: PseudoId,
        carrier: TypeId,
        vec: TypeId,
    ) -> PseudoId {
        let result = self.frame_temp_addr("__vec", vec);
        let bits = self.types.size_bits(carrier);
        self.emit(Instruction::store(value, result, 0, carrier, bits));
        result
    }

    /// The vector of type `vec` a call returning its `carrier` produced in
    /// `result`: the carrier's bits, stored to a temporary, or the address
    /// of the aggregate the callee wrote through the hidden pointer.
    pub(crate) fn vector_call_result(
        &mut self,
        result: PseudoId,
        carrier: TypeId,
        vec: TypeId,
    ) -> PseudoId {
        if self.carrier_is_aggregate(carrier) {
            self.rvalue_addr(result, carrier)
        } else {
            self.vector_from_carrier(result, carrier, vec)
        }
    }

    /// `__builtin_shuffle` and `__builtin_shufflevector`: lane `k` of the
    /// result is lane `index(k)` of `first` followed by `second`. A mask
    /// lane is taken modulo the lanes the operands hold together, as gcc
    /// does; a `-1` index of `__builtin_shufflevector` gives zero.
    pub(crate) fn linearize_vector_shuffle(
        &mut self,
        first: &Expr,
        second: Option<&Expr>,
        selector: &ShuffleSelector,
        result_typ: TypeId,
    ) -> PseudoId {
        let (lane, count, size) = self.vector_shape(result_typ);
        let in_typ = self.expr_type(first);
        let (_, in_count, _) = self.vector_shape(in_typ);
        let a = self.vector_addr(first);
        let b = second.map(|e| self.vector_addr(e));
        let mask = match selector {
            ShuffleSelector::Mask(m) => {
                Some((self.vector_addr(m), self.vector_shape(self.expr_type(m))))
            }
            ShuffleSelector::Indices(_) => None,
        };
        let result = self.frame_temp_addr("__vec", result_typ);
        let bits = self.types.size_bits(lane);
        let long = self.types.long_id;
        for k in 0..count {
            let value = match (selector, mask) {
                (ShuffleSelector::Indices(indices), _) => match indices[k] {
                    None => self.emit_const(0, lane),
                    Some(i) => {
                        let i = i as usize;
                        // The operands may differ in length.
                        let (base, i) = if i < in_count {
                            (a, i)
                        } else {
                            (b.unwrap_or(a), i - in_count)
                        };
                        self.load_lane(base, i as i64 * size, lane)
                    }
                },
                (ShuffleSelector::Mask(_), Some((maddr, (mlane, _, msize)))) => {
                    // The index, wrapped to the lanes there are.
                    let m = self.load_lane(maddr, k as i64 * msize, mlane);
                    let m = self.emit_convert(m, mlane, long);
                    let total = if b.is_some() { 2 * in_count } else { in_count };
                    let wrap = self.emit_const(total as i128 - 1, long);
                    let idx = self.emit_binary(BinaryOp::BitAnd, m, wrap, long, long);
                    let within = self.emit_const(in_count as i128 - 1, long);
                    let lane_idx = self.emit_binary(BinaryOp::BitAnd, idx, within, long, long);
                    let stride = self.emit_const(size as i128, long);
                    let offset = self.emit_binary(BinaryOp::Mul, lane_idx, stride, long, long);
                    let from_a = self.lane_at(a, offset, lane);
                    match b {
                        None => from_a,
                        Some(b) => {
                            let from_b = self.lane_at(b, offset, lane);
                            let n = self.emit_const(in_count as i128, long);
                            let high = self.emit_binary(BinaryOp::BitAnd, idx, n, long, long);
                            let zero = self.emit_const(0, long);
                            let int = self.types.int_id;
                            let in_b = self.emit_binary(BinaryOp::Ne, high, zero, int, long);
                            let chosen = self.alloc_reg_pseudo();
                            self.emit(Instruction::select(
                                chosen, in_b, from_b, from_a, lane, bits,
                            ));
                            chosen
                        }
                    }
                }
                (ShuffleSelector::Mask(_), None) => unreachable!("a mask has an address"),
            };
            self.emit(Instruction::store(
                value,
                result,
                k as i64 * size,
                lane,
                bits,
            ));
        }
        result
    }

    /// `__builtin_convertvector`: each lane converted, as by a cast.
    pub(crate) fn linearize_convert_vector(
        &mut self,
        value: &Expr,
        result_typ: TypeId,
    ) -> PseudoId {
        let from_typ = self.expr_type(value);
        let src = self.vector_addr(value);
        self.convert_vector_at(src, from_typ, result_typ)
    }

    /// The vector at `src`, of type `from_typ`, with each lane converted to
    /// those of `result_typ`, as by a cast.
    fn convert_vector_at(
        &mut self,
        src: PseudoId,
        from_typ: TypeId,
        result_typ: TypeId,
    ) -> PseudoId {
        let (to, count, to_size) = self.vector_shape(result_typ);
        let (from, _, from_size) = self.vector_shape(from_typ);
        let result = self.frame_temp_addr("__vec", result_typ);
        let bits = self.types.size_bits(to);
        for k in 0..count {
            let v = self.load_lane(src, k as i64 * from_size, from);
            let v = self.emit_convert(v, from, to);
            self.emit(Instruction::store(v, result, k as i64 * to_size, to, bits));
        }
        result
    }

    /// The lane of type `lane` at `addr + offset`.
    fn load_lane(&mut self, addr: PseudoId, offset: i64, lane: TypeId) -> PseudoId {
        let bits = self.types.size_bits(lane);
        let value = self.alloc_reg_pseudo();
        self.emit(Instruction::load(value, addr, offset, lane, bits));
        value
    }

    /// The lane of type `lane` at `addr` plus the run-time byte `offset`.
    fn lane_at(&mut self, addr: PseudoId, offset: PseudoId, lane: TypeId) -> PseudoId {
        let long = self.types.long_id;
        let at = self.emit_binary(BinaryOp::Add, addr, offset, long, long);
        self.load_lane(at, 0, lane)
    }

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
    /// compute at the left operand's lane type, which is the result's, but
    /// compare unsigned whichever side is unsigned, as gcc does.
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
        let (mut lane, count, _) = self.vector_shape(vector_typ);
        if op.is_comparison() {
            lane = self.comparison_lane(lane, left_typ, right_typ);
        }
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

    /// The lane type a comparison of two vectors computes at: unsigned when
    /// either operand's integer lanes are.
    fn comparison_lane(&self, lane: TypeId, left: TypeId, right: TypeId) -> TypeId {
        if !self.types.is_vector(left) || !self.types.is_vector(right) {
            return lane;
        }
        let unsigned = [left, right]
            .into_iter()
            .filter_map(|v| self.types.vector_lanes(v))
            .any(|(l, _)| self.types.is_unsigned(l));
        if !unsigned || self.types.is_float(lane) {
            return lane;
        }
        self.types
            .unsigned_of_size(self.types.size_bytes(lane))
            .unwrap_or(lane)
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
