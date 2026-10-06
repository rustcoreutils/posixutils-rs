//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Scalars stored in the reverse of the target's byte order
//

//! gcc's `scalar_storage_order`: the scalar members of a struct or union
//! declared with an order other than the target's are stored with their
//! bytes reversed.
//!
//! The order is a property of the member's type
//! ([`crate::types::TypeModifiers::REVERSE_ORDER`]), so every access reaches
//! the object at a type that says so. [`Linearizer::emit`] is the one place a
//! `Load` or `Store` enters the IR, and it hands any access at such a type to
//! [`Linearizer::reverse_order_access`], which rewrites it as an access in
//! the target's order plus explicit byte swaps. Nothing after the linearizer
//! sees a reversed access: the optimizer's load forwarding, memory analysis
//! and constant folding all reason about ordinary loads and stores and the
//! `Bswap` between them, and stay correct without knowing the attribute
//! exists.
//!
//! Integers up to eight bytes are swapped in a register. Everything else --
//! the floating types, which have no swap of their own, and the 16-byte
//! scalars, which have no 128-bit swap -- goes through a frame temporary, a
//! word at a time. Bit-fields and the halves of a complex value are reached at
//! their carriers' and halves' types, and their emitters call
//! [`Linearizer::emit_reversed_load`] and [`Linearizer::emit_reversed_store`]
//! themselves.

use super::linearize::Linearizer;
use super::{Initializer, Instruction, Opcode, PseudoId};
use crate::diag;
use crate::float::{FloatVal, FpFormat};
use crate::target::ByteOrder;
use crate::types::{TypeId, TypeKind};

impl<'a> Linearizer<'a> {
    /// A `Load` or `Store` of an object stored in reverse order, emitted as
    /// accesses in the target's order and byte swaps. Hands back any other
    /// instruction, untouched, for the caller to emit.
    pub(crate) fn reverse_order_access(&mut self, mut insn: Instruction) -> Option<Instruction> {
        let typ = match insn.typ {
            Some(t) if matches!(insn.op, Opcode::Load | Opcode::Store) => t,
            _ => return Some(insn),
        };
        if !self.types.reverses_storage(typ) {
            return Some(insn);
        }
        if self.types.size_bytes(typ) <= 1 {
            // One byte has no order: the access is the target's own.
            insn.typ = Some(self.native_scalar_type(typ));
            return Some(insn);
        }
        let addr = insn.src[0];
        match insn.op {
            Opcode::Load => {
                let target = insn.target.expect("a load defines its target");
                self.emit_reversed_load(target, addr, insn.offset, typ, insn.is_volatile);
            }
            _ => {
                let value = insn.src[1];
                self.emit_reversed_store(value, addr, insn.offset, typ, insn.is_volatile);
            }
        }
        None
    }

    /// Load the scalar of type `typ` stored in reverse order at `addr +
    /// offset` into `target`, as a value in the target's order.
    pub(crate) fn emit_reversed_load(
        &mut self,
        target: PseudoId,
        addr: PseudoId,
        offset: i64,
        typ: TypeId,
        volatile: bool,
    ) {
        let bytes = self.types.size_bytes(typ);
        if !self.reject_unswappable(typ) && self.types.is_integer(typ) && bytes <= 8 {
            let word = self.word_type(bytes);
            let raw = self.alloc_reg_pseudo();
            let bits = self.types.size_bits(word);
            self.emit(Instruction::load(raw, addr, offset, word, bits).with_volatile(volatile));
            self.emit_bswap_into(target, raw, bytes);
            return;
        }
        let native = self.native_scalar_type(typ);
        let temp = self.frame_temp_addr("__sso", native);
        self.copy_reversed(temp, 0, addr, offset, bytes, (false, volatile));
        let bits = self.types.size_bits(native);
        self.emit(Instruction::load(target, temp, 0, native, bits));
    }

    /// Store `value`, a scalar of type `typ` in the target's order, at `addr
    /// + offset` in reverse order.
    pub(crate) fn emit_reversed_store(
        &mut self,
        value: PseudoId,
        addr: PseudoId,
        offset: i64,
        typ: TypeId,
        volatile: bool,
    ) {
        let bytes = self.types.size_bytes(typ);
        if !self.reject_unswappable(typ) && self.types.is_integer(typ) && bytes <= 8 {
            let word = self.word_type(bytes);
            let swapped = self.alloc_reg_pseudo();
            self.emit_bswap_into(swapped, value, bytes);
            let bits = self.types.size_bits(word);
            self.emit(
                Instruction::store(swapped, addr, offset, word, bits).with_volatile(volatile),
            );
            return;
        }
        let native = self.native_scalar_type(typ);
        let temp = self.frame_temp_addr("__sso", native);
        let bits = self.types.size_bits(native);
        self.emit(Instruction::store(value, temp, 0, native, bits));
        self.copy_reversed(addr, offset, temp, 0, bytes, (volatile, false));
    }

    /// Copy the `bytes` bytes at `src + src_offset` to `dst + dst_offset` in
    /// the opposite order, a word at a time: the first word read, swapped,
    /// is the last word written. `volatile` marks the (destination, source)
    /// accesses that reach a volatile object.
    fn copy_reversed(
        &mut self,
        dst: PseudoId,
        dst_offset: i64,
        src: PseudoId,
        src_offset: i64,
        bytes: usize,
        volatile: (bool, bool),
    ) {
        let step = bytes.min(8);
        let word = self.word_type(step);
        let bits = self.types.size_bits(word);
        for k in 0..bytes / step {
            let from = src_offset + (k * step) as i64;
            let to = dst_offset + (bytes - (k + 1) * step) as i64;
            let raw = self.alloc_reg_pseudo();
            self.emit(Instruction::load(raw, src, from, word, bits).with_volatile(volatile.1));
            let swapped = self.alloc_reg_pseudo();
            self.emit_bswap_into(swapped, raw, step);
            self.emit(Instruction::store(swapped, dst, to, word, bits).with_volatile(volatile.0));
        }
    }

    /// `target = bswap(value)` for a value of `bytes` bytes: two, four or
    /// eight.
    pub(crate) fn emit_bswap_into(&mut self, target: PseudoId, value: PseudoId, bytes: usize) {
        let (op, typ) = match bytes {
            2 => (Opcode::Bswap16, self.types.ushort_id),
            4 => (Opcode::Bswap32, self.types.uint_id),
            8 => (Opcode::Bswap64, self.types.ulonglong_id),
            _ => unreachable!("no {bytes}-byte swap: the callers split wider values into words"),
        };
        let bits = (bytes * 8) as u32;
        self.emit(Instruction::unop(op, target, value, typ, bits));
    }

    /// The unsigned integer type of `bytes` bytes.
    fn word_type(&self, bytes: usize) -> TypeId {
        match bytes {
            1 => self.types.uchar_id,
            2 => self.types.ushort_id,
            4 => self.types.uint_id,
            8 => self.types.ulonglong_id,
            _ => self.types.uint128_id,
        }
    }

    /// The scalar `typ` is, in the target's order: the pre-interned type of
    /// its kind, which differs from `typ` in nothing a load or store reads.
    fn native_scalar_type(&self, typ: TypeId) -> TypeId {
        match self.types.kind(typ) {
            TypeKind::Float => self.types.float_id,
            TypeKind::Double => self.types.double_id,
            TypeKind::LongDouble => self.types.longdouble_id,
            TypeKind::Float16 => self.types.float16_id,
            TypeKind::Float128 => self.types.float128_id,
            TypeKind::Bool => self.types.bool_id,
            _ => self.word_type(self.types.size_bytes(typ)),
        }
    }

    /// gcc does not implement a reversed x87 extended value, whose ten
    /// significant bytes do not fill its sixteen, and refuses it where it is
    /// accessed: so does c17. True when the access was refused.
    fn reject_unswappable(&self, typ: TypeId) -> bool {
        if self.types.fp_format(typ) != Some(FpFormat::X87Extended) {
            return false;
        }
        let pos = self.current_pos.unwrap_or_default();
        diag::error(
            pos,
            "sorry, unimplemented: reverse storage order for XFmode",
        );
        true
    }

    /// The complex value of type `typ` stored at `addr`, at an address that
    /// holds it in the target's order: `addr` itself, unless the object is
    /// stored in reverse order, when it is a copy with each half swapped.
    ///
    /// A complex value travels by address, and every consumer reads its
    /// halves through that address at the base type, which knows nothing of
    /// the order -- so the value is put right once, here, where it is read.
    pub(crate) fn complex_in_native_order(&mut self, addr: PseudoId, typ: TypeId) -> PseudoId {
        if !self.types.is_complex(typ) || !self.types.reverses_storage(typ) {
            return addr;
        }
        let half = self.types.complex_base(typ);
        let half_bytes = self.types.size_bytes(half) as i64;
        let half_bits = self.types.size_bits(half);
        let volatile = self.types.contains_volatile(typ);
        let copy = self.frame_temp_addr("__sso", self.types.make_complex(half));
        for offset in [0, half_bytes] {
            let value = self.alloc_reg_pseudo();
            self.emit_reversed_load(value, addr, offset, half, volatile);
            self.emit(Instruction::store(value, copy, offset, half, half_bits));
        }
        copy
    }

    /// The order the bits of a bit-field of type `typ` are counted in: its
    /// struct's storage order.
    pub(crate) fn bit_order(&self, typ: TypeId) -> ByteOrder {
        let native = self.target.byte_order();
        if !self.types.reverses_storage(typ) {
            return native;
        }
        match native {
            ByteOrder::LittleEndian => ByteOrder::BigEndian,
            ByteOrder::BigEndian => ByteOrder::LittleEndian,
        }
    }

    /// `init`, the initializer of an object of type `typ` written in the
    /// target's byte order, as the bytes `typ` stores: reversed for a scalar
    /// stored in reverse order, each half for a complex one, and each element
    /// for a string literal initializing an array of them. Anything else is
    /// already in its order -- an initializer list reverses its own scalars
    /// as it lowers them, and a bit-field is placed by its struct.
    pub(crate) fn in_storage_order(&self, init: Initializer, typ: TypeId) -> Initializer {
        if self.types.kind(typ) == TypeKind::Array && !self.types.is_vector(typ) {
            let Some(elem) = self.types.base_type(typ) else {
                return init;
            };
            let elem_size = self.types.size_bytes(elem);
            if !self.types.reverses_storage(elem) || elem_size <= 1 {
                return init;
            }
            let total = self.types.size_bytes(typ);
            return match init.string_as_array(elem_size, total) {
                Some(Initializer::Array {
                    elem_size,
                    total_size,
                    elements,
                }) => Initializer::Array {
                    elem_size,
                    total_size,
                    elements: elements
                        .into_iter()
                        .map(|(at, unit)| (at, self.in_storage_order(unit, elem)))
                        .collect(),
                },
                _ => init,
            };
        }
        if !self.types.reverses_storage(typ) {
            return init;
        }
        if self.types.is_complex(typ) {
            let half = self.types.complex_base(typ);
            return match init {
                Initializer::Struct { total_size, fields } => Initializer::Struct {
                    total_size,
                    fields: fields
                        .into_iter()
                        .map(|(at, size, part)| (at, size, self.reversed_scalar(part, half)))
                        .collect(),
                },
                other => other,
            };
        }
        self.reversed_scalar(init, typ)
    }

    /// The initializer of the scalar `typ` (an integer or real floating
    /// type, read at its own width) with its bytes reversed.
    fn reversed_scalar(&self, init: Initializer, typ: TypeId) -> Initializer {
        let bytes = self.types.size_bytes(typ);
        if bytes <= 1 || matches!(init, Initializer::None) {
            return init;
        }
        let bits = if let Some(fmt) = self.types.fp_format(typ) {
            if self.reject_unswappable(typ) {
                return init;
            }
            match init {
                Initializer::Float(v) | Initializer::Float128(v) => v.to_bits(fmt),
                Initializer::Int(v) => FloatVal::from_i128(v).to_bits(fmt),
                other => return other,
            }
        } else {
            // An address is written by the linker, in the target's order;
            // `ast_init_to_ir` refused it before it got here.
            match init {
                Initializer::Int(v) => v as u128,
                other => return other,
            }
        };
        let width = bytes * 8;
        let kept = if width >= 128 {
            bits
        } else {
            bits & ((1u128 << width) - 1)
        };
        Initializer::Int((kept.swap_bytes() >> (128 - width)) as i128)
    }
}
