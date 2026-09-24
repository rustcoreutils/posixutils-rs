//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! Emit helpers for the linearizer (constants, block copies, bitfields, operators, assignments)

use super::{CallAbiInfo, Instruction, Opcode, Pseudo, PseudoId};
use crate::abi::get_abi_for_conv;
use crate::diag::{error, Position};
use crate::float::FloatVal;
use crate::parse::ast::{AssignOp, BinaryOp, Expr, ExprKind, UnaryOp};
use crate::types::{MemberInfo, TypeId, TypeKind};

/// A read-modify-write target whose address has been computed **once**.
///
/// C17 evaluates the target of a read-modify-write exactly once -- 6.5.16.2p3
/// for `E1 op= E2`, 6.5.3.1p2 for `++E`, 6.5.2.4p2 for `E++`. All three used
/// to read the old value from the *expression* and then re-derive the address
/// for the store, so every subexpression of the target ran twice:
/// `b[i++] += 5` incremented `i` twice and updated the wrong element,
/// `*p++ += 1` advanced `p` by two, and `a[f()] |= 1` called `f` twice.
///
/// `base` addresses the object, or its storage unit when `bitfield` is set --
/// a bit-field has no address of its own, so it carries its placement
/// instead.
pub(crate) struct RmwPlace {
    base: PseudoId,
    /// `(byte offset, bit offset, bit width, storage bytes, field type)` --
    /// spelled to match `emit_bitfield_load`/`_store`, which are the only
    /// consumers.
    bitfield: Option<(usize, u32, u32, u32, TypeId)>,
}

/// The per-half opcodes a complex operation uses, chosen once from the base
/// type rather than at each emit site.
///
/// `integral` distinguishes the two families where the opcode alone cannot:
/// complex multiply and divide call a runtime helper for floating halves and
/// are open-coded for integer ones.
struct ComplexHalfOps {
    add: Opcode,
    sub: Opcode,
    neg: Opcode,
    eq: Opcode,
    ne: Opcode,
    integral: bool,
}

/// The low-`bit_width` mask for a bit-field value.
///
/// Spelled as a shift of `u64::MAX` rather than `(1 << bit_width) - 1` because
/// the latter overflows at the one width that matters most: Rust masks a shift
/// amount to the operand's width, so `1u64 << 64` is `1` and the mask for
/// `struct { unsigned long long a:64; }` would come out `0`.
///
/// A zero width never reaches here -- a named zero-width bit-field is rejected
/// by `validate_bitfield`, and the unnamed kind allocates nothing -- but the
/// mask is defined for it anyway rather than shifting by 64 again.
pub(crate) fn bitfield_value_mask(bit_width: u32) -> u64 {
    debug_assert!(bit_width <= 64, "bit-field wider than its carrier");
    if bit_width == 0 {
        0
    } else {
        u64::MAX >> (64 - bit_width)
    }
}

/// The same mask, for a carrier that may be 128 bits wide.
///
/// A separate function rather than a widened return type, because the
/// narrow callers rely on `!mask` being bounded by the carrier: at 128 bits
/// a complement carries ones the storage unit does not have, and the byte-wise
/// paths would then write them. Callers that do complement a mask intersect it
/// with the storage width explicitly.
///
/// Shifts `u128::MAX` down for the reason the 64-bit twin does -- `1u128 <<
/// 128` is `1`, so the subtraction spelling yields a zero mask at exactly the
/// full width.
pub(crate) fn bitfield_value_mask_128(bit_width: u32) -> u128 {
    debug_assert!(bit_width <= 128, "bit-field wider than any carrier");
    if bit_width == 0 {
        0
    } else {
        u128::MAX >> (128 - bit_width)
    }
}

impl<'a> super::linearize::Linearizer<'a> {
    pub(crate) fn emit_const(&mut self, val: i128, typ: TypeId) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::val(id, val);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }

        // Emit setval instruction
        let insn = Instruction::new(Opcode::SetVal)
            .with_target(id)
            .with_type_and_size(typ, self.types.size_bits(typ));
        self.emit(insn);

        id
    }

    pub(crate) fn emit_fconst(&mut self, val: FloatVal, typ: TypeId) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::fval(id, val);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }

        // Emit setval instruction
        let insn = Instruction::new(Opcode::SetVal)
            .with_target(id)
            .with_type_and_size(typ, self.types.size_bits(typ));
        self.emit(insn);

        id
    }

    /// Emit a store to a static local variable.
    /// The caller must have already verified that `name_str` refers to a static local
    /// (i.e., the local's sym is the sentinel value u32::MAX).
    pub(crate) fn emit_static_local_store(
        &mut self,
        name_str: &str,
        value: PseudoId,
        typ: TypeId,
        size: u32,
    ) {
        let key = format!("{}.{}", self.current_func_name, name_str);
        if let Some(static_info) = self.static_locals.get(&key).cloned() {
            let sym_id = self.alloc_pseudo();
            let pseudo = Pseudo::sym(sym_id, static_info.global_name);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }
            self.emit(Instruction::store(value, sym_id, 0, typ, size));
        } else {
            unreachable!("static local sentinel without static_locals entry");
        }
    }

    /// Emit stores to zero-initialize an aggregate (struct, union, or array)
    /// This handles C99 6.7.8p19: uninitialized members must be zero-initialized
    pub(crate) fn emit_aggregate_zero(&mut self, base_sym: PseudoId, typ: TypeId) {
        let total_bytes = self.types.size_bytes(typ);
        let mut offset: i64 = 0;

        // Create a zero constant for 64-bit stores
        let zero64 = self.emit_const(0, self.types.long_id);

        // Zero in 8-byte chunks
        while offset + 8 <= total_bytes as i64 {
            self.emit(Instruction::store(
                zero64,
                base_sym,
                offset,
                self.types.long_id,
                64,
            ));
            offset += 8;
        }

        // Handle remaining bytes (if any)
        if offset < total_bytes as i64 {
            let remaining = total_bytes as i64 - offset;
            if remaining >= 4 {
                let zero32 = self.emit_const(0, self.types.int_id);
                self.emit(Instruction::store(
                    zero32,
                    base_sym,
                    offset,
                    self.types.int_id,
                    32,
                ));
                offset += 4;
            }
            if offset < total_bytes as i64 {
                let remaining = total_bytes as i64 - offset;
                if remaining >= 2 {
                    let zero16 = self.emit_const(0, self.types.short_id);
                    self.emit(Instruction::store(
                        zero16,
                        base_sym,
                        offset,
                        self.types.short_id,
                        16,
                    ));
                    offset += 2;
                }
                if offset < total_bytes as i64 {
                    let zero8 = self.emit_const(0, self.types.char_id);
                    self.emit(Instruction::store(
                        zero8,
                        base_sym,
                        offset,
                        self.types.char_id,
                        8,
                    ));
                }
            }
        }
    }

    /// Emit a block copy from src to dst using integer chunks.
    pub(crate) fn emit_block_copy(&mut self, dst: PseudoId, src: PseudoId, size_bytes: i64) {
        self.emit_block_copy_at_offset(dst, 0, src, size_bytes);
    }

    /// Above this many bytes, a block copy becomes a `memcpy` call rather than
    /// an unrolled load/store sequence.
    ///
    /// The unrolled form costs one load, one store and one fresh pseudo per
    /// eight bytes, with no upper bound: a 256 KB struct passed by value --
    /// `struct big { int i[0x10000]; }`, which gcc.c-torture's pr28982b does --
    /// produced 65,536 IR instructions, 131,108 lines of assembly and a
    /// **65-second** compile, against gcc's 48 lines and one `call memcpy`.
    ///
    /// Small copies stay inline, where they belong: a call would cost more than
    /// the moves it replaces, and `test_linearize.rs` pins two 64-bit loads and
    /// two 64-bit stores for a 128-bit copy. 128 bytes leaves every realistic
    /// struct on the inline path and takes only the pathological ones off it.
    const BLOCK_COPY_INLINE_LIMIT: i64 = 128;

    /// Emit a block copy from src to dst using integer chunks.
    /// The destination stores start at dst_base_offset.
    pub(crate) fn emit_block_copy_at_offset(
        &mut self,
        dst: PseudoId,
        dst_base_offset: i64,
        src: PseudoId,
        size_bytes: i64,
    ) {
        if size_bytes > Self::BLOCK_COPY_INLINE_LIMIT {
            self.emit_block_copy_call(dst, dst_base_offset, src, size_bytes);
            return;
        }
        let mut offset: i64 = 0;
        while offset + 8 <= size_bytes {
            let tmp = self.alloc_pseudo();
            self.emit(Instruction::load(tmp, src, offset, self.types.ulong_id, 64));
            self.emit(Instruction::store(
                tmp,
                dst,
                dst_base_offset + offset,
                self.types.ulong_id,
                64,
            ));
            offset += 8;
        }
        let remaining = size_bytes - offset;
        if remaining >= 4 {
            let tmp = self.alloc_pseudo();
            self.emit(Instruction::load(tmp, src, offset, self.types.uint_id, 32));
            self.emit(Instruction::store(
                tmp,
                dst,
                dst_base_offset + offset,
                self.types.uint_id,
                32,
            ));
            offset += 4;
        }
        if remaining % 4 >= 2 {
            let tmp = self.alloc_pseudo();
            self.emit(Instruction::load(
                tmp,
                src,
                offset,
                self.types.ushort_id,
                16,
            ));
            self.emit(Instruction::store(
                tmp,
                dst,
                dst_base_offset + offset,
                self.types.ushort_id,
                16,
            ));
            offset += 2;
        }
        if remaining % 2 == 1 {
            let tmp = self.alloc_pseudo();
            self.emit(Instruction::load(tmp, src, offset, self.types.uchar_id, 8));
            self.emit(Instruction::store(
                tmp,
                dst,
                dst_base_offset + offset,
                self.types.uchar_id,
                8,
            ));
        }
    }

    /// The same copy as a `memcpy` call.
    ///
    /// `dst_base_offset` is folded into the destination pointer first, since
    /// `memcpy` takes an address rather than a base and a displacement.
    fn emit_block_copy_call(
        &mut self,
        dst: PseudoId,
        dst_base_offset: i64,
        src: PseudoId,
        size_bytes: i64,
    ) {
        // `memcpy` takes addresses. A `Sym` pseudo names a local's *storage*,
        // not a pointer to it -- the inline path could store through it
        // directly, this one cannot. Passing the Sym itself handed memcpy a
        // meaningless value and segfaulted every copy over the threshold.
        // `rvalue_addr` is the existing answer to this question and returns a
        // non-Sym pseudo unchanged.
        let void_ptr = self.types.void_ptr_id;
        let dst = self.rvalue_addr(dst, void_ptr);
        let src = self.rvalue_addr(src, void_ptr);

        let dst_ptr = if dst_base_offset == 0 {
            dst
        } else {
            let off = self.emit_const(dst_base_offset as i128, self.types.long_id);
            let adjusted = self.alloc_reg_pseudo();
            self.emit(Instruction::binop(
                Opcode::Add,
                adjusted,
                dst,
                off,
                self.types.void_ptr_id,
                64,
            ));
            adjusted
        };
        let n = self.emit_const(size_bytes as i128, self.types.ulong_id);
        let result = self.alloc_pseudo();
        self.emit(
            Instruction::new(Opcode::Memcpy)
                .with_target(result)
                .with_src3(dst_ptr, src, n)
                .with_type_and_size(self.types.void_ptr_id, 64),
        );
    }

    /// Emit code to load a bitfield value
    /// Returns the loaded value as a PseudoId
    pub(crate) fn emit_bitfield_load(
        &mut self,
        base: PseudoId,
        byte_offset: usize,
        bit_offset: u32,
        bit_width: u32,
        storage_size: u32,
        typ: TypeId,
    ) -> PseudoId {
        // A span that is not one addressable unit has to be assembled a byte at
        // a time. Only a packed bit-field produces one, and only then can the
        // span exceed eight bytes -- `packed { char c:1; unsigned long long
        // a:64; }` needs nine, in a nine-byte object, so no power-of-two window
        // covers the field without reading past the end of it.
        // 16 joins the list for `__int128` bit-fields wider than 64 bits: the
        // byte-wise fallback assembles into a 64-bit carrier and cannot hold
        // them. A packed field still lands there -- its span is not an
        // addressable unit -- and packing plus a >64-bit width is refused in
        // `validate_bitfield` for that reason.
        if !matches!(storage_size, 1 | 2 | 4 | 8 | 16) {
            return self.emit_bitfield_load_bytewise(
                base,
                byte_offset,
                bit_offset,
                bit_width,
                storage_size,
                typ,
            );
        }

        // Determine storage type based on storage unit size
        let storage_type = self.bitfield_storage_type(storage_size);
        let storage_bits = storage_size * 8;

        // 1. Load the entire storage unit
        let storage_val = self.alloc_pseudo();
        self.emit(Instruction::load(
            storage_val,
            base,
            byte_offset as i64,
            storage_type,
            storage_bits,
        ));

        // 2. Shift right by bit_offset (using logical shift for unsigned extraction)
        let shifted = if bit_offset > 0 {
            let shift_amount = self.emit_const(bit_offset as i128, self.types.int_id);
            let shifted = self.alloc_pseudo();
            self.emit(Instruction::binop(
                Opcode::Lsr,
                shifted,
                storage_val,
                shift_amount,
                storage_type,
                storage_bits,
            ));
            shifted
        } else {
            storage_val
        };

        // 3. Mask to bit_width bits
        let mask = bitfield_value_mask_128(bit_width);
        let mask_val = self.emit_const(mask as i128, storage_type);
        let masked = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            masked,
            shifted,
            mask_val,
            storage_type,
            storage_bits,
        ));

        // 4. Sign extend if this is a signed bitfield.
        //
        // The value's width is the *declared type's*, not the access unit's.
        // Extending to the unit left `long long a:4` sign-extended only as far
        // as the unit reached -- for a packed field that unit is one byte, so
        // -1 came back 4294967295 with `a < 0` false. And where the field
        // exactly fills its unit the guard was false outright, so a signed
        // field got no extension at all.
        let value_bits = self.types.size_bits(typ);
        if !self.types.is_unsigned(typ) && bit_width < value_bits {
            self.emit_sign_extend_bitfield(masked, bit_width, value_bits)
        } else {
            self.widen_bitfield_to_declared(masked, storage_type, storage_bits, typ, value_bits)
        }
    }

    /// Give the extracted field the width its declared type claims.
    ///
    /// Everything above runs at the *storage unit's* width, and a packed
    /// field's unit can be narrower than the type it was declared with:
    /// `unsigned short k : 8` sitting alone in one byte is masked by an
    /// eight-bit `and`, and the caller then converts it *from sixteen bits*,
    /// because sixteen is what its type says. The value is right in a
    /// register -- the unit's own load zero-extended it -- so nothing failed
    /// while it stayed in one. It fails the moment it becomes a constant:
    /// `0xFF` canonicalized at eight bits is `-1`, and the sixteen-bit
    /// zero-extension the caller emits then reads that as `0xFFFF`. So the
    /// answer is not to teach the folder about the discrepancy but to not
    /// have one -- the value a bit-field load hands back is readable at the
    /// width its type names.
    fn widen_bitfield_to_declared(
        &mut self,
        value: PseudoId,
        storage_type: TypeId,
        storage_bits: u32,
        typ: TypeId,
        value_bits: u32,
    ) -> PseudoId {
        if storage_bits >= value_bits {
            // Already at least as wide, and masked to the field, so reading
            // it at the declared width gives the same value.
            return value;
        }
        self.emit_convert(value, storage_type, typ)
    }

    /// Reduce a value to what a bit-field of `bit_width` would hold.
    ///
    /// C17 6.5.16.1p2 makes the value of an assignment expression the value
    /// *stored in* the object, and for a bit-field that is the truncated,
    /// sign-extended field -- not the value that was assigned. So
    /// `(x.f = 9)` with `signed int f : 3` is 1, and `x.f = 7` is -1.
    /// `++x.f`, `x.f--` and the compound forms are the same question.
    ///
    /// The store path already narrows correctly; this is only about the value
    /// handed back to the surrounding expression, which was the unnarrowed
    /// one. Mirrors the tail of `emit_bitfield_load`, including its rule that
    /// the width to extend to is the *declared type's*, not the storage
    /// unit's.
    pub(crate) fn narrow_to_bitfield(
        &mut self,
        value: PseudoId,
        bit_width: u32,
        typ: TypeId,
    ) -> PseudoId {
        let value_bits = self.types.size_bits(typ);
        if bit_width >= value_bits {
            // The field fills its declared type; nothing to narrow, and the
            // mask below would be a no-op that only costs an instruction.
            return value;
        }
        let mask = bitfield_value_mask_128(bit_width);
        let mask_val = self.emit_const(mask as i128, typ);
        let masked = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            masked,
            value,
            mask_val,
            typ,
            value_bits,
        ));
        if self.types.is_unsigned(typ) {
            masked
        } else {
            self.emit_sign_extend_bitfield(masked, bit_width, value_bits)
        }
    }

    /// Assemble a bit-field from an arbitrary byte range, one byte at a time.
    ///
    /// This is what gcc emits for a packed bit-field on both targets, and it is
    /// not one option among several: a packed struct can be *smaller* than any
    /// power-of-two window covering the field, so a wide load would read past
    /// the object -- a fault risk at a page boundary and an out-of-bounds
    /// report under any sanitizer.
    ///
    /// The carrier is 64-bit whenever the field's bits reach past bit 32. Every
    /// shift is in range: the last byte contributes at `8*(span-1) -
    /// bit_offset`, which is below `bit_width` and so below 64.
    fn emit_bitfield_load_bytewise(
        &mut self,
        base: PseudoId,
        byte_offset: usize,
        bit_offset: u32,
        bit_width: u32,
        span: u32,
        typ: TypeId,
    ) -> PseudoId {
        let wide = bit_offset + bit_width > 32;
        let carrier = if wide {
            self.types.ulong_id
        } else {
            self.types.uint_id
        };
        let carrier_bits = if wide { 64 } else { 32 };
        let byte_type = self.types.uchar_id;

        let mut acc: Option<PseudoId> = None;
        let (field_lo, field_hi) = (bit_offset, bit_offset + bit_width);
        for i in 0..span {
            // Only the bytes the field's own bits reach. A span wider than the
            // field -- every `__int128` bit-field, whose access window is
            // sixteen bytes -- would otherwise read the object's *padding* and
            // shift it by more than the carrier's width, which x86-64 masks
            // into the result and aarch64's assembler rejects outright.
            let (byte_lo, byte_hi) = (8 * i, 8 * i + 8);
            if field_lo.max(byte_lo) >= field_hi.min(byte_hi) {
                continue;
            }
            let byte = self.alloc_pseudo();
            self.emit(Instruction::load(
                byte,
                base,
                (byte_offset + i as usize) as i64,
                byte_type,
                8,
            ));
            // Widen before shifting, or the shift is done at eight bits and
            // drops everything it moves. `uchar` is unsigned, so this is a
            // zero-extension and the byte's own value is preserved.
            let widened = self.emit_convert(byte, byte_type, carrier);

            // Byte `i` covers bits `8i..8i+8` of the span, and the field starts
            // at `bit_offset`, so this byte lands at `8i - bit_offset` --
            // negative only for the first byte, which shifts *down* instead.
            let shift = 8i64 * i as i64 - bit_offset as i64;
            let placed = if shift == 0 {
                widened
            } else {
                let amount = self.emit_const(shift.unsigned_abs() as i128, self.types.int_id);
                let out = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    if shift > 0 { Opcode::Shl } else { Opcode::Lsr },
                    out,
                    widened,
                    amount,
                    carrier,
                    carrier_bits,
                ));
                out
            };

            acc = Some(match acc {
                None => placed,
                Some(prev) => {
                    let out = self.alloc_pseudo();
                    self.emit(Instruction::binop(
                        Opcode::Or,
                        out,
                        prev,
                        placed,
                        carrier,
                        carrier_bits,
                    ));
                    out
                }
            });
        }

        let assembled = acc.expect("a bit-field spans at least one byte");
        let mask_val = self.emit_const(bitfield_value_mask(bit_width) as i128, carrier);
        let masked = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            masked,
            assembled,
            mask_val,
            carrier,
            carrier_bits,
        ));

        // As above: the declared type's width, not the carrier's.
        let value_bits = self.types.size_bits(typ);
        if !self.types.is_unsigned(typ) && bit_width < value_bits {
            self.emit_sign_extend_bitfield(masked, bit_width, value_bits)
        } else {
            self.widen_bitfield_to_declared(masked, carrier, carrier_bits, typ, value_bits)
        }
    }

    /// Sign-extend a bitfield value from bit_width to target_bits
    pub(crate) fn emit_sign_extend_bitfield(
        &mut self,
        value: PseudoId,
        bit_width: u32,
        target_bits: u32,
    ) -> PseudoId {
        // Sign extend by shifting the field's top bit up to the sign bit and
        // arithmetic-shifting it back down.
        //
        // The shift is measured against the width the *operation* runs at, not
        // against the storage unit: a field backed by a one-byte unit is still
        // extracted in a 32-bit register, and shifting by `8 - width` would
        // leave its top bit at bit 7, where an arithmetic shift sees a positive
        // value and extends nothing. A field of `__int128` needs the 128-bit
        // tier for the same reason -- capping at 64 sign-extends only the low
        // half.
        let (typ, op_bits) = if target_bits <= 32 {
            (self.types.int_id, 32)
        } else if target_bits <= 64 {
            (self.types.long_id, 64)
        } else {
            (self.types.int128_id, 128)
        };
        let shift_amount = op_bits - bit_width;

        let shift_val = self.emit_const(shift_amount as i128, typ);
        let shifted_left = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Shl,
            shifted_left,
            value,
            shift_val,
            typ,
            op_bits,
        ));

        let result = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Asr,
            result,
            shifted_left,
            shift_val,
            typ,
            op_bits,
        ));
        result
    }

    /// Emit code to store a value into a bitfield
    pub(crate) fn emit_bitfield_store(
        &mut self,
        base: PseudoId,
        byte_offset: usize,
        bit_offset: u32,
        bit_width: u32,
        storage_size: u32,
        new_value: PseudoId,
    ) {
        if !matches!(storage_size, 1 | 2 | 4 | 8 | 16) {
            return self.emit_bitfield_store_bytewise(
                base,
                byte_offset,
                bit_offset,
                bit_width,
                storage_size,
                new_value,
            );
        }

        // Determine storage type based on storage unit size
        let storage_type = self.bitfield_storage_type(storage_size);
        let storage_bits = storage_size * 8;

        // 1. Load current storage unit value
        let old_val = self.alloc_pseudo();
        self.emit(Instruction::load(
            old_val,
            base,
            byte_offset as i64,
            storage_type,
            storage_bits,
        ));

        // 2. Create mask for the bitfield bits: ~(((1 << width) - 1) << offset)
        //
        // Complemented inside the storage unit rather than inside the widest
        // carrier: `!field_mask` alone sets every bit above the unit.
        let unit_mask = bitfield_value_mask_128(storage_bits);
        let field_mask = bitfield_value_mask_128(bit_width) << bit_offset;
        let clear_mask = !field_mask & unit_mask;
        let clear_mask_val = self.emit_const(clear_mask as i128, storage_type);

        // 3. Clear the bitfield bits in old value
        let cleared = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            cleared,
            old_val,
            clear_mask_val,
            storage_type,
            storage_bits,
        ));

        // 4. Mask new value to bit_width and shift to position
        let value_mask = bitfield_value_mask_128(bit_width);
        let value_mask_val = self.emit_const(value_mask as i128, storage_type);
        let masked_new = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            masked_new,
            new_value,
            value_mask_val,
            storage_type,
            storage_bits,
        ));

        let positioned = if bit_offset > 0 {
            let shift_val = self.emit_const(bit_offset as i128, self.types.int_id);
            let positioned = self.alloc_pseudo();
            self.emit(Instruction::binop(
                Opcode::Shl,
                positioned,
                masked_new,
                shift_val,
                storage_type,
                storage_bits,
            ));
            positioned
        } else {
            masked_new
        };

        // 5. OR cleared value with positioned new value
        let combined = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Or,
            combined,
            cleared,
            positioned,
            storage_type,
            storage_bits,
        ));

        // 6. Store back
        self.emit(Instruction::store(
            combined,
            base,
            byte_offset as i64,
            storage_type,
            storage_bits,
        ));
    }

    /// Write a bit-field occupying an arbitrary byte range, one byte at a time.
    ///
    /// A byte the field covers completely is stored outright; a byte it shares
    /// with a neighbour is read, masked and written back, so the neighbour's
    /// bits survive. Neither ever touches a byte outside the field's own span,
    /// which is what a wide read-modify-write could not promise: the span may
    /// end at the last byte of the object.
    fn emit_bitfield_store_bytewise(
        &mut self,
        base: PseudoId,
        byte_offset: usize,
        bit_offset: u32,
        bit_width: u32,
        span: u32,
        new_value: PseudoId,
    ) {
        let wide = bit_offset + bit_width > 32;
        let carrier = if wide {
            self.types.ulong_id
        } else {
            self.types.uint_id
        };
        let carrier_bits = if wide { 64 } else { 32 };
        let byte_type = self.types.uchar_id;

        // The value, masked to its width once, so no byte can contribute bits
        // the field does not have.
        let width_mask = self.emit_const(bitfield_value_mask(bit_width) as i128, carrier);
        let value = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            value,
            new_value,
            width_mask,
            carrier,
            carrier_bits,
        ));

        let field_lo = bit_offset;
        let field_hi = bit_offset + bit_width;
        for i in 0..span {
            let byte_lo = 8 * i;
            let byte_hi = byte_lo + 8;
            let lo = field_lo.max(byte_lo);
            let hi = field_hi.min(byte_hi);
            if lo >= hi {
                continue; // no bits of the field in this byte
            }

            // Bits [lo,hi) of the span come from bits [lo-field_lo, hi-field_lo)
            // of the value and land at bit `lo - byte_lo` of this byte.
            let from = lo - field_lo;
            let shifted = if from == 0 {
                value
            } else {
                let amount = self.emit_const(from as i128, self.types.int_id);
                let out = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Lsr,
                    out,
                    value,
                    amount,
                    carrier,
                    carrier_bits,
                ));
                out
            };
            let piece = self.emit_convert(shifted, carrier, byte_type);

            let within = lo - byte_lo;
            let covered = hi - lo;
            let byte_mask = (bitfield_value_mask(covered) as u32) << within;
            let placed = if within == 0 {
                piece
            } else {
                let amount = self.emit_const(within as i128, self.types.int_id);
                let out = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Shl,
                    out,
                    piece,
                    amount,
                    byte_type,
                    8,
                ));
                out
            };

            let addr_off = (byte_offset + i as usize) as i64;
            let to_store = if byte_mask == 0xff {
                // The field owns every bit of this byte, so nothing has to be
                // preserved and the read can be skipped.
                placed
            } else {
                let old = self.alloc_pseudo();
                self.emit(Instruction::load(old, base, addr_off, byte_type, 8));
                let keep = self.emit_const((!byte_mask & 0xff) as i128, byte_type);
                let cleared = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::And,
                    cleared,
                    old,
                    keep,
                    byte_type,
                    8,
                ));
                let masked_piece = {
                    let m = self.emit_const(byte_mask as i128, byte_type);
                    let out = self.alloc_pseudo();
                    self.emit(Instruction::binop(
                        Opcode::And,
                        out,
                        placed,
                        m,
                        byte_type,
                        8,
                    ));
                    out
                };
                let out = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Or,
                    out,
                    cleared,
                    masked_piece,
                    byte_type,
                    8,
                ));
                out
            };
            self.emit(Instruction::store(to_store, base, addr_off, byte_type, 8));
        }
    }

    pub(crate) fn emit_unary(&mut self, op: UnaryOp, src: PseudoId, typ: TypeId) -> PseudoId {
        let is_float = self.types.is_float(typ);
        let size = self.types.size_bits(typ);

        let result = self.alloc_pseudo();

        let opcode = match op {
            // Intercepted in `linearize_unary`, which loads the half directly.
            UnaryOp::Real | UnaryOp::Imag => {
                unreachable!("__real__/__imag__ are lowered before emit_unary")
            }
            UnaryOp::Neg => {
                if is_float {
                    Opcode::FNeg
                } else {
                    Opcode::Neg
                }
            }
            UnaryOp::Not => {
                // Logical not: compare with 0
                if is_float {
                    let zero = self.emit_fconst(FloatVal::ZERO, typ);
                    self.emit(Instruction::binop(
                        Opcode::FCmpOEq,
                        result,
                        src,
                        zero,
                        typ,
                        size,
                    ));
                } else {
                    let zero = self.emit_const(0, typ);
                    self.emit(Instruction::binop(
                        Opcode::SetEq,
                        result,
                        src,
                        zero,
                        typ,
                        size,
                    ));
                }
                return result;
            }
            UnaryOp::BitNot => Opcode::Not,
            UnaryOp::AddrOf => {
                return src;
            }
            UnaryOp::Deref => {
                // Dereferencing a pointer-to-array gives an array, which is just an address
                // (arrays decay to their first element's address)
                let type_kind = self.types.kind(typ);
                if type_kind == TypeKind::Array {
                    return src;
                }
                // In C, dereferencing a function pointer is a no-op:
                // *func_ptr == func_ptr (C99 6.5.3.2, 6.3.2.1)
                if type_kind == TypeKind::Function {
                    return src;
                }
                // An aggregate wider than a register travels by address; one
                // that fits travels *as* its value. Both kinds, on the same
                // rule: a struct returned the address at every size while a
                // union already loaded when it fit, and the disagreement was
                // the bug. A caller handed the address where the convention
                // promised the value stored the pointer instead --
                // `struct { unsigned a, b; } q = *p;` put `p` into `q`.
                //
                // Member access is unaffected: `(*p).f` and `p->f` take the
                // address through `linearize_lvalue`, not through here, which
                // is why the union half of this has worked all along.
                if (type_kind == TypeKind::Struct || type_kind == TypeKind::Union) && size > 64 {
                    return src;
                }
                self.emit(Instruction::load(result, src, 0, typ, size));
                return result;
            }
            // Intercepted in `linearize_unary`, which needs the operand
            // expression: a pointer's step is not always its pointee's
            // compile-time size, and a variably-modified one reports 0.
            UnaryOp::PreInc | UnaryOp::PreDec => {
                unreachable!("++/-- are lowered before emit_unary")
            }
        };

        self.emit(Instruction::unop(opcode, result, src, typ, size));
        result
    }

    pub(crate) fn emit_binary(
        &mut self,
        op: BinaryOp,
        left: PseudoId,
        right: PseudoId,
        result_typ: TypeId,
        operand_typ: TypeId,
    ) -> PseudoId {
        let is_float = self.types.is_float(operand_typ);
        // A pointer is not an integer type, so `is_unsigned` says false for
        // one -- but C17 6.5.8 compares pointers by address, and an address is
        // unsigned. Taking the signed answer emitted `setl` where gcc emits
        // `setb`, which differs for any pair straddling the sign bit. The
        // arithmetic opcodes below are unreachable for a pointer operand, so
        // one predicate serves both.
        let is_unsigned = self.types.is_unsigned(operand_typ)
            || self.types.kind(operand_typ) == TypeKind::Pointer;

        let result = self.alloc_pseudo();

        let opcode = match op {
            BinaryOp::Add => {
                if is_float {
                    Opcode::FAdd
                } else {
                    Opcode::Add
                }
            }
            BinaryOp::Sub => {
                if is_float {
                    Opcode::FSub
                } else {
                    Opcode::Sub
                }
            }
            BinaryOp::Mul => {
                if is_float {
                    Opcode::FMul
                } else {
                    Opcode::Mul
                }
            }
            BinaryOp::Div => {
                if is_float {
                    Opcode::FDiv
                } else if is_unsigned {
                    Opcode::DivU
                } else {
                    Opcode::DivS
                }
            }
            BinaryOp::Mod => {
                // Modulo is not supported for floats in hardware - use fmod() library call
                // For now, use integer modulo (semantic analysis should catch float % float)
                if is_unsigned {
                    Opcode::ModU
                } else {
                    Opcode::ModS
                }
            }
            BinaryOp::Lt => {
                if is_float {
                    Opcode::FCmpOLt
                } else if is_unsigned {
                    Opcode::SetB
                } else {
                    Opcode::SetLt
                }
            }
            BinaryOp::Gt => {
                if is_float {
                    Opcode::FCmpOGt
                } else if is_unsigned {
                    Opcode::SetA
                } else {
                    Opcode::SetGt
                }
            }
            BinaryOp::Le => {
                if is_float {
                    Opcode::FCmpOLe
                } else if is_unsigned {
                    Opcode::SetBe
                } else {
                    Opcode::SetLe
                }
            }
            BinaryOp::Ge => {
                if is_float {
                    Opcode::FCmpOGe
                } else if is_unsigned {
                    Opcode::SetAe
                } else {
                    Opcode::SetGe
                }
            }
            BinaryOp::Eq => {
                if is_float {
                    Opcode::FCmpOEq
                } else {
                    Opcode::SetEq
                }
            }
            BinaryOp::Ne => {
                if is_float {
                    Opcode::FCmpONe
                } else {
                    Opcode::SetNe
                }
            }
            // LogAnd and LogOr are handled earlier in linearize_expr via
            // emit_logical_and/emit_logical_or for proper short-circuit evaluation
            BinaryOp::LogAnd | BinaryOp::LogOr => {
                unreachable!("LogAnd/LogOr should be handled in ExprKind::Binary")
            }
            BinaryOp::BitAnd => Opcode::And,
            BinaryOp::BitOr => Opcode::Or,
            BinaryOp::BitXor => Opcode::Xor,
            BinaryOp::Shl => Opcode::Shl,
            BinaryOp::Shr => {
                // Logical shift for unsigned, arithmetic for signed
                if is_unsigned {
                    Opcode::Lsr
                } else {
                    Opcode::Asr
                }
            }
        };

        // For comparison operations, use operand_typ to ensure correct size
        // (comparisons produce int result but must operate at operand size)
        let insn_typ = match opcode {
            Opcode::SetEq
            | Opcode::SetNe
            | Opcode::SetLt
            | Opcode::SetLe
            | Opcode::SetGt
            | Opcode::SetGe
            | Opcode::SetB
            | Opcode::SetBe
            | Opcode::SetA
            | Opcode::SetAe
            | Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => operand_typ,
            _ => result_typ,
        };
        let insn_size = self.types.size_bits(insn_typ);
        self.emit(Instruction::binop(
            opcode, result, left, right, insn_typ, insn_size,
        ));
        result
    }

    /// Emit complex arithmetic operation
    /// Promote a real scalar expression to a complex value (real=value, imag=0.0)
    /// Returns the address of the complex temp
    pub(crate) fn promote_real_to_complex(&mut self, expr: &Expr, complex_typ: TypeId) -> PseudoId {
        let val = self.linearize_expr(expr);
        let expr_typ = self.expr_type(expr);
        self.promote_real_value_to_complex(val, expr_typ, complex_typ)
    }

    /// [`Self::promote_real_to_complex`] for a value already in hand.
    ///
    /// `a ?: b` evaluates its left operand exactly once and then needs it both
    /// as the truth test and as the result, so it cannot go back to the `Expr`
    /// for a second look.
    pub(crate) fn promote_real_value_to_complex(
        &mut self,
        val: PseudoId,
        val_typ: TypeId,
        complex_typ: TypeId,
    ) -> PseudoId {
        let base_typ = self.types.complex_base(complex_typ);
        let base_bits = self.types.size_bits(base_typ);
        let base_bytes = (base_bits / 8) as i64;

        let converted = self.emit_convert(val, val_typ, base_typ);

        let result = self.alloc_local_temp(complex_typ);
        self.emit(Instruction::store(
            converted, result, 0, base_typ, base_bits,
        ));
        let zero = self.complex_half_zero(base_typ);
        self.emit(Instruction::store(
            zero, result, base_bytes, base_typ, base_bits,
        ));
        result
    }

    /// The address of a complex operand, converted to `complex_typ` if its own
    /// base precision differs.
    ///
    /// `emit_complex_binary` reads both operands with the *result* type's base
    /// type and stride, which is only correct when everything already agrees.
    /// It rarely does: `<complex.h>` defines `I` as `__builtin_complex(0.0,
    /// 1.0)`, a **double** complex, so `3.0L * I` mixes precisions. Reading an
    /// 8-byte-strided value with a 16-byte stride picked up the wrong bytes
    /// entirely — `3.0L * I` came out as `0 + 9i`.
    pub(crate) fn complex_operand_at_precision(
        &mut self,
        expr: &Expr,
        complex_typ: TypeId,
    ) -> PseudoId {
        let src_typ = self.expr_type(expr);
        let addr = self.complex_operand_addr(expr);
        self.complex_addr_at_precision(addr, src_typ, complex_typ)
    }

    /// [`Self::complex_operand_at_precision`] for a value already in hand.
    ///
    /// `?:` evaluates its left operand exactly once and then needs it both as
    /// the truth test and as the result, so it cannot go back to the `Expr`
    /// for a second look. Taking the address instead keeps that single
    /// evaluation while still sharing the conversion.
    pub(crate) fn complex_addr_at_precision(
        &mut self,
        addr: PseudoId,
        src_typ: TypeId,
        complex_typ: TypeId,
    ) -> PseudoId {
        let src_base = self.types.complex_base(src_typ);
        let dst_base = self.types.complex_base(complex_typ);
        if src_base == dst_base {
            return addr;
        }

        let src_bits = self.types.size_bits(src_base);
        let src_bytes = (src_bits / 8) as i64;
        let dst_bits = self.types.size_bits(dst_base);
        let dst_bytes = (dst_bits / 8) as i64;

        let result = self.alloc_local_temp(complex_typ);
        for (src_off, dst_off) in [(0, 0), (src_bytes, dst_bytes)] {
            let part = self.alloc_pseudo();
            self.emit(Instruction::load(part, addr, src_off, src_base, src_bits));
            let part = self.emit_convert(part, src_base, dst_base);
            self.emit(Instruction::store(
                part, result, dst_off, dst_base, dst_bits,
            ));
        }
        result
    }

    /// The opcodes that operate on one half of a complex value of `base_typ`.
    ///
    /// `_Complex int` is a GNU extension, and its halves are integers: the
    /// component arithmetic is `Add`/`Sub`/`Mul`, the comparisons are
    /// `SetEq`/`SetNe`, and negation is `Neg`. Every complex emitter used to
    /// hard-code the floating opcode, which handed an integer pair to the SSE
    /// unit.
    fn complex_half_ops(&self, base_typ: TypeId) -> ComplexHalfOps {
        if self.types.is_integer(base_typ) {
            ComplexHalfOps {
                add: Opcode::Add,
                sub: Opcode::Sub,
                neg: Opcode::Neg,
                eq: Opcode::SetEq,
                ne: Opcode::SetNe,
                integral: true,
            }
        } else {
            ComplexHalfOps {
                add: Opcode::FAdd,
                sub: Opcode::FSub,
                neg: Opcode::FNeg,
                // The floating predicates are the *ordered* ones, so a NaN
                // half compares unequal to everything and -0.0 equals 0.0.
                eq: Opcode::FCmpOEq,
                ne: Opcode::FCmpONe,
                integral: false,
            }
        }
    }

    /// Zero of the type one half of a complex value has.
    pub(crate) fn complex_half_zero(&mut self, base_typ: TypeId) -> PseudoId {
        if self.types.is_integer(base_typ) {
            self.emit_const(0, base_typ)
        } else {
            self.emit_fconst(FloatVal::ZERO, base_typ)
        }
    }

    /// `-z` on a complex value: negate both parts (C17 6.5.3.3p3).
    ///
    /// A complex value travels by address, and the scalar unary path did not
    /// know that: it negated the *address* as an integer and handed the result
    /// on as though it were a complex object, so `creal(-z)` dereferenced a
    /// small negative number and the program died on valid code.
    ///
    /// Returns the address of the result, as every complex-valued expression
    /// does.
    pub(crate) fn emit_complex_negate(&mut self, operand: &Expr, complex_typ: TypeId) -> PseudoId {
        self.emit_complex_half_negate(operand, complex_typ, true)
    }

    /// `~z` on a complex value: the complex conjugate (a GNU extension).
    ///
    /// gcc gives `~` this meaning for every complex type, floating and
    /// integer. c17 had no case for it, so the scalar path bit-complemented
    /// the value's *address* and the result was dereferenced as a pointer:
    /// `~z` segfaulted on valid code.
    pub(crate) fn emit_complex_conjugate(
        &mut self,
        operand: &Expr,
        complex_typ: TypeId,
    ) -> PseudoId {
        self.emit_complex_half_negate(operand, complex_typ, false)
    }

    /// The shared body of `-z` and `~z`: negate the imaginary half always, and
    /// the real half only for negation.
    fn emit_complex_half_negate(
        &mut self,
        operand: &Expr,
        complex_typ: TypeId,
        negate_real: bool,
    ) -> PseudoId {
        let base_typ = self.types.complex_base(complex_typ);
        let base_bits = self.types.size_bits(base_typ);
        let base_bytes = (base_bits / 8) as i64;

        let addr = self.complex_operand_at_precision(operand, complex_typ);
        let result = self.alloc_local_temp(complex_typ);

        let ops = self.complex_half_ops(base_typ);
        for (offset, negate) in [(0, negate_real), (base_bytes, true)] {
            let part = self.alloc_pseudo();
            self.emit(Instruction::load(part, addr, offset, base_typ, base_bits));
            let stored = if negate {
                let negated = self.alloc_pseudo();
                self.emit(Instruction::unop(
                    ops.neg, negated, part, base_typ, base_bits,
                ));
                negated
            } else {
                part
            };
            self.emit(Instruction::store(
                stored, result, offset, base_typ, base_bits,
            ));
        }
        result
    }

    /// The real part of a complex value, as the real type `target_typ`.
    ///
    /// C17 6.3.1.7p2: converting a complex value to a real type discards the
    /// imaginary part. The conversion path treated the operand as an ordinary
    /// scalar, so `(double) z` reinterpreted the *address* as a double.
    pub(crate) fn emit_complex_to_real(&mut self, operand: &Expr, target_typ: TypeId) -> PseudoId {
        let src_typ = self.expr_type(operand);
        let base_typ = self.types.complex_base(src_typ);
        let base_bits = self.types.size_bits(base_typ);

        let addr = self.complex_operand_addr(operand);
        let real = self.alloc_pseudo();
        self.emit(Instruction::load(real, addr, 0, base_typ, base_bits));
        self.emit_convert(real, base_typ, target_typ)
    }

    /// Complex values are stored as two adjacent float/double values (real, imag)
    /// This function expands complex ops to operations on the component parts
    pub(crate) fn emit_complex_binary(
        &mut self,
        op: BinaryOp,
        left_addr: PseudoId,
        right_addr: PseudoId,
        complex_typ: TypeId,
    ) -> PseudoId {
        // Get the base float type (float, double, or long double)
        let base_typ = self.types.complex_base(complex_typ);
        let base_size = self.types.size_bits(base_typ);
        let base_bytes = (base_size / 8) as i64;

        // Allocate result temporary (stack space for the complex result)
        let result_addr = self.alloc_local_temp(complex_typ);

        // Load real and imaginary parts of left operand
        let left_real = self.alloc_pseudo();
        self.emit(Instruction::load(
            left_real, left_addr, 0, base_typ, base_size,
        ));

        let left_imag = self.alloc_pseudo();
        self.emit(Instruction::load(
            left_imag, left_addr, base_bytes, base_typ, base_size,
        ));

        // Load real and imaginary parts of right operand
        let right_real = self.alloc_pseudo();
        self.emit(Instruction::load(
            right_real, right_addr, 0, base_typ, base_size,
        ));

        let right_imag = self.alloc_pseudo();
        self.emit(Instruction::load(
            right_imag, right_addr, base_bytes, base_typ, base_size,
        ));

        // Perform the operation on the components
        let ops = self.complex_half_ops(base_typ);
        let (result_real, result_imag) = match op {
            BinaryOp::Add => {
                // (a + bi) + (c + di) = (a+c) + (b+d)i
                let real = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    ops.add, real, left_real, right_real, base_typ, base_size,
                ));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    ops.add, imag, left_imag, right_imag, base_typ, base_size,
                ));
                (real, imag)
            }
            BinaryOp::Sub => {
                // (a + bi) - (c + di) = (a-c) + (b-d)i
                let real = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    ops.sub, real, left_real, right_real, base_typ, base_size,
                ));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    ops.sub, imag, left_imag, right_imag, base_typ, base_size,
                ));
                (real, imag)
            }
            // The integer forms are open-coded: there is no `__mulic3`, and
            // the runtime helpers' NaN and infinity fix-ups are meaningless
            // for integers. gcc emits the textbook formulae inline too.
            BinaryOp::Mul if ops.integral => self.emit_complex_int_mul(
                (left_real, left_imag),
                (right_real, right_imag),
                base_typ,
                base_size,
            ),
            BinaryOp::Div if ops.integral => {
                // Smith's method branches, so it writes the result itself
                // rather than handing back a pair.
                self.emit_complex_int_div(
                    (left_real, left_imag),
                    (right_real, right_imag),
                    result_addr,
                    base_typ,
                    base_size,
                );
                return result_addr;
            }
            BinaryOp::Mul => {
                // Complex multiply via rtlib call (__mulsc3, __muldc3, etc.)
                let base_kind = self.types.kind(base_typ);
                let func_name = crate::arch::mapping::complex_mul_name(base_kind, self.target);
                let call_result = self.emit_complex_rtlib_call(
                    func_name,
                    (left_real, left_imag),
                    (right_real, right_imag),
                    base_typ,
                    complex_typ,
                );
                // Load real/imag from the call result
                let real = self.alloc_pseudo();
                self.emit(Instruction::load(real, call_result, 0, base_typ, base_size));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::load(
                    imag,
                    call_result,
                    base_bytes,
                    base_typ,
                    base_size,
                ));
                (real, imag)
            }
            BinaryOp::Div => {
                // Complex divide via rtlib call (__divsc3, __divdc3, etc.)
                let base_kind = self.types.kind(base_typ);
                let func_name = crate::arch::mapping::complex_div_name(base_kind, self.target);
                let call_result = self.emit_complex_rtlib_call(
                    func_name,
                    (left_real, left_imag),
                    (right_real, right_imag),
                    base_typ,
                    complex_typ,
                );
                // Load real/imag from the call result
                let real = self.alloc_pseudo();
                self.emit(Instruction::load(real, call_result, 0, base_typ, base_size));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::load(
                    imag,
                    call_result,
                    base_bytes,
                    base_typ,
                    base_size,
                ));
                (real, imag)
            }
            _ => {
                // Other operations not supported for complex types
                error(
                    Position::default(),
                    &format!("unsupported operation {:?} on complex types", op),
                );
                return result_addr;
            }
        };

        // Store result components
        self.emit(Instruction::store(
            result_real,
            result_addr,
            0,
            base_typ,
            base_size,
        ));
        self.emit(Instruction::store(
            result_imag,
            result_addr,
            base_bytes,
            base_typ,
            base_size,
        ));

        result_addr
    }

    /// `(a + bi) * (c + di)` for integer halves: `(ac - bd) + (ad + bc)i`.
    ///
    /// Open-coded rather than routed through `__mul?c3`: those helpers exist
    /// only for the floating formats, and their infinity recovery has no
    /// meaning for a type that wraps.
    fn emit_complex_int_mul(
        &mut self,
        left: (PseudoId, PseudoId),
        right: (PseudoId, PseudoId),
        base_typ: TypeId,
        base_size: u32,
    ) -> (PseudoId, PseudoId) {
        let (a, b) = left;
        let (c, d) = right;
        let ac = self.emit_int_binop(Opcode::Mul, a, c, base_typ, base_size);
        let bd = self.emit_int_binop(Opcode::Mul, b, d, base_typ, base_size);
        let ad = self.emit_int_binop(Opcode::Mul, a, d, base_typ, base_size);
        let bc = self.emit_int_binop(Opcode::Mul, b, c, base_typ, base_size);
        let real = self.emit_int_binop(Opcode::Sub, ac, bd, base_typ, base_size);
        let imag = self.emit_int_binop(Opcode::Add, ad, bc, base_typ, base_size);
        (real, imag)
    }

    /// `(a + bi) / (c + di)` for integer halves, by Smith's method -- the
    /// algorithm gcc uses, so the same source computes the same thing.
    ///
    /// The textbook formula `((ac + bd) + (bc - ad)i) / (c*c + d*d)` is exact
    /// but overflows: `4000000000u / 2u` needs `a * c` to hold 8e9, which a
    /// 32-bit half cannot, and the quotient came out 926258176. Smith's method
    /// divides through by the larger half first, so the products stay near the
    /// magnitude of the operands.
    ///
    /// The cost is a branch -- the two arms differ in which half scales, and
    /// the arm not taken would divide by zero if it were evaluated anyway, so
    /// this cannot be done branch-free. The division truncates toward zero at
    /// every step, like any integer division, which is why gcc (and c17)
    /// answer `6 + 1i` for `(-9 + 38i) / (5 + 6i)` where the exact quotient is
    /// `3 + 4i`. `cc/BUILTIN.md` records that.
    ///
    /// Writes both halves to `result_addr` and leaves the cursor on the merge
    /// block.
    fn emit_complex_int_div(
        &mut self,
        left: (PseudoId, PseudoId),
        right: (PseudoId, PseudoId),
        result_addr: PseudoId,
        base_typ: TypeId,
        base_size: u32,
    ) {
        let (a, b) = left;
        let (c, d) = right;
        let unsigned = self.types.is_unsigned(base_typ);
        let div = if unsigned { Opcode::DivU } else { Opcode::DivS };
        let base_bytes = (base_size / 8) as i64;

        // `|c| < |d|` decides which half scales. The absolute values are
        // non-negative, so an unsigned compare would do for both -- but the
        // signed predicate is used for signed halves so the comparison reads
        // the same way the values do.
        let abs_c = self.emit_int_abs(c, base_typ, base_size);
        let abs_d = self.emit_int_abs(d, base_typ, base_size);
        let cmp = if unsigned {
            Opcode::SetB
        } else {
            Opcode::SetLt
        };
        let cond = self.emit_int_binop(cmp, abs_c, abs_d, base_typ, base_size);

        let small_bb = self.alloc_bb();
        let big_bb = self.alloc_bb();
        let done_bb = self.alloc_bb();
        let entry_bb = self.current_bb.expect("complex divide outside a block");
        self.emit(Instruction::cbr(cond, small_bb, big_bb));
        self.link_bb(entry_bb, small_bb);
        self.link_bb(entry_bb, big_bb);

        // `|c| >= |d|`: r = d/c, denom = c + d*r,
        //               re = (a + b*r)/denom, im = (b - a*r)/denom.
        self.switch_bb(big_bb);
        let r = self.emit_int_binop(div, d, c, base_typ, base_size);
        let dr = self.emit_int_binop(Opcode::Mul, d, r, base_typ, base_size);
        let denom = self.emit_int_binop(Opcode::Add, c, dr, base_typ, base_size);
        let br = self.emit_int_binop(Opcode::Mul, b, r, base_typ, base_size);
        let num_re = self.emit_int_binop(Opcode::Add, a, br, base_typ, base_size);
        let ar = self.emit_int_binop(Opcode::Mul, a, r, base_typ, base_size);
        let num_im = self.emit_int_binop(Opcode::Sub, b, ar, base_typ, base_size);
        self.store_complex_quotient(
            result_addr,
            (num_re, num_im),
            denom,
            div,
            base_typ,
            base_size,
            base_bytes,
        );
        let big_end = self.current_bb.expect("complex divide lost its block");
        self.emit(Instruction::br(done_bb));
        self.link_bb(big_end, done_bb);

        // `|c| < |d|`: r = c/d, denom = d + c*r,
        //              re = (a*r + b)/denom, im = (b*r - a)/denom.
        self.switch_bb(small_bb);
        let r = self.emit_int_binop(div, c, d, base_typ, base_size);
        let cr = self.emit_int_binop(Opcode::Mul, c, r, base_typ, base_size);
        let denom = self.emit_int_binop(Opcode::Add, d, cr, base_typ, base_size);
        let ar = self.emit_int_binop(Opcode::Mul, a, r, base_typ, base_size);
        let num_re = self.emit_int_binop(Opcode::Add, ar, b, base_typ, base_size);
        let br = self.emit_int_binop(Opcode::Mul, b, r, base_typ, base_size);
        let num_im = self.emit_int_binop(Opcode::Sub, br, a, base_typ, base_size);
        self.store_complex_quotient(
            result_addr,
            (num_re, num_im),
            denom,
            div,
            base_typ,
            base_size,
            base_bytes,
        );
        let small_end = self.current_bb.expect("complex divide lost its block");
        self.emit(Instruction::br(done_bb));
        self.link_bb(small_end, done_bb);

        self.switch_bb(done_bb);
    }

    /// Divide both numerators by the shared denominator and store the halves.
    ///
    /// Shared by Smith's two arms, which differ only in how they build the
    /// numerators.
    #[allow(clippy::too_many_arguments)]
    fn store_complex_quotient(
        &mut self,
        result_addr: PseudoId,
        num: (PseudoId, PseudoId),
        denom: PseudoId,
        div: Opcode,
        base_typ: TypeId,
        base_size: u32,
        base_bytes: i64,
    ) {
        let re = self.emit_int_binop(div, num.0, denom, base_typ, base_size);
        let im = self.emit_int_binop(div, num.1, denom, base_typ, base_size);
        self.emit(Instruction::store(re, result_addr, 0, base_typ, base_size));
        self.emit(Instruction::store(
            im,
            result_addr,
            base_bytes,
            base_typ,
            base_size,
        ));
    }

    /// `|x|` for an integer, without a branch.
    ///
    /// `t = x >> (width - 1)` is all ones for a negative value and zero
    /// otherwise, so `(x ^ t) - t` is the magnitude either way. An unsigned
    /// value is already its own magnitude.
    fn emit_int_abs(&mut self, x: PseudoId, typ: TypeId, size: u32) -> PseudoId {
        if self.types.is_unsigned(typ) {
            return x;
        }
        let shift = self.emit_const((size - 1) as i128, typ);
        let sign = self.emit_int_binop(Opcode::Asr, x, shift, typ, size);
        let flipped = self.emit_int_binop(Opcode::Xor, x, sign, typ, size);
        self.emit_int_binop(Opcode::Sub, flipped, sign, typ, size)
    }

    /// One integer binary operation on complex halves, into a fresh pseudo.
    pub(crate) fn emit_int_binop(
        &mut self,
        op: Opcode,
        lhs: PseudoId,
        rhs: PseudoId,
        typ: TypeId,
        size: u32,
    ) -> PseudoId {
        let dst = self.alloc_pseudo();
        self.emit(Instruction::binop(op, dst, lhs, rhs, typ, size));
        dst
    }

    /// Allocate a local temporary variable for a complex result
    pub(crate) fn alloc_local_temp(&mut self, typ: TypeId) -> PseudoId {
        let size = self.types.size_bytes(typ);
        let size_const = self.emit_const(size as i128, self.types.ulong_id);
        let addr = self.alloc_pseudo();
        let alloca_insn = Instruction::new(Opcode::Alloca)
            .with_target(addr)
            .with_src(size_const)
            .with_type_and_size(self.types.void_ptr_id, 64);
        self.emit(alloca_insn);
        addr
    }

    /// Emit a call to a complex rtlib function (__mulXc3, __divXc3).
    ///
    /// These functions take 4 scalar args (left_real, left_imag, right_real, right_imag)
    /// and return a complex value. The result is stored in newly allocated local storage.
    ///
    /// Returns the address where the complex result is stored.
    pub(crate) fn emit_complex_rtlib_call(
        &mut self,
        func_name: &str,
        left: (PseudoId, PseudoId),
        right: (PseudoId, PseudoId),
        base_typ: TypeId,
        complex_typ: TypeId,
    ) -> PseudoId {
        let (left_real, left_imag) = left;
        let (right_real, right_imag) = right;
        // Allocate local storage for the complex result
        let result_sym = self.alloc_pseudo();
        let unique_name = format!("__cret_{}", result_sym.0);
        let result_pseudo = Pseudo::sym(result_sym, unique_name.clone());
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(result_pseudo);
            func.add_local(
                &unique_name,
                result_sym,
                complex_typ,
                false, // not volatile
                false, // not atomic
                self.current_bb,
                None, // no explicit alignment
            );
        }

        // Build argument list: 4 scalar FP values
        let arg_vals = vec![left_real, left_imag, right_real, right_imag];
        let arg_types = vec![base_typ, base_typ, base_typ, base_typ];

        // Compute ABI classification for the call
        let abi = get_abi_for_conv(self.current_calling_conv, self.target);
        let param_classes: Vec<_> = arg_types
            .iter()
            .map(|&t| abi.classify_param(t, self.types))
            .collect();
        let ret_class = abi.classify_return(complex_typ, self.types);
        let call_abi_info = Box::new(CallAbiInfo::new(param_classes, ret_class));

        // Create the call instruction
        let ret_size = self.types.size_bits(complex_typ);
        let mut call_insn = Instruction::call(
            Some(result_sym),
            func_name,
            arg_vals,
            arg_types,
            complex_typ,
            ret_size,
        );
        call_insn.abi_info = Some(call_abi_info);
        self.emit(call_insn);

        result_sym
    }

    /// The two halves of a complex value held at `addr`.
    ///
    /// Every complex operation needs this and each one used to spell it out;
    /// the offsets and the base type have to agree or the halves come back
    /// swapped or mis-sized.
    pub(crate) fn load_complex_halves(
        &mut self,
        addr: PseudoId,
        complex_typ: TypeId,
    ) -> (PseudoId, PseudoId, TypeId, u32) {
        let base_typ = self.types.complex_base(complex_typ);
        let base_bits = self.types.size_bits(base_typ);
        let real = self.alloc_pseudo();
        self.emit(Instruction::load(real, addr, 0, base_typ, base_bits));
        let imag = self.alloc_pseudo();
        self.emit(Instruction::load(
            imag,
            addr,
            (base_bits / 8) as i64,
            base_typ,
            base_bits,
        ));
        (real, imag, base_typ, base_bits)
    }

    /// `expr != 0` for a complex operand: true when *either* half is nonzero.
    ///
    /// C17 6.3.1.2 converts to `_Bool` by comparing against 0, and 6.5.3.3p5
    /// defines `!` in those terms; for a complex value that comparison is
    /// against `0.0 + 0.0i`, so the imaginary half counts. Compared with
    /// `FCmpONe` rather than by OR-ing the bit patterns, so `-0.0 + -0.0i`
    /// comes out false.
    pub(crate) fn emit_complex_nonzero(&mut self, expr: &Expr) -> PseudoId {
        let complex_typ = self.expr_type(expr);
        let addr = self.complex_operand_addr(expr);
        self.emit_complex_nonzero_at(addr, complex_typ)
    }

    /// `!= 0` for the complex value at `addr`.
    pub(crate) fn emit_complex_nonzero_at(
        &mut self,
        addr: PseudoId,
        complex_typ: TypeId,
    ) -> PseudoId {
        let (real, imag, base_typ, base_bits) = self.load_complex_halves(addr, complex_typ);

        let ne = self.complex_half_ops(base_typ).ne;
        let zero = self.complex_half_zero(base_typ);
        let real_nz = self.alloc_pseudo();
        self.emit(Instruction::binop(
            ne, real_nz, real, zero, base_typ, base_bits,
        ));
        let imag_nz = self.alloc_pseudo();
        self.emit(Instruction::binop(
            ne, imag_nz, imag, zero, base_typ, base_bits,
        ));

        let result = self.alloc_pseudo();
        let int_typ = self.types.int_id;
        let int_bits = self.types.size_bits(int_typ);
        self.emit(Instruction::binop(
            Opcode::Or,
            result,
            real_nz,
            imag_nz,
            int_typ,
            int_bits,
        ));
        result
    }

    /// `left == right` / `left != right` for complex operands.
    ///
    /// Two complex values are equal when both halves are (C17 6.5.9): the
    /// halves are compared with the floating-point predicates, so `-0.0`
    /// equals `0.0` and a NaN half compares unequal to everything, and the two
    /// results are combined with `And` for `==` and `Or` for `!=`.
    pub(crate) fn emit_complex_equality(
        &mut self,
        op: BinaryOp,
        left_addr: PseudoId,
        right_addr: PseudoId,
        complex_typ: TypeId,
    ) -> PseudoId {
        let (lre, lim, base_typ, base_bits) = self.load_complex_halves(left_addr, complex_typ);
        let (rre, rim, _, _) = self.load_complex_halves(right_addr, complex_typ);

        let ops = self.complex_half_ops(base_typ);
        let (half_op, combine) = if op == BinaryOp::Eq {
            (ops.eq, Opcode::And)
        } else {
            (ops.ne, Opcode::Or)
        };

        let real_cmp = self.alloc_pseudo();
        self.emit(Instruction::binop(
            half_op, real_cmp, lre, rre, base_typ, base_bits,
        ));
        let imag_cmp = self.alloc_pseudo();
        self.emit(Instruction::binop(
            half_op, imag_cmp, lim, rim, base_typ, base_bits,
        ));

        let result = self.alloc_pseudo();
        let int_typ = self.types.int_id;
        let int_bits = self.types.size_bits(int_typ);
        self.emit(Instruction::binop(
            combine, result, real_cmp, imag_cmp, int_typ, int_bits,
        ));
        result
    }

    /// `expr` converted to `_Bool`, when the source is complex.
    ///
    /// `None` when this does not apply, so a caller can fall through to its
    /// ordinary conversion. Handed the *expression* rather than a value on
    /// purpose: a complex operand is sometimes materialized as its two halves
    /// and sometimes as a pointer to them, and only re-addressing the
    /// expression tells the two apart. The backends have no 128-bit compare
    /// either, so the ordinary path silently narrowed to the real half.
    pub(crate) fn complex_to_bool(&mut self, expr: &Expr, target_typ: TypeId) -> Option<PseudoId> {
        let src_typ = self.expr_type(expr);
        if self.types.is_complex(src_typ) && self.types.kind(target_typ) == TypeKind::Bool {
            return Some(self.emit_complex_nonzero(expr));
        }
        None
    }

    /// Turn `expr` into the 0/1 truth value a branch or logical operator wants.
    ///
    /// The single place that answers "is this nonzero", so the answer cannot
    /// differ between `if`, `while`, `&&`, `!` and a `_Bool` conversion. It
    /// used to: statement conditions handed the raw value to `cbr` with no
    /// comparison at all, which tests a bit pattern, while `emit_compare_zero`
    /// compared against an *integer* zero whatever the operand's type. Both
    /// made `-0.0` true, and both read a complex value as its real half.
    pub(crate) fn linearize_condition(&mut self, cond: &Expr) -> PseudoId {
        let typ = self.expr_type(cond);
        if self.types.is_complex(typ) {
            return self.emit_complex_nonzero(cond);
        }
        let val = self.linearize_expr(cond);
        self.emit_compare_zero(val, typ)
    }

    /// `val != 0`, read the way `operand_typ` says to read it.
    pub(crate) fn emit_compare_zero(&mut self, val: PseudoId, operand_typ: TypeId) -> PseudoId {
        let result = self.alloc_pseudo();
        let size = self.types.size_bits(operand_typ);
        // A float is compared against 0.0, not against its bit pattern:
        // -0.0 is zero and every NaN is not.
        if self.types.is_float(operand_typ) {
            let zero = self.emit_fconst(FloatVal::ZERO, operand_typ);
            self.emit(Instruction::binop(
                Opcode::FCmpONe,
                result,
                val,
                zero,
                operand_typ,
                size,
            ));
            return result;
        }
        let zero = self.emit_const(0, operand_typ);
        self.emit(Instruction::binop(
            Opcode::SetNe,
            result,
            val,
            zero,
            operand_typ,
            size,
        ));
        result
    }

    /// Emit short-circuit logical AND: a && b
    /// If a is false, skip evaluation of b and return 0.
    /// Otherwise, evaluate b and return (b != 0).
    pub(crate) fn emit_logical_and(&mut self, left: &Expr, right: &Expr) -> PseudoId {
        let result_typ = self.types.int_id;

        // Create basic blocks
        let eval_b_bb = self.alloc_bb();
        let merge_bb = self.alloc_bb();

        // Evaluate LHS
        let left_bool = self.linearize_condition(left);

        // Emit the short-circuit value (0) BEFORE the branch, while still in LHS block
        // This value will be used if we short-circuit (LHS is false)
        let zero = self.emit_const(0, result_typ);

        // Get the block where LHS evaluation ended (may differ from initial block
        // if LHS contains nested control flow)
        let lhs_end_bb = self.current_bb.unwrap();

        // Branch: if LHS is false, go to merge (result = 0); else evaluate RHS
        self.emit(Instruction::cbr(left_bool, eval_b_bb, merge_bb));
        self.link_bb(lhs_end_bb, eval_b_bb);
        self.link_bb(lhs_end_bb, merge_bb);

        // eval_b_bb: Evaluate RHS
        self.switch_bb(eval_b_bb);
        let right_bool = self.linearize_condition(right);

        // Get the actual block where RHS evaluation ended (may differ from eval_b_bb
        // if RHS contains nested control flow like another &&/||)
        let rhs_end_bb = self.current_bb.unwrap();

        // Branch to merge
        self.emit(Instruction::br(merge_bb));
        self.link_bb(rhs_end_bb, merge_bb);

        // merge_bb: Create phi node to merge results
        self.switch_bb(merge_bb);

        // Result is 0 if we came from lhs_end_bb (LHS was false),
        // or right_bool if we came from rhs_end_bb (LHS was true)
        let result = self.alloc_pseudo();
        // Create a phi pseudo for the target - important for register allocation
        let phi_pseudo = Pseudo::phi(result, result.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(phi_pseudo);
        }
        let mut phi_insn = Instruction::phi(result, result_typ, 32);
        let phisrc1 = self.emit_phi_source(lhs_end_bb, zero, result, merge_bb, result_typ, 32);
        phi_insn.phi_list.push((lhs_end_bb, phisrc1));
        let phisrc2 =
            self.emit_phi_source(rhs_end_bb, right_bool, result, merge_bb, result_typ, 32);
        phi_insn.phi_list.push((rhs_end_bb, phisrc2));
        self.emit(phi_insn);

        result
    }

    /// Emit short-circuit logical OR: a || b
    /// If a is true, skip evaluation of b and return 1.
    /// Otherwise, evaluate b and return (b != 0).
    pub(crate) fn emit_logical_or(&mut self, left: &Expr, right: &Expr) -> PseudoId {
        let result_typ = self.types.int_id;

        // Create basic blocks
        let eval_b_bb = self.alloc_bb();
        let merge_bb = self.alloc_bb();

        // Evaluate LHS
        let left_bool = self.linearize_condition(left);

        // Emit the short-circuit value (1) BEFORE the branch, while still in LHS block
        // This value will be used if we short-circuit (LHS is true)
        let one = self.emit_const(1, result_typ);

        // Get the block where LHS evaluation ended (may differ from initial block
        // if LHS contains nested control flow)
        let lhs_end_bb = self.current_bb.unwrap();

        // Branch: if LHS is true, go to merge (result = 1); else evaluate RHS
        self.emit(Instruction::cbr(left_bool, merge_bb, eval_b_bb));
        self.link_bb(lhs_end_bb, merge_bb);
        self.link_bb(lhs_end_bb, eval_b_bb);

        // eval_b_bb: Evaluate RHS
        self.switch_bb(eval_b_bb);
        let right_bool = self.linearize_condition(right);

        // Get the actual block where RHS evaluation ended (may differ from eval_b_bb
        // if RHS contains nested control flow like another &&/||)
        let rhs_end_bb = self.current_bb.unwrap();

        // Branch to merge
        self.emit(Instruction::br(merge_bb));
        self.link_bb(rhs_end_bb, merge_bb);

        // merge_bb: Create phi node to merge results
        self.switch_bb(merge_bb);

        // Result is 1 if we came from lhs_end_bb (LHS was true),
        // or right_bool if we came from rhs_end_bb (LHS was false)
        let result = self.alloc_pseudo();
        // Create a phi pseudo for the target - important for register allocation
        let phi_pseudo = Pseudo::phi(result, result.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(phi_pseudo);
        }
        let mut phi_insn = Instruction::phi(result, result_typ, 32);
        let phisrc1 = self.emit_phi_source(lhs_end_bb, one, result, merge_bb, result_typ, 32);
        phi_insn.phi_list.push((lhs_end_bb, phisrc1));
        let phisrc2 =
            self.emit_phi_source(rhs_end_bb, right_bool, result, merge_bb, result_typ, 32);
        phi_insn.phi_list.push((rhs_end_bb, phisrc2));
        self.emit(phi_insn);

        result
    }

    /// The address this place names, or `None` when it is a bit-field.
    ///
    /// A bit-field has no address, so a consumer that can only work with one
    /// -- a memory-class `asm` operand, say -- has to know that.
    pub(crate) fn rmw_place_address(place: &RmwPlace) -> Option<PseudoId> {
        place.bitfield.is_none().then_some(place.base)
    }

    /// Resolve a read-modify-write target, evaluating its subexpressions once.
    ///
    /// `None` for a bare identifier: it has no subexpressions, so nothing can
    /// be evaluated twice, and the name-based paths handle the shapes that
    /// have no address at all -- a parameter living in `var_map`, and a static
    /// local behind its sentinel.
    pub(crate) fn resolve_rmw_place(&mut self, target: &Expr) -> Option<RmwPlace> {
        match &target.kind {
            ExprKind::Ident(_) => None,
            ExprKind::Member { expr, member } => {
                let base = self.linearize_lvalue(expr);
                let struct_type = {
                    let declared = self.expr_type(expr);
                    self.resolve_struct_type(declared)
                };
                Some(self.member_place(base, struct_type, *member, target))
            }
            ExprKind::Arrow { expr, member } => {
                // The pointer's *value* is the base address.
                let base = self.linearize_expr(expr);
                let struct_type = {
                    let ptr_type = self.expr_type(expr);
                    let declared = self
                        .types
                        .base_type(ptr_type)
                        .unwrap_or_else(|| self.expr_type(target));
                    self.resolve_struct_type(declared)
                };
                Some(self.member_place(base, struct_type, *member, target))
            }
            // Every other lvalue has one address and no bit-field placement.
            // `linearize_lvalue` is what evaluates the subexpressions, and it
            // does so once.
            _ => Some(RmwPlace {
                base: self.linearize_lvalue(target),
                bitfield: None,
            }),
        }
    }

    /// The place a struct or union member occupies, relative to `base`.
    fn member_place(
        &mut self,
        base: PseudoId,
        struct_type: TypeId,
        member: crate::strings::StringId,
        target: &Expr,
    ) -> RmwPlace {
        let target_typ = self.expr_type(target);
        let info = self
            .types
            .find_member(struct_type, member)
            .unwrap_or(MemberInfo {
                offset: 0,
                typ: target_typ,
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
            });
        let bitfield = match (info.bit_offset, info.bit_width, info.access_bytes) {
            (Some(bit_offset), Some(bit_width), Some(storage)) => {
                Some((info.offset, bit_offset, bit_width, storage, info.typ))
            }
            // Not a bit-field: fold the member offset into the base so the
            // load and the store share one address.
            _ => {
                let base = self.offset_address(base, info.offset as i64);
                return RmwPlace {
                    base,
                    bitfield: None,
                };
            }
        };
        RmwPlace { base, bitfield }
    }

    /// `base + offset` as an address, or `base` itself when the offset is zero.
    fn offset_address(&mut self, base: PseudoId, offset: i64) -> PseudoId {
        if offset == 0 {
            return base;
        }
        let delta = self.emit_const(offset as i128, self.types.long_id);
        let addr = self.alloc_reg_pseudo();
        self.emit(Instruction::binop(
            Opcode::Add,
            addr,
            base,
            delta,
            self.types.long_id,
            64,
        ));
        addr
    }

    /// The target's current value, read through an already-resolved place.
    pub(crate) fn load_rmw_place(&mut self, place: &RmwPlace, typ: TypeId) -> PseudoId {
        if let Some((offset, bit_offset, bit_width, storage, field_typ)) = place.bitfield {
            return self.emit_bitfield_load(
                place.base, offset, bit_offset, bit_width, storage, field_typ,
            );
        }
        let size = self.types.size_bits(typ);
        let val = self.alloc_reg_pseudo();
        self.emit(Instruction::load(val, place.base, 0, typ, size));
        val
    }

    /// Store back through the same place.
    ///
    /// Answers the bit-field's width and type when the store went through one,
    /// so the caller can reduce the expression's own value to what the field
    /// now holds (C17 6.5.16.1p2).
    pub(crate) fn store_rmw_place(
        &mut self,
        place: &RmwPlace,
        val: PseudoId,
        typ: TypeId,
    ) -> Option<(u32, TypeId)> {
        if let Some((offset, bit_offset, bit_width, storage, field_typ)) = place.bitfield {
            self.emit_bitfield_store(place.base, offset, bit_offset, bit_width, storage, val);
            return Some((bit_width, field_typ));
        }
        let size = self.types.size_bits(typ);
        self.emit(Instruction::store(val, place.base, 0, typ, size));
        None
    }

    pub(crate) fn emit_assign(&mut self, op: AssignOp, target: &Expr, value: &Expr) -> PseudoId {
        let target_typ = self.expr_type(target);
        let value_typ = self.expr_type(value);

        // An assignment to an `_Atomic` object is an atomic store, and a
        // compound assignment is a single atomic read-modify-write
        // (C17 6.5.16.2p3) -- not the load/compute/store this would otherwise
        // emit. Branch before the complex and struct early-returns below so
        // those shapes reach atomic_lvalue's diagnostic rather than silently
        // block-copying.
        if let Some(result) = self.try_emit_atomic_assign(op, target, value) {
            return result;
        }

        // A compound assignment on a complex object is `t = t op v`
        // (C17 6.5.16.2p3), and both sides travel by address. The scalar path
        // below loaded the target's *address* as though it were the number, so
        // `z += 1.0` computed on a pointer bit pattern and stored the result
        // over the object -- the program then died reading it back.
        if self.types.is_complex(target_typ) && op != AssignOp::Assign {
            let binop = match op {
                AssignOp::AddAssign => Some(BinaryOp::Add),
                AssignOp::SubAssign => Some(BinaryOp::Sub),
                AssignOp::MulAssign => Some(BinaryOp::Mul),
                AssignOp::DivAssign => Some(BinaryOp::Div),
                // Every other compound operator is a constraint violation on a
                // complex operand; leave those to the path that reports it.
                _ => None,
            };

            if let Some(binop) = binop {
                let target_addr = self.linearize_lvalue(target);
                let value_addr = if self.types.is_complex(value_typ) {
                    self.complex_operand_at_precision(value, target_typ)
                } else {
                    self.promote_real_to_complex(value, target_typ)
                };
                let result_addr =
                    self.emit_complex_binary(binop, target_addr, value_addr, target_typ);

                let base_typ = self.types.complex_base(target_typ);
                let base_bits = self.types.size_bits(base_typ);
                let base_bytes = (base_bits / 8) as i64;
                for offset in [0, base_bytes] {
                    let part = self.alloc_pseudo();
                    self.emit(Instruction::load(
                        part,
                        result_addr,
                        offset,
                        base_typ,
                        base_bits,
                    ));
                    self.emit(Instruction::store(
                        part,
                        target_addr,
                        offset,
                        base_typ,
                        base_bits,
                    ));
                }
                return target_addr;
            }
        }

        // For complex type assignment, handle specially - copy real and imag parts
        if self.types.is_complex(target_typ) && op == AssignOp::Assign {
            let target_addr = self.linearize_lvalue(target);

            // Assigning a *real* to a complex is a conversion, not a copy:
            // C99 6.3.1.7 gives the result that value as its real part and a
            // zero imaginary part. Taking the address of the right-hand side
            // here treated a non-lvalue as one -- `dc = 1.0` produced
            // `movabsq $4607182418800017408, %r11` (the bit pattern of 1.0)
            // followed by a dereference of it, so any such assignment
            // segfaulted, for locals and globals alike.
            if !self.types.is_complex(value_typ) {
                let dst_base = self.types.complex_base(target_typ);
                let dst_size = self.types.size_bits(dst_base);
                let dst_stride = (dst_size / 8) as i64;

                let real = self.linearize_expr(value);
                let real = self.emit_convert(real, value_typ, dst_base);
                self.emit(Instruction::store(real, target_addr, 0, dst_base, dst_size));

                let zero = if self.types.is_float(dst_base) {
                    self.emit_fconst(FloatVal::ZERO, dst_base)
                } else {
                    self.emit_const(0, dst_base)
                };
                self.emit(Instruction::store(
                    zero,
                    target_addr,
                    dst_stride,
                    dst_base,
                    dst_size,
                ));
                return target_addr;
            }

            let value_addr = self.complex_operand_addr(value);

            // The two sides may have *different* base precisions — assigning a
            // `double _Complex` to a `long double _Complex` is an ordinary
            // conversion. Reading the source with the target's base type and
            // stride, as this did, loaded 16 bytes from an 8-byte real part
            // and then read 16 bytes past the end of the source object.
            let dst_base = self.types.complex_base(target_typ);
            let dst_size = self.types.size_bits(dst_base);
            let dst_stride = (dst_size / 8) as i64;

            let src_base = if self.types.is_complex(value_typ) {
                self.types.complex_base(value_typ)
            } else {
                dst_base
            };
            let src_size = self.types.size_bits(src_base);
            let src_stride = (src_size / 8) as i64;

            // Real part
            let real = self.alloc_pseudo();
            self.emit(Instruction::load(real, value_addr, 0, src_base, src_size));
            let real = self.emit_convert(real, src_base, dst_base);
            self.emit(Instruction::store(real, target_addr, 0, dst_base, dst_size));

            // Imaginary part
            let imag = self.alloc_pseudo();
            self.emit(Instruction::load(
                imag, value_addr, src_stride, src_base, src_size,
            ));
            let imag = self.emit_convert(imag, src_base, dst_base);
            self.emit(Instruction::store(
                imag,
                target_addr,
                dst_stride,
                dst_base,
                dst_size,
            ));

            // The value of the assignment is the object assigned to, and a
            // complex object travels by *address* -- as the real-to-complex
            // branch above already returns. Handing back the real part's value
            // gave every consumer a `double` where a pointer belongs, so
            // `c = a` was fine as a statement and `if (c = a)` segfaulted.
            return target_addr;
        }

        // For struct/union assignment, do a block copy via addresses.
        // Structs are not loaded into registers by linearize_expr — they return
        // an address. So we must handle ALL struct sizes here, not just large ones.
        let target_kind = self.types.kind(target_typ);
        let target_size = self.types.size_bits(target_typ);
        if (target_kind == TypeKind::Struct || target_kind == TypeKind::Union)
            && target_size > 0
            && op == AssignOp::Assign
        {
            let target_addr = self.linearize_lvalue(target);
            let value_addr = self.linearize_lvalue(value);
            let target_size_bytes = target_size / 8;

            self.emit_block_copy(target_addr, value_addr, target_size_bytes as i64);

            // Return the target address as the result
            return target_addr;
        }

        // A complex right-hand side assigned to a `_Bool` needs the
        // expression, not its value -- see `complex_to_bool`. Already the
        // target type when it fires, so the conversion below is skipped.
        let bool_rhs = match op {
            AssignOp::Assign => self.complex_to_bool(value, target_typ),
            _ => None,
        };
        let rhs = match bool_rhs {
            Some(b) => b,
            None => self.linearize_expr(value),
        };

        // Check for pointer compound assignment (p += n or p -= n)
        let is_ptr_arith = self.types.kind(target_typ) == TypeKind::Pointer
            && self.types.is_integer(value_typ)
            && (op == AssignOp::AddAssign || op == AssignOp::SubAssign);

        // Convert RHS to target type if needed (but not for pointer arithmetic)
        let rhs = if is_ptr_arith {
            // For pointer arithmetic, scale the integer by element size
            let scale = self.pointer_step_bytes(target, target_typ);

            // Extend the integer to 64-bit for proper arithmetic
            let rhs_extended = self.emit_convert(rhs, value_typ, self.types.long_id);

            let scaled = self.alloc_reg_pseudo();
            self.emit(Instruction::binop(
                Opcode::Mul,
                scaled,
                rhs_extended,
                scale,
                self.types.long_id,
                64,
            ));
            scaled
        } else if bool_rhs.is_some() || op != AssignOp::Assign {
            // A compound assignment leaves its right operand alone here. It is
            // converted to the *common* type below, not down to the target's:
            // narrowing `-5` to `unsigned char` first made `x /= y` divide
            // 50 by 251 and store 0, where C17 6.5.16.2p3 computes `50 / -5`
            // at `int` and stores `(unsigned char)-10`.
            rhs
        } else {
            self.emit_convert(rhs, value_typ, target_typ)
        };

        // The target is resolved exactly once (C17 6.5.16.2p3) and the same
        // place serves the load below and the store further down. Reading the
        // old value from the *expression* and then re-deriving the address ran
        // every subexpression of the target twice.
        //
        // A plain assignment resolves its target here too. It never had the
        // double-evaluation bug -- there is no load to pair with the store --
        // but it must share the one store path, and C17 6.5.16p3 leaves the
        // order of the two operands unsequenced, so computing the address
        // after the value is allowed.
        let place = self.resolve_rmw_place(target);

        let final_val = match op {
            AssignOp::Assign => rhs,
            _ => {
                // Compound assignment - get current value and apply operation
                let lhs = match &place {
                    Some(p) => self.load_rmw_place(p, target_typ),
                    None => self.linearize_expr(target),
                };
                let result = self.alloc_reg_pseudo();

                // `E1 op= E2` is `E1 = E1 op E2` (C17 6.5.16.2p3), so the
                // operation runs at the operands' common type after the
                // integer promotions -- not at the target's type, which is
                // only what the *result* converts back to.
                //
                // The shifts are the exception: 6.5.7p3 gives the result the
                // promoted *left* operand's type, and promotes the right one
                // on its own.
                let arith_type = if is_ptr_arith {
                    // Pointer arithmetic already scaled the index; the add
                    // happens at pointer width.
                    self.types.long_id
                } else if matches!(op, AssignOp::ShlAssign | AssignOp::ShrAssign) {
                    self.types.integer_promote(target_typ)
                } else {
                    self.types.common_type(target_typ, value_typ)
                };

                let is_float = self.types.is_float(arith_type);
                let is_unsigned = self.types.is_unsigned(arith_type);
                let opcode = match op {
                    AssignOp::AddAssign => {
                        if is_float {
                            Opcode::FAdd
                        } else {
                            Opcode::Add
                        }
                    }
                    AssignOp::SubAssign => {
                        if is_float {
                            Opcode::FSub
                        } else {
                            Opcode::Sub
                        }
                    }
                    AssignOp::MulAssign => {
                        if is_float {
                            Opcode::FMul
                        } else {
                            Opcode::Mul
                        }
                    }
                    AssignOp::DivAssign => {
                        if is_float {
                            Opcode::FDiv
                        } else if is_unsigned {
                            Opcode::DivU
                        } else {
                            Opcode::DivS
                        }
                    }
                    AssignOp::ModAssign => {
                        // Modulo not supported for floats
                        if is_unsigned {
                            Opcode::ModU
                        } else {
                            Opcode::ModS
                        }
                    }
                    AssignOp::AndAssign => Opcode::And,
                    AssignOp::OrAssign => Opcode::Or,
                    AssignOp::XorAssign => Opcode::Xor,
                    AssignOp::ShlAssign => Opcode::Shl,
                    AssignOp::ShrAssign => {
                        if is_unsigned {
                            Opcode::Lsr
                        } else {
                            Opcode::Asr
                        }
                    }
                    AssignOp::Assign => unreachable!(),
                };

                let arith_size = self.types.size_bits(arith_type);
                // Both operands into the arithmetic type. The left one is the
                // object's current value, read at the target's type; the right
                // one is whatever it was written as.
                let lhs = if is_ptr_arith {
                    lhs
                } else {
                    self.emit_convert(lhs, target_typ, arith_type)
                };
                let rhs = if is_ptr_arith {
                    rhs
                } else if matches!(op, AssignOp::ShlAssign | AssignOp::ShrAssign) {
                    // The shift count is promoted on its own and is not
                    // brought to the left operand's type.
                    self.emit_convert(rhs, value_typ, self.types.integer_promote(value_typ))
                } else {
                    self.emit_convert(rhs, value_typ, arith_type)
                };
                self.emit(Instruction::binop(
                    opcode, result, lhs, rhs, arith_type, arith_size,
                ));
                // And the result back, which is the conversion that makes
                // `(x /= y)` yield what `x` now holds.
                if is_ptr_arith {
                    result
                } else {
                    self.emit_convert(result, arith_type, target_typ)
                }
            }
        };

        // Store based on target expression type
        let target_size = self.types.size_bits(target_typ);
        if let Some(p) = &place {
            // The address the load came from, so no subexpression runs twice.
            let narrowed = self.store_rmw_place(p, final_val, target_typ);
            return match narrowed {
                Some((bit_width, typ)) => self.narrow_to_bitfield(final_val, bit_width, typ),
                None => final_val,
            };
        }
        // Only a bare identifier reaches here: `resolve_rmw_place`
        // answers `Some` for every other lvalue and the branch above
        // stores through it. The arms that used to be here re-derived
        // an address that had already been computed, which is exactly
        // what ran the target a second time.
        if let ExprKind::Ident(symbol_id) = &target.kind {
            let name_str = self.symbol_name(*symbol_id);
            if let Some(local) = self.locals.get(symbol_id).cloned() {
                // Check if this is a static local (sentinel value)
                if local.sym.0 == u32::MAX {
                    self.emit_static_local_store(&name_str, final_val, target_typ, target_size);
                } else {
                    // Regular local variable: emit Store
                    self.emit(Instruction::store(
                        final_val,
                        local.sym,
                        0,
                        target_typ,
                        target_size,
                    ));
                }
            } else if self.var_map.contains_key(&name_str) {
                // Parameter: this is not SSA-correct but parameters
                // shouldn't be reassigned. If they are, we'd need to
                // demote them to locals. For now, just update the mapping.
                self.var_map.insert(name_str.clone(), final_val);
            } else {
                // Global variable - emit store
                let sym_id = self.alloc_pseudo();
                let pseudo = Pseudo::sym(sym_id, name_str);
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(pseudo);
                }
                self.emit(Instruction::store(
                    final_val,
                    sym_id,
                    0,
                    target_typ,
                    target_size,
                ));
            }
        }

        // A bare identifier is never a bit-field, so there is nothing to
        // reduce: the bit-field answer comes from `store_rmw_place` above.
        final_val
    }
}
