//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The padding of a type, for `__builtin_clear_padding`
//
// An object's bytes are its members' bits and padding. gcc's builtin zeroes
// the padding and leaves the members alone, so what it needs from a type is
// a map of which bits are a member's -- a mask per byte, bit set where a
// member lives. The rules are gcc 13's (`clear_padding_type`):
//
// - a structure's padding is every bit no member covers: the gaps between
//   members, the tail, and the unused bits around its bit-fields; an unnamed
//   bit-field is padding;
// - a union's padding is the bits that are padding in *every* member. A
//   bit-field directly in a union covers whole bytes, as many as its width
//   needs, since gcc measures it by its declaration's byte size;
// - x87 `long double` holds 10 bytes of value in 16, so 6 are padding, in
//   each half of a complex one too;
// - an array's padding is its elements'; a zero-length array has none;
// - a flexible array member has no defined padding at all, and gcc rejects
//   the call.
//

use super::linearize::{Linearizer, VmDim};
use super::{BasicBlockId, Instruction, Opcode, PseudoId};
use crate::float::FpFormat;
use crate::parse::ast::Expr;
use crate::strings::StringId;
use crate::types::{TypeId, TypeKind, TypeTable};

/// Which bits of each byte of an object of type `typ` belong to a member:
/// one mask per byte, a bit set where some member keeps a value. Every other
/// bit is padding.
///
/// `Err` names the flexible array member that leaves the padding undefined.
pub(crate) fn value_bits(types: &TypeTable, typ: TypeId) -> Result<Vec<u8>, StringId> {
    let mut bits = vec![0u8; types.size_bytes(typ)];
    mark(types, typ, 0, &mut bits)?;
    Ok(bits)
}

/// Mark the value bits of an object of type `typ` at byte `at` of `bits`.
fn mark(types: &TypeTable, typ: TypeId, at: usize, bits: &mut [u8]) -> Result<(), StringId> {
    match types.kind(typ) {
        TypeKind::Struct | TypeKind::Union => mark_members(types, typ, at, bits),
        TypeKind::Array => mark_array(types, typ, at, bits),
        // Ahead of the real kinds: a complex type has its halves' kind.
        _ if types.is_complex(typ) => {
            let half = types.complex_base(typ);
            let step = types.size_bytes(half);
            mark(types, half, at, bits)?;
            mark(types, half, at + step, bits)
        }
        TypeKind::LongDouble if types.fp_format(typ) == Some(FpFormat::X87Extended) => {
            set_bytes(bits, at, X87_VALUE_BYTES);
            Ok(())
        }
        _ => {
            set_bytes(bits, at, types.size_bytes(typ));
            Ok(())
        }
    }
}

/// The bytes of an x87 extended value: a 64-bit significand, then the sign
/// and the 15-bit exponent.
const X87_VALUE_BYTES: usize = 10;

/// A structure's or a union's members. In a union every member starts at 0,
/// and a bit is a member's if any member's -- which is padding only where
/// every member has padding.
fn mark_members(
    types: &TypeTable,
    typ: TypeId,
    at: usize,
    bits: &mut [u8],
) -> Result<(), StringId> {
    let Some(composite) = types.composite(typ) else {
        return Ok(());
    };
    let is_union = types.kind(typ) == TypeKind::Union;
    for m in &composite.members {
        if let Some(width) = m.bit_width {
            // Unnamed bit-fields only lay out the others.
            if m.name == StringId::EMPTY || width == 0 {
                continue;
            }
            let bit_offset = m.bit_offset.unwrap_or(0) as usize;
            if is_union {
                set_bytes(
                    bits,
                    at + m.offset,
                    (bit_offset + width as usize).div_ceil(8),
                );
            } else {
                set_bits(bits, (at + m.offset) * 8 + bit_offset, width as usize);
            }
        } else if types.is_flexible_array_member(m) {
            return Err(m.name);
        } else {
            mark(types, m.typ, at + m.offset, bits)?;
        }
    }
    Ok(())
}

/// Each element of an array in turn; an array without a bound has none.
fn mark_array(types: &TypeTable, typ: TypeId, at: usize, bits: &mut [u8]) -> Result<(), StringId> {
    let (Some(count), Some(elem)) = (types.array_size(typ), types.base_type(typ)) else {
        return Ok(());
    };
    let size = types.size_bytes(elem);
    if count == 0 || size == 0 {
        return Ok(());
    }
    // One element's map, copied: the elements are identical.
    let mut one = vec![0u8; size];
    mark(types, elem, 0, &mut one)?;
    for i in 0..count {
        let start = at + i * size;
        for (dst, &src) in bits[start..start + size].iter_mut().zip(&one) {
            *dst |= src;
        }
    }
    Ok(())
}

/// Mark `len` whole bytes from `at`, as far as the object goes.
fn set_bytes(bits: &mut [u8], at: usize, len: usize) {
    let end = (at + len).min(bits.len());
    if at < end {
        bits[at..end].fill(0xff);
    }
}

/// Mark `width` bits from bit `start`, counting from the least significant
/// bit of byte 0 -- every target is little-endian.
fn set_bits(bits: &mut [u8], start: usize, width: usize) {
    for bit in start..start + width {
        if let Some(byte) = bits.get_mut(bit / 8) {
            *byte |= 1 << (bit % 8);
        }
    }
}

/// What clearing a map's padding takes, in byte order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum PaddingOp {
    /// `len` bytes of nothing but padding, from byte `at`: stored as zero.
    Zero { at: usize, len: usize },
    /// One byte partly padding: and-ed with the member bits `keep`.
    Mask { at: usize, keep: u8 },
}

/// The stores and masks that clear the padding a [`value_bits`] map shows.
pub(crate) fn padding_ops(bits: &[u8]) -> Vec<PaddingOp> {
    let mut ops = Vec::new();
    let mut i = 0;
    while i < bits.len() {
        match bits[i] {
            0xff => i += 1,
            0 => {
                let start = i;
                while i < bits.len() && bits[i] == 0 {
                    i += 1;
                }
                ops.push(PaddingOp::Zero {
                    at: start,
                    len: i - start,
                });
            }
            keep => {
                ops.push(PaddingOp::Mask { at: i, keep });
                i += 1;
            }
        }
    }
    ops
}

impl Linearizer<'_> {
    /// `__builtin_clear_padding(ptr)`: zero the padding of the `pointee`
    /// object `ptr` points at, in place.
    ///
    /// A whole byte of padding is stored as zero, a run of them through
    /// [`Linearizer::emit_block_zero`]; a byte that is partly a bit-field's
    /// is loaded, and-ed with the bits to keep and stored back. A variable
    /// length array is its innermost fixed-size element's padding, cleared
    /// element by element in a loop. A pointer to a function, or an object
    /// with no padding, needs nothing; the pointer is still evaluated.
    pub(crate) fn linearize_clear_padding(&mut self, ptr: &Expr, pointee: TypeId) {
        let base = self.linearize_expr(ptr);
        if self.types.kind(pointee) == TypeKind::Function {
            return;
        }
        let volatile = self.types.contains_volatile(pointee);
        if self.types.unsized_array_levels(pointee) == 0 {
            // A flexible array member was reported by the parser.
            if let Ok(bits) = value_bits(self.types, pointee) {
                self.emit_padding_ops(base, &padding_ops(&bits), volatile);
            }
            return;
        }
        let Some((dims, elem)) = self.clear_padding_extents(ptr) else {
            return;
        };
        let Ok(bits) = value_bits(self.types, elem) else {
            return;
        };
        let ops = padding_ops(&bits);
        if ops.is_empty() || dims.iter().any(|d| matches!(d, VmDim::Const(0))) {
            return;
        }
        let Some(count) = self.vm_extent_product(&dims) else {
            return;
        };
        self.emit_padding_loop(base, count, bits.len(), &ops, volatile);
    }

    /// The extents of the variable length array `ptr` points at, outermost
    /// first, with its innermost element type. An array argument points at
    /// its first element, one level in from the array itself.
    fn clear_padding_extents(&self, ptr: &Expr) -> Option<(Vec<VmDim>, TypeId)> {
        let (dims, elem) = self.vm_type_extents(ptr)?;
        let is_array = ptr
            .typ
            .is_some_and(|t| self.types.kind(t) == TypeKind::Array);
        let dims = if is_array {
            dims.get(1..)?.to_vec()
        } else {
            dims
        };
        Some((dims, elem))
    }

    /// Clear `ops` in each of `count` consecutive elements of `elem_size`
    /// bytes from `base`:
    ///
    /// ```text
    ///     cur = base; end = base + count * elem_size
    /// cond: if (cur < end) goto body; else goto exit
    /// body: <ops at cur>; cur += elem_size; goto cond
    /// exit:
    /// ```
    fn emit_padding_loop(
        &mut self,
        base: PseudoId,
        count: PseudoId,
        elem_size: usize,
        ops: &[PaddingOp],
        volatile: bool,
    ) {
        let ulong = self.types.ulong_id;
        let ptr_type = self.types.char_ptr_id;
        let size = self.emit_const(elem_size as i128, ulong);
        let bytes = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Mul,
            bytes,
            count,
            size,
            ulong,
            64,
        ));
        let end = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Add,
            end,
            base,
            bytes,
            ptr_type,
            64,
        ));
        let cursor = self.frame_temp("__clear_padding", ptr_type);
        self.emit(Instruction::store(base, cursor, 0, ptr_type, 64));

        let (cond_bb, body_bb, exit_bb) = (self.alloc_bb(), self.alloc_bb(), self.alloc_bb());
        self.branch_to(cond_bb);
        self.switch_bb(cond_bb);
        let cur = self.alloc_pseudo();
        self.emit(Instruction::load(cur, cursor, 0, ptr_type, 64));
        let more = self.alloc_pseudo();
        self.emit(Instruction::compare(
            Opcode::SetB,
            more,
            (cur, end),
            (ptr_type, 64),
            (self.types.int_id, 32),
        ));
        self.emit(Instruction::cbr(more, body_bb, exit_bb));
        self.link_bb(cond_bb, body_bb);
        self.link_bb(cond_bb, exit_bb);

        self.switch_bb(body_bb);
        let cur = self.alloc_pseudo();
        self.emit(Instruction::load(cur, cursor, 0, ptr_type, 64));
        self.emit_padding_ops(cur, ops, volatile);
        let next = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Add,
            next,
            cur,
            size,
            ptr_type,
            64,
        ));
        self.emit(Instruction::store(next, cursor, 0, ptr_type, 64));
        self.branch_to(cond_bb);
        self.switch_bb(exit_bb);
    }

    /// End the current block with a branch to `target`.
    fn branch_to(&mut self, target: BasicBlockId) {
        if let Some(current) = self.current_bb {
            self.emit(Instruction::br(target));
            self.link_bb(current, target);
        }
    }

    /// The stores and masks of `ops`, at the object `base` points at.
    fn emit_padding_ops(&mut self, base: PseudoId, ops: &[PaddingOp], volatile: bool) {
        let uchar = self.types.uchar_id;
        for op in ops {
            match *op {
                PaddingOp::Zero { at, len } => {
                    self.emit_block_zero(base, at as i64, len as i64, volatile);
                }
                PaddingOp::Mask { at, keep } => {
                    let at = at as i64;
                    let byte = self.alloc_pseudo();
                    self.emit(Instruction::load(byte, base, at, uchar, 8).with_volatile(volatile));
                    let mask = self.emit_const(i128::from(keep), uchar);
                    let kept = self.alloc_pseudo();
                    self.emit(Instruction::binop(Opcode::And, kept, byte, mask, uchar, 8));
                    self.emit(Instruction::store(kept, base, at, uchar, 8).with_volatile(volatile));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::{Arch, Os, Target};
    use crate::types::{CompositeType, MemberAlign, StructMember, Type};

    fn field(typ: TypeId, offset: usize) -> StructMember {
        StructMember {
            name: StringId(1),
            typ,
            offset,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::default(),
        }
    }

    fn bitfield(typ: TypeId, offset: usize, bit_offset: u32, width: u32) -> StructMember {
        StructMember {
            bit_offset: Some(bit_offset),
            bit_width: Some(width),
            access_bytes: Some(4),
            ..field(typ, offset)
        }
    }

    fn composite(
        types: &mut TypeTable,
        kind: TypeKind,
        members: Vec<StructMember>,
        size: usize,
        align: usize,
    ) -> TypeId {
        let mut c = CompositeType::incomplete(None);
        c.members = members;
        c.size = size;
        c.align = align;
        c.member_align = align;
        c.is_complete = true;
        let mut t = Type::basic(kind);
        t.composite = Some(Box::new(c));
        types.intern(t)
    }

    #[test]
    fn struct_gaps_tail_and_bitfield_bits_are_padding() {
        let mut types = TypeTable::new(&Target::new(Arch::X86_64, Os::Linux));
        let (c, i, s, l) = (types.char_id, types.int_id, types.short_id, types.long_id);
        // struct { char c; int i; short s; long l; }
        let a = composite(
            &mut types,
            TypeKind::Struct,
            vec![field(c, 0), field(i, 4), field(s, 8), field(l, 16)],
            24,
            8,
        );
        let bits = value_bits(&types, a).unwrap();
        let mut want = vec![0u8; 24];
        want[0] = 0xff;
        want[4..10].fill(0xff);
        want[16..24].fill(0xff);
        assert_eq!(bits, want);
        assert_eq!(
            padding_ops(&bits),
            vec![
                PaddingOp::Zero { at: 1, len: 3 },
                PaddingOp::Zero { at: 10, len: 6 },
            ]
        );

        // struct { char a; int b:3; int :4; char d; }: the unnamed field is
        // padding, so byte 1 keeps three bits.
        let mut unnamed = bitfield(i, 0, 11, 4);
        unnamed.name = StringId::EMPTY;
        let b = composite(
            &mut types,
            TypeKind::Struct,
            vec![field(c, 0), bitfield(i, 0, 8, 3), unnamed, field(c, 2)],
            4,
            4,
        );
        assert_eq!(value_bits(&types, b).unwrap(), vec![0xff, 0x07, 0xff, 0]);
    }

    #[test]
    fn union_padding_is_what_every_member_leaves() {
        let mut types = TypeTable::new(&Target::new(Arch::Aarch64, Os::Linux));
        let (c, i) = (types.char_id, types.int_id);
        // struct { char a; int b; } and struct { char x; char p; int q; }
        let s1 = composite(
            &mut types,
            TypeKind::Struct,
            vec![field(c, 0), field(i, 4)],
            8,
            4,
        );
        let s2 = composite(
            &mut types,
            TypeKind::Struct,
            vec![field(c, 0), field(c, 1), field(i, 4)],
            8,
            4,
        );
        let u = composite(
            &mut types,
            TypeKind::Union,
            vec![field(s1, 0), field(s2, 0)],
            8,
            4,
        );
        assert_eq!(
            value_bits(&types, u).unwrap(),
            vec![0xff, 0xff, 0, 0, 0xff, 0xff, 0xff, 0xff]
        );
        // A bit-field directly in a union covers whole bytes: 12 bits, two.
        let v = composite(
            &mut types,
            TypeKind::Union,
            vec![bitfield(i, 0, 0, 12), field(c, 0)],
            4,
            4,
        );
        assert_eq!(value_bits(&types, v).unwrap(), vec![0xff, 0xff, 0, 0]);
    }

    #[test]
    fn long_double_padding_follows_the_format() {
        let x86 = TypeTable::new(&Target::new(Arch::X86_64, Os::Linux));
        let bits = value_bits(&x86, x86.longdouble_id).unwrap();
        assert_eq!(bits.iter().filter(|&&b| b == 0xff).count(), 10);
        assert_eq!(&bits[10..], &[0; 6]);
        let z = value_bits(&x86, x86.complex_longdouble_id).unwrap();
        assert_eq!(z.iter().filter(|&&b| b == 0).count(), 12);
        // binary128 on aarch64 is all value.
        let a64 = TypeTable::new(&Target::new(Arch::Aarch64, Os::Linux));
        assert!(value_bits(&a64, a64.longdouble_id)
            .unwrap()
            .iter()
            .all(|&b| b == 0xff));
    }

    #[test]
    fn arrays_repeat_their_elements_padding_and_a_flexible_member_is_refused() {
        let mut types = TypeTable::new(&Target::new(Arch::X86_64, Os::Linux));
        let (c, i) = (types.char_id, types.int_id);
        let s = composite(
            &mut types,
            TypeKind::Struct,
            vec![field(c, 0), field(i, 4)],
            8,
            4,
        );
        let arr = types.intern(Type::array(s, 2));
        assert_eq!(
            padding_ops(&value_bits(&types, arr).unwrap()),
            vec![
                PaddingOp::Zero { at: 1, len: 3 },
                PaddingOp::Zero { at: 9, len: 3 },
            ]
        );
        let empty = types.intern(Type::array(s, 0));
        assert!(value_bits(&types, empty).unwrap().is_empty());

        let flex = types.intern(Type {
            array_size: None,
            ..Type::array(c, 0)
        });
        let mut tail = field(flex, 5);
        tail.name = StringId(7);
        let f = composite(
            &mut types,
            TypeKind::Struct,
            vec![field(i, 0), field(c, 4), tail],
            8,
            4,
        );
        assert_eq!(value_bits(&types, f), Err(StringId(7)));
    }

    /// What a function's clear-padding code does to the object, read off
    /// the IR once `memexpand` has turned its block fills into stores.
    struct PaddingWrites {
        /// Each byte masked in place: `(offset, mask)`.
        masks: Vec<(i64, i128)>,
        /// Each zero store: `(offset, bytes)`, the offset taken through the
        /// address it is made at.
        zeros: Vec<(i64, u32)>,
    }

    fn padding_writes(src: &str, target: &Target) -> PaddingWrites {
        let (mut module, types) =
            super::super::linearize::test_linearize::linearize_source_with_types(src, target);
        let func = &mut module.functions[0];
        crate::ir::memexpand::run(func, &types);
        let insns: Vec<_> = func.blocks.iter().flat_map(|b| b.insns.clone()).collect();
        // The constant each address pseudo adds to the pointer, if any.
        let base_offset = |addr: PseudoId| -> i64 {
            insns
                .iter()
                .find(|i| i.op == Opcode::Add && i.target == Some(addr))
                .and_then(|i| i.src.iter().find_map(|&s| func.const_val(s)))
                .unwrap_or(0) as i64
        };
        let mut masks = Vec::new();
        let mut zeros = Vec::new();
        for insn in &insns {
            match insn.op {
                Opcode::And => {
                    let mask = insn.src.iter().find_map(|&s| func.const_val(s)).unwrap();
                    let store = insns
                        .iter()
                        .find(|i| i.op == Opcode::Store && i.src.get(1) == insn.target.as_ref())
                        .unwrap();
                    assert_eq!(store.size, 8, "a partial byte is stored as a byte");
                    masks.push((store.offset, mask));
                }
                Opcode::Store if func.const_val(insn.src[1]) == Some(0) => {
                    zeros.push((base_offset(insn.src[0]) + insn.offset, insn.size / 8));
                }
                _ => {}
            }
        }
        assert!(!insns.iter().any(|i| i.op == Opcode::Memset));
        zeros.sort();
        PaddingWrites { masks, zeros }
    }

    /// `struct B { char a; int b:3; short s; char t; }` is 8 bytes on both
    /// targets: byte 1 keeps `b`'s three bits, and bytes 5..8 are the tail.
    /// x87 `long double` adds six bytes of padding that binary128 does not
    /// have.
    #[test]
    fn clear_padding_masks_the_bitfield_byte_and_zeroes_the_tail() {
        let src = "struct B { char a; int b:3; short s; char t; };\n\
                   void f(struct B *p) { __builtin_clear_padding(p); }\n";
        for arch in [Arch::X86_64, Arch::Aarch64] {
            let target = Target::new(arch, Os::Linux);
            let PaddingWrites { masks, zeros } = padding_writes(src, &target);
            assert_eq!(masks, vec![(1, 0x07)], "{arch:?}");
            let tail: u32 = zeros.iter().map(|&(_, n)| n).sum();
            assert_eq!(tail, 3, "{arch:?}: {zeros:?}");
            assert!(zeros
                .iter()
                .all(|&(at, n)| at >= 5 && at + i64::from(n) <= 8));
        }

        let src = "struct L { char c; long double d; };\n\
                   void f(struct L *p) { __builtin_clear_padding(p); }\n";
        let x86 = padding_writes(src, &Target::new(Arch::X86_64, Os::Linux)).zeros;
        let a64 = padding_writes(src, &Target::new(Arch::Aarch64, Os::Linux)).zeros;
        let bytes = |z: &[(i64, u32)]| z.iter().map(|&(_, n)| n).sum::<u32>();
        assert_eq!(bytes(&x86), 15 + 6, "{x86:?}");
        assert_eq!(bytes(&a64), 15, "{a64:?}");
    }
}
