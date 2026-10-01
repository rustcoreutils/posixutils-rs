//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! Emit helpers for the linearizer (constants, block copies, bitfields, operators, assignments)

use super::linearize::BlockVolatility;
use super::memexpand;
use super::{BasicBlockId, CallAbiInfo, Instruction, Opcode, Pseudo, PseudoId};
use crate::abi::get_abi_for_conv;
use crate::constexpr::ConstScope;
use crate::diag::{error, Position};
use crate::float::FloatVal;
use crate::parse::ast::{AssignOp, BinaryOp, Expr, ExprKind, FpCompare, LibFn, MathErrno, UnaryOp};
use crate::strings::StringId;
use crate::types::Bitfield;
use crate::types::{MemberInfo, TypeId, TypeKind, TypeTable};

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
    /// The bit-field's placement and its type as the access reaches it.
    bitfield: Option<(Bitfield, TypeId)>,
}

/// Everything `E1 op= E2` needs beyond the two operand *values*: the operator
/// and the types the arithmetic is decided by.
///
/// C17 6.5.16.2p3 makes `E1 op= E2` mean `E1 = E1 op E2` bar evaluating `E1`
/// twice. So the arithmetic runs at the type the usual arithmetic conversions
/// give the two operands -- `target_typ` and `value_typ` -- and only the
/// *result* converts back to `target_typ`. Narrowing the right operand to the
/// target first is a different computation: `_Atomic unsigned char c = 50;
/// c /= -5;` becomes `50 / 251` and stores 0 where the standard stores
/// `(unsigned char)(50 / -5)`, 246.
///
/// This exists because the ordinary and the `_Atomic` lowerings each had their
/// own copy of these rules and the copies disagreed. Both now build one of
/// these and hand it to [`Linearizer::compound_assign_value`].
#[derive(Clone, Copy)]
pub(crate) struct CompoundAssign {
    /// The operator.
    pub(crate) op: AssignOp,
    /// `E1`'s type: what the left operand is read at and what the result
    /// converts back to.
    pub(crate) target_typ: TypeId,
    /// `E2`'s type **as written**, before any conversion. For pointer
    /// arithmetic it is the type of the already-scaled addend.
    pub(crate) value_typ: TypeId,
    /// `p += n`: the right operand arrives already scaled by the pointee size,
    /// the addition happens at pointer width, and the result is a pointer
    /// already -- so neither operand nor result is converted.
    pub(crate) is_ptr_arith: bool,
    /// Complement the arithmetic result *before* it converts back to
    /// `target_typ`. This is `nand`, which has no operator spelling of its
    /// own: `__atomic_fetch_nand` stores `~(old & value)`, and on a `_Bool`
    /// object that complement has to happen before the conversion to 0 or 1,
    /// not after it.
    pub(crate) invert: bool,
}

impl CompoundAssign {
    /// `E1 op= E2` with both operand types as written.
    pub(crate) fn new(op: AssignOp, target_typ: TypeId, value_typ: TypeId) -> Self {
        Self {
            op,
            target_typ,
            value_typ,
            is_ptr_arith: false,
            invert: false,
        }
    }
}

/// The type the arithmetic of `ca` is performed at.
///
/// The usual arithmetic conversions (C17 6.3.1.8) on the two operands, with
/// two exceptions:
///
/// * The shifts. C17 6.5.7p3 promotes each operand *separately* and gives the
///   result the promoted **left** operand's type, so the right operand has no
///   say: `_Atomic signed char s = -8; s >>= 1;` shifts -8 as an `int` and
///   stores -4, where computing at the target's width would shift the byte
///   pattern and store 124.
/// * Pointer arithmetic, whose addend the caller has already scaled to a
///   byte count; the addition happens at pointer width.
pub(crate) fn compound_assign_arith_type(types: &TypeTable, ca: &CompoundAssign) -> TypeId {
    if ca.is_ptr_arith {
        types.long_id
    } else if matches!(ca.op, AssignOp::ShlAssign | AssignOp::ShrAssign) {
        types.integer_promote(ca.target_typ)
    } else {
        types.common_type(ca.target_typ, ca.value_typ)
    }
}

/// The arithmetic opcode a compound assignment operator applies at `typ`.
///
/// `typ` is the type the operation is *performed* at -- the answer of
/// [`compound_assign_arith_type`] -- because that is what decides between the
/// integer and floating forms and between the signed and unsigned ones. Asking
/// the target's type instead makes `unsigned char x; x /= -5;` an unsigned
/// divide of a value the standard computes as a signed `int`.
pub(crate) fn compound_assign_opcode(types: &TypeTable, op: AssignOp, typ: TypeId) -> Opcode {
    let is_float = types.is_float(typ);
    let is_unsigned = types.is_unsigned(typ);
    match op {
        AssignOp::Assign => unreachable!("plain assignment has no arithmetic opcode"),
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
        // Modulo not supported for floats.
        AssignOp::ModAssign => {
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
    }
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

/// The floating comparison C's relational or equality operator `op` is.
pub(crate) fn float_comparison(op: BinaryOp) -> Option<Opcode> {
    Some(match op {
        BinaryOp::Lt => Opcode::FCmpOLt,
        BinaryOp::Gt => Opcode::FCmpOGt,
        BinaryOp::Le => Opcode::FCmpOLe,
        BinaryOp::Ge => Opcode::FCmpOGe,
        BinaryOp::Eq => Opcode::FCmpOEq,
        BinaryOp::Ne => Opcode::FCmpONe,
        _ => return None,
    })
}

/// The one floating comparison a member of the `isgreater` family is, or
/// `None` for the two that take more than one (`islessgreater`,
/// `isunordered`).
pub(crate) fn fp_compare_opcode(cmp: FpCompare) -> Option<Opcode> {
    Some(match cmp {
        FpCompare::Greater => Opcode::FCmpOGt,
        FpCompare::GreaterEqual => Opcode::FCmpOGe,
        FpCompare::Less => Opcode::FCmpOLt,
        FpCompare::LessEqual => Opcode::FCmpOLe,
        // C23 7.12.17.1 has `iseqsig` raise `FE_INVALID` for an unordered
        // pair, quiet NaN included -- the reverse of its siblings. The quiet
        // compare emitted for it does not raise it for a quiet NaN. The
        // *answer* is exact; only the exception flag differs -- the same gap
        // c17's `<` and `>` have, which use this compare too.
        FpCompare::Equal => Opcode::FCmpOEq,
        FpCompare::LessGreater | FpCompare::Unordered => return None,
    })
}

/// A controlling expression, evaluated for a branch.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Controlling {
    /// A constant expression, and whether it holds: the branch goes one way
    /// only, and the arm it never takes has no edge into it.
    Constant(bool),
    /// A value computed at run time, nonzero when the condition holds.
    Value(PseudoId),
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

    /// Zero a whole aggregate (struct, union or array), which is what C17
    /// 6.7.9p19 asks for before an initializer list is applied: every member
    /// the list does not reach is initialized as a static object would be.
    pub(crate) fn emit_aggregate_zero(&mut self, base_sym: PseudoId, typ: TypeId) {
        let total_bytes = self.types.size_bytes(typ) as i64;
        let volatile = self.types.contains_volatile(typ);
        self.emit_block_zero(base_sym, 0, total_bytes, volatile);
    }

    /// Which ends of a block move between objects of type `dst` and `src`
    /// reach something volatile -- the answer every chunk of the move is
    /// marked with. By `contains_volatile`, the rule `emit` applies to a
    /// single access: a struct with one `volatile` member is read and written
    /// whole, so every chunk of it is observable.
    pub(crate) fn block_volatility(&self, dst: TypeId, src: TypeId) -> BlockVolatility {
        BlockVolatility {
            dst: self.types.contains_volatile(dst),
            src: self.types.contains_volatile(src),
        }
    }

    /// Emit a fill of `size_bytes` zero bytes at `dst` + `dst_base_offset`.
    ///
    /// One `Opcode::Memset`, which `memexpand` turns into stores when the run
    /// is short and leaves as a call when it is not. So the bound on the
    /// unroll is `memexpand::INLINE_LIMIT_BYTES` -- the one every block memory
    /// operation in the compiler shares -- rather than another copy of the
    /// 8/4/2/1 descent with a cap of its own, which is what this was: a
    /// hand-rolled ladder with **no** upper bound at all, so
    /// `char buf[N] = {0}` emitted one store per chunk for any N. 8 KB cost
    /// 2081 instructions in the function body and 1 MB did not finish
    /// compiling in 25 minutes, while its sibling
    /// [`Self::emit_block_copy_at_offset`] had capped at the shared limit all
    /// along.
    ///
    /// `memexpand::run` runs at every optimization level, `-O0` included, so
    /// the expansion does not depend on optimizing -- the same reason the
    /// opcode a program's own `memset` becomes is expanded there rather than
    /// here.
    ///
    /// A fill of a `volatile` object is the exception. `memexpand` treats a
    /// `Memset` as the library call it names, which promised nothing about
    /// volatility, so its stores come out unmarked and a pass may then drop
    /// them. Within the inline limit such a fill is emitted here, as stores
    /// marked for what they write; past it, the call stays a call, which
    /// nothing deletes.
    pub(crate) fn emit_block_zero(
        &mut self,
        dst: PseudoId,
        dst_base_offset: i64,
        size_bytes: i64,
        volatile: bool,
    ) {
        if size_bytes <= 0 {
            return;
        }
        if volatile && size_bytes <= memexpand::INLINE_LIMIT_BYTES {
            for (offset, chunk) in memexpand::block_chunks(size_bytes) {
                let (typ, bits) = (chunk.typ(self.types), chunk.bits());
                let zero = self.emit_const(0, typ);
                self.emit(
                    Instruction::store(zero, dst, dst_base_offset + offset, typ, bits)
                        .with_volatile(true),
                );
            }
            return;
        }
        let dst_ptr = self.block_dest_addr(dst, dst_base_offset);
        let byte = self.emit_const(0, self.types.int_id);
        let n = self.emit_const(size_bytes as i128, self.types.ulong_id);
        let result = self.alloc_pseudo();
        self.emit(
            Instruction::new(Opcode::Memset)
                .with_func(self.library_function_name("memset"))
                .with_target(result)
                .with_src3(dst_ptr, byte, n)
                .with_type_and_size(self.types.void_ptr_id, 64),
        );
    }

    /// Emit a block copy from src to dst using integer chunks.
    pub(crate) fn emit_block_copy(
        &mut self,
        dst: PseudoId,
        src: PseudoId,
        size_bytes: i64,
        vol: BlockVolatility,
    ) {
        self.emit_block_copy_at_offset(dst, 0, src, size_bytes, vol);
    }

    /// Emit a block copy from src to dst using integer chunks.
    /// The destination stores start at dst_base_offset.
    ///
    /// Above `memexpand::INLINE_LIMIT_BYTES` the copy is a `memcpy` call
    /// instead, by the same rule and in the same chunks as a `memcpy` the
    /// program wrote.
    ///
    /// Each chunk is marked with `vol`: the chunks are integers, so the
    /// qualifier of the aggregate they move cannot be read back off their
    /// type, and an unmarked chunk load of a `volatile` struct nothing
    /// used was deleted outright from `-O1` up.
    pub(crate) fn emit_block_copy_at_offset(
        &mut self,
        dst: PseudoId,
        dst_base_offset: i64,
        src: PseudoId,
        size_bytes: i64,
        vol: BlockVolatility,
    ) {
        if size_bytes > memexpand::INLINE_LIMIT_BYTES {
            self.emit_block_copy_call(dst, dst_base_offset, src, size_bytes);
            return;
        }
        for (offset, chunk) in memexpand::block_chunks(size_bytes) {
            let (typ, bits) = (chunk.typ(self.types), chunk.bits());
            let tmp = self.alloc_pseudo();
            self.emit(Instruction::load(tmp, src, offset, typ, bits).with_volatile(vol.src));
            self.emit(
                Instruction::store(tmp, dst, dst_base_offset + offset, typ, bits)
                    .with_volatile(vol.dst),
            );
        }
    }

    /// The address `dst` + `dst_base_offset` names, as a block memory
    /// operation takes it.
    ///
    /// These opcodes take addresses. A `Sym` pseudo names a local's *storage*,
    /// not a pointer to it -- a `Store` can name it directly, a call cannot.
    /// Passing the Sym itself handed `memcpy` a meaningless value and
    /// segfaulted every copy over the threshold. `rvalue_addr` is the existing
    /// answer to this question and returns a non-Sym pseudo unchanged.
    ///
    /// `dst_base_offset` is folded into the pointer, since these take an
    /// address rather than a base and a displacement.
    fn block_dest_addr(&mut self, dst: PseudoId, dst_base_offset: i64) -> PseudoId {
        let void_ptr = self.types.void_ptr_id;
        let dst = self.rvalue_addr(dst, void_ptr);
        self.offset_address(dst, dst_base_offset)
    }

    /// The same copy as a `memcpy` call.
    fn emit_block_copy_call(
        &mut self,
        dst: PseudoId,
        dst_base_offset: i64,
        src: PseudoId,
        size_bytes: i64,
    ) {
        let dst_ptr = self.block_dest_addr(dst, dst_base_offset);
        let src = self.rvalue_addr(src, self.types.void_ptr_id);
        let n = self.emit_const(size_bytes as i128, self.types.ulong_id);
        let result = self.alloc_pseudo();
        self.emit(
            Instruction::new(Opcode::Memcpy)
                .with_func(self.library_function_name("memcpy"))
                .with_target(result)
                .with_src3(dst_ptr, src, n)
                .with_type_and_size(self.types.void_ptr_id, 64),
        );
    }

    /// Emit code to load a bitfield value
    /// Returns the loaded value as a PseudoId
    ///
    /// `typ` is the field's type as the access reaches it -- the declared type
    /// so-qualified by the object (C17 6.5.2.3p3) -- and the access itself is
    /// of the *carrier*, whose type is an unqualified storage unit. So nothing
    /// downstream can derive the qualifier from the instruction's own type, and
    /// the volatile marker is set here instead;
    /// [`Self::mark_volatile_access`] preserves a marker its caller set for
    /// exactly this case. Without it a volatile bit-field read was deleted
    /// outright from `-O1` up.
    pub(crate) fn emit_bitfield_load(
        &mut self,
        base: PseudoId,
        bf: Bitfield,
        typ: TypeId,
    ) -> PseudoId {
        let Bitfield {
            offset: byte_offset,
            bit_offset,
            bit_width,
            access_bytes: storage_size,
        } = bf;
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
            return self.emit_bitfield_load_bytewise(base, bf, typ);
        }

        // Determine storage type based on storage unit size
        let storage_type = self.bitfield_storage_type(storage_size as usize);
        let storage_bits = storage_size * 8;
        let volatile = self.types.contains_volatile(typ);

        // 1. Load the entire storage unit
        let storage_val = self.alloc_pseudo();
        self.emit(
            Instruction::load(
                storage_val,
                base,
                byte_offset as i64,
                storage_type,
                storage_bits,
            )
            .with_volatile(volatile),
        );

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
        bf: Bitfield,
        typ: TypeId,
    ) -> PseudoId {
        let Bitfield {
            offset: byte_offset,
            bit_offset,
            bit_width,
            access_bytes: span,
        } = bf;
        let wide = bit_offset + bit_width > 32;
        let carrier = if wide {
            self.types.ulong_id
        } else {
            self.types.uint_id
        };
        let carrier_bits = if wide { 64 } else { 32 };
        let byte_type = self.types.uchar_id;
        // Every byte of a volatile field is part of the one observable read.
        let volatile = self.types.contains_volatile(typ);

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
            self.emit(
                Instruction::load(byte, base, (byte_offset + i as usize) as i64, byte_type, 8)
                    .with_volatile(volatile),
            );
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
    ///
    /// `typ` is the field's type as the access reaches it, and is here for the
    /// same reason as in [`Self::emit_bitfield_load`]: the read-modify-write is
    /// performed on the *carrier*, so the instructions cannot show the
    /// qualifier and the marker is set from the field's type instead. Both
    /// halves are marked -- the read of the storage unit is as observable as
    /// the write of it.
    pub(crate) fn emit_bitfield_store(
        &mut self,
        base: PseudoId,
        bf: Bitfield,
        new_value: PseudoId,
        typ: TypeId,
    ) {
        let Bitfield {
            offset: byte_offset,
            bit_offset,
            bit_width,
            access_bytes: storage_size,
        } = bf;
        if !matches!(storage_size, 1 | 2 | 4 | 8 | 16) {
            return self.emit_bitfield_store_bytewise(base, bf, new_value, typ);
        }

        // Determine storage type based on storage unit size
        let storage_type = self.bitfield_storage_type(storage_size as usize);
        let storage_bits = storage_size * 8;
        let volatile = self.types.contains_volatile(typ);

        // 1. Load current storage unit value
        let old_val = self.alloc_pseudo();
        self.emit(
            Instruction::load(
                old_val,
                base,
                byte_offset as i64,
                storage_type,
                storage_bits,
            )
            .with_volatile(volatile),
        );

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
        self.emit(
            Instruction::store(
                combined,
                base,
                byte_offset as i64,
                storage_type,
                storage_bits,
            )
            .with_volatile(volatile),
        );
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
        bf: Bitfield,
        new_value: PseudoId,
        typ: TypeId,
    ) {
        let Bitfield {
            offset: byte_offset,
            bit_offset,
            bit_width,
            access_bytes: span,
        } = bf;
        let wide = bit_offset + bit_width > 32;
        let carrier = if wide {
            self.types.ulong_id
        } else {
            self.types.uint_id
        };
        let carrier_bits = if wide { 64 } else { 32 };
        let byte_type = self.types.uchar_id;
        let volatile = self.types.contains_volatile(typ);

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
                self.emit(
                    Instruction::load(old, base, addr_off, byte_type, 8).with_volatile(volatile),
                );
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
            self.emit(
                Instruction::store(to_store, base, addr_off, byte_type, 8).with_volatile(volatile),
            );
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
                let (op, zero) = if is_float {
                    (Opcode::FCmpOEq, self.emit_fconst(FloatVal::ZERO, typ))
                } else {
                    (Opcode::SetEq, self.emit_const(0, typ))
                };
                self.emit(self.compare_insn(op, result, (src, zero), typ));
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

        let opcode = if let Some(fcmp) = float_comparison(op).filter(|_| is_float) {
            fcmp
        } else {
            match op {
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
                    if is_unsigned {
                        Opcode::SetB
                    } else {
                        Opcode::SetLt
                    }
                }
                BinaryOp::Gt => {
                    if is_unsigned {
                        Opcode::SetA
                    } else {
                        Opcode::SetGt
                    }
                }
                BinaryOp::Le => {
                    if is_unsigned {
                        Opcode::SetBe
                    } else {
                        Opcode::SetLe
                    }
                }
                BinaryOp::Ge => {
                    if is_unsigned {
                        Opcode::SetAe
                    } else {
                        Opcode::SetGe
                    }
                }
                BinaryOp::Eq => Opcode::SetEq,
                BinaryOp::Ne => Opcode::SetNe,
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
            }
        };

        let insn = if opcode.is_comparison() {
            self.compare_insn(opcode, result, (left, right), operand_typ)
        } else {
            let size = self.types.size_bits(result_typ);
            Instruction::binop(opcode, result, left, right, result_typ, size)
        };
        self.emit(insn);
        result
    }

    /// A comparison of `operands`, which are of type `operand`, into
    /// `target`, as C's relational and equality operators produce it: an
    /// `int` that is 0 or 1 (C17 6.5.8p6, 6.5.9p3).
    ///
    /// The one place the linearizer decides what width a comparison reads.
    /// An array or a function operand is compared as the pointer it decays
    /// to (C17 6.3.2.1p3-4): typed as itself it has no width -- a function's
    /// is 0 -- and both back ends raised that to 32 bits, so `if (weak_fn)`
    /// and `foo == foo` compared the low half of an address.
    pub(crate) fn compare_insn(
        &self,
        op: Opcode,
        target: PseudoId,
        operands: (PseudoId, PseudoId),
        operand: TypeId,
    ) -> Instruction {
        let operand = match self.types.kind(operand) {
            TypeKind::Array | TypeKind::Function => self.types.void_ptr_id,
            _ => operand,
        };
        let width = self.types.size_bits(operand);
        let int = self.types.int_id;
        let result = (int, self.types.size_bits(int));
        Instruction::compare(op, target, operands, (operand, width), result)
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

        let result = self.frame_temp_addr("__ctmp", complex_typ);
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

        let result = self.frame_temp_addr("__ctmp", complex_typ);
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
        let result = self.frame_temp_addr("__ctmp", complex_typ);

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
        let result_addr = self.frame_temp_addr("__ctmp", complex_typ);

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
            BinaryOp::Mul | BinaryOp::Div => self.emit_complex_float_muldiv(
                op,
                (left_real, left_imag),
                (right_real, right_imag),
                base_typ,
            ),
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

    /// A floating complex `*` or `/`, by libgcc's `__mul?c3`/`__div?c3`.
    ///
    /// The routine is the one for the halves' routine format, as gcc picks
    /// it: `_Float16 _Complex` has no routine of its own that gcc calls, so
    /// its halves are widened to `float`, `__mulsc3`/`__divsc3` computes, and
    /// the result is narrowed back -- the same steps the constant folder takes
    /// (`FloatVal::complex_mul`), so a folded and a run-time result agree.
    /// It used to call the `double` routines on half-precision bits.
    fn emit_complex_float_muldiv(
        &mut self,
        op: BinaryOp,
        left: (PseudoId, PseudoId),
        right: (PseudoId, PseudoId),
        base_typ: TypeId,
    ) -> (PseudoId, PseudoId) {
        let (work_typ, routine) = self
            .types
            .complex_routine_type(base_typ)
            .expect("a non-integral complex type has a floating base");
        let routine_fn = match op {
            BinaryOp::Mul => (
                LibFn::MulComplex,
                crate::arch::mapping::complex_mul_name(routine),
            ),
            _ => (
                LibFn::DivComplex,
                crate::arch::mapping::complex_div_name(routine),
            ),
        };
        let mut widen = |v| self.emit_convert(v, base_typ, work_typ);
        let left = (widen(left.0), widen(left.1));
        let right = (widen(right.0), widen(right.1));
        let call_result = self.emit_complex_rtlib_call(
            routine_fn,
            left,
            right,
            work_typ,
            self.types.make_complex(work_typ),
        );
        let (real, imag, _, _) =
            self.load_complex_halves(call_result, self.types.make_complex(work_typ));
        (
            self.emit_convert(real, work_typ, base_typ),
            self.emit_convert(imag, work_typ, base_typ),
        )
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
        let cond = self.emit_compare(cmp, abs_c, abs_d, base_typ);

        // Both arms write their halves to `result_addr`, so there is no value
        // to merge and the void diamond serves: it builds the same blocks and
        // edges, and reads `current_bb` back through the accessor that copes
        // with a `goto` out of an arm.
        self.emit_diamond_void(
            cond,
            // `|c| < |d|`: r = c/d, denom = d + c*r,
            //              re = (a*r + b)/denom, im = (b*r - a)/denom.
            |lin| {
                let r = lin.emit_int_binop(div, c, d, base_typ, base_size);
                let cr = lin.emit_int_binop(Opcode::Mul, c, r, base_typ, base_size);
                let denom = lin.emit_int_binop(Opcode::Add, d, cr, base_typ, base_size);
                let ar = lin.emit_int_binop(Opcode::Mul, a, r, base_typ, base_size);
                let num_re = lin.emit_int_binop(Opcode::Add, ar, b, base_typ, base_size);
                let br = lin.emit_int_binop(Opcode::Mul, b, r, base_typ, base_size);
                let num_im = lin.emit_int_binop(Opcode::Sub, br, a, base_typ, base_size);
                lin.store_complex_quotient(
                    result_addr,
                    (num_re, num_im),
                    denom,
                    div,
                    base_typ,
                    base_size,
                    base_bytes,
                );
            },
            // `|c| >= |d|`: r = d/c, denom = c + d*r,
            //               re = (a + b*r)/denom, im = (b - a*r)/denom.
            |lin| {
                let r = lin.emit_int_binop(div, d, c, base_typ, base_size);
                let dr = lin.emit_int_binop(Opcode::Mul, d, r, base_typ, base_size);
                let denom = lin.emit_int_binop(Opcode::Add, c, dr, base_typ, base_size);
                let br = lin.emit_int_binop(Opcode::Mul, b, r, base_typ, base_size);
                let num_re = lin.emit_int_binop(Opcode::Add, a, br, base_typ, base_size);
                let ar = lin.emit_int_binop(Opcode::Mul, a, r, base_typ, base_size);
                let num_im = lin.emit_int_binop(Opcode::Sub, b, ar, base_typ, base_size);
                lin.store_complex_quotient(
                    result_addr,
                    (num_re, num_im),
                    denom,
                    div,
                    base_typ,
                    base_size,
                    base_bytes,
                );
            },
        );
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
    pub(crate) fn emit_int_abs(&mut self, x: PseudoId, typ: TypeId, size: u32) -> PseudoId {
        if self.types.is_unsigned(typ) {
            return x;
        }
        let shift = self.emit_const((size - 1) as i128, typ);
        let sign = self.emit_int_binop(Opcode::Asr, x, shift, typ, size);
        let flipped = self.emit_int_binop(Opcode::Xor, x, sign, typ, size);
        self.emit_int_binop(Opcode::Sub, flipped, sign, typ, size)
    }

    /// `|x|` for a `float`, `double` or `long double`, whichever `typ` is:
    /// the `Fabs` opcode, which both backends compute in place by clearing
    /// the sign bit -- never a call, so no program needs libm for it.
    pub(crate) fn emit_fabs(&mut self, x: PseudoId, typ: TypeId) -> PseudoId {
        let size = self.types.size_bits(typ);
        let result = self.alloc_pseudo();
        let insn = Instruction::new(Opcode::Fabs)
            .with_target(result)
            .with_src(x)
            .with_size(size)
            .with_type(typ);
        self.emit(insn);
        result
    }

    /// `copysign(x, y)` at `typ`: the `CopySign` opcode, computed in place
    /// by both backends like `Fabs`.
    pub(crate) fn emit_copysign(&mut self, x: PseudoId, y: PseudoId, typ: TypeId) -> PseudoId {
        let size = self.types.size_bits(typ);
        let result = self.alloc_pseudo();
        let insn = Instruction::new(Opcode::CopySign)
            .with_target(result)
            .with_src2(x, y)
            .with_size(size)
            .with_type(typ);
        self.emit(insn);
        result
    }

    /// `sqrt(x)` at `typ`, where `callee` is the library function the call
    /// named (`sqrtf`): the `Sqrt` opcode, which the backends compute with
    /// their square-root instruction, or which a late mapping turns back into
    /// a call to `callee` on a target that has none for `typ`.
    ///
    /// When `errno` is to be set, an argument below zero goes to the library
    /// instead, as gcc arranges it: `x < 0` is ordered, so `-0`, and a NaN,
    /// which the instruction answers the same way the library does, stay on
    /// the instruction. A target that calls the library for every argument
    /// anyway gets only the call -- the call sets `errno` itself, and the
    /// comparison would cost a call of its own on binary128.
    pub(crate) fn emit_sqrt(
        &mut self,
        x: PseudoId,
        typ: TypeId,
        callee: StringId,
        errno: MathErrno,
    ) -> PseudoId {
        let callee = self.library_function_name(self.strings.get(callee));
        let in_place = self.types.fp_format(typ).is_some_and(|fmt| {
            crate::arch::mapping::computes_in_place(Opcode::Sqrt, fmt, self.target)
        });
        let sqrt = |lin: &mut Self| lin.emit_libm_insn(Opcode::Sqrt, &[x], typ, &callee);
        match errno {
            MathErrno::Ignored => sqrt(self),
            MathErrno::Set if !in_place => self.emit_library_call(&callee, &[(x, typ)], typ),
            MathErrno::Set => {
                let zero = self.emit_fconst(FloatVal::ZERO, typ);
                let below = self.emit_compare(Opcode::FCmpOLt, x, zero, typ);
                let size = self.types.size_bits(typ);
                self.emit_diamond(
                    below,
                    typ,
                    size,
                    |lin| lin.emit_library_call(&callee, &[(x, typ)], typ),
                    sqrt,
                )
            }
        }
    }

    /// The libm opcode `op` ([`Opcode::is_libm`]) of `args` at `typ`, for a
    /// call that named the library function `callee`: which a target
    /// without the instruction calls instead.
    pub(crate) fn emit_libm(
        &mut self,
        op: Opcode,
        args: &[PseudoId],
        typ: TypeId,
        callee: StringId,
    ) -> PseudoId {
        let callee = self.library_function_name(self.strings.get(callee));
        self.emit_libm_insn(op, args, typ, &callee)
    }

    /// [`Self::emit_libm`], with the callee's assembler name resolved.
    fn emit_libm_insn(
        &mut self,
        op: Opcode,
        args: &[PseudoId],
        typ: TypeId,
        callee: &str,
    ) -> PseudoId {
        let size = self.types.size_bits(typ);
        let result = self.alloc_pseudo();
        let mut insn = Instruction::new(op)
            .with_target(result)
            .with_size(size)
            .with_type(typ)
            .with_func(callee);
        insn.src = args.to_vec();
        self.emit(insn);
        result
    }

    /// A call to the C library function `callee` (its assembler name) with
    /// `args`, each with its type, returning `typ`.
    pub(crate) fn emit_library_call(
        &mut self,
        callee: &str,
        args: &[(PseudoId, TypeId)],
        typ: TypeId,
    ) -> PseudoId {
        let result = self.alloc_pseudo();
        let (args, arg_types) = args.iter().copied().unzip();
        self.emit(Instruction::call_with_abi(
            Some(result),
            callee,
            args,
            arg_types,
            typ,
            crate::abi::CallingConv::C,
            self.types,
            self.target,
        ));
        result
    }

    /// `cond ? then_arm() : else_arm()`, merged by a `size`-bit phi of type
    /// `typ`.
    ///
    /// The one place a two-armed conditional's blocks and edges are built, so
    /// the one place that has to know `current_bb` is `None` wherever control
    /// cannot arrive -- see [`Linearizer::current_or_unreachable_bb`]. Both
    /// the block the branch leaves and the block each arm *ends* in are read
    /// back through that accessor: an arm is arbitrary code and may itself
    /// `goto` away, so `x ? ({ goto L; g(); }) : g()` has no block at the end
    /// of its true arm.
    ///
    /// `size` is passed rather than taken from `typ` because the two are not
    /// always the same: a complex conditional merges *addresses*, so its phi
    /// is pointer-wide over a pointer type, and a function designator's
    /// `size_bits` is 0 where the merge wants a pointer's 64.
    pub(crate) fn emit_diamond(
        &mut self,
        cond: PseudoId,
        typ: TypeId,
        size: u32,
        then_arm: impl FnOnce(&mut Self) -> PseudoId,
        else_arm: impl FnOnce(&mut Self) -> PseudoId,
    ) -> PseudoId {
        let (merge_bb, arms) = self.emit_fork(cond, then_arm, else_arm);

        self.switch_bb(merge_bb);
        let result = self.alloc_pseudo();
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(Pseudo::phi(result, result.0));
        }
        let mut phi = Instruction::phi(result, typ, size);
        for (end, value) in arms {
            let src = self.emit_phi_source(end, value, result, merge_bb, typ, size);
            phi.phi_list.push((end, src));
        }
        self.emit(phi);
        result
    }

    /// [`Self::emit_diamond`] for arms that produce no value: they write
    /// their results where the caller can find them, so there is nothing to
    /// merge and no phi. Leaves the cursor on the merge block.
    pub(crate) fn emit_diamond_void(
        &mut self,
        cond: PseudoId,
        then_arm: impl FnOnce(&mut Self),
        else_arm: impl FnOnce(&mut Self),
    ) {
        let (merge_bb, _) = self.emit_fork(cond, then_arm, else_arm);
        self.switch_bb(merge_bb);
    }

    /// The block plumbing both diamonds share: branch on `cond` into a block
    /// per arm, run each arm, and join them.
    ///
    /// Returns the merge block -- which the caller has *not* switched to yet,
    /// so a phi can be placed at its head -- and, per arm, the block it ended
    /// in and whatever it produced.
    fn emit_fork<T>(
        &mut self,
        cond: PseudoId,
        then_arm: impl FnOnce(&mut Self) -> T,
        else_arm: impl FnOnce(&mut Self) -> T,
    ) -> (BasicBlockId, [(BasicBlockId, T); 2]) {
        let (then_bb, else_bb, merge_bb) = (self.alloc_bb(), self.alloc_bb(), self.alloc_bb());
        let from = self.current_or_unreachable_bb();
        self.emit(Instruction::cbr(cond, then_bb, else_bb));
        self.link_bb(from, then_bb);
        self.link_bb(from, else_bb);

        let arms = [
            self.emit_arm(then_bb, merge_bb, then_arm),
            self.emit_arm(else_bb, merge_bb, else_arm),
        ];
        (merge_bb, arms)
    }

    /// One arm of [`Self::emit_fork`]: `arm` evaluated in `bb`, which then
    /// branches to `merge`. Returns the block the arm ended in, and its value.
    fn emit_arm<T>(
        &mut self,
        bb: BasicBlockId,
        merge: BasicBlockId,
        arm: impl FnOnce(&mut Self) -> T,
    ) -> (BasicBlockId, T) {
        self.switch_bb(bb);
        let value = arm(self);
        let end = self.current_or_unreachable_bb();
        self.emit(Instruction::br(merge));
        self.link_bb(end, merge);
        (end, value)
    }

    /// `signbit(x)` of `x`, a value of the real floating type `typ`: 0 or 1,
    /// an `int`.
    pub(crate) fn emit_signbit(&mut self, x: PseudoId, typ: TypeId) -> PseudoId {
        let int_id = self.types.int_id;
        let result = self.alloc_pseudo();
        let mut insn = Instruction::new(Opcode::Signbit)
            .with_target(result)
            .with_src(x)
            .with_type_and_size(int_id, self.types.size_bits(int_id));
        insn.src_typ = Some(typ);
        insn.src_size = self.types.size_bits(typ);
        self.emit(insn);
        result
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
        debug_assert!(
            !op.is_comparison(),
            "a comparison goes through emit_compare"
        );
        let dst = self.alloc_pseudo();
        self.emit(Instruction::binop(op, dst, lhs, rhs, typ, size));
        dst
    }

    /// `op` comparing `lhs` and `rhs`, of type `typ` at `size` bits, into a
    /// fresh `int` pseudo. See [`Self::compare_insn`].
    pub(crate) fn emit_compare(
        &mut self,
        op: Opcode,
        lhs: PseudoId,
        rhs: PseudoId,
        typ: TypeId,
    ) -> PseudoId {
        let dst = self.alloc_pseudo();
        self.emit(self.compare_insn(op, dst, (lhs, rhs), typ));
        dst
    }

    /// A fresh frame-resident local of `typ`, named `{prefix}_{id}`, and its
    /// `Sym` pseudo.
    ///
    /// The one way the linearizer makes a compiler temporary with a fixed stack
    /// slot -- a call's result buffer, a `va_arg` aggregate, an argument copy.
    /// A slot in the frame costs nothing in a loop, where an `alloca` would
    /// grow the stack on every evaluation until the function returned.
    pub(crate) fn frame_temp(&mut self, prefix: &str, typ: TypeId) -> PseudoId {
        let sym = self.alloc_pseudo();
        let name = format!("{prefix}_{}", sym.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(Pseudo::sym(sym, name.clone()));
            func.add_local(&name, sym, typ, self.current_bb, None);
        }
        sym
    }

    /// The address of a fresh [`Self::frame_temp`], in a register.
    ///
    /// For a temporary whose consumers take a plain pointer: a complex value,
    /// which travels by address and may be phi-merged or passed on, and a CAS
    /// expected-value slot, which `AtomicCas` writes back through. A `Sym`
    /// names the storage itself, so handing one of those the `Sym` would have
    /// them read the slot's contents where they expect its address.
    pub(crate) fn frame_temp_addr(&mut self, prefix: &str, typ: TypeId) -> PseudoId {
        let sym = self.frame_temp(prefix, typ);
        let addr = self.alloc_reg_pseudo();
        let ptr_type = self.types.pointer_to(typ);
        self.emit(Instruction::sym_addr(addr, sym, ptr_type));
        addr
    }

    /// Emit a call to a complex rtlib function (__mulXc3, __divXc3).
    ///
    /// These functions take 4 scalar args (left_real, left_imag, right_real, right_imag)
    /// and return a complex value. The result is stored in newly allocated local storage.
    ///
    /// They are ordinary C functions, so they are called by the ABI's
    /// classification of their signature like any other: each half in its
    /// own argument register, and the result wherever the complex type's
    /// return class puts it. On x86-64 `__multc3` hands its
    /// `_Float128 _Complex` result back through the hidden pointer -- the
    /// type is MEMORY class -- which is what gcc calls it with.
    ///
    /// The call is tagged with the routine it calls (`routine_fn`: the
    /// `LibFn` and its name), so that `ir::libcall_fold` can compute it when
    /// its operands are constant.
    ///
    /// Returns the address where the complex result is stored.
    pub(crate) fn emit_complex_rtlib_call(
        &mut self,
        routine_fn: (LibFn, &str),
        left: (PseudoId, PseudoId),
        right: (PseudoId, PseudoId),
        base_typ: TypeId,
        complex_typ: TypeId,
    ) -> PseudoId {
        let (left_real, left_imag) = left;
        let (right_real, right_imag) = right;
        let (known, func_name) = routine_fn;
        // libgcc's routines are ordinary functions of the target's own
        // convention, whatever the function calling them is.
        let conv = crate::abi::CallingConv::C;
        let sret = self.returns_via_hidden_pointer(complex_typ, conv);

        // The result's storage, and the hidden pointer to it if there is one.
        let (result_sym, mut arg_vals, mut arg_types) = if sret {
            let slot = self.hidden_return_slot(complex_typ);
            (slot.storage, vec![slot.arg], vec![slot.arg_typ])
        } else {
            (
                self.frame_temp("__cret", complex_typ),
                Vec::new(),
                Vec::new(),
            )
        };
        arg_vals.extend([left_real, left_imag, right_real, right_imag]);
        arg_types.extend([base_typ; 4]);

        // Compute ABI classification for the call
        let abi = get_abi_for_conv(conv, self.target);
        let param_classes: Vec<_> = arg_types
            .iter()
            .map(|&t| abi.classify_param(t, self.types))
            .collect();
        let ret_class = abi.classify_return(complex_typ, self.types);
        let call_abi_info = Box::new(CallAbiInfo::new(param_classes, ret_class));

        // Through the hidden pointer, the call's own value is that pointer
        // and the result is read from the storage; otherwise the backend
        // writes the returned registers into the storage directly.
        let mut call_insn = if sret {
            let arg_typ = arg_types[0];
            Instruction::call(
                Some(self.alloc_reg_pseudo()),
                func_name,
                arg_vals,
                arg_types,
                arg_typ,
                64,
            )
        } else {
            Instruction::call(
                Some(result_sym),
                func_name,
                arg_vals,
                arg_types,
                complex_typ,
                self.types.size_bits(complex_typ),
            )
        };
        call_insn.extra_mut().abi_info = Some(call_abi_info);
        call_insn.extra_mut().known = Some(known);
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
        let (real, imag, base_typ, _) = self.load_complex_halves(addr, complex_typ);

        let ne = self.complex_half_ops(base_typ).ne;
        let zero = self.complex_half_zero(base_typ);
        let real_nz = self.emit_compare(ne, real, zero, base_typ);
        let imag_nz = self.emit_compare(ne, imag, zero, base_typ);

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
        let (lre, lim, base_typ, _) = self.load_complex_halves(left_addr, complex_typ);
        let (rre, rim, _, _) = self.load_complex_halves(right_addr, complex_typ);

        let ops = self.complex_half_ops(base_typ);
        let (half_op, combine) = if op == BinaryOp::Eq {
            (ops.eq, Opcode::And)
        } else {
            (ops.ne, Opcode::Or)
        };

        let real_cmp = self.emit_compare(half_op, lre, rre, base_typ);
        let imag_cmp = self.emit_compare(half_op, lim, rim, base_typ);

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

    /// Whether a controlling expression holds, when it is a constant
    /// expression (C17 6.6) and so decides that by itself.
    ///
    /// The one question every construct that branches asks before it emits
    /// the branch: `if`, the three loops, `?:`, and the left operand of `&&`
    /// and `||`. gcc's front end folds the same conditions at every level,
    /// `-O0` included, and emits nothing for the arm one makes unreachable.
    /// A `const` object is not a constant expression, so `const int k = 0;
    /// if (k)` keeps its arm, as it does in gcc.
    pub(crate) fn constant_condition(&self, cond: &Expr) -> Option<bool> {
        crate::constexpr::eval_truth(self, ConstScope::Standard, cond)
    }

    /// Evaluate a controlling expression for a branch: as the constant it is,
    /// or as a value computed at run time. See [`Self::constant_condition`].
    ///
    /// `0 && g()` is a constant too. C17 6.6p3 admits a call in an operand
    /// that is not evaluated, and gcc folds the branch -- but the shared walk
    /// refuses it, because gcc does not make it an *integer* constant
    /// expression (`int a[0 && g()]` is a VLA there). So the short circuit is
    /// taken here: a left operand that decides `&&` or `||` decides the
    /// branch, and one that does not leaves the right operand to decide it.
    pub(crate) fn controlling_value(&mut self, cond: &Expr) -> Controlling {
        if let Some(holds) = self.constant_condition(cond) {
            return Controlling::Constant(holds);
        }
        if let Some(holds) = self.decided_float_comparison(cond) {
            return Controlling::Constant(holds);
        }
        if let ExprKind::Binary {
            op: op @ (BinaryOp::LogAnd | BinaryOp::LogOr),
            left,
            right,
        } = &cond.kind
        {
            // `&&` is decided by a false left operand, `||` by a true one.
            let decides = *op == BinaryOp::LogOr;
            match self.constant_condition(left) {
                Some(holds) if holds == decides => return Controlling::Constant(holds),
                Some(_) => return self.controlling_value(right),
                None => {}
            }
        }
        Controlling::Value(self.linearize_condition(cond))
    }

    /// A floating comparison of an unknown value with a constant that no
    /// value can change the answer of -- `x > +Inf` is 0 whatever `x` is, a
    /// NaN included -- under `-fno-trapping-math` only. The unknown operand is
    /// still evaluated, for its side effects.
    ///
    /// gcc folds these at `-O0` exactly when trapping math is off: an ordered
    /// comparison with a NaN raises `FE_INVALID`, and a folded one would not.
    /// Which comparisons are decided is the optimizer's own rule
    /// (`constfold::fcmp_against_constant`), asked of the constant as it
    /// converts to the operands' common type -- `1e308 * 10` is `+Inf` as a
    /// `double`. The `isgreater` family asks it too.
    fn decided_float_comparison(&mut self, cond: &Expr) -> Option<bool> {
        if self.trapping_math {
            return None;
        }
        let (op, left, right) = match &cond.kind {
            ExprKind::Binary { op, left, right } => (float_comparison(*op)?, left, right),
            ExprKind::FpCompare { cmp, lhs, rhs } => (fp_compare_opcode(*cmp)?, lhs, rhs),
            _ => return None,
        };
        let common = self.types.common_type(left.typ?, right.typ?);
        let fmt = self.types.fp_format(common)?;
        if !self.types.is_float(common) {
            return None;
        }
        let known = |e: &Expr| {
            crate::constexpr::eval_as_float(self, ConstScope::Standard, e, common)
                .map(|v| v.round_to_format(fmt))
        };
        let (c, const_first, unknown) = match (known(left), known(right)) {
            (None, Some(c)) => (c, false, left),
            (Some(c), None) => (c, true, right),
            _ => return None,
        };
        let holds = super::constfold::fcmp_against_constant(op, c, const_first)?;
        self.linearize_expr(unknown);
        Some(holds)
    }

    /// Branch from the current block to `then_bb` when `cond` holds and to
    /// `else_bb` when it does not.
    ///
    /// A constant condition jumps straight to the block it selects, and the
    /// other one gets no edge. Nothing else in the linearizer has to know:
    /// a block that nothing reaches is removed when the function is finished
    /// (`Function::remove_unreachable_blocks`), and one a label, `case` or `default`
    /// inside the dead arm still reaches keeps its edge and so survives.
    ///
    /// With no current block -- control cannot reach here -- the branch is
    /// emitted into a fresh unreachable one, like every other terminator:
    /// returning without a branch would leave `then_bb` and `else_bb` with one
    /// predecessor fewer than their callers built them for, and nothing would
    /// say so.
    pub(crate) fn branch_on(
        &mut self,
        cond: Controlling,
        then_bb: BasicBlockId,
        else_bb: BasicBlockId,
    ) {
        let current = self.current_or_unreachable_bb();
        match cond {
            Controlling::Constant(holds) => {
                let target = if holds { then_bb } else { else_bb };
                self.emit(Instruction::br(target));
                self.link_bb(current, target);
            }
            Controlling::Value(val) => {
                self.emit(Instruction::cbr(val, then_bb, else_bb));
                self.link_bb(current, then_bb);
                self.link_bb(current, else_bb);
            }
        }
    }

    /// [`Self::controlling_value`] of `cond`, then [`Self::branch_on`] it.
    pub(crate) fn branch_on_condition(
        &mut self,
        cond: &Expr,
        then_bb: BasicBlockId,
        else_bb: BasicBlockId,
    ) {
        let cond = self.controlling_value(cond);
        self.branch_on(cond, then_bb, else_bb);
    }

    /// `val != 0`, read the way `operand_typ` says to read it.
    pub(crate) fn emit_compare_zero(&mut self, val: PseudoId, operand_typ: TypeId) -> PseudoId {
        // A float is compared against 0.0, not against its bit pattern:
        // -0.0 is zero and every NaN is not.
        let (op, zero) = if self.types.is_float(operand_typ) {
            (
                Opcode::FCmpONe,
                self.emit_fconst(FloatVal::ZERO, operand_typ),
            )
        } else {
            (Opcode::SetNe, self.emit_const(0, operand_typ))
        };
        self.emit_compare(op, val, zero, operand_typ)
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
        let left_cond = self.controlling_value(left);
        // A constant true LHS always goes on to the RHS, so the merge has no
        // edge from here.
        let short_circuits = !matches!(left_cond, Controlling::Constant(true));

        // Emit the short-circuit value (0) BEFORE the branch, while still in LHS block
        // This value will be used if we short-circuit (LHS is false)
        let zero = self.emit_const(0, result_typ);

        // Get the block where LHS evaluation ended (may differ from initial block
        // if LHS contains nested control flow)
        //
        // Read through the accessor, not `unwrap`: control cannot arrive at a
        // statement before a `switch`'s first `case` or after a `goto`, and the
        // phi below needs a real predecessor block to hold its source. See
        // `current_or_unreachable_bb`. `branch_on` re-reads `current_bb`
        // itself, so it sees the same block.
        let lhs_end_bb = self.current_or_unreachable_bb();

        // Branch: if LHS is false, go to merge (result = 0); else evaluate RHS
        self.branch_on(left_cond, eval_b_bb, merge_bb);

        // eval_b_bb: Evaluate RHS
        self.switch_bb(eval_b_bb);
        let right_bool = self.linearize_condition(right);

        // Get the actual block where RHS evaluation ended (may differ from eval_b_bb
        // if RHS contains nested control flow like another &&/||), and may be
        // gone entirely where the RHS jumped away: `x && ({ goto L; g(); })`.
        let rhs_end_bb = self.current_or_unreachable_bb();

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
        if short_circuits {
            let phisrc1 = self.emit_phi_source(lhs_end_bb, zero, result, merge_bb, result_typ, 32);
            phi_insn.phi_list.push((lhs_end_bb, phisrc1));
        }
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
        let left_cond = self.controlling_value(left);
        // A constant false LHS always goes on to the RHS, so the merge has no
        // edge from here.
        let short_circuits = !matches!(left_cond, Controlling::Constant(false));

        // Emit the short-circuit value (1) BEFORE the branch, while still in LHS block
        // This value will be used if we short-circuit (LHS is true)
        let one = self.emit_const(1, result_typ);

        // Get the block where LHS evaluation ended (may differ from initial block
        // if LHS contains nested control flow)
        //
        // Through the accessor for the reason `emit_logical_and` records.
        let lhs_end_bb = self.current_or_unreachable_bb();

        // Branch: if LHS is true, go to merge (result = 1); else evaluate RHS
        self.branch_on(left_cond, merge_bb, eval_b_bb);

        // eval_b_bb: Evaluate RHS
        self.switch_bb(eval_b_bb);
        let right_bool = self.linearize_condition(right);

        // Get the actual block where RHS evaluation ended (may differ from eval_b_bb
        // if RHS contains nested control flow like another &&/||), and may be
        // gone entirely where the RHS jumped away: `x || ({ goto L; g(); })`.
        let rhs_end_bb = self.current_or_unreachable_bb();

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
        if short_circuits {
            let phisrc1 = self.emit_phi_source(lhs_end_bb, one, result, merge_bb, result_typ, 32);
            phi_insn.phi_list.push((lhs_end_bb, phisrc1));
        }
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
            .unwrap_or_else(|| MemberInfo::standing_in(target_typ));
        let bitfield = match info.bitfield() {
            // The target expression's type, not the member's declared one:
            // they name the same width and sign, and only the expression's
            // carries the object's qualifiers (C17 6.5.2.3p3), which is what
            // tells the bit-field emitters that the access is volatile.
            Some(bf) => Some((bf, target_typ)),
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
    pub(crate) fn offset_address(&mut self, base: PseudoId, offset: i64) -> PseudoId {
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
        if let Some((bf, field_typ)) = place.bitfield {
            return self.emit_bitfield_load(place.base, bf, field_typ);
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
        if let Some((bf, field_typ)) = place.bitfield {
            self.emit_bitfield_store(place.base, bf, val, field_typ);
            return Some((bf.bit_width, field_typ));
        }
        let size = self.types.size_bits(typ);
        self.emit(Instruction::store(val, place.base, 0, typ, size));
        None
    }

    /// The value `E1 op= E2` stores, given `E1`'s current value and `E2`'s.
    ///
    /// The whole of C17 6.5.16.2p3's arithmetic lives here: choose the type to
    /// compute at, choose the opcode for it, bring both operands to it, apply
    /// the operator, and convert the result back to the target. Callers supply
    /// the two values and nothing else, which is what keeps the ordinary and
    /// the `_Atomic` lowerings computing the same thing.
    ///
    /// `rhs` arrives **unconverted**, at `ca.value_typ`. Converting it to the
    /// target first is not an optimization of this: it is a different
    /// computation, and it was the bug (see [`CompoundAssign`]).
    ///
    /// The value this returns is also the value of the assignment expression
    /// (C17 6.5.16p3: the left operand's value *after* the assignment), which
    /// is why the conversion back is part of the helper rather than of the
    /// store: `_Atomic _Bool b = 0; (b -= 1)` has to yield the 1 it stored,
    /// not the 255 the subtraction produced.
    pub(crate) fn compound_assign_value(
        &mut self,
        ca: &CompoundAssign,
        lhs: PseudoId,
        rhs: PseudoId,
    ) -> PseudoId {
        let arith_type = compound_assign_arith_type(self.types, ca);
        let arith_size = self.types.size_bits(arith_type);
        let opcode = compound_assign_opcode(self.types, ca.op, arith_type);

        // Both operands into the arithmetic type. The left one is the object's
        // current value, read at the target's type; the right one is whatever
        // it was written as.
        let lhs = if ca.is_ptr_arith {
            lhs
        } else {
            self.emit_convert(lhs, ca.target_typ, arith_type)
        };
        let rhs = if ca.is_ptr_arith {
            rhs
        } else if matches!(ca.op, AssignOp::ShlAssign | AssignOp::ShrAssign) {
            // The shift count is promoted on its own and is not brought to the
            // left operand's type (C17 6.5.7p3).
            self.emit_convert(rhs, ca.value_typ, self.types.integer_promote(ca.value_typ))
        } else {
            self.emit_convert(rhs, ca.value_typ, arith_type)
        };

        let result = self.alloc_reg_pseudo();
        self.emit(Instruction::binop(
            opcode, result, lhs, rhs, arith_type, arith_size,
        ));

        // `nand` is `and` with the result complemented, and the complement
        // belongs on this side of the conversion below.
        let result = if ca.invert {
            let inverted = self.alloc_reg_pseudo();
            self.emit(Instruction::unop(
                Opcode::Not,
                inverted,
                result,
                arith_type,
                arith_size,
            ));
            inverted
        } else {
            result
        };

        // And the result back, which is the conversion that makes `(x /= y)`
        // yield what `x` now holds. Pointer arithmetic is already a pointer.
        if ca.is_ptr_arith {
            result
        } else {
            self.emit_convert(result, arith_type, ca.target_typ)
        }
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
            // Every other compound operator is a constraint violation on a
            // complex operand, which the parser has reported.
            let binop = op.binary_op().filter(|b| {
                matches!(
                    b,
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div
                )
            });

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

        // For struct/union assignment, do a block copy via addresses, at every
        // size: `linearize_lvalue` gives the source's address whether the
        // expression yields a small aggregate's value or a large one's
        // address.
        let target_kind = self.types.kind(target_typ);
        let target_size_bytes = self.types.size_bytes(target_typ);
        if (target_kind == TypeKind::Struct || target_kind == TypeKind::Union)
            && target_size_bytes > 0
            && op == AssignOp::Assign
        {
            let target_addr = self.linearize_lvalue(target);
            let value_addr = self.linearize_lvalue(value);

            let vol = self.block_volatility(target_typ, self.expr_type(value));
            self.emit_block_copy(target_addr, value_addr, target_size_bytes as i64, vol);

            // The assignment's value is the target's new value, in the IR's
            // convention for an aggregate: its value when it fits in a
            // register, its address otherwise. Returning the address at every
            // size handed `x = (t = u)` a pointer where a small struct's bits
            // belong.
            if self.aggregate_travels_by_value(target_typ) {
                let size = self.types.size_bits(target_typ);
                let value = self.alloc_pseudo();
                self.emit(Instruction::load(value, target_addr, 0, target_typ, size));
                return value;
            }
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
            // A compound assignment leaves its right operand alone here. It
            // is converted to the *common* type by `compound_assign_value`,
            // not down to the target's:
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
                // One helper owns the whole of C17 6.5.16.2p3's arithmetic,
                // shared with the `_Atomic` lowering, which used to carry its
                // own copy of these rules and disagree with this one.
                let ca = CompoundAssign {
                    is_ptr_arith,
                    ..CompoundAssign::new(op, target_typ, value_typ)
                };
                self.compound_assign_value(&ca, lhs, rhs)
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
