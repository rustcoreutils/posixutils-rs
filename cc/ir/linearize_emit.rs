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
use crate::parse::ast::{AssignOp, BinaryOp, Expr, ExprKind, UnaryOp};
use crate::types::{MemberInfo, TypeId, TypeKind};

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

    pub(crate) fn emit_fconst(&mut self, val: f64, typ: TypeId) -> PseudoId {
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
        let total_bytes = self.types.size_bits(typ) / 8;
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

    /// Emit a block copy from src to dst using integer chunks.
    /// The destination stores start at dst_base_offset.
    pub(crate) fn emit_block_copy_at_offset(
        &mut self,
        dst: PseudoId,
        dst_base_offset: i64,
        src: PseudoId,
        size_bytes: i64,
    ) {
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
        let mask = (1u64 << bit_width) - 1;
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

        // 4. Sign extend if this is a signed bitfield
        if !self.types.is_unsigned(typ) && bit_width < storage_bits {
            self.emit_sign_extend_bitfield(masked, bit_width, storage_bits)
        } else {
            masked
        }
    }

    /// Sign-extend a bitfield value from bit_width to target_bits
    pub(crate) fn emit_sign_extend_bitfield(
        &mut self,
        value: PseudoId,
        bit_width: u32,
        target_bits: u32,
    ) -> PseudoId {
        // Sign extend by shifting left then arithmetic shifting right
        let shift_amount = target_bits - bit_width;
        let typ = if target_bits <= 32 {
            self.types.int_id
        } else {
            self.types.long_id
        };

        let shift_val = self.emit_const(shift_amount as i128, typ);
        let shifted_left = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Shl,
            shifted_left,
            value,
            shift_val,
            typ,
            target_bits,
        ));

        let result = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Asr,
            result,
            shifted_left,
            shift_val,
            typ,
            target_bits,
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
        let field_mask = ((1u64 << bit_width) - 1) << bit_offset;
        let clear_mask = !field_mask;
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
        let value_mask = (1u64 << bit_width) - 1;
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

    pub(crate) fn emit_unary(&mut self, op: UnaryOp, src: PseudoId, typ: TypeId) -> PseudoId {
        let is_float = self.types.is_float(typ);
        let size = self.types.size_bits(typ);

        let result = self.alloc_pseudo();

        let opcode = match op {
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
                    let zero = self.emit_fconst(0.0, typ);
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
                // For struct types, always return the address — struct member
                // access requires an address for offset-based field access.
                if type_kind == TypeKind::Struct {
                    return src;
                }
                // For large union types (> 64 bits), return the address.
                // For small unions (<= 64 bits), LOAD the value — unions are
                // accessed as whole values, not via member offsets, and
                // returning the pointer causes callers to store the pointer
                // instead of the union value (Bug L).
                if type_kind == TypeKind::Union && size > 64 {
                    return src;
                }
                self.emit(Instruction::load(result, src, 0, typ, size));
                return result;
            }
            UnaryOp::PreInc => {
                let is_ptr = self.types.kind(typ) == TypeKind::Pointer;
                let increment = if is_ptr {
                    let elem_type = self.types.base_type(typ).unwrap_or(self.types.char_id);
                    let elem_size = self.types.size_bits(elem_type) / 8;
                    self.emit_const(elem_size as i128, self.types.long_id)
                } else if is_float {
                    self.emit_fconst(1.0, typ)
                } else {
                    self.emit_const(1, typ)
                };
                let opcode = if is_float { Opcode::FAdd } else { Opcode::Add };
                self.emit(Instruction::binop(
                    opcode, result, src, increment, typ, size,
                ));
                return result;
            }
            UnaryOp::PreDec => {
                let is_ptr = self.types.kind(typ) == TypeKind::Pointer;
                let decrement = if is_ptr {
                    let elem_type = self.types.base_type(typ).unwrap_or(self.types.char_id);
                    let elem_size = self.types.size_bits(elem_type) / 8;
                    self.emit_const(elem_size as i128, self.types.long_id)
                } else if is_float {
                    self.emit_fconst(1.0, typ)
                } else {
                    self.emit_const(1, typ)
                };
                let opcode = if is_float { Opcode::FSub } else { Opcode::Sub };
                self.emit(Instruction::binop(
                    opcode, result, src, decrement, typ, size,
                ));
                return result;
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
        let is_unsigned = self.types.is_unsigned(operand_typ);

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
        let (result_real, result_imag) = match op {
            BinaryOp::Add => {
                // (a + bi) + (c + di) = (a+c) + (b+d)i
                let real = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FAdd,
                    real,
                    left_real,
                    right_real,
                    base_typ,
                    base_size,
                ));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FAdd,
                    imag,
                    left_imag,
                    right_imag,
                    base_typ,
                    base_size,
                ));
                (real, imag)
            }
            BinaryOp::Sub => {
                // (a + bi) - (c + di) = (a-c) + (b-d)i
                let real = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FSub,
                    real,
                    left_real,
                    right_real,
                    base_typ,
                    base_size,
                ));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FSub,
                    imag,
                    left_imag,
                    right_imag,
                    base_typ,
                    base_size,
                ));
                (real, imag)
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

    pub(crate) fn emit_compare_zero(&mut self, val: PseudoId, operand_typ: TypeId) -> PseudoId {
        let result = self.alloc_pseudo();
        let zero = self.emit_const(0, operand_typ);
        let size = self.types.size_bits(operand_typ);
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
        let left_typ = self.expr_type(left);
        let left_val = self.linearize_expr(left);
        let left_bool = self.emit_compare_zero(left_val, left_typ);

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
        let right_typ = self.expr_type(right);
        let right_val = self.linearize_expr(right);
        let right_bool = self.emit_compare_zero(right_val, right_typ);

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
        let left_typ = self.expr_type(left);
        let left_val = self.linearize_expr(left);
        let left_bool = self.emit_compare_zero(left_val, left_typ);

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
        let right_typ = self.expr_type(right);
        let right_val = self.linearize_expr(right);
        let right_bool = self.emit_compare_zero(right_val, right_typ);

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

    pub(crate) fn emit_assign(&mut self, op: AssignOp, target: &Expr, value: &Expr) -> PseudoId {
        let target_typ = self.expr_type(target);
        let value_typ = self.expr_type(value);

        // For complex type assignment, handle specially - copy real and imag parts
        if self.types.is_complex(target_typ) && op == AssignOp::Assign {
            let target_addr = self.linearize_lvalue(target);
            let value_addr = self.linearize_lvalue(value);
            let base_typ = self.types.complex_base(target_typ);
            let base_size = self.types.size_bits(base_typ);
            let base_bytes = (base_size / 8) as i64;

            // Load and store real part
            let real = self.alloc_pseudo();
            self.emit(Instruction::load(real, value_addr, 0, base_typ, base_size));
            self.emit(Instruction::store(
                real,
                target_addr,
                0,
                base_typ,
                base_size,
            ));

            // Load and store imaginary part
            let imag = self.alloc_pseudo();
            self.emit(Instruction::load(
                imag, value_addr, base_bytes, base_typ, base_size,
            ));
            self.emit(Instruction::store(
                imag,
                target_addr,
                base_bytes,
                base_typ,
                base_size,
            ));

            return real; // Return real part as the result value
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

        let rhs = self.linearize_expr(value);

        // Check for pointer compound assignment (p += n or p -= n)
        let is_ptr_arith = self.types.kind(target_typ) == TypeKind::Pointer
            && self.types.is_integer(value_typ)
            && (op == AssignOp::AddAssign || op == AssignOp::SubAssign);

        // Convert RHS to target type if needed (but not for pointer arithmetic)
        let rhs = if is_ptr_arith {
            // For pointer arithmetic, scale the integer by element size
            let elem_type = self
                .types
                .base_type(target_typ)
                .unwrap_or(self.types.char_id);
            let elem_size = self.types.size_bits(elem_type) / 8;
            let scale = self.emit_const(elem_size as i128, self.types.long_id);

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
        } else {
            self.emit_convert(rhs, value_typ, target_typ)
        };

        let final_val = match op {
            AssignOp::Assign => rhs,
            _ => {
                // Compound assignment - get current value and apply operation
                let lhs = self.linearize_expr(target);
                let result = self.alloc_reg_pseudo();

                let is_float = self.types.is_float(target_typ);
                let is_unsigned = self.types.is_unsigned(target_typ);
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

                // For pointer arithmetic, use Long type for the operation
                let arith_type = if is_ptr_arith {
                    self.types.long_id
                } else {
                    target_typ
                };

                let arith_size = self.types.size_bits(arith_type);
                self.emit(Instruction::binop(
                    opcode, result, lhs, rhs, arith_type, arith_size,
                ));
                result
            }
        };

        // Store based on target expression type
        let target_size = self.types.size_bits(target_typ);
        match &target.kind {
            ExprKind::Ident(symbol_id) => {
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
            ExprKind::Member { expr, member } => {
                // Struct member: get address and store with offset
                let base = self.linearize_lvalue(expr);
                let base_struct_type = self.expr_type(expr);
                // Resolve if the struct type is incomplete (forward-declared)
                let struct_type = self.resolve_struct_type(base_struct_type);
                let member_info =
                    self.types
                        .find_member(struct_type, *member)
                        .unwrap_or(MemberInfo {
                            offset: 0,
                            typ: target_typ,
                            bit_offset: None,
                            bit_width: None,
                            storage_unit_size: None,
                        });
                if let (Some(bit_offset), Some(bit_width), Some(storage_size)) = (
                    member_info.bit_offset,
                    member_info.bit_width,
                    member_info.storage_unit_size,
                ) {
                    // Bitfield store
                    self.emit_bitfield_store(
                        base,
                        member_info.offset,
                        bit_offset,
                        bit_width,
                        storage_size,
                        final_val,
                    );
                } else {
                    let member_size = self.types.size_bits(member_info.typ);
                    self.emit(Instruction::store(
                        final_val,
                        base,
                        member_info.offset as i64,
                        member_info.typ,
                        member_size,
                    ));
                }
            }
            ExprKind::Arrow { expr, member } => {
                // Pointer member: pointer value is the base address
                let ptr = self.linearize_expr(expr);
                let ptr_type = self.expr_type(expr);
                let base_struct_type = self.types.base_type(ptr_type).unwrap_or(target_typ);
                // Resolve if the struct type is incomplete (forward-declared)
                let struct_type = self.resolve_struct_type(base_struct_type);
                let member_info =
                    self.types
                        .find_member(struct_type, *member)
                        .unwrap_or(MemberInfo {
                            offset: 0,
                            typ: target_typ,
                            bit_offset: None,
                            bit_width: None,
                            storage_unit_size: None,
                        });
                if let (Some(bit_offset), Some(bit_width), Some(storage_size)) = (
                    member_info.bit_offset,
                    member_info.bit_width,
                    member_info.storage_unit_size,
                ) {
                    // Bitfield store
                    self.emit_bitfield_store(
                        ptr,
                        member_info.offset,
                        bit_offset,
                        bit_width,
                        storage_size,
                        final_val,
                    );
                } else {
                    let member_size = self.types.size_bits(member_info.typ);
                    self.emit(Instruction::store(
                        final_val,
                        ptr,
                        member_info.offset as i64,
                        member_info.typ,
                        member_size,
                    ));
                }
            }
            ExprKind::Unary {
                op: UnaryOp::Deref,
                operand,
            } => {
                // Dereference: store to the pointer address
                let ptr = self.linearize_expr(operand);
                self.emit(Instruction::store(
                    final_val,
                    ptr,
                    0,
                    target_typ,
                    target_size,
                ));
            }
            ExprKind::Index { array, index } => {
                // Array subscript: compute address and store
                // Handle commutative form: 0[arr] is equivalent to arr[0]
                let array_type = self.expr_type(array);
                let index_type = self.expr_type(index);

                let array_kind = self.types.kind(array_type);
                let (ptr_expr, idx_expr, idx_type) =
                    if array_kind == TypeKind::Pointer || array_kind == TypeKind::Array {
                        (array, index, index_type)
                    } else {
                        // Swap: index is actually the pointer/array
                        (index, array, array_type)
                    };

                let arr = self.linearize_expr(ptr_expr);
                let idx = self.linearize_expr(idx_expr);
                let elem_size = target_size / 8;
                let elem_size_val = self.emit_const(elem_size as i128, self.types.long_id);

                // Sign-extend index to 64-bit for proper pointer arithmetic (negative indices)
                let idx_extended = self.emit_convert(idx, idx_type, self.types.long_id);

                let offset = self.alloc_pseudo();
                let ptr_typ = self.types.long_id;
                self.emit(Instruction::binop(
                    Opcode::Mul,
                    offset,
                    idx_extended,
                    elem_size_val,
                    ptr_typ,
                    64,
                ));

                let addr = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Add,
                    addr,
                    arr,
                    offset,
                    ptr_typ,
                    64,
                ));

                self.emit(Instruction::store(
                    final_val,
                    addr,
                    0,
                    target_typ,
                    target_size,
                ));
            }
            _ => {
                // Other lvalues - should not happen for valid C code
            }
        }

        final_val
    }
}
