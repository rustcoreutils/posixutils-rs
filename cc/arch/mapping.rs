//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Architecture-independent instruction mapping interface
//
// This pass runs after SSA construction and before optimization.
// It handles target-specific lowering: expanding unsupported operations
// into sequences of simpler instructions or runtime library calls.
//

use crate::abi::{get_abi_for_conv, ArgClass, CallingConv};
use crate::ir::{CallAbiInfo, Function, Instruction, Module, Opcode, Pseudo, PseudoId};
use crate::rtlib::{Float16Abi, RtlibNames};
use crate::target::{Arch, Os, Target};
use crate::types::{TypeId, TypeKind, TypeTable};

// ============================================================================
// Trait and types
// ============================================================================

/// Context passed to arch mapper. Provides mutable access to the
/// function for pseudo allocation, plus type/target info.
pub struct MappingCtx<'a> {
    pub func: &'a mut Function,
    pub types: &'a TypeTable,
    pub target: &'a Target,
}

/// Result of mapping a single instruction.
pub enum MappedInsn {
    /// Instruction is natively supported — keep unchanged.
    Legal,
    /// Replace with these instructions in the same basic block.
    Replace(Vec<Instruction>),
}

/// Per-architecture instruction mapper.
pub trait ArchMapper {
    /// Map one instruction. The arch impl calls shared helpers
    /// to build replacement IR, then returns it in MappedInsn::Replace.
    fn map_insn(&self, insn: &Instruction, ctx: &mut MappingCtx<'_>) -> MappedInsn;
}

// ============================================================================
// Complex number rtlib name selection
// ============================================================================

/// Get the rtlib function name for complex multiplication.
/// Target-dependent for long double (x87 vs IEEE quad).
pub fn complex_mul_name(base_kind: TypeKind, target: &Target) -> &'static str {
    match base_kind {
        TypeKind::Float => "__mulsc3",
        TypeKind::Double => "__muldc3",
        TypeKind::LongDouble => {
            if target.arch == Arch::Aarch64 && target.os == Os::MacOS {
                "__muldc3" // macOS aarch64: long double == double
            } else {
                match target.arch {
                    Arch::X86_64 => "__mulxc3",
                    Arch::Aarch64 => "__multc3",
                }
            }
        }
        _ => "__muldc3",
    }
}

/// Get the rtlib function name for complex division.
/// Target-dependent for long double (x87 vs IEEE quad).
pub fn complex_div_name(base_kind: TypeKind, target: &Target) -> &'static str {
    match base_kind {
        TypeKind::Float => "__divsc3",
        TypeKind::Double => "__divdc3",
        TypeKind::LongDouble => {
            if target.arch == Arch::Aarch64 && target.os == Os::MacOS {
                "__divdc3"
            } else {
                match target.arch {
                    Arch::X86_64 => "__divxc3",
                    Arch::Aarch64 => "__divtc3",
                }
            }
        }
        _ => "__divdc3",
    }
}

// ============================================================================
// Utility helpers
// ============================================================================

/// Get the rtlib suffix for a float type kind on the given target.
pub(crate) fn float_suffix(kind: TypeKind, target: &Target) -> &'static str {
    match kind {
        TypeKind::Float => "sf",
        TypeKind::Double => "df",
        TypeKind::LongDouble => {
            if target.arch == Arch::X86_64 {
                "xf"
            } else {
                "tf"
            }
        }
        _ => "",
    }
}

/// Check if long double needs soft-float rtlib on this target.
/// Returns true only for aarch64/Linux (128-bit IEEE quad).
/// x86_64 uses native x87; macOS aarch64 long double == double.
pub(crate) fn longdouble_needs_rtlib(target: &Target) -> bool {
    target.arch == Arch::Aarch64 && target.os != Os::MacOS
}

/// Get the integer suffix for a long double↔int conversion.
pub(crate) fn int_suffix_for_longdouble(types: &TypeTable, int_type: TypeId) -> &'static str {
    let size = types.size_bits(int_type);
    let is_unsigned = types.is_unsigned(int_type);
    match (is_unsigned, size <= 32) {
        (true, true) => "usi",
        (true, false) => "udi",
        (false, true) => "si",
        (false, false) => "di",
    }
}

// ============================================================================
// Pseudo/instruction helpers
// ============================================================================

/// Allocate a new 64-bit register pseudo.
pub(crate) fn alloc_reg64(func: &mut Function) -> PseudoId {
    let id = func.alloc_pseudo();
    func.add_pseudo(Pseudo::reg(id, id.0));
    id
}

/// Extract lo and hi 64-bit halves from a 128-bit pseudo.
fn extract_halves(
    func: &mut Function,
    insns: &mut Vec<Instruction>,
    src: PseudoId,
    long_type: TypeId,
) -> (PseudoId, PseudoId) {
    let lo = alloc_reg64(func);
    insns.push(Instruction::unop(Opcode::Lo64, lo, src, long_type, 64));
    let hi = alloc_reg64(func);
    insns.push(Instruction::unop(Opcode::Hi64, hi, src, long_type, 64));
    (lo, hi)
}

// ============================================================================
// Rtlib call builders
// ============================================================================

/// Parameters for building an explicit rtlib call.
pub(crate) struct RtlibCallParams<'a> {
    pub target_pseudo: PseudoId,
    pub arg_vals: &'a [PseudoId],
    pub func_name: &'a str,
    pub arg_types: Vec<TypeId>,
    pub ret_type: TypeId,
    pub pos: Option<crate::diag::Position>,
}

/// Build a runtime library call instruction replacing an IR instruction.
///
/// Creates a Call instruction with proper ABI classification using the
/// C calling convention, mirroring the linearizer's `emit_rtlib_call`.
fn build_rtlib_call(
    insn: &Instruction,
    func_name: &str,
    arg_types: Vec<TypeId>,
    ret_type: TypeId,
    types: &TypeTable,
    target: &Target,
) -> Instruction {
    let target_pseudo = insn.target.expect("insn must have target");
    let ret_size = types.size_bits(ret_type);

    let arg_vals = insn.src.clone();

    let abi = get_abi_for_conv(CallingConv::C, target);
    let param_classes: Vec<_> = arg_types
        .iter()
        .map(|&t| abi.classify_param(t, types))
        .collect();
    let ret_class = abi.classify_return(ret_type, types);
    let call_abi_info = Box::new(CallAbiInfo::new(param_classes, ret_class));

    let mut call_insn = Instruction::call(
        Some(target_pseudo),
        func_name,
        arg_vals,
        arg_types,
        ret_type,
        ret_size,
    );
    call_insn.abi_info = Some(call_abi_info);
    call_insn.pos = insn.pos;
    call_insn
}

/// Build a rtlib call with explicit parameters.
/// Used for expansion patterns where the call target differs from
/// the original instruction's target.
pub(crate) fn build_rtlib_call_explicit(
    params: RtlibCallParams<'_>,
    types: &TypeTable,
    target: &Target,
) -> Instruction {
    let ret_size = types.size_bits(params.ret_type);

    let abi = get_abi_for_conv(CallingConv::C, target);
    let param_classes: Vec<_> = params
        .arg_types
        .iter()
        .map(|&t| abi.classify_param(t, types))
        .collect();
    let ret_class = abi.classify_return(params.ret_type, types);
    let call_abi_info = Box::new(CallAbiInfo::new(param_classes, ret_class));

    let mut call_insn = Instruction::call(
        Some(params.target_pseudo),
        params.func_name,
        params.arg_vals.to_vec(),
        params.arg_types,
        params.ret_type,
        ret_size,
    );
    call_insn.abi_info = Some(call_abi_info);
    call_insn.pos = params.pos;
    call_insn
}

/// Build a rtlib call for a binop (both args same type as result).
pub(crate) fn build_binop_rtlib_call(
    insn: &Instruction,
    func_name: &str,
    types: &TypeTable,
    target: &Target,
) -> Instruction {
    let ret_type = insn.typ.expect("binop must have type");
    let arg_types = vec![ret_type; insn.src.len()];
    build_rtlib_call(insn, func_name, arg_types, ret_type, types, target)
}

/// Build a rtlib call for a conversion (single arg, different src/dst types).
pub(crate) fn build_convert_rtlib_call(
    insn: &Instruction,
    func_name: &str,
    types: &TypeTable,
    target: &Target,
) -> Instruction {
    let ret_type = insn.typ.expect("conversion must have type");
    let src_type = insn.src_typ.expect("conversion must have src_typ");
    let arg_types = vec![src_type];
    build_rtlib_call(insn, func_name, arg_types, ret_type, types, target)
}

/// Build a call to __extendhfsf2 (Float16 → float) with proper ABI.
fn build_f16_extend_call(
    target_pseudo: PseudoId,
    src: PseudoId,
    pos: Option<crate::diag::Position>,
    types: &TypeTable,
    target: &Target,
) -> Instruction {
    let rtlib = RtlibNames::new(target);
    let f16_abi = rtlib.float16_abi();
    let float_type = types.float_id;
    let float_size = types.size_bits(float_type);

    // Arg type: ushort for compiler-rt, Float16 for libgcc
    let arg_type = if f16_abi == Float16Abi::Integer {
        types.ushort_id
    } else {
        types.float16_id
    };

    // Arg classification
    let param_class = if f16_abi == Float16Abi::Integer {
        ArgClass::Extend {
            signed: false,
            size_bits: 16,
        }
    } else {
        let abi = get_abi_for_conv(CallingConv::C, target);
        abi.classify_param(types.float16_id, types)
    };

    // Return is always SSE float
    let abi = get_abi_for_conv(CallingConv::C, target);
    let ret_class = abi.classify_return(float_type, types);

    let call_abi_info = Box::new(CallAbiInfo::new(vec![param_class], ret_class));

    let mut call_insn = Instruction::call(
        Some(target_pseudo),
        "__extendhfsf2",
        vec![src],
        vec![arg_type],
        float_type,
        float_size,
    );
    call_insn.abi_info = Some(call_abi_info);
    call_insn.pos = pos;
    call_insn
}

/// Build a call to __truncsfhf2 (float → Float16) with proper ABI.
fn build_f16_truncate_call(
    target_pseudo: PseudoId,
    src: PseudoId,
    pos: Option<crate::diag::Position>,
    types: &TypeTable,
    target: &Target,
) -> Instruction {
    let rtlib = RtlibNames::new(target);
    let f16_abi = rtlib.float16_abi();
    let float_type = types.float_id;
    let float16_type = types.float16_id;
    let f16_size = types.size_bits(float16_type);

    // Arg is always SSE float
    let abi = get_abi_for_conv(CallingConv::C, target);
    let param_class = abi.classify_param(float_type, types);

    // Return: ushort for compiler-rt, Float16/SSE for libgcc
    let ret_class = if f16_abi == Float16Abi::Integer {
        ArgClass::Extend {
            signed: false,
            size_bits: 16,
        }
    } else {
        abi.classify_return(float16_type, types)
    };

    let call_abi_info = Box::new(CallAbiInfo::new(vec![param_class], ret_class));

    let mut call_insn = Instruction::call(
        Some(target_pseudo),
        "__truncsfhf2",
        vec![src],
        vec![float_type],
        float16_type,
        f16_size,
    );
    call_insn.abi_info = Some(call_abi_info);
    call_insn.pos = pos;
    call_insn
}

// ============================================================================
// Int128 expansion helpers
// ============================================================================

/// Expand int128 bitwise op (And/Or/Xor) into 64-bit operations.
fn expand_int128_bitwise(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let (a_lo, a_hi) = extract_halves(func, &mut insns, insn.src[0], long_type);
    let (b_lo, b_hi) = extract_halves(func, &mut insns, insn.src[1], long_type);

    let r_lo = alloc_reg64(func);
    insns.push(Instruction::binop(insn.op, r_lo, a_lo, b_lo, long_type, 64));
    let r_hi = alloc_reg64(func);
    insns.push(Instruction::binop(insn.op, r_hi, a_hi, b_hi, long_type, 64));

    let int128_type = insn.typ.unwrap();
    insns.push(Instruction::binop(
        Opcode::Pair64,
        result,
        r_lo,
        r_hi,
        int128_type,
        128,
    ));
    insns
}

/// Expand int128 Not into 64-bit operations.
fn expand_int128_not(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let (s_lo, s_hi) = extract_halves(func, &mut insns, insn.src[0], long_type);

    let r_lo = alloc_reg64(func);
    insns.push(Instruction::unop(Opcode::Not, r_lo, s_lo, long_type, 64));
    let r_hi = alloc_reg64(func);
    insns.push(Instruction::unop(Opcode::Not, r_hi, s_hi, long_type, 64));

    let int128_type = insn.typ.unwrap();
    insns.push(Instruction::binop(
        Opcode::Pair64,
        result,
        r_lo,
        r_hi,
        int128_type,
        128,
    ));
    insns
}

/// Expand int128 Neg (0 - value with borrow chain).
fn expand_int128_neg(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let (s_lo, s_hi) = extract_halves(func, &mut insns, insn.src[0], long_type);
    let zero = func.create_const_pseudo(0);

    let r_lo = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::SubC,
        r_lo,
        zero,
        s_lo,
        long_type,
        64,
    ));
    let r_hi = alloc_reg64(func);
    let mut sbc = Instruction::binop(Opcode::SbcC, r_hi, zero, s_hi, long_type, 64);
    sbc.src.push(r_lo);
    insns.push(sbc);

    let int128_type = insn.typ.unwrap();
    insns.push(Instruction::binop(
        Opcode::Pair64,
        result,
        r_lo,
        r_hi,
        int128_type,
        128,
    ));
    insns
}

/// Expand int128 Add (carry chain).
fn expand_int128_add(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let (a_lo, a_hi) = extract_halves(func, &mut insns, insn.src[0], long_type);
    let (b_lo, b_hi) = extract_halves(func, &mut insns, insn.src[1], long_type);

    let r_lo = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::AddC,
        r_lo,
        a_lo,
        b_lo,
        long_type,
        64,
    ));
    let r_hi = alloc_reg64(func);
    let mut adc = Instruction::binop(Opcode::AdcC, r_hi, a_hi, b_hi, long_type, 64);
    adc.src.push(r_lo);
    insns.push(adc);

    let int128_type = insn.typ.unwrap();
    insns.push(Instruction::binop(
        Opcode::Pair64,
        result,
        r_lo,
        r_hi,
        int128_type,
        128,
    ));
    insns
}

/// Expand int128 Sub (borrow chain).
fn expand_int128_sub(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let (a_lo, a_hi) = extract_halves(func, &mut insns, insn.src[0], long_type);
    let (b_lo, b_hi) = extract_halves(func, &mut insns, insn.src[1], long_type);

    let r_lo = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::SubC,
        r_lo,
        a_lo,
        b_lo,
        long_type,
        64,
    ));
    let r_hi = alloc_reg64(func);
    let mut sbc = Instruction::binop(Opcode::SbcC, r_hi, a_hi, b_hi, long_type, 64);
    sbc.src.push(r_lo);
    insns.push(sbc);

    let int128_type = insn.typ.unwrap();
    insns.push(Instruction::binop(
        Opcode::Pair64,
        result,
        r_lo,
        r_hi,
        int128_type,
        128,
    ));
    insns
}

/// Expand int128 Mul (cross-product decomposition).
fn expand_int128_mul(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let (a_lo, a_hi) = extract_halves(func, &mut insns, insn.src[0], long_type);
    let (b_lo, b_hi) = extract_halves(func, &mut insns, insn.src[1], long_type);

    let low_result = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::Mul,
        low_result,
        a_lo,
        b_lo,
        long_type,
        64,
    ));

    let high_part = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::UMulHi,
        high_part,
        a_lo,
        b_lo,
        long_type,
        64,
    ));

    let cross1 = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::Mul,
        cross1,
        a_lo,
        b_hi,
        long_type,
        64,
    ));

    let cross2 = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::Mul,
        cross2,
        a_hi,
        b_lo,
        long_type,
        64,
    ));

    let sum1 = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::Add,
        sum1,
        high_part,
        cross1,
        long_type,
        64,
    ));
    let final_hi = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::Add,
        final_hi,
        sum1,
        cross2,
        long_type,
        64,
    ));

    let int128_type = insn.typ.unwrap();
    insns.push(Instruction::binop(
        Opcode::Pair64,
        result,
        low_result,
        final_hi,
        int128_type,
        128,
    ));
    insns
}

/// Expand int128 equality comparison (SetEq/SetNe).
fn expand_int128_cmp_eq(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let (a_lo, a_hi) = extract_halves(func, &mut insns, insn.src[0], long_type);
    let (b_lo, b_hi) = extract_halves(func, &mut insns, insn.src[1], long_type);

    let xor_lo = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::Xor,
        xor_lo,
        a_lo,
        b_lo,
        long_type,
        64,
    ));
    let xor_hi = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::Xor,
        xor_hi,
        a_hi,
        b_hi,
        long_type,
        64,
    ));
    let or_result = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::Or,
        or_result,
        xor_lo,
        xor_hi,
        long_type,
        64,
    ));

    let zero = func.create_const_pseudo(0);
    insns.push(Instruction::binop(
        insn.op, result, or_result, zero, long_type, 64,
    ));
    insns
}

/// Expand int128 ordered comparison (SetLt/SetLe/SetGt/SetGe/SetB/SetBe/SetA/SetAe).
fn expand_int128_cmp_ord(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let (a_lo, a_hi) = extract_halves(func, &mut insns, insn.src[0], long_type);
    let (b_lo, b_hi) = extract_halves(func, &mut insns, insn.src[1], long_type);

    let hi_eq = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::SetEq,
        hi_eq,
        a_hi,
        b_hi,
        long_type,
        64,
    ));

    let hi_cmp = alloc_reg64(func);
    insns.push(Instruction::binop(
        insn.op, hi_cmp, a_hi, b_hi, long_type, 64,
    ));

    // Low halves always use unsigned compare
    let lo_op = match insn.op {
        Opcode::SetLt | Opcode::SetB => Opcode::SetB,
        Opcode::SetLe | Opcode::SetBe => Opcode::SetBe,
        Opcode::SetGt | Opcode::SetA => Opcode::SetA,
        Opcode::SetGe | Opcode::SetAe => Opcode::SetAe,
        _ => unreachable!(),
    };
    let lo_cmp = alloc_reg64(func);
    insns.push(Instruction::binop(lo_op, lo_cmp, a_lo, b_lo, long_type, 64));

    insns.push(Instruction::select(
        result, hi_eq, lo_cmp, hi_cmp, long_type, 64,
    ));
    insns
}

/// Expand int128 Zext (zero-extend to 128 bits).
fn expand_int128_zext(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let src = insn.src[0];
    let src_size = insn.src_size;

    // Zero-extend src to 64-bit if needed
    let lo = if src_size < 64 {
        let ext = alloc_reg64(func);
        let mut zext_insn = Instruction::unop(Opcode::Zext, ext, src, long_type, 64);
        zext_insn.src_size = src_size;
        insns.push(zext_insn);
        ext
    } else {
        src
    };

    let zero = func.create_const_pseudo(0);
    let int128_type = insn.typ.unwrap();
    insns.push(Instruction::binop(
        Opcode::Pair64,
        result,
        lo,
        zero,
        int128_type,
        128,
    ));
    insns
}

/// Expand int128 Sext (sign-extend to 128 bits).
fn expand_int128_sext(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
) -> Vec<Instruction> {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;
    let mut insns = Vec::new();

    let src = insn.src[0];
    let src_size = insn.src_size;

    // Sign-extend src to 64-bit if needed
    let lo = if src_size < 64 {
        let ext = alloc_reg64(func);
        let mut sext_insn = Instruction::unop(Opcode::Sext, ext, src, long_type, 64);
        sext_insn.src_size = src_size;
        insns.push(sext_insn);
        ext
    } else {
        src
    };

    let shift_amount = func.create_const_pseudo(63);
    let hi = alloc_reg64(func);
    insns.push(Instruction::binop(
        Opcode::Asr,
        hi,
        lo,
        shift_amount,
        long_type,
        64,
    ));
    let int128_type = insn.typ.unwrap();
    insns.push(Instruction::binop(
        Opcode::Pair64,
        result,
        lo,
        hi,
        int128_type,
        128,
    ));
    insns
}

// ============================================================================
// Float16 expansion helpers
// ============================================================================

/// Expand Float16 binary arithmetic (promote-operate-truncate).
pub(crate) fn expand_float16_arith(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
    target: &Target,
) -> Vec<Instruction> {
    let float_type = types.float_id;
    let float_size = types.size_bits(float_type);
    let pos = insn.pos;
    let result = insn.target.expect("binop must have target");
    let left = insn.src[0];
    let right = insn.src[1];
    let mut insns = Vec::new();

    // Extend left to float
    let left_ext = alloc_reg64(func);
    insns.push(build_f16_extend_call(left_ext, left, pos, types, target));

    // Extend right to float
    let right_ext = alloc_reg64(func);
    insns.push(build_f16_extend_call(right_ext, right, pos, types, target));

    // Native float operation
    let float_result = alloc_reg64(func);
    insns.push(Instruction::binop(
        insn.op,
        float_result,
        left_ext,
        right_ext,
        float_type,
        float_size,
    ));

    // Truncate result back to Float16
    insns.push(build_f16_truncate_call(
        result,
        float_result,
        pos,
        types,
        target,
    ));
    insns
}

/// Expand Float16 negation (promote-negate-truncate).
pub(crate) fn expand_float16_neg(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
    target: &Target,
) -> Vec<Instruction> {
    let float_type = types.float_id;
    let float_size = types.size_bits(float_type);
    let pos = insn.pos;
    let result = insn.target.expect("unary must have target");
    let src = insn.src[0];
    let mut insns = Vec::new();

    let src_ext = alloc_reg64(func);
    insns.push(build_f16_extend_call(src_ext, src, pos, types, target));

    let neg_result = alloc_reg64(func);
    insns.push(Instruction::unop(
        Opcode::FNeg,
        neg_result,
        src_ext,
        float_type,
        float_size,
    ));

    insns.push(build_f16_truncate_call(
        result, neg_result, pos, types, target,
    ));
    insns
}

/// Expand Float16 comparison (promote both, compare — no truncate).
pub(crate) fn expand_float16_cmp(
    insn: &Instruction,
    func: &mut Function,
    types: &TypeTable,
    target: &Target,
) -> Vec<Instruction> {
    let float_type = types.float_id;
    let float_size = types.size_bits(float_type);
    let pos = insn.pos;
    let result = insn.target.expect("cmp must have target");
    let left = insn.src[0];
    let right = insn.src[1];
    let mut insns = Vec::new();

    let left_ext = alloc_reg64(func);
    insns.push(build_f16_extend_call(left_ext, left, pos, types, target));

    let right_ext = alloc_reg64(func);
    insns.push(build_f16_extend_call(right_ext, right, pos, types, target));

    // Float comparison — result type is int, keep original type/size
    let mut cmp = Instruction::binop(
        insn.op,
        result,
        left_ext,
        right_ext,
        insn.typ.unwrap_or(types.int_id),
        float_size,
    );
    cmp.src_typ = Some(float_type);
    insns.push(cmp);
    insns
}

// ============================================================================
// Shared mapping decision functions
// ============================================================================

/// Classify and expand an int128 div/mod instruction into a rtlib call.
pub(crate) fn map_int128_divmod(
    insn: &Instruction,
    ctx: &mut MappingCtx<'_>,
) -> Option<MappedInsn> {
    if insn.size != 128 {
        return None;
    }
    let typ = insn.typ?;
    if ctx.types.kind(typ) != TypeKind::Int128 {
        return None;
    }
    let name = match insn.op {
        Opcode::DivS => "__divti3",
        Opcode::DivU => "__udivti3",
        Opcode::ModS => "__modti3",
        Opcode::ModU => "__umodti3",
        _ => return None,
    };
    let call = build_binop_rtlib_call(insn, name, ctx.types, ctx.target);
    Some(MappedInsn::Replace(vec![call]))
}

/// Classify and expand an int128 operation into 64-bit sequences.
pub(crate) fn map_int128_expand(
    insn: &Instruction,
    ctx: &mut MappingCtx<'_>,
) -> Option<MappedInsn> {
    if insn.size != 128 {
        return None;
    }
    let types = ctx.types;

    match insn.op {
        // Arithmetic/bitwise/unary: result type is int128
        Opcode::And | Opcode::Or | Opcode::Xor => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_bitwise(
                insn, ctx.func, types,
            )))
        }
        Opcode::Not => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_not(
                insn, ctx.func, types,
            )))
        }
        Opcode::Neg => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_neg(
                insn, ctx.func, types,
            )))
        }
        Opcode::Add => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_add(
                insn, ctx.func, types,
            )))
        }
        Opcode::Sub => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_sub(
                insn, ctx.func, types,
            )))
        }
        Opcode::Mul => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_mul(
                insn, ctx.func, types,
            )))
        }
        // Extensions to 128: result type is int128
        Opcode::Zext => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_zext(
                insn, ctx.func, types,
            )))
        }
        Opcode::Sext => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_sext(
                insn, ctx.func, types,
            )))
        }
        // Equality comparisons
        Opcode::SetEq | Opcode::SetNe => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_cmp_eq(
                insn, ctx.func, types,
            )))
        }
        // Ordered comparisons
        Opcode::SetLt
        | Opcode::SetLe
        | Opcode::SetGt
        | Opcode::SetGe
        | Opcode::SetB
        | Opcode::SetBe
        | Opcode::SetA
        | Opcode::SetAe => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(MappedInsn::Replace(expand_int128_cmp_ord(
                insn, ctx.func, types,
            )))
        }
        _ => None,
    }
}

/// Classify and expand an int128↔float conversion into a rtlib call.
pub(crate) fn map_int128_float_convert(
    insn: &Instruction,
    ctx: &mut MappingCtx<'_>,
) -> Option<MappedInsn> {
    let types = ctx.types;
    let target = ctx.target;
    match insn.op {
        // int128 → float
        Opcode::SCvtF | Opcode::UCvtF => {
            if insn.src_size != 128 {
                return None;
            }
            let src_typ = insn.src_typ?;
            if types.kind(src_typ) != TypeKind::Int128 {
                return None;
            }
            let dst_typ = insn.typ?;
            let dst_kind = types.kind(dst_typ);
            let fsuf = float_suffix(dst_kind, target);
            if fsuf.is_empty() {
                return None;
            }
            let is_unsigned = insn.op == Opcode::UCvtF;
            let func_name: &'static str = match (is_unsigned, fsuf) {
                (false, "sf") => "__floattisf",
                (false, "df") => "__floattidf",
                (false, "xf") => "__floattixf",
                (false, "tf") => "__floattitf",
                (true, "sf") => "__floatuntisf",
                (true, "df") => "__floatuntidf",
                (true, "xf") => "__floatuntixf",
                (true, "tf") => "__floatuntitf",
                _ => return None,
            };
            let call = build_convert_rtlib_call(insn, func_name, types, target);
            Some(MappedInsn::Replace(vec![call]))
        }
        // float → int128
        Opcode::FCvtS | Opcode::FCvtU => {
            if insn.size != 128 {
                return None;
            }
            let dst_typ = insn.typ?;
            if types.kind(dst_typ) != TypeKind::Int128 {
                return None;
            }
            let src_typ = insn.src_typ?;
            let src_kind = types.kind(src_typ);
            let fsuf = float_suffix(src_kind, target);
            if fsuf.is_empty() {
                return None;
            }
            let is_unsigned = insn.op == Opcode::FCvtU;
            let func_name: &'static str = match (is_unsigned, fsuf) {
                (false, "sf") => "__fixsfti",
                (false, "df") => "__fixdfti",
                (false, "xf") => "__fixxfti",
                (false, "tf") => "__fixtfti",
                (true, "sf") => "__fixunssfti",
                (true, "df") => "__fixunsdfti",
                (true, "xf") => "__fixunsxfti",
                (true, "tf") => "__fixunstfti",
                _ => return None,
            };
            let call = build_convert_rtlib_call(insn, func_name, types, target);
            Some(MappedInsn::Replace(vec![call]))
        }
        _ => None,
    }
}

// ============================================================================
// Pass infrastructure
// ============================================================================

/// Create the appropriate ArchMapper for the given target.
fn create_mapper(target: &Target) -> Box<dyn ArchMapper> {
    match target.arch {
        Arch::X86_64 => Box::new(crate::arch::x86_64::mapping::X86_64Mapper),
        Arch::Aarch64 => Box::new(crate::arch::aarch64::mapping::Aarch64Mapper),
    }
}

/// Run the instruction mapping pass on a single function.
fn map_function(func: &mut Function, types: &TypeTable, target: &Target, mapper: &dyn ArchMapper) {
    for block_idx in 0..func.blocks.len() {
        // Take the insns out of the block to avoid borrow conflicts
        let old_insns = std::mem::take(&mut func.blocks[block_idx].insns);
        let mut new_insns = Vec::with_capacity(old_insns.len());
        let mut changed = false;

        for insn in &old_insns {
            let mut ctx = MappingCtx {
                func: &mut *func,
                types,
                target,
            };
            match mapper.map_insn(insn, &mut ctx) {
                MappedInsn::Legal => new_insns.push(insn.clone()),
                MappedInsn::Replace(replacements) => {
                    new_insns.extend(replacements);
                    changed = true;
                }
            }
        }

        if changed {
            func.blocks[block_idx].insns = new_insns;
        } else {
            func.blocks[block_idx].insns = old_insns;
        }
    }
}

/// Run the instruction mapping pass on an entire module.
pub fn run_mapping(module: &mut Module, types: &TypeTable, target: &Target) {
    let mapper = create_mapper(target);
    for func in &mut module.functions {
        map_function(func, types, target, mapper.as_ref());
    }
}

// ============================================================================
// Shared test helpers
// ============================================================================

#[cfg(test)]
pub(crate) mod test_helpers {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Opcode, Pseudo, PseudoId};
    use crate::types::{TypeId, TypeTable};

    /// Create a function with 3 pseudos for classification tests.
    pub fn make_minimal_func(types: &TypeTable) -> Function {
        let mut func = Function::new("test", types.int_id);
        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::reg(PseudoId(2), 2));
        func.next_pseudo = 3;
        func
    }

    /// Create a function with various legal instructions for pass runner tests.
    pub fn make_test_func(types: &TypeTable) -> Function {
        let mut func = Function::new("test_mapping", types.int_id);

        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::reg(PseudoId(2), 2));

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));

        // Integer arithmetic
        bb.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::binop(
            Opcode::Sub,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::binop(
            Opcode::Mul,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        ));

        // Bitwise
        bb.add_insn(Instruction::binop(
            Opcode::And,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::binop(
            Opcode::Or,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::binop(
            Opcode::Xor,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        ));

        // Comparisons
        bb.add_insn(Instruction::binop(
            Opcode::SetEq,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::binop(
            Opcode::SetLt,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        ));

        // Unary
        bb.add_insn(Instruction::unop(
            Opcode::Neg,
            PseudoId(2),
            PseudoId(0),
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::unop(
            Opcode::Not,
            PseudoId(2),
            PseudoId(0),
            types.int_id,
            32,
        ));

        // Float ops
        bb.add_insn(Instruction::binop(
            Opcode::FAdd,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.double_id,
            64,
        ));
        bb.add_insn(Instruction::unop(
            Opcode::FNeg,
            PseudoId(2),
            PseudoId(0),
            types.double_id,
            64,
        ));

        // Conversions
        let mut sext = Instruction::unop(Opcode::Sext, PseudoId(2), PseudoId(0), types.long_id, 64);
        sext.src_size = 32;
        bb.add_insn(sext);
        let mut zext =
            Instruction::unop(Opcode::Zext, PseudoId(2), PseudoId(0), types.ulong_id, 64);
        zext.src_size = 32;
        bb.add_insn(zext);

        // Memory
        bb.add_insn(Instruction::load(
            PseudoId(2),
            PseudoId(0),
            0,
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::store(
            PseudoId(1),
            PseudoId(0),
            0,
            types.int_id,
            32,
        ));

        // Terminator
        bb.add_insn(Instruction::ret(Some(PseudoId(2))));

        func.add_block(bb);
        func.entry = BasicBlockId(0);
        func
    }

    /// Create a conversion instruction for testing.
    pub fn make_convert_insn(
        op: Opcode,
        dst_type: TypeId,
        dst_size: u32,
        src_type: TypeId,
        src_size: u32,
    ) -> Instruction {
        let mut insn = Instruction::new(op)
            .with_target(PseudoId(2))
            .with_src(PseudoId(0))
            .with_type_and_size(dst_type, dst_size);
        insn.src_size = src_size;
        insn.src_typ = Some(src_type);
        insn
    }

    /// Assert the mapping result is Legal.
    pub fn assert_legal(result: &MappedInsn) {
        assert!(matches!(result, MappedInsn::Legal));
    }

    /// Assert the mapping result is a single LibCall replacement.
    pub fn assert_libcall(result: &MappedInsn, expected_name: &str) {
        match result {
            MappedInsn::Replace(insns) => {
                assert_eq!(insns.len(), 1, "expected single Call replacement");
                assert_eq!(insns[0].op, Opcode::Call);
                assert_eq!(insns[0].func_name.as_deref(), Some(expected_name));
            }
            MappedInsn::Legal => {
                panic!("expected Replace with LibCall to {expected_name}, got Legal")
            }
        }
    }

    /// Assert the mapping result is a multi-instruction expansion.
    pub fn assert_expand(result: &MappedInsn) {
        match result {
            MappedInsn::Replace(insns) => {
                assert!(!insns.is_empty(), "expected non-empty expansion");
            }
            MappedInsn::Legal => panic!("expected Replace with expansion, got Legal"),
        }
    }

    /// Assert the mapping result is a CmpLibCall (call + int compare).
    pub fn assert_cmp_libcall(result: &MappedInsn, expected_name: &str, expected_cmp_op: Opcode) {
        match result {
            MappedInsn::Replace(insns) => {
                assert_eq!(insns.len(), 2, "expected Call + compare");
                assert_eq!(insns[0].op, Opcode::Call);
                assert_eq!(insns[0].func_name.as_deref(), Some(expected_name));
                assert_eq!(insns[1].op, expected_cmp_op);
            }
            MappedInsn::Legal => {
                panic!("expected Replace with CmpLibCall to {expected_name}, got Legal")
            }
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::test_helpers::*;
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Opcode, Pseudo, PseudoId};
    use crate::target::{Arch, Os, Target};
    use crate::types::TypeTable;

    // ========================================================================
    // Pass runner tests
    // ========================================================================

    #[test]
    fn test_run_mapping_empty() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mut module = Module::new();

        run_mapping(&mut module, &types, &target);
    }

    #[test]
    fn test_run_mapping_with_functions() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);

        let mut module = Module::new();
        module.add_function(make_test_func(&types));
        module.add_function(make_test_func(&types));

        run_mapping(&mut module, &types, &target);
    }

    #[test]
    fn test_mapping_idempotent() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);

        let mut module = Module::new();
        module.add_function(make_test_func(&types));

        run_mapping(&mut module, &types, &target);
        run_mapping(&mut module, &types, &target);
    }

    #[test]
    fn test_mapping_all_targets() {
        let targets = vec![
            Target::new(Arch::X86_64, Os::Linux),
            Target::new(Arch::X86_64, Os::MacOS),
            Target::new(Arch::X86_64, Os::FreeBSD),
            Target::new(Arch::Aarch64, Os::Linux),
            Target::new(Arch::Aarch64, Os::MacOS),
        ];

        for target in &targets {
            let types = TypeTable::new(target);
            let mut module = Module::new();
            module.add_function(make_test_func(&types));
            run_mapping(&mut module, &types, target);
        }
    }

    #[test]
    fn test_mapping_all_legal_x86_64() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);

        let mut module = Module::new();
        module.add_function(make_test_func(&types));
        let orig_insn_count = module.functions[0].blocks[0].insns.len();

        run_mapping(&mut module, &types, &target);

        // All instructions should be unchanged (all legal)
        assert_eq!(module.functions[0].blocks[0].insns.len(), orig_insn_count);
    }

    #[test]
    fn test_mapping_all_legal_aarch64() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);

        let mut module = Module::new();
        module.add_function(make_test_func(&types));
        let orig_insn_count = module.functions[0].blocks[0].insns.len();

        run_mapping(&mut module, &types, &target);

        assert_eq!(module.functions[0].blocks[0].insns.len(), orig_insn_count);
    }

    // ========================================================================
    // Integration: int128 div/mod transformation
    // ========================================================================

    #[test]
    fn test_mapping_transforms_int128_divmod() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);

        let mut func = Function::new("test_divmod", types.int128_id);
        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::reg(PseudoId(2), 2));
        func.next_pseudo = 3;

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(Instruction::binop(
            Opcode::DivS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int128_id,
            128,
        ));
        bb.add_insn(Instruction::ret(Some(PseudoId(2))));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        let mut module = Module::new();
        module.add_function(func);
        run_mapping(&mut module, &types, &target);

        // After mapping, the DivS should be replaced with a Call to __divti3
        let block = &module.functions[0].blocks[0];
        assert_eq!(block.insns.len(), 3); // Entry, Call, Ret
        assert_eq!(block.insns[1].op, Opcode::Call);
        assert_eq!(block.insns[1].func_name.as_deref(), Some("__divti3"));
        assert_eq!(block.insns[1].target, Some(PseudoId(2)));
        assert_eq!(block.insns[1].src, vec![PseudoId(0), PseudoId(1)]);
        assert!(block.insns[1].abi_info.is_some());
    }

    // ========================================================================
    // Integration: int128↔float conversion transformation
    // ========================================================================

    #[test]
    fn test_mapping_transforms_int128_conversion() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);

        let mut func = Function::new("test_convert", types.double_id);
        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.next_pseudo = 2;

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(make_convert_insn(
            Opcode::SCvtF,
            types.double_id,
            64,
            types.int128_id,
            128,
        ));
        bb.add_insn(Instruction::ret(Some(PseudoId(1))));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        let mut module = Module::new();
        module.add_function(func);
        run_mapping(&mut module, &types, &target);

        let block = &module.functions[0].blocks[0];
        assert_eq!(block.insns.len(), 3);
        assert_eq!(block.insns[1].op, Opcode::Call);
        assert_eq!(block.insns[1].func_name.as_deref(), Some("__floattidf"));
        assert!(block.insns[1].abi_info.is_some());
    }

    // ========================================================================
    // Complex mul/div rtlib name tests
    // ========================================================================

    #[test]
    fn test_complex_mul_name_float() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        assert_eq!(complex_mul_name(TypeKind::Float, &target), "__mulsc3");
    }

    #[test]
    fn test_complex_mul_name_double() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        assert_eq!(complex_mul_name(TypeKind::Double, &target), "__muldc3");
    }

    #[test]
    fn test_complex_mul_name_longdouble() {
        let x86 = Target::new(Arch::X86_64, Os::Linux);
        assert_eq!(complex_mul_name(TypeKind::LongDouble, &x86), "__mulxc3");

        let arm_linux = Target::new(Arch::Aarch64, Os::Linux);
        assert_eq!(
            complex_mul_name(TypeKind::LongDouble, &arm_linux),
            "__multc3"
        );

        let arm_macos = Target::new(Arch::Aarch64, Os::MacOS);
        assert_eq!(
            complex_mul_name(TypeKind::LongDouble, &arm_macos),
            "__muldc3"
        );
    }

    #[test]
    fn test_complex_div_name_float() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        assert_eq!(complex_div_name(TypeKind::Float, &target), "__divsc3");
    }

    #[test]
    fn test_complex_div_name_longdouble() {
        let x86 = Target::new(Arch::X86_64, Os::Linux);
        assert_eq!(complex_div_name(TypeKind::LongDouble, &x86), "__divxc3");

        let arm_linux = Target::new(Arch::Aarch64, Os::Linux);
        assert_eq!(
            complex_div_name(TypeKind::LongDouble, &arm_linux),
            "__divtc3"
        );

        let arm_macos = Target::new(Arch::Aarch64, Os::MacOS);
        assert_eq!(
            complex_div_name(TypeKind::LongDouble, &arm_macos),
            "__divdc3"
        );
    }
}
