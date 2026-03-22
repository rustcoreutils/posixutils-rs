//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Hardware Mapping Pass for pcc C99 compiler
//
// This pass runs after SSA construction and before optimization.
// It centralizes decisions about how each IR instruction maps to hardware:
//   - Legal: instruction is directly supported by the target
//   - LibCall: instruction must be lowered to a runtime library call
//   - Expand: instruction must be expanded into multiple simpler instructions
//

use crate::abi::{get_abi_for_conv, ArgClass, CallingConv};
use crate::ir::{CallAbiInfo, Function, Instruction, Module, Opcode, Pseudo, PseudoId};
use crate::rtlib::{Float16Abi, RtlibNames};
use crate::target::{Arch, Os, Target};
use crate::types::{TypeId, TypeKind, TypeTable};

/// Action the hwmap pass should take for a given instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HwMapAction {
    /// Instruction is directly supported by the target hardware.
    Legal,
    /// Instruction must be replaced with a call to a runtime library function.
    LibCall(&'static str),
    /// Long double comparison: call rtlib, then compare result against 0.
    /// Contains (rtlib_func_name, int_compare_opcode).
    CmpLibCall(&'static str, Opcode),
    /// Expand into multiple instructions (promote-operate-truncate, etc.).
    Expand,
}

/// Classify an int128 div/mod instruction into a LibCall action.
fn map_int128_divmod(insn: &Instruction, types: &TypeTable) -> Option<HwMapAction> {
    if insn.size != 128 {
        return None;
    }
    let typ = insn.typ?;
    if types.kind(typ) != TypeKind::Int128 {
        return None;
    }
    match insn.op {
        Opcode::DivS => Some(HwMapAction::LibCall("__divti3")),
        Opcode::DivU => Some(HwMapAction::LibCall("__udivti3")),
        Opcode::ModS => Some(HwMapAction::LibCall("__modti3")),
        Opcode::ModU => Some(HwMapAction::LibCall("__umodti3")),
        _ => None,
    }
}

/// Get the rtlib suffix for a float type kind on the given target.
fn float_suffix(kind: TypeKind, target: &Target) -> &'static str {
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

/// Classify an int128↔float conversion instruction into a LibCall action.
///
/// Handles:
///   - SCvtF/UCvtF with src_size==128: int128 → float (e.g. __floattisf)
///   - FCvtS/FCvtU with size==128: float → int128 (e.g. __fixsfti)
fn map_int128_float_convert(
    insn: &Instruction,
    types: &TypeTable,
    target: &Target,
) -> Option<HwMapAction> {
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
            Some(HwMapAction::LibCall(func_name))
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
            Some(HwMapAction::LibCall(func_name))
        }
        _ => None,
    }
}

/// Check if long double needs soft-float rtlib on this target.
/// Returns true only for aarch64/Linux (128-bit IEEE quad).
/// x86_64 uses native x87; macOS aarch64 long double == double.
fn longdouble_needs_rtlib(target: &Target) -> bool {
    target.arch == Arch::Aarch64 && target.os != Os::MacOS
}

/// Classify a long double binary op (FAdd/FSub/FMul/FDiv) into a LibCall.
fn map_longdouble_binop(
    insn: &Instruction,
    types: &TypeTable,
    target: &Target,
) -> Option<HwMapAction> {
    if !longdouble_needs_rtlib(target) {
        return None;
    }
    let typ = insn.typ?;
    if types.kind(typ) != TypeKind::LongDouble {
        return None;
    }
    match insn.op {
        Opcode::FAdd => Some(HwMapAction::LibCall("__addtf3")),
        Opcode::FSub => Some(HwMapAction::LibCall("__subtf3")),
        Opcode::FMul => Some(HwMapAction::LibCall("__multf3")),
        Opcode::FDiv => Some(HwMapAction::LibCall("__divtf3")),
        _ => None,
    }
}

/// Classify a long double negation (FNeg) into a LibCall.
fn map_longdouble_neg(
    insn: &Instruction,
    types: &TypeTable,
    target: &Target,
) -> Option<HwMapAction> {
    if !longdouble_needs_rtlib(target) {
        return None;
    }
    if insn.op != Opcode::FNeg {
        return None;
    }
    let typ = insn.typ?;
    if types.kind(typ) != TypeKind::LongDouble {
        return None;
    }
    Some(HwMapAction::LibCall("__negtf2"))
}

/// Classify a long double comparison (FCmpO*) into a CmpLibCall.
/// The rtlib cmp function returns int; caller must compare vs 0.
fn map_longdouble_cmp(
    insn: &Instruction,
    types: &TypeTable,
    target: &Target,
) -> Option<HwMapAction> {
    if !longdouble_needs_rtlib(target) {
        return None;
    }
    // FCmpO* instructions don't store the operand type in insn.typ (that's the
    // result type, which is int). Check src_typ or fall back to size check.
    // The comparison has size == size of the operands being compared.
    // For long double on aarch64/Linux, size == 128.
    if insn.size != 128 {
        return None;
    }
    // Also check src_typ if available
    if let Some(src_typ) = insn.src_typ {
        if types.kind(src_typ) != TypeKind::LongDouble {
            return None;
        }
    }
    match insn.op {
        Opcode::FCmpOLt => Some(HwMapAction::CmpLibCall("__lttf2", Opcode::SetLt)),
        Opcode::FCmpOLe => Some(HwMapAction::CmpLibCall("__letf2", Opcode::SetLe)),
        Opcode::FCmpOGt => Some(HwMapAction::CmpLibCall("__gttf2", Opcode::SetGt)),
        Opcode::FCmpOGe => Some(HwMapAction::CmpLibCall("__getf2", Opcode::SetGe)),
        Opcode::FCmpOEq => Some(HwMapAction::CmpLibCall("__eqtf2", Opcode::SetEq)),
        Opcode::FCmpONe => Some(HwMapAction::CmpLibCall("__netf2", Opcode::SetNe)),
        _ => None,
    }
}

/// Get the integer suffix for a long double↔int conversion.
fn int_suffix_for_longdouble(types: &TypeTable, int_type: TypeId) -> &'static str {
    let size = types.size_bits(int_type);
    let is_unsigned = types.is_unsigned(int_type);
    match (is_unsigned, size <= 32) {
        (true, true) => "usi",
        (true, false) => "udi",
        (false, true) => "si",
        (false, false) => "di",
    }
}

/// Classify a long double conversion into a LibCall.
fn map_longdouble_convert(
    insn: &Instruction,
    types: &TypeTable,
    target: &Target,
) -> Option<HwMapAction> {
    if !longdouble_needs_rtlib(target) {
        return None;
    }
    match insn.op {
        // Float-to-float: longdouble ↔ float/double
        Opcode::FCvtF => {
            let dst_typ = insn.typ?;
            let src_typ = insn.src_typ?;
            let dst_kind = types.kind(dst_typ);
            let src_kind = types.kind(src_typ);
            if src_kind == TypeKind::LongDouble {
                // longdouble → float/double
                match dst_kind {
                    TypeKind::Float => Some(HwMapAction::LibCall("__trunctfsf2")),
                    TypeKind::Double => Some(HwMapAction::LibCall("__trunctfdf2")),
                    _ => None,
                }
            } else if dst_kind == TypeKind::LongDouble {
                // float/double → longdouble
                match src_kind {
                    TypeKind::Float => Some(HwMapAction::LibCall("__extendsftf2")),
                    TypeKind::Double => Some(HwMapAction::LibCall("__extenddftf2")),
                    _ => None,
                }
            } else {
                None
            }
        }
        // Int-to-float: int → longdouble
        Opcode::SCvtF | Opcode::UCvtF => {
            let dst_typ = insn.typ?;
            if types.kind(dst_typ) != TypeKind::LongDouble {
                return None;
            }
            let src_typ = insn.src_typ?;
            // Skip int128 (handled by map_int128_float_convert)
            if types.kind(src_typ) == TypeKind::Int128 {
                return None;
            }
            let isuf = int_suffix_for_longdouble(types, src_typ);
            let func_name: &'static str = match isuf {
                "si" => "__floatsitf",
                "di" => "__floatditf",
                "usi" => "__floatunsitf",
                "udi" => "__floatunditf",
                _ => return None,
            };
            Some(HwMapAction::LibCall(func_name))
        }
        // Float-to-int: longdouble → int
        Opcode::FCvtS | Opcode::FCvtU => {
            let src_typ = insn.src_typ?;
            if types.kind(src_typ) != TypeKind::LongDouble {
                return None;
            }
            let dst_typ = insn.typ?;
            // Skip int128 (handled by map_int128_float_convert)
            if types.kind(dst_typ) == TypeKind::Int128 {
                return None;
            }
            let isuf = int_suffix_for_longdouble(types, dst_typ);
            let func_name: &'static str = match isuf {
                "si" => "__fixtfsi",
                "di" => "__fixtfdi",
                "usi" => "__fixunstfsi",
                "udi" => "__fixunstfdi",
                _ => return None,
            };
            Some(HwMapAction::LibCall(func_name))
        }
        _ => None,
    }
}

/// Classify an int128 operation that needs inline expansion.
fn map_int128_expand(insn: &Instruction, types: &TypeTable) -> Option<HwMapAction> {
    if insn.size != 128 {
        return None;
    }

    match insn.op {
        // Arithmetic/bitwise/unary: result type is int128
        Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::Neg
        | Opcode::Not
        | Opcode::Add
        | Opcode::Sub
        | Opcode::Mul => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(HwMapAction::Expand)
        }
        // Zext/Sext to 128: result type is int128
        Opcode::Zext | Opcode::Sext => {
            let typ = insn.typ?;
            if types.kind(typ) != TypeKind::Int128 {
                return None;
            }
            Some(HwMapAction::Expand)
        }
        // Comparisons: size==128 is the operand size, result is int/bool
        Opcode::SetEq
        | Opcode::SetNe
        | Opcode::SetLt
        | Opcode::SetLe
        | Opcode::SetGt
        | Opcode::SetGe
        | Opcode::SetB
        | Opcode::SetBe
        | Opcode::SetA
        | Opcode::SetAe => Some(HwMapAction::Expand),
        _ => None,
    }
}

/// Common hardware mapping logic shared by all targets.
fn map_common(insn: &Instruction, types: &TypeTable, target: &Target) -> Option<HwMapAction> {
    if let Some(action) = map_int128_divmod(insn, types) {
        return Some(action);
    }
    if let Some(action) = map_int128_expand(insn, types) {
        return Some(action);
    }
    if let Some(action) = map_int128_float_convert(insn, types, target) {
        return Some(action);
    }
    if let Some(action) = map_longdouble_binop(insn, types, target) {
        return Some(action);
    }
    if let Some(action) = map_longdouble_neg(insn, types, target) {
        return Some(action);
    }
    if let Some(action) = map_longdouble_cmp(insn, types, target) {
        return Some(action);
    }
    map_longdouble_convert(insn, types, target)
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

/// Trait for target-specific hardware mapping decisions.
pub trait TargetHwMap {
    /// Determine how the target handles a given instruction.
    fn map_op(&self, insn: &Instruction, types: &TypeTable) -> HwMapAction;
}

/// Classify a Float16 instruction that needs soft-float expansion on x86-64.
/// Returns Expand for arithmetic/neg (promote-operate-truncate) and
/// comparisons (promote-compare). Returns LibCall for conversions.
fn map_float16_softfloat(insn: &Instruction, types: &TypeTable) -> Option<HwMapAction> {
    match insn.op {
        // Arithmetic: promote-operate-truncate
        Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
            let typ = insn.typ?;
            if types.kind(typ) == TypeKind::Float16 {
                return Some(HwMapAction::Expand);
            }
            None
        }
        // Negation: promote-negate-truncate
        Opcode::FNeg => {
            let typ = insn.typ?;
            if types.kind(typ) == TypeKind::Float16 {
                return Some(HwMapAction::Expand);
            }
            None
        }
        // Comparisons: promote both, compare (no truncate)
        Opcode::FCmpOEq
        | Opcode::FCmpONe
        | Opcode::FCmpOLt
        | Opcode::FCmpOLe
        | Opcode::FCmpOGt
        | Opcode::FCmpOGe => {
            // Comparisons store the operand type in src_typ or check size
            if let Some(src_typ) = insn.src_typ {
                if types.kind(src_typ) == TypeKind::Float16 {
                    return Some(HwMapAction::Expand);
                }
            }
            // Also check by size: Float16 operations have size==16
            if insn.size == 16 {
                if let Some(typ) = insn.typ {
                    // Result type is int, but check if this is a float comparison
                    if matches!(
                        insn.op,
                        Opcode::FCmpOEq
                            | Opcode::FCmpONe
                            | Opcode::FCmpOLt
                            | Opcode::FCmpOLe
                            | Opcode::FCmpOGt
                            | Opcode::FCmpOGe
                    ) {
                        let _ = typ;
                        return Some(HwMapAction::Expand);
                    }
                }
            }
            None
        }
        // Float16 conversions via rtlib
        Opcode::FCvtF => {
            let dst_typ = insn.typ?;
            let src_typ = insn.src_typ?;
            let dst_kind = types.kind(dst_typ);
            let src_kind = types.kind(src_typ);
            if src_kind == TypeKind::Float16 || dst_kind == TypeKind::Float16 {
                // These are handled by the linearizer's Float16 conversion code
                // which already emits the correct calls. No hwmap action needed
                // because the linearizer emits Call instructions directly.
            }
            None
        }
        _ => None,
    }
}

/// x86-64 hardware mapping.
pub struct X86_64HwMap {
    target: Target,
}

impl TargetHwMap for X86_64HwMap {
    fn map_op(&self, insn: &Instruction, types: &TypeTable) -> HwMapAction {
        if let Some(action) = map_common(insn, types, &self.target) {
            return action;
        }
        if let Some(action) = map_float16_softfloat(insn, types) {
            return action;
        }
        HwMapAction::Legal
    }
}

/// AArch64 hardware mapping.
pub struct Aarch64HwMap {
    target: Target,
}

impl TargetHwMap for Aarch64HwMap {
    fn map_op(&self, insn: &Instruction, types: &TypeTable) -> HwMapAction {
        if let Some(action) = map_common(insn, types, &self.target) {
            return action;
        }
        HwMapAction::Legal
    }
}

/// Get the appropriate TargetHwMap implementation for the given target.
fn get_target_hwmap(target: &Target) -> Box<dyn TargetHwMap> {
    match target.arch {
        Arch::X86_64 => Box::new(X86_64HwMap {
            target: target.clone(),
        }),
        Arch::Aarch64 => Box::new(Aarch64HwMap {
            target: target.clone(),
        }),
    }
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

/// Parameters for building an explicit rtlib call.
struct RtlibCallParams<'a> {
    target_pseudo: PseudoId,
    arg_vals: &'a [PseudoId],
    func_name: &'a str,
    arg_types: Vec<TypeId>,
    ret_type: TypeId,
    pos: Option<crate::diag::Position>,
}

/// Build a rtlib call with explicit parameters.
/// Used for expansion patterns where the call target differs from
/// the original instruction's target.
fn build_rtlib_call_explicit(
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
fn build_binop_rtlib_call(
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
fn build_convert_rtlib_call(
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

/// Dispatch Expand actions to the appropriate expansion function.
fn expand_insn(
    insn: &Instruction,
    func: &mut Function,
    new_insns: &mut Vec<Instruction>,
    types: &TypeTable,
    target: &Target,
) {
    if insn.size == 128 {
        if let Some(typ) = insn.typ {
            if types.kind(typ) == TypeKind::Int128 {
                expand_int128(insn, func, new_insns, types);
                return;
            }
        }
    }
    expand_float16(insn, func, new_insns, types, target);
}

/// Allocate a new 64-bit register pseudo.
fn alloc_reg64(func: &mut Function) -> PseudoId {
    let id = func.alloc_pseudo();
    func.add_pseudo(Pseudo::reg(id, id.0));
    id
}

/// Expand an int128 instruction into 64-bit operations using Lo64/Hi64/Pair64.
fn expand_int128(
    insn: &Instruction,
    func: &mut Function,
    new_insns: &mut Vec<Instruction>,
    types: &TypeTable,
) {
    let result = insn.target.expect("int128 op must have target");
    let long_type = types.ulong_id;

    match insn.op {
        // Bitwise: Lo64+Hi64 both operands, 64-bit op on each half, Pair64
        Opcode::And | Opcode::Or | Opcode::Xor => {
            let src1 = insn.src[0];
            let src2 = insn.src[1];

            let a_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, a_lo, src1, long_type, 64));

            let a_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, a_hi, src1, long_type, 64));

            let b_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, b_lo, src2, long_type, 64));

            let b_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, b_hi, src2, long_type, 64));

            // 64-bit op on lo halves
            let r_lo = alloc_reg64(func);
            new_insns.push(Instruction::binop(insn.op, r_lo, a_lo, b_lo, long_type, 64));

            // 64-bit op on hi halves
            let r_hi = alloc_reg64(func);
            new_insns.push(Instruction::binop(insn.op, r_hi, a_hi, b_hi, long_type, 64));

            // Combine into 128-bit result
            let int128_type = insn.typ.unwrap();
            new_insns.push(Instruction::binop(
                Opcode::Pair64,
                result,
                r_lo,
                r_hi,
                int128_type,
                128,
            ));
        }

        // Not: Lo64+Hi64, Not each, Pair64
        Opcode::Not => {
            let src = insn.src[0];
            let s_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, s_lo, src, long_type, 64));
            let s_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, s_hi, src, long_type, 64));

            let r_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Not, r_lo, s_lo, long_type, 64));
            let r_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Not, r_hi, s_hi, long_type, 64));

            let int128_type = insn.typ.unwrap();
            new_insns.push(Instruction::binop(
                Opcode::Pair64,
                result,
                r_lo,
                r_hi,
                int128_type,
                128,
            ));
        }

        // Neg: SubC(0, lo), SbcC(0, hi, carry), Pair64
        Opcode::Neg => {
            let src = insn.src[0];
            let s_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, s_lo, src, long_type, 64));
            let s_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, s_hi, src, long_type, 64));

            let zero = func.create_const_pseudo(0);

            let r_lo = alloc_reg64(func);
            func.add_pseudo(Pseudo::reg(r_lo, r_lo.0));
            new_insns.push(Instruction::binop(
                Opcode::SubC,
                r_lo,
                zero,
                s_lo,
                long_type,
                64,
            ));

            let r_hi = alloc_reg64(func);
            func.add_pseudo(Pseudo::reg(r_hi, r_hi.0));
            let mut sbc = Instruction::binop(Opcode::SbcC, r_hi, zero, s_hi, long_type, 64);
            sbc.src.push(r_lo); // src[2] = borrow producer
            new_insns.push(sbc);

            let int128_type = insn.typ.unwrap();
            new_insns.push(Instruction::binop(
                Opcode::Pair64,
                result,
                r_lo,
                r_hi,
                int128_type,
                128,
            ));
        }

        // Add: AddC(lo,lo), AdcC(hi,hi,carry), Pair64
        Opcode::Add => {
            let src1 = insn.src[0];
            let src2 = insn.src[1];

            let a_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, a_lo, src1, long_type, 64));
            let a_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, a_hi, src1, long_type, 64));
            let b_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, b_lo, src2, long_type, 64));
            let b_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, b_hi, src2, long_type, 64));

            let r_lo = alloc_reg64(func);
            func.add_pseudo(Pseudo::reg(r_lo, r_lo.0));
            new_insns.push(Instruction::binop(
                Opcode::AddC,
                r_lo,
                a_lo,
                b_lo,
                long_type,
                64,
            ));

            let r_hi = alloc_reg64(func);
            func.add_pseudo(Pseudo::reg(r_hi, r_hi.0));
            let mut adc = Instruction::binop(Opcode::AdcC, r_hi, a_hi, b_hi, long_type, 64);
            adc.src.push(r_lo); // src[2] = carry producer
            new_insns.push(adc);

            let int128_type = insn.typ.unwrap();
            new_insns.push(Instruction::binop(
                Opcode::Pair64,
                result,
                r_lo,
                r_hi,
                int128_type,
                128,
            ));
        }

        // Sub: SubC(lo,lo), SbcC(hi,hi,borrow), Pair64
        Opcode::Sub => {
            let src1 = insn.src[0];
            let src2 = insn.src[1];

            let a_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, a_lo, src1, long_type, 64));
            let a_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, a_hi, src1, long_type, 64));
            let b_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, b_lo, src2, long_type, 64));
            let b_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, b_hi, src2, long_type, 64));

            let r_lo = alloc_reg64(func);
            func.add_pseudo(Pseudo::reg(r_lo, r_lo.0));
            new_insns.push(Instruction::binop(
                Opcode::SubC,
                r_lo,
                a_lo,
                b_lo,
                long_type,
                64,
            ));

            let r_hi = alloc_reg64(func);
            func.add_pseudo(Pseudo::reg(r_hi, r_hi.0));
            let mut sbc = Instruction::binop(Opcode::SbcC, r_hi, a_hi, b_hi, long_type, 64);
            sbc.src.push(r_lo); // src[2] = borrow producer
            new_insns.push(sbc);

            let int128_type = insn.typ.unwrap();
            new_insns.push(Instruction::binop(
                Opcode::Pair64,
                result,
                r_lo,
                r_hi,
                int128_type,
                128,
            ));
        }

        // Mul: a*b = (a_lo*b_lo) + ((a_lo*b_hi + a_hi*b_lo) << 64)
        Opcode::Mul => {
            let src1 = insn.src[0];
            let src2 = insn.src[1];

            let a_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, a_lo, src1, long_type, 64));
            let a_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, a_hi, src1, long_type, 64));
            let b_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, b_lo, src2, long_type, 64));
            let b_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, b_hi, src2, long_type, 64));

            // low_result = a_lo * b_lo (lower 64 bits)
            let low_result = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::Mul,
                low_result,
                a_lo,
                b_lo,
                long_type,
                64,
            ));

            // high_part = umulhi(a_lo, b_lo) (upper 64 bits of full 128-bit product)
            let high_part = alloc_reg64(func);
            func.add_pseudo(Pseudo::reg(high_part, high_part.0));
            new_insns.push(Instruction::binop(
                Opcode::UMulHi,
                high_part,
                a_lo,
                b_lo,
                long_type,
                64,
            ));

            // cross1 = a_lo * b_hi
            let cross1 = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::Mul,
                cross1,
                a_lo,
                b_hi,
                long_type,
                64,
            ));

            // cross2 = a_hi * b_lo
            let cross2 = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::Mul,
                cross2,
                a_hi,
                b_lo,
                long_type,
                64,
            ));

            // final_hi = high_part + cross1 + cross2
            let sum1 = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::Add,
                sum1,
                high_part,
                cross1,
                long_type,
                64,
            ));
            let final_hi = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::Add,
                final_hi,
                sum1,
                cross2,
                long_type,
                64,
            ));

            let int128_type = insn.typ.unwrap();
            new_insns.push(Instruction::binop(
                Opcode::Pair64,
                result,
                low_result,
                final_hi,
                int128_type,
                128,
            ));
        }

        // Eq/Ne: xor lo halves, xor hi halves, or results, compare to 0
        Opcode::SetEq | Opcode::SetNe => {
            let src1 = insn.src[0];
            let src2 = insn.src[1];

            let a_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, a_lo, src1, long_type, 64));
            let a_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, a_hi, src1, long_type, 64));
            let b_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, b_lo, src2, long_type, 64));
            let b_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, b_hi, src2, long_type, 64));

            let xor_lo = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::Xor,
                xor_lo,
                a_lo,
                b_lo,
                long_type,
                64,
            ));
            let xor_hi = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::Xor,
                xor_hi,
                a_hi,
                b_hi,
                long_type,
                64,
            ));
            let or_result = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::Or,
                or_result,
                xor_lo,
                xor_hi,
                long_type,
                64,
            ));

            let zero = func.create_const_pseudo(0);
            // Final comparison is 64-bit (comparing reduced or-result against 0)
            new_insns.push(Instruction::binop(
                insn.op, result, or_result, zero, long_type, 64,
            ));
        }

        // Ordered comparisons: compare hi halves, if equal compare lo halves (unsigned)
        Opcode::SetLt
        | Opcode::SetLe
        | Opcode::SetGt
        | Opcode::SetGe
        | Opcode::SetB
        | Opcode::SetBe
        | Opcode::SetA
        | Opcode::SetAe => {
            let src1 = insn.src[0];
            let src2 = insn.src[1];

            let a_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, a_lo, src1, long_type, 64));
            let a_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, a_hi, src1, long_type, 64));
            let b_lo = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Lo64, b_lo, src2, long_type, 64));
            let b_hi = alloc_reg64(func);
            new_insns.push(Instruction::unop(Opcode::Hi64, b_hi, src2, long_type, 64));

            // All decomposed comparisons are 64-bit
            // hi_eq = (a_hi == b_hi)
            let hi_eq = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::SetEq,
                hi_eq,
                a_hi,
                b_hi,
                long_type,
                64,
            ));

            // hi_cmp = signed/unsigned comparison on hi halves (original op)
            let hi_cmp = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                insn.op, hi_cmp, a_hi, b_hi, long_type, 64,
            ));

            // lo_cmp = UNSIGNED comparison on lo halves
            let lo_op = match insn.op {
                Opcode::SetLt | Opcode::SetB => Opcode::SetB,
                Opcode::SetLe | Opcode::SetBe => Opcode::SetBe,
                Opcode::SetGt | Opcode::SetA => Opcode::SetA,
                Opcode::SetGe | Opcode::SetAe => Opcode::SetAe,
                _ => unreachable!(),
            };
            let lo_cmp = alloc_reg64(func);
            new_insns.push(Instruction::binop(lo_op, lo_cmp, a_lo, b_lo, long_type, 64));

            // result = hi_eq ? lo_cmp : hi_cmp
            new_insns.push(Instruction::select(
                result, hi_eq, lo_cmp, hi_cmp, long_type, 64,
            ));
        }

        // Zext to 128: zero-extend src to 64-bit, Pair64(lo, 0)
        Opcode::Zext => {
            let src = insn.src[0];
            let src_size = insn.src_size;

            // Zero-extend src to 64-bit if needed
            let lo = if src_size < 64 {
                let ext = alloc_reg64(func);
                let mut zext_insn = Instruction::unop(Opcode::Zext, ext, src, long_type, 64);
                zext_insn.src_size = src_size;
                new_insns.push(zext_insn);
                ext
            } else {
                src
            };

            let zero = func.create_const_pseudo(0);
            let int128_type = insn.typ.unwrap();
            new_insns.push(Instruction::binop(
                Opcode::Pair64,
                result,
                lo,
                zero,
                int128_type,
                128,
            ));
        }

        // Sext to 128: first sext src to 64-bit, then hi = Asr(lo, 63)
        Opcode::Sext => {
            let src = insn.src[0];
            let src_size = insn.src_size;

            // Sign-extend src to 64-bit if needed
            let lo = if src_size < 64 {
                let ext = alloc_reg64(func);
                let mut sext_insn = Instruction::unop(Opcode::Sext, ext, src, long_type, 64);
                sext_insn.src_size = src_size;
                new_insns.push(sext_insn);
                ext
            } else {
                src
            };

            let shift_amount = func.create_const_pseudo(63);
            let hi = alloc_reg64(func);
            new_insns.push(Instruction::binop(
                Opcode::Asr,
                hi,
                lo,
                shift_amount,
                long_type,
                64,
            ));
            let int128_type = insn.typ.unwrap();
            new_insns.push(Instruction::binop(
                Opcode::Pair64,
                result,
                lo,
                hi,
                int128_type,
                128,
            ));
        }

        _ => panic!("expand_int128: unexpected opcode {}", insn.op),
    }
}

/// Expand a Float16 arithmetic/neg/cmp instruction using promote-operate-truncate.
fn expand_float16(
    insn: &Instruction,
    func: &mut Function,
    new_insns: &mut Vec<Instruction>,
    types: &TypeTable,
    target: &Target,
) {
    let float_type = types.float_id;
    let float_size = types.size_bits(float_type);
    let pos = insn.pos;

    match insn.op {
        // Binary arithmetic: extend both → float op → truncate
        Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
            let result = insn.target.expect("binop must have target");
            let left = insn.src[0];
            let right = insn.src[1];

            // Extend left to float
            let left_ext = func.alloc_pseudo();
            func.add_pseudo(Pseudo::reg(left_ext, left_ext.0));
            new_insns.push(build_f16_extend_call(left_ext, left, pos, types, target));

            // Extend right to float
            let right_ext = func.alloc_pseudo();
            func.add_pseudo(Pseudo::reg(right_ext, right_ext.0));
            new_insns.push(build_f16_extend_call(right_ext, right, pos, types, target));

            // Native float operation
            let float_result = func.alloc_pseudo();
            func.add_pseudo(Pseudo::reg(float_result, float_result.0));
            new_insns.push(Instruction::binop(
                insn.op,
                float_result,
                left_ext,
                right_ext,
                float_type,
                float_size,
            ));

            // Truncate result back to Float16
            new_insns.push(build_f16_truncate_call(
                result,
                float_result,
                pos,
                types,
                target,
            ));
        }

        // Negation: extend → negate → truncate
        Opcode::FNeg => {
            let result = insn.target.expect("unary must have target");
            let src = insn.src[0];

            let src_ext = func.alloc_pseudo();
            func.add_pseudo(Pseudo::reg(src_ext, src_ext.0));
            new_insns.push(build_f16_extend_call(src_ext, src, pos, types, target));

            let neg_result = func.alloc_pseudo();
            func.add_pseudo(Pseudo::reg(neg_result, neg_result.0));
            new_insns.push(Instruction::unop(
                Opcode::FNeg,
                neg_result,
                src_ext,
                float_type,
                float_size,
            ));

            new_insns.push(build_f16_truncate_call(
                result, neg_result, pos, types, target,
            ));
        }

        // Comparison: extend both → float compare (no truncate)
        Opcode::FCmpOEq
        | Opcode::FCmpONe
        | Opcode::FCmpOLt
        | Opcode::FCmpOLe
        | Opcode::FCmpOGt
        | Opcode::FCmpOGe => {
            let result = insn.target.expect("cmp must have target");
            let left = insn.src[0];
            let right = insn.src[1];

            let left_ext = func.alloc_pseudo();
            func.add_pseudo(Pseudo::reg(left_ext, left_ext.0));
            new_insns.push(build_f16_extend_call(left_ext, left, pos, types, target));

            let right_ext = func.alloc_pseudo();
            func.add_pseudo(Pseudo::reg(right_ext, right_ext.0));
            new_insns.push(build_f16_extend_call(right_ext, right, pos, types, target));

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
            new_insns.push(cmp);
        }

        _ => panic!("expand_float16: unexpected opcode {}", insn.op),
    }
}

/// Run the hardware mapping pass on a single function.
///
/// Walks all instructions and transforms non-Legal ops:
///   - LibCall: replace with a runtime library call instruction
///   - Expand: replace with multiple simpler instructions
pub fn hwmap_function(func: &mut Function, types: &TypeTable, target: &Target) {
    let hwmap = get_target_hwmap(target);

    for block_idx in 0..func.blocks.len() {
        // Take the insns out of the block to avoid borrow conflicts
        let old_insns = std::mem::take(&mut func.blocks[block_idx].insns);
        let mut new_insns = Vec::with_capacity(old_insns.len());
        let mut block_changed = false;

        for insn in &old_insns {
            match hwmap.map_op(insn, types) {
                HwMapAction::Legal => {
                    new_insns.push(insn.clone());
                }
                HwMapAction::LibCall(name) => {
                    let call = match insn.op {
                        Opcode::FCvtS
                        | Opcode::FCvtU
                        | Opcode::SCvtF
                        | Opcode::UCvtF
                        | Opcode::FCvtF => build_convert_rtlib_call(insn, name, types, target),
                        Opcode::FNeg => {
                            // Unary: single arg, same type as result
                            build_binop_rtlib_call(insn, name, types, target)
                        }
                        _ => build_binop_rtlib_call(insn, name, types, target),
                    };
                    new_insns.push(call);
                    block_changed = true;
                }
                HwMapAction::CmpLibCall(name, cmp_op) => {
                    // Long double comparison expansion:
                    // 1. Call rtlib cmp function (returns int)
                    // 2. Compare result against 0
                    let result_pseudo = insn.target.expect("cmp must have target");
                    let int_type = types.int_id;
                    let int_size = types.size_bits(int_type);
                    let ld_type = types.longdouble_id;

                    // Allocate pseudo for cmp call result
                    let cmp_result = func.alloc_pseudo();
                    func.add_pseudo(Pseudo::reg(cmp_result, cmp_result.0));

                    // Allocate pseudo for zero constant
                    let zero = func.create_const_pseudo(0);

                    // Build the rtlib call: cmp_result = __lttf2(left, right)
                    let arg_vals = insn.src.clone();
                    let arg_types = vec![ld_type; arg_vals.len()];
                    let call = build_rtlib_call_explicit(
                        RtlibCallParams {
                            target_pseudo: cmp_result,
                            arg_vals: &arg_vals,
                            func_name: name,
                            arg_types,
                            ret_type: int_type,
                            pos: insn.pos,
                        },
                        types,
                        target,
                    );
                    new_insns.push(call);

                    // Build the int comparison: result = cmp_op(cmp_result, 0)
                    new_insns.push(Instruction::binop(
                        cmp_op,
                        result_pseudo,
                        cmp_result,
                        zero,
                        int_type,
                        int_size,
                    ));
                    block_changed = true;
                }
                HwMapAction::Expand => {
                    expand_insn(insn, func, &mut new_insns, types, target);
                    block_changed = true;
                }
            }
        }

        if block_changed {
            func.blocks[block_idx].insns = new_insns;
        } else {
            // Put the original insns back (no change)
            func.blocks[block_idx].insns = old_insns;
        }
    }
}

/// Run the hardware mapping pass on an entire module.
pub fn hwmap_module(module: &mut Module, types: &TypeTable, target: &Target) {
    for func in &mut module.functions {
        hwmap_function(func, types, target);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Opcode, Pseudo, PseudoId};
    use crate::target::{Arch, Os, Target};
    use crate::types::TypeTable;

    fn make_test_func(types: &TypeTable) -> Function {
        let mut func = Function::new("test_hwmap", types.int_id);

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

    #[test]
    fn test_map_op_returns_legal_x86_64() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        let func = make_test_func(&types);
        for block in &func.blocks {
            for insn in &block.insns {
                assert_eq!(
                    hwmap.map_op(insn, &types),
                    HwMapAction::Legal,
                    "expected Legal for {} on x86_64",
                    insn.op
                );
            }
        }
    }

    #[test]
    fn test_map_op_returns_legal_aarch64() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = Aarch64HwMap {
            target: target.clone(),
        };

        let func = make_test_func(&types);
        for block in &func.blocks {
            for insn in &block.insns {
                assert_eq!(
                    hwmap.map_op(insn, &types),
                    HwMapAction::Legal,
                    "expected Legal for {} on aarch64",
                    insn.op
                );
            }
        }
    }

    #[test]
    fn test_hwmap_function_all_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mut func = make_test_func(&types);

        hwmap_function(&mut func, &types, &target);
    }

    #[test]
    fn test_hwmap_function_aarch64() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mut func = make_test_func(&types);

        hwmap_function(&mut func, &types, &target);
    }

    #[test]
    fn test_hwmap_module_empty() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mut module = Module::new();

        hwmap_module(&mut module, &types, &target);
    }

    #[test]
    fn test_hwmap_module_with_functions() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);

        let mut module = Module::new();
        module.add_function(make_test_func(&types));
        module.add_function(make_test_func(&types));

        hwmap_module(&mut module, &types, &target);
    }

    #[test]
    fn test_hwmap_idempotent() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mut func = make_test_func(&types);

        hwmap_function(&mut func, &types, &target);
        hwmap_function(&mut func, &types, &target);
    }

    #[test]
    fn test_hwmap_action_enum() {
        assert_eq!(HwMapAction::Legal, HwMapAction::Legal);
        assert_ne!(HwMapAction::Legal, HwMapAction::LibCall("__divti3"));
        assert_eq!(
            HwMapAction::LibCall("__divti3"),
            HwMapAction::LibCall("__divti3")
        );
        assert_ne!(
            HwMapAction::LibCall("__divti3"),
            HwMapAction::LibCall("__modti3")
        );
    }

    #[test]
    fn test_hwmap_all_targets() {
        let targets = vec![
            Target::new(Arch::X86_64, Os::Linux),
            Target::new(Arch::X86_64, Os::MacOS),
            Target::new(Arch::X86_64, Os::FreeBSD),
            Target::new(Arch::Aarch64, Os::Linux),
            Target::new(Arch::Aarch64, Os::MacOS),
        ];

        for target in &targets {
            let types = TypeTable::new(target);
            let mut func = make_test_func(&types);
            hwmap_function(&mut func, &types, target);
        }
    }

    // ========================================================================
    // Phase 2a: Int128 div/mod → LibCall tests
    // ========================================================================

    #[test]
    fn test_int128_divs_libcall_x86_64() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        let insn = Instruction::binop(
            Opcode::DivS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int128_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__divti3")
        );
    }

    #[test]
    fn test_int128_divu_libcall_x86_64() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        let insn = Instruction::binop(
            Opcode::DivU,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.uint128_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__udivti3")
        );
    }

    #[test]
    fn test_int128_mods_libcall_x86_64() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        let insn = Instruction::binop(
            Opcode::ModS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int128_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__modti3")
        );
    }

    #[test]
    fn test_int128_modu_libcall_x86_64() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        let insn = Instruction::binop(
            Opcode::ModU,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.uint128_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__umodti3")
        );
    }

    #[test]
    fn test_int128_divmod_libcall_aarch64() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = Aarch64HwMap {
            target: target.clone(),
        };

        let insn = Instruction::binop(
            Opcode::DivS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int128_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__divti3")
        );

        let insn = Instruction::binop(
            Opcode::ModU,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.uint128_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__umodti3")
        );
    }

    #[test]
    fn test_int32_div_stays_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        let insn = Instruction::binop(
            Opcode::DivS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        assert_eq!(hwmap.map_op(&insn, &types), HwMapAction::Legal);
    }

    #[test]
    fn test_int128_add_expands() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        let insn = Instruction::binop(
            Opcode::Add,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int128_id,
            128,
        );
        assert_eq!(hwmap.map_op(&insn, &types), HwMapAction::Expand);
    }

    #[test]
    fn test_hwmap_transforms_int128_divmod() {
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

        hwmap_function(&mut func, &types, &target);

        // After hwmap, the DivS should be replaced with a Call to __divti3
        let block = &func.blocks[0];
        assert_eq!(block.insns.len(), 3); // Entry, Call, Ret
        assert_eq!(block.insns[1].op, Opcode::Call);
        assert_eq!(block.insns[1].func_name.as_deref(), Some("__divti3"));
        assert_eq!(block.insns[1].target, Some(PseudoId(2)));
        assert_eq!(block.insns[1].src, vec![PseudoId(0), PseudoId(1)]);
        assert!(block.insns[1].abi_info.is_some());
    }

    // ========================================================================
    // Phase 2b: Int128↔float conversion → LibCall tests
    // ========================================================================

    /// Helper to create a conversion instruction.
    fn make_convert_insn(
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

    #[test]
    fn test_int128_to_float_libcall() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        // signed int128 → float
        let insn = make_convert_insn(Opcode::SCvtF, types.float_id, 32, types.int128_id, 128);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__floattisf")
        );

        // signed int128 → double
        let insn = make_convert_insn(Opcode::SCvtF, types.double_id, 64, types.int128_id, 128);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__floattidf")
        );

        // unsigned int128 → float
        let insn = make_convert_insn(Opcode::UCvtF, types.float_id, 32, types.uint128_id, 128);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__floatuntisf")
        );

        // unsigned int128 → double
        let insn = make_convert_insn(Opcode::UCvtF, types.double_id, 64, types.uint128_id, 128);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__floatuntidf")
        );
    }

    #[test]
    fn test_float_to_int128_libcall() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        // float → signed int128
        let insn = make_convert_insn(Opcode::FCvtS, types.int128_id, 128, types.float_id, 32);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__fixsfti")
        );

        // double → signed int128
        let insn = make_convert_insn(Opcode::FCvtS, types.int128_id, 128, types.double_id, 64);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__fixdfti")
        );

        // float → unsigned int128
        let insn = make_convert_insn(Opcode::FCvtU, types.uint128_id, 128, types.float_id, 32);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__fixunssfti")
        );

        // double → unsigned int128
        let insn = make_convert_insn(Opcode::FCvtU, types.uint128_id, 128, types.double_id, 64);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__fixunsdfti")
        );
    }

    #[test]
    fn test_int128_longdouble_x86_64() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        // x86-64 long double uses "xf" suffix
        let insn = make_convert_insn(Opcode::SCvtF, types.longdouble_id, 80, types.int128_id, 128);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__floattixf")
        );

        let insn = make_convert_insn(Opcode::FCvtS, types.int128_id, 128, types.longdouble_id, 80);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__fixxfti")
        );
    }

    #[test]
    fn test_int128_longdouble_aarch64() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = Aarch64HwMap {
            target: target.clone(),
        };

        // aarch64 long double uses "tf" suffix
        let insn = make_convert_insn(
            Opcode::SCvtF,
            types.longdouble_id,
            128,
            types.int128_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__floattitf")
        );
    }

    #[test]
    fn test_non_int128_conversion_stays_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        // int32 → double should remain Legal
        let insn = make_convert_insn(Opcode::SCvtF, types.double_id, 64, types.int_id, 32);
        assert_eq!(hwmap.map_op(&insn, &types), HwMapAction::Legal);

        // double → int32 should remain Legal
        let insn = make_convert_insn(Opcode::FCvtS, types.int_id, 32, types.double_id, 64);
        assert_eq!(hwmap.map_op(&insn, &types), HwMapAction::Legal);
    }

    #[test]
    fn test_hwmap_transforms_int128_conversion() {
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

        hwmap_function(&mut func, &types, &target);

        let block = &func.blocks[0];
        assert_eq!(block.insns.len(), 3);
        assert_eq!(block.insns[1].op, Opcode::Call);
        assert_eq!(block.insns[1].func_name.as_deref(), Some("__floattidf"));
        assert!(block.insns[1].abi_info.is_some());
    }

    // ========================================================================
    // Phase 2c: Long double → LibCall/CmpLibCall tests (aarch64/Linux only)
    // ========================================================================

    #[test]
    fn test_longdouble_binop_aarch64_linux() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = Aarch64HwMap {
            target: target.clone(),
        };

        let insn = Instruction::binop(
            Opcode::FAdd,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.longdouble_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__addtf3")
        );

        let insn = Instruction::binop(
            Opcode::FSub,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.longdouble_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__subtf3")
        );

        let insn = Instruction::binop(
            Opcode::FMul,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.longdouble_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__multf3")
        );

        let insn = Instruction::binop(
            Opcode::FDiv,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.longdouble_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__divtf3")
        );
    }

    #[test]
    fn test_longdouble_binop_x86_64_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        // x86_64 long double (x87) is native — should be Legal
        let insn = Instruction::binop(
            Opcode::FAdd,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.longdouble_id,
            80,
        );
        assert_eq!(hwmap.map_op(&insn, &types), HwMapAction::Legal);
    }

    #[test]
    fn test_longdouble_binop_macos_legal() {
        let target = Target::new(Arch::Aarch64, Os::MacOS);
        let types = TypeTable::new(&target);
        let hwmap = Aarch64HwMap {
            target: target.clone(),
        };

        // macOS aarch64: long double == double, native
        let insn = Instruction::binop(
            Opcode::FAdd,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.longdouble_id,
            64,
        );
        assert_eq!(hwmap.map_op(&insn, &types), HwMapAction::Legal);
    }

    #[test]
    fn test_longdouble_neg_aarch64_linux() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = Aarch64HwMap {
            target: target.clone(),
        };

        let insn = Instruction::unop(
            Opcode::FNeg,
            PseudoId(2),
            PseudoId(0),
            types.longdouble_id,
            128,
        );
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__negtf2")
        );
    }

    #[test]
    fn test_longdouble_cmp_aarch64_linux() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = Aarch64HwMap {
            target: target.clone(),
        };

        let mut insn = Instruction::binop(
            Opcode::FCmpOLt,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            128,
        );
        insn.src_typ = Some(types.longdouble_id);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::CmpLibCall("__lttf2", Opcode::SetLt)
        );

        let mut insn = Instruction::binop(
            Opcode::FCmpOEq,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            128,
        );
        insn.src_typ = Some(types.longdouble_id);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::CmpLibCall("__eqtf2", Opcode::SetEq)
        );
    }

    #[test]
    fn test_longdouble_convert_aarch64_linux() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = Aarch64HwMap {
            target: target.clone(),
        };

        // float → longdouble
        let insn = make_convert_insn(Opcode::FCvtF, types.longdouble_id, 128, types.float_id, 32);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__extendsftf2")
        );

        // longdouble → double
        let insn = make_convert_insn(Opcode::FCvtF, types.double_id, 64, types.longdouble_id, 128);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__trunctfdf2")
        );

        // int32 → longdouble
        let insn = make_convert_insn(Opcode::SCvtF, types.longdouble_id, 128, types.int_id, 32);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__floatsitf")
        );

        // longdouble → int64
        let insn = make_convert_insn(Opcode::FCvtS, types.long_id, 64, types.longdouble_id, 128);
        assert_eq!(
            hwmap.map_op(&insn, &types),
            HwMapAction::LibCall("__fixtfdi")
        );
    }

    #[test]
    fn test_longdouble_convert_x86_64_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let hwmap = X86_64HwMap {
            target: target.clone(),
        };

        // x86_64 long double conversions are native
        let insn = make_convert_insn(Opcode::FCvtF, types.longdouble_id, 80, types.float_id, 32);
        assert_eq!(hwmap.map_op(&insn, &types), HwMapAction::Legal);
    }

    // ========================================================================
    // Phase 2e: Complex mul/div rtlib name tests
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
