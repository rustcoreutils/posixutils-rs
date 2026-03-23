//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 instruction mapping
//

use crate::abi::CallingConv;
use crate::arch::mapping::{
    build_binop_rtlib_call, build_convert_rtlib_call, int_suffix_for_longdouble,
    longdouble_needs_rtlib, map_int128_divmod, map_int128_expand, map_int128_float_convert,
    ArchMapper, MappedInsn, MappingCtx,
};
use crate::ir::{Instruction, Opcode};
use crate::types::TypeKind;

/// AArch64 instruction mapper.
pub struct Aarch64Mapper;

impl ArchMapper for Aarch64Mapper {
    fn map_insn(&self, insn: &Instruction, ctx: &mut MappingCtx<'_>) -> MappedInsn {
        // Shared: int128 div/mod → rtlib
        if let Some(r) = map_int128_divmod(insn, ctx) {
            return r;
        }
        // Shared: int128 expand (add/sub/mul/bitwise/neg/not/cmp/zext/sext)
        if let Some(r) = map_int128_expand(insn, ctx) {
            return r;
        }
        // Shared: int128↔float → rtlib
        if let Some(r) = map_int128_float_convert(insn, ctx) {
            return r;
        }
        // aarch64 only: long double → rtlib (Linux, not macOS)
        if let Some(r) = self.map_longdouble(insn, ctx) {
            return r;
        }
        MappedInsn::Legal
    }
}

impl Aarch64Mapper {
    /// Classify and expand long double operations via rtlib calls.
    /// Only applies on aarch64/Linux where long double is 128-bit IEEE quad.
    fn map_longdouble(&self, insn: &Instruction, ctx: &mut MappingCtx<'_>) -> Option<MappedInsn> {
        if !longdouble_needs_rtlib(ctx.target) {
            return None;
        }

        match insn.op {
            // Binary arithmetic: FAdd/FSub/FMul/FDiv → single rtlib call
            Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
                let typ = insn.typ?;
                if ctx.types.kind(typ) != TypeKind::LongDouble {
                    return None;
                }
                let name = match insn.op {
                    Opcode::FAdd => "__addtf3",
                    Opcode::FSub => "__subtf3",
                    Opcode::FMul => "__multf3",
                    Opcode::FDiv => "__divtf3",
                    _ => unreachable!(),
                };
                let call = build_binop_rtlib_call(insn, name, ctx.types, ctx.target);
                Some(MappedInsn::Replace(vec![call]))
            }

            // Negation: FNeg → single rtlib call
            Opcode::FNeg => {
                let typ = insn.typ?;
                if ctx.types.kind(typ) != TypeKind::LongDouble {
                    return None;
                }
                let call = build_binop_rtlib_call(insn, "__negtf2", ctx.types, ctx.target);
                Some(MappedInsn::Replace(vec![call]))
            }

            // Comparisons: call rtlib cmp, then compare result against 0
            Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe
            | Opcode::FCmpOEq
            | Opcode::FCmpONe => {
                if insn.size != 128 {
                    return None;
                }
                // Also check src_typ if available
                if let Some(src_typ) = insn.src_typ {
                    if ctx.types.kind(src_typ) != TypeKind::LongDouble {
                        return None;
                    }
                }
                let (name, cmp_op) = match insn.op {
                    Opcode::FCmpOLt => ("__lttf2", Opcode::SetLt),
                    Opcode::FCmpOLe => ("__letf2", Opcode::SetLe),
                    Opcode::FCmpOGt => ("__gttf2", Opcode::SetGt),
                    Opcode::FCmpOGe => ("__getf2", Opcode::SetGe),
                    Opcode::FCmpOEq => ("__eqtf2", Opcode::SetEq),
                    Opcode::FCmpONe => ("__netf2", Opcode::SetNe),
                    _ => unreachable!(),
                };

                let result_pseudo = insn.target.expect("cmp must have target");
                let int_type = ctx.types.int_id;
                let int_size = ctx.types.size_bits(int_type);
                let ld_type = ctx.types.longdouble_id;

                // Allocate pseudo for cmp call result
                let cmp_result = ctx.func.create_reg_pseudo();
                let zero = ctx.func.create_const_pseudo(0);

                // Build the rtlib call: cmp_result = __lttf2(left, right)
                let arg_vals = insn.src.clone();
                let arg_types = vec![ld_type; arg_vals.len()];
                let mut call = Instruction::call_with_abi(
                    Some(cmp_result),
                    name,
                    arg_vals,
                    arg_types,
                    int_type,
                    CallingConv::C,
                    ctx.types,
                    ctx.target,
                );
                call.pos = insn.pos;

                // Build the int comparison: result = cmp_op(cmp_result, 0)
                let cmp =
                    Instruction::binop(cmp_op, result_pseudo, cmp_result, zero, int_type, int_size);

                Some(MappedInsn::Replace(vec![call, cmp]))
            }

            // Float-to-float conversions involving long double
            Opcode::FCvtF => {
                let dst_typ = insn.typ?;
                let src_typ = insn.src_typ?;
                let dst_kind = ctx.types.kind(dst_typ);
                let src_kind = ctx.types.kind(src_typ);
                if src_kind == TypeKind::LongDouble {
                    // longdouble → float/double
                    let name = match dst_kind {
                        TypeKind::Float => "__trunctfsf2",
                        TypeKind::Double => "__trunctfdf2",
                        _ => return None,
                    };
                    let call = build_convert_rtlib_call(insn, name, ctx.types, ctx.target);
                    Some(MappedInsn::Replace(vec![call]))
                } else if dst_kind == TypeKind::LongDouble {
                    // float/double → longdouble
                    let name = match src_kind {
                        TypeKind::Float => "__extendsftf2",
                        TypeKind::Double => "__extenddftf2",
                        _ => return None,
                    };
                    let call = build_convert_rtlib_call(insn, name, ctx.types, ctx.target);
                    Some(MappedInsn::Replace(vec![call]))
                } else {
                    None
                }
            }

            // Int-to-float: int → longdouble
            Opcode::SCvtF | Opcode::UCvtF => {
                let dst_typ = insn.typ?;
                if ctx.types.kind(dst_typ) != TypeKind::LongDouble {
                    return None;
                }
                let src_typ = insn.src_typ?;
                // Skip int128 (handled by map_int128_float_convert)
                if ctx.types.kind(src_typ) == TypeKind::Int128 {
                    return None;
                }
                let isuf = int_suffix_for_longdouble(ctx.types, src_typ);
                let name: &'static str = match isuf {
                    "si" => "__floatsitf",
                    "di" => "__floatditf",
                    "usi" => "__floatunsitf",
                    "udi" => "__floatunditf",
                    _ => return None,
                };
                let call = build_convert_rtlib_call(insn, name, ctx.types, ctx.target);
                Some(MappedInsn::Replace(vec![call]))
            }

            // Float-to-int: longdouble → int
            Opcode::FCvtS | Opcode::FCvtU => {
                let src_typ = insn.src_typ?;
                if ctx.types.kind(src_typ) != TypeKind::LongDouble {
                    return None;
                }
                let dst_typ = insn.typ?;
                // Skip int128 (handled by map_int128_float_convert)
                if ctx.types.kind(dst_typ) == TypeKind::Int128 {
                    return None;
                }
                let isuf = int_suffix_for_longdouble(ctx.types, dst_typ);
                let name: &'static str = match isuf {
                    "si" => "__fixtfsi",
                    "di" => "__fixtfdi",
                    "usi" => "__fixunstfsi",
                    "udi" => "__fixunstfdi",
                    _ => return None,
                };
                let call = build_convert_rtlib_call(insn, name, ctx.types, ctx.target);
                Some(MappedInsn::Replace(vec![call]))
            }

            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arch::mapping::test_helpers::*;
    use crate::arch::mapping::MappingCtx;
    use crate::ir::{Instruction, Opcode, PseudoId};
    use crate::target::{Arch, Os, Target};
    use crate::types::TypeTable;

    #[test]
    fn test_aarch64_legal_insns() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        let func_template = make_test_func(&types);
        for block in &func_template.blocks {
            for insn in &block.insns {
                let mut func = make_minimal_func(&types);
                let mut ctx = MappingCtx {
                    func: &mut func,
                    types: &types,
                    target: &target,
                };
                let result = mapper.map_insn(insn, &mut ctx);
                assert_legal(&result);
            }
        }
    }

    // ========================================================================
    // Int128 div/mod
    // ========================================================================

    #[test]
    fn test_aarch64_int128_divmod() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        let insn = Instruction::binop(
            Opcode::DivS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int128_id,
            128,
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__divti3");

        let insn = Instruction::binop(
            Opcode::ModU,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.uint128_id,
            128,
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__umodti3");
    }

    // ========================================================================
    // Long double → rtlib (aarch64/Linux only)
    // ========================================================================

    #[test]
    fn test_aarch64_longdouble_binop() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        for (op, name) in [
            (Opcode::FAdd, "__addtf3"),
            (Opcode::FSub, "__subtf3"),
            (Opcode::FMul, "__multf3"),
            (Opcode::FDiv, "__divtf3"),
        ] {
            let insn = Instruction::binop(
                op,
                PseudoId(2),
                PseudoId(0),
                PseudoId(1),
                types.longdouble_id,
                128,
            );
            let mut func = make_minimal_func(&types);
            let mut ctx = MappingCtx {
                func: &mut func,
                types: &types,
                target: &target,
            };
            assert_libcall(&mapper.map_insn(&insn, &mut ctx), name);
        }
    }

    #[test]
    fn test_aarch64_longdouble_binop_macos_legal() {
        let target = Target::new(Arch::Aarch64, Os::MacOS);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        // macOS aarch64: long double == double, native
        let insn = Instruction::binop(
            Opcode::FAdd,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.longdouble_id,
            64,
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
    }

    #[test]
    fn test_aarch64_longdouble_neg() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        let insn = Instruction::unop(
            Opcode::FNeg,
            PseudoId(2),
            PseudoId(0),
            types.longdouble_id,
            128,
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__negtf2");
    }

    #[test]
    fn test_aarch64_longdouble_cmp() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        let mut insn = Instruction::binop(
            Opcode::FCmpOLt,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            128,
        );
        insn.src_typ = Some(types.longdouble_id);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_cmp_libcall(&mapper.map_insn(&insn, &mut ctx), "__lttf2", Opcode::SetLt);

        let mut insn = Instruction::binop(
            Opcode::FCmpOEq,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            128,
        );
        insn.src_typ = Some(types.longdouble_id);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_cmp_libcall(&mapper.map_insn(&insn, &mut ctx), "__eqtf2", Opcode::SetEq);
    }

    #[test]
    fn test_aarch64_longdouble_convert() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        // float → longdouble
        let insn = make_convert_insn(Opcode::FCvtF, types.longdouble_id, 128, types.float_id, 32);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__extendsftf2");

        // longdouble → double
        let insn = make_convert_insn(Opcode::FCvtF, types.double_id, 64, types.longdouble_id, 128);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__trunctfdf2");

        // int32 → longdouble
        let insn = make_convert_insn(Opcode::SCvtF, types.longdouble_id, 128, types.int_id, 32);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__floatsitf");

        // longdouble → int64
        let insn = make_convert_insn(Opcode::FCvtS, types.long_id, 64, types.longdouble_id, 128);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__fixtfdi");
    }

    #[test]
    fn test_aarch64_int128_longdouble() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        // aarch64 long double uses "tf" suffix
        let insn = make_convert_insn(
            Opcode::SCvtF,
            types.longdouble_id,
            128,
            types.int128_id,
            128,
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__floattitf");
    }
}
