//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 instruction mapping
//

use crate::arch::mapping::{
    build_f16_convert_call, expand_float16_arith, expand_float16_cmp, expand_float16_int_convert,
    expand_float16_neg, float_suffix, map_binary128, map_int128_divmod, map_int128_expand,
    map_int128_float_convert, ArchMapper, MappedInsn, MappingCtx,
};
use crate::ir::{Instruction, Opcode};
use crate::rtlib::RtlibNames;
use crate::types::TypeKind;

/// x86-64 instruction mapper.
pub struct X86_64Mapper;

impl ArchMapper for X86_64Mapper {
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
        // Shared: IEEE binary128 → rtlib soft-float. On x86-64 this is
        // `__float128` only; `long double` is x87 and stays in hardware.
        if let Some(r) = map_binary128(insn, ctx) {
            return r;
        }
        // x86-64 only: Float16 soft-float → expand
        if let Some(r) = self.map_float16(insn, ctx) {
            return r;
        }
        MappedInsn::Legal
    }
}

impl X86_64Mapper {
    /// Classify and expand Float16 operations via promote-operate-truncate.
    fn map_float16(&self, insn: &Instruction, ctx: &mut MappingCtx<'_>) -> Option<MappedInsn> {
        let types = ctx.types;
        match insn.op {
            // Arithmetic: promote-operate-truncate
            Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
                let typ = insn.typ?;
                if types.kind(typ) == TypeKind::Float16 {
                    Some(MappedInsn::Replace(expand_float16_arith(
                        insn, ctx.func, types, ctx.target,
                    )))
                } else {
                    None
                }
            }
            // Negation: promote-negate-truncate
            Opcode::FNeg => {
                let typ = insn.typ?;
                if types.kind(typ) == TypeKind::Float16 {
                    Some(MappedInsn::Replace(expand_float16_neg(
                        insn, ctx.func, types, ctx.target,
                    )))
                } else {
                    None
                }
            }
            // Comparisons: promote both, compare (no truncate)
            Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => {
                if let Some(src_typ) = insn.src_typ {
                    if types.kind(src_typ) == TypeKind::Float16 {
                        return Some(MappedInsn::Replace(expand_float16_cmp(
                            insn, ctx.func, types, ctx.target,
                        )));
                    }
                }
                // Fallback: check operand size (Float16 = 16 bits)
                if insn.size == 16 {
                    return Some(MappedInsn::Replace(expand_float16_cmp(
                        insn, ctx.func, types, ctx.target,
                    )));
                }
                None
            }
            // Float16↔float/double/longdouble conversions
            Opcode::FCvtF => {
                let src_typ = insn.src_typ?;
                let dst_typ = insn.typ?;
                let src_kind = types.kind(src_typ);
                let dst_kind = types.kind(dst_typ);
                if src_kind == TypeKind::Float16 {
                    let to_suffix = float_suffix(dst_kind, ctx.target);
                    let rtlib = RtlibNames::new(ctx.target);
                    let func_name = rtlib.float16_convert("hf", to_suffix)?;
                    let call = build_f16_convert_call(
                        insn, func_name, src_typ, dst_typ, types, ctx.target,
                    );
                    Some(MappedInsn::Replace(vec![call]))
                } else if dst_kind == TypeKind::Float16 {
                    let from_suffix = float_suffix(src_kind, ctx.target);
                    let rtlib = RtlibNames::new(ctx.target);
                    let func_name = rtlib.float16_convert(from_suffix, "hf")?;
                    let call = build_f16_convert_call(
                        insn, func_name, src_typ, dst_typ, types, ctx.target,
                    );
                    Some(MappedInsn::Replace(vec![call]))
                } else {
                    None
                }
            }
            // Float16 <-> integer, converted through `float`.
            Opcode::FCvtS | Opcode::FCvtU => {
                if types.kind(insn.src_typ?) != TypeKind::Float16 {
                    return None;
                }
                Some(MappedInsn::Replace(expand_float16_int_convert(
                    insn, ctx.func, types, ctx.target,
                )))
            }
            Opcode::SCvtF | Opcode::UCvtF => {
                if types.kind(insn.typ?) != TypeKind::Float16 {
                    return None;
                }
                Some(MappedInsn::Replace(expand_float16_int_convert(
                    insn, ctx.func, types, ctx.target,
                )))
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
    fn test_x86_64_legal_insns() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

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

    // Int128 div/mod

    #[test]
    fn test_x86_64_int128_divs() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

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
    }

    #[test]
    fn test_x86_64_int128_divu() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        let insn = Instruction::binop(
            Opcode::DivU,
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
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__udivti3");
    }

    #[test]
    fn test_x86_64_int128_mods() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        let insn = Instruction::binop(
            Opcode::ModS,
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
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__modti3");
    }

    #[test]
    fn test_x86_64_int128_modu() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

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

    #[test]
    fn test_x86_64_int32_div_stays_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        let insn = Instruction::binop(
            Opcode::DivS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
    }

    // Int128 expand

    #[test]
    fn test_x86_64_int128_add_expands() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        let insn = Instruction::binop(
            Opcode::Add,
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
        assert_expand(&mapper.map_insn(&insn, &mut ctx));
    }

    // Int128↔float conversion

    #[test]
    fn test_x86_64_int128_to_float() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // signed int128 → float
        let insn = make_convert_insn(Opcode::SCvtF, types.float_id, 32, types.int128_id, 128);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__floattisf");

        // signed int128 → double
        let insn = make_convert_insn(Opcode::SCvtF, types.double_id, 64, types.int128_id, 128);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__floattidf");

        // unsigned int128 → float
        let insn = make_convert_insn(Opcode::UCvtF, types.float_id, 32, types.uint128_id, 128);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__floatuntisf");

        // unsigned int128 → double
        let insn = make_convert_insn(Opcode::UCvtF, types.double_id, 64, types.uint128_id, 128);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__floatuntidf");
    }

    #[test]
    fn test_x86_64_float_to_int128() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // float → signed int128
        let insn = make_convert_insn(Opcode::FCvtS, types.int128_id, 128, types.float_id, 32);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__fixsfti");

        // double → signed int128
        let insn = make_convert_insn(Opcode::FCvtS, types.int128_id, 128, types.double_id, 64);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__fixdfti");

        // float → unsigned int128
        let insn = make_convert_insn(Opcode::FCvtU, types.uint128_id, 128, types.float_id, 32);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__fixunssfti");

        // double → unsigned int128
        let insn = make_convert_insn(Opcode::FCvtU, types.uint128_id, 128, types.double_id, 64);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__fixunsdfti");
    }

    #[test]
    fn test_x86_64_int128_longdouble() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // x86-64 long double uses "xf" suffix
        let insn = make_convert_insn(Opcode::SCvtF, types.longdouble_id, 80, types.int128_id, 128);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__floattixf");

        let insn = make_convert_insn(Opcode::FCvtS, types.int128_id, 128, types.longdouble_id, 80);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__fixxfti");
    }

    #[test]
    fn test_x86_64_non_int128_conversion_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // int32 → double should remain Legal
        let insn = make_convert_insn(Opcode::SCvtF, types.double_id, 64, types.int_id, 32);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));

        // double → int32 should remain Legal
        let insn = make_convert_insn(Opcode::FCvtS, types.int_id, 32, types.double_id, 64);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
    }

    #[test]
    fn test_x86_64_longdouble_binop_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // x86_64 long double (x87) is native — should be Legal
        let insn = Instruction::binop(
            Opcode::FAdd,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.longdouble_id,
            80,
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
    fn test_x86_64_longdouble_convert_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // x86_64 long double conversions are native
        let insn = make_convert_insn(Opcode::FCvtF, types.longdouble_id, 80, types.float_id, 32);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
    }

    // Int128 constant shifts

    #[test]
    fn test_x86_64_int128_const_shl_expands() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // Create Shl.128 with constant shift amount
        let mut func = make_minimal_func(&types);
        let shift_const = func.create_const_pseudo(5);
        let insn = Instruction::binop(
            Opcode::Shl,
            PseudoId(2),
            PseudoId(0),
            shift_const,
            types.int128_id,
            128,
        );
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_expand(&mapper.map_insn(&insn, &mut ctx));
    }

    #[test]
    fn test_x86_64_int128_const_lsr_expands() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        let mut func = make_minimal_func(&types);
        let shift_const = func.create_const_pseudo(64);
        let insn = Instruction::binop(
            Opcode::Lsr,
            PseudoId(2),
            PseudoId(0),
            shift_const,
            types.uint128_id,
            128,
        );
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_expand(&mapper.map_insn(&insn, &mut ctx));
    }

    #[test]
    fn test_x86_64_int128_const_asr_expands() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        let mut func = make_minimal_func(&types);
        let shift_const = func.create_const_pseudo(127);
        let insn = Instruction::binop(
            Opcode::Asr,
            PseudoId(2),
            PseudoId(0),
            shift_const,
            types.int128_id,
            128,
        );
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_expand(&mapper.map_insn(&insn, &mut ctx));
    }

    #[test]
    fn test_x86_64_int128_variable_shift_legal() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // Variable shift (PseudoId(1) is a register, not a constant) → stays Legal
        let insn = Instruction::binop(
            Opcode::Shl,
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
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
    }

    // Float16 conversions

    #[test]
    fn test_x86_64_float16_to_float_conversion() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // Float16 → float should expand to __extendhfsf2
        let insn = make_convert_insn(Opcode::FCvtF, types.float_id, 32, types.float16_id, 16);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__extendhfsf2");
    }

    #[test]
    fn test_x86_64_float_to_float16_conversion() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // float → Float16 should expand to __truncsfhf2
        let insn = make_convert_insn(Opcode::FCvtF, types.float16_id, 16, types.float_id, 32);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__truncsfhf2");
    }

    #[test]
    fn test_x86_64_float16_to_double_conversion() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        let insn = make_convert_insn(Opcode::FCvtF, types.double_id, 64, types.float16_id, 16);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__extendhfdf2");
    }

    /// A `_Float16` <-> integer conversion goes through `float`, because
    /// libgcc implements no half<->integer helper at all.
    #[test]
    fn test_x86_64_float16_int_conversions_go_through_float() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        // Each row: the conversion, and the one rtlib call the expansion is
        // allowed to make. The other step is a native float<->int instruction.
        let cases = [
            (
                Opcode::FCvtS,
                types.int_id,
                32,
                types.float16_id,
                16,
                "__extendhfsf2",
            ),
            (
                Opcode::FCvtU,
                types.uint_id,
                32,
                types.float16_id,
                16,
                "__extendhfsf2",
            ),
            (
                Opcode::FCvtS,
                types.long_id,
                64,
                types.float16_id,
                16,
                "__extendhfsf2",
            ),
            (
                Opcode::SCvtF,
                types.float16_id,
                16,
                types.int_id,
                32,
                "__truncsfhf2",
            ),
            (
                Opcode::UCvtF,
                types.float16_id,
                16,
                types.uint_id,
                32,
                "__truncsfhf2",
            ),
            (
                Opcode::SCvtF,
                types.float16_id,
                16,
                types.long_id,
                64,
                "__truncsfhf2",
            ),
        ];

        for (op, dst, dst_size, src, src_size, expected_call) in cases {
            let insn = make_convert_insn(op, dst, dst_size, src, src_size);
            let mut func = make_minimal_func(&types);
            let mut ctx = MappingCtx {
                func: &mut func,
                types: &types,
                target: &target,
            };
            let MappedInsn::Replace(insns) = mapper.map_insn(&insn, &mut ctx) else {
                panic!("{op:?} should be expanded");
            };
            assert_eq!(insns.len(), 2, "{op:?}: expected a two-step expansion");

            let calls: Vec<&str> = insns
                .iter()
                .filter(|i| i.op == Opcode::Call)
                .filter_map(|i| i.func_name.as_deref())
                .collect();
            assert_eq!(
                calls,
                vec![expected_call],
                "{op:?}: exactly one rtlib call, and it must be one libgcc has"
            );

            // None of the eight symbols libgcc lacks may appear.
            for name in &calls {
                assert!(
                    !name.contains("hf") || *name == expected_call,
                    "{op:?}: emitted {name}, which libgcc does not define"
                );
            }
        }
    }

    /// A 128-bit operand builds its own libcall inside the expansion rather
    /// than leaving a `float` <-> `__int128` conversion for a later pass:
    /// `map_function` does not re-map a replacement. Picking the helper by
    /// `dst_size <= 32` would hand it the 64-bit `__fixhfdi`/`__floatdihf`.
    #[test]
    fn test_x86_64_float16_int128_conversions_use_the_128_bit_helper() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = X86_64Mapper;

        let cases = [
            (
                Opcode::FCvtS,
                types.int128_id,
                128,
                types.float16_id,
                16,
                ["__extendhfsf2", "__fixsfti"],
            ),
            (
                Opcode::FCvtU,
                types.uint128_id,
                128,
                types.float16_id,
                16,
                ["__extendhfsf2", "__fixunssfti"],
            ),
            (
                Opcode::SCvtF,
                types.float16_id,
                16,
                types.int128_id,
                128,
                ["__floattisf", "__truncsfhf2"],
            ),
            (
                Opcode::UCvtF,
                types.float16_id,
                16,
                types.uint128_id,
                128,
                ["__floatuntisf", "__truncsfhf2"],
            ),
        ];

        for (op, dst, dst_size, src, src_size, expected) in cases {
            let insn = make_convert_insn(op, dst, dst_size, src, src_size);
            let mut func = make_minimal_func(&types);
            let mut ctx = MappingCtx {
                func: &mut func,
                types: &types,
                target: &target,
            };
            let MappedInsn::Replace(insns) = mapper.map_insn(&insn, &mut ctx) else {
                panic!("{op:?} should be expanded");
            };
            let calls: Vec<&str> = insns
                .iter()
                .filter(|i| i.op == Opcode::Call)
                .filter_map(|i| i.func_name.as_deref())
                .collect();
            assert_eq!(
                calls,
                expected.to_vec(),
                "{op:?}: both steps must be libgcc-provided calls"
            );
        }
    }
}
