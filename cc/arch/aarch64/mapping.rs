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

use crate::arch::mapping::{
    map_int128_divmod, map_int128_expand, map_int128_float_convert, map_int128_to_float16,
    ArchMapper, MappedInsn, MappingCtx,
};
use crate::float::FpFormat;
use crate::ir::{Instruction, Opcode};

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
        // int128 → _Float16, which the pass above declines: it has no
        // `float_suffix` for a half. Left unlowered, codegen emitted a native
        // `scvtf h0, x9` reading only the low 64 bits.
        if let Some(r) = map_int128_to_float16(insn, ctx) {
            return r;
        }
        MappedInsn::Legal
    }

    /// `fsqrt`, the six `frint` instructions, `fminnm`, `fmaxnm` and
    /// `fmadd` for the scalar formats; binary128, `long double` on Linux, is
    /// software.
    fn computes_in_place(&self, op: Opcode, fmt: FpFormat) -> bool {
        match op {
            Opcode::Sqrt
            | Opcode::RoundToIntegral(_)
            | Opcode::FMin
            | Opcode::FMax
            | Opcode::Fma => matches!(fmt, FpFormat::Binary32 | FpFormat::Binary64),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arch::mapping::test_helpers::*;
    use crate::arch::mapping::{library_call, MappingCtx};
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

    // Int128 div/mod

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

    // Long double → rtlib (aarch64/Linux only), after the optimizer: the
    // early mapping leaves binary128 alone for the folders.

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
            assert_legal(&mapper.map_insn(&insn, &mut ctx));
            assert_libcall(&library_call(&insn, &mut ctx, &mapper), name);
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
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_libcall(&library_call(&insn, &mut ctx, &mapper), "__negtf2");
    }

    #[test]
    fn test_aarch64_longdouble_cmp() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        let mut insn = Instruction::compare(
            Opcode::FCmpOLt,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (types.int_id, 128),
            (types.int_id, 32),
        );
        insn.src_typ = Some(types.longdouble_id);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_cmp_libcall(
            &library_call(&insn, &mut ctx, &mapper),
            "__lttf2",
            Opcode::SetLt,
        );

        let mut insn = Instruction::compare(
            Opcode::FCmpOEq,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (types.int_id, 128),
            (types.int_id, 32),
        );
        insn.src_typ = Some(types.longdouble_id);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_cmp_libcall(
            &library_call(&insn, &mut ctx, &mapper),
            "__eqtf2",
            Opcode::SetEq,
        );
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
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_libcall(&library_call(&insn, &mut ctx, &mapper), "__extendsftf2");

        // longdouble → double
        let insn = make_convert_insn(Opcode::FCvtF, types.double_id, 64, types.longdouble_id, 128);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_libcall(&library_call(&insn, &mut ctx, &mapper), "__trunctfdf2");

        // int32 → longdouble
        let insn = make_convert_insn(Opcode::SCvtF, types.longdouble_id, 128, types.int_id, 32);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_libcall(&library_call(&insn, &mut ctx, &mapper), "__floatsitf");

        // longdouble → int64
        let insn = make_convert_insn(Opcode::FCvtS, types.long_id, 64, types.longdouble_id, 128);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_libcall(&library_call(&insn, &mut ctx, &mapper), "__fixtfdi");
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
