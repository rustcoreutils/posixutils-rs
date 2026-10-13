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
    map_int128_divmod, map_int128_expand, map_int128_float16_convert, map_int128_float_convert,
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
        // int128 <-> _Float16, which the pass above declines: it has no
        // `float_suffix` for a half. Lowered through `float`.
        if let Some(r) = map_int128_float16_convert(insn, ctx) {
            return r;
        }
        MappedInsn::Legal
    }

    /// `fsqrt`, the six `frint` instructions, `fminnm`, `fmaxnm` and
    /// `fmadd` for the scalar formats, and `lrint` and `lround` by `frintx`
    /// with `fcvtzs` and by `fcvtas`; binary128, `long double` on Linux, is
    /// software.
    fn computes_in_place(&self, op: Opcode, fmt: FpFormat, _isa: crate::target::X86Isa) -> bool {
        match op {
            Opcode::Sqrt
            | Opcode::RoundToIntegral(_)
            | Opcode::RoundToInteger(_)
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
    use crate::units::Bits;

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
            Bits::new(128),
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
            Bits::new(128),
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
                Bits::new(128),
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
            Bits::new(64),
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
            Bits::new(128),
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
            Opcode::FCmpsOLt,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (types.int_id, Bits::new(128)),
            (types.int_id, Bits::new(32)),
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
            (types.int_id, Bits::new(128)),
            (types.int_id, Bits::new(32)),
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

    /// libgcc's ordering helpers are the *signaling* comparisons, and its
    /// equality helpers the quiet ones, which is C's split exactly. A quiet
    /// relational has no helper, and the linearizer never emits one for
    /// binary128 (`has_quiet_relational`): reaching here is a bug, loudly.
    #[test]
    fn test_aarch64_longdouble_cmp_names_the_signaling_helpers() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;
        for (op, name, int_op) in [
            (Opcode::FCmpsOLt, "__lttf2", Opcode::SetLt),
            (Opcode::FCmpsOLe, "__letf2", Opcode::SetLe),
            (Opcode::FCmpsOGt, "__gttf2", Opcode::SetGt),
            (Opcode::FCmpsOGe, "__getf2", Opcode::SetGe),
            (Opcode::FCmpOEq, "__eqtf2", Opcode::SetEq),
            (Opcode::FCmpONe, "__netf2", Opcode::SetNe),
        ] {
            let mut insn = Instruction::compare(
                op,
                PseudoId(2),
                (PseudoId(0), PseudoId(1)),
                (types.int_id, Bits::new(128)),
                (types.int_id, Bits::new(32)),
            );
            insn.src_typ = Some(types.longdouble_id);
            let mut func = make_minimal_func(&types);
            let mut ctx = MappingCtx {
                func: &mut func,
                types: &types,
                target: &target,
            };
            assert_cmp_libcall(&library_call(&insn, &mut ctx, &mapper), name, int_op);
        }
    }

    #[test]
    #[should_panic(expected = "quiet binary128")]
    fn test_aarch64_quiet_longdouble_relational_never_reaches_mapping() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mut insn = Instruction::compare(
            Opcode::FCmpOLt,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (types.int_id, Bits::new(128)),
            (types.int_id, Bits::new(32)),
        );
        insn.src_typ = Some(types.longdouble_id);
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        library_call(&insn, &mut ctx, &Aarch64Mapper);
    }

    #[test]
    fn test_aarch64_longdouble_convert() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;

        // float → longdouble
        let insn = make_convert_insn(
            Opcode::FCvtF,
            types.longdouble_id,
            Bits::new(128),
            types.float_id,
            Bits::new(32),
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_libcall(&library_call(&insn, &mut ctx, &mapper), "__extendsftf2");

        // longdouble → double
        let insn = make_convert_insn(
            Opcode::FCvtF,
            types.double_id,
            Bits::new(64),
            types.longdouble_id,
            Bits::new(128),
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_libcall(&library_call(&insn, &mut ctx, &mapper), "__trunctfdf2");

        // int32 → longdouble
        let insn = make_convert_insn(
            Opcode::SCvtF,
            types.longdouble_id,
            Bits::new(128),
            types.int_id,
            Bits::new(32),
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_legal(&mapper.map_insn(&insn, &mut ctx));
        assert_libcall(&library_call(&insn, &mut ctx, &mapper), "__floatsitf");

        // longdouble → int64
        let insn = make_convert_insn(
            Opcode::FCvtS,
            types.long_id,
            Bits::new(64),
            types.longdouble_id,
            Bits::new(128),
        );
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
            Bits::new(128),
            types.int128_id,
            Bits::new(128),
        );
        let mut func = make_minimal_func(&types);
        let mut ctx = MappingCtx {
            func: &mut func,
            types: &types,
            target: &target,
        };
        assert_libcall(&mapper.map_insn(&insn, &mut ctx), "__floattitf");
    }

    /// `_Float16` to and from binary128: libgcc's `hf` routines, as gcc
    /// calls them, since A64 has no `fcvt` between `h` and `q` registers.
    #[test]
    fn test_aarch64_float16_binary128() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let types = TypeTable::new(&target);
        let mapper = Aarch64Mapper;
        let cases = [
            (types.longdouble_id, types.float16_id, "__extendhftf2"),
            (types.float16_id, types.longdouble_id, "__trunctfhf2"),
            (types.float128_id, types.float16_id, "__extendhftf2"),
            (types.float16_id, types.float128_id, "__trunctfhf2"),
        ];
        for (to, from, name) in cases {
            let insn = make_convert_insn(
                Opcode::FCvtF,
                to,
                types.value_bits(to),
                from,
                types.value_bits(from),
            );
            let mut func = make_minimal_func(&types);
            let mut ctx = MappingCtx {
                func: &mut func,
                types: &types,
                target: &target,
            };
            // Legal to map; the library-call pass makes it the call.
            assert_legal(&mapper.map_insn(&insn, &mut ctx));
            assert_libcall(&library_call(&insn, &mut ctx, &mapper), name);
        }
    }

    /// `__int128` <-> `_Float16` goes through `float` with the `sf` routines
    /// and a native `fcvt` on the half side, on Linux and on Apple, whose
    /// runtime has no half<->TI helper.
    #[test]
    fn test_aarch64_float16_int128_via_float() {
        for os in [Os::Linux, Os::MacOS] {
            let target = Target::new(Arch::Aarch64, os);
            let types = TypeTable::new(&target);
            let mapper = Aarch64Mapper;
            let cases = [
                (
                    Opcode::SCvtF,
                    types.float16_id,
                    types.int128_id,
                    "__floattisf",
                ),
                (
                    Opcode::UCvtF,
                    types.float16_id,
                    types.uint128_id,
                    "__floatuntisf",
                ),
                (
                    Opcode::FCvtS,
                    types.int128_id,
                    types.float16_id,
                    "__fixsfti",
                ),
                (
                    Opcode::FCvtU,
                    types.uint128_id,
                    types.float16_id,
                    "__fixunssfti",
                ),
            ];
            for (op, to, from, name) in cases {
                let insn =
                    make_convert_insn(op, to, types.value_bits(to), from, types.value_bits(from));
                let mut func = make_minimal_func(&types);
                let mut ctx = MappingCtx {
                    func: &mut func,
                    types: &types,
                    target: &target,
                };
                let MappedInsn::Replace(insns) = mapper.map_insn(&insn, &mut ctx) else {
                    panic!("{os:?} {op:?}: left unlowered");
                };
                let ops: Vec<Opcode> = insns.iter().map(|i| i.op).collect();
                let call = insns
                    .iter()
                    .find(|i| i.op == Opcode::Call)
                    .expect("a library call");
                assert_eq!(
                    call.extra().func_name.as_deref(),
                    Some(name),
                    "{os:?} {op:?}"
                );
                // The half side is a native conversion to or from `float`.
                let fcvt = insns
                    .iter()
                    .find(|i| i.op == Opcode::FCvtF)
                    .expect("a half <-> float conversion");
                let (half, single) = if matches!(op, Opcode::SCvtF | Opcode::UCvtF) {
                    assert_eq!(ops, [Opcode::Call, Opcode::FCvtF]);
                    (fcvt.typ, fcvt.src_typ)
                } else {
                    assert_eq!(ops, [Opcode::FCvtF, Opcode::Call]);
                    (fcvt.src_typ, fcvt.typ)
                };
                assert_eq!(half, Some(types.float16_id));
                assert_eq!(single, Some(types.float_id));
            }
        }
    }
}
