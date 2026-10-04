//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for GNU vectors: how a vector value is copied and which
// operations are lowered to the target's packed instructions.
//

use super::test_linearize::{insns_of, linearize_source};
use crate::ir::Opcode;
use crate::target::Target;

const DECLS: &str = "typedef int v4si __attribute__((vector_size(16)));\n\
    typedef float v4sf __attribute__((vector_size(16)));\n\
    typedef int v2si __attribute__((vector_size(8)));\n";

/// The widths of the loads in function `name`.
fn load_widths(src: &str, name: &str) -> Vec<u32> {
    let module = linearize_source(&format!("{DECLS}{src}"), &Target::host());
    insns_of(&module, name)
        .into_iter()
        .filter(|i| i.op == Opcode::Load)
        .map(|i| i.size)
        .collect()
}

/// A whole vector moves in one access of its own width -- the 16-byte
/// carrier, a single XMM or Q register -- not in eight-byte chunks.
#[test]
fn test_vector_copy_is_one_access() {
    for (src, name, width) in [
        ("void cp(v4si *d, v4si *s) { *d = *s; }", "cp", 128),
        ("void pos(v4si *d, v4si *s) { *d = +*s; }", "pos", 128),
        (
            "void cast(v4sf *d, v4si *s) { *d = (v4sf)*s; }",
            "cast",
            128,
        ),
        ("void cp8(v2si *d, v2si *s) { *d = *s; }", "cp8", 64),
    ] {
        let widths = load_widths(src, name);
        assert!(!widths.is_empty(), "{name}: no load");
        assert!(widths.iter().all(|&w| w == width), "{name}: {widths:?}");
    }
}

/// The operations in function `name` of `src`, linearized for `target`.
fn ops_of(src: &str, name: &str, target: &Target) -> Vec<Opcode> {
    let module = linearize_source(&format!("{DECLS}{src}"), target);
    insns_of(&module, name).into_iter().map(|i| i.op).collect()
}

/// A vector operation the target computes with a packed instruction is one
/// `Simd`, with no lane loop; one it does not is the lane loop, with none.
/// The binary operators, their compound assignments and the unary ones all
/// take the same decision (`arch::simd::native`).
#[test]
fn test_native_vector_operations_are_one_instruction() {
    use crate::ir::SimdOp;
    use crate::target::{Arch, Os};
    let x86 = Target::new(Arch::X86_64, Os::Linux);
    let a64 = Target::new(Arch::Aarch64, Os::Linux);
    let one_simd = |src: &str, target: &Target, simd: SimdOp| {
        let ops = ops_of(src, "f", target);
        assert_eq!(
            ops.iter().filter(|&&o| o == Opcode::Simd(simd)).count(),
            1,
            "{src}: {ops:?}"
        );
        let lane_ops = [
            Opcode::Add,
            Opcode::Sub,
            Opcode::Xor,
            Opcode::Not,
            Opcode::Neg,
        ];
        let fp_ops = [Opcode::FAdd, Opcode::FDiv, Opcode::FNeg];
        assert!(
            !ops.iter()
                .any(|o| lane_ops.contains(o) || fp_ops.contains(o)),
            "{src}: a lane loop is left: {ops:?}"
        );
    };
    let no_simd = |src: &str, target: &Target| {
        let ops = ops_of(src, "f", target);
        assert!(!ops.iter().any(|o| matches!(o, Opcode::Simd(_))), "{src}");
    };
    for (src, simd) in [
        (
            "void f(v4si *d, v4si *a, v4si *b) { *d = *a + *b; }",
            SimdOp::Add,
        ),
        ("void f(v4si *d, v4si *a) { *d -= *a; }", SimdOp::Sub),
        (
            "void f(v2si *d, v2si *a, v2si *b) { *d = *a ^ *b; }",
            SimdOp::Xor,
        ),
        ("void f(v4si *d, v4si *a) { *d = ~*a; }", SimdOp::Not),
        ("void f(v4si *d, v4si *a) { *d = -*a; }", SimdOp::Neg),
        (
            "void f(v4sf *d, v4sf *a, v4sf *b) { *d = *a / *b; }",
            SimdOp::FDiv,
        ),
        ("void f(v4sf *d, v4sf *a) { *d = -*a; }", SimdOp::FNeg),
    ] {
        one_simd(src, &x86, simd);
        one_simd(src, &a64, simd);
    }
    // Division of integers stays lane by lane everywhere; eight bytes of
    // floats does on x86-64 alone.
    let div = "void f(v4si *d, v4si *a, v4si *b) { *d = *a / *b; }";
    no_simd(div, &x86);
    no_simd(div, &a64);
    let v2sf = "typedef float v2sf __attribute__((vector_size(8)));\n\
                void f(v2sf *d, v2sf *a, v2sf *b) { *d = *a + *b; }";
    no_simd(v2sf, &x86);
    one_simd(v2sf, &a64, SimdOp::FAdd);
}

/// A scalar operand is spread to every lane (`Splat`) for the packed
/// operation, but a shift count stays one scalar where the target shifts
/// every lane by one count -- SSE2 does, NEON shifts by per-lane counts and
/// takes the splat. A multiply is native where the target has the width.
#[test]
fn test_native_splat_shift_and_multiply() {
    use crate::ir::SimdOp;
    use crate::target::{Arch, Os};
    let x86 = Target::new(Arch::X86_64, Os::Linux);
    let a64 = Target::new(Arch::Aarch64, Os::Linux);
    let simds = |src: &str, target: &Target| -> Vec<SimdOp> {
        ops_of(src, "f", target)
            .into_iter()
            .filter_map(|o| match o {
                Opcode::Simd(s) => Some(s),
                _ => None,
            })
            .collect()
    };
    let splat_add = "void f(v4sf *d, v4sf *a, float s) { *d = *a + s; }";
    for t in [&x86, &a64] {
        assert_eq!(simds(splat_add, t), [SimdOp::Splat, SimdOp::FAdd]);
    }
    let shift = "void f(v4si *d, v4si *a, int s) { *d = *a << s; }";
    assert_eq!(simds(shift, &x86), [SimdOp::ShlScalar]);
    assert_eq!(simds(shift, &a64), [SimdOp::Splat, SimdOp::Shl]);
    let right = "typedef unsigned v4su __attribute__((vector_size(16)));\n\
                 void f(v4su *d, v4su *a, v4su *c) { *d = *a >> *c; }";
    assert!(simds(right, &x86).is_empty(), "SSE2 has no per-lane counts");
    assert_eq!(simds(right, &a64), [SimdOp::Lsr]);
    let mul = "void f(v4si *d, v4si *a, v4si *b) { *d = *a * *b; }";
    assert!(simds(mul, &x86).is_empty(), "SSE2 multiplies words only");
    assert_eq!(simds(mul, &a64), [SimdOp::Mul]);
}

/// A comparison of vectors is one packed compare giving the mask: `<` and
/// `<=` are `>` and `>=` of the operands swapped, and the lane type of a
/// mixed-signedness compare is unsigned. SSE2 has no unsigned order.
#[test]
fn test_native_comparisons() {
    use crate::ir::SimdOp;
    use crate::target::{Arch, Os};
    let x86 = Target::new(Arch::X86_64, Os::Linux);
    let a64 = Target::new(Arch::Aarch64, Os::Linux);
    let simds = |src: &str, target: &Target| -> Vec<SimdOp> {
        ops_of(src, "f", target)
            .into_iter()
            .filter_map(|o| match o {
                Opcode::Simd(s) => Some(s),
                _ => None,
            })
            .collect()
    };
    for (src, x86_want, a64_want) in [
        (
            "void f(v4si *d, v4si *a, v4si *b) { *d = *a == *b; }",
            Some(SimdOp::CmpEq),
            SimdOp::CmpEq,
        ),
        (
            "void f(v4si *d, v4si *a, v4si *b) { *d = *a < *b; }",
            Some(SimdOp::CmpGt),
            SimdOp::CmpGt,
        ),
        (
            "void f(v4si *d, v4si *a, v4si *b) { *d = *a >= *b; }",
            Some(SimdOp::CmpGe),
            SimdOp::CmpGe,
        ),
        (
            "void f(v4si *d, v4sf *a, v4sf *b) { *d = *a != *b; }",
            Some(SimdOp::FCmpNe),
            SimdOp::FCmpNe,
        ),
        (
            "void f(v4si *d, v4sf *a, v4sf *b) { *d = *a <= *b; }",
            Some(SimdOp::FCmpGe),
            SimdOp::FCmpGe,
        ),
        (
            "typedef unsigned v4su __attribute__((vector_size(16)));\n\
             void f(v4si *d, v4si *a, v4su *b) { *d = *a > *b; }",
            None,
            SimdOp::CmpGtU,
        ),
    ] {
        assert_eq!(
            simds(src, &x86),
            x86_want.into_iter().collect::<Vec<_>>(),
            "{src}"
        );
        assert_eq!(simds(src, &a64), [a64_want], "{src}");
    }
}

/// A constant shuffle keeping the operands' shape, and a same-width
/// conversion between integer and floating lanes, are one packed operation
/// where the target has one: any shuffle on NEON (`tbl`), dword and qword
/// ones on SSE2; signed 32-bit conversions on SSE2, all on NEON. A shuffle
/// to another length stays lane by lane.
#[test]
fn test_native_shuffles_and_conversions() {
    use crate::ir::SimdOp;
    use crate::target::{Arch, Os};
    let x86 = Target::new(Arch::X86_64, Os::Linux);
    let a64 = Target::new(Arch::Aarch64, Os::Linux);
    let count = |src: &str, target: &Target| {
        ops_of(src, "f", target)
            .into_iter()
            .filter(|o| matches!(o, Opcode::Simd(_)))
            .count()
    };
    let shuffle = |src: &str, target: &Target| {
        ops_of(src, "f", target)
            .into_iter()
            .any(|o| matches!(o, Opcode::Simd(SimdOp::Shuffle)))
    };
    let rev = "void f(v4si *d, v4si *a) { *d = __builtin_shufflevector(*a, *a, 3, 2, 1, 0); }";
    let words = "typedef short v8hi __attribute__((vector_size(16)));\n\
                 void f(v8hi *d, v8hi *a, v8hi *b) \
                 { *d = __builtin_shufflevector(*a, *b, 0, 8, 1, 9, 2, 10, 3, 11); }";
    let narrow = "void f(v2si *d, v4si *a) { *d = __builtin_shufflevector(*a, *a, 1, 0); }";
    assert!(shuffle(rev, &x86) && shuffle(rev, &a64));
    assert!(!shuffle(words, &x86) && shuffle(words, &a64));
    assert!(!shuffle(narrow, &x86) && !shuffle(narrow, &a64));
    let to_float = "void f(v4sf *d, v4si *a) { *d = __builtin_convertvector(*a, v4sf); }";
    let unsigned = "typedef unsigned v4su __attribute__((vector_size(16)));\n\
                    void f(v4sf *d, v4su *a) { *d = __builtin_convertvector(*a, v4sf); }";
    for t in [&x86, &a64] {
        assert!(ops_of(to_float, "f", t).contains(&Opcode::Simd(SimdOp::CvtSF)));
    }
    assert_eq!(count(unsigned, &x86), 0);
    assert!(ops_of(unsigned, "f", &a64).contains(&Opcode::Simd(SimdOp::CvtUF)));
}
