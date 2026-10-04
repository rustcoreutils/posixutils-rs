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
