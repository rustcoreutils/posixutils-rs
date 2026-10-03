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
