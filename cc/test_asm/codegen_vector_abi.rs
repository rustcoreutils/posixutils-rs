//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU vectors at call boundaries: the assembly and diagnostic half,
// compiled in process. The interop half, against gcc in every pairing,
// is `tests/codegen/vector_abi.rs`.
//

use super::asm_probe::{asm_for, body_of, AARCH64_DARWIN, AARCH64_LINUX};
use crate::test_compile::compile;

/// gcc's aarch64 passes a one-float vector on the stack along with the
/// arguments after it, and returns it in a general register -- like no type
/// c17 has. c17 refuses it there, and passes it in memory on System V as gcc
/// does.
#[test]
fn vector_abi_small_float_vector_is_refused_on_aarch64() {
    let src = "typedef float v1sf __attribute__((vector_size(4)));\nv1sf f(v1sf a) { return a; }\n";
    let a64 = compile("vec_abi_v1sf", src, &["--target=aarch64-unknown-linux-gnu"]);
    assert!(!a64.success, "aarch64 accepted it");
    assert!(
        a64.stderr
            .contains("c17 does not pass or return this vector type on this target"),
        "{}",
        a64.stderr
    );
    let x86 = compile("vec_abi_v1sf", src, &["--target=x86_64-unknown-linux-gnu"]);
    assert!(x86.success, "{}", x86.stderr);
}

/// clang -- Darwin's compiler -- returns an integer vector of four bytes or
/// fewer in V0: one lane in its low bits, several widened to fill D0
/// (`v2hi` as two 32-bit lanes, `v4qi` as four 16-bit ones). It passes one
/// in a general register, as gcc does on Linux, where the return stays in
/// W0. Read off `llc -mtriple=arm64-apple-macos` for clang's lowering; a
/// clang caller of a c17 callee returning one in W0 read garbage.
#[test]
fn vector_abi_darwin_returns_small_integer_vectors_in_v0() {
    let src = r#"
typedef short v2hi __attribute__((vector_size(4)));
typedef unsigned char v4qi __attribute__((vector_size(4)));
typedef int v1si __attribute__((vector_size(4)));
v2hi r2(v2hi a, v2hi b) { return a - b; }
v4qi r4(v4qi a) { return a + a; }
v1si r1(v1si a) { return a + 1; }
v2hi ext(v2hi);
int c2(v2hi a) { return ext(a)[1]; }
"#;
    for triple in [AARCH64_DARWIN, AARCH64_LINUX] {
        let asm = asm_for("vec_small_ret", triple, src);
        let darwin = triple == AARCH64_DARWIN;
        for f in ["r2", "r4", "r1"] {
            let body = body_of(&asm, f);
            assert_eq!(mentions_v0(body), darwin, "{triple} {f}:\n{body}");
        }
        // The caller takes the result from V0 on Darwin, W0 on Linux.
        let body = body_of(&asm, "c2");
        let after_call = body.split_once("bl").map(|(_, rest)| rest).unwrap_or("");
        assert_eq!(mentions_v0(after_call), darwin, "{triple} c2:\n{body}");
    }
}

/// Whether `asm` names V0 at any width: `v0`, `d0`, `s0`, `h0` or `b0`.
fn mentions_v0(asm: &str) -> bool {
    asm.split(|c: char| !c.is_ascii_alphanumeric())
        .any(|t| matches!(t, "v0" | "d0" | "s0" | "h0" | "b0"))
}
