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

use super::asm_probe::{asm_for, body_of, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX};

/// A floating vector of four bytes or fewer on aarch64, by each platform
/// compiler's rule, read off `aarch64-linux-gnu-gcc -O2 -S` and, for Darwin,
/// clang's coercion of such a vector to `i32` lowered by
/// `llc -mtriple=arm64-apple-macos`:
///
/// - gcc lays it on the stack and sends the general-register arguments after
///   it there too (`after` reads `i` from the stack, `call` stores the `7`
///   above the vector), leaves the V registers alone (`fp_after` reads `f`
///   in S0), and returns it in W0.
/// - clang passes it in a general register and returns it in V0: S0, or H0
///   for the two-byte `v1hf`.
#[test]
fn vector_abi_small_float_vectors_on_aarch64() {
    let src = r#"
typedef float v1sf __attribute__((vector_size(4)));
typedef _Float16 v1hf __attribute__((vector_size(2)));
long after(v1sf a, long i) { return i; }
float fp_after(v1sf a, float f) { return f; }
v1sf r1(float x) { v1sf r = {x}; return r; }
v1hf r1h(_Float16 x) { v1hf r = {x}; return r; }
long ext(v1sf, long);
long call(float x) { v1sf a = {x}; return ext(a, 7); }
"#;
    for triple in [AARCH64_LINUX, AARCH64_DARWIN] {
        let asm = asm_for("vec_small_float", triple, src);
        let linux = triple == AARCH64_LINUX;
        let after = body_of(&asm, "after");
        assert_eq!(names(after, &["x1"]), !linux, "{triple} after:\n{after}");
        assert_eq!(
            after.contains("ldr x0, [x29"),
            linux,
            "{triple} after:\n{after}"
        );
        let fp_after = body_of(&asm, "fp_after");
        assert!(!fp_after.contains("ldr"), "{triple} fp_after:\n{fp_after}");
        for f in ["r1", "r1h"] {
            let body = body_of(&asm, f);
            assert_eq!(names(body, &["w0", "x0"]), linux, "{triple} {f}:\n{body}");
        }
        let call = body_of(&asm, "call");
        let before_call = call.split_once("bl ").map_or(call, |(head, _)| head);
        assert_eq!(
            names(before_call, &["x1", "w1"]),
            !linux,
            "{triple} call:\n{call}"
        );
        assert_eq!(
            before_call.contains("[sp, #8]"),
            linux,
            "{triple} call:\n{call}"
        );
    }
}

/// gcc's System V passes and returns `v2hf` in XMM0, as the SSE class of its
/// one eightbyte, and the general registers stay free for what follows.
#[test]
fn vector_abi_v2hf_travels_in_xmm0_on_x86_64() {
    let src = r#"
typedef _Float16 v2hf __attribute__((vector_size(4)));
v2hf swap(v2hf a, int k) { v2hf r = {a[1], a[0]}; return r; }
v2hf ext(v2hf, int);
int call(v2hf a) { return ext(a, 3)[1] > 0; }
"#;
    let asm = asm_for("vec_v2hf", X86_64_LINUX, src);
    let swap = body_of(&asm, "swap");
    assert!(names(swap, &["xmm0"]), "swap:\n{swap}");
    let call = body_of(&asm, "call");
    let before_call = call.split_once("call ").map_or(call, |(head, _)| head);
    assert!(before_call.contains("$3, %edi"), "call:\n{call}");
}

/// Whether `asm` names any of the registers `regs`.
fn names(asm: &str, regs: &[&str]) -> bool {
    asm.split(|c: char| !c.is_ascii_alphanumeric())
        .any(|t| regs.contains(&t))
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

/// clang on Darwin passes every vector of four bytes or fewer as `i32`, so
/// the one- and two-byte integer vectors `v2qi`, `v1hi` and `v1qi` each take
/// four bytes of the stack once the general registers run out: offsets 0, 4
/// and 8, read off `llc -mtriple=arm64-apple-macos` for `i32` arguments.
/// They were passed as the integer of their own size, which Darwin packs
/// at its own size -- offsets 0, 2 and 4. Linux gives each an eight-byte
/// slot either way.
#[test]
fn vector_abi_darwin_stacks_small_integer_vectors_as_i32() {
    let src = r#"
typedef char v2qi __attribute__((vector_size(2)));
typedef short v1hi __attribute__((vector_size(2)));
typedef char v1qi __attribute__((vector_size(1)));
void take(v2qi, v2qi, v2qi, v2qi, v2qi, v2qi, v2qi, v2qi, v2qi, v1hi, v1qi);
void caller(v2qi a, v1hi h, v1qi q) { take(a, a, a, a, a, a, a, a, a, h, q); }
int callee(v2qi a0, v2qi a1, v2qi a2, v2qi a3, v2qi a4, v2qi a5, v2qi a6, v2qi a7,
           v2qi a8, v1hi h, v1qi q) {
    return a8[1] + h[0] * 3 + q[0] * 5;
}
"#;
    for (triple, slot) in [(AARCH64_DARWIN, 4), (AARCH64_LINUX, 8)] {
        let asm = asm_for("vec_small_int_stack", triple, src);
        let caller = body_of(&asm, "caller");
        let before_call = caller.split_once("bl ").map_or(caller, |(head, _)| head);
        for at in [
            "[sp]".to_string(),
            format!("[sp, #{slot}]"),
            format!("[sp, #{}]", 2 * slot),
        ] {
            assert!(
                before_call.contains(&at),
                "{triple} caller, {at}:\n{caller}"
            );
        }
        assert!(
            !before_call.contains("[sp, #2]"),
            "{triple} caller:\n{caller}"
        );
        // The callee reads them above its frame, at the same offsets.
        let callee = body_of(&asm, "callee");
        let frame: i64 = callee
            .split_once("stp x29, x30, [sp, #-")
            .and_then(|(_, rest)| rest.split_once(']'))
            .and_then(|(n, _)| n.parse().ok())
            .unwrap_or_else(|| panic!("{triple} callee frame:\n{callee}"));
        for k in 0..3 {
            let at = format!("[x29, #{}]", frame + k * slot);
            assert!(callee.contains(&at), "{triple} callee, {at}:\n{callee}");
        }
    }
}

/// Whether `asm` names V0 at any width: `v0`, `d0`, `s0`, `h0` or `b0`.
fn mentions_v0(asm: &str) -> bool {
    asm.split(|c: char| !c.is_ascii_alphanumeric())
        .any(|t| matches!(t, "v0" | "d0" | "s0" | "h0" | "b0"))
}
