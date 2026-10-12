//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Register allocation, seen in the assembly: properties a program's exit
// status cannot show. Moved from `tests/codegen/regalloc.rs`.
//

use super::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, body_of, stack_use, AARCH64_DARWIN,
    AARCH64_LINUX, X86_64_LINUX,
};

/// A value is read back at the width it was stored at.
///
/// A comparison's result is an `int`, and spilled to the frame it is stored
/// as one -- `str w16, [x29, #136]` -- but `Cbr` and `Select` read their
/// condition as sixty-four bits whatever it was: `ldr x9, [x29, #136]`. The
/// upper half is whatever the frame held there, and only the prologue's
/// zeroing of the whole frame made it 0. Fourteen comparisons live across a
/// call, more than aarch64 has callee-saved registers for, put some of them
/// in the frame.
///
/// The aarch64 run of the same program is the integration test of the same
/// name.
#[test]
fn codegen_a_spilled_condition_is_read_at_its_own_width() {
    let src = r#"
__attribute__((noinline)) int touch(int x) { return x + 1; }
__attribute__((noinline)) int many_fcmp(double a, double b) {
    int c0 = a < b, c1 = a > b, c2 = a <= b, c3 = a >= b, c4 = a == b,
        c5 = a != b, c6 = a < b, c7 = a > b, c8 = a <= b, c9 = a >= b,
        c10 = a == b, c11 = a != b, c12 = a < b, c13 = a > b;
    touch(0);
    int r = 0;
    if (c0) r |= 1;    if (c1) r |= 2;    if (c2) r |= 4;    if (c3) r |= 8;
    if (c4) r |= 16;   if (c5) r |= 32;   if (c6) r |= 64;   if (c7) r |= 128;
    if (c8) r |= 256;  if (c9) r |= 512;  if (c10) r |= 1024; if (c11) r |= 2048;
    if (c12 ? c13 : 0) r |= 4096;
    return r;
}
"#;
    for opt in ["-O1", "-O2"] {
        let asm = asm_for_with("spilled_cond", AARCH64_LINUX, src, &[opt]);
        let body = body_of(&asm, "many_fcmp");
        let mut stored_w: std::collections::HashSet<&str> = std::collections::HashSet::new();
        for line in body.lines().map(str::trim) {
            let Some((op, rest)) = line.split_once(' ') else {
                continue;
            };
            let Some((reg, addr)) = rest.split_once(", ") else {
                continue;
            };
            if op == "str" && reg.starts_with('w') {
                stored_w.insert(addr);
            }
            assert!(
                !(op == "ldr" && reg.starts_with('x') && stored_w.contains(addr)),
                "at {opt}, `{line}` reads 64 bits of a slot stored as 32:\n{body}"
            );
        }
    }
}

/// Each value of `chain` is the result of one call and an argument of the
/// next: it is never live *across* a call, so it needs no callee-saved
/// register, and the incoming argument, already in the register the first
/// call takes it in, needs no frame slot. The frame is gcc's: the return
/// address and the saved frame pointer.
///
/// Every call position used to count as crossing for its own result and
/// for every value whose positions merely spanned it, so each of these
/// values took `%rbx` or `%r12`, and `a` a frame slot: 48 bytes.
#[test]
fn values_between_calls_take_no_callee_saved_register() {
    let src = "long g(long);\n\
               long chain(long a) {\n\
                   long x = g(a) + 1; long y = g(x) + 2; long z = g(y) + 3;\n\
                   return g(z);\n\
               }\n";
    let used = stack_use("chain_frame", X86_64_LINUX, src, &["-O2"]);
    assert_eq!(used["chain"], 16, "{used:?}");
    let asm = asm_for_with("chain_regs", X86_64_LINUX, src, &["-O2"]);
    for reg in ["%rbx", "%r12", "%r13", "%r14", "%r15"] {
        assert_body_lacks(&asm, "chain", reg, "no value lives across a call");
    }
}

/// `x` is computed before a branch and read only on the arm that makes no
/// call; the arm with the call is laid out between them, so the hull of
/// `x`'s positions spans the call, but `x` is dead there. It takes a
/// caller-saved register: no callee-saved register is saved, on either
/// target.
#[test]
fn a_value_whose_positions_span_a_call_it_is_dead_across_is_caller_saved() {
    let src = "long g(long);\n\
               long spans_call(long a, long b) {\n\
                   long x = a * b;\n\
                   if (b > 3) return g(a) + 1;\n\
                   return (x ^ a) + x;\n\
               }\n";
    let used = stack_use("spans_call_frame", X86_64_LINUX, src, &["-O2"]);
    assert_eq!(used["spans_call"], 16, "{used:?}");
    let asm = asm_for_with("spans_call_x86", X86_64_LINUX, src, &["-O2"]);
    for reg in ["%rbx", "%r12", "%r13", "%r14", "%r15"] {
        assert_body_lacks(&asm, "spans_call", reg, "x is dead across the call");
    }
    let asm = asm_for_with("spans_call_a64", AARCH64_LINUX, src, &["-O2"]);
    for reg in ["x19", "x20", "x21"] {
        assert_body_lacks(&asm, "spans_call", reg, "x is dead across the call");
    }
}

/// The same for a constraint: `x`'s positions span an `idivq`, which
/// writes `%rax` and `%rdx`, but `x` is dead there, so it may take `%rax`
/// -- the first register the colorer tries.
#[test]
fn a_value_dead_across_a_distant_divide_may_take_rax() {
    let src = "long spans_div(long a, long b, long c) {\n\
                   long x = a - b;\n\
                   if (c > 3) return a / c;\n\
                   return x + b;\n\
               }\n";
    let asm = asm_for_with("spans_div", X86_64_LINUX, src, &["-O2"]);
    let body = body_of(&asm, "spans_div");
    let sub = body
        .lines()
        .map(str::trim)
        .find(|l| l.starts_with("subq"))
        .unwrap_or_else(|| panic!("no subq in:\n{body}"));
    assert!(sub.ends_with("%rax"), "`{sub}`:\n{body}");
}

/// The lines of `func` that move a floating-point or vector register to or
/// from the frame: an `xmm` register and an `%rbp` address on x86-64, a
/// `d`/`s`/`q` register and an `x29` address on aarch64.
fn fp_frame_accesses<'a>(asm: &'a str, func: &str) -> Vec<&'a str> {
    body_of(asm, func)
        .lines()
        .map(str::trim)
        .filter(|l| {
            (l.contains("%xmm") && l.contains("(%rbp)"))
                || (l.contains("[x29")
                    && ["ldr d", "str d", "ldr s", "str s", "ldr q", "str q"]
                        .iter()
                        .any(|op| l.starts_with(op)))
        })
        .collect()
}

/// Floating-point values that live across blocks but cross no call are
/// colored like any other: none of them is kept in the frame, and the leaf
/// below needs no frame beyond its frame record -- 16 bytes, where gcc,
/// omitting the frame pointer, needs 8 on x86-64 and 0 on aarch64. The
/// arguments stay in the registers they arrived in.
///
/// Every such value used to be sent to a frame slot whatever the register
/// pressure, a leftover from a linear-scan allocator that lost track of a
/// register across blocks; and on x86-64 every `double` parameter was
/// stored at entry too.
#[test]
fn fp_values_across_blocks_take_registers() {
    let branch = "double fp_branch(double a, double b, int k) {\n\
                      double x = a * b, y = a + b;\n\
                      if (k > 0) x = x * y + a;\n\
                      else if (k < 0) y = y - x * b;\n\
                      return x / y;\n\
                  }\n";
    let looped = "double fp_loop(const double *p, int n) {\n\
                      double s = 0.0, t = 1.0;\n\
                      for (int i = 0; i < n; i++) {\n\
                          double v = p[i];\n\
                          if (v > s) s = v; else t = t * v + 1.0;\n\
                      }\n\
                      return s * t;\n\
                  }\n";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let used = stack_use("fp_branch_frame", triple, branch, &["-O2"]);
        assert_eq!(used["fp_branch"], 16, "{triple}: {used:?}");
        for (name, src) in [("fp_branch", branch), ("fp_loop", looped)] {
            let asm = asm_for_with(name, triple, src, &["-O2"]);
            let spills = fp_frame_accesses(&asm, name);
            assert!(
                spills.is_empty(),
                "{triple}: {name} keeps a floating-point value in the frame: {spills:?}\n{}",
                body_of(&asm, name)
            );
        }
    }
}

/// AAPCS64 preserves the low 64 bits of V8-V15 across a call, so a `double`
/// live across one takes one of them, saved in pairs in the prologue -- the
/// arguments too, moved there at entry -- and is never stored to the frame
/// and read back: gcc's frame, `stp d8, d9`, on Linux and on Darwin alike.
///
/// Every such value used to be stored in the frame at its def and loaded
/// back after each call.
#[test]
fn doubles_across_calls_take_d8_to_d15() {
    let src = "double g(double);\n\
               double f2(double a, double b) {\n\
                   double x = g(a);\n\
                   return x + g(b) + a;\n\
               }\n";
    for triple in [AARCH64_LINUX, AARCH64_DARWIN] {
        let asm = asm_for_with("f2_d8", triple, src, &["-O2"]);
        assert_body_contains(&asm, "f2", "stp d8, d9", "a and b live across g(a)");
        let loads: Vec<&str> = fp_frame_accesses(&asm, "f2")
            .into_iter()
            .filter(|l| l.starts_with("ldr"))
            .collect();
        assert!(
            loads.is_empty(),
            "{triple}: a value is read back from the frame: {loads:?}\n{}",
            body_of(&asm, "f2")
        );
    }
    // gcc -O2 needs 48 too: the frame record and d8-d10.
    let used = stack_use("f2_d8_frame", AARCH64_LINUX, src, &["-O2"]);
    assert!(used["f2"] <= 48, "{used:?}");
}

/// Only the low half of V8-V15 survives a call, so a binary128 `long double`
/// live across one takes none of them: it is kept in the frame, and no
/// callee-saved V register is saved for it. (`h` and every `+` are calls.)
#[test]
fn binary128_across_calls_takes_no_callee_saved_v_register() {
    let src = "long double h(long double);\n\
               long double w(long double a, long double b) {\n\
                   long double x = h(a);\n\
                   return x + h(b) + a;\n\
               }\n";
    let asm = asm_for_with("binary128_across", AARCH64_LINUX, src, &["-O2"]);
    for reg in ["d8", "q8", "v8"] {
        assert_body_lacks(
            &asm,
            "w",
            reg,
            "a binary128 value loses its high half in v8",
        );
    }
}
