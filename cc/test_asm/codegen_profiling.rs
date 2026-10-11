//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-pg`: every function calls the profiler's counting routine on entry, in
// gcc's shape for each target, and x86-64's `-mfentry`, `-mrecord-mcount`
// and `-mnop-mcount` reshape that call as gcc's do.
//

use super::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, body_of, count_in_body, AARCH64_LINUX,
    X86_64_LINUX,
};

const PROGRAM: &str = r#"
extern int g(int);
int leaf(int x) { return x * 2; }
int f(int x) { return g(x) + x; }
__attribute__((no_instrument_function)) int quiet(int x) { return x + 5; }
static int sq(int x) { return x * x; }
int inl(int x) { return sq(x) + 1; }
"#;

/// The text before the first occurrence of `needle` in `func`'s body.
#[track_caller]
fn before<'a>(asm: &'a str, func: &str, needle: &str) -> &'a str {
    let body = body_of(asm, func);
    let at = body
        .find(needle)
        .unwrap_or_else(|| panic!("no `{needle}` in {func}:\n{body}"));
    &body[..at]
}

/// x86-64 without position-independent code: gcc's `1: call mcount`, once
/// the whole prologue is done -- the frame pointer, the callee-saved pushes
/// and the frame -- and before an argument register is touched, since
/// `mcount` preserves them. A leaf calls it too, and a function inlined into
/// another is counted only as the one it was inlined into.
#[test]
fn pg_x86_64_calls_mcount_after_the_prologue() {
    for level in ["-O0", "-O2"] {
        let asm = asm_for_with("pg_x86", X86_64_LINUX, PROGRAM, &[level, "-pg", "-fno-pic"]);
        let why = format!("-pg {level}");
        for func in ["leaf", "f", "inl"] {
            assert_body_contains(&asm, func, "\n1:\n    call mcount\n", &why);
            assert_eq!(count_in_body(&asm, func, "mcount"), 1, "{why} {func}");
            let prologue = before(&asm, func, "call mcount");
            assert!(prologue.contains("movq %rsp, %rbp"), "{why} {func}:\n{asm}");
            for arg in ["%edi", "%rdi"] {
                assert!(!prologue.contains(arg), "{why} {func}: {arg} touched first");
            }
        }
        assert_body_lacks(&asm, "quiet", "mcount", "no_instrument_function");
    }
    // The frame `f` keeps `x` in a callee-saved register; it is pushed first.
    let asm = asm_for_with("pg_x86", X86_64_LINUX, PROGRAM, &["-O2", "-pg", "-fno-pic"]);
    assert!(
        before(&asm, "f", "call mcount").contains("pushq %rbx"),
        "callee-saved pushes precede the call:\n{asm}"
    );
}

/// Position-independent code, PIE included, calls through the GOT, as gcc
/// does: `call *mcount@GOTPCREL(%rip)`.
#[test]
fn pg_x86_64_pic_calls_mcount_through_the_got() {
    for pic in ["-fPIC", "-fpic", "-fPIE", "-fpie"] {
        let asm = asm_for_with("pg_x86_pic", X86_64_LINUX, PROGRAM, &["-O2", "-pg", pic]);
        assert_body_contains(&asm, "leaf", "\n1:\n    call *mcount@GOTPCREL(%rip)\n", pic);
    }
}

/// `-p` is `-pg` on Linux, as in gcc; without either there is no call.
#[test]
fn pg_spellings_and_default() {
    let asm = asm_for_with("pg_p", X86_64_LINUX, PROGRAM, &["-O2", "-p", "-fno-pic"]);
    assert_body_contains(&asm, "leaf", "call mcount", "-p");
    let asm = asm_for_with("pg_none", X86_64_LINUX, PROGRAM, &["-O2"]);
    assert!(!asm.contains("mcount"), "no -pg:\n{asm}");
}

/// `-mfentry` calls `__fentry__` before the prologue, `-mrecord-mcount`
/// records each call's address in `__mcount_loc`, and `-mnop-mcount` leaves
/// a five-byte nop where the call would be -- each in gcc's text.
#[test]
fn pg_x86_64_mcount_machine_options() {
    let asm = asm_for_with(
        "pg_fentry",
        X86_64_LINUX,
        PROGRAM,
        &["-O2", "-pg", "-fno-pic", "-mfentry"],
    );
    assert_body_contains(&asm, "f", "\n1:\n    call __fentry__\n", "-mfentry");
    assert!(
        !before(&asm, "f", "call __fentry__").contains("pushq"),
        "-mfentry: before the prologue:\n{asm}"
    );
    assert!(!asm.contains("mcount"), "-mfentry: no mcount:\n{asm}");

    let asm = asm_for_with(
        "pg_fentry_pic",
        X86_64_LINUX,
        PROGRAM,
        &["-O2", "-pg", "-fPIC", "-mfentry"],
    );
    assert_body_contains(
        &asm,
        "f",
        "call *__fentry__@GOTPCREL(%rip)",
        "-mfentry -fPIC",
    );

    let record = "    .section __mcount_loc, \"a\",@progbits\n    .quad 1b\n    .previous\n";
    let asm = asm_for_with(
        "pg_record",
        X86_64_LINUX,
        PROGRAM,
        &["-O2", "-pg", "-fno-pic", "-mrecord-mcount"],
    );
    assert_body_contains(
        &asm,
        "f",
        &format!("\n1:\n    call mcount\n{record}"),
        "-mrecord-mcount",
    );

    let nop = "\n1:\n    .byte 0x0f, 0x1f, 0x44, 0x00, 0x00\n";
    let asm = asm_for_with(
        "pg_nop",
        X86_64_LINUX,
        PROGRAM,
        &["-O2", "-pg", "-fno-pic", "-mnop-mcount"],
    );
    assert_body_contains(&asm, "f", nop, "-mnop-mcount");
    assert_body_lacks(&asm, "f", "call mcount", "-mnop-mcount");

    let asm = asm_for_with(
        "pg_all",
        X86_64_LINUX,
        PROGRAM,
        &[
            "-O2",
            "-pg",
            "-fno-pic",
            "-mfentry",
            "-mrecord-mcount",
            "-mnop-mcount",
        ],
    );
    assert_body_contains(&asm, "f", &format!("{nop}{record}"), "all three");
    assert!(
        !before(&asm, "f", ".byte 0x0f").contains("pushq"),
        "all three: before the prologue:\n{asm}"
    );

    // Without `-pg` they do nothing.
    let asm = asm_for_with(
        "pg_mflags_alone",
        X86_64_LINUX,
        PROGRAM,
        &["-O2", "-fno-pic", "-mfentry", "-mrecord-mcount"],
    );
    assert!(
        !asm.contains("fentry") && !asm.contains("mcount"),
        "no -pg:\n{asm}"
    );
}

/// aarch64: gcc's `bl _mcount` with the function's return address in x0,
/// an ordinary call, so the arguments are kept across it.
#[test]
fn pg_aarch64_calls_underscore_mcount() {
    for level in ["-O0", "-O2"] {
        let asm = asm_for_with("pg_a64", AARCH64_LINUX, PROGRAM, &[level, "-pg"]);
        let why = format!("aarch64 -pg {level}");
        for func in ["leaf", "f", "inl"] {
            assert_body_contains(&asm, func, "bl _mcount", &why);
            assert_eq!(count_in_body(&asm, func, "_mcount"), 1, "{why} {func}");
            // The return address, from the frame record the prologue wrote.
            let setup = before(&asm, func, "bl _mcount");
            assert!(
                setup.contains("ldr x9, [x9, #8]") && setup.contains("stp x29, x30"),
                "{why} {func}:\n{setup}"
            );
        }
        assert_body_lacks(&asm, "quiet", "mcount", "no_instrument_function");
    }
}
