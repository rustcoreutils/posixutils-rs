//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `__builtin_setjmp` / `__builtin_longjmp`: inline code over a
// five-word buffer, with no library call, on every target.
//

use super::asm_probe::{
    asm_for, assert_body_contains, assert_body_lacks, body_of, AARCH64_DARWIN, AARCH64_LINUX,
    X86_64_LINUX,
};

const PROGRAM: &str = r#"
void *buf[5];
extern void g(void);
int f(void) { if (__builtin_setjmp(buf)) return 1; g(); return 0; }
void j(void) { __builtin_longjmp(buf, 1); }
"#;

/// The setjmp stores gcc's three words -- frame pointer, resume address,
/// stack pointer -- and the longjmp reloads them and jumps indirectly. No
/// library function is called, and the prologue saves every callee-saved
/// register, since a longjmp skips the epilogues that would restore them.
#[test]
fn builtin_setjmp_x86_64_buffer_and_jump() {
    let asm = asm_for("builtin_setjmp_x86", X86_64_LINUX, PROGRAM);
    let why = "x86-64 __builtin_setjmp";
    for needle in [
        "movq %rbp, (%r10)",
        "leaq .L.sjlj_resume.",
        "movq %r11, 8(%r10)",
        "movq %rsp, 16(%r10)",
        "\n.L.sjlj_resume.",
    ] {
        assert_body_contains(&asm, "f", needle, why);
    }
    for reg in ["%rbx", "%r12", "%r13", "%r14", "%r15"] {
        assert_body_contains(&asm, "f", &format!("pushq {reg}"), why);
    }
    assert_body_lacks(&asm, "f", "setjmp", "no library call");
    let why = "x86-64 __builtin_longjmp";
    for needle in [
        "movq 8(%r11), %r10",
        "movq (%r11), %rbp",
        "movq 16(%r11), %rsp",
        "jmp *%r10",
    ] {
        assert_body_contains(&asm, "j", needle, why);
    }
    assert_body_lacks(&asm, "j", "longjmp", "no library call");
}

/// The same on aarch64, for Linux and Darwin alike: the code is the
/// target's, not the C library's.
#[test]
fn builtin_setjmp_aarch64_buffer_and_jump() {
    for (triple, resume, prefix) in [
        (AARCH64_LINUX, ".L.sjlj_resume.", ""),
        (AARCH64_DARWIN, "L.sjlj_resume.", "_"),
    ] {
        let asm = asm_for("builtin_setjmp_a64", triple, PROGRAM);
        let why = format!("{triple} __builtin_setjmp");
        for needle in [
            "str x29, [x9]",
            "str x10, [x9, #8]",
            "mov x10, sp",
            "str x10, [x9, #16]",
            &format!("adrp x10, {resume}"),
            &format!("\n{resume}"),
            "stp x19, x20",
            "stp x27, x28",
            "stp d8, d9",
            "stp d14, d15",
        ] {
            assert_body_contains(&asm, "f", needle, &why);
        }
        assert_body_lacks(&asm, "f", "setjmp", "no library call");
        assert_body_contains(&asm, "f", &format!("bl {prefix}g"), &why);
        let why = format!("{triple} __builtin_longjmp");
        for needle in [
            "ldr x10, [x9, #8]",
            "ldr x11, [x9, #16]",
            "ldr x29, [x9]",
            "mov sp, x11",
            "br x10",
        ] {
            assert_body_contains(&asm, "j", needle, &why);
        }
        assert_body_lacks(&asm, "j", "bl ", "no library call");
    }
}

/// A value read after the setjmp lives in memory across it: the resume path
/// restores no register but the frame and stack pointers.
#[test]
fn builtin_setjmp_keeps_no_value_in_a_register_across_it() {
    let src = r#"
void *buf[5];
extern void g(long);
long f(long a, long b) {
    long p = a * 7, q = b * 13;
    if (__builtin_setjmp(buf)) return p + q;
    g(p);
    return 0;
}
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for("builtin_setjmp_spill", triple, src);
        let body = body_of(&asm, "f");
        let resume = body
            .find("sjlj_resume.0:")
            .unwrap_or_else(|| panic!("{triple}: no resume label:\n{body}"));
        // Past the resume point, the sum is computed from frame slots.
        let after = &body[resume..];
        let frame_reads = after
            .lines()
            .filter(|l| l.contains("(%rbp)") || l.contains("[x29"))
            .count();
        assert!(
            frame_reads >= 2,
            "{triple}: p and q must be reloaded from the frame after the resume:\n{body}"
        );
    }
}
