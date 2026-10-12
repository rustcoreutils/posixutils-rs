//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fstack-clash-protection`: where the stack is probed as it grows, on
// each target. gcc 13's scheme, `target::StackClash`; that the probes keep a
// too-large frame off the memory beyond the guard, and that the programs
// still compute the same answers, is `tests/codegen/stack_clash.rs`.
//

use super::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, body_of, count_in_body, AARCH64_DARWIN,
    AARCH64_LINUX, X86_64_LINUX,
};

const X86_64_DARWIN: &str = "x86_64-apple-darwin";

/// A frame of each size that matters, a dynamic allocation of each kind, and
/// a by-value argument too large to stack without probing.
const FRAMES: &str = r#"
void use(char *);
struct huge { char c[20000]; };
void take(struct huge);
void small(void) { char b[64]; use(b); }
void f4k(void) { char b[4096]; use(b); }
void f12k(void) { char b[12000]; use(b); }
void f16k(void) { char b[16384 + 512]; use(b); }
void f40k(void) { char b[40000]; use(b); }
void f64k(void) { char b[65000]; use(b); }
void f200k(void) { char b[200000]; use(b); }
void f1m(void) { char b[1 << 20]; use(b); }
void dyn_alloca(int n) { use(__builtin_alloca(n)); }
void dyn_vla(int n) { char b[n]; use(b); }
void pass_huge(struct huge *p) { take(*p); }
"#;

const PROBED: &[&str] = &["-O2", "-fstack-clash-protection"];

fn x86(triple: &str, flags: &[&str]) -> String {
    asm_for_with("stack_clash_x86", triple, FRAMES, flags)
}

/// x86-64: a frame of a probe interval or more is allocated an interval at
/// a time, each followed by a probe at the new stack pointer; past four
/// intervals, in a loop counted down to `%r11`. What is left under one
/// interval is not probed: the next call's return address is. A smaller
/// frame is allocated as before.
#[test]
fn codegen_stack_clash_x86_64_frames() {
    for triple in [X86_64_LINUX, X86_64_DARWIN] {
        let asm = x86(triple, PROBED);
        let probe = "orq $0, (%rsp)";
        assert_body_lacks(&asm, "small", "orq", "a small frame needs no probe");
        assert_eq!(count_in_body(&asm, "f4k", probe), 1, "{triple}: 4 KiB");
        assert_eq!(count_in_body(&asm, "f12k", probe), 2, "{triple}: 12 KiB");
        assert_eq!(
            count_in_body(&asm, "f12k", "subq $4096, %rsp"),
            2,
            "{triple}: 12 KiB is two unrolled intervals"
        );
        for f in ["f16k", "f1m"] {
            assert_eq!(count_in_body(&asm, f, probe), 1, "{triple}: {f} loops");
            assert_body_contains(&asm, f, "cmpq %r11, %rsp", "the loop's end");
        }
        assert_body_contains(&asm, "f1m", "leaq -1048576(%rsp), %r11", "1 MiB");

        // The same functions without the option: no probe anywhere.
        let plain = x86(triple, &["-O2"]);
        for f in [
            "f4k",
            "f12k",
            "f16k",
            "f1m",
            "dyn_alloca",
            "dyn_vla",
            "pass_huge",
        ] {
            assert_body_lacks(&plain, f, "orq $0", "unprobed without the option");
        }
    }
}

/// x86-64 `alloca` and VLAs: moved down an interval at a time, each probed
/// at its top -- safe however much of its frame the prologue left
/// unprobed -- then the rest, then the new stack pointer itself.
#[test]
fn codegen_stack_clash_x86_64_dynamic() {
    for triple in [X86_64_LINUX, X86_64_DARWIN] {
        let asm = x86(triple, PROBED);
        for f in ["dyn_alloca", "dyn_vla"] {
            for want in [
                "subq $4096, %rsp",
                "orq $0, 4088(%rsp)",
                "orq $0, -8(%rsp)",
                "movq %r11, %rsp",
                "orq $0, (%rsp)",
            ] {
                assert_body_contains(&asm, f, want, "a probed dynamic allocation");
            }
        }
        // A by-value argument's area of 20000 bytes is probed the same way.
        assert_body_contains(&asm, "pass_huge", "orq $0, 4088(%rsp)", "argument area");
        assert_body_lacks(&asm, "pass_huge", "subq $20000, %rsp", "never in one step");
    }
}

/// `--param stack-clash-protection-guard-size=` and `-probe-interval=` move
/// the threshold and the step on x86-64.
#[test]
fn codegen_stack_clash_x86_64_params() {
    let flags = |param: &'static str| {
        let mut f = PROBED.to_vec();
        f.push(param);
        f
    };
    let asm = x86(
        X86_64_LINUX,
        &flags("--param=stack-clash-protection-guard-size=16"),
    );
    assert_body_lacks(&asm, "f40k", "orq", "under a 64 KiB guard");
    assert_body_contains(&asm, "f200k", "orq $0, (%rsp)", "past a 64 KiB guard");
    let asm = x86(
        X86_64_LINUX,
        &flags("--param=stack-clash-protection-probe-interval=13"),
    );
    assert_body_lacks(&asm, "f4k", "orq", "under an 8 KiB interval");
    assert_eq!(count_in_body(&asm, "f12k", "subq $8192, %rsp"), 1);
    assert_body_contains(&asm, "dyn_vla", "orq $0, 8184(%rsp)", "8 KiB steps");
}

fn a64(triple: &str, flags: &[&str]) -> String {
    asm_for_with("stack_clash_a64", triple, FRAMES, flags)
}

/// aarch64: the frame record stored at the bottom of the frame probes any
/// frame under the 64 KiB guard less the caller's 1 KiB. Past that the frame
/// is allocated 64 KiB at a time, each probed 1 KiB above the new stack
/// pointer, in a loop past four intervals -- with the CFA in the loop's end
/// register while `sp` moves -- and a residual of 63 KiB or more is probed
/// too.
#[test]
fn codegen_stack_clash_aarch64_frames() {
    for triple in [AARCH64_LINUX, AARCH64_DARWIN] {
        let asm = a64(triple, PROBED);
        let probe = "str xzr, [sp, #1024]";
        for f in ["small", "f4k", "f16k", "f40k"] {
            assert_body_lacks(&asm, f, "xzr, [sp", "under the guard");
        }
        assert_eq!(count_in_body(&asm, "f64k", probe), 1, "{triple}: 64 KiB");
        assert_eq!(count_in_body(&asm, "f200k", probe), 3, "{triple}: 200 KiB");
        assert_body_lacks(&asm, "f200k", "cmp sp", "three intervals unroll");
        assert_eq!(
            count_in_body(&asm, "f1m", probe),
            1,
            "{triple}: 1 MiB loops"
        );
        assert_body_contains(&asm, "f1m", "cmp sp, x17", "the loop's end");
        assert_body_contains(&asm, "f1m", ".cfi_def_cfa x17, 1048576", "CFA");
        assert_body_contains(&asm, "f1m", ".cfi_def_cfa sp, 1048576", "CFA");

        let plain = a64(triple, &["-O2"]);
        for f in ["f64k", "f200k", "f1m", "dyn_alloca", "dyn_vla", "pass_huge"] {
            assert_body_lacks(&plain, f, "xzr, [sp", "unprobed without the option");
        }
    }
}

/// aarch64 `alloca` and VLAs: whole intervals probed 1 KiB up, then the
/// residual, probed 1 KiB up when it is past the guard's 63 KiB, and the new
/// stack pointer probed last, so the next call's 1 KiB allowance starts
/// from it. A stacked argument area over 1 KiB ends the same way.
#[test]
fn codegen_stack_clash_aarch64_dynamic() {
    for triple in [AARCH64_LINUX, AARCH64_DARWIN] {
        let asm = a64(triple, PROBED);
        for f in ["dyn_alloca", "dyn_vla"] {
            for want in [
                "sub sp, sp, #65536",
                "str xzr, [sp, #1024]",
                "str xzr, [sp]",
            ] {
                assert_body_contains(&asm, f, want, "a probed dynamic allocation");
            }
        }
    }
}

/// A call stacking `n` arguments of eight bytes, beyond the eight that go
/// in registers.
fn stacked_call(n: usize) -> String {
    let params = vec!["long"; n + 8].join(", ");
    let args: Vec<String> = (0..n + 8).map(|i| format!("{i}L")).collect();
    format!(
        "void callee({params});\nvoid caller(void) {{ callee({}); }}\n",
        args.join(", ")
    )
}

/// aarch64 moves `sp` for each call's stacked arguments: an area of up to
/// 1 KiB is within the allowance a callee assumes, and needs no probe; a
/// larger one is probed as an `alloca` of that size is, ending at the new
/// stack pointer. (A large aggregate goes by reference on aarch64, so only
/// many arguments make a large area.) x86-64's area for the same call gets
/// the same treatment once it could pass the guard.
#[test]
fn codegen_stack_clash_argument_areas() {
    for (n, probed) in [(100, false), (140, true)] {
        let src = stacked_call(n);
        for triple in [AARCH64_LINUX, AARCH64_DARWIN] {
            let asm = asm_for_with("stack_clash_args", triple, &src, PROBED);
            let has = asm.contains("str xzr, [sp]");
            assert_eq!(has, probed, "{triple}: {n} stacked arguments:\n{asm}");
        }
    }
    // Darwin stacks variadic arguments, and rounds `sp` down for an
    // over-aligned one by an amount only the run knows: the most it can
    // take is probed first, and `sp` put back.
    let args: Vec<String> = (0..140).map(|i| format!("{i}L")).collect();
    let src = format!(
        "typedef long al __attribute__((aligned(32)));\n\
         void vf(int, ...);\n\
         void g(al x) {{ vf(1, x, {}); }}\n",
        args.join(", ")
    );
    let asm = asm_for_with("stack_clash_va", AARCH64_DARWIN, &src, PROBED);
    let body = body_of(&asm, "g");
    let probe = body.find("str xzr, [sp]").expect("probed");
    let realign = body.find("add x16, sp, #0").expect("realigned");
    assert!(probe < realign, "probed before the rounding:\n{body}");
    let asm = asm_for_with("stack_clash_args", X86_64_LINUX, &stacked_call(600), PROBED);
    assert_body_contains(&asm, "caller", "orq $0, (%rsp)", "a 4800-byte area");
    let asm = asm_for_with("stack_clash_args", X86_64_LINUX, &stacked_call(100), PROBED);
    assert_body_lacks(&asm, "caller", "orq", "an 800-byte area");
}

/// A 4 KiB guard on aarch64 makes the threshold 3 KiB and the step 4 KiB.
#[test]
fn codegen_stack_clash_aarch64_small_guard() {
    let mut flags = PROBED.to_vec();
    flags.push("--param=stack-clash-protection-guard-size=12");
    let asm = a64(AARCH64_LINUX, &flags);
    assert_body_lacks(&asm, "small", "xzr", "still under the guard");
    // Two whole intervals, and a residual (3824 bytes) past the 3 KiB.
    assert_eq!(count_in_body(&asm, "f12k", "str xzr, [sp, #1024]"), 3);
    assert_body_contains(&asm, "f12k", "sub sp, sp, #4096", "4 KiB steps");
}
