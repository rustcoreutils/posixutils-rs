//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The options a Linux kernel build passes c17, as the driver takes and
// refuses them: the x86-64 code model, stack and mitigation options, the
// stack guard's placement, and the `-f` options of `KBUILD_CFLAGS`. Every
// refusal is worded as gcc 13 words it. What the options do to the code is
// `test_asm/codegen_kernel.rs` and `codegen/kernel.rs`.
//

use super::{s, WorkDir};
use crate::common::run_c17;

const X86: &str = "--target=x86_64-unknown-linux-gnu";
const AARCH64: &str = "--target=aarch64-unknown-linux-gnu";

/// `c17 <flags> -S` of a small unit, for its status and diagnostics.
fn compile(flags: &[&str]) -> crate::common::C17Run {
    let dir = WorkDir::new("kernel_flags");
    let c = dir.write("k.c", "int f(int x) { return x + 1; }\n");
    let mut args = flags.to_vec();
    let c = s(&c);
    args.extend(["-S", "-o", "/dev/null", &c]);
    run_c17(&args)
}

/// The refusal of `flags`: the run must fail and say exactly `want`.
fn refused(flags: &[&str], want: &str) {
    let r = compile(flags);
    assert!(!r.success, "{flags:?} accepted");
    assert_eq!(r.stderr, want, "{flags:?}");
}

/// `-mcmodel=`: the small model and the kernel's are taken; medium and
/// large are not models c17 generates; `32` is x32's, which gcc refuses in
/// 64-bit mode; anything else is not a model at all. The kernel model
/// cannot be position-independent, and c17's default, like gcc's on a PIE
/// distribution, is.
#[test]
fn kernel_flags_code_model() {
    for flags in [
        &[X86, "-mcmodel=small"][..],
        &[X86, "-mcmodel=kernel", "-fno-pie"],
        &[X86, "-mcmodel=kernel", "-fno-pic"],
        &[AARCH64, "-mcmodel=small"],
    ] {
        let r = compile(flags);
        assert!(r.success, "{flags:?}: {}", r.stderr);
    }
    for model in ["medium", "large"] {
        let flag = format!("-mcmodel={model}");
        refused(
            &[X86, &flag],
            &format!("c17: unsupported machine flag: {flag}\n"),
        );
    }
    refused(
        &[X86, "-mcmodel=32"],
        "c17: error: code model '32' not supported in the 64 bit mode\n",
    );
    refused(
        &[X86, "-mcmodel=foo"],
        "c17: error: unrecognized argument in option '-mcmodel=foo'\n\
         c17: note: valid arguments to '-mcmodel=' are: 32 kernel large medium small\n",
    );
    for pic in ["-fpie", "-fPIC", "-fpic"] {
        refused(
            &[X86, "-fno-pie", "-mcmodel=kernel", pic],
            "c17: error: code model kernel does not support PIC mode\n",
        );
    }
    refused(
        &[X86, "-mcmodel=kernel", "-fPIE"],
        "c17: error: code model kernel does not support PIC mode\n",
    );
    refused(
        &["--target=x86_64-apple-darwin", "-mcmodel=kernel"],
        "c17: unsupported machine flag: -mcmodel=kernel\n",
    );
}

/// `-mno-red-zone` asks for what c17 does, and `-mred-zone` permits what it
/// never does; both are x86-64's alone, and aarch64 gcc does not know them.
#[test]
fn kernel_flags_red_zone() {
    for flag in ["-mno-red-zone", "-mred-zone"] {
        let r = compile(&[X86, flag]);
        assert!(r.success, "{flag}: {}", r.stderr);
        refused(
            &[AARCH64, flag],
            &format!("c17: error: unrecognized command-line option '{flag}'\n"),
        );
    }
}

/// The `-m` options of a kernel's `KBUILD_CFLAGS` that need no code of
/// their own here, and `-mskip-rax-setup`, `-mpreferred-stack-boundary=3`
/// and `-mno-fp-ret-in-387`, which do: all taken on x86-64.
/// `-mgeneral-regs-only` is aarch64's too, where c17 has no code without
/// the FP registers. gcc's bounds on the stack boundary are 3 and 12; c17
/// keeps the stack at 8 or 16 bytes, and refuses the wider ones.
#[test]
fn kernel_flags_machine_options() {
    let kbuild = [
        X86,
        "-mno-sse",
        "-mno-mmx",
        "-mno-sse2",
        "-mno-3dnow",
        "-mno-avx",
        "-mno-80387",
        "-mno-fp-ret-in-387",
        "-mgeneral-regs-only",
        "-msoft-float",
        "-mhard-float",
        "-mskip-rax-setup",
        "-mno-skip-rax-setup",
        "-mpreferred-stack-boundary=3",
        "-mpreferred-stack-boundary=4",
        "-mno-red-zone",
        "-mcmodel=kernel",
        "-fno-pie",
        "-mindirect-branch-register",
        "-mindirect-branch-cs-prefix",
    ];
    let r = compile(&kbuild);
    assert!(r.success, "{}", r.stderr);
    refused(
        &[AARCH64, "-mgeneral-regs-only"],
        "c17: unsupported machine flag: -mgeneral-regs-only\n",
    );
    refused(
        &[X86, "-mpreferred-stack-boundary=2"],
        "c17: error: '-mpreferred-stack-boundary=2' is not between 3 and 12\n",
    );
    refused(
        &[X86, "-mpreferred-stack-boundary=5"],
        "c17: unsupported machine flag: -mpreferred-stack-boundary=5\n",
    );
    refused(
        &[X86, "-mpreferred-stack-boundary=x"],
        "c17: error: argument to '-mpreferred-stack-boundary=' should be a non-negative integer\n",
    );
}

/// The `-f` options of a kernel's `KBUILD_CFLAGS`: taken, each meaning
/// what gcc 13 means by it, and refused where gcc refuses the value.
#[test]
fn kernel_flags_f_options() {
    for flag in [
        "-fno-delete-null-pointer-checks",
        "-fdelete-null-pointer-checks",
        "-fno-stack-check",
        "-fstack-check=no",
        "-fshort-wchar",
        "-fno-short-wchar",
        "-ffreestanding",
        "-fhosted",
        "-foptimize-sibling-calls",
        "-fno-optimize-sibling-calls",
        "-falign-functions",
        "-falign-functions=16",
        "-falign-functions=32:8:16:4",
        "-fno-align-functions",
        "-fmin-function-alignment=16",
        "-falign-jumps=1",
        "-falign-loops=1",
        "-falign-labels=1",
        "-falign-jumps",
        "-fno-align-loops",
        "-fno-allow-store-data-races",
        "-fallow-store-data-races",
        "-fstrict-flex-arrays",
        "-fstrict-flex-arrays=3",
        "-fno-strict-flex-arrays",
    ] {
        let r = compile(&[X86, flag]);
        assert!(r.success && r.stderr.is_empty(), "{flag}: {}", r.stderr);
    }
    for (flag, want) in [
        (
            "-falign-functions=x",
            "c17: error: invalid arguments for '-falign-functions' option: 'x'\n",
        ),
        (
            "-falign-loops=70000",
            "c17: error: '-falign-loops' is not between 0 and 65536\n",
        ),
        (
            "-fstrict-flex-arrays=4",
            "c17: error: argument to '-fstrict-flex-arrays=' is not between 0 and 3\n",
        ),
        (
            "-fstrict-flex-arrays=x",
            "c17: error: argument to '-fstrict-flex-arrays=' should be a non-negative integer\n",
        ),
    ] {
        refused(&[X86, flag], want);
    }
    // Stack probes: c17's `-fstack-clash-protection`, taken in silence.
    let r = compile(&[X86, "-fstack-check=specific"]);
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);
}

/// The mitigation and stack-guard options: taken with each of gcc's values,
/// refused in gcc's words with any other, and a retpoline the unit writes
/// itself refused beside a shadow stack (`-fcf-protection=return`), as gcc
/// refuses it. The thunks are ELF comdat sections Darwin has no form for.
#[test]
fn kernel_flags_mitigations_and_guard() {
    for flags in [
        &["-mindirect-branch=keep", "-mindirect-branch=thunk"][..],
        &["-mindirect-branch=thunk-inline"],
        &["-mindirect-branch=thunk-extern", "-fcf-protection"],
        &["-mfunction-return=thunk-extern", "-fcf-protection=full"],
        &["-mfunction-return=thunk", "-fcf-protection=branch"],
        &[
            "-mharden-sls=none",
            "-mharden-sls=all",
            "-mharden-sls=return",
            "-mharden-sls=indirect-jmp",
        ],
        &[
            "-mstack-protector-guard=tls",
            "-mstack-protector-guard=global",
        ],
        &[
            "-mstack-protector-guard-reg=fs",
            "-mstack-protector-guard-reg=gs",
        ],
        &[
            "-mstack-protector-guard-offset=0x28",
            "-mstack-protector-guard-offset=-8",
        ],
        &["-mstack-protector-guard-symbol=__stack_chk_guard"],
        &[
            "-fpatchable-function-entry=16,16",
            "-fpatchable-function-entry=3",
        ],
        &[
            "-fzero-call-used-regs=used-gpr",
            "-fzero-call-used-regs=skip",
        ],
        &[
            "-ftrivial-auto-var-init=zero",
            "-ftrivial-auto-var-init=pattern",
        ],
        &["-ftrivial-auto-var-init=uninitialized"],
    ] {
        let mut args = vec![X86];
        args.extend_from_slice(flags);
        let r = compile(&args);
        assert!(r.success && r.stderr.is_empty(), "{flags:?}: {}", r.stderr);
    }
    for (flags, want) in [
        (
            &["-mindirect-branch=foo"][..],
            "c17: error: unrecognized argument in option '-mindirect-branch=foo'\n\
             c17: note: valid arguments to '-mindirect-branch=' are: keep thunk thunk-extern thunk-inline\n",
        ),
        (
            &["-mfunction-return=bar"],
            "c17: error: unrecognized argument in option '-mfunction-return=bar'\n\
             c17: note: valid arguments to '-mfunction-return=' are: keep thunk thunk-extern thunk-inline\n",
        ),
        (
            &["-mstack-protector-guard=x"],
            "c17: error: unrecognized argument in option '-mstack-protector-guard=x'\n\
             c17: note: valid arguments to '-mstack-protector-guard=' are: global tls\n",
        ),
        (
            &["-mstack-protector-guard-reg=es"],
            "c17: error: 'es' is not a valid base register in '-mstack-protector-guard-reg='\n",
        ),
        (
            &["-mstack-protector-guard-symbol="],
            "c17: error: missing argument to '-mstack-protector-guard-symbol='\n",
        ),
        (
            &["-mindirect-branch=thunk", "-fcf-protection"],
            "c17: error: '-mindirect-branch' and '-fcf-protection' are not compatible\n",
        ),
        (
            &["-mfunction-return=thunk-inline", "-fcf-protection=return"],
            "c17: error: '-mfunction-return' and '-fcf-protection' are not compatible\n",
        ),
        (
            &["-fpatchable-function-entry=2,3"],
            "c17: error: invalid arguments for '-fpatchable-function-entry'\n",
        ),
        (
            &["-fzero-call-used-regs=x"],
            "c17: error: unrecognized argument to '-fzero-call-used-regs=': 'x'\n",
        ),
        (
            &["-ftrivial-auto-var-init=x"],
            "c17: error: unrecognized automatic variable initialization type 'x'\n\
             c17: note: valid arguments to '-ftrivial-auto-var-init=' are: pattern uninitialized zero\n",
        ),
    ] {
        let mut args = vec![X86];
        args.extend_from_slice(flags);
        refused(&args, want);
    }
    refused(
        &[AARCH64, "-fzero-call-used-regs=used-gpr"],
        "c17: error: '-fzero-call-used-regs' is not supported for this target\n",
    );
    refused(
        &["--target=x86_64-apple-darwin", "-mindirect-branch=thunk"],
        "c17: unsupported machine flag: -mindirect-branch=thunk\n",
    );
    refused(
        &[
            "--target=x86_64-apple-darwin",
            "-fpatchable-function-entry=2",
        ],
        "c17: error: '-fpatchable-function-entry' is not supported for this target\n",
    );
}
