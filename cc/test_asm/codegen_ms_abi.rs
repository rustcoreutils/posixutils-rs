//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((ms_abi))`: the diagnostic half, compiled in process. The
// interop half, against gcc in every pairing, is `tests/codegen/ms_abi.rs`.
//

use crate::test_compile::compile_expect_warning_with;

/// gcc for aarch64 has no `ms_abi` or `sysv_abi`: it warns that each is
/// ignored, and the function keeps the native convention. So does c17 --
/// the warning is in the attributes group. The run against gcc is
/// `codegen_ms_abi_is_ignored_on_aarch64` in `tests/codegen/ms_abi.rs`.
#[test]
fn codegen_ms_abi_is_ignored_on_aarch64_with_a_warning() {
    let src = "__attribute__((ms_abi)) long f(long a);\n\
               __attribute__((sysv_abi)) long g(long a);\n";
    let mut args: Vec<String> = vec!["--target=aarch64-unknown-linux-gnu".to_string()];
    let stderr = compile_expect_warning_with("ms_abi_a64", src, &args);
    assert!(
        stderr.contains("'ms_abi' attribute directive ignored")
            && stderr.contains("'sysv_abi' attribute directive ignored"),
        "{stderr}"
    );
    args.push("-Wno-attributes".to_string());
    let quiet = compile_expect_warning_with("ms_abi_a64_quiet", src, &args);
    assert!(!quiet.contains("ms_abi"), "{quiet}");
}

/// An `ms_abi` function saves `%rsi` and `%rdi`, callee-saved under Win64 --
/// but not one that is a global register variable, which is the variable's:
/// restoring it on the way out undid every store the function made to it.
#[test]
fn codegen_ms_abi_does_not_save_a_global_register() {
    use super::asm_probe::{asm_for_with, body_of, X86_64_LINUX};
    let src = "register long g asm(\"rdi\");\n\
               __attribute__((ms_abi)) long w(long a) { g = a; return a + 1; }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with("ms_abi_global_reg", X86_64_LINUX, src, &[opt]);
        let body = body_of(&asm, "w");
        assert!(body.contains("pushq %rsi"), "{opt}:\n{body}");
        assert!(!body.contains("pushq %rdi"), "{opt}:\n{body}");
        assert!(!body.contains("popq %rdi"), "{opt}:\n{body}");
    }
}
