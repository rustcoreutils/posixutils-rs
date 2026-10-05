//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Helpers for tests that must inspect generated assembly.
//
// Some properties are invisible to a program's exit status. Atomicity is the
// clearest case: `_Atomic int x; x = 100;` reads back 100 whether or not the
// store was atomic, so a behavioral test passes against plain `movl`. The only
// way to pin it is to look at the instructions.
//
// Everything here takes an explicit `--target`, so an x86_64 host asserts
// aarch64 codegen and vice versa. That matters because half the atomic and
// floating-point defects this suite guards are architecture-specific and CI
// does not run every architecture.
//

use crate::common::run_c17;

pub const X86_64_LINUX: &str = "x86_64-unknown-linux-gnu";
pub const AARCH64_LINUX: &str = "aarch64-unknown-linux-gnu";
pub const AARCH64_DARWIN: &str = "aarch64-apple-darwin";

/// Emit assembly for `src` at `triple` and return it.
///
/// Compiles at `-O` because several of the defects these tests cover exist
/// only once the optimizer runs.
pub fn asm_for(name: &str, triple: &str, src: &str) -> String {
    asm_for_with(name, triple, src, &["-O"])
}

/// `asm_for` with explicit extra options (e.g. a different `-O` level).
pub fn asm_for_with(name: &str, triple: &str, src: &str, extra: &[&str]) -> String {
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("c17_cross_{}_", name))
        .tempdir()
        .expect("failed to create work dir");
    let c = dir.path().join("t.c");
    std::fs::write(&c, src).expect("failed to write source");
    let s = dir.path().join("t.s");

    let mut args: Vec<String> = vec!["--target".into(), triple.into(), "-S".into()];
    args.extend(extra.iter().map(|s| s.to_string()));
    args.push(c.to_string_lossy().into_owned());
    args.push("-o".into());
    args.push(s.to_string_lossy().into_owned());

    let arg_refs: Vec<&str> = args.iter().map(String::as_str).collect();
    let r = run_c17(&arg_refs);
    assert!(
        r.success,
        "c17 --target {} failed for {}:\n{}{}",
        triple, name, r.stdout, r.stderr
    );
    std::fs::read_to_string(&s).expect("no assembly produced")
}

/// The body of function `name`, from its label to `.cfi_endproc`.
///
/// Accepts the Mach-O underscore-prefixed spelling so the same assertion works
/// against a Darwin triple.
pub fn body_of<'a>(asm: &'a str, name: &str) -> &'a str {
    let label = format!("\n{}:\n", name);
    let underscored = format!("\n_{}:\n", name);
    let start = asm
        .find(&label)
        .or_else(|| asm.find(&underscored))
        .unwrap_or_else(|| panic!("no function {} in:\n{}", name, asm));
    let rest = &asm[start..];
    let end = rest.find(".cfi_endproc").unwrap_or(rest.len());
    &rest[..end]
}

/// Assert that function `func` does *not* contain `needle`.
///
/// Negative assertions keep the positive ones honest: a test that only checks
/// for `lock` cannot tell whether the compiler emits it everywhere.
pub fn assert_body_lacks(asm: &str, func: &str, needle: &str, why: &str) {
    let body = body_of(asm, func);
    assert!(
        !body.contains(needle),
        "{why}\nunexpected `{needle}` in {func}:\n{body}"
    );
}

/// Bytes of stack frame function `func` reserves in its prologue.
///
/// `None` means the prologue reserves nothing -- either no allocation at all,
/// or an allocation this parser does not recognize. Callers should assert on
/// the number rather than on its absence, so an unrecognized spelling reads as
/// "no claim" instead of as a passing test.
///
/// Frame size is the property this file otherwise cannot see: whether a local
/// was promoted out of memory is invisible to a program's exit status, because
/// the answer comes out the same either way. Only the prologue tells you.
///
/// Recognizes the x86-64 `subq $N, %rsp` and the aarch64 `sub sp, sp, #N`.
pub fn frame_size(asm: &str, func: &str) -> Option<i64> {
    let body = body_of(asm, func);
    for line in body.lines() {
        let line = line.trim();
        // x86-64: subq $112, %rsp
        if let Some(rest) = line.strip_prefix("subq $") {
            if let Some((imm, dst)) = rest.split_once(',') {
                if dst.trim() == "%rsp" {
                    return imm.trim().parse().ok();
                }
            }
        }
        // aarch64: sub sp, sp, #112
        if let Some(rest) = line.strip_prefix("sub sp, sp, #") {
            return rest.trim().parse().ok();
        }
    }
    None
}

/// The host's assembly for `src` at `-O0`, for tests that need to see the
/// directives rather than the program's answer.
pub fn host_asm(prefix: &str, src: &str) -> String {
    crate::common::asm_for_at(prefix, src, &[])
}
