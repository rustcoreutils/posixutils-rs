//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The integration suite's `tests/codegen/asm_probe.rs` helpers, compiling in
// process through `test_compile` instead of spawning `c17 -S`. The names and
// signatures are the same, so a case moves between the two unchanged.
//

use crate::test_compile;

pub const X86_64_LINUX: &str = "x86_64-unknown-linux-gnu";
pub const AARCH64_LINUX: &str = "aarch64-unknown-linux-gnu";
pub const AARCH64_DARWIN: &str = "aarch64-apple-darwin";

/// Emit assembly for `src` at `triple` and return it, at `-O` as the
/// integration helper does.
#[track_caller]
pub fn asm_for(name: &str, triple: &str, src: &str) -> String {
    asm_for_with(name, triple, src, &["-O"])
}

/// `asm_for` with explicit extra options (e.g. a different `-O` level).
#[track_caller]
pub fn asm_for_with(name: &str, triple: &str, src: &str, extra: &[&str]) -> String {
    let target = format!("--target={triple}");
    let mut flags = vec![target.as_str()];
    flags.extend_from_slice(extra);
    test_compile::asm_for(name, src, &flags)
}

/// The integration suite's `common::asm_for_at`: the host's assembly at
/// `-O0` unless `extra` says otherwise. A `--target TRIPLE` pair is accepted
/// in the driver's spelling.
#[track_caller]
pub fn asm_for_at(prefix: &str, src: &str, extra: &[&str]) -> String {
    let mut flags: Vec<String> = Vec::new();
    let mut it = extra.iter();
    while let Some(&flag) = it.next() {
        if flag == "--target" {
            let triple = it.next().expect("--target needs a triple");
            flags.push(format!("--target={triple}"));
        } else {
            flags.push(flag.to_string());
        }
    }
    let flags: Vec<&str> = flags.iter().map(String::as_str).collect();
    test_compile::asm_for(prefix, src, &flags)
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

/// Assert that function `func` contains `needle`.
pub fn assert_body_contains(asm: &str, func: &str, needle: &str, why: &str) {
    let body = body_of(asm, func);
    assert!(
        body.contains(needle),
        "{why}\nexpected `{needle}` in {func}:\n{body}"
    );
}

/// Assert that function `func` does *not* contain `needle`.
pub fn assert_body_lacks(asm: &str, func: &str, needle: &str, why: &str) {
    let body = body_of(asm, func);
    assert!(
        !body.contains(needle),
        "{why}\nunexpected `{needle}` in {func}:\n{body}"
    );
}

/// How many times `needle` appears in function `func`.
pub fn count_in_body(asm: &str, func: &str, needle: &str) -> usize {
    body_of(asm, func).matches(needle).count()
}

/// Bytes of stack frame function `func` reserves in its prologue: the x86-64
/// `subq $N, %rsp` or the aarch64 `sub sp, sp, #N`. `None` means the prologue
/// reserves nothing this parser recognizes.
pub fn frame_size(asm: &str, func: &str) -> Option<i64> {
    let body = body_of(asm, func);
    for line in body.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("subq $") {
            if let Some((imm, dst)) = rest.split_once(',') {
                if dst.trim() == "%rsp" {
                    return imm.trim().parse().ok();
                }
            }
        }
        if let Some(rest) = line.strip_prefix("sub sp, sp, #") {
            return rest.trim().parse().ok();
        }
    }
    None
}
