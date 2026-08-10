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

/// Emit assembly for `src` at `triple` and return it.
///
/// Compiles at `-O` because several of the defects these tests cover exist
/// only once the optimizer runs.
pub fn asm_for(name: &str, triple: &str, src: &str) -> String {
    asm_for_with(name, triple, src, &["-O"])
}

/// `asm_for` with explicit extra options (e.g. a different `-O` level).
pub fn asm_for_with(name: &str, triple: &str, src: &str, extra: &[&str]) -> String {
    let dir = tempfile::Builder::new()
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

/// Assert that function `func` contains `needle`.
pub fn assert_body_contains(asm: &str, func: &str, needle: &str, why: &str) {
    let body = body_of(asm, func);
    assert!(
        body.contains(needle),
        "{why}\nexpected `{needle}` in {func}:\n{body}"
    );
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

/// How many times `needle` appears in function `func`.
pub fn count_in_body(asm: &str, func: &str, needle: &str) -> usize {
    body_of(asm, func).matches(needle).count()
}
