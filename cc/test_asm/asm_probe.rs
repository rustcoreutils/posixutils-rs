//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The in-process twin of `tests/codegen/asm_probe.rs`: the same helpers, by
// the same names and signatures, compiling through `test_compile` instead of
// spawning `c17 -S`, so an assembly-inspecting case moves here unchanged.
//
// Everything takes an explicit target triple, so an x86_64 host asserts
// aarch64 codegen and vice versa.
//

use crate::test_compile::{self, compile};

pub const X86_64_LINUX: &str = "x86_64-unknown-linux-gnu";
pub const AARCH64_LINUX: &str = "aarch64-unknown-linux-gnu";
pub const AARCH64_DARWIN: &str = "aarch64-apple-darwin";

/// Emit assembly for `src` at `triple` and return it.
///
/// Compiles at `-O` because several of the defects these tests cover exist
/// only once the optimizer runs.
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
    let c = compile(name, src, &flags);
    match c.asm {
        Some(asm) => asm,
        None => panic!("c17 --target {triple} failed for {name}:\n{}", c.stderr),
    }
}

/// The host's assembly for `src` with `extra` options; `-O0` unless they
/// name a level, as `c17 -S` would. A `--target TRIPLE` pair is accepted as
/// the integration helper spells it.
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

/// The host's assembly for `src` at `-O0`, for tests that need to see the
/// directives rather than the program's answer.
#[track_caller]
pub fn host_asm(prefix: &str, src: &str) -> String {
    asm_for_at(prefix, src, &[])
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
#[track_caller]
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
#[track_caller]
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

/// The section directive in force where `name:` is defined. Only the ELF
/// section tests use it, and they run on an x86-64 Linux host.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
pub fn section_of<'a>(asm: &'a str, name: &str) -> Option<&'a str> {
    let label = format!("{name}:");
    let mut current = None;
    for line in asm.lines() {
        let t = line.trim();
        if t == ".bss" || t == ".data" || t == ".text" || t.starts_with(".section ") {
            current = Some(t);
        } else if t == label {
            return current;
        }
    }
    None
}

/// `name` as the assembler spells it on this host. Only for a test that
/// compiles for the *host*.
pub fn asm_symbol(name: &str) -> String {
    if cfg!(target_os = "macos") {
        format!("_{name}")
    } else {
        name.to_string()
    }
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

/// The symbol prefix `asm` uses, read off a symbol it is known to define.
///
/// Mach-O spells every C identifier with a leading underscore. Reading the
/// prefix off the output is right both for a test that names a `--target`
/// and for one compiled for the host.
pub fn asm_prefix(asm: &str, defined: &str) -> &'static str {
    let mangled = format!("_{defined}:");
    if asm.lines().any(|l| l.trim_start() == mangled) {
        "_"
    } else {
        ""
    }
}

/// Whether `asm` calls a function whose name ends in one of `names`.
pub fn calls_any(asm: &str, names: &[&str]) -> bool {
    asm.lines().any(|l| {
        let mut w = l.split_whitespace();
        matches!(w.next(), Some("call" | "bl" | "jmp" | "b"))
            && w.next().is_some_and(|t| {
                let t = t.trim_end_matches("@PLT");
                names.iter().any(|n| t.ends_with(n))
            })
    })
}

/// Whether the assembly mentions the function `name` at all -- a call, a
/// tail jump or an address taken.
pub fn mentions(asm: &str, name: &str) -> bool {
    asm.lines()
        .filter(|l| !l.trim_start().starts_with(".file"))
        .any(|l| {
            l.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
                .any(|w| w == name || w.strip_prefix('_') == Some(name))
        })
}
