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

#[cfg(test)]
mod verbose_asm {
    use super::asm_for_with;

    /// `-fverbose-asm` was accepted and did nothing: the `-f*` catch-all in
    /// `preprocess_args` dropped it before clap saw it, so the output was
    /// byte-identical to plain `-S`. Accept-and-discard, the same shape as the
    /// `-E -o` bug.
    #[test]
    fn verbose_asm_annotates_what_plain_output_does_not() {
        let src = "int add(int a, int b) { int c = a + b; return c * 2; }\n";

        for triple in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
            let plain = asm_for_with("verbose_off", triple, src, &[]);
            let verbose = asm_for_with("verbose_on", triple, src, &["-fverbose-asm"]);

            assert_ne!(
                plain, verbose,
                "{triple}: -fverbose-asm produced identical output to plain -S"
            );

            // The operands' source-level names are the point.
            for name in ["a", "b", "c"] {
                assert!(
                    verbose.contains(name),
                    "{triple}: no annotation naming `{name}`:\n{verbose}"
                );
            }
        }
    }

    /// The comment introducer is the assembler's, not one spelling for both.
    /// aarch64 gas reads a *trailing* `#` as the start of an immediate, so
    /// getting this wrong does not produce an ugly comment -- it produces
    /// assembly that will not assemble.
    #[test]
    fn verbose_asm_uses_each_assemblers_comment_syntax() {
        let src = "int add(int a, int b) { return a + b; }\n";

        let x86 = asm_for_with(
            "verbose_x86",
            "x86_64-unknown-linux-gnu",
            src,
            &["-fverbose-asm"],
        );
        let arm = asm_for_with(
            "verbose_arm",
            "aarch64-unknown-linux-gnu",
            src,
            &["-fverbose-asm"],
        );

        // A trailing annotation on an instruction line, in the right dialect.
        assert!(
            x86.lines()
                .any(|l| l.trim_start().starts_with("mov") && l.contains(" # ")),
            "x86-64 annotations must use `#`:\n{x86}"
        );
        assert!(
            arm.lines()
                .any(|l| l.trim_start().starts_with("mov") && l.contains(" // ")),
            "aarch64 annotations must use `//`:\n{arm}"
        );
        assert!(
            !arm.lines()
                .any(|l| l.trim_start().starts_with("mov") && l.contains(" # ")),
            "a trailing `#` on aarch64 parses as an immediate:\n{arm}"
        );
    }

    /// Plain `-S` is untouched, and the flag does not disturb the
    /// bit-identical-output invariant `determinism.rs` exists to protect.
    #[test]
    fn verbose_asm_is_deterministic_and_leaves_plain_output_alone() {
        let src = "int f(int x) { int y = x * 3; return y - 1; }\n";
        let triple = "x86_64-unknown-linux-gnu";

        // Each call compiles in its own scratch directory, so the `.file`
        // directive names a different path; that is the harness, not the
        // compiler. Everything else must match exactly.
        let without_file = |asm: &str| -> String {
            asm.lines()
                .filter(|l| !l.trim_start().starts_with(".file"))
                .collect::<Vec<_>>()
                .join("\n")
        };

        let plain_a = without_file(&asm_for_with("det_plain_a", triple, src, &[]));
        let plain_b = without_file(&asm_for_with("det_plain_b", triple, src, &[]));
        assert_eq!(plain_a, plain_b, "plain -S must be reproducible");

        let verbose_a = without_file(&asm_for_with(
            "det_verbose_a",
            triple,
            src,
            &["-fverbose-asm"],
        ));
        let verbose_b = without_file(&asm_for_with(
            "det_verbose_b",
            triple,
            src,
            &["-fverbose-asm"],
        ));
        assert_eq!(
            verbose_a, verbose_b,
            "-fverbose-asm must be reproducible too"
        );

        // Stripping the annotations must give back exactly the plain output:
        // the flag may add comments and must change nothing else.
        let stripped: String = verbose_a
            .lines()
            .map(|l| match l.find(" # ") {
                // Leave the header comment, which plain output also carries.
                Some(i) if !l.trim_start().starts_with('#') => &l[..i],
                _ => l,
            })
            .collect::<Vec<_>>()
            .join("\n");
        assert_eq!(
            stripped.trim_end(),
            plain_a.trim_end(),
            "-fverbose-asm changed more than the comments"
        );
    }
}
