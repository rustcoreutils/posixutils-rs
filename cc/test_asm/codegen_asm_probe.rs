//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The tests that lived in `tests/codegen/asm_probe.rs` beside its helpers.
//

mod verbose_asm {
    use crate::test_asm::asm_probe::asm_for_with;

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

    /// Plain `-S` is untouched, and compiling the same source twice with the
    /// flag gives the same assembly both times.
    #[test]
    fn verbose_asm_is_deterministic_and_leaves_plain_output_alone() {
        let src = "int f(int x) { int y = x * 3; return y - 1; }\n";
        let triple = "x86_64-unknown-linux-gnu";

        // Each call compiles under its own name, so the `.file` directive
        // names a different source; that is the harness, not the compiler.
        // Everything else must match exactly.
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
