//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Identifiers containing characters outside the basic source character set:
// the assembly-text half of the block-label case, whose run-time half is in
// `tests/c99/identifiers.rs`.
//

use crate::test_compile::asm_for;

/// A block label carries its function's name, so an extended character in the
/// identifier reaches the assembler inside a `.L` label too.
///
/// Only the *definition* went out raw. `&&label` builds the same spelling as a
/// local symbol, which is quoted downstream, so one label was spelled two ways
/// in one file: `.Lmüller_1:` where it was defined, `".Lmüller_1"(%rip)` where
/// it was referenced. GNU as resolves both to the same symbol, which is why
/// this ran; Mach-O's assembler rejects the raw bytes.
///
/// That it assembles, links and runs is checked by
/// `c99_extended_identifiers_mega` in the integration suite; this checks that
/// every mention of the label is spelled the same way, on the text, because
/// that is where the two spellings diverged.
#[test]
fn c99_extended_identifier_in_block_labels_is_quoted() {
    let code = r#"
int müller(int x) {
    void *targets[] = { &&lab0, &&lab1 };
    goto *targets[x & 1];
lab0: return 1;
lab1: return 2;
}
int main(void) { return (müller(0) == 1 && müller(1) == 2) ? 0 : 1; }
"#;
    let text = asm_for("lbl", code, &[]);

    let mentions: Vec<&str> = text
        .lines()
        .filter(|l| l.contains("müller_"))
        .map(|l| l.trim())
        .collect();
    assert!(
        !mentions.is_empty(),
        "expected the function's labels in:\n{}",
        text
    );
    for line in &mentions {
        assert!(
            !line.contains("Lmüller_")
                || line.contains("\".Lmüller_")
                || line.contains("\"Lmüller_"),
            "an unquoted label reached the assembler: {:?}\nin:\n{}",
            line,
            text
        );
    }
}
