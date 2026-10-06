//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-E` output that has to read back as the same tokens.
//
// Preprocessed text is compiled again (`-save-temps`, ccache, distcc) and
// read by tools, so the spacing between tokens is part of what it means: two
// tokens written side by side that lex as one are a different program.
//

use crate::common::{preprocess_text, run_c17};

/// The tokens on the line of `-E -P` output that holds `marker`.
fn line_with(out: &str, marker: &str) -> String {
    out.lines()
        .find(|l| l.contains(marker))
        .unwrap_or_else(|| panic!("no line with {marker:?} in:\n{out}"))
        .trim()
        .to_string()
}

/// A substituted argument is spaced as its parameter was in the body.
/// libffi's `unix64.S` writes `.org BASE + X * 8` with `BASE` an `.L` label;
/// `-E` printed `.org.Lstore_table` and the assembler rejected the pseudo-op.
#[test]
fn preprocessor_argument_keeps_its_parameters_space() {
    let src = "#define L(X) .L ## X\n\
               #define E(BASE, X) .balign 8; .org BASE + X * 8\n\
               E(L(tab), 3)\n\
               #define F(a) x - a\n\
               F(-1) F1\n";
    let r = preprocess_text("pp_arg_space", src, &["-P"]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_eq!(
        line_with(&r.stdout, ".org"),
        ".balign 8; .org .Ltab + 3 * 8"
    );
    assert_eq!(line_with(&r.stdout, "F1"), "x - -1 F1");

    // The same text through the assembler's preprocessor, which is how
    // libffi reached it.
    let dir = plib::tmp::Builder::new()
        .prefix("c17_pp_arg_space_")
        .tempdir()
        .unwrap();
    let s = dir.path().join("t.S");
    std::fs::write(&s, src).unwrap();
    let r = run_c17(&["-E", "-P", &s.to_string_lossy()]);
    assert!(r.success, "-E of a .S failed: {}", r.stderr);
    assert_eq!(
        line_with(&r.stdout, ".org"),
        ".balign 8; .org .Ltab + 3 * 8"
    );
}
