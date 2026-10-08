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

/// Every file a line marker names is a file, or one of the pseudo-names gcc
/// writes for text that has none. Tools read the markers as a dependency
/// list: perl's `makedepend` turns each into a make prerequisite, drops
/// `<built-in>` and `<command-line>`, and choked on `<builtin:stdarg.h>`
/// ("target pattern contains no '%'") -- c17's bundled headers are not on
/// disk.
#[test]
fn preprocessor_markers_name_files_or_gcc_pseudo_files() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_pp_marker_names_")
        .tempdir()
        .unwrap();
    let src = dir.path().join("m.c");
    std::fs::write(
        &src,
        "#include <stdarg.h>\n#include <stddef.h>\n#include <stdio.h>\nint x;\n",
    )
    .unwrap();
    let r = run_c17(&["-E", &src.to_string_lossy()]);
    assert!(r.success, "-E failed: {}", r.stderr);
    let mut pseudo = 0;
    for line in r.stdout.lines().filter(|l| l.starts_with("# ")) {
        let name = line.split('"').nth(1).expect("marker without a name");
        if name.starts_with('<') {
            assert_eq!(name, "<built-in>", "marker {line:?}");
            pseudo += 1;
        } else {
            assert!(
                std::path::Path::new(name).exists(),
                "marker {line:?} names no file"
            );
        }
    }
    assert!(pseudo > 0, "no bundled header was marked:\n{}", r.stdout);
}

/// Tokens from different places can meet with nothing between them: a macro
/// boundary, an argument, an empty expansion. Written side by side they must
/// still read back as the tokens they are, or `-M` (with `M` defined as `-`)
/// becomes `--`. Tokens that came from the source together keep their
/// spacing. gcc agrees on every pair here but `N+1`, which it writes `1 +1`
/// for fear of an exponent; `1+` is no pp-number, so nothing is needed.
#[test]
fn preprocessor_output_never_pastes_tokens_together() {
    let src = "#define M -\n\
               #define E\n\
               #define I(a) a\n\
               #define P L\n\
               #define D .\n\
               #define N 1\n\
               #define S /\n\
               -M; -E-; +I(+)+; x-I(-1); P\"x\"; D.D; N.; I(1)e+1; S/ S*x*/; \
               <I(:); I(%):; I(<)<=; a+++b; 1+2; a.b; p->q; f(x); x+=1; 1e+5; \
               I(x)I(y); I(1)N; D N; N+1; P'c'; I(%:)%:\n";
    let r = preprocess_text("pp_avoid_paste", src, &["-P"]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_eq!(
        line_with(&r.stdout, "a+++b"),
        "- -; - -; + + +; x- -1; L \"x\"; . . .; 1 .; 1 e+1; / / / *x*/; \
         < :; % :; < <=; a+++b; 1+2; a.b; p->q; f(x); x+=1; 1e+5; \
         x y; 1 1; . 1; 1+1; L 'c'; %: %:"
    );
}
