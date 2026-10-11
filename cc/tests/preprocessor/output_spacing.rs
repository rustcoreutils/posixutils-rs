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
/// writes for text that has none, `<built-in>` and `<command-line>`. Tools
/// read the markers as a dependency list: perl's `makedepend` turns each
/// into a make prerequisite, drops those two, and choked on
/// `<builtin:stdarg.h>` ("target pattern contains no '%'") -- c17's bundled
/// headers are not on disk.
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
            assert!(
                matches!(name, "<built-in>" | "<command-line>"),
                "marker {line:?}"
            );
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

/// Each line keeps its indentation, as gcc keeps it: the first token of a
/// line goes back to its column. glibc's `versions.awk` reads the
/// preprocessed `Versions` files by indentation, and with every line flush
/// left it found no version nodes at all. Under `-P` and without it, and
/// after a macro expansion, which stands where its name stood.
#[test]
fn preprocessor_output_keeps_leading_indentation() {
    let src = "lib {\n  VER {\n    sym;\n  };\n};\n\
               #define M(a) a+1\n    M(q) r;\nfoo(a,\n      b);\n   /**/v;\n";
    let want = "lib {\n  VER {\n    sym;\n  };\n};\n    q+1 r;\nfoo(a,\n      b);\n       v;\n";
    let r = preprocess_text("pp_indent", src, &["-P"]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_eq!(r.stdout, want);

    let r = preprocess_text("pp_indent_markers", src, &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    let text: Vec<&str> = r
        .stdout
        .lines()
        .filter(|l| !l.starts_with('#') && !l.is_empty())
        .collect();
    assert_eq!(text.join("\n") + "\n", want);
}

/// A tab is one column, as gcc counts it, however it mixes with spaces and
/// wherever a macro's expansion starts the line. The expected text is
/// gcc 13's.
#[test]
fn preprocessor_output_indents_a_tab_as_one_column() {
    let src = "\tx;\n \t x;\n\t\ty;\n#define M(a) a+1\n\t  M(q) r;\n\
               foo(a,\n\t\tb);\n";
    let want = " x;\n   x;\n  y;\n   q+1 r;\nfoo(a,\n  b);\n";
    let r = preprocess_text("pp_indent_tab", src, &["-P"]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_eq!(r.stdout, want);

    let r = preprocess_text("pp_indent_tab_markers", src, &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    let text: Vec<&str> = r
        .stdout
        .lines()
        .filter(|l| !l.starts_with('#') && !l.is_empty())
        .collect();
    assert_eq!(text.join("\n") + "\n", want);

    // A macro invocation running onto the next line, under `-P`.
    let r = preprocess_text(
        "pp_indent_tab_args",
        "#define M(a) a+1\n\tM(\n\tq) s;\n",
        &["-P"],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_eq!(r.stdout, " q+1 s;\n");
}

/// The same for an assembler source, whose `-E` text is written apart.
#[test]
fn preprocessor_asm_output_keeps_leading_indentation() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_pp_asm_indent_")
        .tempdir()
        .unwrap();
    let s = dir.path().join("t.S");
    std::fs::write(&s, "#define R ret\n  foo:\n    movl $1, %eax\n    R\n").unwrap();
    let r = run_c17(&["-E", "-P", &s.to_string_lossy()]);
    assert!(r.success, "-E of a .S failed: {}", r.stderr);
    let text: Vec<&str> = r.stdout.lines().filter(|l| !l.is_empty()).collect();
    assert_eq!(text, ["  foo:", "    movl $1, %eax", "    ret"]);

    // Tabs, as gcc 13 writes them.
    std::fs::write(&s, "\tfoo:\n \t movl $1, %eax\n#define R ret\n\t\tR\n").unwrap();
    let r = run_c17(&["-E", "-P", &s.to_string_lossy()]);
    assert!(r.success, "-E of a .S failed: {}", r.stderr);
    let text: Vec<&str> = r.stdout.lines().filter(|l| !l.is_empty()).collect();
    assert_eq!(text, [" foo:", "   movl $1, %eax", "  ret"]);
}

/// Without `-P`, a token written on a later source line than the output line
/// it would join goes back to its own line, as gcc's does: one with white
/// space before it, and the first token of a macro's expansion or the first
/// after one. A function-like macro's arguments are the usual way there --
/// the expansion stands on the line of its name, and what follows the `)`
/// on the line of the `)`. c17 left it on the expansion's line and wrote the
/// lines it skipped afterwards, so the next token's line was right but its
/// own was not. From eight lines a marker moves the text, as at the start of
/// a line, where c17 wrote blank lines for exactly eight. Each expected text
/// is gcc 13's, after its marker for the file.
#[test]
fn preprocessor_output_returns_a_token_to_its_line() {
    let cases = [
        (
            "#define M(x) [x]\n\tM(\n\tq) s;\nt;\n",
            "\n [q]\n    s;\nt;\n",
        ),
        (
            "#define M(x,y) [x y]\nint a = M(1,\n\n\n2) + 3;\nint b;\n",
            "\nint a = [1 2]\n\n\n   + 3;\nint b;\n",
        ),
        (
            "#define M(x) [x]\nM(a\n)M(b\n) end\nlast\n",
            "\n[a]\n [b]\n  end\nlast\n",
        ),
        (
            "#define M(x) [x]\n#define N M\nN\n(z) w\nv\n",
            "\n\n[z]\n    w\nv\n",
        ),
        (
            "#define M(x) [x]\nfoo M\n\n\n(\nq\n) bar\nbaz\n",
            "\nfoo [q]\n\n\n\n\n  bar\nbaz\n",
        ),
        (
            "#define M(x) [x]\n  x = M(1\n      ) + M(2\n  ); y;\n",
            "\n  x = [1]\n        + [2]\n   ; y;\n",
        ),
        ("#define M(x) [x]\nM(1\n)x\n", "\n[1]\n x\n"),
        (
            "#define M(x,y) [x y]\nint a = M(1,\n\n\n\n\n\n\n\n\n\n2) + 3;\nint b;\n",
            "\nint a = [1 2]\n# 12\n   + 3;\nint b;\n",
        ),
        // The threshold itself, between two lines of plain text: seven
        // blank lines are written out, eight are a marker.
        ("a\n\n\n\n\n\n\n\nb\n", "a\n\n\n\n\n\n\n\nb\n"),
        ("a\n\n\n\n\n\n\n\n\nb\n", "a\n# 10\nb\n"),
    ];
    for (i, (src, want)) in cases.iter().enumerate() {
        let r = preprocess_text(&format!("pp_token_line{i}"), src, &[]);
        assert!(r.success, "-E failed: {}", r.stderr);
        // The text after the marker naming the file, each later marker
        // without the file name, which is the test's own temporary path.
        let text: String = r
            .stdout
            .lines()
            .skip_while(|l| !l.starts_with("# 1 \""))
            .skip(1)
            .map(|l| match l.strip_prefix("# ") {
                Some(rest) => format!("# {}\n", rest.split(' ').next().unwrap_or("")),
                None => format!("{l}\n"),
            })
            .collect();
        assert_eq!(text, *want, "source:\n{src}");
    }
    // `-P` keeps the run on one line, as gcc's does.
    let r = preprocess_text("pp_token_line_p", cases[0].0, &["-P"]);
    assert_eq!(r.stdout, " [q] s;\nt;\n");
}
