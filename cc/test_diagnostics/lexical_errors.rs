//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Lexical and directive mistakes gcc 13 refuses: an empty `\x` escape, a
// comment or literal left open, and an invalid directive. Each is checked
// where gcc refuses it, under `-w` (an error is not a warning to hide), and
// where gcc stays quiet -- a skipped group, a macro never expanded.
//

use crate::test_compile::{compile, compile_accepted, compile_rejected_with};

/// Require `src` refused with `msg` as an error, with and without `-w`.
#[track_caller]
fn assert_refused(name: &str, src: &str, msg: &str) {
    for flags in [&[][..], &["-w"][..]] {
        let stderr = compile_rejected_with(name, src, flags);
        assert!(
            stderr.contains(&format!("error: {msg}")),
            "{name} {flags:?}: expected the error {msg:?}:\n{stderr}"
        );
    }
}

/// C17 6.4.4.4p1: `\x` needs a hex digit. gcc's error, in a string, a
/// character constant, and `#if`.
#[test]
fn empty_hex_escape_is_an_error() {
    let msg = "\\x used with no following hex digits";
    for (name, src) in [
        ("hex_string", "const char *s = \"\\x\";\n"),
        ("hex_string_g", "const char *s = \"a\\xg\";\n"),
        ("hex_char", "int c = '\\x';\n"),
        ("hex_wide", "int c = L'\\x';\n"),
        ("hex_if", "#if '\\x' == 'x'\n#endif\nint x;\n"),
    ] {
        assert_refused(name, src, msg);
    }
}

/// A literal that is never decoded is never diagnosed: gcc is silent about
/// `\x` in a skipped group or a macro nobody expands.
#[test]
fn empty_hex_escape_not_decoded_is_quiet() {
    let src = "#if 0\nconst char *s = \"\\x\";\n#endif\n#define S \"\\x\"\nint x = '\\x41';\n";
    let stderr = compile_accepted("hex_quiet", src, &[]);
    assert!(stderr.is_empty(), "{stderr}");
}

/// C17 6.4.9p1: a comment ends with `*/`. One that reaches the end of the
/// file is gcc's error, even inside a skipped group.
#[test]
fn unterminated_comment_is_an_error() {
    assert_refused("comment_eof", "int a;\n/* foo\n", "unterminated comment");
    let c = compile("comment_eof_skipped", "#if 0\n/* foo\n#endif\n", &[]);
    assert!(
        !c.success && c.stderr.contains("2:1: error: unterminated comment"),
        "{}",
        c.stderr
    );
}

/// C17 6.4.5p1 and 6.4.4.4p1: a literal ends on the line it starts. gcc's
/// lexer gives a default pedwarn, and the token, which is no literal, is an
/// error once it reaches the compiler -- even where the rest of the line would
/// still parse.
#[test]
fn unterminated_literal_in_compiled_code_is_an_error() {
    for (name, src, delim) in [
        ("string_sizeof", "unsigned long n = sizeof \"abc\n;\n", '"'),
        ("char_sizeof", "unsigned long n = sizeof 'a\n;\n", '\''),
        ("string_eof", "int x; unsigned long n = sizeof \"abc", '"'),
        (
            "string_through_macro",
            "#define S \"abc\nunsigned long n = sizeof S\n;\n",
            '"',
        ),
    ] {
        assert_refused(name, src, &format!("missing terminating {delim} character"));
    }
}

/// Where the token never reaches the compiler, the lexer's pedwarn is all
/// gcc gives: a skipped group, a macro defined but not expanded.
#[test]
fn unterminated_literal_not_compiled_is_a_pedwarn() {
    for (name, src) in [
        (
            "string_skipped",
            "#if 0\nconst char *s = \"abc;\n#endif\nint x;\n",
        ),
        ("string_unused_macro", "#define S \"abc\nint x;\n"),
    ] {
        let stderr = compile_accepted(name, src, &[]);
        assert!(
            stderr.contains("warning: missing terminating \" character")
                && !stderr.contains("error"),
            "{name}: {stderr}"
        );
        compile_rejected_with(name, src, &["-pedantic-errors"]);
        let quiet = compile_accepted(name, src, &["-w"]);
        assert!(quiet.is_empty(), "{name}: {quiet}");
    }
}

/// C17 6.10p1: a directive's name must be one of the directives. gcc's error
/// names the token after the `#`.
#[test]
fn invalid_directive_is_an_error() {
    for (name, src, spelled) in [
        ("dir_word", "#foo\nint x;\n", "#foo"),
        ("dir_case", "#Define X 1\nint x;\n", "#Define"),
        ("dir_punct", "#!foo\nint x;\n", "#!"),
    ] {
        assert_refused(
            name,
            src,
            &format!("invalid preprocessing directive {spelled}"),
        );
    }
    assert_refused(
        "dir_linemarker_word",
        "# 12abc\nint x;\n",
        "\"12abc\" after # is not a positive integer",
    );
}

/// The null directive and a linemarker are directives, and a skipped group
/// is only searched for the conditionals that nest.
#[test]
fn valid_and_skipped_directives_are_quiet() {
    let src = "#\n# 33 \"file.c\"\n#if 0\n#foo\n#!\n#endif\nint x;\n";
    let stderr = compile_accepted("dir_quiet", src, &[]);
    assert!(stderr.is_empty(), "{stderr}");
}
