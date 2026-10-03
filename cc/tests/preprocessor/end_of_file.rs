//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A translation unit that ends in the middle of something.
//
// A directive reads to the next token that begins a line, and `_Pragma`
// reads its operand token by token. Each once took the end-of-stream marker
// as part of what it was reading -- a last line closed by a backslash-newline
// has no newline of its own -- and the parser, never seeing the end of the
// file, looped allocating until the machine ran out of memory. Every case is
// run under a time limit, so a regression fails instead of hanging the suite.
//

use crate::common::compile_bounded;

const LIMIT_SECS: u64 = 30;

/// A directive whose line is continued into the end of the file: the
/// continuation joins nothing, and the directive ends where the file does.
#[test]
fn eof_directive_continued_into_end_of_file() {
    let accepted = [
        ("eof_define", "int x;\n#define X \\\n"),
        ("eof_define_no_newline", "int x;\n#define X \\"),
        ("eof_undef", "int x;\n#undef X \\\n"),
        ("eof_pragma", "int x;\n#pragma weak x \\\n"),
    ];
    for (name, src) in accepted {
        let run = compile_bounded(name, src, LIMIT_SECS);
        assert!(run.success, "{name}: rejected:\n{}", run.stderr);
    }

    // Rejected, and with their own diagnostic only: the end-of-stream marker
    // used to be read as an operand, so `#error` printed `<STREAM_END>` and
    // `#if` reported a missing operator before an empty token.
    let rejected = [
        ("eof_if", "#if 1 \\\n", "unterminated #if"),
        ("eof_error", "#error foo \\\n", "#error foo"),
        (
            "eof_include",
            "#include \"c17-no-such-header.h\" \\\n",
            "file not found",
        ),
    ];
    for (name, src, expected) in rejected {
        let run = compile_bounded(name, src, LIMIT_SECS);
        assert!(!run.success, "{name}: accepted");
        assert!(run.stderr.contains(expected), "{name}:\n{}", run.stderr);
        assert!(
            !run.stderr.contains("STREAM_END"),
            "{name}:\n{}",
            run.stderr
        );
        assert!(
            !run.stderr.contains("missing binary operator"),
            "{name}:\n{}",
            run.stderr
        );
    }
}

/// `_Pragma` with its operand cut short by the end of the file is gcc's
/// error, not an infinite loop.
#[test]
fn eof_pragma_operator_without_operand() {
    let cases = [
        ("eof_pragma_bare", "int x;\n_Pragma"),
        ("eof_pragma_bare_nl", "_Pragma\n"),
        ("eof_pragma_paren", "int x;\n_Pragma("),
        ("eof_pragma_unclosed", "int x;\n_Pragma(\"once\""),
        ("eof_pragma_not_string", "int x;\n_Pragma(x)\nint y;\n"),
    ];
    for (name, src) in cases {
        let run = compile_bounded(name, src, LIMIT_SECS);
        assert!(!run.success, "{name}: accepted");
        assert!(
            run.stderr
                .contains("_Pragma takes a parenthesized string literal"),
            "{name}:\n{}",
            run.stderr
        );
    }

    // The well-formed operator still works, in the file and from a macro.
    let run = compile_bounded(
        "eof_pragma_ok",
        "#define P _Pragma(\"pack(1)\")\nP struct s { char a; int b; };\n\
         _Static_assert(sizeof(struct s) == 5, \"packed\");\n",
        LIMIT_SECS,
    );
    assert!(run.success, "{}", run.stderr);
}
