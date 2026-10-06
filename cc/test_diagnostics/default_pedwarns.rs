//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's two kinds of pedwarn. The default kind is a warning that
// `-pedantic-errors` makes an error; the `-Wpedantic` kind is silent until
// `-pedantic` asks for it. c17 gives each construct the verdict gcc gives it,
// in gcc's words, and an error only where gcc 14 has one.
//

use crate::test_compile::{compile, compile_accepted};

/// Constraint violations gcc 13 and 14 only warn about by default: accepted
/// with gcc's warning, refused under `-pedantic-errors`.
const DEFAULT_PEDWARNS: &[(&str, &str, &str)] = &[
    (
        "struct_member_missing_semicolon",
        "struct S { int a; int b };\nint main(void){ struct S s = {1, 2}; return s.b - 2; }\n",
        "no semicolon at end of struct or union",
    ),
    (
        "octal_escape_out_of_range",
        "int c = '\\400';\n",
        "octal escape sequence out of range",
    ),
    (
        "integer_constant_too_large",
        "unsigned long long x = 123456789012345678901234567890;\n",
        "integer constant is too large for its type",
    ),
    (
        "integer_constant_too_large_in_if",
        "#if 0 && 123456789012345678901234567890\n#endif\nint x;\n",
        "integer constant is too large for its type",
    ),
    (
        "line_number_wraps",
        "#line 4294967297\nint x;\n",
        "line number out of range",
    ),
    (
        "useless_type_name",
        "int;\n",
        "useless type name in empty declaration",
    ),
];

#[test]
fn gccs_default_pedwarns_warn_and_are_errors_under_pedantic_errors() {
    for (name, src, msg) in DEFAULT_PEDWARNS {
        let warned = compile_accepted(name, src, &[]);
        assert!(
            warned.contains(&format!("warning: {msg}")),
            "{name}: expected the warning {msg:?}:\n{warned}"
        );
        // `-Wno-pedantic` is about the other kind, and leaves these alone.
        let still = compile_accepted(name, src, &["-Wno-pedantic"]);
        assert!(
            still.contains(msg),
            "{name}: -Wno-pedantic hid it:\n{still}"
        );
        let strict = compile(name, src, &["-pedantic-errors"]);
        assert!(
            !strict.success && strict.stderr.contains(&format!("error: {msg}")),
            "{name}: -pedantic-errors should refuse it:\n{}",
            strict.stderr
        );
    }
}

/// `-pedantic-errors` makes errors of what is *given*. As in gcc, `-w` gives
/// nothing, and a system header -- which uses the extensions it objects to --
/// is not held to it.
#[test]
fn pedantic_errors_do_not_reach_hidden_warnings() {
    let src = "struct S { int a; int b };\n";
    let quiet = compile("pedwarn_w", src, &["-w", "-pedantic-errors"]);
    assert!(
        quiet.success && quiet.stderr.is_empty(),
        "-w: {}",
        quiet.stderr
    );
    let system = "# 1 \"sys.h\" 1 3\n\
                  struct S { int a; int b };\n\
                  void f(void); static void g(int c, int x) { c ? f() : x; }\n\
                  # 4 \"main.c\" 2\n\
                  int z;\n";
    let sys = compile("pedwarn_system_header", system, &["-pedantic-errors"]);
    assert!(sys.success && sys.stderr.is_empty(), "{}", sys.stderr);
}

/// What gcc reports only under `-pedantic`: silent by default, a warning
/// under `-pedantic`, an error under `-pedantic-errors`.
#[test]
fn gccs_pedantic_only_diagnostics_are_silent_by_default() {
    for (name, src, msg) in [
        (
            "label_at_end_of_block",
            "void f(void){ goto l; l: }\n",
            "label at end of compound statement",
        ),
        (
            "enumerator_outside_int",
            "enum E { A = 0x80000000 };\n",
            "ISO C restricts enumerator values to range of 'int'",
        ),
        (
            "line_number_zero",
            "#line 0\nint x;\n",
            "line number out of range",
        ),
        (
            "line_number_past_c17s_bound",
            "#line 3000000000\nint x;\n",
            "line number out of range",
        ),
    ] {
        let quiet = compile_accepted(name, src, &[]);
        assert!(!quiet.contains(msg), "{name}: warned by default:\n{quiet}");
        let loud = compile_accepted(name, src, &["-pedantic"]);
        assert!(loud.contains(&format!("warning: {msg}")), "{name}:\n{loud}");
        let strict = compile(name, src, &["-pedantic-errors"]);
        assert!(
            !strict.success && strict.stderr.contains(msg),
            "{name}:\n{}",
            strict.stderr
        );
    }
}

/// A null character between tokens is whitespace, with gcc's plain warning
/// once per run of them -- not a stray token that derails the declaration.
#[test]
fn null_characters_are_ignored_with_a_warning() {
    let src = "int x;\0\0 int y;\0\nint z\0;\n";
    let warned = compile_accepted("null_chars", src, &[]);
    assert_eq!(
        warned.matches("warning: null character(s) ignored").count(),
        3,
        "{warned}"
    );
    // A plain warning, which `-pedantic-errors` leaves alone.
    compile_accepted("null_chars_strict", src, &["-pedantic-errors"]);
}
