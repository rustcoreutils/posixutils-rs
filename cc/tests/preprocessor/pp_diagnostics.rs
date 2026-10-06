//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Preprocessor diagnostics gcc gives and c17 once did not, each with the
// accept side that proves the check does not fire on correct code.
//

use crate::common::preprocess_text;

/// Run `src` through `c17 -E`, requiring it to succeed and warn `want`.
fn assert_warns(name: &str, src: &str, want: &str) {
    let r = preprocess_text(name, src, &[]);
    assert!(r.success, "{name}: a warning, not an error:\n{}", r.stderr);
    assert!(
        r.stderr.contains(want),
        "{name}: expected {want:?}:\n{}",
        r.stderr
    );
}

/// Run `src` through `c17 -E`, requiring no diagnostic at all.
fn assert_quiet(name: &str, src: &str) {
    let r = preprocess_text(name, src, &[]);
    assert!(r.success && r.stderr.is_empty(), "{name}:\n{}", r.stderr);
}

/// C17 6.10p1 gives these directives exactly one operand; gcc warns about
/// anything after it.
#[test]
fn pp_extra_tokens_after_an_operand_warn() {
    let cases = [
        ("extra_ifdef", "#ifdef A B\n#endif\n", "#ifdef"),
        ("extra_ifndef", "#ifndef A B\n#endif\n", "#ifndef"),
        ("extra_undef", "#undef A B\n", "#undef"),
        ("extra_include", "#include <stddef.h> junk\n", "#include"),
        ("extra_include_q", "#include <stddef.h> \"x\"\n", "#include"),
        ("extra_line", "#line 5 \"f.c\" junk\n", "#line"),
    ];
    for (name, src, directive) in cases {
        assert_warns(
            name,
            src,
            &format!("extra tokens at end of {directive} directive"),
        );
    }
    assert_quiet(
        "extra_none",
        "#ifdef A\n#endif\n#ifndef A\n#endif\n#undef A\n#include <stddef.h>\n",
    );
}

/// `#line` with trailing tokens is still obeyed, as gcc obeys it.
#[test]
fn pp_line_with_extra_tokens_still_applies() {
    let r = preprocess_text(
        "line_extra_applies",
        "#line 41 \"f.c\" junk\nint x = __LINE__;\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert!(r.stdout.contains("int x = 41;"), "{}", r.stdout);
}

/// C17 6.10.3p3: white space separates an object-like macro's name from its
/// replacement list. gcc warns, and still defines the macro.
#[test]
fn pp_object_like_macro_needs_whitespace() {
    let r = preprocess_text("ws_after_name", "#define A+1\nint x = A;\n", &[]);
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stderr
            .contains("ISO C99 requires whitespace after the macro name"),
        "{}",
        r.stderr
    );
    assert!(r.stdout.contains("int x = +1;"), "{}", r.stdout);

    assert_quiet(
        "ws_after_name_ok",
        "#define A +1\n#define B(x) x\n#define C\n#define D\t1\n",
    );
}

/// C17 6.10.3p5: `__VA_ARGS__` only in the replacement list of a variadic
/// macro spelled with `...`.
#[test]
fn pp_stray_va_args_warns() {
    let want = "__VA_ARGS__ can only appear in the expansion of a C99 variadic macro";
    for (name, src) in [
        ("va_nonvariadic", "#define F(x) __VA_ARGS__\n"),
        ("va_as_name", "#define __VA_ARGS__ 1\n"),
        ("va_as_param", "#define F(__VA_ARGS__) 1\n"),
        ("va_gnu_named", "#define G(a...) __VA_ARGS__\n"),
        ("va_text", "int __VA_ARGS__;\n"),
        ("va_if", "#if __VA_ARGS__\n#endif\n"),
        ("va_ifdef", "#ifdef __VA_ARGS__\n#endif\n"),
    ] {
        assert_warns(name, src, want);
    }

    // Once each: an `#if` operand used to be seen twice, before and after
    // its expansion.
    let r = preprocess_text("va_if_once", "#if __VA_ARGS__\n#endif\n", &[]);
    assert_eq!(r.stderr.matches(want).count(), 1, "{}", r.stderr);

    assert_quiet(
        "va_ok",
        "#define H(...) __VA_ARGS__\n#define S(...) #__VA_ARGS__\nH(1) S(a)\n",
    );
}

/// C17 6.10.2p4: the operand has to be `<...>` or `"..."` after replacement;
/// `#include stdio.h` is neither, and gcc does not guess.
#[test]
fn pp_include_needs_a_header_name() {
    let r = preprocess_text("include_bare", "#include stdio.h\n", &[]);
    assert!(!r.success);
    assert!(
        r.stderr
            .contains("#include expects \"FILENAME\" or <FILENAME>"),
        "{}",
        r.stderr
    );

    // A macro that expands to a header name is still fine.
    assert_quiet(
        "include_macro",
        "#define HDR <stddef.h>\n#include HDR\n#define Q \"c17-no-such.h\"\n\
         #if __has_include(Q)\n#error\n#endif\n",
    );
}

/// `..` is not a punctuator (C17 6.4.6p1), so `.` ## `.` cannot paste into
/// one token, and gcc rejects it.
#[test]
fn pp_paste_of_two_dots_is_invalid() {
    let r = preprocess_text("paste_dots", "#define c(a,b) a##b\nc(.,.)\n", &[]);
    assert!(!r.success);
    assert!(
        r.stderr
            .contains("pasting \".\" and \".\" does not give a valid preprocessing token"),
        "{}",
        r.stderr
    );

    // `...` is one token however it is reached; `..` is two.
    let r = preprocess_text(
        "dots_lex",
        "#define S(x) #x\nS(..) S(. . .) S(...) S(....)\nint f(int, ...);\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stdout.contains("\"..\" \". . .\" \"...\" \"....\""),
        "{}",
        r.stdout
    );
}

/// C17 6.10.8p2 forbids `#define` and `#undef` of the standard's names, and
/// gcc warns when any predefine is redefined to something else. A
/// redefinition to the same thing is quiet, as are the feature-test macros
/// c17 predefines where gcc does not.
#[test]
fn pp_predefined_macro_redefinition() {
    let different = "redefined: it is predefined with a different definition";
    assert_warns("redef_stdc", "#define __STDC__ 2\n", different);
    assert_warns("redef_version", "#define __STDC_VERSION__ 1\n", different);
    assert_warns("redef_char_bit", "#define __CHAR_BIT__ 9\n", different);
    assert_warns(
        "undef_version",
        "#undef __STDC_VERSION__\n",
        "undefining \"__STDC_VERSION__\"",
    );
    assert_warns(
        "undef_hosted",
        "#undef __STDC_HOSTED__\n",
        "undefining \"__STDC_HOSTED__\"",
    );

    assert_quiet(
        "redef_quiet",
        "#define __STDC__ 1\n#define __CHAR_BIT__ 8\n#undef __CHAR_BIT__\n\
         #define _GNU_SOURCE\n#define _XOPEN_SOURCE 700\n\
         #define _POSIX_C_SOURCE 200809L\n#define _DEFAULT_SOURCE 1\n",
    );

    let r = preprocess_text("undef_defined", "#undef defined\n", &[]);
    assert!(!r.success);
    assert!(
        r.stderr
            .contains("\"defined\" cannot be used as a macro name"),
        "{}",
        r.stderr
    );
}

/// `#if` evaluates only the arm of `?:` it takes (C17 6.5.15p4), and the
/// result has the type both arms convert to (p5).
#[test]
fn pp_conditional_operator_in_if() {
    let r = preprocess_text(
        "if_ternary",
        "#if 1 ? 2 : (1/0)\nA\n#endif\n#if 0 ? (1/0) : 3\nB\n#endif\n\
         #if (1 ? -1 : 0u) > 0\nC\n#endif\n#if (0 ? 1 : 2 ? 3 : (1/0)) == 3\nE\n#endif\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    for want in ["A", "B", "C", "E"] {
        assert!(r.stdout.lines().any(|l| l == want), "{want}:\n{}", r.stdout);
    }

    let r = preprocess_text("if_ternary_taken", "#if 0 ? 1 : (1/0)\n#endif\n", &[]);
    assert!(!r.success);
    assert!(r.stderr.contains("division by zero"), "{}", r.stderr);
}

/// `__VA_ARGS__` is the variadic arguments with their separating commas, each
/// spaced as written (C17 6.10.3.1p2, 6.10.3.2p2): `#__VA_ARGS__` of `a , b`
/// is `"a , b"`. The commas were rebuilt with no space before them.
#[test]
fn pp_va_args_keeps_comma_spacing() {
    let r = preprocess_text(
        "va_comma_spacing",
        "#define H(...) #__VA_ARGS__\n#define V(...) __VA_ARGS__\n\
         #define G(x, ...) #__VA_ARGS__\n\
         H(a , b) H( x ,y , z ) H(a,b) H(a ,\nb)\nV(1 , 2,3)\nG(q, r , s)\n",
        &["-P"],
    );
    assert!(r.success, "{}", r.stderr);
    let out: Vec<&str> = r.stdout.lines().filter(|l| !l.is_empty()).collect();
    assert_eq!(
        out,
        [
            "\"a , b\" \"x ,y , z\" \"a,b\" \"a , b\"",
            "1 , 2,3",
            "\"r , s\""
        ],
        "{}",
        r.stdout
    );
}

/// A `##` result is spaced as its left operand was. It took the invocation's
/// spacing instead, so `S(a[b##n])` stringified as `"a[ b1]"` -- which broke
/// systemtap's `<sys/sdt.h>`, whose probe macros build asm operand names
/// this way.
#[test]
fn pp_paste_result_keeps_left_operand_spacing() {
    let r = preprocess_text(
        "paste_spacing",
        "#define S(x) #x\n#define F(n) S(a[b##n])\n#define L(x) S(L##x)\n\
         #define G(n) S(x = b##n)\nF(1) L(\"s\") G(2)\n",
        &["-P"],
    );
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stdout.contains("\"a[b1]\" \"L\\\"s\\\"\" \"x = b2\""),
        "{}",
        r.stdout
    );
}

/// `-E` decodes no literal, so gcc says nothing about `\x` with no digits
/// and copies it through; only a compile refuses it.
#[test]
fn pp_empty_hex_escape_is_copied_quietly() {
    let r = preprocess_text(
        "hex_e",
        "const char *s = \"\\x\";\nint c = '\\x';\n",
        &["-P"],
    );
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);
    assert!(r.stdout.contains("\"\\x\""), "{}", r.stdout);
    let c = crate::common::compile_rejected("hex_c", "const char *s = \"\\x\";\n");
    assert!(
        c.contains("error: \\x used with no following hex digits"),
        "{c}"
    );
}

/// gcc's `-E` gives its pedwarn about a literal left open and copies the
/// line as written -- no quote is invented; a compile refuses the token.
#[test]
fn pp_unterminated_literal_is_copied_as_written() {
    let src = "const char *s = \"abc;\nint c = 'a;\nint x;\n";
    let r = preprocess_text("open_literal_e", src, &["-P"]);
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stderr
            .contains("warning: missing terminating \" character")
            && r.stderr
                .contains("warning: missing terminating ' character"),
        "{}",
        r.stderr
    );
    assert!(r.stdout.contains("= \"abc;\n"), "{}", r.stdout);
    assert!(r.stdout.contains("= 'a;\n"), "{}", r.stdout);
    let c = crate::common::compile_rejected(
        "open_literal_c",
        "unsigned long n = sizeof \"abc\n;\nint main(void){return 0;}\n",
    );
    assert!(c.contains("error: missing terminating \" character"), "{c}");
}

/// An unterminated comment and an invalid directive are errors under `-E`
/// as well, as gcc's are.
#[test]
fn pp_unterminated_comment_and_invalid_directive_fail() {
    for (name, src, msg) in [
        (
            "open_comment_e",
            "int a;\n/* foo\n",
            "error: unterminated comment",
        ),
        (
            "bad_directive_e",
            "#foo bar\nint x;\n",
            "error: invalid preprocessing directive #foo",
        ),
    ] {
        let r = preprocess_text(name, src, &[]);
        assert!(!r.success, "{name}: should fail:\n{}", r.stderr);
        assert!(r.stderr.contains(msg), "{name}: {}", r.stderr);
    }
}

/// gcc's default pedwarn for signed overflow in `#if`, which `-w` hides.
#[test]
fn pp_if_signed_overflow_warns() {
    let src = "#if 9223372036854775807 + 1\n#endif\n#if 9223372036854775807 + 1u\n#endif\n";
    let r = preprocess_text("if_overflow", src, &[]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stderr
            .matches("warning: integer overflow in preprocessor expression")
            .count(),
        1,
        "{}",
        r.stderr
    );
    let quiet = preprocess_text("if_overflow_w", src, &["-w"]);
    assert!(quiet.success && quiet.stderr.is_empty(), "{}", quiet.stderr);
}
