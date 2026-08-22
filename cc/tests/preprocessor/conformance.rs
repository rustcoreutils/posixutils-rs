//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// POSIX/C17 preprocessor conformance tests that assert on `-E` *text*.
//
// The rest of the preprocessor suite compiles a program and checks its exit
// code, which cannot observe spacing, stringification, or which branch of a
// `#if` survived — the defects in this file all hid behind that.
//

use crate::common::{preprocess_text, run_c17};

/// Assert the preprocessed text contains `needle`.
fn assert_has(out: &str, needle: &str, what: &str) {
    assert!(
        out.contains(needle),
        "{}: expected {:?} in -E output:\n{}",
        what,
        needle,
        out
    );
}

/// Assert the preprocessed text does *not* contain `needle`.
fn assert_lacks(out: &str, needle: &str, what: &str) {
    assert!(
        !out.contains(needle),
        "{}: did not expect {:?} in -E output:\n{}",
        what,
        needle,
        out
    );
}

// ============================================================================
// Literal payloads are byte sequences
// ============================================================================

/// A string literal's payload holds one `char` per source *byte*. Rendering it
/// through a Rust `String` UTF-8-encoded each of those chars, so every source
/// byte >= 0x80 came out of `c17 -E` as two: `"café"` gained a byte, and
/// preprocessing a file then compiling it changed what the string held.
#[test]
fn preprocessor_non_ascii_literal_survives_byte_for_byte() {
    let r = preprocess_text(
        "utf8_literal",
        "const char *s = \"café ☕\";\nconst char c = 'é';\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "\"café ☕\"", "utf8 string literal");
    assert_has(&r.stdout, "'é'", "utf8 char literal");
    assert_lacks(&r.stdout, "Ã", "double-encoded UTF-8");
}

/// `__FILE__` and a stringified identifier arrive as Rust text rather than as
/// source bytes, so they have to be converted into the payload form or the two
/// conventions mix inside one literal.
#[test]
fn preprocessor_synthesized_literals_use_payload_form() {
    let r = preprocess_text(
        "utf8_synth",
        "#define S(x) #x\nconst char *a = __FILE__;\nconst char *b = S(caf\\u00e9);\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "utf8_synth", "__FILE__");
    assert_has(&r.stdout, "\"café\"", "stringified UCN identifier");
    assert_lacks(&r.stdout, "Ã", "double-encoded UTF-8");
}

/// A header name is a literal payload -- source bytes, one per `char` -- and
/// has to be decoded before it can be opened as a path. Using it as text
/// looked for `cafÃ©.h` and reported the real file missing.
#[test]
fn preprocessor_non_ascii_header_name_opens() {
    let dir = tempfile::Builder::new()
        .prefix("c17_utf8_include_")
        .tempdir()
        .expect("failed to create work dir");
    std::fs::write(dir.path().join("café.h"), "int seven(void){return 7;}\n").unwrap();
    let src = dir.path().join("main.c");
    std::fs::write(
        &src,
        "#include \"café.h\"\nint main(void){return seven()-7;}\n",
    )
    .unwrap();

    let exe = dir.path().join("prog");
    let r = run_c17(&[
        &src.to_string_lossy(),
        "-o",
        &exe.to_string_lossy(),
        "-I",
        &dir.path().to_string_lossy(),
    ]);
    assert!(
        r.success,
        "compiling with a UTF-8 header name failed:\n{}",
        r.stderr
    );
    let status = std::process::Command::new(&exe)
        .status()
        .expect("failed to run the built program");
    assert_eq!(
        status.code(),
        Some(0),
        "wrong result from the included function"
    );
}

/// A `#error` message is the directive's tokens spelled as they were written
/// -- GCC echoes the line verbatim. The renderer handled four token types and
/// silently dropped the rest, so quotes vanished from strings and character
/// and wide-string operands disappeared entirely.
#[test]
fn preprocessor_error_message_spells_every_token() {
    let r = preprocess_text(
        "error_spelling",
        "#error \"quoted\" and 'c' and L\"wide\" and 3.5 >> 1\n",
        &[],
    );
    assert!(!r.success, "#error must fail the run");
    assert_has(
        &r.stderr,
        "#error \"quoted\" and 'c' and L\"wide\" and 3.5 >> 1",
        "#error message",
    );
}

/// C99 6.10.3.2p2 asks `#` for "the spelling of the preprocessing token", and
/// `-E` has to round-trip it. `u8"..."` has type `char[]` (C11 6.4.5p6) so it
/// folds into the narrow string token, and the prefix was dropped with it.
#[test]
fn preprocessor_u8_prefix_survives() {
    let r = preprocess_text(
        "u8_prefix",
        "#define S(x) #x\n#define B u8\"body\"\n\
         const char *a = S(u8\"hi\");\nconst char *b = B;\nconst char *c = u8\"plain\";\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "\"u8\\\"hi\\\"\"", "stringified u8 literal");
    assert_has(&r.stdout, "u8\"body\"", "u8 literal from a macro body");
    assert_has(&r.stdout, "u8\"plain\"", "u8 literal in plain text");
}

/// C99 6.4.7: a header name is one preprocessing token, and nothing inside it
/// is reinterpreted. Lexing the characters as ordinary tokens meant `//`
/// became a comment and an apostrophe opened a character literal, so the
/// directive was destroyed before it could be reassembled.
#[test]
fn preprocessor_header_name_is_one_token() {
    let dir = tempfile::Builder::new()
        .prefix("c17_header_name_")
        .tempdir()
        .expect("failed to create work dir");
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    std::fs::write(dir.path().join("sub/t.h"), "int seven(void){return 7;}\n").unwrap();
    std::fs::write(dir.path().join("it's.h"), "int eight(void){return 8;}\n").unwrap();

    for (header, call, want) in [
        ("<sub//t.h>", "seven()", 7),
        ("<it's.h>", "eight()", 8),
        ("\"it's.h\"", "eight()", 8),
    ] {
        let src = dir.path().join("main.c");
        std::fs::write(
            &src,
            format!("#include {header}\nint main(void){{return {call}-{want};}}\n"),
        )
        .unwrap();
        let exe = dir.path().join("prog");
        let r = run_c17(&[
            &src.to_string_lossy(),
            "-o",
            &exe.to_string_lossy(),
            "-I",
            &dir.path().to_string_lossy(),
        ]);
        assert!(r.success, "#include {header} failed:\n{}", r.stderr);
        let status = std::process::Command::new(&exe).status().unwrap();
        assert_eq!(status.code(), Some(0), "wrong result for #include {header}");
    }
}

/// C99 6.4.6p3: the six digraphs "behave the same, respectively, as the six
/// primary tokens ... **except for their spelling**". 6.10.3.2p2 then asks `#`
/// for "the spelling of the preprocessing token", and `-E` has to round-trip
/// it. c17 lexed each digraph straight to its primary token, so the spelling
/// was gone before either could ask: `S(<:1:>)` stringified to `"[1]"` and
/// `c17 -E` rewrote every digraph in the output.
#[test]
fn preprocessor_digraph_spelling_survives() {
    let r = preprocess_text(
        "digraph_spelling",
        "#define S(x) #x\n\
         a: S(<:1:>)\n\
         b: S(%:%:)\n\
         c: S(<% %>)\n\
         d: <% %> <: :> %: %:%:\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "\"<:1:>\"", "stringified <: :>");
    assert_has(&r.stdout, "\"%:%:\"", "stringified %:%:");
    assert_has(&r.stdout, "\"<% %>\"", "stringified <% %>");
    assert_has(&r.stdout, "<% %> <: :> %: %:%:", "digraphs in plain text");
    // The primary spellings must not appear where a digraph was written.
    assert_lacks(
        &r.stdout,
        "\"[1]\"",
        "digraph rewritten to its primary token",
    );
    assert_lacks(
        &r.stdout,
        "\"##\"",
        "digraph rewritten to its primary token",
    );
}

/// A digraph still *means* its primary token everywhere else (6.4.6p3), so it
/// has to keep working as syntax, as a directive introducer, and as `##`.
#[test]
fn preprocessor_digraphs_still_mean_their_primary_tokens() {
    let r = preprocess_text(
        "digraph_meaning",
        "%:define CAT(a,b) a%:%:b\n\
         %:define STR(x) %:x\n\
         int main(void) <% int v<:2:> = <%7,8%>; return CAT(v,)<:0:> + v<:1:> - 15; %>\n\
         s: STR(q)\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "\"q\"", "%: as the stringify operator");
    assert_has(&r.stdout, "v<:0:>", "%:%: as ## still pastes");
}

/// C99 6.10.3.1p2 makes `__VA_ARGS__` stand for the *whole* variadic token
/// sequence, commas included. c17 stringified only its first element, so
/// `#define V(...) #__VA_ARGS__` turned `V(1,2,3)` into `"1"` -- everything
/// after the first comma silently gone, with no diagnostic. The idiom is
/// common in logging macros.
#[test]
fn preprocessor_stringified_va_args_keeps_every_argument() {
    let r = preprocess_text(
        "va_args_stringify",
        "#define V(...) #__VA_ARGS__\n\
         #define W(a, ...) a: #__VA_ARGS__\n\
         #define X(...) __VA_ARGS__\n\
         A V(1,2,3)\n\
         B V(1)\n\
         C V()\n\
         D W(k, 1,2)\n\
         E X(1,2,3)\n\
         F V(f(1,2),3)\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "A \"1,2,3\"", "all variadic arguments");
    assert_has(&r.stdout, "B \"1\"", "one variadic argument");
    assert_has(&r.stdout, "C \"\"", "no variadic arguments");
    assert_has(
        &r.stdout,
        "D k: \"1,2\"",
        "named parameter before the variadic ones",
    );
    assert_has(
        &r.stdout,
        "E 1,2,3",
        "unstringified __VA_ARGS__ does not gain spaces",
    );
    assert_has(
        &r.stdout,
        "F \"f(1,2),3\"",
        "a parenthesised comma is not a separator",
    );
}

/// `__VA_ARGS__` is spaced as it was written in the macro *body*, like every
/// other body token. Taking the spacing from the invocation instead ran the
/// expansion into the token before it.
#[test]
fn preprocessor_va_args_takes_the_body_spacing() {
    let r = preprocess_text(
        "va_args_spacing",
        "#define F(...) x __VA_ARGS__\n#define G(...) (__VA_ARGS__)\n\
         A F(*p)\nB F(y)\nC G(y)\nD G(1,2)\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "A x *p", "a space in the body is kept");
    assert_has(&r.stdout, "B x y", "and still separates identifiers");
    // No space in the body, and none invented.
    assert_has(&r.stdout, "C (y)", "no space where the body had none");
    assert_has(&r.stdout, "D (1,2)", "nor around the separators");
}

/// Pins a known divergence, so closing it is a deliberate change.
///
/// The argument splitter discards the separating comma, so the white space
/// that preceded it is gone by the time `#__VA_ARGS__` rebuilds the sequence:
/// `V(a , b)` stringifies as `"a, b"` where GCC gives `"a , b"`. Everything
/// else about the sequence is right; only the space *before* a separator is
/// lost, and only when the source writes one.
#[test]
fn preprocessor_va_args_loses_space_before_a_separator() {
    let r = preprocess_text(
        "va_args_sep_space",
        "#define V(...) #__VA_ARGS__\nA V(a , b)\nB V(a, b)\nC V(a ,b)\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    // What GCC gives is "a , b"; c17 drops the space before the comma.
    assert_has(&r.stdout, "A \"a, b\"", "space before a separator");
    // Space *after* a separator is preserved, and so is its absence.
    assert_has(&r.stdout, "B \"a, b\"", "space after a separator");
    assert_has(&r.stdout, "C \"a,b\"", "no space around a separator");
}

/// A non-ASCII byte outside a literal lexes as its own single-character
/// punctuator, so its value is a source *byte*. Both consumers of a
/// punctuator's spelling have to keep that apart from text: `#`
/// stringification builds a payload (one `char` per byte), and a `#error`
/// message is assembled from bytes and decoded once at the end.
///
/// Rendering the byte through a Rust `String` UTF-8-encodes it. Doing that
/// inside stringification doubled it; doing it per-token in the message
/// renderer left a replacement character, because one byte of a multi-byte
/// character is not valid UTF-8 on its own.
#[test]
fn preprocessor_non_ascii_punctuator_bytes_survive() {
    let r = preprocess_text(
        "punct_bytes",
        "#define S(x) #x\nA S(café)\nB S(a<:1:>b)\nC S(a >> b)\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "A \"café\"", "stringified non-ASCII bytes");
    assert_lacks(&r.stdout, "Ã", "double-encoded UTF-8");
    assert_lacks(&r.stdout, "\u{fffd}", "a byte lost to lossy decoding");
    // The text spellings still work.
    assert_has(&r.stdout, "B \"a<:1:>b\"", "digraph spelling");
    assert_has(&r.stdout, "C \"a >> b\"", "multi-character operator");
}

/// The same bytes in a `#error` message, which is assembled by a different
/// renderer and so needs the same care.
#[test]
fn preprocessor_error_message_keeps_non_ascii_bytes() {
    let r = preprocess_text("err_bytes", "#error café is bad\n", &[]);
    assert!(!r.success, "#error must fail the run");
    assert!(
        r.stderr.contains("café is bad"),
        "expected the message verbatim, got:\n{}",
        r.stderr
    );
    assert!(
        !r.stderr.contains('\u{fffd}'),
        "a byte was lost to lossy decoding:\n{}",
        r.stderr
    );
}

// ============================================================================
// #P2 — the null directive
// ============================================================================

/// C17 6.10p7: a `#` alone on a line has no effect. It used to consume the
/// first token of the *next* line as a directive name and then skip the rest
/// of that line, silently deleting a line of source.
#[test]
fn preprocessor_null_directive_keeps_the_following_line() {
    let r = preprocess_text("null_directive", "#\nint kept_one;\nint kept_two;\n", &[]);
    assert!(r.success, "null directive should not fail: {}", r.stderr);
    assert_has(&r.stdout, "kept_one", "#P2");
    assert_has(&r.stdout, "kept_two", "#P2");
    assert_lacks(&r.stderr, "unknown preprocessor directive", "#P2");
}

/// A null directive must not disturb the directives around it.
#[test]
fn preprocessor_null_directive_between_real_directives() {
    let r = preprocess_text(
        "null_between",
        "#define A 5\n#\n#ifdef A\nint have_a = A;\n#endif\n#\nint after_null;\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert_has(&r.stdout, "have_a = 5", "#P2");
    assert_has(&r.stdout, "after_null", "#P2");
}

// ============================================================================
// #P10 — a comment is whitespace
// ============================================================================

/// Translation phase 3 replaces a comment with one space, so `S(a/**/b)`
/// stringifies as `"a b"`. It used to yield `"ab"`.
#[test]
fn preprocessor_comment_is_whitespace_for_stringification() {
    let r = preprocess_text(
        "comment_ws",
        "#define S(x) #x\nconst char *a = S(a/**/b);\nconst char *b = S(c d);\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert_has(&r.stdout, "\"a b\"", "#P10 block comment becomes a space");
    // The control: real whitespace already worked, and must keep working.
    assert_has(&r.stdout, "\"c d\"", "#P10 plain whitespace");
}

// ============================================================================
// #P6 — `#if` arithmetic is intmax_t/uintmax_t, not i64
// ============================================================================

/// `#if 0xFFFFFFFFFFFFFFFF` used to take the *false* branch, because the
/// constant overflowed `i64` and was silently replaced by 0.
#[test]
fn preprocessor_if_handles_full_width_unsigned_constants() {
    let r = preprocess_text(
        "if_big",
        "#if 0xFFFFFFFFFFFFFFFF\nint big_yes;\n#else\nint big_no;\n#endif\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert_has(&r.stdout, "big_yes", "#P6");
    assert_lacks(&r.stdout, "big_no", "#P6");
}

/// The predefined limits must survive `#if`, which is the practical reason
/// this matters: `SIZE_MAX`-style feature tests used to misfire.
#[test]
fn preprocessor_if_uint64_max_is_not_zero() {
    let r = preprocess_text(
        "if_umax",
        "#if __UINT64_MAX__ == 18446744073709551615U\nint umax_ok;\n#else\nint umax_broken;\n#endif\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert_has(&r.stdout, "umax_ok", "#P6");
}

/// Signed arithmetic must stay signed when no unsigned operand is involved.
#[test]
fn preprocessor_if_signed_comparison_is_signed() {
    let r = preprocess_text(
        "if_signed",
        "#if -1 > 0\nint wrong;\n#else\nint right;\n#endif\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert_has(&r.stdout, "right", "#P6 signed compare");
    assert_lacks(&r.stdout, "wrong", "#P6 signed compare");
}

/// ...but the usual arithmetic conversions still apply: with an unsigned
/// operand, `-1` becomes UINTMAX_MAX and the comparison flips.
#[test]
fn preprocessor_if_unsigned_operand_promotes_the_comparison() {
    let r = preprocess_text(
        "if_promote",
        "#if -1 > 0u\nint promoted;\n#else\nint not_promoted;\n#endif\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert_has(&r.stdout, "promoted", "#P6 usual arithmetic conversions");
    assert_lacks(&r.stdout, "not_promoted", "#P6");
}

// ============================================================================
// #P14 — `&&` and `||` short-circuit
// ============================================================================

/// The right operand of a false `&&` is not evaluated, so a division by zero
/// there must not be diagnosed.
#[test]
fn preprocessor_logical_and_short_circuits() {
    let r = preprocess_text(
        "sc_and",
        "#if 0 && (1/0)\nint bad;\n#else\nint ok;\n#endif\n",
        &[],
    );
    assert!(r.success, "short-circuited operand must not fail the run");
    assert_has(&r.stdout, "int ok", "#P14");
    assert_lacks(&r.stderr, "division by zero", "#P14");
}

/// Same for a true `||`.
#[test]
fn preprocessor_logical_or_short_circuits() {
    let r = preprocess_text("sc_or", "#if 1 || (1/0)\nint ok;\n#endif\n", &[]);
    assert!(r.success, "short-circuited operand must not fail the run");
    assert_has(&r.stdout, "int ok", "#P14");
    assert_lacks(&r.stderr, "division by zero", "#P14");
}

/// A division by zero that *is* reached must still be diagnosed — the
/// short-circuit must not have silenced the check outright.
#[test]
fn preprocessor_division_by_zero_is_diagnosed_when_reached() {
    let r = preprocess_text("div_zero", "#if 1/0\nint x;\n#endif\n", &[]);
    assert!(
        r.stderr.contains("division by zero"),
        "expected a division-by-zero diagnostic, got:\n{}",
        r.stderr
    );
}

// ============================================================================
// #P13 — `#line` argument errors
// ============================================================================

#[test]
fn preprocessor_line_directive_rejects_bad_arguments() {
    for (name, src, why) in [
        (
            "line_nonnumeric",
            "#line abc\nint x;\n",
            "non-numeric operand",
        ),
        (
            "line_out_of_range",
            "#line 99999999999\nint x;\n",
            "operand above 2147483647",
        ),
        (
            "line_bad_filename",
            "#line 42 notastring\nint x;\n",
            "filename that is not a string literal",
        ),
        ("line_missing", "#line\nint x;\n", "missing operand"),
    ] {
        let r = preprocess_text(name, src, &[]);
        assert!(
            r.stderr.contains("error"),
            "#line with a {} must be diagnosed; stderr was:\n{}",
            why,
            r.stderr
        );
    }
}

/// The valid forms must stay silent.
#[test]
fn preprocessor_line_directive_accepts_valid_forms() {
    for (name, src) in [
        ("line_ok_num", "#line 42\nint x;\n"),
        ("line_ok_file", "#line 42 \"ok.c\"\nint x;\n"),
    ] {
        let r = preprocess_text(name, src, &[]);
        assert!(
            r.success && !r.stderr.contains("error"),
            "valid #line was rejected; stderr was:\n{}",
            r.stderr
        );
    }
}

// ============================================================================
// #P9 — bundled headers must not shadow the user's own
// ============================================================================

/// A header next to the source, included with the `"..."` form, wins over the
/// bundled one. c17.md 87905-87910 requires the including file's directory to
/// be searched first; builtins used to pre-empt the whole search.
#[test]
fn preprocessor_local_header_wins_over_builtin() {
    let dir = tempfile::Builder::new()
        .prefix("c17_p9_quote_")
        .tempdir()
        .unwrap();
    std::fs::write(
        dir.path().join("stddef.h"),
        "#define MY_LOCAL_STDDEF 1\ntypedef unsigned long size_t;\n",
    )
    .unwrap();
    let src = dir.path().join("use.c");
    std::fs::write(
        &src,
        "#include \"stddef.h\"\n#ifndef MY_LOCAL_STDDEF\n#error bundled stddef.h shadowed the local one\n#endif\nint ok;\n",
    )
    .unwrap();

    let r = run_c17(&["-E", &src.to_string_lossy()]);
    assert!(
        r.success && !r.stderr.contains("error"),
        "local stddef.h was shadowed:\n{}",
        r.stderr
    );
    assert_has(&r.stdout, "int ok", "#P9");
}

/// A `-I` directory also wins, for the `<...>` form.
#[test]
fn preprocessor_dash_i_wins_over_builtin() {
    let dir = tempfile::Builder::new()
        .prefix("c17_p9_inc_")
        .tempdir()
        .unwrap();
    let inc = dir.path().join("inc");
    std::fs::create_dir_all(&inc).unwrap();
    std::fs::write(inc.join("limits.h"), "#define MY_LIMITS 1\n").unwrap();
    let src = dir.path().join("use.c");
    std::fs::write(
        &src,
        "#include <limits.h>\n#ifndef MY_LIMITS\n#error bundled limits.h shadowed -I\n#endif\nint ok;\n",
    )
    .unwrap();

    let r = run_c17(&[
        "-E",
        &format!("-I{}", inc.to_string_lossy()),
        &src.to_string_lossy(),
    ]);
    assert!(
        r.success && !r.stderr.contains("error"),
        "-I limits.h was shadowed:\n{}",
        r.stderr
    );
    assert_has(&r.stdout, "int ok", "#P9");
}

/// With nothing shadowing them, the bundled headers must still be found —
/// they are the only source of these on a bare system.
#[test]
fn preprocessor_builtin_headers_still_resolve() {
    let r = preprocess_text(
        "builtin_still_works",
        "#include <stdarg.h>\n#include <stdbool.h>\n#include <limits.h>\n#include <stddef.h>\nint n = INT_MAX;\nbool b = true;\n",
        &[],
    );
    assert!(
        r.success && !r.stderr.contains("error"),
        "bundled headers stopped resolving:\n{}",
        r.stderr
    );
    assert_has(&r.stdout, "int n =", "#P9 regression");
}

// ============================================================================
// #P3 — incompatible macro redefinition
// ============================================================================

/// C17 6.10.3p2 requires a diagnostic when a macro is redefined by anything
/// but an identical definition. A warning satisfies it; making this fatal
/// would reject a great deal of code that redefines a macro benignly.
#[test]
fn preprocessor_incompatible_redefinition_is_diagnosed() {
    for (name, src, why) in [
        (
            "redef_value",
            "#define A 1\n#define A 2\nint x = A;\n",
            "different replacement list",
        ),
        (
            "redef_kind",
            "#define C 1\n#define C(x) x\nint z;\n",
            "object-like then function-like",
        ),
        (
            "redef_params",
            "#define D(a) a\n#define D(b) b\nint w;\n",
            "differently spelled parameters",
        ),
        (
            "redef_arity",
            "#define E(a) a\n#define E(a,b) a\nint v;\n",
            "different parameter counts",
        ),
    ] {
        let r = preprocess_text(name, src, &[]);
        assert!(
            r.stderr.contains("redefined"),
            "a redefinition with a {} must be diagnosed; stderr was:\n{}",
            why,
            r.stderr
        );
    }
}

/// An identical redefinition is explicitly legal and must stay silent — this
/// is the common idiom where two headers define the same macro the same way.
#[test]
fn preprocessor_identical_redefinition_is_silent() {
    for (name, src) in [
        ("redef_same", "#define A 1\n#define A 1\nint x = A;\n"),
        (
            "redef_same_fn",
            "#define B(x) (x)\n#define B(x) (x)\nint y = B(2);\n",
        ),
        // All white-space separations count as identical (6.10.3p1).
        (
            "redef_spacing",
            "#define C(x) (x + 1)\n#define C(x) (x   +   1)\nint z = C(2);\n",
        ),
    ] {
        let r = preprocess_text(name, src, &[]);
        assert!(
            !r.stderr.contains("redefined"),
            "an identical redefinition must not be diagnosed; stderr was:\n{}",
            r.stderr
        );
    }
}

/// A `#define` that redefines a macro the *implementation* predefined is not
/// the `#define`-versus-`#define` conflict the constraint governs.
#[test]
fn preprocessor_redefining_a_predefine_is_silent() {
    let r = preprocess_text(
        "redef_predefined",
        "#define __GNUC_MINOR__ 99\nint x = __GNUC_MINOR__;\n",
        &[],
    );
    assert!(
        !r.stderr.contains("redefined"),
        "redefining an implementation predefine must not warn; stderr was:\n{}",
        r.stderr
    );
    assert_has(&r.stdout, "99", "the redefinition still takes effect");
}

/// The reason that exemption matters: glibc's `features.h` redefines several
/// macros we predefine (`__GLIBC_MINOR__` among them, where we hardcode 17 and
/// the host says 39). Without it, every compilation against glibc would warn.
#[test]
fn preprocessor_including_a_system_header_is_warning_free() {
    let r = preprocess_text("sys_header_quiet", "#include <stdio.h>\nint x;\n", &[]);
    assert!(
        !r.stderr.contains("redefined"),
        "a plain system include must not produce redefinition warnings; stderr was:\n{}",
        r.stderr
    );
}

// ============================================================================
// #P4 — macro argument count
// ============================================================================

#[test]
fn preprocessor_macro_arity_mismatch_is_diagnosed() {
    for (name, src, why) in [
        (
            "arity_few",
            "#define F(a,b) (a+b)\nint x = F(1);\n",
            "too few",
        ),
        (
            "arity_many",
            "#define F(a,b) (a+b)\nint x = F(1,2,3);\n",
            "too many",
        ),
        (
            "arity_variadic_short",
            "#define V(a,b,...) (a)\nint x = V(1);\n",
            "fewer than the named parameters of a variadic macro",
        ),
    ] {
        let r = preprocess_text(name, src, &[]);
        assert!(
            r.stderr.contains("error"),
            "{} arguments must be diagnosed; stderr was:\n{}",
            why,
            r.stderr
        );
    }
}

/// The legal shapes must stay quiet — especially `F()` for a one-parameter
/// macro, which supplies one *empty* argument, not zero.
#[test]
fn preprocessor_legal_macro_arities_are_accepted() {
    for (name, src) in [
        ("arity_empty_arg", "#define G(x) (x)\nint a = G();\n"),
        ("arity_zero_params", "#define H() 7\nint a = H();\n"),
        ("arity_exact", "#define I(a,b) (a+b)\nint a = I(1,2);\n"),
        (
            "arity_variadic_empty",
            "#define V(a,...) (a)\nint a = V(1);\n",
        ),
        (
            "arity_variadic_full",
            "#define V(a,...) (a)\nint a = V(1,2,3);\n",
        ),
    ] {
        let r = preprocess_text(name, src, &[]);
        assert!(
            !r.stderr.contains("error"),
            "a legal invocation was rejected; stderr was:\n{}",
            r.stderr
        );
    }
}

// ============================================================================
// #P5 — `#` and `##` placement
// ============================================================================

#[test]
fn preprocessor_paste_at_either_end_is_diagnosed() {
    for (name, src) in [
        ("paste_lead", "#define BAD(a) ## a\nint x;\n"),
        ("paste_trail", "#define BAD(a) a ##\nint x;\n"),
    ] {
        let r = preprocess_text(name, src, &[]);
        assert!(
            r.stderr.contains("error"),
            "'##' at the edge of a replacement list must be diagnosed (6.10.3.3p1); stderr:\n{}",
            r.stderr
        );
    }
}

#[test]
fn preprocessor_stringify_without_a_parameter_is_diagnosed() {
    let r = preprocess_text("hash_nonparam", "#define S(x) # y\nint a;\n", &[]);
    assert!(
        r.stderr.contains("error"),
        "'#' not followed by a parameter must be diagnosed (6.10.3.2p1); stderr:\n{}",
        r.stderr
    );
}

/// The legal uses, plus the case that is legal only because the macro is
/// object-like — there `#` is an ordinary token.
#[test]
fn preprocessor_legal_hash_forms_are_accepted() {
    for (name, src) in [
        ("paste_ok", "#define OK(a,b) a ## b\nint OK(x,y);\n"),
        ("hash_ok", "#define S(x) #x\nconst char *s = S(ok);\n"),
        ("hash_object_like", "#define OBJ # notaparam\nint a;\n"),
    ] {
        let r = preprocess_text(name, src, &[]);
        assert!(
            !r.stderr.contains("error"),
            "a legal use of #/## was rejected; stderr was:\n{}",
            r.stderr
        );
    }
}

// ============================================================================
// #P15 — `-D` may define a function-like macro
// ============================================================================

/// `-D'FOO(x)=x+1'` used to make `"FOO(x)"` the macro *name*, which no source
/// file can ever reference — a silent no-op.
#[test]
fn preprocessor_dash_d_defines_function_like_macros() {
    let r = preprocess_text("dashd_fn", "int x = FOO(1);\n", &["-DFOO(x)=x+1"]);
    assert!(r.success, "{}", r.stderr);
    assert_lacks(&r.stdout, "FOO(", "#P15: the macro must have expanded");
    assert_has(&r.stdout, "1", "#P15");

    let r = preprocess_text(
        "dashd_fn2",
        "int y = BAR(2,3);\n",
        &["-DBAR(a,b)=((a)*(b))"],
    );
    assert!(r.success, "{}", r.stderr);
    assert_lacks(&r.stdout, "BAR(", "#P15");
    assert_has(&r.stdout, "2", "#P15");
    assert_has(&r.stdout, "3", "#P15");
}

/// The object-like `-D` forms must keep working.
#[test]
fn preprocessor_dash_d_object_forms_still_work() {
    let r = preprocess_text("dashd_obj", "int a = P;\nint b = Q;\n", &["-DP=42", "-DQ"]);
    assert!(r.success, "{}", r.stderr);
    assert_has(&r.stdout, "42", "#P15 -DNAME=VALUE");
    assert_has(&r.stdout, "int b = 1", "#P15 bare -DNAME defines it as 1");
}

// ============================================================================
// #P16 — GNU named variadic macros
// ============================================================================

/// `#define LOG(fmt, args...)` binds *all* trailing arguments to `args`. It
/// used to be parsed as an ordinary positional parameter, so it captured only
/// the first — and pushed the `__VA_ARGS__` start index one too far.
#[test]
fn preprocessor_named_variadic_binds_all_trailing_arguments() {
    let src = "#include <stdio.h>\n\
               #define LOG(fmt, args...) printf(fmt, args)\n\
               int main(void){ LOG(\"%d %d %d\\n\", 1, 2, 3); return 0; }\n";
    assert_eq!(
        crate::common::compile_and_run("named_variadic", src, &[]),
        0,
        "a named variadic macro must forward every trailing argument"
    );
}

/// The C99 `__VA_ARGS__` spelling must be unaffected.
#[test]
fn preprocessor_va_args_form_still_works() {
    let src = "#include <stdio.h>\n\
               #define LOG2(fmt, ...) printf(fmt, __VA_ARGS__)\n\
               int main(void){ LOG2(\"%d %d %d\\n\", 4, 5, 6); return 0; }\n";
    assert_eq!(crate::common::compile_and_run("va_args_form", src, &[]), 0);
}

/// Sanity check for the two shadowing tests above: a header that really is
/// absent must still be diagnosed, so a silently-skipped `#include` cannot
/// make them pass vacuously.
#[test]
fn preprocessor_missing_include_is_still_an_error() {
    let r = preprocess_text(
        "missing_include",
        "#include \"definitely_not_a_real_header_xyzzy.h\"\nint x;\n",
        &[],
    );
    assert!(
        !r.success || r.stderr.contains("error"),
        "a missing include must be diagnosed"
    );
}

// ============================================================================
// #U5 — `-E` line markers
// ============================================================================

/// STDOUT (88032-88038): the `-E` output shall contain at least one
/// `# <line> "<file>"` line for each file processed via `#include`.
/// RATIONALE (88370-88374) names makefile dependency generation as the point.
/// None were emitted at all — the markers were discarded outright.
#[test]
fn preprocessor_emits_line_markers_for_includes() {
    let dir = tempfile::Builder::new()
        .prefix("c17_markers_")
        .tempdir()
        .unwrap();
    let inc = dir.path().join("inc");
    std::fs::create_dir_all(&inc).unwrap();
    std::fs::write(inc.join("h1.h"), "int from_header(void);\n").unwrap();
    let src = dir.path().join("m.c");
    std::fs::write(&src, "#include \"h1.h\"\nint from_main(void);\n").unwrap();

    let r = run_c17(&[
        "-E",
        &format!("-I{}", inc.to_string_lossy()),
        &src.to_string_lossy(),
    ]);
    assert!(r.success, "{}", r.stderr);

    let markers: Vec<&str> = r
        .stdout
        .lines()
        .filter(|l| l.starts_with("# ") && l.contains('"'))
        .collect();
    assert!(
        markers.len() >= 2,
        "expected a marker for the primary source and for the include, got {:?}\n{}",
        markers,
        r.stdout
    );
    assert!(
        markers[0].contains("m.c"),
        "the first marker must name the primary source: {:?}",
        markers
    );
    assert!(
        markers.iter().any(|m| m.contains("h1.h")),
        "no marker for the included file: {:?}",
        markers
    );
    // Entering a file is flagged 1, returning to one is flagged 2 (GCC's
    // convention), so a consumer can follow the nesting.
    assert!(
        markers.iter().any(|m| m.ends_with(" 1")),
        "no 'entering' marker: {:?}",
        markers
    );
    assert!(
        markers.iter().any(|m| m.ends_with(" 2")),
        "no 'returning' marker: {:?}",
        markers
    );
}

/// A source with no `#include` still gets one marker naming it.
#[test]
fn preprocessor_emits_a_marker_without_includes() {
    let r = preprocess_text("marker_solo", "int only_main;\n", &[]);
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stdout.lines().any(|l| l.starts_with("# 1 \"")),
        "expected a leading marker:\n{}",
        r.stdout
    );
}

/// The markers must not break the output as C: `-E` output is a valid `.i`
/// operand, and the spec says a `.i` is not re-preprocessed.
#[test]
fn preprocessor_output_with_markers_still_compiles() {
    let dir = tempfile::Builder::new()
        .prefix("c17_marker_recompile_")
        .tempdir()
        .unwrap();
    let src = dir.path().join("r.c");
    std::fs::write(
        &src,
        "#include <stdio.h>\nint main(void){ printf(\"ok\\n\"); return 0; }\n",
    )
    .unwrap();
    let i = dir.path().join("r.i");
    let exe = dir.path().join("r.out");

    let r = run_c17(&["-E", &src.to_string_lossy()]);
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stdout.contains("# "),
        "no markers in the preprocessed output"
    );
    std::fs::write(&i, &r.stdout).unwrap();

    let r = run_c17(&[&i.to_string_lossy(), "-o", &exe.to_string_lossy()]);
    assert!(
        r.success,
        "preprocessed output with markers failed to compile: {}",
        r.stderr
    );
    let out = std::process::Command::new(&exe).output().unwrap();
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "ok");
}

/// A `# N "file" flags` linemarker attributes what follows to the original
/// file, not to the position in the preprocessed text.
///
/// POSIX 87981 makes `c17 -E` output a `.i` operand, and every such file is
/// built out of these markers. Nothing consumed one: `handle_directive` took a
/// directive name only from an identifier, so a marker's leading number fell
/// through to the "not a directive" arm and the whole line was swallowed. Every
/// diagnostic about a preprocessed file therefore cited the `.i`. GCC reports
/// `orig.c:42`, and so does c17 now.
#[test]
fn preprocessor_linemarker_sets_the_reported_position() {
    let dir = tempfile::Builder::new()
        .prefix("c17_linemarker_")
        .tempdir()
        .unwrap();
    let i = dir.path().join("m.i");
    std::fs::write(&i, "# 42 \"orig.c\"\nint main(void){ return bad; }\n").unwrap();

    let r = run_c17(&[
        "-c",
        &i.to_string_lossy(),
        "-o",
        &dir.path().join("m.o").to_string_lossy(),
    ]);
    assert!(!r.success, "undeclared identifier should fail");
    assert!(
        r.stderr.contains("orig.c:42:"),
        "linemarker ignored, diagnostic not attributed to orig.c:42:\n{}",
        r.stderr
    );
    // The operand-level summary still names `m.i`, which is the file the user
    // named; what must not survive is the *physical* position inside it.
    assert!(
        !r.stderr.contains("m.i:2:"),
        "diagnostic still cites the position in the preprocessed text:\n{}",
        r.stderr
    );
}

/// Successive markers re-attribute, including the return to an earlier file.
///
/// The mapping has to bind to the stream the marker was read from, so that
/// text spliced in from an `#include` keeps its own attribution, and a file
/// named twice has to resolve to one stream rather than accumulating a new one
/// per marker.
#[test]
fn preprocessor_linemarkers_track_entering_and_returning() {
    let dir = tempfile::Builder::new()
        .prefix("c17_linemarker_seq_")
        .tempdir()
        .unwrap();
    let i = dir.path().join("multi.i");
    std::fs::write(
        &i,
        "# 10 \"a.c\"\nint p(void){return aa;}\n\
         # 77 \"b.h\" 1\nint q(void){return bb;}\n\
         # 12 \"a.c\" 2\nint r(void){return cc;}\n",
    )
    .unwrap();

    let r = run_c17(&[
        "-c",
        &i.to_string_lossy(),
        "-o",
        &dir.path().join("multi.o").to_string_lossy(),
    ]);
    assert!(!r.success);
    for want in ["a.c:10:", "b.h:77:", "a.c:12:"] {
        assert!(
            r.stderr.contains(want),
            "expected a diagnostic at {want}\n{}",
            r.stderr
        );
    }
}

/// Flag 3 marks a system header, and its warnings are not the user's to act on.
///
/// A preprocessed file carries the whole of glibc inline. Without this,
/// compiling one buries the user's own diagnostics under the system headers'.
#[test]
fn preprocessor_linemarker_flag_three_silences_the_stream() {
    let dir = tempfile::Builder::new()
        .prefix("c17_linemarker_sys_")
        .tempdir()
        .unwrap();
    let body = "int g(void){ return 1<<64; }\n";

    let sys = dir.path().join("s3.i");
    std::fs::write(&sys, format!("# 5 \"sys.h\" 3\n{body}")).unwrap();
    let r = run_c17(&[
        "-c",
        &sys.to_string_lossy(),
        "-o",
        &dir.path().join("s3.o").to_string_lossy(),
    ]);
    assert!(
        !r.stderr.contains("warning"),
        "a system-header stream still warned:\n{}",
        r.stderr
    );

    // Control: the identical body without flag 3 must still warn, or the
    // assertion above would pass for the wrong reason.
    let usr = dir.path().join("s1.i");
    std::fs::write(&usr, format!("# 5 \"usr.h\" 1\n{body}")).unwrap();
    let r = run_c17(&[
        "-c",
        &usr.to_string_lossy(),
        "-o",
        &dir.path().join("s1.o").to_string_lossy(),
    ]);
    assert!(
        r.stderr.contains("warning") && r.stderr.contains("usr.h:5:"),
        "expected the shift warning at usr.h:5:\n{}",
        r.stderr
    );
}

/// C17 6.10.3.2p2: `#` produces a literal whose spelling is the argument's
/// original spelling — white space between tokens collapses to one space, and
/// white space that was not there does not appear. `stringify_arg` inserted a
/// separator between every pair of tokens, which the direct form hid (its
/// tokens still carried their source spacing) but the two-level
/// `XSTR(x)`/`STR(x)` idiom exposed, since the inner argument arrives already
/// expanded.
#[test]
fn c17_nested_stringification_preserves_spelling() {
    let r = preprocess_text(
        "c17_nested_stringify",
        "#define STR(x) #x\n\
         #define XSTR(x) STR(x)\n\
         #define VER 1\n\
         direct STR(1+2)\n\
         nested XSTR(1+2)\n\
         index  XSTR(a[0]+b[1])\n\
         call   XSTR(f(1,2))\n\
         expand XSTR(VER)\n\
         spaced XSTR( 1  +  2 )\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert_has(&r.stdout, "direct \"1+2\"", "direct stringification");
    assert_has(&r.stdout, "nested \"1+2\"", "nested stringification");
    assert_has(&r.stdout, "index \"a[0]+b[1]\"", "nested with subscripts");
    assert_has(&r.stdout, "call \"f(1,2)\"", "nested with a call");
    assert_has(&r.stdout, "expand \"1\"", "argument macro-expanded first");
    // Interior white space collapses to exactly one space; leading and
    // trailing are deleted.
    assert_has(&r.stdout, "spaced \"1 + 2\"", "interior space collapses");
}

/// A diagnostic inside a header names the `#include` chain that reached it.
///
/// The machinery was all there -- `Stream::include_pos`, `show_include_chain`,
/// the "(through ...)" note -- but the only three functions that ever set
/// `include_pos` were `#[cfg(test)]`, and production registered every file
/// with `init_stream`, which leaves it `None`. So the chain was live in test
/// builds only, and a full CPython build produced zero "through" lines across
/// 552 diagnostics. Recorded at #C54.
#[test]
fn preprocessor_diagnostic_in_a_header_names_the_include_chain() {
    let dir = tempfile::Builder::new()
        .prefix("c17_chain_")
        .tempdir()
        .unwrap();
    let inc = dir.path().join("inc");
    std::fs::create_dir_all(&inc).unwrap();
    // Two levels deep, so the order of the chain is observable.
    std::fs::write(
        inc.join("inner.h"),
        "int bad(void) { return undeclared_thing; }\n",
    )
    .unwrap();
    std::fs::write(inc.join("outer.h"), "#include \"inner.h\"\n").unwrap();
    let src = dir.path().join("m.c");
    std::fs::write(&src, "#include \"outer.h\"\nint main(void) { return 0; }\n").unwrap();

    let r = run_c17(&[
        "-S",
        "-o",
        "/dev/null",
        &format!("-I{}", inc.to_string_lossy()),
        &src.to_string_lossy(),
    ]);
    assert!(!r.success, "the program should not compile:\n{}", r.stderr);
    assert!(
        r.stderr.contains("inner.h"),
        "the diagnostic must name the file it is in:\n{}",
        r.stderr
    );
    let chain = r
        .stderr
        .lines()
        .find(|l| l.contains("through"))
        .unwrap_or_else(|| panic!("no include chain in:\n{}", r.stderr));
    // The line opens by naming the primary source, so read only the list.
    let list = chain
        .split_once("through ")
        .map(|(_, rest)| rest)
        .unwrap_or_else(|| panic!("no chain list in: {}", chain));
    // Innermost first: the file that included inner.h, then the one that
    // included that.
    let outer_at = list
        .find("outer.h")
        .expect("outer.h missing from the chain");
    let main_at = list.find("m.c").expect("m.c missing from the chain");
    assert!(
        outer_at < main_at,
        "the chain must read outwards, got: {}",
        chain
    );
}

/// A diagnostic in the primary source has no chain to name.
#[test]
fn preprocessor_diagnostic_outside_a_header_has_no_include_chain() {
    let dir = tempfile::Builder::new()
        .prefix("c17_nochain_")
        .tempdir()
        .unwrap();
    let src = dir.path().join("m.c");
    std::fs::write(&src, "int bad(void) { return undeclared_thing; }\n").unwrap();

    let r = run_c17(&["-S", "-o", "/dev/null", &src.to_string_lossy()]);
    assert!(!r.success, "the program should not compile:\n{}", r.stderr);
    assert!(
        !r.stderr.contains("through"),
        "nothing included m.c, so there is no chain:\n{}",
        r.stderr
    );
}

/// Every one of these used to evaluate to zero in silence and pick a branch.
/// A typo in a feature test compiled the wrong half of the file with nothing
/// on stderr to say so, which is the worst failure a preprocessor has.
///
/// The message text is asserted, not just the rejection: a check that only
/// looks at the exit status passes when the wrong diagnostic fires.
#[test]
fn preprocessor_malformed_if_is_diagnosed() {
    for (name, cond, needle) in [
        ("empty", "", "#if with no expression"),
        ("unclosed_paren", "(1", "missing ')' in expression"),
        ("garbage_tail", "1 2 3", "missing binary operator"),
        (
            "assignment",
            "X = 2",
            "not valid in preprocessor expressions",
        ),
        ("float", "1.5", "floating constant"),
        ("hex_float", "0x1p3", "floating constant"),
        ("string", "\"abc\"", "not valid in preprocessor expressions"),
        ("bad_suffix", "1zz", "invalid suffix"),
        ("bad_octal", "07778", "invalid suffix"),
        ("defined_nonident", "defined(1)", "requires an identifier"),
        (
            "defined_unclosed",
            "defined(X",
            "missing ')' after \"defined\"",
        ),
    ] {
        let src = format!(
            "#if {}\nint taken = 1;\n#endif\nint main(void){{return 0;}}\n",
            cond
        );
        let r = preprocess_text(&format!("bad_if_{}", name), &src, &[]);
        assert!(
            !r.success,
            "#if {:?} should be rejected, but -E succeeded:\n{}",
            cond, r.stdout
        );
        assert!(
            r.stderr.contains(needle),
            "#if {:?} should say {:?}, got:\n{}",
            cond,
            needle,
            r.stderr
        );
    }
}

/// A shift count outside [0, 64) is undefined. c17 clamps it, which is a fine
/// answer, but clamping in silence made `#if (1 << 64) == 0` false with
/// nothing to explain it. gcc warns rather than erroring, so the expression
/// still evaluates and the file still compiles.
#[test]
fn preprocessor_out_of_range_shift_warns() {
    let src = "#if (1 << 64) == 0\nint taken = 1;\n#endif\nint main(void){return 0;}\n";
    let r = preprocess_text("shift_overflow", src, &[]);
    assert!(
        r.success,
        "a shift overflow is a warning, not an error:\n{}",
        r.stderr
    );
    assert!(
        r.stderr
            .contains("integer overflow in preprocessor expression"),
        "expected an overflow warning, got:\n{}",
        r.stderr
    );
}

/// A short-circuited operand is not evaluated, so nothing in it may be
/// diagnosed — the same rule that already keeps `#if 0 && 1/0` quiet.
#[test]
fn preprocessor_short_circuit_suppresses_if_diagnostics() {
    let src = "#if 0 && (1\nint taken = 1;\n#endif\nint main(void){return 0;}\n";
    let r = preprocess_text("short_circuit_quiet", src, &[]);
    assert!(
        r.success,
        "the skipped operand must not be diagnosed:\n{}",
        r.stderr
    );
    assert!(
        !r.stderr.contains("missing ')'"),
        "the skipped operand must not be diagnosed:\n{}",
        r.stderr
    );
}

/// `#if 'c'` and the compiled `'c'` must agree. They did not: the evaluator
/// packed the source spelling, so `'\n'` was 23662 and `'\0'` was true.
#[test]
fn preprocessor_if_character_constants_match_the_compiler() {
    let src = r#"
#if '\n' != 10
#error newline
#endif
#if '\0' != 0
#error nul
#endif
#if '\x41' != 65
#error hex
#endif
#if '\101' != 65
#error octal
#endif
#if L'\n' != 10
#error wide
#endif
int main(void) {
    // The same expressions, compiled. A disagreement here is the bug.
    if ('\n' != 10) return 1;
    if ('\0' != 0) return 2;
    if ('\x41' != 65) return 3;
    if ('\101' != 65) return 4;
    if (L'\n' != 10) return 5;
    return 0;
}
"#;
    assert_eq!(
        crate::common::compile_and_run("if_char_agrees", src, &[]),
        0
    );
}

/// C17 6.4.4.4p10: an ordinary character constant carries plain `char`'s
/// signedness, which differs by target. `compile_and_run` only ever exercises
/// the host, so the other answer is only visible through `--target`.
#[test]
fn preprocessor_if_char_signedness_is_per_target() {
    let src = "#if '\\xff' < 0\nSIGNED_CHAR\n#else\nUNSIGNED_CHAR\n#endif\n";
    for (target, want) in [
        ("x86_64-unknown-linux-gnu", "SIGNED_CHAR"),
        ("aarch64-unknown-linux-gnu", "UNSIGNED_CHAR"),
    ] {
        let r = preprocess_text("char_sign", src, &["--target", target]);
        assert!(r.success, "-E failed for {}: {}", target, r.stderr);
        assert_has(&r.stdout, want, target);
    }
}

/// An expansion stands where the invocation stood, so one at the start of a
/// line still starts a line. c17 dropped that flag and ran the expansion onto
/// the previous line; stringification hid the bug for its own case by copying
/// the invocation position wholesale, which then made a `#x` result claim to
/// begin a line even in the middle of one.
#[test]
fn preprocessor_expansion_keeps_the_invocation_line_break() {
    let src = "#define M 42\n#define F(x) (x)\n#define S(x) #x\n\
               int a;\nM int b;\nint c;\nF(9) int d;\nint e;\nS(hi) int f;\n";
    let r = preprocess_text("expansion_linebreak", src, &[]);
    assert!(r.success, "-E failed: {}", r.stderr);

    // Each expansion begins its own line, exactly as gcc lays it out.
    for (before, after) in [
        ("int a;", "42 int b;"),
        ("int c;", "(9) int d;"),
        ("int e;", "\"hi\" int f;"),
    ] {
        assert_lacks(
            &r.stdout,
            &format!("{} {}", before, after),
            "expansion ran onto the previous line",
        );
        assert_has(&r.stdout, after, "the expansion itself");
    }
}

/// A macro argument may span lines, and its tokens go into the expansion
/// carrying the file's flags. One that still says it begins a line would be
/// read back as starting a directive once expansions are rescanned in place.
#[test]
fn preprocessor_multiline_argument_does_not_begin_a_line() {
    let src = "#define C(a,b) a##b\nint xy_z = 1;\nint v = C(x,\ny_z);\n";
    let r = preprocess_text("multiline_arg", src, &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "int v = xy_z;", "pasted across a line break");
}

/// Recursion is stopped by the hide set carried on the expansion's tokens, so
/// each of these has to terminate on that alone. They are here rather than in
/// the unit tests because a failure is a hang, and the integration harness
/// runs `c17` as a subprocess where a hang is visible as one.
#[test]
fn preprocessor_recursive_macros_terminate() {
    for (name, src, want) in [
        // The classic: the macro's own name in its own body.
        ("self", "#define f(x) f(x)\nf(1)\n", "f(1)"),
        // Indirect, through a second name.
        ("mutual_object", "#define a b\n#define b a\na\n", "a"),
        // Indirect, function-like, so the hide set has to survive argument
        // collection as well as substitution.
        (
            "mutual_function",
            "#define F(x) G(x)\n#define G(x) F(x)\nF(1)\n",
            "F(1)",
        ),
        // The argument is the macro, and the body calls whatever it is given.
        // If argument-derived tokens were exempt from hiding, each round would
        // produce another unhidden `f`.
        ("arg_is_the_macro", "#define f(x) x(x)\nf(f)\n", "f(f)"),
        // A macro name built by `##`. The pasted token is new and so is
        // eligible for expansion, but it must still inherit the hiding of the
        // expansion it was built in, or this never settles.
        (
            "pasted_name",
            "#define CAT(a,b) a##b\n#define X CAT(A,B)\n#define AB X\nX\n",
            "X",
        ),
        // expat's portability shim, and the case that proves a token has to
        // carry the *whole* hide set rather than only names matching its own
        // spelling: the chain runs `__inline` -> `inline` -> `__inline`, and
        // neither name matches the other.
        (
            "chain_through_another_name",
            "#define __inline inline\n#define inline __inline\n__inline int f(void);\n",
            "inline int f(void);",
        ),
        // glibc's enum-and-macro idiom, where the body is the name itself.
        (
            "body_is_the_name",
            "#define MSG_DONTROUTE MSG_DONTROUTE\nint x = MSG_DONTROUTE;\n",
            "int x = MSG_DONTROUTE;",
        ),
    ] {
        let r = preprocess_text(&format!("recursive_{}", name), src, &[]);
        assert!(r.success, "{}: -E failed: {}", name, r.stderr);
        assert_has(&r.stdout, want, name);
    }
}

/// C17 6.10.3.4 EXAMPLE 3 — the standard's own definitive test of rescanning.
///
/// `h` expands to `g(~`, an expansion that ends in the middle of a call. The
/// rest of that call is in the file, not in the replacement list, so a rescan
/// that only ever saw the replacement list could not finish it: c17 used to
/// reject this with "unterminated argument list invoking macro f".
#[test]
fn preprocessor_c17_example_3() {
    let src = "#define f(a) f(2 * (a))\n#define g f\n#define h g(~\n\
               #define m(a) a(w)\n#define w 0,1\nint x = h 5) ;\n";
    let r = preprocess_text("example3", src, &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "f(2 * (~ 5))", "EXAMPLE 3");
}

/// An expansion may consume tokens from the rest of the file, and one that
/// runs out of file has to give them back rather than expand with whatever it
/// managed to collect. Splicing made the scan run to end of file looking for
/// the `)`, so expanding anyway substituted the remainder of the translation
/// unit into the macro and emitted the result in its place.
#[test]
fn preprocessor_unterminated_call_keeps_the_rest_of_the_file() {
    let src = "#define f(x) ((x)+1)\n#define BAD f(\nint before = 1;\nBAD\nint after = 2;\n";
    let r = preprocess_text("unterminated_call", src, &[]);
    assert!(
        !r.success,
        "an unterminated call is an error:\n{}",
        r.stdout
    );
    assert!(
        r.stderr.contains("unterminated argument list"),
        "expected the unterminated-call diagnostic, got:\n{}",
        r.stderr
    );
    assert_has(&r.stdout, "int before = 1;", "text before the call");
    assert_has(&r.stdout, "int after = 2;", "text after the call");
    assert_lacks(&r.stdout, "((", "the macro must not have expanded");
}

/// A function-like macro name that only exists after an expansion still finds
/// its argument list in the file. This worked before only because of a
/// hand-rolled loop that reached back into the output and re-spliced; deleting
/// that loop must not lose the behaviour.
#[test]
fn preprocessor_pasted_macro_name_takes_arguments_from_the_file() {
    let src = "#define CONCAT(a,b) a##b\n#define CALL(name) CONCAT(name, _func)\n\
               #define ADD_func(a,b) ((a)+(b))\nint v = CALL(ADD)(10, 32);\n";
    let r = preprocess_text("pasted_call", src, &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    // Compared without spacing: c17 puts a space before a substituted
    // argument that had one at the call site, which gcc does not. That is a
    // separate, pre-existing divergence and not what this test is about.
    let squeezed: String = r.stdout.split_whitespace().collect();
    assert!(
        squeezed.contains("((10)+(32))"),
        "expected the arguments to be taken from the file, got:\n{}",
        r.stdout
    );
}

/// The loop that re-spliced from the output also popped tokens that predated
/// the expansion, so a function-like macro name left legitimately unexpanded
/// got pulled back in once a *following* macro expanded to nothing. gcc and
/// clang both leave this alone.
#[test]
fn preprocessor_empty_macro_does_not_trigger_the_previous_name() {
    let src = "#define f(x) ((x)+1)\n#define E\nf E (3)\n";
    let r = preprocess_text("empty_between", src, &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_has(&r.stdout, "f (3)", "the call must not be formed");
    assert_lacks(&r.stdout, "((3)+1)", "f was expanded across E");
}

/// C17 6.10.3.3p3 requires `##` to produce a single preprocessing token, and
/// a diagnostic when it does not. The result used to be whatever re-lexing the
/// concatenated spelling produced, with no check and no message.
///
/// `cat(/,/)` is the one that mattered: `//` is a comment, so both operands
/// *and* the tokens around them disappeared from the output.
#[test]
fn preprocessor_invalid_paste_is_diagnosed() {
    for (name, call) in [
        ("two_punctuators", "cat(+,-)"),
        ("ident_and_string", "cat(x,\"s\")"),
        ("comment", "cat(/,/)"),
    ] {
        let src = format!(
            "#define cat(a,b) a##b\nint y = 1 {} 2;\nint main(void){{return 0;}}\n",
            call
        );
        let r = preprocess_text(&format!("bad_paste_{}", name), &src, &[]);
        assert!(!r.success, "{} should be rejected:\n{}", call, r.stdout);
        assert!(
            r.stderr
                .contains("does not give a valid preprocessing token"),
            "{} should say so, got:\n{}",
            call,
            r.stderr
        );
    }
}

/// The pastes that are valid have to stay valid, including the ones that join
/// two punctuators into a third and the one that puts a `.` in front of a
/// digit sequence.
#[test]
fn preprocessor_valid_pastes_still_work() {
    let src = "#define cat(a,b) a##b\n\
               int ab; int x12; int y = 1 cat(<,<) 2; int *p; int q = p cat(-,>) ab;\n\
               double d = cat(1,.5);\nint z = cat(x,12);\n";
    let r = preprocess_text("good_pastes", src, &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    for want in ["1 << 2", "p -> ab", "1.5", "x12"] {
        assert_has(&r.stdout, want, "valid paste");
    }
}

/// `#define` accepted three malformed forms in silence. A duplicate parameter
/// was the worst: substitution matches a parameter by name and takes the first,
/// so `#define F(a,a) a` made `F(1,2)` expand to `1`.
#[test]
fn preprocessor_malformed_define_is_diagnosed() {
    for (name, src, needle) in [
        (
            "duplicate_param",
            "#define F(a,a) a\n",
            "duplicate macro parameter",
        ),
        (
            "defined",
            "#define defined 1\n",
            "cannot be used as a macro name",
        ),
        ("unclosed_params", "#define G(x,y\n", "expected ')'"),
        (
            "junk_in_params",
            "#define H(x + y) x\n",
            "expected ',' or ')'",
        ),
    ] {
        let full = format!("{}int main(void){{return 0;}}\n", src);
        let r = preprocess_text(&format!("bad_define_{}", name), &full, &[]);
        assert!(!r.success, "{:?} should be rejected:\n{}", src, r.stdout);
        assert!(
            r.stderr.contains(needle),
            "{:?} should say {:?}, got:\n{}",
            src,
            needle,
            r.stderr
        );
    }
}

/// Recovery from an unterminated call puts *file* tokens back in front of the
/// cursor. They have to be read back as file tokens: `remap_pos` and the
/// directive check both ask where a token came from, so tokens unread as if
/// they were macro output leave the rest of the file unpreprocessed — the
/// later `#define` was not processed and `LATER` came out unexpanded.
#[test]
fn preprocessor_recovery_keeps_processing_the_rest_of_the_file() {
    let src = "#define f(x) ((x)+1)\n#define BAD f(\nint before;\nBAD\n\
               #define LATER 7\nint after = LATER;\n";
    let r = preprocess_text("recovery_provenance", src, &[]);
    assert!(
        !r.success,
        "an unterminated call is an error:\n{}",
        r.stdout
    );
    assert_has(
        &r.stdout,
        "int after = 7;",
        "the later #define was processed",
    );
    assert_lacks(&r.stdout, "LATER", "the macro should have expanded");
    assert_lacks(
        &r.stdout,
        "#define",
        "the later directive is not passed through",
    );
}

/// C23 6.10.5: `__VA_OPT__(content)` is `content` when the variadic arguments
/// have at least one token, and nothing when they do not. gcc and clang accept
/// it in every mode; c17 used to pass it through literally.
///
/// Each row was probed against gcc before being written down.
#[test]
fn preprocessor_va_opt() {
    for (name, def, empty_call, full_call, want_empty, want_full) in [
        // The idiom it exists for: a separator that appears only when there is
        // something to separate.
        (
            "separator",
            "#define P(f,...) g(f __VA_OPT__(,) __VA_ARGS__)",
            "P(a)",
            "P(a,b)",
            "g(a)",
            "g(a, b)",
        ),
        // `##` reaches into the group: the markers are not tokens, so paste
        // adjacency has to look past them.
        (
            "paste_into",
            "#define Q(...) a ## __VA_OPT__(b)",
            "Q()",
            "Q(1)",
            "a",
            "ab",
        ),
        // The content may contain parentheses of its own.
        (
            "nested_parens",
            "#define N(...) __VA_OPT__(f(a,b))",
            "N()",
            "N(1)",
            "",
            "f(a,b)",
        ),
        // A group that produces nothing still separates its neighbours.
        (
            "adjacent",
            "#define R(...) __VA_OPT__(x)y",
            "R()",
            "R(1)",
            "y",
            "x y",
        ),
        // `#` against the group gives its spelling, or the empty string.
        (
            "stringify",
            "#define S(...) #__VA_OPT__(x)",
            "S()",
            "S(1)",
            "\"\"",
            "\"x\"",
        ),
        // `##` inside the group works like `##` anywhere.
        (
            "paste_inside",
            "#define D(...) __VA_OPT__(a ## b)",
            "D()",
            "D(1)",
            "",
            "ab",
        ),
    ] {
        let src = format!("{}\nA {}\nB {}\n", def, empty_call, full_call);
        let r = preprocess_text(&format!("va_opt_{}", name), &src, &["-P"]);
        assert!(r.success, "{}: -E failed: {}", name, r.stderr);

        let line = |prefix: &str| -> String {
            r.stdout
                .lines()
                .find(|l| l.trim_start().starts_with(prefix))
                .unwrap_or_default()
                .trim()
                .strip_prefix(prefix)
                .unwrap_or_default()
                .trim()
                .to_string()
        };
        assert_eq!(
            line("A"),
            want_empty,
            "{}: empty variadic\n{}",
            name,
            r.stdout
        );
        assert_eq!(
            line("B"),
            want_full,
            "{}: non-empty variadic\n{}",
            name,
            r.stdout
        );
    }
}

/// An empty *argument* is not the same as no arguments: C23 asks the question
/// of the argument tokens before expansion, so `F(a,)` is empty and `F(a,,)`
/// — which denotes a comma — is not.
#[test]
fn preprocessor_va_opt_emptiness_is_about_tokens() {
    let src = "#define P(f,...) g(f __VA_OPT__(!) __VA_ARGS__)\n\
               #define EMPTY\nA P(a)\nB P(a,)\nC P(a,,)\nD P(a,EMPTY)\n";
    let r = preprocess_text("va_opt_empty", src, &["-P"]);
    assert!(r.success, "-E failed: {}", r.stderr);
    let has = |prefix: &str| -> bool {
        r.stdout
            .lines()
            .find(|l| l.trim_start().starts_with(prefix))
            .is_some_and(|l| l.contains('!'))
    };
    assert!(!has("A"), "no arguments at all is empty:\n{}", r.stdout);
    assert!(!has("B"), "one empty argument is empty:\n{}", r.stdout);
    assert!(
        has("C"),
        "two empty arguments denote a comma:\n{}",
        r.stdout
    );
    assert!(
        has("D"),
        "an argument that expands to nothing is still a token:\n{}",
        r.stdout
    );
}

/// In a non-variadic macro `__VA_OPT__` means nothing. gcc warns and leaves it
/// alone rather than rejecting the definition.
#[test]
fn preprocessor_va_opt_in_a_non_variadic_macro_warns() {
    let r = preprocess_text(
        "va_opt_nonvariadic",
        "#define E(a) __VA_OPT__(x)\nA E(1)\n",
        &["-P"],
    );
    assert!(r.success, "it is a warning, not an error:\n{}", r.stderr);
    assert!(
        r.stderr.contains("__VA_OPT__"),
        "expected a diagnostic naming it, got:\n{}",
        r.stderr
    );
    assert!(
        r.stdout.contains("__VA_OPT__(x)"),
        "and it is left alone:\n{}",
        r.stdout
    );
}

/// An expansion that produces nothing must not take the *next* token's line
/// break with it.
///
/// The invocation's spacing is handed to whichever token comes next, so that
/// an expansion beginning with a macro that expands to nothing still lands
/// where the invocation stood. When the expansion is empty the token it lands
/// on is the next one from the *file*, which already knows where it stands —
/// so the flags are added to, never taken away. Overwriting merged the lines:
/// `#define E` with `A E` and `B c` came out as `A B c`.
#[test]
fn preprocessor_empty_expansion_keeps_the_next_line_break() {
    let r = preprocess_text("empty_expansion_break", "#define E\nA E\nB c\n", &["-P"]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_eq!(r.stdout, "A\nB c\n", "the lines must stay separate");
}

/// A call with fewer arguments than parameters is diagnosed and then expanded
/// anyway, so the rest of the file still makes sense. The per-parameter
/// expansion memo is indexed by parameter, so sizing it by the *argument*
/// count made this panic the compiler rather than diagnose anything.
#[test]
fn preprocessor_too_few_arguments_does_not_crash() {
    for (name, src, want) in [
        ("one_none", "#define ONE(x) [x]\nA ONE()\n", "[]"),
        ("two_one", "#define TWO(a,b) [a|b]\nA TWO(1)\n", "[1|]"),
        ("variadic_none", "#define V(a,...) [a]\nA V()\n", "[]"),
    ] {
        let r = preprocess_text(&format!("too_few_{}", name), src, &["-P"]);
        assert!(
            !r.stderr.contains("panicked"),
            "{}: the compiler crashed:\n{}",
            name,
            r.stderr
        );
        assert!(
            r.stdout.contains(want),
            "{}: expected {:?} in:\n{}",
            name,
            want,
            r.stdout
        );
    }
}

/// C17 6.10.3.4, Prosser's rule: a function-like invocation's result hides
/// `(HS(name) ∩ HS(rparen)) ∪ {name}` — what *both* ends of the call were
/// hiding, not what the name alone was.
///
/// Propagating the name's set whole over-hides, because the `)` is usually a
/// token of the file hiding nothing: `f(2)(9)` stopped at `2*f(9)`.
#[test]
fn preprocessor_hide_set_intersects_at_the_closing_paren() {
    let r = preprocess_text(
        "hide_intersect",
        "#define f(a) a*g\n#define g(a) f(a)\nX f(2)(9)\n",
        &["-P"],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert!(
        r.stdout.contains("2*9*g"),
        "expected 2*9*g, got:\n{}",
        r.stdout
    );
}

/// `#ifdef`/`#ifndef` track nesting inside a dead branch but must not examine
/// their operand there. gcc skips it entirely, and junk inside an `#if 0` is
/// common enough that diagnosing it would reject working code.
#[test]
fn preprocessor_malformed_conditional_operand_in_a_dead_branch_is_quiet() {
    let r = preprocess_text(
        "dead_branch_junk",
        "#if 0\n#ifdef\n#ifndef 42\n#endif\n#endif\n#endif\nint v;\n",
        &["-P"],
    );
    assert!(
        r.success,
        "junk in a dead branch must not fail:\n{}",
        r.stderr
    );
    assert!(
        r.stdout.contains("int v;"),
        "the live text survives:\n{}",
        r.stdout
    );
}
