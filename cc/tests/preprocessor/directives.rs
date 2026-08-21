//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Preprocessor Directives Tests
//
// Tests for #line and other preprocessor directives.
//

use crate::common::{compile_and_run, preprocess_text, run_c17};

#[test]
fn preprocessor_line_directive() {
    let code = r#"
#include <string.h>

int main(void) {
    // ========== #line sets __LINE__ (returns 1-9) ==========
    // #line N means the NEXT line is line N
    {
#line 100
        if (__LINE__ != 100) return 1;  // this is line 100
        if (__LINE__ != 101) return 2;  // this is line 101
    }

    // ========== #line sets __FILE__ (returns 10-19) ==========
    {
#line 200 "fake.c"
        if (strcmp(__FILE__, "fake.c") != 0) return 10;  // line 200
        if (__LINE__ != 201) return 11;  // line 201
        if (__LINE__ != 202) return 12;  // line 202
    }

    // ========== #line in false #if branch has no effect (returns 20-29) ==========
    {
#line 300
#if 0
#line 999 "wrong.c"
#endif
        // __LINE__ should NOT be 999
        if (__LINE__ == 999) return 20;
        if (strcmp(__FILE__, "wrong.c") == 0) return 21;
    }

    // ========== #line with only line number keeps previous file (returns 30-39) ==========
    {
#line 50 "first.c"
#line 400
        if (__LINE__ != 400) return 30;  // line 400
        if (strcmp(__FILE__, "first.c") != 0) return 31;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("line_directive", code, &[]), 0);
}

// ============================================================================
// Test: #line with macro-expanded tokens
// ============================================================================

#[test]
fn preprocessor_line_directive_macro_expansion() {
    let code = r#"
int main(void) {
    // #line should macro-expand its tokens before parsing
#define LINENUM 100
#line LINENUM
    if (__LINE__ != 100) return 1;

    // Macro-expanded filename
#define FNAME "expanded.c"
#line 200 FNAME
    if (__LINE__ != 200) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("line_directive_macro_expand", code, &[]), 0);
}

/// A directive's operand has to be on the directive's own line.
///
/// There is no newline token in the stream, so fetching the operand with a
/// bare `next()` reached into the following line and `skip_to_eol` then ate
/// the rest of it. A lone `#define` followed by `alpha beta` silently defined
/// `alpha` as `beta` and deleted the line.
#[test]
fn preprocessor_bare_directive_is_diagnosed() {
    for (name, directive, close) in [
        ("define", "#define", ""),
        ("undef", "#undef", ""),
        ("ifdef", "#ifdef", "#endif\n"),
        ("ifndef", "#ifndef", "#endif\n"),
    ] {
        let src = format!("{}\nalpha beta\n{}int after;\n", directive, close);
        let r = preprocess_text(&format!("bare_{}", name), &src, &[]);
        let want = format!("no macro name given in {} directive", directive);
        assert!(
            !r.success,
            "{} alone should be rejected:\n{}",
            directive, r.stdout
        );
        assert!(
            r.stderr.contains(&want),
            "{} alone should say {:?}, got:\n{}",
            directive,
            want,
            r.stderr
        );
    }
}

/// The line after a bare directive survives untouched.
///
/// Only `#define` and `#undef` can show this directly: a bare `#ifdef` opens a
/// group that is (correctly) skipped, so the following line is absent for a
/// reason that has nothing to do with token theft.
#[test]
fn preprocessor_bare_directive_keeps_the_next_line() {
    for (name, directive) in [("define", "#define"), ("undef", "#undef")] {
        let src = format!("{}\nalpha beta;\nint after;\n", directive);
        let r = preprocess_text(&format!("bare_keeps_{}", name), &src, &[]);
        assert!(
            r.stdout.contains("alpha beta;"),
            "{} alone must not consume the next line, got:\n{}",
            directive,
            r.stdout
        );
        assert!(
            r.stdout.contains("int after;"),
            "{}: later lines survive too, got:\n{}",
            directive,
            r.stdout
        );
    }
}

/// gcc: "macro names must be identifiers".
#[test]
fn preprocessor_macro_name_must_be_an_identifier() {
    for (name, src) in [
        ("define", "#define 123 x\n"),
        ("undef", "#undef 456\n"),
        ("ifdef", "#ifdef 789\n#endif\n"),
        ("ifndef", "#ifndef +\n#endif\n"),
    ] {
        let r = preprocess_text(&format!("nonident_{}", name), src, &[]);
        assert!(!r.success, "{} should be rejected:\n{}", name, r.stdout);
        assert!(
            r.stderr.contains("macro names must be identifiers"),
            "{} should say so, got:\n{}",
            name,
            r.stderr
        );
    }
}

/// A rejected `#define` is not a macro and its replacement list is not
/// anything. Returning from the parameter-list error without draining the line
/// left those tokens to be emitted as ordinary code, after the directive that
/// produced them had already been diagnosed.
#[test]
fn preprocessor_rejected_define_does_not_emit_its_body() {
    let r = preprocess_text(
        "rejected_define_body",
        "#define H(x + y) THIS_SHOULD_NOT_APPEAR\nint ok;\n",
        &[],
    );
    assert!(
        !r.success,
        "the directive should be rejected:\n{}",
        r.stdout
    );
    assert!(
        !r.stdout.contains("THIS_SHOULD_NOT_APPEAR"),
        "the rejected body must not be emitted:\n{}",
        r.stdout
    );
    assert!(
        r.stdout.contains("int ok;"),
        "the next line survives:\n{}",
        r.stdout
    );
}

/// An unterminated parameter list must stop at the line boundary rather than
/// consuming the next line's first token. Two loops did the opposite --
/// consume, then test `pos.newline` -- so the token was already gone.
#[test]
fn preprocessor_unterminated_param_list_keeps_the_next_line() {
    for (name, src) in [
        ("plain", "#define F(x\nDROPPED kept;\nint ok;\n"),
        (
            "named_variadic",
            "#define G(a, rest...\nDROPPED kept;\nint ok;\n",
        ),
    ] {
        let r = preprocess_text(&format!("unterminated_params_{}", name), src, &[]);
        assert!(!r.success, "{} should be rejected:\n{}", name, r.stdout);
        assert!(
            r.stdout.contains("DROPPED"),
            "{}: the next line's first token must survive, got:\n{}",
            name,
            r.stdout
        );
    }
}

/// `-D` is one directive however the shell wrapped it. Its first token is the
/// first token of its own buffer, so it is flagged as beginning a line, which
/// the operand's same-line check read as `#define` with nothing after it.
#[test]
fn preprocessor_command_line_defines_still_work() {
    let r = preprocess_text(
        "cmdline_defines",
        "const char *v = GITVERSION;\nint p = PLAIN;\nint q = VAL;\n",
        &["-DGITVERSION=\"abc\"", "-DPLAIN=1", "-DVAL=3"],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    for want in ["\"abc\"", "int p = 1;", "int q = 3;"] {
        assert!(
            r.stdout.contains(want),
            "expected {:?} in:\n{}",
            want,
            r.stdout
        );
    }
}

/// C17 6.10.1: a conditional group runs `#if`, then any `#elif`s, then at most
/// one `#else`, and it must be closed. None of that was checked.
///
/// The two duplicate cases are the destructive ones: a second `#else` was a
/// legal state transition that turned the group `Done`, truncating the first
/// `#else` body and dropping the second, with nothing on stderr.
#[test]
fn preprocessor_conditional_nesting_is_validated() {
    for (name, src, needle) in [
        ("stray_endif", "#endif\nint v;\n", "#endif without #if"),
        ("stray_else", "#else\nint v;\n", "#else without #if"),
        ("stray_elif", "#elif 1\nint v;\n", "#elif without #if"),
        (
            "double_else",
            "#if 1\nint a;\n#else\nint b;\n#else\nint c;\n#endif\n",
            "#else after #else",
        ),
        (
            "elif_after_else",
            "#if 0\nint a;\n#else\nint b;\n#elif 1\nint c;\n#endif\n",
            "#elif after #else",
        ),
        ("unterminated", "#if 1\nint a;\n", "unterminated #if"),
    ] {
        let r = preprocess_text(&format!("cond_{}", name), src, &[]);
        assert!(!r.success, "{} should be rejected:\n{}", name, r.stdout);
        assert!(
            r.stderr.contains(needle),
            "{} should say {:?}, got:\n{}",
            name,
            needle,
            r.stderr
        );
    }
}

/// The conditional stack is swapped out around an inclusion so a header cannot
/// close one of the includer's groups. That also meant an unterminated `#if`
/// in a header was discarded rather than reported.
#[test]
fn preprocessor_unterminated_conditional_in_a_header_is_diagnosed() {
    let dir = tempfile::Builder::new()
        .prefix("c17_unterm_hdr_")
        .tempdir()
        .unwrap();
    std::fs::write(dir.path().join("u.h"), "#if 1\nint never_closed;\n").unwrap();
    let src = dir.path().join("m.c");
    std::fs::write(&src, "#include \"u.h\"\nint after;\n").unwrap();

    let r = run_c17(&["-E", &src.to_string_lossy()]);
    assert!(!r.success, "the header leaves a group open:\n{}", r.stdout);
    assert!(
        r.stderr.contains("unterminated #if"),
        "expected the unterminated-#if error, got:\n{}",
        r.stderr
    );
}

/// gcc warns about anything after `#else` or `#endif`, which take no operands.
/// These were eaten in silence.
#[test]
fn preprocessor_extra_tokens_after_a_conditional_warn() {
    let r = preprocess_text(
        "extra_after_cond",
        "#if 1\nint a;\n#else JUNK\nint b;\n#endif TRAILING\n",
        &[],
    );
    assert!(r.success, "these are warnings, not errors:\n{}", r.stderr);
    for want in [
        "extra tokens at end of #else directive",
        "extra tokens at end of #endif directive",
    ] {
        assert!(
            r.stderr.contains(want),
            "expected {:?}, got:\n{}",
            want,
            r.stderr
        );
    }
}
