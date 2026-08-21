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
