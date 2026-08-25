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
    let dir = plib::tmp::Builder::new()
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

/// A header is only skipped on re-inclusion once it has been read through to
/// the end and found to be exactly one guarded group.
///
/// Guessing instead — scanning for an `#ifndef` and skipping whenever that
/// name happened to be defined — deleted source three ways, all of them here.
#[test]
fn preprocessor_include_guard_never_deletes_source() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_guard_")
        .tempdir()
        .unwrap();
    let write = |name: &str, body: &str| {
        std::fs::write(dir.path().join(name), body).unwrap();
    };
    let run = |name: &str, extra: &[&str]| {
        let src = dir.path().join(name).to_string_lossy().to_string();
        let mut args: Vec<&str> = vec!["-E"];
        args.extend_from_slice(extra);
        args.push(&src);
        run_c17(&args)
    };

    // 1. Code after the closing `#endif` is not guarded, so it appears on
    //    every include. The old scan stopped at the first body token and never
    //    saw the `#endif`, let alone what followed it.
    write(
        "g1.h",
        "#ifndef G1_H\n#define G1_H\nint inside;\n#endif\nint outside;\n",
    );
    write("g1.c", "#include \"g1.h\"\n#include \"g1.h\"\n");
    let r = run("g1.c", &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_eq!(
        r.stdout.matches("int outside;").count(),
        2,
        "code outside the guard belongs to every include:\n{}",
        r.stdout
    );
    assert_eq!(
        r.stdout.matches("int inside;").count(),
        1,
        "the guarded body belongs to the first include only:\n{}",
        r.stdout
    );

    // 2. Defining the guard name on the command line must not delete the file.
    //    It was never read, so nothing was known about it.
    write(
        "g2.h",
        "#ifndef G2_H\n#define G2_H\nint guarded;\n#endif\nint outside;\n",
    );
    write("g2.c", "#include \"g2.h\"\n");
    let r = run("g2.c", &["-DG2_H"]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert!(
        r.stdout.contains("int outside;"),
        "-D<guard> must not delete code outside the guard:\n{}",
        r.stdout
    );
    assert!(
        !r.stdout.contains("int guarded;"),
        "the guarded body is still skipped:\n{}",
        r.stdout
    );

    // 3. A header including itself under a counter guard. The recursion is
    //    bounded by the guard, and both arms belong in the output.
    write(
        "g3.h",
        "#ifndef DEPTH\n#define DEPTH 1\n#include \"g3.h\"\nint outer_arm;\n\
         #else\nint inner_arm;\n#endif\n",
    );
    write("g3.c", "#include \"g3.h\"\n");
    let r = run("g3.c", &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    let inner = r.stdout.find("int inner_arm;");
    let outer = r.stdout.find("int outer_arm;");
    assert!(
        inner.is_some() && outer.is_some() && inner < outer,
        "expected the inner arm then the outer, got:\n{}",
        r.stdout
    );

    // 4. The optimization still works: a properly guarded header contributes
    //    nothing the second time.
    write("g4.h", "#ifndef G4_H\n#define G4_H\nint once;\n#endif\n");
    write("g4.c", "#include \"g4.h\"\n#include \"g4.h\"\n");
    let r = run("g4.c", &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert_eq!(
        r.stdout.matches("int once;").count(),
        1,
        "a guarded header is included once:\n{}",
        r.stdout
    );
}

/// A genuine cycle is bounded by include depth rather than by an immediate
/// error, which is what gcc does and what lets a counter-guarded self-include
/// work at all.
#[test]
fn preprocessor_unguarded_include_cycle_is_bounded() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_cycle_")
        .tempdir()
        .unwrap();
    std::fs::write(dir.path().join("a.h"), "#include \"b.h\"\n").unwrap();
    std::fs::write(dir.path().join("b.h"), "#include \"a.h\"\n").unwrap();
    let src = dir.path().join("m.c");
    std::fs::write(&src, "#include \"a.h\"\nint x;\n").unwrap();

    let r = run_c17(&["-E", &src.to_string_lossy()]);
    assert!(!r.success, "a cycle must not succeed:\n{}", r.stdout);
    assert!(
        r.stderr.contains("nested too deeply"),
        "expected the depth limit to catch it, got:\n{}",
        r.stderr
    );
}

/// `c17 -E` used to keep one pragma line out of five. POSIX makes a `.i` a
/// valid operand and c17 compiles one, so dropping the rest meant that
/// preprocessing and compiling in two steps did something different from
/// compiling in one.
#[test]
fn preprocessor_every_pragma_survives_preprocessing() {
    let src = "#pragma GCC visibility push(default)\n\
               #pragma GCC diagnostic ignored \"-Wunused\"\n\
               #pragma omp parallel for\n\
               #pragma pack(1)\n\
               _Pragma(\"GCC diagnostic pop\")\n\
               int q;\n";
    let r = preprocess_text("all_pragmas", src, &[]);
    assert!(r.success, "-E failed: {}", r.stderr);
    for want in [
        "#pragma GCC visibility push(default)",
        "#pragma GCC diagnostic ignored \"-Wunused\"",
        "#pragma omp parallel for",
        "#pragma pack(1)",
        // The operator lowers to the directive it stands for (C99 6.10.9p1).
        "#pragma GCC diagnostic pop",
    ] {
        assert!(
            r.stdout.contains(want),
            "expected {:?} in:\n{}",
            want,
            r.stdout
        );
    }
    assert_eq!(
        r.stdout.matches("#pragma").count(),
        5,
        "expected exactly five pragma lines:\n{}",
        r.stdout
    );
}

/// A `_Pragma` operand is a string literal, so the escaping the lexer kept has
/// to come back off before the directive is written.
#[test]
fn preprocessor_pragma_operator_destringifies() {
    let r = preprocess_text(
        "pragma_destringify",
        "_Pragma(\"GCC diagnostic ignored \\\"-Wunused\\\"\")\nint q;\n",
        &[],
    );
    assert!(r.success, "-E failed: {}", r.stderr);
    assert!(
        r.stdout
            .contains("#pragma GCC diagnostic ignored \"-Wunused\""),
        "the operand's escaping should be undone:\n{}",
        r.stdout
    );
}

/// Preprocessing to a `.i` and compiling that must lay a struct out the same
/// way as compiling the source directly — which is the whole reason the
/// pragmas have to survive.
#[test]
fn preprocessor_packing_survives_a_two_step_compile() {
    let src = r#"
#pragma pack(push, 1)
struct S { char c; int i; };
#pragma pack(pop)
struct T { char c; int i; };
int main(void) {
    if (sizeof(struct S) != 5) return 1;
    if (sizeof(struct T) <= 5) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("pack_two_step", src, &[]), 0);
}

/// `#include_next` resumes the search *after* the directory the current file
/// came from -- and `-I` directories are part of that search.
///
/// The `-I` list and the system list are stored separately, and the resume
/// index only ever indexed the system list. A header found through `-I` had no
/// index to resume from, so it restarted at the system list's first entry and
/// never saw the remaining `-I` directories: `#include_next <n.h>` written in
/// `ia/n.h` under `-Iia -Iib` reported "file not found" where gcc finds
/// `ib/n.h`.
#[test]
fn preprocessor_include_next_walks_the_dash_i_path() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_include_next_")
        .tempdir()
        .unwrap();
    let (a, b, c) = (
        dir.path().join("a"),
        dir.path().join("b"),
        dir.path().join("c"),
    );
    for d in [&a, &b, &c] {
        std::fs::create_dir(d).unwrap();
    }
    // Three levels, so the resume point has to advance each time rather than
    // merely leave the first directory.
    std::fs::write(a.join("n.h"), "#include_next <n.h>\n#define L1 1\n").unwrap();
    std::fs::write(b.join("n.h"), "#include_next <n.h>\n#define L2 2\n").unwrap();
    std::fs::write(c.join("n.h"), "#define L3 3\n").unwrap();

    let src = dir.path().join("m.c");
    std::fs::write(&src, "#include <n.h>\nint x = L1 + L2 + L3;\n").unwrap();

    let r = run_c17(&[
        "-E",
        &format!("-I{}", a.display()),
        &format!("-I{}", b.display()),
        &format!("-I{}", c.display()),
        &src.to_string_lossy(),
    ]);
    assert!(r.success, "include_next failed:\n{}", r.stderr);
    let text: String = r.stdout.split_whitespace().collect::<Vec<_>>().join(" ");
    assert!(
        text.contains("int x = 1 + 2 + 3;"),
        "every level should have been reached:\n{}",
        r.stdout
    );
}

/// The other half of the same index space: a file found on a *system* path
/// still resumes after that path, not from the front of it. `-I` directories
/// come first in the numbering, so the system entries had to be renumbered
/// past them -- getting that offset wrong would make `#include_next` from a
/// system header re-find the very file it was written in, and loop.
#[test]
fn preprocessor_include_next_from_a_system_path_still_advances() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_include_next_sys_")
        .tempdir()
        .unwrap();
    let (q, s1, s2) = (
        dir.path().join("q"),
        dir.path().join("s1"),
        dir.path().join("s2"),
    );
    for d in [&q, &s1, &s2] {
        std::fs::create_dir(d).unwrap();
    }
    // A -I directory that is *not* on the chain, so it only contributes to the
    // numbering.
    std::fs::write(q.join("unrelated.h"), "#define UNUSED 0\n").unwrap();
    std::fs::write(s1.join("n.h"), "#include_next <n.h>\n#define S1 1\n").unwrap();
    std::fs::write(s2.join("n.h"), "#define S2 2\n").unwrap();

    let src = dir.path().join("m.c");
    std::fs::write(&src, "#include <n.h>\nint y = S1 + S2;\n").unwrap();

    let r = run_c17(&[
        "-E",
        &format!("-I{}", q.display()),
        "--isystem",
        &s1.to_string_lossy(),
        "--isystem",
        &s2.to_string_lossy(),
        &src.to_string_lossy(),
    ]);
    assert!(
        r.success,
        "include_next from a system path failed:\n{}",
        r.stderr
    );
    let text: String = r.stdout.split_whitespace().collect::<Vec<_>>().join(" ");
    assert!(
        text.contains("int y = 1 + 2;"),
        "expected both system levels:\n{}",
        r.stdout
    );
}

/// `-nostdinc` drops the *target's own* header directories. It does not drop
/// the ones the caller named with `-isystem` or `-idirafter`: those were asked
/// for explicitly, and gcc keeps searching them.
///
/// c17 held one flag for "use system paths", cleared it for `-nostdinc`, and
/// so lost all three at once -- `-nostdinc -isystem d` could not find a header
/// sitting in `d`.
#[test]
fn preprocessor_nostdinc_keeps_the_caller_s_system_paths() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_nostdinc_")
        .tempdir()
        .unwrap();
    let (sys, quote) = (dir.path().join("sys"), dir.path().join("q"));
    for d in [&sys, &quote] {
        std::fs::create_dir(d).unwrap();
    }
    std::fs::write(sys.join("h.h"), "#define FROM_SYSTEM 1\n").unwrap();
    std::fs::write(quote.join("qh.h"), "#define FROM_QUOTE 2\n").unwrap();
    let src = dir.path().join("m.c");
    std::fs::write(
        &src,
        "#include <h.h>\n#include \"qh.h\"\nint a = FROM_SYSTEM;\nint b = FROM_QUOTE;\n",
    )
    .unwrap();

    for flag in ["--isystem", "--idirafter"] {
        let r = run_c17(&[
            "-E",
            "-nostdinc",
            flag,
            &sys.to_string_lossy(),
            &format!("-I{}", quote.display()),
            &src.to_string_lossy(),
        ]);
        assert!(r.success, "{} under -nostdinc failed:\n{}", flag, r.stderr);
        let text: String = r.stdout.split_whitespace().collect::<Vec<_>>().join(" ");
        assert!(
            text.contains("int a = 1;") && text.contains("int b = 2;"),
            "{} should still be searched under -nostdinc:\n{}",
            flag,
            r.stdout
        );
    }

    // The half that must keep working: `-nostdinc` on its own really does drop
    // the standard directories, the bundled headers among them -- `gcc
    // -nostdinc` cannot find <stddef.h> either.
    let bare = dir.path().join("bare.c");
    std::fs::write(&bare, "#include <stddef.h>\n").unwrap();
    let r = run_c17(&["-E", "-nostdinc", &bare.to_string_lossy()]);
    assert!(
        !r.success,
        "-nostdinc should drop the standard directories:\n{}",
        r.stdout
    );
}
