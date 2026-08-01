//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// POSIX.1-2024 conformance tests for cflow, ctags, and cxref.
//
// Each test names the audit item it covers (see cc/audit.md). These are
// deliberately negative-path and format-exactness tests: the pre-existing
// mega-tests in this directory prove that accepted input produces plausible
// output, which is why none of them caught the defects recorded in the audit.
//

use std::fs;
use std::process::Command;
use tempfile::TempDir;

fn exe_for(bin: &str) -> &'static str {
    match bin {
        "cflow" => env!("CARGO_BIN_EXE_cflow"),
        "ctags" => env!("CARGO_BIN_EXE_ctags"),
        "cxref" => env!("CARGO_BIN_EXE_cxref"),
        _ => unreachable!(),
    }
}

fn run(bin: &str, args: &[&str]) -> (String, String, i32) {
    run_env(bin, args, &[])
}

fn run_env(bin: &str, args: &[&str], env: &[(&str, &str)]) -> (String, String, i32) {
    let mut cmd = Command::new(exe_for(bin));
    cmd.args(args);
    for (k, v) in env {
        cmd.env(k, v);
    }
    let output = cmd
        .output()
        .unwrap_or_else(|e| panic!("failed to execute {}: {}", bin, e));
    (
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
        output.status.code().unwrap_or(-1),
    )
}

/// Write `content` into a fresh temp dir and return (dir, path-as-string).
fn src(dir: &TempDir, name: &str, content: &str) -> String {
    let p = dir.path().join(name);
    fs::write(&p, content).unwrap();
    p.to_str().unwrap().to_string()
}

// ============================================================================
// EXIT STATUS — audit #F1, #T1, #R3
//
// POSIX EXIT STATUS for all three utilities: "0 Successful completion.
// >0 An error occurred."  Every one of them previously printed a diagnostic
// and then returned 0.
// ============================================================================

#[test]
fn tools_exit_status_mega() {
    let dir = TempDir::new().unwrap();
    let missing = dir.path().join("nosuch.c");
    let missing = missing.to_str().unwrap();

    let good = src(&dir, "good.c", "int alpha(void) { return 0; }\n");
    let unknown = src(&dir, "thing.pas", "begin end.\n");
    // Refers to an identifier that was never declared: the C front end emits a
    // diagnostic of its own, which must also force a non-zero status.
    let bad = src(&dir, "bad.c", "int f(void) { return never_declared(); }\n");

    for tool in ["cflow", "ctags", "cxref"] {
        // A file that cannot be opened is an error.
        let (_, stderr, code) = run(tool, &[missing]);
        assert_ne!(code, 0, "{}: missing file must exit non-zero", tool);
        assert!(
            stderr.contains("nosuch.c"),
            "{}: diagnostic should name the file, got {:?}",
            tool,
            stderr
        );

        // An operand the utility refuses to process is an error.
        let (_, _, code) = run(tool, &[&unknown]);
        assert_ne!(code, 0, "{}: unusable operand must exit non-zero", tool);

        // A front-end diagnostic must reach the exit status too.
        let (_, stderr, code) = run(tool, &[&bad]);
        assert_ne!(
            code, 0,
            "{}: front-end error must exit non-zero (stderr: {})",
            tool, stderr
        );

        // ...and a clean run must still succeed.
        let (_, stderr, code) = run(tool, &[&good]);
        assert_eq!(code, 0, "{}: clean run must exit 0 ({})", tool, stderr);
    }
}

// ============================================================================
// ctags — audit #T2, #T3, #T6, #T7, #T9
// ============================================================================

/// #T2: a tag name that appears in more than one file must not lose entries.
/// The old map was keyed by name alone, so the second `init` silently replaced
/// the first.
#[test]
fn ctags_duplicate_names_all_survive() {
    let dir = TempDir::new().unwrap();
    let f1 = src(&dir, "f1.c", "int init(void) { return 1; }\n");
    let f2 = src(&dir, "f2.c", "int init(void) { return 2; }\n");
    let tags = dir.path().join("tags");

    let (_, stderr, code) = run("ctags", &["-f", tags.to_str().unwrap(), &f1, &f2]);
    assert_eq!(code, 0, "{}", stderr);

    let body = fs::read_to_string(&tags).unwrap();
    let init_lines: Vec<&str> = body.lines().filter(|l| l.starts_with("init\t")).collect();
    assert_eq!(
        init_lines.len(),
        2,
        "both definitions of `init` must be tagged, got: {:?}",
        body
    );
    assert!(init_lines.iter().any(|l| l.contains("f1.c")));
    assert!(init_lines.iter().any(|l| l.contains("f2.c")));
}

/// #T3: POSIX STDOUT mandates `"%s %d %s %s"` for -x — single spaces, and
/// <text> is the text of the line, so leading indentation is part of it.
#[test]
fn ctags_index_format_is_exact() {
    let dir = TempDir::new().unwrap();
    let f = src(
        &dir,
        "x.c",
        "int alpha(void) { return 0; }\n  int inset(void) { return 1; }\n",
    );

    let (stdout, stderr, code) = run("ctags", &["-x", &f]);
    assert_eq!(code, 0, "{}", stderr);

    let alpha = stdout
        .lines()
        .find(|l| l.starts_with("alpha "))
        .expect("alpha line");
    // name SP line SP file SP text — exactly three single-space separators
    // before the text field, and no column padding.
    assert_eq!(
        alpha,
        format!("alpha 1 {} int alpha(void) {{ return 0; }}", f),
        "unexpected -x format"
    );

    let inset = stdout
        .lines()
        .find(|l| l.starts_with("inset "))
        .expect("inset line");
    assert!(
        inset.ends_with("  int inset(void) { return 1; }"),
        "leading indentation of the source line must be preserved, got {:?}",
        inset
    );
}

/// #T6: `typedef struct { ... } Name;` spans lines, so the typedef keyword and
/// the declared name are never on the same line.
#[test]
fn ctags_multiline_typedef_is_tagged() {
    let dir = TempDir::new().unwrap();
    let f = src(
        &dir,
        "t.c",
        "typedef struct {\n    int x;\n} Widget;\n\ntypedef int simple_t;\n",
    );
    let tags = dir.path().join("tags");

    let (_, stderr, code) = run("ctags", &["-f", tags.to_str().unwrap(), &f]);
    assert_eq!(code, 0, "{}", stderr);

    let body = fs::read_to_string(&tags).unwrap();
    assert!(
        body.lines().any(|l| l.starts_with("Widget\t")),
        "multi-line typedef should be tagged: {:?}",
        body
    );
    // The search pattern must match the line the name is actually on.
    assert!(
        body.contains("/^} Widget;$/"),
        "pattern should anchor to the declarator line: {:?}",
        body
    );
    assert!(
        body.lines().any(|l| l.starts_with("simple_t\t")),
        "single-line typedef must still work: {:?}",
        body
    );
}

/// #T7: the two SYNOPSIS forms are mutually exclusive.
#[test]
fn ctags_index_conflicts_with_tagsfile_options() {
    let dir = TempDir::new().unwrap();
    let f = src(&dir, "c.c", "int q(void) { return 0; }\n");

    for extra in [vec!["-a"], vec!["-f", "othertags"]] {
        let mut args = vec!["-x"];
        args.extend(extra.iter().copied());
        args.push(&f);
        let (_, stderr, code) = run("ctags", &args);
        assert_ne!(code, 0, "-x with {:?} should be rejected", extra);
        assert!(
            stderr.contains("cannot be used with"),
            "expected a conflict diagnostic, got {:?}",
            stderr
        );
    }
}

/// #T9: a stray non-UTF-8 byte must not cost the file all of its tags.
#[test]
fn ctags_tolerates_non_utf8_input() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("latin.c");
    let mut bytes = b"/* caf\xe9 */\nint tagme(void) { return 0; }\n".to_vec();
    bytes.push(b'\n');
    fs::write(&p, &bytes).unwrap();
    let tags = dir.path().join("tags");

    let (_, stderr, code) = run(
        "ctags",
        &["-f", tags.to_str().unwrap(), p.to_str().unwrap()],
    );
    assert_eq!(code, 0, "non-UTF-8 input should not be fatal: {}", stderr);
    let body = fs::read_to_string(&tags).unwrap();
    assert!(
        body.contains("tagme"),
        "tags should still be produced: {:?}",
        body
    );
}

// ============================================================================
// cxref — audit #R1, #R2, #R4, #R5, #R7, #R8, #R11
// ============================================================================

/// Return the reference column(s) of the row whose symbol is `name`.
fn cxref_rows<'a>(out: &'a str, name: &str) -> Vec<&'a str> {
    out.lines()
        .filter(|l| l.split_whitespace().next() == Some(name))
        .collect()
}

/// #R1: macros are consumed by the preprocessor, so a `#define` used to be
/// absent from the listing entirely.
#[test]
fn cxref_macros_are_cross_referenced() {
    let dir = TempDir::new().unwrap();
    let f = src(
        &dir,
        "m.c",
        "#define MAXV 10\nint g;\nint f(void) { return g + MAXV; }\n",
    );

    let (stdout, stderr, code) = run("cxref", &["-s", &f]);
    assert_eq!(code, 0, "{}", stderr);

    let rows = cxref_rows(&stdout, "MAXV");
    assert!(!rows.is_empty(), "MAXV should be listed: {:?}", stdout);
    let joined = rows.join(" ");
    assert!(
        joined.contains("*1"),
        "the #define line should be the declaring reference: {:?}",
        joined
    );
    assert!(
        joined.contains(" 3") || joined.ends_with("3"),
        "the use on line 3 should be referenced: {:?}",
        joined
    );
}

/// #R2: declarations without an initializer previously reported line 0, and
/// uninitialized locals were dropped from the listing altogether.
#[test]
fn cxref_declaration_line_numbers_are_real() {
    let dir = TempDir::new().unwrap();
    let f = src(
        &dir,
        "d.c",
        "int g;\nint fn(void) {\n    int loc;\n    loc = g;\n    return loc;\n}\n",
    );

    let (stdout, stderr, code) = run("cxref", &["-s", &f]);
    assert_eq!(code, 0, "{}", stderr);

    assert!(
        !stdout.contains("*0"),
        "no declaration should report line 0: {:?}",
        stdout
    );
    let g = cxref_rows(&stdout, "g").join(" ");
    assert!(
        g.contains("*1"),
        "uninitialized global should be declared on line 1: {:?}",
        g
    );
    let loc = cxref_rows(&stdout, "loc").join(" ");
    assert!(
        loc.contains("*3"),
        "uninitialized local should appear, declared on line 3: {:?}",
        loc
    );
}

/// #R4: POSIX STDOUT requires a per-file name line when -c is absent, which
/// also makes -c meaningfully different from the default.
#[test]
fn cxref_per_file_header_distinguishes_combined() {
    let dir = TempDir::new().unwrap();
    let p = src(&dir, "p.c", "int aaa;\n");
    let q = src(&dir, "q.c", "int bbb;\n");

    let (plain, stderr, code) = run("cxref", &[&p, &q]);
    assert_eq!(code, 0, "{}", stderr);
    assert!(
        plain.lines().any(|l| l.trim() == p),
        "input file name should appear on its own line: {:?}",
        plain
    );
    assert!(plain.lines().any(|l| l.trim() == q));

    let (combined, _, code) = run("cxref", &["-c", &p, &q]);
    assert_eq!(code, 0);
    assert!(
        !combined.lines().any(|l| l.trim() == p),
        "-c must not emit per-file headers: {:?}",
        combined
    );
    assert_ne!(
        plain, combined,
        "-c must produce different output from the default"
    );

    // -s suppresses filenames, header included.
    let (silent, _, _) = run("cxref", &["-s", &p]);
    assert!(!silent.contains(&p), "-s should suppress filenames");
}

/// #R5: a function name is not "a symbol appearing inside a function", so its
/// own definition belongs to file scope.
#[test]
fn cxref_function_definition_is_file_scope() {
    let dir = TempDir::new().unwrap();
    let f = src(&dir, "s.c", "int helper(void) {\n    return 1;\n}\n");

    let (stdout, stderr, code) = run("cxref", &["-s", &f]);
    assert_eq!(code, 0, "{}", stderr);

    let rows = cxref_rows(&stdout, "helper");
    assert_eq!(rows.len(), 1, "expected a single row: {:?}", stdout);
    // Row layout is "name  <file>  <function>  refs"; with -s the file column
    // is blank, so a file-scope row must not name `helper` a second time.
    let after_name = rows[0].trim_start().trim_start_matches("helper");
    assert!(
        !after_name.contains("helper"),
        "function's own definition should not be scoped to itself: {:?}",
        rows[0]
    );
}

/// #R7: LC_COLLATE determines the order of the symbol listing.
#[test]
fn cxref_sort_respects_lc_collate() {
    let dir = TempDir::new().unwrap();
    let f = src(&dir, "coll.c", "int apple;\nint Banana;\nint cherry;\n");

    let order = |env: &[(&str, &str)]| -> Vec<String> {
        let (out, _, _) = run_env("cxref", &["-s", &f], env);
        out.lines()
            .filter_map(|l| l.split_whitespace().next())
            .map(String::from)
            .collect()
    };

    // In the POSIX locale collation is by byte value, so uppercase sorts first.
    let c_order = order(&[("LC_ALL", "C")]);
    assert_eq!(
        c_order,
        vec!["Banana", "apple", "cherry"],
        "POSIX locale should collate by byte value"
    );
}

/// #R8: cxref's OPERANDS section names no filename suffix rule.
#[test]
fn cxref_accepts_any_pathname() {
    let dir = TempDir::new().unwrap();
    let f = src(&dir, "source_without_suffix", "int visible;\n");

    let (stdout, stderr, code) = run("cxref", &["-s", &f]);
    assert_eq!(
        code, 0,
        "suffix-less C source should be accepted: {}",
        stderr
    );
    assert!(
        stdout.contains("visible"),
        "symbols should be listed: {:?}",
        stdout
    );
}

/// #R11: -w bounds the whole line, not just the line-number run.
#[test]
fn cxref_width_bounds_every_line() {
    let dir = TempDir::new().unwrap();
    let body = format!(
        "int aa;\nint use(void) {{ return {}; }}\n",
        vec!["aa"; 20].join(" + ")
    );
    let f = src(&dir, "a_rather_long_source_file_name.c", &body);

    for width in [80usize, 40, 20] {
        let w = width.to_string();
        let (stdout, stderr, code) = run_env("cxref", &["-w", &w, &f], &[("LC_ALL", "C")]);
        assert_eq!(code, 0, "{}", stderr);
        for line in stdout.lines() {
            // The per-file header is the filename itself and cannot be folded.
            if line.trim() == f {
                continue;
            }
            assert!(
                line.len() <= width,
                "-w {} exceeded by {:?} (len {})",
                width,
                line,
                line.len()
            );
        }
    }
}

/// Diagnostics belong on stderr only; stdout carries the report.
/// POSIX STDERR: "used only for diagnostic messages" (all three utilities).
#[test]
fn tools_diagnostics_go_to_stderr() {
    let dir = TempDir::new().unwrap();
    let missing = dir.path().join("absent.c");
    let missing = missing.to_str().unwrap();

    for tool in ["cflow", "ctags", "cxref"] {
        let (stdout, stderr, _) = run(tool, &[missing]);
        assert!(
            !stdout.contains("absent.c"),
            "{}: diagnostic leaked to stdout: {:?}",
            tool,
            stdout
        );
        assert!(
            stderr.contains(tool),
            "{}: diagnostic should be prefixed with the utility name, got {:?}",
            tool,
            stderr
        );
    }
}
