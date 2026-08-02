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
    // Run in a scratch directory. ctags writes its default `tags` file into the
    // working directory whenever -f is absent, and the crate root is the cwd
    // under `cargo test`; without this, tests would litter the repository.
    // Every fixture path these tests pass is absolute, so this is transparent.
    let cwd = TempDir::new().unwrap();

    let mut cmd = Command::new(exe_for(bin));
    cmd.args(args).current_dir(cwd.path());
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

// ============================================================================
// cflow — audit #F2, #F3, #F7, #F8, #F9, #F10, #F12
// ============================================================================

/// The worked EXAMPLE from the cflow spec, reproduced verbatim.
///
/// This single fixture pins #F2 (data symbols under -i x), #F12 (the definition
/// line is the declarator's, not the type specifier's) and the `<>` form for an
/// undefined reference all at once.
const SPEC_EXAMPLE: &str = "int i;\nint f();\nint g();\nint h();\nint\nmain(void)\n{\n    f();\n    g();\n    f();\n}\nint\nf()\n{\n    i = h();\n}\n";

#[test]
fn cflow_reproduces_spec_example() {
    let dir = TempDir::new().unwrap();
    let f = src(&dir, "file.c", SPEC_EXAMPLE);

    let (stdout, stderr, code) = run("cflow", &["-i", "x", &f]);
    assert_eq!(code, 0, "{}", stderr);

    // Indentation width is "at least one column position per level" and so is
    // not fixed by the spec; compare with runs of spaces collapsed.
    let squeeze = |s: &str| {
        s.lines()
            .map(|l| {
                let mut out = String::new();
                let mut prev_space = false;
                for c in l.chars() {
                    if c == ' ' {
                        if !prev_space {
                            out.push(' ');
                        }
                        prev_space = true;
                    } else {
                        out.push(c);
                        prev_space = false;
                    }
                }
                out
            })
            .collect::<Vec<_>>()
    };

    let expected = vec![
        format!("1 main: int(), <{} 6>", f),
        format!("2 f: int(), <{} 13>", f),
        "3 h: <>".to_string(),
        format!("4 i: int, <{} 1>", f),
        "5 g: <>".to_string(),
    ];
    assert_eq!(squeeze(&stdout), expected, "raw output was:\n{}", stdout);
}

/// #F2: without -i x, data symbols stay out of the graph.
#[test]
fn cflow_data_symbols_require_i_x() {
    let dir = TempDir::new().unwrap();
    let f = src(&dir, "file.c", SPEC_EXAMPLE);

    let (stdout, _, _) = run("cflow", &[&f]);
    assert!(
        !stdout.lines().any(|l| l.contains(" i: ")),
        "data symbol should be absent without -i x: {:?}",
        stdout
    );
}

/// #F3: a repeated callee prints only the reference number of its definition,
/// in the same "%d %s:%s" shape as every other line.
#[test]
fn cflow_back_reference_format() {
    let dir = TempDir::new().unwrap();
    let f = src(
        &dir,
        "dia.c",
        "int shared(void){return 1;}\nint a(void){return shared();}\nint b(void){return shared();}\nint main(void){return a()+b();}\n",
    );

    let (stdout, stderr, code) = run("cflow", &[&f]);
    assert_eq!(code, 0, "{}", stderr);

    let repeat = stdout
        .lines()
        .rfind(|l| l.contains("shared:"))
        .expect("a repeated reference to `shared`");
    assert!(
        repeat.trim_end().ends_with("shared: 3"),
        "back-reference should be `name: <refnum>`, got {:?}",
        repeat
    );
    assert!(
        !stdout.contains('{') && !stdout.contains('}'),
        "no brace-wrapped reference form should remain: {:?}",
        stdout
    );
}

/// #F7: "Attempts to set the cut-off depth to a non-positive integer shall be
/// ignored."
#[test]
fn cflow_non_positive_depth_is_ignored() {
    let dir = TempDir::new().unwrap();
    let f = src(&dir, "file.c", SPEC_EXAMPLE);

    let (baseline, _, code) = run("cflow", &[&f]);
    assert_eq!(code, 0);

    for depth in ["0", "-1"] {
        let (out, stderr, code) = run("cflow", &["-d", depth, &f]);
        assert_eq!(code, 0, "-d {} should be accepted: {}", depth, stderr);
        assert_eq!(
            out, baseline,
            "-d {} should behave as if -d were absent",
            depth
        );
    }

    // A positive depth still truncates.
    let (shallow, _, _) = run("cflow", &["-d", "1", &f]);
    assert!(shallow.lines().count() < baseline.lines().count());
}

/// #F9: -i takes exactly `x` or `_`.
#[test]
fn cflow_rejects_invalid_include_argument() {
    let dir = TempDir::new().unwrap();
    let f = src(&dir, "file.c", SPEC_EXAMPLE);

    let (_, stderr, code) = run("cflow", &["-i", "bogus", &f]);
    assert_ne!(code, 0, "invalid -i argument should be rejected");
    assert!(stderr.contains("bogus"), "got {:?}", stderr);

    for good in ["x", "_"] {
        let (_, _, code) = run("cflow", &["-i", good, &f]);
        assert_eq!(code, 0, "-i {} should be accepted", good);
    }
}

/// #F8: LC_COLLATE orders the -r listing.
#[test]
fn cflow_reverse_sort_respects_lc_collate() {
    let dir = TempDir::new().unwrap();
    let f = src(
        &dir,
        "coll.c",
        "int apple(void){return 0;}\nint Banana(void){return 0;}\nint cherry(void){return 0;}\n",
    );

    let (out, stderr, code) = run_env("cflow", &["-r", &f], &[("LC_ALL", "C")]);
    assert_eq!(code, 0, "{}", stderr);
    let names: Vec<&str> = out
        .lines()
        .filter_map(|l| l.split_whitespace().nth(1))
        .map(|n| n.trim_end_matches(':'))
        .collect();
    assert_eq!(
        names,
        vec!["Banana", "apple", "cherry"],
        "POSIX locale should collate by byte value: {:?}",
        out
    );
}

// ============================================================================
// -D / -U ordering — audit #F4, #R6
//
// cflow and cxref both state they conform to XBD 12.2 "except that the order of
// the -D, -I, and -U options ... is significant". That is the opposite of c17,
// where -U wins regardless of order, and the three utilities share one
// preprocessor.
// ============================================================================

#[test]
fn cflow_cxref_define_undefine_order_is_significant() {
    let dir = TempDir::new().unwrap();
    let f = src(
        &dir,
        "ord.c",
        "#if defined(FOO) && FOO == 2\nint foo_is_two(void) { return 0; }\n#elif defined(FOO)\nint foo_other(void) { return 0; }\n#else\nint foo_undefined(void) { return 0; }\n#endif\n",
    );

    // (options, the symbol that must appear)
    let cases = [
        (vec!["-DFOO=1", "-UFOO", "-DFOO=2"], "foo_is_two"),
        (vec!["-UFOO", "-DFOO=2"], "foo_is_two"),
        (vec!["-DFOO=1", "-UFOO"], "foo_undefined"),
        (vec!["-DFOO=1"], "foo_other"),
        // Separated spelling must behave identically to the attached one.
        (
            vec!["-D", "FOO=1", "-U", "FOO", "-D", "FOO=2"],
            "foo_is_two",
        ),
    ];

    for (opts, expected) in cases {
        for tool in ["cflow", "cxref"] {
            let mut args = opts.clone();
            if tool == "cxref" {
                args.insert(0, "-s");
            }
            args.push(&f);
            let (stdout, stderr, code) = run(tool, &args);
            assert_eq!(code, 0, "{} {:?}: {}", tool, opts, stderr);
            assert!(
                stdout.contains(expected),
                "{} {:?} should select {}, got:\n{}",
                tool,
                opts,
                expected,
                stdout
            );
        }
    }
}

// ============================================================================
// cflow object-file and lex/yacc input — audit #F5, #F6
// ============================================================================

/// #F5: DESCRIPTION requires cflow to analyze "a collection of object files".
/// Definitions from an object file report the filename and location counter
/// rather than a source line.
#[test]
fn cflow_analyzes_object_files() {
    let dir = TempDir::new().unwrap();
    let c = src(
        &dir,
        "obj.c",
        "int leaf(void){return 1;}\nint middle(void){return leaf();}\nint top(void){return middle();}\n",
    );
    let obj = dir.path().join("obj.o");

    // Build the object with our own compiler so the test needs no host toolchain
    // beyond the assembler c17 already relies on.
    let built = Command::new(env!("CARGO_BIN_EXE_c17"))
        .args(["-c", &c, "-o", obj.to_str().unwrap()])
        .output()
        .expect("run c17");
    assert!(
        built.status.success() && obj.exists(),
        "failed to build fixture object: {}",
        String::from_utf8_lossy(&built.stderr)
    );

    let (stdout, stderr, code) = run("cflow", &[obj.to_str().unwrap()]);
    assert_eq!(code, 0, "{}", stderr);

    // The call graph is recovered from relocations. Note the symbols are named
    // exactly as in the C source: Mach-O's leading underscore is an ABI
    // artifact and must not reach the listing (nor be mistaken for a C name
    // beginning with an underscore, which -i _ governs).
    assert!(
        stdout.contains("top:") && stdout.contains("middle:") && stdout.contains("leaf:"),
        "expected all three symbols: {:?}",
        stdout
    );

    // Assembler-local labels are not program symbols. c17 emits `.L…` on ELF
    // and the Mach-O assembler adds `ltmp…`/`L…`; neither belongs in a flowgraph.
    for line in stdout.lines() {
        let name = line.split_whitespace().nth(1).unwrap_or("");
        assert!(
            !name.starts_with(".L") && !name.starts_with("ltmp"),
            "assembler-local label leaked into the graph: {:?}",
            line
        );
    }
    // "...indicate the filename and location counter under which the symbol
    // appeared (for example, text)."
    assert!(
        stdout.contains("obj.o text>"),
        "definitions should carry the location counter: {:?}",
        stdout
    );
    // Nesting proves the edges were recovered, not just the symbol list.
    //
    // Only asserted where a call between two functions in the same object
    // actually leaves a relocation behind. ELF emits one even for a same-object
    // call (R_X86_64_PLT32, because the symbol may be interposed), but the
    // Mach-O arm64 assembler resolves an intra-section `bl` itself and emits
    // nothing, so there is no relocation to attribute a caller to. Edge recovery
    // for calls that *do* carry a relocation is covered on every platform by
    // `cflow_recovers_external_call_edges` below.
    #[cfg(target_os = "linux")]
    {
        let top_idx = stdout.find("top:").unwrap();
        let mid_idx = stdout.find("middle:").unwrap();
        assert!(
            mid_idx > top_idx,
            "middle should nest under top: {}",
            stdout
        );
    }
}

/// #F5, edge recovery: a call to a function defined elsewhere always leaves a
/// relocation, on every object format, because the target address cannot be
/// known at assembly time. This is the portable half of the relocation-walking
/// logic exercised by `cflow_analyzes_object_files`.
#[test]
fn cflow_recovers_external_call_edges() {
    let dir = TempDir::new().unwrap();
    let c = src(
        &dir,
        "ext.c",
        "extern int outside(int);\nint caller(void) { return outside(1); }\n",
    );
    let obj = dir.path().join("ext.o");

    let built = Command::new(env!("CARGO_BIN_EXE_c17"))
        .args(["-c", &c, "-o", obj.to_str().unwrap()])
        .output()
        .expect("run c17");
    assert!(
        built.status.success() && obj.exists(),
        "failed to build fixture object: {}",
        String::from_utf8_lossy(&built.stderr)
    );

    let (stdout, stderr, code) = run("cflow", &[obj.to_str().unwrap()]);
    assert_eq!(code, 0, "{}", stderr);

    let caller_idx = stdout.find("caller:").expect("caller should be listed");
    let outside_idx = stdout
        .find("outside:")
        .unwrap_or_else(|| panic!("the external callee should be reached: {}", stdout));
    assert!(
        outside_idx > caller_idx,
        "outside should nest under caller: {}",
        stdout
    );
    // It has no definition in this object, so it takes the undefined form.
    assert!(
        stdout.contains("outside: <>"),
        "an undefined reference is written as `<>`: {:?}",
        stdout
    );
}

/// #F6: `.l`/`.y` operands are lex/yacc input and must be "processed as
/// appropriate", not parsed as if they were C.
///
/// The error path is asserted unconditionally (a .l operand with no reachable
/// `lex` must fail loudly rather than silently contribute nothing, which is what
/// used to happen). The success path is additionally asserted when a `lex`
/// binary is present alongside the tools under test.
#[test]
fn cflow_processes_lex_input() {
    let dir = TempDir::new().unwrap();
    let l = src(
        &dir,
        "scan.l",
        "%{\nint count_digits(void);\nint yywrap(void);\n%}\n%%\n[0-9]+  { return count_digits(); }\n%%\nint count_digits(void) { return 1; }\nint yywrap(void) { return 1; }\n",
    );

    // Unreachable generator: must be a loud failure.
    let (_, stderr, code) = run_env("cflow", &[&l], &[("PATH", "/nonexistent")]);
    assert_ne!(code, 0, "a .l operand with no lex must fail");
    assert!(
        stderr.contains("lex"),
        "diagnostic should name the generator: {:?}",
        stderr
    );

    // Success path, when lex was built alongside these binaries.
    let bindir = std::path::Path::new(exe_for("cflow")).parent().unwrap();
    if bindir.join("lex").exists() {
        let path = format!("{}:{}", bindir.display(), std::env::var("PATH").unwrap());
        let (stdout, stderr, code) = run_env("cflow", &[&l], &[("PATH", &path)]);
        assert_eq!(code, 0, "lex input should analyze cleanly: {}", stderr);
        assert!(
            stdout.contains("yylex"),
            "the generated scanner should appear in the graph: {:?}",
            stdout
        );
        // Output must reference the operand, never the temporary file.
        assert!(
            !stdout.contains("lex.yy.c"),
            "output should name the .l operand, not the generated file: {:?}",
            stdout
        );
        assert!(stdout.contains("scan.l"), "got {:?}", stdout);

        // Diagnostics must not leak the temporary path either: it is deleted
        // when the run ends, so it tells the reader nothing actionable.
        let broken = src(
            &dir,
            "broken.l",
            "%%\n[0-9]+  { return 1; }\n%%\nthis is not valid C at all @@@\n",
        );
        let (_, stderr, code) = run_env("cflow", &[&broken], &[("PATH", &path)]);
        assert_ne!(code, 0, "a .l whose generated C will not parse must fail");
        // (The fixture itself lives in a temp dir, so only the *generated*
        // filename indicates a leak.)
        assert!(
            !stderr.contains("lex.yy.c"),
            "diagnostics should name the operand, not the generated file: {:?}",
            stderr
        );
        assert!(
            stderr.contains("broken.l"),
            "diagnostic should name the operand: {:?}",
            stderr
        );
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
