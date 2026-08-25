//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// cflow Mega-Test
//
// Consolidates: ALL cflow tests
//

use std::process::Command;

fn run_cflow(args: &[&str]) -> (String, String, bool) {
    let output = Command::new(env!("CARGO_BIN_EXE_cflow"))
        .args(args)
        .output()
        .expect("Failed to execute cflow");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    (stdout, stderr, output.status.success())
}

// ============================================================================
// Mega-test: cflow
// ============================================================================

#[test]
fn tools_cflow_mega() {
    // ========== BASIC CFLOW ==========
    {
        let (stdout, stderr, success) = run_cflow(&["tests/cflow/test.c"]);

        assert!(success, "cflow failed: {}", stderr);

        // Check for main as root
        assert!(
            stdout.contains("main:"),
            "Should contain main as root: {}",
            stdout
        );

        // Check for function calls from main
        assert!(
            stdout.contains("f:"),
            "Should contain call to f: {}",
            stdout
        );
        assert!(
            stdout.contains("g:"),
            "Should contain call to g: {}",
            stdout
        );

        // Check for nested call (h called from f)
        assert!(
            stdout.contains("h:"),
            "Should contain call to h: {}",
            stdout
        );
    }

    // ========== DEPTH LIMIT ==========
    {
        let (stdout, stderr, success) = run_cflow(&["-d", "1", "tests/cflow/test.c"]);

        assert!(success, "cflow -d 1 failed: {}", stderr);

        // Should contain main and its direct calls
        assert!(stdout.contains("main:"), "Should contain main");
        assert!(stdout.contains("f:"), "Should contain f (depth 1)");
        assert!(stdout.contains("g:"), "Should contain g (depth 1)");

        // Count indentation to verify depth limit
        let lines: Vec<&str> = stdout.lines().collect();
        for line in &lines {
            let indent_count = line.len() - line.trim_start().len();
            assert!(
                indent_count <= 4,
                "With -d 1, no line should have more than 4 spaces indent: {}",
                line
            );
        }
    }

    // ========== REVERSE MODE ==========
    {
        let (stdout, stderr, success) = run_cflow(&["-r", "tests/cflow/test.c"]);

        assert!(success, "cflow -r failed: {}", stderr);

        // In reverse mode, functions are listed with their callers
        assert!(stdout.contains("h:"), "Should contain h");

        // Check that h has f as a caller
        let h_section = stdout
            .lines()
            .skip_while(|line| !line.contains("h:"))
            .take_while(|line| !line.is_empty() || line.contains("h:"))
            .collect::<Vec<&str>>()
            .join("\n");

        assert!(
            h_section.contains("f:"),
            "h should be called by f: {}",
            h_section
        );
    }

    // ========== OUTPUT FORMAT ==========
    {
        let (stdout, _stderr, success) = run_cflow(&["tests/cflow/test.c"]);

        assert!(success);

        // Check POSIX-style format: "num name: type(), <file line>"
        let first_line = stdout.lines().next().unwrap_or("");

        // Should start with a line number
        let first_char = first_line.chars().next().unwrap_or(' ');
        assert!(
            first_char.is_ascii_digit(),
            "First line should start with a number: {}",
            first_line
        );

        // Should contain file reference
        assert!(
            first_line.contains('<') && first_line.contains('>'),
            "Should contain file reference in angle brackets: {}",
            first_line
        );
    }

    // ========== MULTIPLE CALLS ==========
    {
        let (stdout, _stderr, success) = run_cflow(&["tests/cflow/test.c"]);

        assert!(success);

        // f should appear in the output (called by main)
        assert!(stdout.contains("f:"), "Should contain f");
    }
}

/// cflow prints the type a declaration *spelled*, not the signedness the
/// target gives it.
///
/// Plain `char` is an unsigned type on aarch64 (AAPCS64), so a type printer
/// that asks `is_unsigned` renders `char` as `unsigned char` -- and only when
/// cflow is built for an aarch64 host, which is why nothing caught it here.
/// Pinned in both directions so the spelling cannot drift either way.
#[test]
fn tools_cflow_prints_the_declared_char_spelling() {
    use std::io::Write;

    let dir = std::env::temp_dir().join(format!("c17_cflow_char_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("mkdir");
    let src = dir.join("chars.c");
    let mut f = std::fs::File::create(&src).expect("create");
    f.write_all(
        b"char retc(void) { return 0; }\n\
          signed char rets(void) { return 0; }\n\
          unsigned char retu(void) { return 0; }\n\
          int main(void) { return retc() + rets() + retu(); }\n",
    )
    .expect("write");
    drop(f);

    let (stdout, stderr, success) = run_cflow(&[src.to_str().unwrap()]);
    let _ = std::fs::remove_dir_all(&dir);
    assert!(success, "cflow failed: {stderr}");

    // The row that would flip on an aarch64 host: a plain `char` return type
    // must render as `char`, never `unsigned char`.
    assert!(
        stdout.contains("retc: char()"),
        "plain char must print as `char`:\n{stdout}"
    );
    // Controls, so the assertion cannot pass by dropping the keyword entirely.
    assert!(
        stdout.contains("rets: signed char()"),
        "signed char keeps its spelling:\n{stdout}"
    );
    assert!(
        stdout.contains("retu: unsigned char()"),
        "unsigned char keeps its spelling:\n{stdout}"
    );
}

/// The type cflow prints is the one the declaration wrote.
///
/// cflow had its own `format_type`, a partial copy of `TypeTable::format_type`
/// that dropped everything below the top level: an array printed `int[]`
/// whatever its extent, a function printed `int()` whatever its parameters,
/// and a tagged struct printed bare `struct`. It calls the shared formatter
/// now. Recorded at #C131.
#[test]
fn tools_cflow_prints_the_whole_type() {
    let dir = plib::tmp::Builder::new()
        .prefix("cflow_types_")
        .tempdir()
        .unwrap();
    let src = dir.path().join("t.c");
    std::fs::write(
        &src,
        "struct point { int x; int y; };\n\
         int table[10];\n\
         char *name;\n\
         char **argv_copy;\n\
         struct point origin;\n\
         int matrix[4][8];\n\
         int use(void) { return table[0] + *name + **argv_copy + origin.x + matrix[0][0]; }\n",
    )
    .unwrap();

    let (stdout, stderr, ok) = run_cflow(&["-i", "x", &src.to_string_lossy()]);
    assert!(ok, "cflow failed: {}", stderr);

    for (symbol, expected) in [
        // The extent, which the copy dropped.
        ("table", "int[10]"),
        // Both dimensions, innermost last.
        ("matrix", "int[4][8]"),
        // A space before the first star and none between stars, as gcc and
        // the source write them.
        ("name", "char *"),
        ("argv_copy", "char **"),
        // The tag, without which one struct reads like any other.
        ("origin", "struct point"),
    ] {
        let line = stdout
            .lines()
            .find(|l| l.contains(&format!("{}: ", symbol)))
            .unwrap_or_else(|| panic!("no line for {} in:\n{}", symbol, stdout));
        assert!(
            line.contains(expected),
            "expected {} to be typed {:?}, got: {}",
            symbol,
            expected,
            line
        );
    }
}

// ============================================================================
// #F10 (reopened) — a `.i` operand must actually work
//
// The original fix skipped the preprocessor outright for a `.i`. That does not
// achieve "the processing already performed by c17 -E ... shall not be
// repeated" -- it breaks it. Real `-E` output is built out of `# N "file"`
// linemarkers, and the parser has no `#` of its own, so the first marker was a
// syntax error and cflow rejected every preprocessed file it was handed. The
// entry was closed [static], on the premise that preprocessing is "usually
// idempotent"; it was never run against real output.
// ============================================================================

/// A lone linemarker is enough to reproduce the original failure.
#[test]
fn cflow_accepts_a_linemarker() {
    let dir = plib::tmp::Builder::new()
        .prefix("cflow_dot_i_")
        .tempdir()
        .unwrap();
    let i = dir.path().join("tiny.i");
    std::fs::write(&i, "# 1 \"x.c\"\nint f(void){return 0;}\n").unwrap();

    let (out, err, ok) = run_cflow(&[&i.to_string_lossy()]);
    assert!(ok, "cflow rejected a linemarker: {err}");
    assert!(out.contains("f:"), "no entry for f:\n{out}");
}

/// Real `c17 -E` output must give the same answer as the source it came from.
///
/// This is the property the `.i` operand exists for, and the one thing the
/// original fix could not do.
#[test]
fn cflow_on_preprocessed_output_matches_the_source() {
    let dir = plib::tmp::Builder::new()
        .prefix("cflow_dot_i_roundtrip_")
        .tempdir()
        .unwrap();
    let c = dir.path().join("cf.c");
    std::fs::write(
        &c,
        "#include <stddef.h>\nint helper(void){return 1;}\n\
         int main(void){helper();return 0;}\n",
    )
    .unwrap();
    let i = dir.path().join("cf.i");

    let pp = Command::new(env!("CARGO_BIN_EXE_c17"))
        .args(["-E", &c.to_string_lossy(), "-o", &i.to_string_lossy()])
        .output()
        .expect("failed to run c17 -E");
    assert!(
        pp.status.success(),
        "{}",
        String::from_utf8_lossy(&pp.stderr)
    );

    let (from_c, _, ok_c) = run_cflow(&[&c.to_string_lossy()]);
    assert!(ok_c);
    let (from_i, err_i, ok_i) = run_cflow(&[&i.to_string_lossy()]);
    assert!(ok_i, "cflow rejected real -E output: {err_i}");

    // Identical, including the file each definition is attributed to: the
    // definitions came from `cf.c`, and naming `cf.i` points the reader at a
    // place the code does not live.
    assert_eq!(
        from_i, from_c,
        "a preprocessed file gave a different graph than its source"
    );
    assert!(
        !from_i.contains("cf.i"),
        "definitions attributed to the preprocessed file:\n{from_i}"
    );
}

/// A `.i` is not preprocessed again, in cflow as in c17.
#[test]
fn cflow_does_not_expand_macros_in_a_dot_i() {
    let dir = plib::tmp::Builder::new()
        .prefix("cflow_dot_i_nomacro_")
        .tempdir()
        .unwrap();
    let i = dir.path().join("m.i");
    // If the macro were expanded again, `f` would be renamed to `g`.
    std::fs::write(&i, "#define f g\nint f(void){return 0;}\n").unwrap();

    let (out, err, ok) = run_cflow(&[&i.to_string_lossy()]);
    assert!(ok, "{err}");
    assert!(
        out.contains("f:") && !out.contains("g:"),
        "the macro was expanded a second time:\n{out}"
    );
}
