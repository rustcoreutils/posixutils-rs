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

fn run(bin: &str, args: &[&str]) -> (String, String, i32) {
    let exe = match bin {
        "cflow" => env!("CARGO_BIN_EXE_cflow"),
        "ctags" => env!("CARGO_BIN_EXE_ctags"),
        "cxref" => env!("CARGO_BIN_EXE_cxref"),
        _ => unreachable!(),
    };
    let output = Command::new(exe)
        .args(args)
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
