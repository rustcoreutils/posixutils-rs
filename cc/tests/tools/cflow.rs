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
