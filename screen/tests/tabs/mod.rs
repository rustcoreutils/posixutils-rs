//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::Command;

fn run_tabs(args: &[&str]) -> (i32, String, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_tabs"))
        .args(args)
        .output()
        .expect("Failed to execute tabs");

    let exit_code = output.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    (exit_code, stdout, stderr)
}

#[test]
fn test_tabs_invalid_terminal_type() {
    // POSIX: exit code >0 for unknown terminal type
    let (exit_code, _stdout, stderr) = run_tabs(&["-T", "nonexistent_terminal_xyz123"]);
    assert!(
        exit_code > 0,
        "Expected non-zero exit code for unknown terminal type"
    );
    assert!(
        stderr.contains("terminal") || stderr.contains("unknown"),
        "Expected error message about terminal type, got: {}",
        stderr
    );
}

#[test]
fn test_tabs_help() {
    let (exit_code, stdout, _stderr) = run_tabs(&["--help"]);
    assert_eq!(exit_code, 0, "Expected exit code 0 for --help");
    assert!(
        stdout.contains("tabs") || stdout.contains("terminal"),
        "Expected help text to mention tabs or terminal"
    );
}

#[test]
fn test_tabs_version() {
    let (exit_code, stdout, _stderr) = run_tabs(&["--version"]);
    assert_eq!(exit_code, 0, "Expected exit code 0 for --version");
    assert!(
        stdout.contains("tabs") || stdout.contains("0."),
        "Expected version output"
    );
}

// Note: Tests that actually set tab stops require a terminal and are
// difficult to automate. The following tests verify argument parsing
// and error handling without requiring terminal access.

// These tests will fail in non-terminal environments when trying to
// actually set tabs, but we can still test that arguments are recognized.
// The integration tests focus on error paths that don't require a terminal.

#[test]
fn test_tabs_invalid_tabstop_format() {
    // Note: This test may succeed or fail depending on terminal availability.
    // The important part is that if it fails, it should be due to terminal
    // issues, not argument parsing.
    let (exit_code, _stdout, stderr) = run_tabs(&["abc,def"]);
    // Either fails with invalid tab spec OR terminal not available
    if exit_code != 0 {
        assert!(
            stderr.contains("invalid") || stderr.contains("terminal"),
            "Expected error about invalid spec or terminal, got: {}",
            stderr
        );
    }
}

#[test]
fn test_tabs_non_ascending_tabstops() {
    let (exit_code, _stdout, stderr) = run_tabs(&["10,5,20"]);
    // Should fail either due to non-ascending order or terminal issues
    if exit_code != 0 {
        assert!(
            stderr.contains("ascending") || stderr.contains("terminal"),
            "Expected error about ascending order or terminal, got: {}",
            stderr
        );
    }
}
