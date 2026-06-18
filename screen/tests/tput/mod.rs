//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::Command;

fn run_tput(args: &[&str]) -> (i32, String, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_tput"))
        .args(args)
        .output()
        .expect("Failed to execute tput");

    let exit_code = output.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    (exit_code, stdout, stderr)
}

#[test]
fn test_tput_invalid_operand() {
    // POSIX exit code 4 for invalid operand
    let (exit_code, _stdout, stderr) = run_tput(&["invalidoperand12345"]);
    assert_eq!(exit_code, 4, "Expected exit code 4 for invalid operand");
    assert!(
        stderr.contains("Invalid operand"),
        "Expected error message about invalid operand"
    );
}

#[test]
fn test_tput_invalid_terminal_type() {
    // POSIX exit code 3 for no terminal info
    let (exit_code, _stdout, stderr) = run_tput(&["-T", "nonexistent_terminal_type_xyz", "clear"]);
    assert_eq!(
        exit_code, 3,
        "Expected exit code 3 for unknown terminal type"
    );
    assert!(
        stderr.contains("terminal type") || stderr.contains("Unknown"),
        "Expected error message about terminal type"
    );
}

#[test]
fn test_tput_no_operand() {
    // POSIX exit code 2 for usage error (missing required operand)
    let (exit_code, _stdout, _stderr) = run_tput(&[]);
    assert_eq!(exit_code, 2, "Expected exit code 2 for missing operand");
}

// tput writes to stdout even when it is not a terminal (it is commonly used in
// command substitution, e.g. `clear=$(tput clear)`), so these run with a piped
// stdout against a known terminal type.

#[test]
fn test_tput_clear_xterm() {
    let (exit_code, stdout, _stderr) = run_tput(&["-T", "xterm", "clear"]);
    assert_eq!(exit_code, 0, "tput -T xterm clear should exit 0");
    assert!(
        !stdout.is_empty(),
        "tput clear should emit the xterm clear sequence"
    );
}

// Audit #T3: a valid operand runs even when a later operand is invalid; the
// invalid operand still yields exit 4.
#[test]
fn test_tput_valid_then_invalid_operand() {
    let (exit_code, stdout, stderr) = run_tput(&["-T", "xterm", "clear", "bogus"]);
    assert_eq!(
        exit_code, 4,
        "an invalid operand should yield exit 4 even after a valid one"
    );
    assert!(
        !stdout.is_empty(),
        "the leading 'clear' operand should have produced output before the invalid one"
    );
    assert!(stderr.contains("Invalid operand"));
}

#[test]
fn test_tput_multiple_valid_operands() {
    let (exit_code, _stdout, _stderr) = run_tput(&["-T", "xterm", "clear", "init"]);
    assert_eq!(exit_code, 0, "clear + init should both succeed on xterm");
}
