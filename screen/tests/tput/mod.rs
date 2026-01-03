//
// Copyright (c) 2024 Jeff Garzik
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

// Note: Testing clear/init/reset requires a terminal and produces
// terminal-specific escape sequences. These are verified to work
// through manual testing. The terminfo database lookup and capability
// expansion is handled by the terminfo crate.
