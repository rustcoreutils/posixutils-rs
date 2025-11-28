//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::fs;
use std::process::Output;

fn uux_test_with_checker<F: FnMut(&TestPlan, &Output)>(args: &[&str], checker: F) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("uux"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        checker,
    );
}

#[test]
fn test_uux_no_args() {
    // No arguments should show usage and fail
    uux_test_with_checker(&[], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("Usage:") || stderr.contains("usage:"),
            "Expected usage message in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_invalid_option() {
    // Invalid option should fail
    uux_test_with_checker(&["-X", "echo hello"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("unexpected argument") || stderr.contains("unknown option"),
            "Expected error about invalid option in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_local_command() {
    // Run a simple local command (no system prefix = local)
    uux_test_with_checker(&["echo hello"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_local_command_with_prefix() {
    // Explicit local system with ! prefix
    uux_test_with_checker(&["!echo hello"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_with_j_option() {
    // -j should print job ID
    uux_test_with_checker(&["-j", "echo test"], |_, output| {
        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(!stdout.trim().is_empty(), "Should print job ID with -j");
    });
}

#[test]
fn test_uux_disallowed_append_redirect() {
    // >> is not allowed
    uux_test_with_checker(&["echo hello >> /tmp/file"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains(">>"),
            "Expected '>>' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_disallowed_heredoc() {
    // << is not allowed
    uux_test_with_checker(&["cat << EOF"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("<<"),
            "Expected '<<' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_disallowed_clobber() {
    // >| is not allowed
    uux_test_with_checker(&["echo test >| /tmp/file"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains(">|"),
            "Expected '>|' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_disallowed_redirect_both() {
    // >& is not allowed
    uux_test_with_checker(&["command >& /tmp/file"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains(">&"),
            "Expected '>&' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_pipe_stdin() {
    // -p should be accepted
    uux_test_with_checker(&["-p", "cat"], |_, output| {
        // This will succeed since stdin is empty
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_n_option_accepted() {
    // -n should be accepted (suppress notification)
    uux_test_with_checker(&["-n", "echo test"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_combined_options() {
    // Combined options should work
    uux_test_with_checker(&["-jn", "echo combined"], |_, output| {
        assert!(output.status.success());
        // Should print job ID
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(!stdout.trim().is_empty());
    });
}

#[test]
fn test_uux_dash_as_p() {
    // Single dash is equivalent to -p
    uux_test_with_checker(&["-", "cat"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_command_with_args() {
    // Command with multiple arguments
    uux_test_with_checker(&["echo one two three"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_command_exit_status() {
    // Command that succeeds
    uux_test_with_checker(&["true"], |_, output| {
        assert!(output.status.success());
    });

    // Command that fails
    uux_test_with_checker(&["false"], |_, output| {
        assert!(!output.status.success());
    });
}

#[test]
fn test_uux_simple_redirect() {
    let test_dir = &format!("{}/test_uux_redirect", env!("CARGO_TARGET_TMPDIR"));
    let output_file = &format!("{}/output.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();

    // Simple > redirect should be allowed
    let cmd = format!("echo redirected > {}", output_file);
    uux_test_with_checker(&[&cmd], |_, output| {
        assert!(output.status.success());
        // Verify file was created
        assert!(std::path::Path::new(output_file).exists());
        let content = fs::read_to_string(output_file).unwrap();
        assert!(content.contains("redirected"));
    });

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uux_too_many_args() {
    // Only one command string should be accepted
    uux_test_with_checker(&["cmd1", "cmd2"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("too many"),
            "Expected 'too many' in stderr, got: {}",
            stderr
        );
    });
}
