//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::fuser_test;
use std::{fs::File, path::PathBuf, process::Command, str};

/// Tests the basic functionality of `fuser` by ensuring it can find the PID of a process.
///
/// **Setup:**
/// - Starts a process running `tail -f` on a temporary file.
///
/// **Assertions:**
/// - Verifies that the PID of the process is included in the output of `fuser`.
#[test]
fn test_fuser_basic() {
    fn get_temp_file_path() -> PathBuf {
        let mut path = std::env::temp_dir();

        path.push("test_file");

        path
    }
    let binding = get_temp_file_path();
    let temp_file_path = binding.to_str().unwrap();
    File::create(temp_file_path).expect("Failed to create temporary file");

    let mut process = Command::new("tail")
        .arg("-f")
        .arg(temp_file_path)
        .spawn()
        .expect("Failed to start process");

    let pid = process.id();

    fuser_test(vec![temp_file_path.to_string()], "", 0, |_, output| {
        let stdout_str = str::from_utf8(&output.stdout).expect("Invalid UTF-8 in stdout");
        let pid_str = pid.to_string();
        assert!(
            stdout_str.contains(&pid_str),
            "PID {} not found in the output.",
            pid_str
        );
    });

    process.kill().expect("Failed to kill the process");
    process.wait().expect("Failed to wait for process");
    std::fs::remove_file(temp_file_path).expect("Failed to remove temporary file");
}

/// #F1: the stdout PID list must use the POSIX `" %1d"` format — exactly one
/// leading space per PID (the old code emitted two spaces for <=4-digit PIDs).
#[test]
fn test_fuser_pid_format_single_space() {
    let temp_file_path = {
        let mut path = std::env::temp_dir();
        path.push("test_file_fmt");
        path
    };
    let temp_file_path = temp_file_path.to_str().unwrap();
    File::create(temp_file_path).expect("Failed to create temporary file");

    let mut process = Command::new("tail")
        .arg("-f")
        .arg(temp_file_path)
        .spawn()
        .expect("Failed to start process");
    std::thread::sleep(std::time::Duration::from_millis(200));

    fuser_test(vec![temp_file_path.to_string()], "", 0, |_, output| {
        let stdout = str::from_utf8(&output.stdout).expect("Invalid UTF-8 in stdout");
        // Format: each PID is preceded by exactly one space; no double spaces.
        assert!(
            !stdout.is_empty(),
            "expected at least one PID on stdout, got empty"
        );
        assert!(
            !stdout.contains("  "),
            "PID output must not contain double spaces (POSIX \" %1d\"): {:?}",
            stdout
        );
        for field in stdout.split_whitespace() {
            assert!(
                field.chars().all(|c| c.is_ascii_digit()),
                "non-numeric token {:?} in PID output {:?}",
                field,
                stdout
            );
        }
        // The raw stream must start with a single leading space.
        assert!(
            stdout.starts_with(' ') && !stdout.starts_with("  "),
            "PID output must start with exactly one leading space: {:?}",
            stdout
        );
    });

    process.kill().expect("Failed to kill the process");
    process.wait().expect("Failed to wait for process");
    std::fs::remove_file(temp_file_path).expect("Failed to remove temporary file");
}
