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

/// Block until `pid` has `path` open, or a short timeout elapses.
///
/// Polls `/proc/<pid>/fd` on Linux; elsewhere it just yields briefly, since the
/// tests that need this are Linux-gated anyway.
fn wait_for_open_fd(pid: u32, path: &str) {
    use std::time::Duration;

    #[cfg(target_os = "linux")]
    {
        use std::time::Instant;
        let deadline = Instant::now() + Duration::from_secs(5);
        while Instant::now() < deadline {
            let fd_dir = format!("/proc/{}/fd", pid);
            if let Ok(entries) = std::fs::read_dir(&fd_dir) {
                for e in entries.flatten() {
                    if let Ok(target) = std::fs::read_link(e.path()) {
                        if target.to_string_lossy() == path {
                            return;
                        }
                    }
                }
            }
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    // No /proc to poll, and the tests that depend on this are Linux-gated, so
    // just yield long enough for the child to get its file open. Kept outside
    // a loop: with nothing to re-check, iterating would only sleep again.
    #[cfg(not(target_os = "linux"))]
    {
        let _ = (pid, path);
        std::thread::sleep(Duration::from_millis(200));
    }
}

/// Tests the basic functionality of `fuser` by ensuring it can find the PID of a process.
///
/// **Setup:**
/// - Starts a process running `tail -f` on a temporary file.
///
/// **Assertions:**
/// - Verifies that the PID of the process is included in the output of `fuser`.
#[test]
fn test_fuser_basic() {
    // Unique per run: the path used to be a fixed `$TMPDIR/test_file`, shared
    // with the sibling tests that cargo runs in parallel, so one test could
    // delete the file another was still using.
    fn get_temp_file_path() -> PathBuf {
        let mut path = std::env::temp_dir();
        path.push(format!("fuser_basic_test_{}", std::process::id()));
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

    // `tail` is spawned asynchronously and has not necessarily opened the file
    // by the time fuser scans /proc. Waiting for the descriptor to appear
    // removes the race that made this test fail intermittently in CI; the
    // sibling format test already sleeps for the same reason.
    wait_for_open_fd(pid, temp_file_path);

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
        path.push(format!("fuser_fmt_test_{}", std::process::id()));
        path
    };
    let temp_file_path = temp_file_path.to_str().unwrap();
    File::create(temp_file_path).expect("Failed to create temporary file");

    let mut process = Command::new("tail")
        .arg("-f")
        .arg(temp_file_path)
        .spawn()
        .expect("Failed to start process");
    wait_for_open_fd(process.id(), temp_file_path);

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
