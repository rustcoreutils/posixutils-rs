//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::Duration;

fn run_kill_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "kill".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };
    run_test_with_checker(plan, check_fn);
}

fn run_kill_test_exact(
    args: Vec<&str>,
    expected_out: &str,
    expected_err: &str,
    expected_exit_code: i32,
) {
    let plan = TestPlan {
        cmd: "kill".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: expected_out.to_string(),
        expected_err: expected_err.to_string(),
        expected_exit_code,
    };
    run_test(plan);
}

// Helper to get the test binary path
fn get_kill_binary_path() -> std::path::PathBuf {
    let target_dir = std::env::var("CARGO_TARGET_DIR")
        .or_else(|_| std::env::var("CARGO_LLVM_COV_TARGET_DIR"))
        .unwrap_or_else(|_| String::from("target"));

    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };

    std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap()
        .join(format!("{}/{}/kill", target_dir, profile))
}

// ============================================
// Signal listing tests (-l)
// ============================================

#[test]
fn test_list_signals() {
    run_kill_test(vec!["-l"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should contain common signal names
        assert!(stdout.contains("HUP"), "Expected HUP in signal list");
        assert!(stdout.contains("INT"), "Expected INT in signal list");
        assert!(stdout.contains("KILL"), "Expected KILL in signal list");
        assert!(stdout.contains("TERM"), "Expected TERM in signal list");
        // Should end with newline only (no trailing space)
        assert!(stdout.ends_with('\n'), "Output should end with newline");
        let trimmed = stdout.trim_end_matches('\n');
        assert!(
            !trimmed.ends_with(' '),
            "Should not have trailing space before newline"
        );
    });
}

#[test]
fn test_list_signal_by_number_direct() {
    // kill -l 15 should output TERM
    run_kill_test_exact(vec!["-l", "15"], "TERM\n", "", 0);
}

#[test]
fn test_list_signal_by_number_sigterm() {
    run_kill_test_exact(vec!["-l", "9"], "KILL\n", "", 0);
}

#[test]
fn test_list_signal_by_number_sighup() {
    run_kill_test_exact(vec!["-l", "1"], "HUP\n", "", 0);
}

#[test]
fn test_list_signal_shell_exit_status() {
    // Shell exit status for signaled process is 128 + signal_number
    // kill -l 143 should output TERM (143 - 128 = 15 = SIGTERM)
    run_kill_test_exact(vec!["-l", "143"], "TERM\n", "", 0);
}

#[test]
fn test_list_signal_shell_exit_status_sighup() {
    // 129 - 128 = 1 = SIGHUP
    run_kill_test_exact(vec!["-l", "129"], "HUP\n", "", 0);
}

#[test]
fn test_list_signal_shell_exit_status_sigkill() {
    // 137 - 128 = 9 = SIGKILL
    run_kill_test_exact(vec!["-l", "137"], "KILL\n", "", 0);
}

#[test]
fn test_list_signal_invalid_number() {
    run_kill_test(vec!["-l", "999"], 1, |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("invalid signal number"),
            "Expected invalid signal error"
        );
    });
}

// ============================================
// Signal specification tests
// ============================================

#[test]
fn test_signal_zero_check_self() {
    // kill -0 $$ should succeed for our own process
    let pid = std::process::id().to_string();
    run_kill_test_exact(vec!["-0", &pid], "", "", 0);
}

#[test]
fn test_signal_zero_with_s_flag() {
    let pid = std::process::id().to_string();
    run_kill_test_exact(vec!["-s", "0", &pid], "", "", 0);
}

#[test]
fn test_signal_by_name_term() {
    // Just test parsing - send signal 0 which doesn't actually terminate
    let pid = std::process::id().to_string();
    run_kill_test_exact(vec!["-s", "TERM", "-0", &pid], "", "", 0);
}

#[test]
fn test_signal_with_sig_prefix() {
    // -s SIGTERM should work (SIG prefix is optional)
    let pid = std::process::id().to_string();
    run_kill_test(vec!["-s", "SIGTERM", "-0", &pid], 0, |_, _| {});
}

#[test]
fn test_signal_case_insensitive() {
    // -s term should work (case insensitive)
    let pid = std::process::id().to_string();
    run_kill_test(vec!["-s", "term", "-0", &pid], 0, |_, _| {});
}

#[test]
fn test_signal_invalid_name() {
    run_kill_test(vec!["-s", "INVALID", "1"], 1, |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !stderr.is_empty(),
            "Expected error message for invalid signal name"
        );
    });
}

// ============================================
// Error handling tests
// ============================================

#[test]
fn test_no_pids_error() {
    // kill (with no PIDs) should error
    run_kill_test(vec![], 1, |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("No process ID"),
            "Expected 'No process ID' error"
        );
    });
}

#[test]
fn test_nonexistent_pid() {
    // Use a very high PID that likely doesn't exist
    run_kill_test(vec!["-0", "999999999"], 1, |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Should have an error message about the process
        assert!(!stderr.is_empty(), "Expected error for nonexistent PID");
    });
}

#[test]
fn test_invalid_pid() {
    run_kill_test(vec!["notapid"], 1, |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!stderr.is_empty(), "Expected error for invalid PID");
    });
}

// ============================================
// Option separator tests
// ============================================

#[test]
fn test_double_dash_separator() {
    let pid = std::process::id().to_string();
    run_kill_test_exact(vec!["-0", "--", &pid], "", "", 0);
}

// ============================================
// Signal delivery tests (spawn child, send signal)
// ============================================

#[test]
fn test_send_sigterm_to_child() {
    // Spawn a sleep process, send SIGTERM, verify it terminates
    let mut child = Command::new("sleep")
        .arg("60")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("Failed to spawn sleep process");

    let child_pid = child.id().to_string();

    // Give the child a moment to start
    thread::sleep(Duration::from_millis(50));

    // Use our kill utility to send SIGTERM
    let kill_path = get_kill_binary_path();
    let output = Command::new(&kill_path)
        .args(["-TERM", &child_pid])
        .output()
        .expect("Failed to run kill");

    assert!(output.status.success(), "kill command should succeed");

    // Wait for child to terminate
    let status = child.wait().expect("Failed to wait for child");

    // On Unix, check that the process was terminated by a signal
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        assert!(
            status.signal().is_some(),
            "Process should be killed by signal"
        );
    }
}

#[test]
fn test_send_sigkill_to_child() {
    let mut child = Command::new("sleep")
        .arg("60")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("Failed to spawn sleep process");

    let child_pid = child.id().to_string();
    thread::sleep(Duration::from_millis(50));

    let kill_path = get_kill_binary_path();
    let output = Command::new(&kill_path)
        .args(["-9", &child_pid])
        .output()
        .expect("Failed to run kill");

    assert!(output.status.success(), "kill -9 should succeed");

    let status = child.wait().expect("Failed to wait for child");

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        assert_eq!(
            status.signal(),
            Some(9),
            "Process should be killed by SIGKILL (9)"
        );
    }
}

#[test]
fn test_send_signal_with_s_flag() {
    let mut child = Command::new("sleep")
        .arg("60")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("Failed to spawn sleep process");

    let child_pid = child.id().to_string();
    thread::sleep(Duration::from_millis(50));

    let kill_path = get_kill_binary_path();
    let output = Command::new(&kill_path)
        .args(["-s", "KILL", &child_pid])
        .output()
        .expect("Failed to run kill");

    assert!(output.status.success(), "kill -s KILL should succeed");

    let status = child.wait().expect("Failed to wait for child");

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        assert_eq!(
            status.signal(),
            Some(9),
            "Process should be killed by SIGKILL"
        );
    }
}

// ============================================
// Multiple PIDs tests
// ============================================

#[test]
fn test_multiple_pids() {
    // Spawn two sleep processes
    let mut child1 = Command::new("sleep")
        .arg("60")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("Failed to spawn first sleep process");

    let mut child2 = Command::new("sleep")
        .arg("60")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("Failed to spawn second sleep process");

    let pid1 = child1.id().to_string();
    let pid2 = child2.id().to_string();
    thread::sleep(Duration::from_millis(50));

    // Kill both with a single command
    let kill_path = get_kill_binary_path();
    let output = Command::new(&kill_path)
        .args(["-9", &pid1, &pid2])
        .output()
        .expect("Failed to run kill");

    assert!(
        output.status.success(),
        "kill with multiple PIDs should succeed"
    );

    // Both should be terminated
    let status1 = child1.wait().expect("Failed to wait for child1");
    let status2 = child2.wait().expect("Failed to wait for child2");

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        assert_eq!(status1.signal(), Some(9), "First process should be killed");
        assert_eq!(status2.signal(), Some(9), "Second process should be killed");
    }
}

// ============================================
// Process group tests (negative PIDs)
// ============================================

#[test]
fn test_negative_pid_syntax() {
    // Test that negative PIDs are parsed correctly
    // We can't easily test actual process group killing without more setup,
    // but we can verify the syntax is accepted

    // Use signal 0 to just check if process group exists (will likely fail with ESRCH)
    run_kill_test(vec!["-0", "--", "-1"], 1, |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Should get an error (probably ESRCH or EPERM), not a parse error
        assert!(
            !stderr.contains("Invalid PID"),
            "Negative PID should be parsed correctly"
        );
    });
}
