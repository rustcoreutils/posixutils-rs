//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{get_binary_path, run_test_with_checker, TestPlan};
use std::fs;
use std::process::Output;

fn uustat_test_with_checker<F: FnMut(&TestPlan, &Output)>(args: &[&str], checker: F) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("uustat"),
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
fn test_uustat_no_jobs() {
    // With empty/nonexistent spool, uustat should succeed with no output
    uustat_test_with_checker(&[], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uustat_queue_summary_empty() {
    // -q with empty spool should succeed
    uustat_test_with_checker(&["-q"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uustat_kill_nonexistent() {
    // Trying to kill a non-existent job should fail
    uustat_test_with_checker(&["-k", "nonexistent123"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("not found"),
            "Expected 'not found' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uustat_rejuvenate_nonexistent() {
    // Trying to rejuvenate a non-existent job should fail
    uustat_test_with_checker(&["-r", "nonexistent123"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("not found"),
            "Expected 'not found' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uustat_invalid_option() {
    // Invalid option should fail
    uustat_test_with_checker(&["-X"], |_, output| {
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
fn test_uustat_conflicting_options() {
    // -q and -k together should fail (mutually exclusive)
    uustat_test_with_checker(&["-q", "-k", "somejob"], |_, output| {
        assert!(!output.status.success());
    });
}

#[test]
fn test_uustat_filter_by_system() {
    // -s with no jobs should succeed with no output
    uustat_test_with_checker(&["-s", "somehost"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uustat_filter_by_user() {
    // -u with no jobs should succeed with no output
    uustat_test_with_checker(&["-u", "someuser"], |_, output| {
        assert!(output.status.success());
    });
}

/// Helper to create a test spool environment and run tests within it
struct SpoolEnv {
    spool_dir: std::path::PathBuf,
    test_dir: std::path::PathBuf,
}

impl SpoolEnv {
    fn new(test_name: &str) -> Self {
        let base = std::path::PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(test_name);
        let spool_dir = base.join("spool");
        let test_dir = base.join("data");

        // Clean up any previous run
        let _ = fs::remove_dir_all(&base);

        fs::create_dir_all(&spool_dir).expect("Failed to create spool dir");
        fs::create_dir_all(&test_dir).expect("Failed to create test dir");

        SpoolEnv {
            spool_dir,
            test_dir,
        }
    }

    fn run_uucp(&self, args: &[&str]) -> std::process::Output {
        let test_bin_path = get_binary_path("uucp");

        std::process::Command::new(test_bin_path)
            .args(args)
            .env("UUCP_SPOOL", &self.spool_dir)
            .current_dir(&self.test_dir)
            .output()
            .expect("Failed to run uucp")
    }

    fn run_uustat(&self, args: &[&str]) -> std::process::Output {
        let test_bin_path = get_binary_path("uustat");

        std::process::Command::new(test_bin_path)
            .args(args)
            .env("UUCP_SPOOL", &self.spool_dir)
            .current_dir(&self.test_dir)
            .output()
            .expect("Failed to run uustat")
    }

    fn create_file(&self, name: &str, content: &str) -> std::path::PathBuf {
        let path = self.test_dir.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("Failed to create parent dir");
        }
        fs::write(&path, content).expect("Failed to write test file");
        path
    }
}

impl Drop for SpoolEnv {
    fn drop(&mut self) {
        // Clean up test directories
        if let Some(parent) = self.spool_dir.parent() {
            let _ = fs::remove_dir_all(parent);
        }
    }
}

fn stdout_str(output: &std::process::Output) -> String {
    String::from_utf8_lossy(&output.stdout).to_string()
}

fn stderr_str(output: &std::process::Output) -> String {
    String::from_utf8_lossy(&output.stderr).to_string()
}

#[test]
fn test_uustat_with_queued_job() {
    let env = SpoolEnv::new("uustat_with_job");

    // Create a test file and queue a job
    env.create_file("testfile.txt", "test content");

    // Queue a job using uucp -r
    let output = env.run_uucp(&["-j", "-r", "testfile.txt", "testhost!~/dest.txt"]);
    assert!(
        output.status.success(),
        "uucp failed: {}",
        stderr_str(&output)
    );

    let job_id = stdout_str(&output).trim().to_string();
    assert!(!job_id.is_empty(), "Job ID should be printed");

    // Now uustat should show the job
    let output = env.run_uustat(&[]);
    assert!(output.status.success());
    let stdout = stdout_str(&output);
    assert!(
        stdout.contains(&job_id),
        "Job ID should appear in uustat output"
    );
    assert!(stdout.contains("testhost"), "System name should appear");

    // Queue summary should show 1 job
    let output = env.run_uustat(&["-q"]);
    assert!(output.status.success());
    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("testhost"),
        "System should appear in queue summary"
    );
    assert!(stdout.contains("1"), "Should show 1 job");
}

#[test]
fn test_uustat_kill_job() {
    let env = SpoolEnv::new("uustat_kill_job");

    // Create and queue a job
    env.create_file("testfile.txt", "test content");
    let output = env.run_uucp(&["-j", "-r", "testfile.txt", "killhost!~/dest.txt"]);
    assert!(output.status.success());

    let job_id = stdout_str(&output).trim().to_string();

    // Verify job exists
    let output = env.run_uustat(&["-q"]);
    assert!(stdout_str(&output).contains("killhost"));

    // Kill the job
    let output = env.run_uustat(&["-k", &job_id]);
    assert!(output.status.success());

    // Verify job is gone
    let output = env.run_uustat(&["-q"]);
    assert!(output.status.success());
    assert!(stdout_str(&output).is_empty() || !stdout_str(&output).contains("killhost"));
}

#[test]
fn test_uustat_rejuvenate_job() {
    let env = SpoolEnv::new("uustat_rejuv_job");

    // Create and queue a job
    env.create_file("testfile.txt", "test content");
    let output = env.run_uucp(&["-j", "-r", "testfile.txt", "rejuvhost!~/dest.txt"]);
    assert!(output.status.success());

    let job_id = stdout_str(&output).trim().to_string();

    // Rejuvenate the job (should succeed)
    let output = env.run_uustat(&["-r", &job_id]);
    assert!(output.status.success());

    // Job should still exist
    let output = env.run_uustat(&["-q"]);
    assert!(stdout_str(&output).contains("rejuvhost"));
}
