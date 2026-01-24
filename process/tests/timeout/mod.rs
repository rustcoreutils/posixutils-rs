//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    io::Write,
    process::{Command, Output, Stdio},
    thread,
    time::Duration,
};

use plib::testing::get_binary_path;
use sysinfo::System;

pub struct TestPlan {
    pub cmd: String,
    pub args: Vec<String>,
    pub stdin_data: String,
    pub expected_out: String,
    pub expected_err: String,
    pub expected_exit_code: Option<i32>,
    pub has_subprocesses: bool,
}

fn run_test_base(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> (Output, i32) {
    let test_bin_path = get_binary_path(cmd);

    let mut command = Command::new(test_bin_path);
    let mut child = command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|_| panic!("failed to spawn command {}", cmd));

    let pgid = unsafe { libc::getpgid(child.id() as i32) };

    // Separate the mutable borrow of stdin from the child process
    if let Some(mut stdin) = child.stdin.take() {
        let chunk_size = 1024; // Arbitrary chunk size, adjust if needed
        for chunk in stdin_data.chunks(chunk_size) {
            // Write each chunk
            if let Err(e) = stdin.write_all(chunk) {
                eprintln!("Error writing to stdin: {}", e);
                break;
            }
            // Flush after writing each chunk
            if let Err(e) = stdin.flush() {
                eprintln!("Error flushing stdin: {}", e);
                break;
            }

            // Sleep briefly to avoid CPU spinning
            thread::sleep(Duration::from_millis(10));
        }
        // Explicitly drop stdin to close the pipe
        drop(stdin);
    }

    // Ensure we wait for the process to complete after writing to stdin
    let output = child.wait_with_output().expect("failed to wait for child");
    (output, pgid)
}

pub fn run_test(plan: TestPlan) {
    let (output, pgid) = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, plan.expected_out);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(stderr, plan.expected_err);

    assert_eq!(output.status.code(), plan.expected_exit_code);
    if let Some(0) = plan.expected_exit_code {
        assert!(output.status.success());
    }

    let mut system = System::new_all();
    system.refresh_all();
    for process in system.processes().values() {
        if let Some(gid) = process.group_id() {
            if *gid == pgid as u32 {
                assert!(plan.has_subprocesses)
            }
        }
    }
}

fn timeout_test(args: &[&str], expected_err: &str, expected_exit_code: i32) {
    run_test(TestPlan {
        cmd: String::from("timeout"),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(expected_err),
        expected_exit_code: Some(expected_exit_code),
        has_subprocesses: false,
    });
}

fn timeout_test_extended(
    args: &[&str],
    expected_err: &str,
    expected_exit_code: Option<i32>,
    has_subprocesses: bool,
) {
    run_test(TestPlan {
        cmd: String::from("timeout"),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(expected_err),
        expected_exit_code,
        has_subprocesses,
    });
}

const TRUE: &str = "true";
const SLEEP: &str = "sleep";
const NON_EXECUTABLE: &str = "tests/timeout/non_executable.sh";
const WITH_ARGUMENT: &str = "tests/timeout/with_argument.sh";
const SPAWN_CHILD: &str = "tests/timeout/spawn_child.sh";

#[test]
fn test_absent_duration() {
    timeout_test(&[TRUE], "timeout: invalid duration format 'true'\n", 125);
}

#[test]
fn test_absent_utility() {
    timeout_test(
        &["5"],
        "timeout: one or more required arguments were not provided\n",
        125,
    );
}

#[test]
fn test_signal_parsing_invalid() {
    timeout_test(
        &["-s", "MY_SIGNAL", "1", TRUE],
        "timeout: invalid signal name 'MY_SIGNAL'\n",
        125,
    );
}

#[test]
fn test_signal_parsing_uppercase() {
    timeout_test(&["-s", "TERM", "1", TRUE], "", 0);
    timeout_test(&["-s", "KILL", "1", TRUE], "", 0);
    timeout_test(&["-s", "CONT", "1", TRUE], "", 0);
    timeout_test(&["-s", "STOP", "1", TRUE], "", 0);
}

#[test]
fn test_signal_parsing_lowercase() {
    timeout_test(&["-s", "term", "1", TRUE], "", 0);
    timeout_test(&["-s", "kill", "1", TRUE], "", 0);
    timeout_test(&["-s", "cont", "1", TRUE], "", 0);
    timeout_test(&["-s", "stop", "1", TRUE], "", 0);
}

#[test]
fn test_signal_parsing_uppercase_with_prefix() {
    timeout_test(&["-s", "SIGTERM", "1", TRUE], "", 0);
    timeout_test(&["-s", "SIGKILL", "1", TRUE], "", 0);
    timeout_test(&["-s", "SIGCONT", "1", TRUE], "", 0);
    timeout_test(&["-s", "SIGSTOP", "1", TRUE], "", 0);
}

#[test]
fn test_signal_parsing_lowercase_with_prefix() {
    timeout_test(&["-s", "sigterm", "1", TRUE], "", 0);
    timeout_test(&["-s", "sigkill", "1", TRUE], "", 0);
    timeout_test(&["-s", "sigcont", "1", TRUE], "", 0);
    timeout_test(&["-s", "sigstop", "1", TRUE], "", 0);
}

#[test]
fn test_multiple_signals() {
    timeout_test(
        &["-s", "TERM", "-s", "KILL", "1", TRUE],
        "timeout: an argument cannot be used with one or more of the other specified arguments\n",
        125,
    );
}

#[test]
fn test_invalid_duration_negative() {
    // "-1" is considered as argument, not a value
    timeout_test(&["-1", TRUE], "timeout: unexpected argument found\n", 125);
}

#[test]
fn test_invalid_duration_empty_float() {
    timeout_test(&[".", TRUE], "timeout: invalid duration format '.'\n", 125);
}

#[test]
fn test_invalid_duration_format_invalid_suffix() {
    timeout_test(
        &["1a", TRUE],
        "timeout: invalid duration format '1a'\n",
        125,
    );
}

#[test]
fn test_invalid_duration_only_suffixes() {
    timeout_test(&["s", TRUE], "timeout: invalid duration format 's'\n", 125);
    timeout_test(&["m", TRUE], "timeout: invalid duration format 'm'\n", 125);
    timeout_test(&["h", TRUE], "timeout: invalid duration format 'h'\n", 125);
    timeout_test(&["d", TRUE], "timeout: invalid duration format 'd'\n", 125);
}

#[test]
fn test_valid_duration_parsing_with_suffixes() {
    timeout_test(&["1.1s", TRUE], "", 0);
    timeout_test(&["1.1m", TRUE], "", 0);
    timeout_test(&["1.1h", TRUE], "", 0);
    timeout_test(&["1.1d", TRUE], "", 0);
}

#[test]
fn test_utility_cound_not_execute() {
    timeout_test(
        &["1", NON_EXECUTABLE],
        "timeout: unable to run the utility 'tests/timeout/non_executable.sh'\n",
        126,
    );
}

#[test]
fn test_utility_not_found() {
    timeout_test(
        &["1", "inexistent_utility"],
        "timeout: utility 'inexistent_utility' not found\n",
        127,
    );
}

#[test]
fn test_utility_error() {
    timeout_test(&["1", WITH_ARGUMENT], "error: enter some argument\n", 1);
}

#[test]
fn test_basic() {
    timeout_test(&["2", SLEEP, "1"], "", 0);
}

#[test]
fn test_send_kill() {
    timeout_test_extended(&["-s", "KILL", "1", SLEEP, "2"], "", None, false);
}

#[test]
fn test_zero_duration() {
    timeout_test(&["0", SLEEP, "2"], "", 0);
}

#[test]
fn test_timeout_reached() {
    timeout_test(&["1", SLEEP, "2"], "", 124);
}

#[test]
fn test_preserve_status_wait() {
    timeout_test(&["-p", "2", SLEEP, "1"], "", 0);
}

#[test]
fn test_preserve_status_with_sigterm() {
    // 143 = 128 + 15 (SIGTERM after first timeout)
    timeout_test(&["-p", "1", SLEEP, "2"], "", 143);
}

#[test]
fn test_preserve_status_sigcont_with_sigkill() {
    // 137 = 128 + 9 (SIGKILL after second timeout)
    timeout_test_extended(
        &["-p", "-s", "CONT", "-k", "1", "1", SLEEP, "3"],
        "",
        None,
        false,
    );
}

#[test]
fn test_preserve_status_cont() {
    // First duration is 0, so sending SIGCONT and second timeout won't happen
    timeout_test(&["-p", "-s", "CONT", "-k", "1", "0", SLEEP, "3"], "", 0);
}

#[test]
fn test_not_foreground_timeout() {
    timeout_test_extended(&["1", SPAWN_CHILD], "", Some(124), true);
}

#[test]
fn test_foreground_timeout() {
    timeout_test_extended(&["-f", "1", SPAWN_CHILD], "", Some(124), false);
}

#[test]
fn test_not_foreground_ok() {
    timeout_test_extended(&["5", SPAWN_CHILD], "", Some(0), true);
}

#[test]
fn test_foreground_ok() {
    timeout_test_extended(&["-f", "5", SPAWN_CHILD], "", Some(0), false);
}
