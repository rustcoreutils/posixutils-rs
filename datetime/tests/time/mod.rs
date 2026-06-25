//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::Output;

use plib::testing::{run_test_base, TestPlan};

fn get_output(plan: TestPlan) -> Output {
    run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes())
}

fn run_test_time(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    let output = get_output(TestPlan {
        cmd: String::from("time"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });

    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(stderr.contains(expected_error));
}

#[test]
fn simple_test() {
    run_test_time(&["--", "ls", "-l"], "", "User time", 0);
}

#[test]
fn p_test() {
    run_test_time(&["-p", "--", "ls", "-l"], "", "user", 0);
}

#[test]
fn parse_error_test() {
    run_test_time(&[], "", "not provided", 0);
}

#[test]
fn command_error_test() {
    run_test_time(&["-s", "ls", "-l"], "", "unexpected argument '-s' found", 0);
}

/// Parse the `user`/`sys` seconds out of `time -p` output on stderr.
fn parse_p_user_sys(stderr: &str) -> (f64, f64) {
    let mut user = None;
    let mut sys = None;
    for line in stderr.lines() {
        if let Some(rest) = line.strip_prefix("user ") {
            user = rest.trim().parse::<f64>().ok();
        } else if let Some(rest) = line.strip_prefix("sys ") {
            sys = rest.trim().parse::<f64>().ok();
        }
    }
    (
        user.expect("missing `user` line"),
        sys.expect("missing `sys` line"),
    )
}

// Regression for #T1/#T2: a CPU-bound child must report non-zero CPU time.
// The pre-fix code never refilled tms_end and read the parent's own counters,
// so user/sys were always ~0 regardless of the child's work.
#[test]
fn cpu_bound_child_reports_nonzero_cpu_time() {
    let busy = "i=0; while [ $i -lt 3000000 ]; do i=$((i+1)); done";
    let output = get_output(TestPlan {
        cmd: String::from("time"),
        args: vec![
            String::from("-p"),
            String::from("--"),
            String::from("sh"),
            String::from("-c"),
            String::from(busy),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(output.status.success(), "time of busy child should exit 0");
    let stderr = String::from_utf8_lossy(&output.stderr);
    let (user, sys) = parse_p_user_sys(&stderr);
    assert!(
        user + sys > 0.0,
        "CPU-bound child should report non-zero user+sys CPU time, got user={user} sys={sys}\n{stderr}"
    );
}

// Regression for #T3: time must propagate the utility's exit status.
// The pre-fix code discarded child.wait() and always exited 0.
#[test]
fn propagates_child_exit_status() {
    let output = get_output(TestPlan {
        cmd: String::from("time"),
        args: vec![
            String::from("--"),
            String::from("sh"),
            String::from("-c"),
            String::from("exit 7"),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 7,
    });

    assert_eq!(
        output.status.code(),
        Some(7),
        "time should exit with the utility's exit status"
    );
    // Timing statistics are still written even when the utility fails.
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("User time"),
        "timing stats missing: {stderr}"
    );
}
