//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::Output;

use plib::testing::{get_binary_path, run_test_base, TestPlan};

/// A utility to time: this crate's own `date`, which every platform has.
fn date_path() -> String {
    get_binary_path("date").to_string_lossy().into_owned()
}

fn get_output(plan: TestPlan) -> Output {
    run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes())
}

fn run_test_time(
    args: &[String],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let output = get_output(TestPlan {
        cmd: String::from("time"),
        args: args.to_vec(),
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
    run_test_time(&["--".into(), date_path(), "-u".into()], "", "User time", 0);
}

#[test]
fn p_test() {
    run_test_time(
        &["-p".into(), "--".into(), date_path(), "-u".into()],
        "",
        "user",
        0,
    );
}

#[test]
fn parse_error_test() {
    run_test_time(&[], "", "not provided", 0);
}

#[test]
fn command_error_test() {
    run_test_time(
        &["-s".into(), date_path(), "-u".into()],
        "",
        "unexpected argument '-s' found",
        0,
    );
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

/// Not a test of its own: the CPU-bound child of
/// `cpu_bound_child_reports_nonzero_cpu_time`, which runs this test binary
/// with `--ignored --exact`.
#[test]
#[ignore = "run as a child by cpu_bound_child_reports_nonzero_cpu_time"]
fn busy_child() {
    let start = std::time::Instant::now();
    let mut n: u64 = 0;
    while start.elapsed() < std::time::Duration::from_millis(300) {
        n = std::hint::black_box(n.wrapping_add(1));
    }
}

// Regression for #T1/#T2: a CPU-bound child must report non-zero CPU time.
// The pre-fix code never refilled tms_end and read the parent's own counters,
// so user/sys were always ~0 regardless of the child's work.
#[test]
fn cpu_bound_child_reports_nonzero_cpu_time() {
    let this_test = std::env::current_exe().unwrap();
    let output = get_output(TestPlan {
        cmd: String::from("time"),
        args: vec![
            String::from("-p"),
            String::from("--"),
            this_test.to_string_lossy().into_owned(),
            String::from("--ignored"),
            String::from("--exact"),
            String::from("time::busy_child"),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(output.status.success(), "time of busy child should exit 0");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("1 passed"),
        "the busy child did not run: {stdout}"
    );
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
        // `sleep` rejects a non-numeric operand with status 2.
        args: vec![
            String::from("--"),
            get_binary_path("sleep").to_string_lossy().into_owned(),
            String::from("abc"),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 2,
    });

    assert_eq!(
        output.status.code(),
        Some(2),
        "time should exit with the utility's exit status"
    );
    // Timing statistics are still written even when the utility fails.
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("User time"),
        "timing stats missing: {stderr}"
    );
}

// EXIT STATUS (spec 118102-118106): 127 if the utility could not be found,
// 126 if it was found but could not be invoked. The suite previously only
// exercised clap's own argument errors, so neither mapping was asserted.
#[test]
fn reports_127_when_the_utility_is_not_found() {
    let output = run_test_base("time", &["/nonexistent/utility/xyz".to_string()], b"");
    assert_eq!(output.status.code(), Some(127));
}

#[test]
fn reports_126_when_the_utility_cannot_be_invoked() {
    // A regular, non-executable file: found, but not invocable.
    let path = plib::testing::TempFile::new("noexec", b"");

    let output = run_test_base("time", &[path.to_string_lossy().into_owned()], b"");
    assert_eq!(output.status.code(), Some(126));
}

// The timed utility may take its own options. Until 2026-08-06 `time` declared
// its trailing operand list with `trailing_var_arg` but without
// `allow_hyphen_values`, so clap tried to parse the utility's first hyphenated
// argument as one of time's own and rejected it: `time ls -l` and
// `time sh -c '...'` both failed. See `#C4` in the process/ audit — env, nice
// and timeout had the identical defect. The timed utility here is `date -u`.
#[test]
fn utility_arguments_may_start_with_a_hyphen() {
    let output = run_test_base(
        "time",
        &[date_path(), "-u".to_string(), "+passed-through".to_string()],
        b"",
    );

    assert_eq!(
        output.status.code(),
        Some(0),
        "time rejected the utility's own option: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim_end(),
        "passed-through"
    );
}

// time's own `-p` must still be recognized when it precedes the utility, so
// the fix above cannot have been "stop parsing options entirely".
#[test]
fn own_p_option_still_parses_before_the_utility() {
    let output = run_test_base(
        "time",
        &[
            "-p".to_string(),
            date_path(),
            "-u".to_string(),
            "+ok".to_string(),
        ],
        b"",
    );

    assert_eq!(output.status.code(), Some(0));
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim_end(), "ok");
    // -p selects the POSIX output format: "real %f\nuser %f\nsys %f\n".
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("real ") && stderr.contains("user ") && stderr.contains("sys "),
        "-p must still select the POSIX format, got {stderr:?}"
    );
}
