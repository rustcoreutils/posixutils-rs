//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Write;
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::Duration;

pub struct TestPlan {
    pub cmd: String,
    pub args: Vec<String>,
    pub stdin_data: String,
    pub expected_out: String,
    pub expected_err: String,
    pub expected_exit_code: i32,
}

pub struct TestPlanU8 {
    pub cmd: String,
    pub args: Vec<String>,
    pub stdin_data: Vec<u8>,
    pub expected_out: Vec<u8>,
    pub expected_err: Vec<u8>,
    pub expected_exit_code: i32,
}

pub fn run_test_base(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> Output {
    let relpath = if cfg!(debug_assertions) {
        format!("target/debug/{}", cmd)
    } else {
        format!("target/release/{}", cmd)
    };
    let test_bin_path = std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap() // Move up to the workspace root from the current package directory
        .join(relpath); // Adjust the path to the binary

    let mut command = Command::new(test_bin_path);
    let mut child = command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|_| panic!("failed to spawn command {cmd}"));

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
    output
}

pub fn run_test(plan: TestPlan) {
    use std::fmt::Write;

    const LONG_MARKER: &str =
        "------------------------------------------------------------------------------------------------------------------------";
    const MARKER: &str =
        "--------------------------------------------------------------------------------";

    let TestPlan {
        cmd,
        args,
        stdin_data,
        expected_out,
        expected_err,
        expected_exit_code,
    } = plan;

    let output = run_test_base(&cmd, &args, stdin_data.as_bytes());

    let Output {
        status,
        stdout,
        stderr,
    } = output;

    let mut failures = Vec::<String>::new();

    let stdout_cow = String::from_utf8_lossy(&stdout);

    if stdout_cow != expected_out {
        failures.push(format!(
            "\
stdout differs from what was expected
{MARKER}

Actual
{MARKER}
{stdout_cow}
{MARKER}

Expected
{MARKER}
{expected_out}
{MARKER}
"
        ));
    }

    let stderr_cow = String::from_utf8_lossy(&stderr);

    if stderr_cow != expected_err {
        failures.push(format!(
            "\
stderr differs from what was expected
{MARKER}

Actual
{MARKER}
{stderr_cow}
{MARKER}

Expected
{MARKER}
{expected_err}
{MARKER}
"
        ));
    }

    let status_code = status.code();

    let expected_exit_code_option = Some(expected_exit_code);

    if status_code != expected_exit_code_option {
        failures.push(format!(
            "\
Exit status differs from what was expected
{MARKER}

Actual
{MARKER}
{status_code:?}
{MARKER}

Expected
{MARKER}
{expected_exit_code_option:?}
{MARKER}
"
        ));
    }

    if expected_exit_code == 0 && !status.success() {
        failures.push("Execution was expected to succeed, but it failed".to_owned());
    }

    let failures_len = failures.len();

    if failures_len > 0 {
        let mut buffer = String::with_capacity(4_096_usize);

        let args_join = args.join(" ");

        writeln!(
            &mut buffer,
            "\
{LONG_MARKER}
{MARKER}
Test failed with {failures_len} total failure types
{MARKER}

Command executed
{MARKER}
{cmd} {args_join}
{MARKER}
"
        )
        .unwrap();

        for (us, st) in failures.iter().enumerate() {
            let failure_number = us + 1;

            writeln!(
                &mut buffer,
                "Failure {failure_number} of {failures_len}: {st}"
            )
            .unwrap();
        }

        let stderr_truncated = stderr_cow.chars().take(1_024_usize).collect::<String>();

        writeln!(
            &mut buffer,
            "
stderr, for diagnosing failure{}
{MARKER}
{stderr_cow}
{MARKER}",
            if stderr_truncated.len() != stderr_cow.len() {
                " (truncated to 1 kibibyte)"
            } else {
                ""
            }
        )
        .unwrap();

        writeln!(&mut buffer, "{LONG_MARKER}").unwrap();

        panic!("{buffer}");
    }
}

pub fn run_test_u8(plan: TestPlanU8) {
    let output = run_test_base(&plan.cmd, &plan.args, &plan.stdin_data);

    assert_eq!(output.stdout, plan.expected_out);

    assert_eq!(output.stderr, plan.expected_err);

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

pub fn run_test_with_checker<F: FnMut(&TestPlan, &Output)>(plan: TestPlan, mut checker: F) {
    let output = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());
    checker(&plan, &output);
}
