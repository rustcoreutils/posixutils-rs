//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::os::unix::fs::OpenOptionsExt;

fn env_plan(args: Vec<&str>, expected_out: &str, expected_exit: i32) -> TestPlan {
    TestPlan {
        cmd: String::from("env"),
        args: args.into_iter().map(String::from).collect(),
        stdin_data: String::new(),
        expected_out: String::from(expected_out),
        expected_err: String::new(),
        expected_exit_code: expected_exit,
    }
}

fn env_exit(args: Vec<&str>, expected_exit: i32) {
    let plan = TestPlan {
        cmd: String::from("env"),
        args: args.into_iter().map(String::from).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: expected_exit,
    };
    run_test_with_checker(plan, |_, output| {
        assert_eq!(output.status.code(), Some(expected_exit));
    });
}

// `-i` with a single assignment: only that pair is printed.
#[test]
fn env_ignore_single() {
    run_test(env_plan(vec!["-i", "FOO=bar"], "FOO=bar\n", 0));
}

// `-i` dump is sorted by key (#E4), regardless of operand order.
#[test]
fn env_ignore_sorted() {
    run_test(env_plan(
        vec!["-i", "B=2", "A=1", "C=3"],
        "A=1\nB=2\nC=3\n",
        0,
    ));
}

// A later assignment overrides an earlier one for the same name.
#[test]
fn env_ignore_override() {
    run_test(env_plan(vec!["-i", "X=first", "X=second"], "X=second\n", 0));
}

// A value may itself contain '='.
#[test]
fn env_value_with_equals() {
    run_test(env_plan(vec!["-i", "K=a=b=c"], "K=a=b=c\n", 0));
}

// #E1: a utility that cannot be found exits 127 (not 1).
#[test]
fn env_not_found() {
    env_exit(vec!["/nonexistent/utility/xyz"], 127);
}

// #E1: a utility that exists but is not executable exits 126 (not 1).
#[test]
fn env_not_executable() {
    let path = std::env::temp_dir().join("posixutils_env_test_noexec");
    {
        let _f = std::fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .mode(0o644)
            .open(&path)
            .unwrap();
    }
    env_exit(vec![path.to_str().unwrap()], 126);
    let _ = std::fs::remove_file(&path);
}

// A utility is invoked with the modified environment and its status passed
// through (PATH inherited because -i is not given).
#[test]
fn env_status_passthrough_true() {
    env_exit(vec!["true"], 0);
}

#[test]
fn env_status_passthrough_false() {
    env_exit(vec!["false"], 1);
}

// #E5: a leading operand whose name part is not a valid env-var name is NOT an
// assignment — it is treated as the utility (here: not found -> 127).
#[test]
fn env_invalid_name_is_utility() {
    env_exit(vec!["-i", "1bad=x"], 127);
}

// `--` terminates options; a following operand is the utility.
#[test]
fn env_double_dash() {
    env_exit(vec!["--", "/nonexistent/utility/xyz"], 127);
}

// With no utility operand, env writes the current environment to stdout, one
// NAME=value per line (spec 94060-94062). The suite's other tests all use -i,
// so nothing exercised the inherited-environment dump.
#[test]
fn env_no_operand_dumps_inherited_environment() {
    let output = plib::testing::run_test_base_with_env(
        "env",
        &[],
        b"",
        &[("POSIXUTILS_ENV_PROBE", "sentinel-value")],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout
            .lines()
            .any(|l| l == "POSIXUTILS_ENV_PROBE=sentinel-value"),
        "env must dump the inherited environment, got:\n{stdout}"
    );
}

// `env -i` with no assignments and no utility clears the environment, so there
// is nothing to print. Paired with the test above: together they show the dump
// reflects the environment rather than being unconditional.
#[test]
fn env_ignore_with_no_assignments_prints_nothing() {
    let output = plib::testing::run_test_base_with_env(
        "env",
        &["-i".to_string()],
        b"",
        &[("POSIXUTILS_ENV_PROBE", "sentinel-value")],
    );

    assert!(output.status.success());
    assert!(
        output.stdout.is_empty(),
        "env -i must print nothing, got {:?}",
        String::from_utf8_lossy(&output.stdout)
    );
}
