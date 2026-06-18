//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker_and_env, TestPlan};

fn newgrp_plan(args: Vec<String>, stdin_data: &str) -> TestPlan {
    TestPlan {
        cmd: String::from("newgrp"),
        args,
        stdin_data: String::from(stdin_data),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    }
}

// newgrp must create a NEW shell execution environment and its exit status is
// the shell's. (Pre-fix, normal mode never started a shell.) The group change
// itself needs privilege (newgrp installed setuid-root) and is expected to fail
// EPERM in CI — but per POSIX that failure must NOT prevent the shell.
#[test]
fn test_newgrp_execs_shell_and_propagates_exit_status() {
    run_test_with_checker_and_env(
        newgrp_plan(vec![], "exit 7\n"),
        &[("SHELL", "/bin/sh")],
        |_plan, output| {
            assert_eq!(
                output.status.code(),
                Some(7),
                "newgrp must exec a shell and return its exit status"
            );
        },
    );
}

#[test]
fn test_newgrp_runs_commands_in_the_new_shell() {
    run_test_with_checker_and_env(
        newgrp_plan(vec![], "echo NEWGRP_SHELL_MARKER\n"),
        &[("SHELL", "/bin/sh")],
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("NEWGRP_SHELL_MARKER"),
                "the new shell should run piped commands; got: {stdout}"
            );
        },
    );
}

// A failure to assign the group (here: an unknown group) shall not prevent the
// shell from being created — newgrp diagnoses and execs the shell anyway.
#[test]
fn test_newgrp_unknown_group_still_execs_shell() {
    run_test_with_checker_and_env(
        newgrp_plan(vec![String::from("nosuchgroup_xyzzy_123")], "exit 3\n"),
        &[("SHELL", "/bin/sh")],
        |_plan, output| {
            assert_eq!(
                output.status.code(),
                Some(3),
                "invoke-anyway: the shell must run even when the group is unknown"
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("no such group"),
                "expected a 'no such group' diagnostic; got: {stderr}"
            );
        },
    );
}
