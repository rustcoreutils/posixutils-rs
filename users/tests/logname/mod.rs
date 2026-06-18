//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};

fn logname_plan(args: Vec<String>) -> TestPlan {
    TestPlan {
        cmd: String::from("logname"),
        args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    }
}

// logname's behavior depends on getlogin(3), which needs a controlling
// terminal / utmp entry. Under the test harness stdin is a pipe, so the
// outcome is environment-dependent: it must EITHER print a non-empty login
// name and exit 0, OR write the diagnostic and exit 1 — never silently
// succeed with an empty / "unknown" name (the pre-fix behavior).
#[test]
fn test_logname_strict_contract() {
    run_test_with_checker(logname_plan(vec![]), |_plan, output| {
        let code = output.status.code();
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        match code {
            Some(0) => {
                let name = stdout.strip_suffix('\n').unwrap_or(&stdout);
                assert!(!name.is_empty(), "exit 0 must print a non-empty login name");
                assert_ne!(
                    name, "unknown",
                    "must not fall back to the 'unknown' sentinel"
                );
                assert!(stderr.is_empty(), "no diagnostic on success");
            }
            Some(1) => {
                assert!(stdout.is_empty(), "no stdout when getlogin() fails");
                assert_eq!(
                    stderr, "logname: no login name\n",
                    "must emit the diagnostic on getlogin() failure"
                );
            }
            other => panic!("unexpected logname exit status: {:?}", other),
        }
    });
}

// logname defines no operands; an extra argument is rejected (clap exit 2).
#[test]
fn test_logname_rejects_operand() {
    run_test_with_checker(
        logname_plan(vec![String::from("bogus")]),
        |_plan, output| {
            assert_eq!(
                output.status.code(),
                Some(2),
                "an unexpected operand should be rejected with exit 2"
            );
        },
    );
}
