//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::common;
use plib::testing::{run_test_with_checker, TestPlan};

const MESG: &str = env!("CARGO_BIN_EXE_mesg");

// Without a terminal on any of fd 0/1/2, mesg cannot find a tty → exit 2
// (an error), NOT the "not allowed" status of 1.
#[test]
fn test_mesg_no_terminal_is_error() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mesg"),
            args: vec![],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 2,
        },
        |_plan, output| {
            assert_eq!(
                output.status.code(),
                Some(2),
                "mesg with no terminal must exit 2 (error), not 0/1"
            );
        },
    );
}

// Under a real PTY: the exit status must reflect the resulting messaging
// state — `mesg y` exits 0 and reports `is y`; `mesg n` exits 1 and reports
// `is n`. (Pre-fix, mesg always exited 0.)
#[test]
fn test_mesg_set_and_query_exit_status() {
    let script = format!(
        "'{m}' y; echo \"y=$?\"; '{m}'; '{m}' n; echo \"n=$?\"; '{m}'",
        m = MESG
    );
    match common::run_sh(&script) {
        Some((_code, out)) => {
            assert!(out.contains("y=0"), "mesg y should exit 0; got:\n{out}");
            assert!(
                out.contains("is y"),
                "after mesg y the report should be 'is y'; got:\n{out}"
            );
            assert!(out.contains("n=1"), "mesg n should exit 1; got:\n{out}");
            assert!(
                out.contains("is n"),
                "after mesg n the report should be 'is n'; got:\n{out}"
            );
        }
        None => eprintln!("Skipping PTY test: no pseudo-terminal available"),
    }
}

// An operand other than y/n is an error (exit 2), even on a terminal.
#[test]
fn test_mesg_invalid_operand_is_error() {
    let script = format!("'{m}' bogus; echo \"x=$?\"", m = MESG);
    match common::run_sh(&script) {
        Some((_code, out)) => {
            assert!(
                out.contains("x=2"),
                "an invalid operand should exit 2; got:\n{out}"
            );
        }
        None => eprintln!("Skipping PTY test: no pseudo-terminal available"),
    }
}
