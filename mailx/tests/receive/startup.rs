//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Start-up file handling.

use super::capitalized::capitalized_fixture;
use crate::common::{copy_test_data, create_temp_mailrc};
use plib::testing::{run_test_with_checker_and_env, TestPlan};

/// A `Copy` command in a start-up file is diagnosed (case-folded match).
#[test]
fn startup_copy_is_diagnosed() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("Copy 1\n");

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-N"),
                String::from("-f"),
                mbox.path().to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc.path().to_str().unwrap())],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("not valid in a start-up file"),
                "Copy in a start-up file should be diagnosed: {}",
                stderr
            );
        },
    );
}

/// Lowercase `copy`/`save` are legal in a start-up file; only the capitalized
/// forms are not (spec 104557-104559).
#[test]
fn startup_rejects_only_the_capitalized_file_commands() {
    for (line, rejected) in [
        ("copy", false),
        ("save", false),
        ("Copy 1", true),
        ("Save 1", true),
    ] {
        let mbox = capitalized_fixture();
        let mbox_path = mbox.path().to_str().unwrap().to_string();
        let rc = create_temp_mailrc(&format!("{}\n", line));
        let rc_path = rc.path().to_str().unwrap().to_string();

        run_test_with_checker_and_env(
            TestPlan {
                cmd: String::from("mailx"),
                args: vec![String::from("-N"), String::from("-f"), mbox_path],
                stdin_data: String::from("exit\n"),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            &[("MAILRC", &rc_path)],
            |_plan, output| {
                let err = String::from_utf8_lossy(&output.stderr);
                assert_eq!(
                    err.contains("not valid in a start-up file"),
                    rejected,
                    "start-up handling of `{}` is wrong: {}",
                    line,
                    err
                );
            },
        );
    }
}
