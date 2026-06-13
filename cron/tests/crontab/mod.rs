//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::run_test_base;
use std::process::Output;

fn run_cron_test(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> Output {
    run_test_base(cmd, args, stdin_data)
}

#[test]
fn no_args() {
    let output = run_cron_test("crontab", &vec![], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn dash_e() {
    let output = run_cron_test("crontab", &vec!["-e".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn dash_l() {
    let output = run_cron_test("crontab", &vec!["-l".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn dash_r() {
    let output = run_cron_test("crontab", &vec!["-r".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn too_many_args() {
    let output = run_cron_test("crontab", &vec!["-erl".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

// Validation used by `crontab` before installing an entry (audit #C4). "Valid"
// means exactly "the daemon will load this crontab".
use cron::job::validate_user_crontab;

#[test]
fn validate_accepts_five_field_and_at_specs() {
    assert!(validate_user_crontab("15 3 * * 1-5 find /tmp -name core").is_ok());
    assert!(validate_user_crontab("*/15 * * * * echo hi").is_ok());
    assert!(validate_user_crontab("@daily echo hi\n@reboot echo boot").is_ok());
}

#[test]
fn validate_ignores_blank_comment_and_short_lines() {
    // Blank lines, comments, and structurally short lines are skipped by the
    // daemon, so they must not be flagged as errors.
    assert!(validate_user_crontab("\n# a comment\n   \nnotacron").is_ok());
}

#[test]
fn validate_rejects_bad_time_field_with_line_number() {
    assert_eq!(validate_user_crontab("0 0 * * * ok\nz * * * * bad"), Err(2));
}

#[test]
fn validate_rejects_unknown_at_spec() {
    assert_eq!(validate_user_crontab("@bogus echo hi"), Err(1));
}
