//
// Copyright (c) 2024 Hemi Labs, Inc.
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
