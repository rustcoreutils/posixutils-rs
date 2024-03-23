//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Write;
use std::process::{Command, Stdio};

pub mod modestr;

pub const PROJECT_NAME: &'static str = "posixutils-rs";

pub const BUFSZ: usize = 8 * 1024;

pub struct TestPlan {
    pub cmd: String,
    pub args: Vec<String>,
    pub stdin_data: String,
    pub expected_out: String,
}

pub fn run_test(plan: TestPlan) {
    let relpath = format!("target/release/{}", plan.cmd);
    let test_bin_path = std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap() // Move up to the workspace root from the current package directory
        .join(relpath); // Adjust the path to the binary

    let mut command = Command::new(test_bin_path);
    let mut child = command
        .args(plan.args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to spawn head");

    let stdin = child.stdin.as_mut().expect("failed to get stdin");
    stdin
        .write_all(plan.stdin_data.as_bytes())
        .expect("failed to write to stdin");

    let output = child.wait_with_output().expect("failed to wait for child");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, plan.expected_out);
    assert!(output.status.success());
    assert_eq!(output.status.code(), Some(0));
}
