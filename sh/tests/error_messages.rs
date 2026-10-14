//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The diagnostics the shell writes, and the exit statuses that go with
//! them.

use plib::testing::{run_test, TestPlan};

fn sh_c(script: &str, expected_out: &str, expected_err: &str, expected_exit_code: i32) {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), script.to_string()],
        stdin_data: String::new(),
        expected_out: expected_out.to_string(),
        expected_err: expected_err.to_string(),
        expected_exit_code,
    });
}

// A built-in utility's diagnostic is one line, with no blank line after it.
#[test]
fn builtin_diagnostic_is_one_line() {
    sh_c("cd ''; echo $?", "1\n", "cd: empty directory operand\n", 0);
    sh_c(
        "set -o nosuchopt; echo $?",
        "",
        "set: invalid option 'nosuchopt'\n",
        1,
    );
}

const MISSING_DIR: &str = "/nonexistent-posixutils-sh-dir";
const MISSING_FILE: &str = "/nonexistent-posixutils-sh-dir/x";

/// The system's text for opening a path in a directory that does not exist.
fn enoent_text() -> String {
    plib::testing::open_error_text(MISSING_FILE)
}

// A failed redirection names the file and whether it was being opened for
// reading or created; the status is 1 and the command does not run.
#[test]
fn failed_redirection_names_the_file() {
    let open = format!("sh: cannot open '{MISSING_FILE}': {}\n", enoent_text());
    let create = format!("sh: cannot create '{MISSING_FILE}': {}\n", enoent_text());
    sh_c(&format!("cat < {MISSING_FILE}; echo $?"), "1\n", &open, 0);
    sh_c(
        &format!("echo a > {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("echo a >> {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("echo a >| {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("cat <> {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("set -C; echo a > {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("x=$(cat < {MISSING_FILE}); echo $?"),
        "1\n",
        &open,
        0,
    );
}

// XCU 2.8.1: a redirection error on a special built-in exits a
// non-interactive shell; on a compound command, a function or another
// utility it does not. Only the message changed.
#[test]
fn failed_redirection_status_follows_xcu_2_8_1() {
    let open = format!("sh: cannot open '{MISSING_FILE}': {}\n", enoent_text());
    let create = format!("sh: cannot create '{MISSING_FILE}': {}\n", enoent_text());
    sh_c(&format!("exec < {MISSING_FILE}; echo after"), "", &open, 1);
    sh_c(&format!(": > {MISSING_FILE}; echo after"), "", &create, 1);
    sh_c(
        &format!("{{ :; }} > {MISSING_FILE}; echo after $?"),
        "after 1\n",
        &create,
        0,
    );
    sh_c(
        &format!("f() {{ :; }}; f > {MISSING_FILE}; echo after $?"),
        "after 1\n",
        &create,
        0,
    );
    sh_c(
        &format!("command : > {MISSING_FILE}; echo after $?"),
        "after 1\n",
        &create,
        0,
    );
}

// A file that cannot be created for another reason gets that reason.
#[test]
fn failed_redirection_reports_the_reason() {
    sh_c(
        "echo a > /; echo $?",
        "1\n",
        "sh: cannot create '/': Is a directory\n",
        0,
    );
}

// cd and dot name their operand too.
#[test]
fn cd_and_dot_failures_name_the_operand() {
    sh_c(
        &format!("cd {MISSING_DIR}; echo $?"),
        "1\n",
        &format!(
            "cd: cannot change directory to '{MISSING_DIR}': {}\n",
            plib::testing::open_error_text(MISSING_DIR)
        ),
        0,
    );
    sh_c(
        ". /; echo $?",
        "",
        "dot: cannot read '/': Is a directory\n",
        1,
    );
}
