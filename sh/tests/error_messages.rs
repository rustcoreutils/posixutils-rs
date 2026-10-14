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

// A built-in utility that cannot write its output reports it once and
// exits 1, as dash and bash do, rather than reporting every failed write
// and exiting 0.
#[test]
fn builtin_write_error_is_reported_once_with_status_1() {
    sh_c(
        "pwd >&-; echo $?",
        "1\n",
        "pwd: write error: Bad file descriptor\n",
        0,
    );
    sh_c(
        "export A=1 B=2; export -p >&-; echo $?",
        "1\n",
        "export: write error: Bad file descriptor\n",
        0,
    );
    sh_c("pwd 2>&- >/dev/null; echo $?", "0\n", "", 0);
}

// "not found" names the command without quotes, as `NAME: not found`, the
// form dash and bash use. GNU make's test suite captures the message and
// echoes it inside single quotes, so quotes in it were lost and its
// functions/shell test failed.
#[test]
fn command_not_found_message_form() {
    sh_c(
        "no_such_cmd_xyz",
        "",
        "sh: no_such_cmd_xyz: not found\n",
        127,
    );
    sh_c(
        "out=$(no_such_cmd_xyz 2>&1); echo \"$out\"",
        "sh: no_such_cmd_xyz: not found\n",
        "",
        0,
    );
}

// The built-ins that search for a command report a failed search in the
// same unquoted `NAME: not found` form.
#[test]
fn builtin_not_found_message_form() {
    sh_c(
        "exec no_such_cmd_xyz",
        "",
        "exec: no_such_cmd_xyz: not found\n",
        127,
    );
    sh_c(
        "command no_such_cmd_xyz",
        "",
        "command: no_such_cmd_xyz: not found\n",
        127,
    );
    sh_c(
        "unalias no_such_alias_xyz",
        "",
        "unalias: no_such_alias_xyz: not found\n",
        1,
    );
}
