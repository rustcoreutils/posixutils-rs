//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `echo` and `printf` regular built-ins.  Every expectation was checked
//! against dash (and against bash where the two agree).

use plib::testing::{run_test, TestPlan};

fn sh_c_full(script: &str, expected_out: &str, expected_err: &str, expected_exit_code: i32) {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), script.to_string()],
        stdin_data: String::new(),
        expected_out: expected_out.to_string(),
        expected_err: expected_err.to_string(),
        expected_exit_code,
    });
}

fn sh_c(script: &str, expected_out: &str) {
    sh_c_full(script, expected_out, "", 0);
}

/// A fresh directory for a test that writes files.
fn scratch_dir(name: &str) -> String {
    let dir = std::path::Path::new(env!("CARGO_TARGET_TMPDIR")).join(name);
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir.to_str().unwrap().to_string()
}

#[test]
fn echo_is_a_builtin() {
    sh_c("type echo", "echo is a shell builtin\n");
    sh_c("command -V echo", "echo is a shell builtin\n");
    sh_c("command -v echo", "echo\n");
}

// XCU 2.9.1.4: a regular built-in is found without searching PATH.
#[test]
fn echo_works_without_path() {
    sh_c("PATH=/nonexistent; echo ok", "ok\n");
}

#[test]
fn echo_dash_n_and_backslash_c() {
    sh_c("echo -n a; echo b", "ab\n");
    sh_c("echo 'b\\cz' y; echo", "b\n");
    // Only an exact first -n is an option.
    sh_c("echo a -n; echo -nn b", "a -n\n-nn b\n");
}

#[test]
fn echo_escapes_are_always_on() {
    sh_c("echo 'a\\tb\\0101\\\\'", "a\tbA\\\n");
    sh_c("echo 'x\\qy'", "x\\qy\n");
    // As in dash, -e is not an option and is printed.
    sh_c("echo -e 'x\\ty'", "-e x\ty\n");
}

#[test]
fn echo_output_follows_redirections() {
    let dir = scratch_dir("echo_redirections");
    sh_c(&format!("cd '{dir}' && echo x >f && cat f"), "x\n");
    sh_c_full("echo y 2>&1 >/dev/null", "", "", 0);
    sh_c_full("echo e >&2", "", "e\n", 0);
    sh_c("{ echo e >&2; } 2>&1", "e\n");
}

#[test]
fn echo_write_error_is_reported_once() {
    sh_c_full(
        "echo z >&-; echo st=$?",
        "st=1\n",
        "echo: write error: Bad file descriptor\n",
        0,
    );
}

#[test]
fn echo_lookup_order() {
    // A path names the external utility.
    sh_c("/bin/echo ext", "ext\n");
    // `command` skips functions and finds the built-in.
    sh_c("command echo cmd", "cmd\n");
    // A function is found before a regular built-in.
    sh_c(
        "echo() { command echo \"fn:$*\"; }; echo hi; command echo plain",
        "fn:hi\nplain\n",
    );
}

#[test]
fn printf_is_a_builtin() {
    sh_c("type printf", "printf is a shell builtin\n");
    sh_c("command -V printf", "printf is a shell builtin\n");
}

// XCU 2.9.1.4: a regular built-in is found without searching PATH.
#[test]
fn printf_works_without_path() {
    sh_c("PATH=/nonexistent; printf '%s\\n' ok", "ok\n");
}

// XCU 1.4: a leading "--" is discarded.
#[test]
fn printf_discards_a_leading_double_dash() {
    sh_c("printf -- '-%s\\n' x; printf -- --; echo", "-x\n--\n");
    sh_c_full("printf --", "", "printf: not enough arguments\n", 1);
}

#[test]
fn printf_reuses_the_format_for_extra_operands() {
    sh_c("printf '%s-%s\\n' a b c", "a-b\nc-\n");
}

#[test]
fn printf_missing_operands_are_empty_or_zero() {
    sh_c("printf '[%s|%d]\\n'", "[|0]\n");
}

#[test]
fn printf_conversions() {
    sh_c(
        "printf '%b|%c|%5.2f|%x\\n' 'x\\ty' hello 3.14159 255",
        "x\ty|h| 3.14|ff\n",
    );
    // \c in a %b operand ends all output.
    sh_c("printf '%b|' 'c\\cd' e; echo", "c\n");
}

// The diagnostic text is the standalone utility's; dash and bash agree that
// the value is still printed and the status is 1.
#[test]
fn printf_invalid_number() {
    sh_c_full(
        "printf '%d\\n' abc",
        "0\n",
        "printf: abc: expected numeric value\n",
        1,
    );
    sh_c(
        "printf '%d|%d\\n' abc 5 2>&1; echo st=$?",
        "printf: abc: expected numeric value\n0|5\nst=1\n",
    );
}

// The standalone utility's text and status (dash and bash exit 2).
#[test]
fn printf_without_a_format_is_an_error() {
    sh_c_full("printf", "", "printf: not enough arguments\n", 1);
}

#[test]
fn printf_malformed_format_is_an_error() {
    sh_c_full(
        "printf '\\x'",
        "",
        "printf: missing hexadecimal number in escape\n",
        1,
    );
}

#[test]
fn printf_output_follows_redirections() {
    let dir = scratch_dir("printf_redirections");
    sh_c(&format!("cd '{dir}' && printf x >f && cat f"), "x");
    sh_c_full("printf e >&2", "", "e", 0);
    sh_c_full(
        "printf z >&-; echo st=$?",
        "st=1\n",
        "printf: write error: Bad file descriptor\n",
        0,
    );
}

#[test]
fn printf_lookup_order() {
    sh_c("command printf '%s\\n' cmd", "cmd\n");
    // A function is found before a regular built-in.
    sh_c(
        "printf() { echo fn; }; printf x; command printf '%s\\n' plain",
        "fn\nplain\n",
    );
}
