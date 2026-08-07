//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{os_bytes, run_test, run_test_os, TestPlan, TestPlanOs};

#[test]
fn basename_basic() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from("/usr/bin/env")],
        stdin_data: String::new(),
        expected_out: String::from("env\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn basename_trailing_slash() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from("/usr/bin/")],
        stdin_data: String::new(),
        expected_out: String::from("bin\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn basename_with_suffix() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from("/usr/bin/env"), String::from("v")],
        stdin_data: String::new(),
        expected_out: String::from("en\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn basename_current_directory() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from(".")],
        stdin_data: String::new(),
        expected_out: String::from(".\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn basename_empty_path() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from("")],
        stdin_data: String::new(),
        expected_out: String::from("\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

fn basename_case(args: &[&str], out: &str) {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::new(),
        expected_out: String::from(out),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// POSIX step 3: a string of all <slash> characters yields a single <slash>.
// Previously these panicked (Path::file_name() == None).
#[test]
fn basename_root() {
    basename_case(&["/"], "/\n");
}

#[test]
fn basename_double_slash() {
    basename_case(&["//"], "/\n");
}

#[test]
fn basename_triple_slash() {
    basename_case(&["///"], "/\n");
}

// Final component "..": no <slash>, so the result is "..". Previously panicked.
#[test]
fn basename_dotdot() {
    basename_case(&[".."], "..\n");
}

#[test]
fn basename_trailing_dotdot() {
    basename_case(&["a/b/.."], "..\n");
}

// POSIX step 6: a suffix identical to the resulting string is NOT removed.
#[test]
fn basename_suffix_identical_to_result() {
    basename_case(&["/usr/bin/env", "env"], "env\n");
}

// The suffix must be applied to the final component, after trailing slashes
// are trimmed -- not to the whole pathname.
#[test]
fn basename_suffix_with_trailing_slash() {
    basename_case(&["src/dir/", "ir"], "d\n");
}

// A leading-hyphen operand is a valid string, not an option.
#[test]
fn basename_leading_hyphen_operand() {
    basename_case(&["-n"], "-n\n");
}

#[test]
fn basename_double_dash_then_hyphen() {
    basename_case(&["--", "-n"], "-n\n");
}

// FUTURE DIRECTIONS: an embedded <newline> in the output is treated as an error.
#[test]
fn basename_newline_is_error() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from("foo\nbar")],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::from("basename: result contains a newline character\n"),
        expected_exit_code: 1,
    });
}

/// A pathname operand is a byte string, not a character string, so `basename`
/// must pass through bytes that are not valid UTF-8 (#B4).
///
/// `TestPlan::args` is `Vec<String>` and cannot express such an operand, which
/// is why this box sat open as "code path is byte-clean; explicit test
/// deferred". `TestPlanOs` takes `OsString` arguments and compares output
/// byte-exactly — a lossy comparison would mask the very corruption being
/// tested for.
#[test]
fn basename_non_utf8_operand() {
    // 0xFF and 0xFE are not valid UTF-8 in any position.
    run_test_os(TestPlanOs {
        cmd: String::from("basename"),
        args: vec![os_bytes(b"/tmp/\xff\xfename")],
        stdin_data: Vec::new(),
        expected_out: b"\xff\xfename\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

/// The suffix operand must be byte-compared too, not UTF-8-decoded.
#[test]
fn basename_non_utf8_suffix() {
    run_test_os(TestPlanOs {
        cmd: String::from("basename"),
        args: vec![os_bytes(b"/tmp/file\xff\xfe"), os_bytes(b"\xff\xfe")],
        stdin_data: Vec::new(),
        expected_out: b"file\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}
