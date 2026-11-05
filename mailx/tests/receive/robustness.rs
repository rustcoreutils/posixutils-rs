//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Inputs that once aborted the process.

use crate::common::{assert_no_panic, copy_test_data, create_temp_mbox};
use plib::testing::{run_test_with_checker, TestPlan};

/// A `From:` whose display name contains `>` before `<` must not abort.
///
/// `extract_address` searched for `>` from the start of the string rather than
/// after the `<`, producing an inverted slice range.
#[test]
fn from_with_reversed_angle_brackets_no_panic() {
    let mbox = create_temp_mbox(
        "From bob@example.com Fri Nov 29 10:00:00 2024\n\
         From: Bob >_< Smith <bob@example.com>\n\
         Subject: reversed brackets\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: String::from("followup 1\n.\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| assert_no_panic(output, "followup on a reversed-bracket From:"),
    );
}

/// A `From:` whose parenthesised comment closes before it opens must not abort.
///
/// `short_from` had the same inverted-range bug, and it fires from the header
/// summary printed at start-up -- before the prompt is ever reached.
#[test]
fn from_with_reversed_parens_no_panic() {
    let mbox = create_temp_mbox(
        "From bob@example.com Fri Nov 29 10:00:00 2024\n\
         From: Smith) Jr (John\n\
         Subject: reversed parens\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![String::from("-n"), String::from("-f"), mbox_path],
            stdin_data: String::from("headers\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| assert_no_panic(output, "header summary for a reversed-paren From:"),
    );
}

/// A multibyte character straight after the escape character must not abort.
///
/// `handle_escape` sliced at byte 1 after reading a `char`, so `~élan` split a
/// UTF-8 sequence. The callers already slice by `len_utf8`; only this function
/// reintroduced the bug.
#[test]
fn multibyte_after_escape_char_no_panic() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: String::from("mail someone@example.com\n~élan\n~x\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| assert_no_panic(output, "a multibyte character after the escape char"),
    );
}

/// `set screen=0` must not divide by zero in `headers` or `z`.
#[test]
fn screen_zero_no_panic() {
    for cmd in ["headers", "z", "z-"] {
        let mbox = copy_test_data("testdata.mbox");
        let mbox_path = mbox.path().to_str().unwrap().to_string();

        run_test_with_checker(
            TestPlan {
                cmd: String::from("mailx"),
                args: vec![
                    String::from("-n"),
                    String::from("-N"),
                    String::from("-f"),
                    mbox_path,
                ],
                stdin_data: format!("set screen=0\n{}\nexit\n", cmd),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_plan, output| assert_no_panic(output, &format!("`set screen=0` then `{}`", cmd)),
        );
    }
}

/// A cyclic alias must be diagnosed, not recursed into until the stack dies.
#[test]
fn cyclic_alias_no_stack_overflow() {
    for defs in ["alias a b\nalias b a\n", "alias a a\n"] {
        let mbox = copy_test_data("testdata-single.mbox");
        let mbox_path = mbox.path().to_str().unwrap().to_string();

        run_test_with_checker(
            TestPlan {
                cmd: String::from("mailx"),
                args: vec![
                    String::from("-n"),
                    String::from("-N"),
                    String::from("-f"),
                    mbox_path,
                ],
                stdin_data: format!("{}mail a\n~x\nexit\n", defs),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_plan, output| {
                let stderr = String::from_utf8_lossy(&output.stderr);
                assert!(
                    !stderr.contains("stack overflow"),
                    "cyclic alias overflowed the stack: {}",
                    stderr
                );
                assert_no_panic(output, "a cyclic alias");
            },
        );
    }
}

/// A `From:` crafted to escape the current directory must not name a file.
///
/// `Save`, `Copy`, `followup`, and `Followup` derive a filename from the
/// author, and the value comes straight out of an attacker-controlled header.
#[test]
fn author_derived_filename_rejects_path_traversal() {
    let mbox = create_temp_mbox(
        "From bob@example.com Fri Nov 29 10:00:00 2024\n\
         From: ../../../../tmp/mailx-traversal-probe@example.com\n\
         Subject: traversal\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();
    let probe = std::path::Path::new("/tmp/mailx-traversal-probe");
    let _ = std::fs::remove_file(probe);

    /// Removes the probe file when dropped, so a regression that creates it
    /// does not leave it behind after the assertion below fails.
    struct RemoveOnDrop<'a>(&'a std::path::Path);
    impl Drop for RemoveOnDrop<'_> {
        fn drop(&mut self) {
            let _ = std::fs::remove_file(self.0);
        }
    }
    let _cleanup = RemoveOnDrop(probe);

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: String::from("followup 1\n.\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert_no_panic(output, "a traversal-shaped author name");
            assert!(
                !probe.exists(),
                "an author-derived filename escaped the working directory"
            );
        },
    );
}
