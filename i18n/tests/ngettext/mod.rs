//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

/// Test ngettext singular form (n=1)
#[test]
fn test_ngettext_singular() {
    run_test(TestPlan {
        cmd: String::from("ngettext"),
        args: vec![
            String::from("One file"),
            String::from("%d files"),
            String::from("1"),
        ],
        stdin_data: String::new(),
        expected_out: String::from("One file\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test ngettext plural form (n=0)
#[test]
fn test_ngettext_plural_zero() {
    run_test(TestPlan {
        cmd: String::from("ngettext"),
        args: vec![
            String::from("One file"),
            String::from("%d files"),
            String::from("0"),
        ],
        stdin_data: String::new(),
        expected_out: String::from("%d files\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test ngettext plural form (n=5)
#[test]
fn test_ngettext_plural_many() {
    run_test(TestPlan {
        cmd: String::from("ngettext"),
        args: vec![
            String::from("One item"),
            String::from("%d items"),
            String::from("5"),
        ],
        stdin_data: String::new(),
        expected_out: String::from("%d items\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test ngettext with escape expansion
#[test]
fn test_ngettext_expand_escapes() {
    run_test(TestPlan {
        cmd: String::from("ngettext"),
        args: vec![
            String::from("-e"),
            String::from("One\\nfile"),
            String::from("%d\\nfiles"),
            String::from("1"),
        ],
        stdin_data: String::new(),
        expected_out: String::from("One\nfile\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test ngettext with large number
#[test]
fn test_ngettext_large_number() {
    run_test(TestPlan {
        cmd: String::from("ngettext"),
        args: vec![
            String::from("One message"),
            String::from("%d messages"),
            String::from("1000000"),
        ],
        stdin_data: String::new(),
        expected_out: String::from("%d messages\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}
