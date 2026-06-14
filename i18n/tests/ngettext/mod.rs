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
        expected_out: String::from("One file"), // GT-2: no trailing newline
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
        expected_out: String::from("%d files"),
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
        expected_out: String::from("%d items"),
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
        expected_out: String::from("One\nfile"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// NG-1: the optional `[textdomain]` operand is accepted (4-operand form),
/// matching the spec synopsis `[textdomain] msgid msgid_plural n`.
#[test]
fn test_ngettext_optional_textdomain_operand() {
    run_test(TestPlan {
        cmd: String::from("ngettext"),
        args: vec![
            String::from("mail"),
            String::from("recipient"),
            String::from("recipients"),
            String::from("1"),
        ],
        stdin_data: String::new(),
        expected_out: String::from("recipient"),
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
        expected_out: String::from("%d messages"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}
