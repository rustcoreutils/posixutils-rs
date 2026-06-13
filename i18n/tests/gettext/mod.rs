//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_env, TestPlan};

/// Test gettext with no catalog - should echo the original message
#[test]
fn test_gettext_no_catalog() {
    run_test_with_env(
        TestPlan {
            cmd: String::from("gettext"),
            args: vec![String::from("Hello, World!")],
            stdin_data: String::new(),
            // GT-2: the non-`-s` form does not append a trailing newline.
            expected_out: String::from("Hello, World!"),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("TEXTDOMAIN", "nonexistent"), ("LC_ALL", "C")],
    );
}

/// Test gettext with -n flag (no newline)
#[test]
fn test_gettext_no_newline() {
    run_test(TestPlan {
        cmd: String::from("gettext"),
        args: vec![String::from("-n"), String::from("Hello")],
        stdin_data: String::new(),
        expected_out: String::from("Hello"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test gettext with -e flag (expand escapes)
#[test]
fn test_gettext_expand_escapes() {
    run_test(TestPlan {
        cmd: String::from("gettext"),
        args: vec![String::from("-e"), String::from("Hello\\nWorld")],
        stdin_data: String::new(),
        // GT-2: no trailing newline in the non-`-s` form (the `\n` here is from
        // the processed escape in the message itself).
        expected_out: String::from("Hello\nWorld"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test gettext with -e and -n flags combined
#[test]
fn test_gettext_expand_no_newline() {
    run_test(TestPlan {
        cmd: String::from("gettext"),
        args: vec![
            String::from("-e"),
            String::from("-n"),
            String::from("Tab\\there"),
        ],
        stdin_data: String::new(),
        expected_out: String::from("Tab\there"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test gettext with domain specified
#[test]
fn test_gettext_with_domain() {
    run_test_with_env(
        TestPlan {
            cmd: String::from("gettext"),
            args: vec![
                String::from("-d"),
                String::from("testdomain"),
                String::from("Test message"),
            ],
            stdin_data: String::new(),
            // GT-2: no trailing newline in the non-`-s` form.
            expected_out: String::from("Test message"),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LC_ALL", "C")],
    );
}

/// GT-1: `-E` disables escape processing (the default), so `\n` stays literal.
#[test]
fn test_gettext_no_escape_with_capital_e() {
    run_test(TestPlan {
        cmd: String::from("gettext"),
        args: vec![String::from("-E"), String::from("Hello\\nWorld")],
        stdin_data: String::new(),
        expected_out: String::from("Hello\\nWorld"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test gettext shell mode (-s) with multiple arguments
#[test]
fn test_gettext_shell_mode() {
    run_test(TestPlan {
        cmd: String::from("gettext"),
        args: vec![
            String::from("-s"),
            String::from("Hello"),
            String::from("World"),
        ],
        stdin_data: String::new(),
        expected_out: String::from("Hello World\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}
