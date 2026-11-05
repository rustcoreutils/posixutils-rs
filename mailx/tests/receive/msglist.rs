//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Message list syntax.

use crate::common::{copy_test_data, create_temp_mbox};
use plib::testing::{run_test_with_checker, TestPlan};

// -----------------------------------------------------------------------------
// Range syntax: n-m
// -----------------------------------------------------------------------------

/// Test message range 1-3
#[test]
fn msglist_range() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from 1-3\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // Should show messages 1, 2, 3
            assert!(
                stdout.contains("alice") && stdout.contains("bob") && stdout.contains("charlie"),
                "Range 1-3 should show first three messages: {}",
                stdout
            );
            // Should NOT show messages 4, 5
            assert!(
                !stdout.contains("dave"),
                "Range 1-3 should not show message 5: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test message range 3-5
#[test]
fn msglist_range_end() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from 3-5\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // Should show messages 3, 4, 5
            assert!(
                stdout.contains("charlie") && stdout.contains("dave"),
                "Range 3-5 should show last messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Special symbols: . ^ $ * + -
// -----------------------------------------------------------------------------

/// Test . (current message)
#[test]
fn msglist_dot_current() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            // Print message 3 to set current, then use . to reference it
            stdin_data: String::from("3\nfrom .\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After viewing 3, . should refer to message 3
            assert!(
                stdout.contains("charlie") || stdout.contains("Urgent"),
                ". should reference current message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test ^ (first message)
#[test]
fn msglist_caret_first() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from ^\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stdout_lower = stdout.to_lowercase();
            // ^ should show first message (alice, Meeting Tomorrow)
            assert!(
                stdout_lower.contains("alice") && stdout_lower.contains("meeting tomorrow"),
                "^ should show first message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test $ (last message)
#[test]
fn msglist_dollar_last() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from $\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stdout_lower = stdout.to_lowercase();
            // $ should show last message (dave, Final Report)
            assert!(
                stdout_lower.contains("dave") && stdout_lower.contains("final report"),
                "$ should show last message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test * (all messages)
#[test]
fn msglist_star_all() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from *\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // * should show all 5 messages
            assert!(
                stdout.contains("1")
                    && stdout.contains("2")
                    && stdout.contains("3")
                    && stdout.contains("4")
                    && stdout.contains("5"),
                "* should show all messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test + (next message)
#[test]
fn msglist_plus_next() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            // Start at msg 2, then + should go to msg 3
            stdin_data: String::from("2\nfrom +\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After msg 2, + should show msg 3 (charlie)
            assert!(
                stdout.contains("charlie") || stdout.contains("Urgent"),
                "+ should show next message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test - (previous message)
#[test]
fn msglist_minus_prev() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            // Start at msg 3, then - should go to msg 2
            stdin_data: String::from("3\nfrom -\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After msg 3, - should show msg 2 (bob)
            assert!(
                stdout.contains("bob") || stdout.contains("Project"),
                "- should show previous message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Subject search: /pattern
// -----------------------------------------------------------------------------

/// Test /pattern searches subjects
#[test]
fn msglist_subject_search() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from /Meeting\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // /Meeting should find messages 1 and 4 (both have "Meeting" in subject)
            assert!(
                stdout.contains("Meeting Tomorrow") || stdout.contains("Another Meeting"),
                "/Meeting should find Meeting messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test /pattern is case-insensitive
#[test]
fn msglist_subject_search_case() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from /urgent\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // /urgent (lowercase) should find "Urgent Request"
            assert!(
                stdout.contains("Urgent") || stdout.contains("charlie"),
                "/urgent should match case-insensitively: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Message type: :n :o :r :u :d
// -----------------------------------------------------------------------------

/// Test :d (deleted messages) - for undelete
#[test]
fn msglist_type_deleted() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            // Delete messages 1 and 3, then undelete :d (all deleted)
            stdin_data: String::from("delete 1 3\nundelete :d\nfrom *\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // After undelete :d, all messages should be visible
            assert!(
                stdout.contains("alice") && stdout.contains("charlie"),
                ":d should select deleted messages for undelete: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Address matching
// -----------------------------------------------------------------------------

/// Test address matching (sender name)
#[test]
fn msglist_address_match() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from bob\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should find bob's message
            assert!(
                stdout.contains("bob") || stdout.contains("Project"),
                "Address match should find bob: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test address matching with full email
#[test]
fn msglist_address_full_email() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from alice@example.com\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should find only alice@example.com (not alice@different.org)
            assert!(
                stdout.contains("Meeting Tomorrow"),
                "Full email should find exact match: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test allnet variable - match by login only
#[test]
fn msglist_allnet_login_match() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            // With allnet, "alice" should match both alice@example.com and alice@different.org
            stdin_data: String::from("set allnet\nfrom alice\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should find both alice messages (1 and 4)
            assert!(
                (stdout.contains("Meeting Tomorrow") || stdout.contains("alice@example.com"))
                    && (stdout.contains("Another Meeting")
                        || stdout.contains("alice@different.org")),
                "allnet should match both alice addresses: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Multiple specs combined
// -----------------------------------------------------------------------------

/// Test multiple message specs separated by space
#[test]
fn msglist_multiple_specs() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from 1 3 5\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // Should show messages 1, 3, 5
            assert!(
                stdout.contains("alice") && stdout.contains("charlie") && stdout.contains("dave"),
                "Multiple specs should show selected messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test range combined with numbers
#[test]
fn msglist_range_and_number() {
    let mbox = copy_test_data("testdata-5msg.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from 1-2 5\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // Should show messages 1, 2, 5 but not 3, 4
            assert!(
                stdout.contains("alice") && stdout.contains("bob") && stdout.contains("dave"),
                "Range + number should show correct messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// A hyphen inside an address is not a message range (spec 104533).
#[test]
fn hyphenated_address_is_not_a_range() {
    let mbox = create_temp_mbox(
        "From alpha@example.com Fri Nov 29 10:00:00 2024\n\
         From: alpha@example.com\n\
         Subject: first message\n\
         \n\
         body one\n\
         \n\
         From beta@example.org Fri Nov 29 11:00:00 2024\n\
         From: beta@example.org\n\
         Subject: second message\n\
         \n\
         body two\n\
         \n\
         From alpha-zeta@example.net Fri Nov 29 12:00:00 2024\n\
         From: alpha-zeta@example.net\n\
         Subject: third message\n\
         \n\
         body three\n\
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
            stdin_data: String::from("from alpha-zeta\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let out = String::from_utf8_lossy(&output.stdout);
            assert!(
                out.contains("third message"),
                "the hyphenated address did not match its own message: {}",
                out
            );
            assert!(
                !out.contains("first message") && !out.contains("second message"),
                "`alpha-zeta` was parsed as the range alpha..zeta: {}",
                out
            );
        },
    );
}

/// `^`, `$`, and `.` are message numbers, so they may end a range (spec 104533).
#[test]
fn range_endpoints_accept_the_positional_forms() {
    let mbox = create_temp_mbox(
        "From a@example.com Fri Nov 29 10:00:00 2024\n\
         From: a@example.com\n\
         Subject: first\n\
         \n\
         one\n\
         \n\
         From c@example.org Fri Nov 29 11:00:00 2024\n\
         From: c@example.org\n\
         Subject: second\n\
         \n\
         two\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    for spec in ["1-$", "^-$", "1-2"] {
        run_test_with_checker(
            TestPlan {
                cmd: String::from("mailx"),
                args: vec![
                    String::from("-n"),
                    String::from("-N"),
                    String::from("-f"),
                    mbox_path.clone(),
                ],
                stdin_data: format!("from {}\nexit\n", spec),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_plan, output| {
                let out = String::from_utf8_lossy(&output.stdout);
                assert!(
                    out.contains("first") && out.contains("second"),
                    "`from {}` did not select the whole range: {}",
                    spec,
                    out
                );
            },
        );
    }
}

/// A range running past the last message is clamped, not rejected.
#[test]
fn undelete_range_past_the_end_is_clamped() {
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
            stdin_data: String::from("delete 1 2\nundelete 1-9999\nfrom *\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            assert!(
                !err.contains("Invalid message number"),
                "an over-long range was rejected instead of clamped: {}",
                err
            );
            let out = String::from_utf8_lossy(&output.stdout).to_lowercase();
            assert!(
                out.contains("alice") && out.contains("bob"),
                "undelete over a clamped range did not restore the messages: {}",
                out
            );
        },
    );
}
