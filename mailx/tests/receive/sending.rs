//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Replies and outgoing mail: recipients, envelopes, Reply-To.

use crate::common::{copy_test_data, create_temp_mailrc, create_temp_mbox};
use plib::testing::{run_test_with_checker, run_test_with_checker_and_env, TestPlan};

/// In the reply-all (lowercase) form, the sender portion comes from Reply-To
/// when present, not From.
#[test]
fn reply_all_uses_reply_to() {
    let body = "From original@example.com Mon Jan  1 10:00:00 2024\n\
                From: original@example.com\n\
                Reply-To: replyto@example.com\n\
                To: listmember@example.com\n\
                Subject: hi\n\
                \n\
                hello\n";
    let mbox = create_temp_mbox(body);
    let mailrc = create_temp_mailrc("set debug\nset dot\n");

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-N"),
                String::from("-f"),
                mbox.path().to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("reply 1\nReplying.\n.\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc.path().to_str().unwrap())],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("replyto@example.com"),
                "reply-all should use Reply-To: {}",
                stderr
            );
            assert!(
                !stderr.contains("original@example.com"),
                "reply-all must not use From when Reply-To is present: {}",
                stderr
            );
        },
    );
}

/// Bcc recipients must actually be handed to the mail delivery software, and
/// must not appear as a header in the delivered message.
#[test]
fn bcc_reaches_the_envelope_but_not_the_message() {
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
            stdin_data: String::from(
                "set debug\nmail to@example.com\n~b blind@example.com\n~s subj\nbody\n.\nexit\n",
            ),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            let envelope = err
                .lines()
                .find(|l| l.starts_with("Envelope:"))
                .unwrap_or_else(|| panic!("debug output has no envelope line: {}", err));
            assert!(
                envelope.contains("blind@example.com"),
                "bcc recipient never reached the envelope: {}",
                envelope
            );
            assert!(
                !err.contains("\nBcc: "),
                "the blind recipient leaked into the message headers: {}",
                err
            );
        },
    );
}

/// An empty `$USER` must not drop every recipient of a reply.
///
/// The self-exclusion compared with `contains`, so an empty login matched every
/// address and `reply` silently addressed nobody but the sender.
#[test]
fn reply_with_empty_user_keeps_recipients() {
    let mbox = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         To: me@example.com, carol@example.com\n\
         Cc: dave@example.com\n\
         Subject: group thread\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: String::from("set debug\nreply 1\nbody\n.\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("USER", ""), ("LOGNAME", "")],
        |_plan, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            for who in ["carol@example.com", "dave@example.com"] {
                assert!(
                    err.contains(who),
                    "an empty $USER dropped {} from the reply: {}",
                    who,
                    err
                );
            }
        },
    );
}

/// A short login must not remove recipients merely because it is a substring.
#[test]
fn reply_self_exclusion_is_not_a_substring_match() {
    let mbox = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         To: ed@example.com, edward@example.com, ted@example.com\n\
         Subject: group thread\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: String::from("set debug\nreply 1\nbody\n.\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("USER", "ed"), ("LOGNAME", "ed")],
        |_plan, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            for who in ["edward@example.com", "ted@example.com"] {
                assert!(
                    err.contains(who),
                    "login `ed` over-matched and dropped {}: {}",
                    who,
                    err
                );
            }
        },
    );
}

/// Envelope operands are bare addresses, not `Name <addr>` header values.
#[test]
fn envelope_recipients_carry_no_display_names() {
    let mbox = create_temp_mbox(
        "From a@example.com Fri Nov 29 10:00:00 2024\n\
         From: Bob Smith <bob@example.com>\n\
         To: me@example.com, carol@example.com\n\
         Subject: s\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: String::from("set debug\nreply 1\nbody\n.\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("USER", "me"), ("LOGNAME", "me")],
        |_plan, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            let envelope = err
                .lines()
                .find(|l| l.starts_with("Envelope:"))
                .unwrap_or_else(|| panic!("no envelope line: {}", err));
            assert!(
                !envelope.contains('<') && !envelope.contains("Bob Smith"),
                "a display name reached the envelope: {}",
                envelope
            );
            assert!(
                envelope.contains("bob@example.com"),
                "the sender's address is missing: {}",
                envelope
            );
        },
    );
}

/// A sender written `addr (Name)` reaches the envelope as the bare address.
#[test]
fn envelope_recipient_drops_a_comment() {
    let mbox = create_temp_mbox(
        "From a@example.com Fri Nov 29 10:00:00 2024\n\
         From: bob@example.com (Bob Smith)\n\
         To: me@example.com\n\
         Subject: s\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: String::from("set debug\nreply 1\nbody\n.\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("USER", "me"), ("LOGNAME", "me")],
        |_plan, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            let envelope = err
                .lines()
                .find(|l| l.starts_with("Envelope:"))
                .unwrap_or_else(|| panic!("no envelope line: {}", err));
            assert_eq!(envelope, "Envelope: bob@example.com");
        },
    );
}

/// Reply-all splits To: and Cc: only between addresses: a comma inside
/// a quoted display name is part of the name.
#[test]
fn reply_all_keeps_quoted_commas() {
    let mbox = create_temp_mbox(
        "From a@example.com Fri Nov 29 10:00:00 2024\n\
         From: Bob <bob@example.com>\n\
         To: me@example.com, \"Smith, John\" <john@example.com>\n\
         Cc: \"Doe, Ann\" <ann@example.com>\n\
         Subject: s\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: String::from("set debug\nreply 1\nbody\n.\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("USER", "me"), ("LOGNAME", "me")],
        |_plan, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            let envelope = err
                .lines()
                .find(|l| l.starts_with("Envelope:"))
                .unwrap_or_else(|| panic!("no envelope line: {}", err));
            assert_eq!(
                envelope,
                "Envelope: bob@example.com, john@example.com, ann@example.com"
            );
        },
    );
}

/// A Reply-To: naming several addresses gives each as a recipient.
#[test]
fn reply_to_list_is_split() {
    let mbox = create_temp_mbox(
        "From a@example.com Fri Nov 29 10:00:00 2024\n\
         From: Bob <bob@example.com>\n\
         To: me@example.com\n\
         Reply-To: \"Lists, All\" <l@example.com>, other@example.com\n\
         Subject: s\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: String::from("set debug\nReply 1\nbody\n.\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("USER", "me"), ("LOGNAME", "me")],
        |_plan, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            let envelope = err
                .lines()
                .find(|l| l.starts_with("Envelope:"))
                .unwrap_or_else(|| panic!("no envelope line: {}", err));
            assert_eq!(envelope, "Envelope: l@example.com, other@example.com");
        },
    );
}
