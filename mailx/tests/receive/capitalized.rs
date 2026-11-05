//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Capitalized command forms, distinct from their lowercase ones.

use crate::common::create_temp_mbox;
use plib::testing::{run_test_with_checker, run_test_with_checker_and_env, TestPlan};
use plib::tmp::NamedTempFile;

/// A mailbox with a header that `discard` can suppress.
pub(super) fn capitalized_fixture() -> NamedTempFile {
    create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: Alice <alice@example.com>\n\
         To: me@example.com, carol@example.com\n\
         Cc: dave@example.com\n\
         Subject: hello\n\
         X-Secret: hidden-header\n\
         \n\
         body one\n\
         \n",
    )
}

/// `Print` and `Type` override `discard`; `print` and `type` honor it
/// (spec 104756).
#[test]
fn capitalized_print_overrides_discard() {
    for (cmd, want_secret) in [
        ("print 1", false),
        ("Print 1", true),
        ("P 1", true),
        ("type 1", false),
        ("Type 1", true),
        ("T 1", true),
    ] {
        let mbox = capitalized_fixture();
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
                stdin_data: format!("discard X-Secret\n{}\nexit\n", cmd),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_plan, output| {
                let out = String::from_utf8_lossy(&output.stdout);
                assert_eq!(
                    out.contains("hidden-header"),
                    want_secret,
                    "`{}` handled the discarded header wrongly: {}",
                    cmd,
                    out
                );
            },
        );
    }
}

/// `Reply` addresses the sender; `reply` addresses everyone (spec 104905-104916).
#[test]
fn capitalized_reply_addresses_sender_only() {
    for (cmd, want_all) in [("reply 1", true), ("Reply 1", false), ("R 1", false)] {
        let mbox = capitalized_fixture();
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
                stdin_data: format!("set debug\n{}\nbody\n.\nexit\n", cmd),
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
                    .unwrap_or_else(|| panic!("no envelope for `{}`: {}", cmd, err));
                assert!(
                    envelope.contains("alice@example.com"),
                    "`{}` dropped the sender: {}",
                    cmd,
                    envelope
                );
                assert_eq!(
                    envelope.contains("carol@example.com"),
                    want_all,
                    "`{}` addressed the wrong set of recipients: {}",
                    cmd,
                    envelope
                );
            },
        );
    }
}

/// `Save` and `Copy` name a file after the author; only `Save` marks the
/// message saved (spec 104738-104742, 104958-104961).
#[test]
fn capitalized_save_and_copy_use_the_author_name() {
    for (cmd, want_saved) in [("Copy 1", false), ("Save 1", true), ("S 1", true)] {
        let dir = plib::tmp::TempDir::new().expect("temp dir");
        let mbox = dir.path().join("box");
        std::fs::write(
            &mbox,
            "From alice@example.com Fri Nov 29 10:00:00 2024\n\
             From: Alice <alice@example.com>\n\
             Subject: hello\n\
             \n\
             body one\n\
             \n",
        )
        .expect("write mbox");

        let out = std::process::Command::new(plib::testing::get_binary_path("mailx"))
            .args(["-n", "-N", "-f", mbox.to_str().unwrap()])
            .current_dir(dir.path())
            .env("LC_ALL", "C")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .and_then(|mut c| {
                use std::io::Write as _;
                c.stdin
                    .take()
                    .unwrap()
                    .write_all(format!("{}\nheaders\nexit\n", cmd).as_bytes())?;
                c.wait_with_output()
            })
            .expect("run mailx");

        assert!(
            dir.path().join("alice").exists(),
            "`{}` did not create a file named after the author: {}",
            cmd,
            String::from_utf8_lossy(&out.stderr)
        );
        let stdout = String::from_utf8_lossy(&out.stdout);
        let row = stdout
            .lines()
            .find(|l| l.contains("hello"))
            .unwrap_or_else(|| panic!("no header row for `{}`: {}", cmd, stdout));
        assert_eq!(
            row.contains('*'),
            want_saved,
            "`{}` marked the message saved incorrectly: {}",
            cmd,
            row
        );
    }
}

/// A command word is matched case-sensitively: `COPY` is not `Copy`.
#[test]
fn wrong_case_command_is_unknown() {
    let mbox = capitalized_fixture();
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
            stdin_data: String::from("COPY 1\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            assert!(
                err.contains("Unknown command: COPY"),
                "`COPY` should not match `Copy`: {}",
                err
            );
        },
    );
}
