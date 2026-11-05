//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Message states and the `hold`, `preserve`, `mbox` dispositions.

use crate::common::{copy_test_data, create_temp_mbox, mailx, mailx_env};
use plib::tmp::NamedTempFile;

/// A message with no Status: header is `new` (shown as `N`); a `:n` selector
/// matches only such messages.
#[test]
fn new_state_and_n_selector() {
    let body = "From new@example.com Mon Jan  1 10:00:00 2024\n\
                From: new@example.com\n\
                Subject: brand new\n\
                \n\
                fresh\n\
                \n\
                From old@example.com Mon Jan  1 09:00:00 2024\n\
                From: old@example.com\n\
                Status: RO\n\
                Subject: already read\n\
                \n\
                stale\n";
    let mbox = create_temp_mbox(body);

    // `from :n` lists header lines of new messages only.
    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "from :n\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("new@example.com"),
        ":n should match the new message: {}",
        stdout
    );
    assert!(
        !stdout.contains("old@example.com"),
        ":n must not match the already-read message: {}",
        stdout
    );
    // The new message shows the `N` state character.
    assert!(
        stdout
            .lines()
            .any(|l| l.chars().nth(1) == Some('N') && l.contains("new@example.com")),
        "new message should display state char N: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `mbox` forces a message into the secondary mbox at quit even when the `hold`
/// variable is set. Uses MAIL to make a temp file the system mailbox.
#[test]
fn mbox_overrides_hold_variable() {
    let sys = create_temp_mbox(
        "From sender@example.com Mon Jan  1 10:00:00 2024\n\
         From: sender@example.com\n\
         Status: RO\n\
         Subject: keep me in mbox\n\
         \n\
         body\n",
    );
    let mbox_file = NamedTempFile::new().expect("mbox file");
    let sys_path = sys.path().to_str().unwrap().to_string();
    let mbox_path = mbox_file.path().to_str().unwrap().to_string();

    // No -f: the MAIL file is the *system* mailbox, so mbox is allowed.
    let output = mailx_env(
        &["-n", "-N"],
        "set hold\nmbox 1\nquit\n",
        &[("MAIL", &sys_path), ("MBOX", &mbox_path)],
    );
    assert!(output.status.success());
    let saved = std::fs::read_to_string(&mbox_path).unwrap_or_default();
    assert!(
        saved.contains("sender@example.com"),
        "mbox should force the message into the secondary mbox despite hold: {:?}",
        saved
    );
}

/// `mbox` is rejected (diagnostic) when not reading the system mailbox.
#[test]
fn mbox_rejected_outside_system_mailbox() {
    let mbox = copy_test_data("testdata.mbox");

    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "mbox 1\nquit\n",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("system mailbox"),
        "mbox outside the system mailbox should be diagnosed: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Read state must survive a reopen, via the `Status:` header (spec 104625).
#[test]
fn read_state_survives_a_reopen() {
    let folder = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: first\n\
         \n\
         body one\n\
         \n\
         From bob@example.org Fri Nov 29 11:00:00 2024\n\
         From: bob@example.org\n\
         Subject: second\n\
         \n\
         body two\n\
         \n",
    );
    let folder_path = folder.path().to_str().unwrap().to_string();

    // Read message 1, delete message 2 so the folder is rewritten.
    let output = mailx(
        &["-n", "-N", "-f", &folder_path],
        "print 1\ndelete 2\nquit\n",
    );
    let after = std::fs::read_to_string(&folder_path).expect("folder");
    assert!(
        after.contains("Status:"),
        "read state was never written back: {:?}",
        after
    );
    assert_eq!(output.status.code(), Some(0), "exit status");

    // Reopening must show it as read, not new.
    let output = mailx(&["-n", "-f", &folder_path], "exit\n");
    let out = String::from_utf8_lossy(&output.stdout);
    let row = out
        .lines()
        .find(|l| l.contains("first"))
        .unwrap_or_else(|| panic!("no header row for message 1: {}", out));
    // The state is the row's second column; the date may hold `Nov`.
    assert!(
        row.chars().nth(1) != Some('N'),
        "a message read in the previous session came back as new: {}",
        row
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `undelete` changes a message "from deleted to read", whatever its state
/// before it was deleted.
#[test]
fn undelete_marks_the_message_read() {
    let mbox = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: never read\n\
         \n\
         body one\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &mbox_path],
        "delete 1\nundelete 1\nheaders\nexit\n",
    );
    let out = String::from_utf8_lossy(&output.stdout);
    let row = out
        .lines()
        .find(|l| l.contains("never read"))
        .unwrap_or_else(|| panic!("no header row: {}", out));
    // The state is the row's second column; the date may hold `Nov`.
    assert!(
        row.chars().nth(1) == Some('R'),
        "undelete should leave the message read: {}",
        row
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `preserve` must not erase read state: `:r` still has to match afterwards.
#[test]
fn preserve_keeps_read_state() {
    let sys = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: read then held\n\
         \n\
         body one\n\
         \n",
    );
    let sys_path = sys.path().to_str().unwrap().to_string();

    let output = mailx_env(
        &["-n", "-N"],
        "print 1\npreserve 1\nfrom :r\nexit\n",
        &[("MAIL", &sys_path)],
    );
    let out = String::from_utf8_lossy(&output.stdout);
    assert!(
        out.contains("read then held"),
        "`preserve` erased the read state, so `:r` no longer matched: {}",
        out
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// The first new message is current on entry even when an unread one comes
/// before it; an unread message is current only when none is new.
#[test]
fn first_new_message_is_current_before_unread_ones() {
    let mbox = create_temp_mbox(
        "From old@example.com Mon Jan  1 09:00:00 2024\n\
         From: old@example.com\n\
         Status: O\n\
         Subject: seen before\n\
         \n\
         stale\n\
         \n\
         From new@example.com Mon Jan  1 10:00:00 2024\n\
         From: new@example.com\n\
         Subject: brand new\n\
         \n\
         fresh\n",
    );
    let output = mailx(&["-n", "-H", "-f", mbox.path().to_str().unwrap()], "");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let current: Vec<&str> = stdout.lines().filter(|l| l.starts_with('>')).collect();
    assert_eq!(current.len(), 1, "one current message: {}", stdout);
    assert!(
        current[0].contains("new@example.com"),
        "the new message should be current: {}",
        stdout
    );
}

/// The header summary shown on entry is the page holding the current
/// message, so the first new message is on screen.
#[test]
fn entry_summary_shows_the_page_with_the_current_message() {
    let mut content = String::new();
    for (n, status) in [(1, "Status: RO\n"), (2, "Status: RO\n"), (3, "")] {
        content.push_str(&format!(
            "From m{n}@example.com Mon Jan  1 0{n}:00:00 2024\n\
             From: m{n}@example.com\n\
             {status}Subject: message {n}\n\n\
             body\n\n"
        ));
    }
    let mbox = create_temp_mbox(&content);
    let mailrc = crate::common::create_temp_mailrc("set screen=2\n");
    let output = mailx_env(
        &["-f", mbox.path().to_str().unwrap()],
        "exit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.lines().any(|l| l.starts_with(">N  3")),
        "the page with new message 3 should be shown: {}",
        stdout
    );
    assert!(!stdout.contains("message 1"), "{}", stdout);
}
