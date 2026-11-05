//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Mail that is not UTF-8, and control characters in headers.

use crate::common::{copy_test_data, create_temp_mbox, mailx};
use std::io::Write;

/// A message carrying bytes that are not UTF-8 does not make the mailbox
/// unreadable, and those bytes survive a rewrite and a `copy`.
///
/// Mail is 8-bit. The mailbox used to be read as a string, so a single
/// Latin-1 message from anyone locked its owner out of all their mail.
#[test]
fn eight_bit_mailbox_loads_and_round_trips() {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let mbox = dir.path().join("in.mbox");
    let copied = dir.path().join("copied.mbox");
    let first: &[u8] =
        b"From a@b Mon Jan  1 00:00:00 2024\nFrom: a@b\nSubject: caf\xe9\n\nbody \xe9\xff\n\n";
    let second: &[u8] = b"From c@d Mon Jan  1 00:00:00 2024\nFrom: c@d\nSubject: two\n\nsecond\n\n";
    std::fs::write(&mbox, [first, second].concat()).expect("write mbox");

    let output = mailx(
        &["-n", "-N", "-f", mbox.to_str().unwrap()],
        format!("size 1\ncopy 1 {}\nd 2\nquit\n", copied.display()),
    );
    assert_eq!(
        output.status.code(),
        Some(0),
        "an 8-bit mailbox was refused: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let out = String::from_utf8_lossy(&output.stdout);
    assert!(out.contains("1: 68 bytes"), "size miscounted: {}", out);
    assert!(
        out.contains("1 messages 68 bytes"),
        "copy miscounted: {}",
        out
    );

    let copy = std::fs::read(&copied).expect("read copy");
    assert_eq!(copy, first, "copy did not keep the 8-bit bytes");
    let rewritten = std::fs::read(&mbox).expect("read rewritten mailbox");
    assert_eq!(
        rewritten,
        first,
        "rewrite did not keep the 8-bit bytes: {}",
        String::from_utf8_lossy(&rewritten)
    );

    // `write` puts out the body alone, with the same bytes.
    let body = dir.path().join("body.txt");
    let out = std::process::Command::new(plib::testing::get_binary_path("mailx"))
        .args(["-n", "-N", "-f", mbox.to_str().unwrap()])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .and_then(|mut child| {
            let script = format!("write 1 {}\nexit\n", body.display());
            child
                .stdin
                .take()
                .expect("stdin")
                .write_all(script.as_bytes())?;
            child.wait_with_output()
        })
        .expect("run mailx");
    assert!(out.status.success(), "write failed: {:?}", out);
    assert_eq!(std::fs::read(&body).expect("read body"), b"body \xe9\xff\n");
}

/// The header summary does not write a sender's control characters to the
/// terminal.
///
/// It is shown for every message without the user choosing to read one, so
/// an escape sequence in `From:` or `Subject:` could clear or retitle the
/// terminal, or redraw the summary to hide or forge entries.
#[test]
fn header_summary_shows_control_characters_as_question_marks() {
    let mbox = create_temp_mbox(
        "From x@y Mon Jan  1 00:00:00 2024\n\
         From: \x1b]0;pwned\x07evil@example.com\n\
         Subject: hi\x1b[2J\x1b[Hgone\r\n\
         \n\
         body\n\
         \n",
    );

    let output = mailx(&["-H", "-n", "-f", mbox.path().to_str().unwrap()], "");
    let out = String::from_utf8_lossy(&output.stdout);
    assert!(
        !out.contains('\x1b') && !out.contains('\x07'),
        "control characters reached the terminal: {:?}",
        out
    );
    assert!(out.contains("hi?[2J?[Hgone"), "subject: {:?}", out);
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Messages are kept as stored bytes: a genuine private-use character comes
/// back as itself, beside raw 8-bit bytes, and `print` shows the bytes as
/// stored rather than a stand-in for them.
#[test]
fn stored_bytes_round_trip_exactly() {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let mbox = dir.path().join("in.mbox");
    let copied = dir.path().join("copied.mbox");
    // U+F7E9 in UTF-8 is EF 9F A9; the stand-in scheme read it as byte E9.
    let first: &[u8] = b"From a@b Mon Jan  1 00:00:00 2024\nFrom: a@b\nSubject: s\n\n\
        pua \xef\x9f\xa9 raw \xe9\n\n";
    let second: &[u8] = b"From c@d Mon Jan  1 00:00:00 2024\nFrom: c@d\nSubject: two\n\nsecond\n\n";
    std::fs::write(&mbox, [first, second].concat()).expect("write mbox");

    let output = mailx(
        &["-n", "-N", "-f", mbox.to_str().unwrap()],
        format!("print 1\ncopy 1 {}\nd 2\nquit\n", copied.display()),
    );
    let wanted: &[u8] = b"pua \xef\x9f\xa9 raw \xe9\n";
    assert!(
        output.stdout.windows(wanted.len()).any(|w| w == wanted),
        "print did not show the stored bytes: {:?}",
        String::from_utf8_lossy(&output.stdout)
    );
    assert_eq!(output.status.code(), Some(0), "exit status");

    // Printing read the message, so both copies record that in `Status:`.
    let read: &[u8] = b"From a@b Mon Jan  1 00:00:00 2024\nFrom: a@b\nSubject: s\n\
        Status: RO\n\npua \xef\x9f\xa9 raw \xe9\n\n";
    assert_eq!(std::fs::read(&copied).expect("read copy"), read);
    assert_eq!(std::fs::read(&mbox).expect("read rewrite"), read);
}

/// Input mode keeps typed lines and `~r` files as their bytes, 8-bit or not.
#[test]
fn input_mode_keeps_eight_bit_bytes() {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let inserted = dir.path().join("in.txt");
    let out = dir.path().join("out.txt");
    std::fs::write(&inserted, b"read \xe9\n").expect("write input");
    let mbox = copy_test_data("testdata-single.mbox");

    let mut stdin = b"mail to@example.com\ntyped \xff\n".to_vec();
    stdin.extend_from_slice(
        format!(
            "~r {}\n~w {}\n~x\nexit\n",
            inserted.display(),
            out.display()
        )
        .as_bytes(),
    );
    let output = mailx(&["-n", "-N", "-f", mbox.path().to_str().unwrap()], &stdin);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        std::fs::read(&out).expect("read ~w output"),
        b"typed \xff\nread \xe9\n"
    );
}
