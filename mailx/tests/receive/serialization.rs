//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! mbox serialization: separators, `From ` quoting, `Status:`.

use crate::common::{create_temp_mbox, mailx};
use plib::tmp::NamedTempFile;

/// Saved messages must be separated by an empty line (spec 104420-104427).
///
/// The separator was smuggled through the message body, so it only appeared
/// when the source file happened to have one -- never for the last message of a
/// mailbox, and never for anything `save` had written.
#[test]
fn saved_messages_are_separated_by_a_blank_line() {
    // Deliberately no trailing empty line: that separator used to be smuggled
    // through the body, so a message without one produced output whose second
    // `From ` line abutted the first message's last body line.
    let mbox = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: only message\n\
         \n\
         single body line\n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();
    let out = NamedTempFile::new().expect("out file");
    let out_path = out.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &mbox_path],
        format!("save 1 {}\nsave 1 {}\nexit\n", out_path, out_path),
    );
    let saved = std::fs::read_to_string(&out_path).expect("read saved mailbox");
    let lines: Vec<&str> = saved.lines().collect();
    assert_eq!(
        lines.iter().filter(|l| l.starts_with("From ")).count(),
        2,
        "expected two messages: {:?}",
        saved
    );
    for (i, line) in lines.iter().enumerate() {
        if i > 0 && line.starts_with("From ") {
            assert_eq!(
                lines[i - 1],
                "",
                "a From line must be preceded by an empty line: {:?}",
                saved
            );
        }
    }
    assert!(
        !saved.contains("\n\n\nFrom "),
        "the separator was doubled: {:?}",
        saved
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Repeated quit cycles must not accumulate blank lines.
///
/// Normalizing on load and emitting exactly one separator on write is what
/// keeps the naive "always append an empty line" fix from growing the mailbox
/// by one line per session.
#[test]
fn repeated_quit_cycles_do_not_grow_the_mailbox() {
    let out = NamedTempFile::new().expect("out file");
    let out_path = out.path().to_str().unwrap().to_string();

    // Feed each round's output back in as the next round's input. If the writer
    // adds a separator the reader does not consume, the blank lines pile up.
    let mut previous = String::new();
    for round in 0..3 {
        let mbox = if round == 0 {
            create_temp_mbox(
                "From alice@example.com Fri Nov 29 10:00:00 2024\n\
                 From: alice@example.com\n\
                 Subject: only message\n\
                 \n\
                 single body line\n",
            )
        } else {
            create_temp_mbox(&previous)
        };
        let mbox_path = mbox.path().to_str().unwrap().to_string();
        std::fs::write(&out_path, "").expect("truncate out");

        let output = mailx(
            &["-n", "-N", "-f", &mbox_path],
            format!("save 1 {}\nexit\n", out_path),
        );

        assert_eq!(output.status.code(), Some(0), "exit status");

        let current = std::fs::read_to_string(&out_path).expect("read saved");
        if round > 0 {
            assert_eq!(
                current,
                previous,
                "round {} changed a mailbox that round {} had already written",
                round,
                round - 1
            );
        }
        previous = current;
    }
}

/// A body line beginning `From ` must round-trip through save unchanged.
#[test]
fn from_line_in_body_round_trips() {
    let mbox = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: quoting\n\
         \n\
         ordinary line\n\
         From here it gets interesting\n\
         >From already quoted\n\
         >>From doubly quoted\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();
    let out = NamedTempFile::new().expect("out file");
    let out_path = out.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &mbox_path],
        format!("save 1 {}\nexit\n", out_path),
    );
    let saved = std::fs::read_to_string(&out_path).expect("read saved");
    // Exactly one envelope line: the body's `From ` must stay escaped.
    assert_eq!(
        saved.lines().filter(|l| l.starts_with("From ")).count(),
        1,
        "an unescaped body line split the message: {:?}",
        saved
    );
    assert!(
        saved.contains(">From here it gets interesting"),
        "the body's From line lost its quoting: {:?}",
        saved
    );
    // `>From already quoted` on disk means `From already quoted` in
    // the message, so it comes back out with one `>`; the doubly
    // quoted line keeps its extra level. Quoting only `From ` and
    // never `>From ` lost a level on every round trip.
    assert!(
        saved.contains(">>From doubly quoted"),
        "a doubly quoted body line lost a level of quoting: {:?}",
        saved
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// A folded line after `Status:` belongs to it, not to the header before it.
#[test]
fn status_continuation_does_not_corrupt_the_previous_header() {
    let mbox = create_temp_mbox(
        "From a@example.com Fri Nov 29 10:00:00 2024\n\
         From: a@example.com\n\
         Subject: folded\n\
         X-Long: first\n\
         Status: RO\n\
         \tcontinued\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(&["-n", "-N", "-f", &mbox_path], "print 1\nexit\n");
    let out = String::from_utf8_lossy(&output.stdout);
    assert!(
        !out.contains("continued"),
        "a continuation of the dropped Status: header was glued onto \
         the header before it: {}",
        out
    );
    assert!(
        out.contains("X-Long: first"),
        "the preceding header was lost: {}",
        out
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// A mailbox stored with CRLF line endings keeps them when it is written
/// back: messages are kept as the bytes they were stored as.
#[test]
fn crlf_mailbox_keeps_its_line_endings() {
    let mbox = create_temp_mbox(
        "From a@example.com Fri Nov 29 10:00:00 2024\r\n\
         From: a@example.com\r\n\
         Subject: one\r\n\
         \r\n\
         first body\r\n\
         \r\n\
         From b@example.com Fri Nov 29 11:00:00 2024\r\n\
         From: b@example.com\r\n\
         Subject: two\r\n\
         \r\n\
         second body\r\n\
         \r\n",
    );
    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "print 1\nquit\n",
    );
    assert!(output.status.success());
    let after = std::fs::read(mbox.path()).unwrap();
    assert_eq!(
        String::from_utf8_lossy(&after),
        "From a@example.com Fri Nov 29 10:00:00 2024\r\n\
         From: a@example.com\r\n\
         Subject: one\r\n\
         Status: RO\r\n\
         \r\n\
         first body\r\n\
         \r\n\
         From b@example.com Fri Nov 29 11:00:00 2024\r\n\
         From: b@example.com\r\n\
         Subject: two\r\n\
         \r\n\
         second body\r\n\
         \r\n"
    );
}
