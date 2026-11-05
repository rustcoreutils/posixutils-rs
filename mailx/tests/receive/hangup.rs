//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A hangup (SIGHUP), in command mode and while composing.

use crate::common::{copy_test_data, create_temp_mailrc};
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};

/// Start mailx in Receive Mode on `mbox`, with `home` as HOME.
fn spawn(mbox: &str, home: &std::path::Path, mailrc: &str) -> Child {
    Command::new(plib::testing::get_binary_path("mailx"))
        .args(["-N", "-f", mbox])
        .env("HOME", home)
        .env("MAILRC", mailrc)
        .env("LC_ALL", "C")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn mailx")
}

/// Feed `input`, wait until a line containing `marker` is written -- so
/// mailx is reading again -- and then hang it up.
fn hang_up_after(child: &mut Child, input: &str, marker: &str) -> std::process::ExitStatus {
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(input.as_bytes()).unwrap();
    stdin.flush().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());
    let mut line = String::new();
    while !line.contains(marker) {
        line.clear();
        assert!(
            stdout.read_line(&mut line).unwrap() > 0,
            "mailx exited early"
        );
    }
    // SAFETY: kill sends a signal to the child we started.
    unsafe {
        libc::kill(child.id() as libc::pid_t, libc::SIGHUP);
    }
    let status = child.wait().unwrap();
    drop(stdin);
    status
}

/// In command mode a hangup ends mailx at once, with nothing written back:
/// the message read this session is not marked read in the mailbox.
#[test]
fn hangup_in_command_mode_writes_nothing_back() {
    let mbox = copy_test_data("testdata.mbox");
    let before = std::fs::read(mbox.path()).unwrap();
    let home = plib::tmp::TempDir::new().unwrap();
    let rc = create_temp_mailrc("");
    let mut child = spawn(
        mbox.path().to_str().unwrap(),
        home.path(),
        rc.path().to_str().unwrap(),
    );
    let status = hang_up_after(&mut child, "print 3\necho READY\n", "READY");
    assert_eq!(status.code(), Some(1), "{:?}", status);
    assert_eq!(
        std::fs::read(mbox.path()).unwrap(),
        before,
        "the mailbox changed"
    );
}

/// While composing, a hangup saves the message to dead.letter, then ends
/// mailx.
#[test]
fn hangup_while_composing_saves_the_message() {
    let mbox = copy_test_data("testdata.mbox");
    let home = plib::tmp::TempDir::new().unwrap();
    let rc = create_temp_mailrc("");
    let mut child = spawn(
        mbox.path().to_str().unwrap(),
        home.path(),
        rc.path().to_str().unwrap(),
    );
    let status = hang_up_after(
        &mut child,
        "mail to@example.com\nunsent draft\n~p\n",
        "-------",
    );
    assert_eq!(status.code(), Some(1), "{:?}", status);
    let dead = std::fs::read_to_string(home.path().join("dead.letter")).unwrap_or_default();
    assert!(dead.contains("unsent draft"), "dead.letter: {:?}", dead);
}
