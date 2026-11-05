//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Signals and terminals: hangups, interrupts in piped Send Mode, and end
//! of input on a terminal.

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

/// In Send Mode with standard input not a terminal, signals take their
/// standard action: an interrupt ends mailx.
#[test]
fn piped_send_mode_takes_the_standard_action() {
    use std::os::unix::process::ExitStatusExt;
    use std::time::{Duration, Instant};

    let home = plib::tmp::TempDir::new().unwrap();
    let rc = create_temp_mailrc("set debug\n");
    let mut child = Command::new(plib::testing::get_binary_path("mailx"))
        .args(["-s", "subject", "to@example.com"])
        .env("HOME", home.path())
        .env("MAILRC", rc.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn mailx");
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(b"body\n").unwrap();
    std::thread::sleep(Duration::from_millis(200));
    // SAFETY: kill sends a signal to the child we started.
    unsafe {
        libc::kill(child.id() as libc::pid_t, libc::SIGINT);
    }
    let deadline = Instant::now() + Duration::from_secs(10);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() > deadline {
            child.kill().unwrap();
            panic!("mailx ignored the interrupt");
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    drop(stdin);
    assert_eq!(status.signal(), Some(libc::SIGINT), "{:?}", status);
}

/// On a terminal, `ignoreeof` makes command mode answer end of input with a
/// reminder instead of quitting, as BSD mailx does.
#[cfg(target_os = "linux")]
#[test]
fn ignoreeof_holds_command_mode_open_on_a_terminal() {
    use std::io::Read;
    use std::os::fd::{FromRawFd, OwnedFd};
    use std::time::{Duration, Instant};

    let mbox = copy_test_data("testdata.mbox");
    let home = plib::tmp::TempDir::new().unwrap();
    let rc = create_temp_mailrc("set ignoreeof\n");
    let (mut master, slave) = unsafe {
        let (mut m, mut s) = (0, 0);
        // SAFETY: openpty fills both descriptors or fails.
        assert_eq!(
            libc::openpty(
                &mut m,
                &mut s,
                std::ptr::null_mut(),
                std::ptr::null(),
                std::ptr::null()
            ),
            0
        );
        (std::fs::File::from_raw_fd(m), OwnedFd::from_raw_fd(s))
    };
    let mut child = Command::new(plib::testing::get_binary_path("mailx"))
        .args(["-N", "-f", mbox.path().to_str().unwrap()])
        .env("HOME", home.path())
        .env("MAILRC", rc.path())
        .stdin(Stdio::from(slave.try_clone().unwrap()))
        .stdout(Stdio::from(slave.try_clone().unwrap()))
        .stderr(Stdio::from(slave))
        .spawn()
        .expect("spawn mailx");

    let mut seen = String::new();
    let mut read_until = |master: &mut std::fs::File, want: &str| {
        let deadline = Instant::now() + Duration::from_secs(10);
        let mut buf = [0u8; 4096];
        while !seen.contains(want) {
            assert!(
                Instant::now() < deadline,
                "never saw {:?} in {:?}",
                want,
                seen
            );
            let n = master
                .read(&mut buf)
                .unwrap_or_else(|e| panic!("mailx ended ({}): {:?}", e, seen));
            seen.push_str(&String::from_utf8_lossy(&buf[..n]));
        }
    };
    read_until(&mut master, "? ");
    master.write_all(b"\x04").unwrap(); // end of input, on a terminal
    read_until(&mut master, "Use \"quit\" to quit.");
    master.write_all(b"exit\n").unwrap();
    let status = child.wait().unwrap();
    assert!(status.success(), "{:?}: {}", status, seen);
}
