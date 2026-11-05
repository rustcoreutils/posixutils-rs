//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Mailbox locking and file permissions.

use crate::common::{copy_test_data, mailx};
use std::io::Write;

/// mailx waits for the `fcntl` lock that Linux delivery agents hold.
///
/// Postfix `local`, Exim, and procmail lock the spool with `fcntl`, which on
/// Linux does not exclude `flock`. Holding only `flock`, mailx read the
/// mailbox mid-delivery and truncated and rewrote it under an agent that was
/// still appending, losing the mail being delivered.
#[cfg(target_os = "linux")]
#[test]
fn mailbox_honors_fcntl_lock_of_delivery_agent() {
    use std::io::{Read as _, Seek as _};
    use std::os::unix::io::AsRawFd;
    use std::process::{Command, Stdio};

    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let mbox = dir.path().join("spool");
    let original = "From a@b Mon Jan  1 00:00:00 2024\nFrom: a@b\nSubject: one\n\nfirst\n\n\
                    From c@d Mon Jan  1 00:00:00 2024\nFrom: c@d\nSubject: two\n\nsecond\n\n";
    std::fs::write(&mbox, original).expect("write mbox");

    // Play the delivery agent: hold a write lock on the whole spool.
    let mut agent = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(&mbox)
        .expect("open spool");
    let set_lock = |file: &std::fs::File, kind: libc::c_int| {
        // SAFETY: an all-zero flock is valid; start 0, length 0 is the whole file.
        let mut fl: libc::flock = unsafe { std::mem::zeroed() };
        fl.l_type = kind as libc::c_short;
        fl.l_whence = libc::SEEK_SET as libc::c_short;
        // SAFETY: the fd is open and `fl` outlives the call.
        let rc = unsafe { libc::fcntl(file.as_raw_fd(), libc::F_SETLK, &fl) };
        assert_eq!(rc, 0, "fcntl lock: {}", std::io::Error::last_os_error());
    };
    set_lock(&agent, libc::F_WRLCK);

    let mut child = Command::new(plib::testing::get_binary_path("mailx"))
        .args(["-n", "-f", mbox.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn mailx");
    child
        .stdin
        .take()
        .expect("stdin")
        .write_all(b"d 1\nquit\n")
        .expect("write script");

    // Read back through the agent's own descriptor: closing any other one
    // this process had open on the spool would drop the lock.
    std::thread::sleep(std::time::Duration::from_millis(500));
    let mut during = String::new();
    agent.read_to_string(&mut during).expect("read spool");
    let finished_early = child.try_wait().expect("try_wait").is_some();

    // The delivery completes, then the agent lets go.
    agent.seek(std::io::SeekFrom::End(0)).expect("seek");
    agent
        .write_all(b"From e@f Mon Jan  1 00:00:00 2024\nFrom: e@f\nSubject: three\n\nthird\n\n")
        .expect("deliver");
    set_lock(&agent, libc::F_UNLCK);

    let output = child.wait_with_output().expect("wait for mailx");
    let after = std::fs::read_to_string(&mbox)
        .unwrap_or_else(|e| panic!("read spool: {}: {:?}", e, output));

    assert_eq!(
        during, original,
        "mailx rewrote the spool while the delivery agent held its lock"
    );
    assert!(!finished_early, "mailx did not wait for the lock");
    assert!(output.status.success(), "mailx failed: {:?}", output);
    let summary = String::from_utf8_lossy(&output.stdout);
    assert!(
        summary.contains("three"),
        "the mailbox was read before the delivery finished: {}",
        summary
    );
    assert!(!after.contains("first"), "deleted message kept: {}", after);
    assert!(
        after.contains("second") && after.contains("third"),
        "mail was lost: {}",
        after
    );
}

/// An interrupt while `quit` waits for a delivery agent's lock does not cut
/// the quit short, as BSD mailx ignores interrupts for its quit: the session's
/// work is written back once the lock is free, not thrown away.
#[cfg(target_os = "linux")]
#[test]
fn interrupt_during_quit_lock_wait_is_held() {
    use std::os::unix::io::AsRawFd;
    use std::process::{Command, Stdio};

    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let mbox = dir.path().join("spool");
    std::fs::write(
        &mbox,
        "From a@b Mon Jan  1 00:00:00 2024\nFrom: a@b\nSubject: one\n\nfirst\n\n\
         From c@d Mon Jan  1 00:00:00 2024\nFrom: c@d\nSubject: two\n\nsecond\n\n",
    )
    .expect("write mbox");

    let mut child = Command::new(plib::testing::get_binary_path("mailx"))
        .args(["-n", "-N", "-f", mbox.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn mailx");
    let mut stdin = child.stdin.take().expect("stdin");
    stdin.write_all(b"d 1\n").expect("write d");
    std::thread::sleep(std::time::Duration::from_millis(300));

    // Play the delivery agent once mailx has read the mailbox.
    let agent = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(&mbox)
        .expect("open spool");
    let set_lock = |kind: libc::c_int| {
        // SAFETY: an all-zero flock is valid; start 0, length 0 is the whole file.
        let mut fl: libc::flock = unsafe { std::mem::zeroed() };
        fl.l_type = kind as libc::c_short;
        fl.l_whence = libc::SEEK_SET as libc::c_short;
        // SAFETY: the fd is open and `fl` outlives the call.
        let rc = unsafe { libc::fcntl(agent.as_raw_fd(), libc::F_SETLK, &fl) };
        assert_eq!(rc, 0, "fcntl lock: {}", std::io::Error::last_os_error());
    };
    set_lock(libc::F_WRLCK);

    stdin.write_all(b"quit\n").expect("write quit");
    drop(stdin);
    std::thread::sleep(std::time::Duration::from_millis(300));
    // SAFETY: kill takes a pid and a signal number; the child is ours.
    unsafe {
        libc::kill(child.id() as libc::pid_t, libc::SIGINT);
    }
    std::thread::sleep(std::time::Duration::from_millis(300));
    set_lock(libc::F_UNLCK);

    let output = child.wait_with_output().expect("wait for mailx");
    drop(agent);
    let after = std::fs::read_to_string(&mbox).expect("read spool");
    assert!(output.status.success(), "quit failed: {:?}", output);
    assert!(!after.contains("first"), "deleted message kept: {}", after);
    assert!(after.contains("second"), "mail was lost: {}", after);
}

/// Files mailx creates to hold mail are private to the user, whatever the
/// umask would otherwise allow.
#[test]
fn created_mail_files_are_private() {
    use std::os::unix::fs::PermissionsExt;

    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let saved = dir.path().join("saved");
    let written = dir.path().join("written");
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &mbox_path],
        format!(
            "save 1 {}\nwrite 2 {}\nexit\n",
            saved.display(),
            written.display()
        ),
    );
    for path in [&saved, &written] {
        let mode = std::fs::metadata(path)
            .unwrap_or_else(|e| panic!("{}: {}", path.display(), e))
            .permissions()
            .mode();
        assert_eq!(
            mode & 0o077,
            0,
            "{} is readable by others: {:o}",
            path.display(),
            mode
        );
    }
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// mailx waits for a delivery agent's `<mailbox>.lock`, and takes and
/// removes its own.
#[test]
fn mailbox_honors_dotlock() {
    use std::process::{Command, Stdio};

    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let mbox = dir.path().join("spool");
    let lock = dir.path().join("spool.lock");
    std::fs::write(
        &mbox,
        "From a@b Mon Jan  1 00:00:00 2024\nFrom: a@b\nSubject: one\n\nfirst\n\n",
    )
    .expect("write mbox");
    std::fs::write(&lock, "").expect("write lock");

    let mut child = Command::new(plib::testing::get_binary_path("mailx"))
        .args(["-n", "-H", "-f", mbox.to_str().unwrap()])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn mailx");

    std::thread::sleep(std::time::Duration::from_millis(1500));
    let finished_early = child.try_wait().expect("try_wait").is_some();
    std::fs::remove_file(&lock).expect("release lock");

    let output = child.wait_with_output().expect("wait for mailx");
    assert!(!finished_early, "mailx did not wait for the dotlock");
    assert!(output.status.success(), "mailx failed: {:?}", output);
    assert!(String::from_utf8_lossy(&output.stdout).contains("one"));
    assert!(!lock.exists(), "mailx left its dotlock behind");
}

/// A dotlock older than five minutes was left by a holder that died; it is
/// broken rather than waited on forever.
#[test]
fn mailbox_breaks_stale_dotlock() {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let mbox = dir.path().join("spool");
    let lock = dir.path().join("spool.lock");
    std::fs::write(
        &mbox,
        "From a@b Mon Jan  1 00:00:00 2024\nFrom: a@b\nSubject: one\n\nfirst\n\n",
    )
    .expect("write mbox");
    let file = std::fs::File::create(&lock).expect("write lock");
    let old = std::time::SystemTime::now() - std::time::Duration::from_secs(600);
    file.set_modified(old).expect("age lock");
    drop(file);

    let started = std::time::Instant::now();
    let output = mailx(&["-n", "-H", "-f", mbox.to_str().unwrap()], "");
    assert!(output.status.success(), "mailx failed: {:?}", output);
    assert!(
        started.elapsed() < std::time::Duration::from_secs(5),
        "mailx waited on a stale dotlock"
    );
    assert!(!lock.exists(), "the stale dotlock was not removed");
}

/// Where the mailbox's directory is not writable, as a spool directory
/// writable only by group mail, the dotlock is skipped, not waited on.
#[test]
fn mailbox_without_dotlock_access_still_opens() {
    use std::os::unix::fs::PermissionsExt;

    // Root can create files in any directory; the case cannot arise.
    // SAFETY: geteuid has no preconditions.
    if unsafe { libc::geteuid() } == 0 {
        return;
    }
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let spool = dir.path().join("spool");
    std::fs::create_dir(&spool).expect("mkdir");
    let mbox = spool.join("user");
    std::fs::write(
        &mbox,
        "From a@b Mon Jan  1 00:00:00 2024\nFrom: a@b\nSubject: one\n\nfirst\n\n",
    )
    .expect("write mbox");
    std::fs::set_permissions(&spool, std::fs::Permissions::from_mode(0o555)).expect("chmod");

    let output = mailx(&["-n", "-H", "-f", mbox.to_str().unwrap()], "");
    std::fs::set_permissions(&spool, std::fs::Permissions::from_mode(0o755)).expect("chmod");
    assert!(output.status.success(), "mailx failed: {:?}", output);
    assert!(String::from_utf8_lossy(&output.stdout).contains("one"));
}
