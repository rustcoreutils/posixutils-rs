//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A source file under a write lease (a Samba oplock, a knfsd delegation)
//! makes a non-blocking open fail with EWOULDBLOCK. pax opens its sources
//! non-blocking so that a FIFO swapped in cannot hang it, but must then wait
//! for the lease to break, as a blocking open does, not fail the file.

use plib::tmp::TempDir;
use std::fs;
use std::os::fd::AsRawFd;
use std::path::Path;
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant};

/// Run pax with `args` in `dir` while this process holds a write lease on
/// `leased`, released a second after pax starts -- as a lease holder does
/// when the kernel tells it another open is waiting.
fn pax_against_a_lease(dir: &Path, args: &[&str], leased: &Path) -> Output {
    // The kernel signals the lease holder (this process) with SIGIO when the
    // lease must break; the default action would end the test run.
    unsafe { libc::signal(libc::SIGIO, libc::SIG_IGN) };
    let holder = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(leased)
        .unwrap();
    // A write lease is refused while anything else holds the inode, which
    // pending writeback of the file just written can do for a moment.
    holder.sync_all().unwrap();
    let deadline = Instant::now() + Duration::from_secs(10);
    while unsafe { libc::fcntl(holder.as_raw_fd(), libc::F_SETLEASE, libc::F_WRLCK) } != 0 {
        let e = std::io::Error::last_os_error();
        assert!(
            e.raw_os_error() == Some(libc::EAGAIN) && Instant::now() < deadline,
            "cannot take a lease: {e}"
        );
        std::thread::sleep(Duration::from_millis(10));
    }
    let child = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to run pax");
    std::thread::sleep(Duration::from_secs(1));
    unsafe { libc::fcntl(holder.as_raw_fd(), libc::F_SETLEASE, libc::F_UNLCK) };
    drop(holder);
    child.wait_with_output().unwrap()
}

#[test]
fn test_copy_waits_for_a_lease_on_the_source() {
    let temp = TempDir::new().unwrap();
    fs::create_dir(temp.path().join("src")).unwrap();
    fs::create_dir(temp.path().join("dst")).unwrap();
    fs::write(temp.path().join("src/f"), "leased data\n").unwrap();

    let out = pax_against_a_lease(
        temp.path(),
        &["-rw", "src", "dst"],
        &temp.path().join("src/f"),
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "stderr: {stderr}");
    assert_eq!(
        fs::read_to_string(temp.path().join("dst/src/f")).unwrap(),
        "leased data\n"
    );
}

#[test]
fn test_write_waits_for_a_lease_on_the_source() {
    let temp = TempDir::new().unwrap();
    fs::create_dir(temp.path().join("src")).unwrap();
    fs::write(temp.path().join("src/f"), "leased data\n").unwrap();

    let args = ["-w", "-f", "a.tar", "src"];
    let out = pax_against_a_lease(temp.path(), &args, &temp.path().join("src/f"));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "stderr: {stderr}");
    let list = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-v", "-f", "a.tar"])
        .current_dir(temp.path())
        .output()
        .unwrap();
    assert!(String::from_utf8_lossy(&list.stdout).contains("src/f"));
}
