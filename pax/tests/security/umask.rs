//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A directory pax makes in a destination others can write is checked to be
//! empty before it is used, which takes reading it. A umask that removes the
//! owner's read permission must not turn that check into a failure.

use plib::tmp::TempDir;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{Command, Output, Stdio};

/// Run pax with `args` in `dir` under umask 0400.
fn pax_under_umask(dir: &Path, args: &[&str], stdin: Stdio) -> Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_pax"));
    command.args(args).current_dir(dir).stdin(stdin);
    // SAFETY: umask is async-signal-safe.
    unsafe {
        command.pre_exec(|| {
            libc::umask(0o400);
            Ok(())
        });
    }
    command.output().unwrap()
}

/// A source tree `src/d/f`, and a destination `dst` anyone may write.
fn setup() -> TempDir {
    let temp = TempDir::new().unwrap();
    fs::create_dir_all(temp.path().join("src/d")).unwrap();
    fs::write(temp.path().join("src/d/f"), "data\n").unwrap();
    fs::create_dir(temp.path().join("dst")).unwrap();
    fs::set_permissions(temp.path().join("dst"), fs::Permissions::from_mode(0o777)).unwrap();
    temp
}

/// Let the test's cleanup read and remove whatever the umask left unreadable.
fn open_up(dir: &Path) {
    for entry in fs::read_dir(dir).into_iter().flatten().flatten() {
        let path = entry.path();
        if path.is_dir() {
            let _ = fs::set_permissions(&path, fs::Permissions::from_mode(0o700));
            open_up(&path);
        }
    }
}

#[test]
fn test_copy_makes_directories_under_a_umask_denying_owner_read() {
    let temp = setup();
    let out = pax_under_umask(temp.path(), &["-rw", "src", "dst"], Stdio::null());
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    open_up(&temp.path().join("dst"));
    assert!(out.status.success(), "stderr: {stderr}");
    // The umask leaves the file unreadable too; its size tells it was copied.
    assert_eq!(
        fs::metadata(temp.path().join("dst/src/d/f")).unwrap().len(),
        5
    );
}

#[test]
fn test_extract_makes_intermediate_directories_under_a_umask_denying_owner_read() {
    let temp = setup();
    // An archive of the file alone: its directories are made only to hold it.
    let mut write = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-w", "-f", "a.tar"])
        .current_dir(temp.path())
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    std::io::Write::write_all(write.stdin.as_mut().unwrap(), b"src/d/f\n").unwrap();
    drop(write.stdin.take());
    assert!(write.wait().unwrap().success());

    let out = pax_under_umask(
        &temp.path().join("dst"),
        &["-r", "-f", "../a.tar"],
        Stdio::null(),
    );
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    open_up(&temp.path().join("dst"));
    assert!(out.status.success(), "stderr: {stderr}");
    assert_eq!(
        fs::metadata(temp.path().join("dst/src/d/f")).unwrap().len(),
        5
    );
}
