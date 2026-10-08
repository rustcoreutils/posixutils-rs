//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A directory member whose name someone else created first. POSIX lets pax
//! extract into an existing directory, but in a parent others can create
//! entries in, the directory found there may be anyone's renamed to the
//! member's name -- a private one of the user's own, say -- and giving it the
//! archive's mode would open it up.

use plib::tmp::TempDir;
use std::fs;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::process::{Command, Stdio};

/// An archive holding a directory `d`, mode 0755, with a file in it.
fn archive_with_open_directory(temp: &TempDir) -> std::path::PathBuf {
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d")).unwrap();
    fs::write(src.join("d/f"), "data\n").unwrap();
    fs::set_permissions(src.join("d"), fs::Permissions::from_mode(0o755)).unwrap();
    let mut write = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-w", "-f", "../a.tar"])
        .current_dir(&src)
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    write
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b"d\nd/f\n")
        .unwrap();
    drop(write.stdin.take());
    assert!(write.wait().unwrap().success());
    temp.path().join("a.tar")
}

#[test]
fn test_extract_leaves_a_renamed_in_private_directory_closed() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);

    // A destination anyone may create entries in, holding a private
    // directory of the user's own, renamed to the member's name before pax
    // runs.
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    fs::set_permissions(&dest, fs::Permissions::from_mode(0o777)).unwrap();
    fs::create_dir(dest.join("secrets")).unwrap();
    fs::write(dest.join("secrets/key"), "secret\n").unwrap();
    fs::set_permissions(dest.join("secrets"), fs::Permissions::from_mode(0o700)).unwrap();
    fs::rename(dest.join("secrets"), dest.join("d")).unwrap();

    let out = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-r", "-p", "e", "-f"])
        .arg(&archive)
        .current_dir(&dest)
        .stdin(Stdio::null())
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);

    let mode = fs::metadata(dest.join("d")).unwrap().permissions().mode() & 0o7777;
    assert_eq!(mode, 0o700, "the private directory was opened up");
    // Extracted into, as POSIX allows.
    assert!(dest.join("d/f").exists());
    assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
    assert!(
        stderr.contains("not applying owner, mode or times"),
        "stderr: {stderr}"
    );
}

/// In a destination only the user can create entries in, an existing
/// directory still takes the member's attributes, as POSIX describes.
#[test]
fn test_extract_stamps_an_existing_directory_in_a_private_destination() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = temp.path().join("dest");
    fs::create_dir_all(dest.join("d")).unwrap();
    fs::set_permissions(&dest, fs::Permissions::from_mode(0o755)).unwrap();
    fs::set_permissions(dest.join("d"), fs::Permissions::from_mode(0o700)).unwrap();

    let out = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-r", "-p", "e", "-f"])
        .arg(&archive)
        .current_dir(&dest)
        .stdin(Stdio::null())
        .output()
        .unwrap();

    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "stderr: {stderr}");
    let mode = fs::metadata(dest.join("d")).unwrap().permissions().mode() & 0o7777;
    assert_eq!(mode, 0o755);
}
