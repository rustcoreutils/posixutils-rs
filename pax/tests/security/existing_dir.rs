//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A directory member whose name is already a directory. POSIX lets pax
//! extract into it. Like libarchive, pax changes such a directory's mode only
//! under `-p p` (or `-p e`), and its owner only under `-p o`; and then only
//! where nobody else could have created the name first -- in a parent others
//! can create entries in, the directory there may be anyone's renamed to the
//! member's name, a private one of the user's own included, and giving it the
//! archive's mode would open it up.

use plib::tmp::TempDir;
use std::fs;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};

/// A source tree with a directory `d`, mode 0755, holding a file.
fn source_tree(temp: &TempDir) -> PathBuf {
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d")).unwrap();
    fs::write(src.join("d/f"), "data\n").unwrap();
    fs::set_permissions(src.join("d"), fs::Permissions::from_mode(0o755)).unwrap();
    src
}

/// An archive of `source_tree`'s `d` and `d/f`.
fn archive_with_open_directory(temp: &TempDir) -> PathBuf {
    let src = source_tree(temp);
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

/// A destination of mode `mode` holding an existing directory `d` of mode
/// 0700 with a file in it -- in the attack, the user's own private
/// `secrets`, renamed to the member's name before pax runs.
fn dest_with_private_d(temp: &TempDir, mode: u32) -> PathBuf {
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    fs::set_permissions(&dest, fs::Permissions::from_mode(mode)).unwrap();
    fs::create_dir(dest.join("secrets")).unwrap();
    fs::write(dest.join("secrets/key"), "secret\n").unwrap();
    fs::set_permissions(dest.join("secrets"), fs::Permissions::from_mode(0o700)).unwrap();
    fs::rename(dest.join("secrets"), dest.join("d")).unwrap();
    dest
}

/// Run pax with `args` in `dir`.
fn pax(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::null())
        .output()
        .unwrap()
}

fn mode_of(path: &Path) -> u32 {
    fs::metadata(path).unwrap().permissions().mode() & 0o7777
}

const DIAGNOSTIC: &str = "not applying owner, mode or times";

/// Without -p p, an existing directory keeps its mode: nothing is wrong, and
/// nothing is said.
#[test]
fn test_extract_without_p_leaves_an_existing_directory_mode_alone() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o777);

    let out = pax(&dest, &["-r", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert!(!stderr.contains(DIAGNOSTIC), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o700);
    assert!(dest.join("d/f").exists());
}

/// With -p e, in a destination others can create entries in, the directory
/// found there may be anyone's renamed to the member's name: it keeps its
/// own attributes, and that is diagnosed.
#[test]
fn test_extract_pe_leaves_a_renamed_in_private_directory_closed() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o777);

    let out = pax(&dest, &["-r", "-p", "e", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        mode_of(&dest.join("d")),
        0o700,
        "the private directory was opened up"
    );
    // Extracted into, as POSIX allows.
    assert!(dest.join("d/f").exists());
    assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
    assert!(stderr.contains(DIAGNOSTIC), "stderr: {stderr}");
}

/// Re-extracting into a group-writable destination (a umask of 002): without
/// -p the directories found there are left as they are, with no error.
#[test]
fn test_reextract_into_a_group_writable_destination_without_p() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o775);

    let out = pax(&dest, &["-r", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o700);
}

/// The same with -p e: group members could have created the name first.
#[test]
fn test_reextract_into_a_group_writable_destination_with_pe() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o775);

    let out = pax(&dest, &["-r", "-p", "e", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
    assert!(stderr.contains(DIAGNOSTIC), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o700);
}

/// In a destination only the user can create entries in, -p e gives an
/// existing directory the member's mode, as POSIX describes.
#[test]
fn test_extract_pe_stamps_an_existing_directory_in_a_private_destination() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o755);

    let out = pax(&dest, &["-r", "-p", "e", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o755);
}

/// And without -p p, even there, it keeps its own.
#[test]
fn test_extract_without_p_keeps_an_existing_directory_mode_in_a_private_destination() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o755);

    let out = pax(&dest, &["-r", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o700);
}

/// Copy mode treats an existing destination directory the same way: left
/// alone without -p, and under -p e only where nobody else can create
/// entries beside it.
#[test]
fn test_copy_onto_an_existing_directory_follows_p() {
    for (privs, parent, code, mode) in [
        (None, 0o777, 0, 0o700),
        (Some("e"), 0o777, 1, 0o700),
        (Some("e"), 0o755, 0, 0o755),
    ] {
        let temp = TempDir::new().unwrap();
        let src = source_tree(&temp);
        let dest = dest_with_private_d(&temp, parent);
        let mut args = vec!["-rw"];
        if let Some(p) = privs {
            args.extend(["-p", p]);
        }
        args.extend(["d", dest.to_str().unwrap()]);
        let out = pax(&src, &args);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(out.status.code(), Some(code), "{args:?}: stderr: {stderr}");
        assert_eq!(mode_of(&dest.join("d")), mode, "{args:?}");
        assert!(dest.join("d/f").exists(), "{args:?}");
    }
}
