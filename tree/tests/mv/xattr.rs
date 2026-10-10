//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! mv across filesystems copies extended attributes, as GNU mv does: one the destination's
//! filesystem takes none of is lost silently; one it refuses otherwise is diagnosed, and the
//! move still completes with exit status 0. Each case needs `/dev/shm` on another filesystem
//! than the scratch directory, and both taking `user.` attributes; it is skipped without them.

use plib::testing::{get_binary_path, set_xattr, xattrs};
use plib::tmp::{tempdir, Builder, TempDir};
use std::fs;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::process::{Command, Output, Stdio};

fn mv(args: &[&Path]) -> Output {
    Command::new(get_binary_path("mv"))
        .args(args)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute mv")
}

fn pair(name: &str, value: &[u8]) -> (Vec<u8>, Vec<u8>) {
    (name.as_bytes().to_vec(), value.to_vec())
}

/// A scratch directory and one in `/dev/shm`, on different filesystems.
fn two_filesystems() -> Option<(TempDir, TempDir)> {
    let Ok(shm) = Builder::new().tempdir_in("/dev/shm") else {
        eprintln!("note: no /dev/shm; test skipped");
        return None;
    };
    let temp = tempdir().unwrap();
    if fs::metadata(shm.path()).unwrap().dev() == fs::metadata(temp.path()).unwrap().dev() {
        eprintln!("note: /dev/shm is on the scratch filesystem; test skipped");
        return None;
    }
    Some((temp, shm))
}

/// A directory and the file in it, moved from the scratch directory to `/dev/shm`, keep their
/// attributes.
#[test]
fn mv_across_filesystems_keeps_xattrs() {
    let Some((temp, shm)) = two_filesystems() else {
        return;
    };
    let d = temp.path().join("d");
    fs::create_dir(&d).unwrap();
    fs::write(d.join("f"), "f\n").unwrap();
    if !set_xattr(&d, b"user.foo", b"dir")
        || !set_xattr(&d.join("f"), b"user.foo", b"file")
        || !set_xattr(shm.path(), b"user.probe", b"")
    {
        return;
    }
    let out = mv(&[&d, shm.path()]);
    assert_eq!(String::from_utf8_lossy(&out.stderr), "");
    assert_eq!(out.status.code(), Some(0));
    assert!(!d.exists());
    let moved = shm.path().join("d");
    assert_eq!(xattrs(&moved), [pair("user.foo", b"dir")]);
    assert_eq!(xattrs(&moved.join("f")), [pair("user.foo", b"file")]);
}

/// A value the destination cannot hold -- 64 KiB from `/dev/shm` onto a filesystem whose limit
/// is lower, ext4's block -- is diagnosed in GNU mv's words, naming the destination; the
/// others are copied, and the move completes with exit status 0. Skipped where the scratch
/// filesystem takes the value.
#[test]
fn mv_diagnoses_a_value_too_big_for_the_destination() {
    let Some((temp, shm)) = two_filesystems() else {
        return;
    };
    let big = vec![b'x'; 65536];
    let probe = temp.path().join("probe");
    fs::write(&probe, "").unwrap();
    let f = shm.path().join("f");
    fs::write(&f, "f\n").unwrap();
    if !set_xattr(&f, b"user.big", &big) || !set_xattr(&f, b"user.small", b"s") {
        return;
    }
    if set_xattr(&probe, b"user.big", &big) {
        eprintln!("note: the scratch filesystem takes a 64 KiB value; test skipped");
        return;
    }
    let g = temp.path().join("g");
    let out = mv(&[&f, &g]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.starts_with(&format!(
            "mv: setting attribute 'user.big' for '{}': ",
            g.display()
        )) && stderr.lines().count() == 1,
        "stderr: {stderr}"
    );
    assert_eq!(out.status.code(), Some(0));
    assert!(!f.exists());
    assert_eq!(fs::read(&g).unwrap(), b"f\n");
    assert_eq!(xattrs(&g), [pair("user.small", b"s")]);
}

/// A destination whose filesystem takes no extended attribute: a message queue in
/// `/dev/mqueue`, made from an empty file. Nothing is said, as GNU mv says nothing, and the
/// move completes.
#[cfg(target_os = "linux")]
#[test]
fn mv_to_a_filesystem_without_xattrs() {
    let temp = tempdir().unwrap();
    let e = temp.path().join("e");
    fs::write(&e, "").unwrap();
    if !set_xattr(&e, b"user.foo", b"bar") {
        return;
    }
    let dest = format!("/dev/mqueue/posixutils-mv-xattr-{}", std::process::id());
    if fs::write(&dest, "").is_err() {
        eprintln!("note: no writable mqueue filesystem at /dev/mqueue; test skipped");
        return;
    }
    fs::remove_file(&dest).unwrap();
    let out = mv(&[&e, Path::new(&dest)]);
    let _ = fs::remove_file(&dest);
    assert_eq!(String::from_utf8_lossy(&out.stderr), "");
    assert_eq!(out.status.code(), Some(0));
    assert!(!e.exists());
}
