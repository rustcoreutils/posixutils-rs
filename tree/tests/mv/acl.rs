//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! mv across filesystems copies ACLs with the mode: the access ACL of every file and
//! directory, and a directory's default ACL. Each case needs `setfacl`/`getfacl`, a
//! filesystem that takes ACLs on both sides, and `/dev/shm` on another filesystem than the
//! scratch directory; it is skipped without them.

use plib::testing::get_binary_path;
use plib::tmp::{tempdir, Builder};
use std::fs;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::process::{Command, Output, Stdio};

fn setfacl(path: &Path, args: &[&str]) -> bool {
    let ok = Command::new("setfacl")
        .args(args)
        .arg(path)
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|s| s.success());
    if !ok {
        eprintln!("note: setfacl is missing or this filesystem takes no ACLs; case skipped");
    }
    ok
}

/// What `getfacl` prints for the tree in `dir` -- `.`, `x` and `sub`, each headed by its name,
/// in that order whatever order the filesystem lists them in.
fn getfacl_tree(dir: &Path) -> String {
    let out = Command::new("getfacl")
        .args([".", "x", "sub"])
        .current_dir(dir)
        .output()
        .expect("getfacl ran once setfacl did");
    assert!(out.status.success(), "getfacl in {}", dir.display());
    String::from_utf8(out.stdout).unwrap()
}

fn mv(args: &[&Path]) -> Output {
    Command::new(get_binary_path("mv"))
        .args(args)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute mv")
}

/// A directory with an access and a default ACL, holding a file with an ACL and a
/// subdirectory with a default ACL only, moved from the scratch directory to `/dev/shm`: the
/// ACLs arrive as they were.
#[test]
fn mv_across_filesystems_keeps_acls() {
    let Ok(other) = Builder::new().tempdir_in("/dev/shm") else {
        eprintln!("note: no /dev/shm; test skipped");
        return;
    };
    let temp = tempdir().unwrap();
    let dev = |p: &Path| fs::metadata(p).unwrap().dev();
    if dev(temp.path()) == dev(other.path()) {
        eprintln!("note: /dev/shm is on the scratch filesystem; test skipped");
        return;
    }
    let d = temp.path().join("d");
    fs::create_dir_all(d.join("sub")).unwrap();
    fs::write(d.join("x"), "x\n").unwrap();
    if !setfacl(&d, &["-m", "u:65534:rx,d:u:65534:rwx"])
        || !setfacl(&d.join("x"), &["-m", "u:65534:rw"])
        || !setfacl(&d.join("sub"), &["-d", "-m", "g:65534:r"])
        || !setfacl(other.path(), &["-m", "u:65534:r"])
    {
        return;
    }
    let before = getfacl_tree(&d);
    let out = mv(&[&d, other.path()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert_eq!(stderr, "");
    assert!(!d.exists());
    assert_eq!(getfacl_tree(&other.path().join("d")), before);
}

/// A destination whose filesystem takes no ACL: a message queue in `/dev/mqueue`, made from an
/// empty file. As GNU mv does, the lost ACL is diagnosed, but the move completes -- the source
/// is removed -- and the exit status is 0 (POSIX mv: a failure to duplicate a characteristic
/// does not change it).
#[cfg(target_os = "linux")]
#[test]
fn mv_to_a_filesystem_without_acls() {
    let temp = tempdir().unwrap();
    let e = temp.path().join("e");
    fs::write(&e, "").unwrap();
    let dest = format!("/dev/mqueue/posixutils-mv-acl-{}", std::process::id());
    if fs::write(&dest, "").is_err() {
        eprintln!("note: no writable mqueue filesystem at /dev/mqueue; test skipped");
        return;
    }
    fs::remove_file(&dest).unwrap();
    if !setfacl(&e, &["-m", "u:65534:r"]) {
        return;
    }
    let out = mv(&[&e, Path::new(&dest)]);
    let _ = fs::remove_file(&dest);
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!("mv: preserving permissions for '{dest}': Operation not supported\n")
    );
    assert_eq!(out.status.code(), Some(0));
    assert!(!e.exists());
}

/// A move that loses the ACL must not leave the destination granting more than the source
/// did: a source whose owning group has nothing and a named user rw- (mode 0660, the mask)
/// arrives with no group permission, not with the mask as the owning group's.
#[cfg(target_os = "linux")]
#[test]
fn mv_losing_an_acl_does_not_widen_the_group() {
    use std::os::unix::fs::PermissionsExt;
    let temp = tempdir().unwrap();
    let e = temp.path().join("e");
    fs::write(&e, "").unwrap();
    fs::set_permissions(&e, fs::Permissions::from_mode(0o600)).unwrap();
    let dest = format!("/dev/mqueue/posixutils-mv-widen-{}", std::process::id());
    if fs::write(&dest, "").is_err() {
        eprintln!("note: no writable mqueue filesystem at /dev/mqueue; test skipped");
        return;
    }
    fs::remove_file(&dest).unwrap();
    if !setfacl(&e, &["-m", "g::---,u:65534:rw,m::rw"]) {
        return;
    }
    let out = mv(&[&e, Path::new(&dest)]);
    let mode = fs::metadata(&dest).map(|md| md.mode() & 0o7777);
    let _ = fs::remove_file(&dest);
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(mode.unwrap(), 0o600);
}
