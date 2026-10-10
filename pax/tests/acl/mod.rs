//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! pax extracting into a directory with a default ACL, from an archive that holds no ACLs.
//!
//! Without -p p a member is made by the normal file-creation action: it takes the ACL the
//! kernel derives from the default, masked by its archived mode, the umask playing no part --
//! as GNU tar extracts without -p. Under -p p it takes its archived mode and no ACL beyond it,
//! the inherited one replaced, as GNU tar --acls -p and cp -p do. Each case needs
//! `setfacl`/`getfacl` and a filesystem that takes ACLs, and is skipped without them.

mod ace;
mod archived;

use plib::testing::{
    get_binary_path, mode_and_acl, run_under_umask, set_default_acl, DEFAULT_ACL_TEXT,
};
use plib::tmp::{tempdir, TempDir};
use std::ffi::CString;
use std::fs;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;

fn set_mode(path: &Path, mode: u32) {
    fs::set_permissions(path, fs::Permissions::from_mode(mode)).unwrap();
}

/// A scratch directory with the tree `src` -- `f` 0754, `p` a FIFO 0664, `d` 0775 holding
/// `g` 0666 and `sub` 0751 -- archived by pax as `a.pax`, and an empty `x` with the default
/// ACL; `None` where no default ACL can be set.
fn archived_tree() -> Option<TempDir> {
    let temp = tempdir().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d/sub")).unwrap();
    fs::write(src.join("f"), "f\n").unwrap();
    fs::write(src.join("d/g"), "g\n").unwrap();
    let fifo = CString::new(src.join("p").as_os_str().as_bytes()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(fifo.as_ptr(), 0o600) }, 0);
    for (name, mode) in [
        ("f", 0o754),
        ("p", 0o664),
        ("d/g", 0o666),
        ("d/sub", 0o751),
        ("d", 0o775),
    ] {
        set_mode(&src.join(name), mode);
    }
    let out = run_under_umask(
        &get_binary_path("pax"),
        &["-w", "-f", "../a.pax", "f", "p", "d"],
        &src,
        0o022,
    );
    assert_eq!(out.status.code(), Some(0), "pax -w");
    fs::create_dir(temp.path().join("x")).unwrap();
    set_default_acl(&temp.path().join("x")).then_some(temp)
}

/// What each member of `archived_tree` ends as: name, then mode and access ACL, then whether
/// it holds the default ACL.
type Expected<'a> = [(&'a str, &'a str, bool); 5];

/// Without -p p, what GNU tar 1.35 leaves without -p.
const INHERITED: Expected = [
    (
        "f",
        "750 user::rwx user:65534:rwx group::r-x mask::r-x other::---",
        false,
    ),
    (
        "p",
        "660 user::rw- user:65534:rwx group::r-x mask::rw- other::---",
        false,
    ),
    (
        "d",
        "770 user::rwx user:65534:rwx group::r-x mask::rwx other::---",
        true,
    ),
    (
        "d/g",
        "660 user::rw- user:65534:rwx group::r-x mask::rw- other::---",
        false,
    ),
    (
        "d/sub",
        "750 user::rwx user:65534:rwx group::r-x mask::r-x other::---",
        true,
    ),
];

/// Under -p p, what GNU tar 1.35 leaves with --acls -p.
const REPLACED: Expected = [
    ("f", "754 user::rwx group::r-x other::r--", false),
    ("p", "664 user::rw- group::rw- other::r--", false),
    ("d", "775 user::rwx group::rwx other::r-x", false),
    ("d/g", "666 user::rw- group::rw- other::rw-", false),
    ("d/sub", "751 user::rwx group::r-x other::--x", false),
];

/// Run pax with `args` under each umask -- in `src` where `from_src`, else in `x` -- and
/// compare every member made in `x` with `expected`.
fn extract(args: &[&str], from_src: bool, expected: &Expected) {
    for umask in [0o022, 0o077] {
        let Some(temp) = archived_tree() else {
            return;
        };
        let x = temp.path().join("x");
        let cwd = if from_src {
            temp.path().join("src")
        } else {
            x.clone()
        };
        let out = run_under_umask(&get_binary_path("pax"), args, &cwd, umask);
        assert_eq!(
            out.status.code(),
            Some(0),
            "pax {args:?}: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        for (name, acl, default) in expected {
            let want = if *default {
                format!("{acl} {DEFAULT_ACL_TEXT}")
            } else {
                acl.to_string()
            };
            assert_eq!(
                mode_and_acl(&x.join(name)),
                want,
                "pax {args:?} under umask {umask:03o}: {name}"
            );
        }
    }
}

#[test]
fn pax_r_makes_members_under_the_default_acl() {
    extract(&["-r", "-f", "../a.pax"], false, &INHERITED);
}

#[test]
fn pax_r_p_replaces_the_inherited_acl() {
    extract(&["-r", "-p", "p", "-f", "../a.pax"], false, &REPLACED);
    extract(&["-r", "-p", "e", "-f", "../a.pax"], false, &REPLACED);
}

#[test]
fn pax_rw_makes_members_under_the_default_acl() {
    extract(&["-rw", "f", "p", "d", "../x"], true, &INHERITED);
}

#[test]
fn pax_rw_p_replaces_the_inherited_acl() {
    extract(&["-rw", "-p", "p", "f", "p", "d", "../x"], true, &REPLACED);
}
