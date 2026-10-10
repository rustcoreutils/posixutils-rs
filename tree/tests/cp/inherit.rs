//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! cp without -p into a directory with a default ACL: what it makes takes the ACL the kernel
//! derives from that default, the umask playing no part, as GNU cp's copies do. Each case needs
//! `setfacl`/`getfacl` and a filesystem that takes ACLs, and is skipped without them.

use plib::testing::{
    get_binary_path, mode_and_acl, run_under_umask, set_default_acl, DEFAULT_ACL_TEXT,
};
use plib::tmp::tempdir;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;

fn set_mode(path: &Path, mode: u32) {
    fs::set_permissions(path, fs::Permissions::from_mode(mode)).unwrap();
}

/// `cp -R src/d dest/d` under `umask`, `dest` having the default ACL: the directories end as
/// GNU cp 9.4 leaves them. GNU makes a directory without the group and other write permission
/// its source has, and gives those back only where the umask allows them; everything else the
/// default ACL decides. The file is opened with its source's mode, which the default masks.
fn copy_tree_under(umask: u32, d_mode: &str, sub_mode: &str) {
    let temp = tempdir().unwrap();
    let src = temp.path().join("src");
    let dest = temp.path().join("dest");
    fs::create_dir_all(src.join("d/sub")).unwrap();
    fs::create_dir(&dest).unwrap();
    if !set_default_acl(&dest) {
        return;
    }
    fs::write(src.join("d/g"), "g\n").unwrap();
    set_mode(&src.join("d/g"), 0o666);
    set_mode(&src.join("d/sub"), 0o751);
    set_mode(&src.join("d"), 0o775);

    let out = run_under_umask(
        &get_binary_path("cp"),
        &["-R", "src/d", "dest/d"],
        temp.path(),
        umask,
    );
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        mode_and_acl(&dest.join("d")),
        format!("{d_mode} {DEFAULT_ACL_TEXT}"),
        "umask {umask:03o}"
    );
    assert_eq!(
        mode_and_acl(&dest.join("d/sub")),
        format!("{sub_mode} {DEFAULT_ACL_TEXT}"),
        "umask {umask:03o}"
    );
    assert_eq!(
        mode_and_acl(&dest.join("d/g")),
        "660 user::rw- user:65534:rwx group::r-x mask::rw- other::---",
        "umask {umask:03o}"
    );
}

const MASK_RX: &str = "750 user::rwx user:65534:rwx group::r-x mask::r-x other::---";
const MASK_RWX: &str = "770 user::rwx user:65534:rwx group::r-x mask::rwx other::---";

#[test]
fn cp_r_gives_a_made_directory_the_default_acl_not_the_umask() {
    copy_tree_under(0o022, MASK_RX, MASK_RX);
    copy_tree_under(0o077, MASK_RX, MASK_RX);
}

#[test]
fn cp_r_gives_back_group_write_where_the_umask_allows() {
    copy_tree_under(0o002, MASK_RWX, MASK_RX);
    copy_tree_under(0o000, MASK_RWX, MASK_RX);
}

/// The directories `cp --parents` makes take the default ACL masked by their source's whole
/// mode, as GNU cp 9.4 makes them.
#[test]
fn cp_parents_makes_directories_under_the_default_acl() {
    for umask in [0o022, 0o077] {
        let temp = tempdir().unwrap();
        let dest = temp.path().join("dest");
        fs::create_dir_all(temp.path().join("d")).unwrap();
        fs::create_dir(&dest).unwrap();
        if !set_default_acl(&dest) {
            return;
        }
        fs::write(temp.path().join("d/g"), "g\n").unwrap();
        set_mode(&temp.path().join("d/g"), 0o666);
        set_mode(&temp.path().join("d"), 0o775);
        let args = ["--parents", "d/g", "dest"];
        let out = run_under_umask(&get_binary_path("cp"), &args, temp.path(), umask);
        assert_eq!(out.status.code(), Some(0));
        assert_eq!(
            mode_and_acl(&dest.join("d")),
            format!("{MASK_RWX} {DEFAULT_ACL_TEXT}"),
            "umask {umask:03o}"
        );
    }
}
