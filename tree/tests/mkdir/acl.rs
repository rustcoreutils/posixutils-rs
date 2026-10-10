//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! mkdir in a directory with a default ACL: a directory takes the ACL the kernel derives from
//! that default, and `-m` then sets the bits it names exactly, as GNU mkdir does. Each case
//! needs `setfacl`/`getfacl` and a filesystem that takes ACLs, and is skipped without them.

use plib::testing::{
    get_binary_path, mode_and_acl, run_under_umask, set_default_acl, DEFAULT_ACL_TEXT,
};
use plib::tmp::tempdir;

/// Run `mkdir args` under each umask in a directory with the default ACL, then compare each
/// of `made` -- a name and the mode and access ACL it should have -- with what GNU mkdir 9.4
/// leaves (the default ACL itself is always inherited).
fn mkdir_under_default_acl(args: &[&str], made: &[(&str, &str)]) {
    for umask in [0o022, 0o077] {
        let temp = tempdir().unwrap();
        if !set_default_acl(temp.path()) {
            return;
        }
        let out = run_under_umask(&get_binary_path("mkdir"), args, temp.path(), umask);
        assert_eq!(
            out.status.code(),
            Some(0),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );
        for (name, acl) in made {
            assert_eq!(
                mode_and_acl(&temp.path().join(name)),
                format!("{acl} {DEFAULT_ACL_TEXT}"),
                "mkdir {args:?} under umask {umask:03o}: {name}"
            );
        }
    }
}

const MASK_RWX: &str = "770 user::rwx user:65534:rwx group::r-x mask::rwx other::---";

#[test]
fn mkdir_takes_the_default_acl() {
    mkdir_under_default_acl(&["d"], &[("d", MASK_RWX)]);
}

#[test]
fn mkdir_m_sets_the_mode_it_names_exactly() {
    let all = "777 user::rwx user:65534:rwx group::r-x mask::rwx other::rwx";
    mkdir_under_default_acl(&["-m", "0777", "d"], &[("d", all)]);
    mkdir_under_default_acl(&["-m", "a=rwx", "d"], &[("d", all)]);
    mkdir_under_default_acl(&["-m", "o+w", "d"], &[("d", all)]);
    mkdir_under_default_acl(
        &["-m", "1777", "d"],
        &[(
            "d",
            "1777 user::rwx user:65534:rwx group::r-x mask::rwx other::rwx",
        )],
    );
    mkdir_under_default_acl(
        &["-m", "2755", "d"],
        &[(
            "d",
            "2755 user::rwx user:65534:rwx group::r-x mask::r-x other::r-x",
        )],
    );
}

/// Bits `-m` does not name stay as the default ACL made them: `g-w` names only group write.
#[test]
fn mkdir_m_leaves_the_bits_it_does_not_name() {
    mkdir_under_default_acl(
        &["-m", "g-w", "d"],
        &[(
            "d",
            "750 user::rwx user:65534:rwx group::r-x mask::r-x other::---",
        )],
    );
}

/// Intermediate directories are made as a plain `mkdir` makes one, with owner write and search
/// added only where missing.
#[test]
fn mkdir_p_makes_intermediates_under_the_default_acl() {
    mkdir_under_default_acl(
        &["-p", "a/b/c"],
        &[("a", MASK_RWX), ("a/b", MASK_RWX), ("a/b/c", MASK_RWX)],
    );
    mkdir_under_default_acl(
        &["-p", "-m", "0700", "a/b"],
        &[
            ("a", MASK_RWX),
            (
                "a/b",
                "700 user::rwx user:65534:rwx group::r-x mask::--- other::---",
            ),
        ],
    );
}
