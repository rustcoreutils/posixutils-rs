//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! mkfifo in a directory with a default ACL: a FIFO takes the ACL the kernel derives from that
//! default, and `-m` then sets its mode exactly, as GNU mkfifo does. Each case needs
//! `setfacl`/`getfacl` and a filesystem that takes ACLs, and is skipped without them.

use plib::testing::{get_binary_path, mode_and_acl, run_under_umask, set_default_acl};
use plib::tmp::tempdir;

/// Run `mkfifo args p` under each umask in a directory with the default ACL, and compare the
/// mode and ACL of `p` with what GNU mkfifo 9.4 leaves.
fn mkfifo_under_default_acl(args: &[&str], acl: &str) {
    for umask in [0o022, 0o077] {
        let temp = tempdir().unwrap();
        if !set_default_acl(temp.path()) {
            return;
        }
        let mut args = args.to_vec();
        args.push("p");
        let out = run_under_umask(&get_binary_path("mkfifo"), &args, temp.path(), umask);
        assert_eq!(
            out.status.code(),
            Some(0),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );
        assert_eq!(
            mode_and_acl(&temp.path().join("p")),
            acl,
            "mkfifo {args:?} under umask {umask:03o}"
        );
    }
}

#[test]
fn mkfifo_takes_the_default_acl() {
    mkfifo_under_default_acl(
        &[],
        "660 user::rw- user:65534:rwx group::r-x mask::rw- other::---",
    );
}

#[test]
fn mkfifo_m_sets_the_mode_exactly() {
    mkfifo_under_default_acl(
        &["-m", "0777"],
        "777 user::rwx user:65534:rwx group::r-x mask::rwx other::rwx",
    );
    mkfifo_under_default_acl(
        &["-m", "0640"],
        "640 user::rw- user:65534:rwx group::r-x mask::r-- other::---",
    );
}
