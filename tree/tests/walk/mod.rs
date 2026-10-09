//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Behavior shared by every utility that walks a tree with `ftw`.

use plib::testing::{run_test, TestPlan};
use std::fs;

/// A trailing-slash operand that names no directory -- a dangling symlink (`ENOENT`) or a regular
/// file (`ENOTDIR`) -- reaches each utility's error reporter with no metadata. Every walker
/// reports it by name and exits 1; none panics, and nothing is changed.
#[test]
fn trailing_slash_operand_naming_no_directory_is_reported() {
    let tmp = plib::tmp::tempdir().unwrap();
    let dir = tmp.path().to_str().unwrap();
    fs::write(format!("{dir}/file"), b"x").unwrap();
    std::os::unix::fs::symlink("nowhere", format!("{dir}/dangling")).unwrap();
    let uid = unsafe { libc::geteuid() }.to_string();
    let gid = unsafe { libc::getegid() }.to_string();
    let copy = format!("{dir}/copy");

    for (name, error) in [
        ("dangling", "No such file or directory"),
        ("file", "Not a directory"),
    ] {
        let p = format!("{dir}/{name}/");
        let cases: [(&str, Vec<&str>, String); 8] = [
            (
                "chown",
                vec!["-h", &uid, &p],
                format!("chown: cannot access '{p}': {error}\n"),
            ),
            (
                "chgrp",
                vec!["-R", &gid, &p],
                format!("chgrp: cannot access '{p}': {error}\n"),
            ),
            (
                "chmod",
                vec!["-R", "644", &p],
                format!("chmod: cannot access '{p}': {error}\n"),
            ),
            ("du", vec![&p], format!("du: {p}: {error}\n")),
            (
                "ls",
                vec!["-R", &p],
                format!("ls: cannot access '{p}': {error}\n"),
            ),
            (
                "cp",
                vec!["-R", &p, &copy],
                format!("cp: cannot access '{p}': {error}\n"),
            ),
            (
                "rm",
                vec!["-r", &p],
                format!("rm: cannot remove '{p}': {error}\n"),
            ),
            (
                "chmod",
                vec!["644", &p],
                format!("chmod: cannot access '{p}': {error}\n"),
            ),
        ];
        for (cmd, args, expected_err) in cases {
            run_test(TestPlan {
                cmd: cmd.to_string(),
                args: args.iter().map(|s| s.to_string()).collect(),
                stdin_data: String::new(),
                expected_out: String::new(),
                expected_err,
                expected_exit_code: 1,
            });
        }
    }

    assert_eq!(fs::read(format!("{dir}/file")).unwrap(), b"x");
    assert!(fs::symlink_metadata(format!("{dir}/dangling"))
        .unwrap()
        .is_symlink());
    assert!(fs::symlink_metadata(&copy).is_err());
}
