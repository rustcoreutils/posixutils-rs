//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A directory cp makes in a destination others can write is checked to be
//! empty before cp copies into it, which takes reading it. A umask that
//! removes the owner's read permission must not turn that check into a
//! failure.

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};

/// Let the test's cleanup read and remove what the umask left unreadable.
fn open_up(dir: &std::path::Path) {
    for entry in fs::read_dir(dir).into_iter().flatten().flatten() {
        let path = entry.path();
        if fs::symlink_metadata(&path).is_ok_and(|md| md.is_dir()) {
            let _ = fs::set_permissions(&path, fs::Permissions::from_mode(0o700));
            open_up(&path);
        }
    }
}

/// `cp --parents` makes the directories leading to its copy the same way.
#[test]
fn test_cp_parents_makes_directories_under_a_umask_denying_owner_read() {
    let test_dir = &format!(
        "{}/test_cp_parents_makes_directories_under_a_umask_denying_owner_read",
        env!("CARGO_TARGET_TMPDIR")
    );
    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir_all(format!("{test_dir}/a/b")).unwrap();
    fs::write(format!("{test_dir}/a/b/f"), "data\n").unwrap();
    let dst = format!("{test_dir}/dst");
    fs::create_dir(&dst).unwrap();
    fs::set_permissions(&dst, fs::Permissions::from_mode(0o777)).unwrap();

    let mut command = Command::new(env!("CARGO_BIN_EXE_cp"));
    command
        .args(["--parents", "a/b/f", "dst"])
        .current_dir(test_dir)
        .stdin(Stdio::null());
    // SAFETY: umask is async-signal-safe.
    unsafe {
        command.pre_exec(|| {
            libc::umask(0o400);
            Ok(())
        });
    }
    let out = command.output().unwrap();
    open_up(std::path::Path::new(&dst));

    assert_eq!(
        out.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(fs::metadata(format!("{dst}/a/b/f")).unwrap().len(), 5);
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_cp_r_makes_directories_under_a_umask_denying_owner_read() {
    let test_dir = &format!(
        "{}/test_cp_r_makes_directories_under_a_umask_denying_owner_read",
        env!("CARGO_TARGET_TMPDIR")
    );
    let _ = fs::remove_dir_all(test_dir);
    let src = format!("{test_dir}/src");
    let dst = format!("{test_dir}/dst");
    fs::create_dir_all(format!("{src}/d")).unwrap();
    fs::write(format!("{src}/d/f"), "data\n").unwrap();
    fs::create_dir(&dst).unwrap();
    fs::set_permissions(&dst, fs::Permissions::from_mode(0o777)).unwrap();

    let mut command = Command::new(env!("CARGO_BIN_EXE_cp"));
    command.args(["-R", &src, &dst]).stdin(Stdio::null());
    // SAFETY: umask is async-signal-safe.
    unsafe {
        command.pre_exec(|| {
            libc::umask(0o400);
            Ok(())
        });
    }
    let out = command.output().unwrap();
    open_up(std::path::Path::new(&dst));

    assert_eq!(
        out.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    // The umask leaves the file unreadable too; its size tells it was copied.
    assert_eq!(fs::metadata(format!("{dst}/src/d/f")).unwrap().len(), 5);
    fs::remove_dir_all(test_dir).unwrap();
}
