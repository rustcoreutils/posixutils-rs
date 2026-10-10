//
// Copyright (c) 2024-2026 Jeff Garzik
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod acl;

use plib::testing::{run_test_with_checker, TestPlan};
use plib::tmp::{tempdir, TempDir};
use std::fs;
use std::path::Path;
use std::process::Output;

/// The pathname `name` inside `dir`, a temporary directory removed with
/// whatever mkfifo made in it when the test ends.
fn fifo_in(dir: &TempDir, name: &str) -> String {
    dir.path().join(name).to_str().unwrap().to_string()
}

fn run_mkfifo_test(args: Vec<&str>, expected_exit_code: i32) {
    let plan = TestPlan {
        cmd: String::from("mkfifo"),
        args: args.iter().map(|&s| s.into()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };

    run_test_with_checker(plan, |_, output: &Output| {
        assert_eq!(output.status.code(), Some(expected_exit_code));
    });
}

#[test]
fn test_create_single_fifo() {
    let dir = tempdir().unwrap();
    let fifo = fifo_in(&dir, "fifo");
    let fifo_path = fifo.as_str();

    run_mkfifo_test(vec![fifo_path], 0);

    assert!(Path::new(fifo_path).exists());

    #[cfg(unix)]
    {
        use std::os::unix::fs::FileTypeExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        assert!(metadata.file_type().is_fifo());
    }
}

#[test]
fn test_fifo_already_exists() {
    let dir = tempdir().unwrap();
    let fifo = fifo_in(&dir, "fifo");
    let fifo_path = fifo.as_str();

    run_mkfifo_test(vec![fifo_path], 0);
    assert!(Path::new(fifo_path).exists());

    run_mkfifo_test(vec![fifo_path], 1);
}

#[test]
fn test_invalid_mode() {
    let dir = tempdir().unwrap();
    let fifo = fifo_in(&dir, "fifo");
    let fifo_path = fifo.as_str();

    run_mkfifo_test(vec!["-m", "invalid", fifo_path], 1);

    assert!(!Path::new(fifo_path).exists());
}

#[test]
fn test_set_fifo_mode_absolute() {
    let dir = tempdir().unwrap();
    let fifo = fifo_in(&dir, "fifo");
    let fifo_path = fifo.as_str();

    run_mkfifo_test(vec!["-m", "644", fifo_path], 0);

    assert!(Path::new(fifo_path).exists());

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        let permissions = metadata.permissions();
        assert_eq!(permissions.mode() & 0o777, 0o644);
    }
}

#[test]
fn test_set_fifo_mode_symbolic_plus() {
    let dir = tempdir().unwrap();
    let fifo = fifo_in(&dir, "fifo");
    let fifo_path = fifo.as_str();

    run_mkfifo_test(vec!["-m", "+x", fifo_path], 0);

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        let permissions = metadata.permissions();
        assert_eq!(
            permissions.mode() & 0o777,
            0o777,
            "+x should produce rwxrwxrwx"
        );
    }
}

#[test]
fn test_set_fifo_mode_symbolic_minus() {
    let dir = tempdir().unwrap();
    let fifo = fifo_in(&dir, "fifo");
    let fifo_path = fifo.as_str();

    run_mkfifo_test(vec!["-m", "-w", fifo_path], 0);

    assert!(Path::new(fifo_path).exists());

    #[cfg(unix)]
    {
        use std::os::unix::fs::FileTypeExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        assert!(metadata.file_type().is_fifo());
    }
}

#[test]
fn test_set_fifo_mode_symbolic_who_specified() {
    let dir = tempdir().unwrap();
    let fifo = fifo_in(&dir, "fifo");
    let fifo_path = fifo.as_str();

    run_mkfifo_test(vec!["-m", "a-w", fifo_path], 0);

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        let permissions = metadata.permissions();
        assert_eq!(
            permissions.mode() & 0o777,
            0o444,
            "a-w should produce r--r--r--"
        );
    }
}

#[test]
fn test_create_multiple_fifos() {
    let dir = tempdir().unwrap();
    let (fifo1, fifo2) = (fifo_in(&dir, "a"), fifo_in(&dir, "b"));

    run_mkfifo_test(vec![&fifo1, &fifo2], 0);

    assert!(Path::new(&fifo1).exists());
    assert!(Path::new(&fifo2).exists());
}

// A <newline> in the pathname is rejected.
#[test]
fn test_mkfifo_newline_rejected() {
    let test_dir = &format!(
        "{}/test_mkfifo_newline_rejected",
        env!("CARGO_TARGET_TMPDIR")
    );
    fs::create_dir(test_dir).unwrap();
    let bad = &format!("{test_dir}/a\nb");

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_mkfifo"))
        .arg(bad)
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert!(!Path::new(bad).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

/// `-m` gives exactly the mode it names, one denying the owner reading included, with a
/// set-user-ID bit `mkfifo(2)` may not take. Off Linux the FIFO is opened for reading to have
/// its mode set, which a mode without owner read used to refuse (EACCES); macOS CI runs that
/// path, Linux sets the mode through an `O_PATH` pin.
#[cfg(unix)]
#[test]
fn mkfifo_m_gives_a_mode_without_owner_read() {
    use std::os::unix::fs::PermissionsExt;
    let dir = tempdir().unwrap();
    for (mode, name) in [("4222", "a"), ("0222", "b"), ("0200", "c")] {
        let path = fifo_in(&dir, name);
        run_mkfifo_test(vec!["-m", mode, &path], 0);
        let got = fs::symlink_metadata(&path).unwrap().permissions().mode() & 0o7777;
        assert_eq!(format!("{got:04o}"), mode, "mkfifo -m {mode}");
    }
}
