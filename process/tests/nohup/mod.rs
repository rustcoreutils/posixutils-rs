//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{get_binary_path, run_test, run_test_with_checker, TestPlan};
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};

fn nohup_exit(args: Vec<&str>, expected_exit: i32) {
    let plan = TestPlan {
        cmd: String::from("nohup"),
        args: args.into_iter().map(String::from).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: expected_exit,
    };
    run_test_with_checker(plan, |_, output| {
        assert_eq!(output.status.code(), Some(expected_exit));
    });
}

// Under the test harness stdin/stdout/stderr are pipes (not terminals), so
// nohup performs no redirection and simply spawns the utility.

// #NH4: a missing utility exits 127 (controlled — not a panic/exit 101).
#[test]
fn nohup_not_found() {
    nohup_exit(vec!["/nonexistent/utility/xyz"], 127);
}

// Utility found but not executable -> 126.
#[test]
fn nohup_not_executable() {
    let path = std::env::temp_dir().join("posixutils_nohup_test_noexec");
    {
        let _f = std::fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .mode(0o644)
            .open(&path)
            .unwrap();
    }
    nohup_exit(vec![path.to_str().unwrap()], 126);
    let _ = std::fs::remove_file(&path);
}

// Utility exit status is propagated.
#[test]
fn nohup_status_passthrough() {
    nohup_exit(vec!["false"], 1);
}

// SIGHUP is ignored in the child (inherited across exec): a child that sends
// itself SIGHUP survives and continues. stdout is a pipe here, so nohup does
// not redirect it and we capture the child's output directly.
#[test]
fn nohup_sighup_ignored_in_child() {
    run_test(TestPlan {
        cmd: String::from("nohup"),
        args: vec!["sh", "-c", "kill -HUP $$; echo survived"]
            .into_iter()
            .map(String::from)
            .collect(),
        stdin_data: String::new(),
        expected_out: String::from("survived\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// #NH1: when stdout is a terminal, nohup creates nohup.out mode 0600. We give
// nohup a real terminal stdout via openpty so the redirect path is exercised.
#[test]
fn nohup_out_created_mode_0600() {
    use std::os::unix::io::FromRawFd;
    use std::process::{Command, Stdio};

    let tmp = std::env::temp_dir().join(format!("posixutils_nohup_pty_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();

    // Allocate a pseudo-terminal; the slave side becomes the child's stdout.
    let mut master: libc::c_int = 0;
    let mut slave: libc::c_int = 0;
    let rc = unsafe {
        libc::openpty(
            &mut master,
            &mut slave,
            std::ptr::null_mut(),
            std::ptr::null(),
            std::ptr::null(),
        )
    };
    assert_eq!(rc, 0, "openpty failed");

    let bin = get_binary_path("nohup");
    let status = unsafe {
        Command::new(bin)
            .current_dir(&tmp)
            .args(["sh", "-c", "echo hi"])
            .stdin(Stdio::null())
            .stdout(Stdio::from_raw_fd(slave)) // a terminal -> triggers redirect
            .stderr(Stdio::piped()) // a pipe -> the "appending output" notice
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
    };
    unsafe { libc::close(master) };
    assert!(status.success());

    let meta = std::fs::metadata(tmp.join("nohup.out")).expect("nohup.out should exist");
    let mode = meta.permissions().mode() & 0o777;
    assert_eq!(mode, 0o600, "nohup.out must be created with mode 0600");

    let _ = std::fs::remove_dir_all(&tmp);
}
