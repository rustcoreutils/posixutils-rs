//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};

fn renice_exit(args: Vec<&str>, expected_exit: i32) {
    let plan = TestPlan {
        cmd: String::from("renice"),
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

// PID 0 = the renice process itself; +0 increment is a privilege-free no-op.
// #RN3: ID 0 must be accepted (was rejected as "Invalid ID").
#[test]
fn renice_id_zero_accepted() {
    renice_exit(vec!["-n", "0", "-p", "0"], 0);
}

// #RN1: multiple IDs must be accepted (was: single ID only).
#[test]
fn renice_multiple_ids() {
    renice_exit(vec!["-n", "0", "-p", "0", "0"], 0);
}

// #RN4: increment +20 must be accepted (range was -20..20, exclusive).
#[test]
fn renice_range_plus_twenty() {
    renice_exit(vec!["-n", "20", "-p", "0"], 0);
}

// #RN2: a per-ID failure (nonexistent PID) must not abort; the valid ID is
// still processed and the exit status is non-zero.
#[test]
fn renice_partial_failure_continues() {
    renice_exit(vec!["-n", "0", "-p", "0", "999999"], 1);
}

// A non-numeric process ID is a per-ID error -> non-zero exit.
#[test]
fn renice_invalid_id() {
    renice_exit(vec!["-n", "0", "-p", "abc"], 1);
}

// `-u` with an unknown user name -> non-zero exit (username resolution path).
#[test]
fn renice_unknown_user() {
    renice_exit(vec!["-n", "0", "-u", "nonexistent_user_xyz123"], 1);
}

// `-n` is required.
#[test]
fn renice_missing_increment() {
    renice_exit(vec!["-p", "0"], 2);
}

// `-g` selects process groups rather than processes (spec 113233-113236). The
// suite exercised only the default (process) and `-u`'s failure path, so
// neither ID-type selector was proven to reach the syscall.
//
// The target is a process group created for this test, not our own: the test
// runner's group is full of other tests' short-lived children, and one exiting
// between getpriority() and setpriority() makes renice report ESRCH. Creating a
// group with exactly one long-lived member removes the race.
#[test]
fn renice_group_id_type_is_applied() {
    use std::os::unix::process::CommandExt;
    use std::process::{Command, Stdio};

    // setsid() makes the child a process-group leader, so its pgid is its pid.
    let mut child = unsafe {
        Command::new("sleep")
            .arg("30")
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .pre_exec(|| {
                if libc::setsid() == -1 {
                    return Err(std::io::Error::last_os_error());
                }
                Ok(())
            })
            .spawn()
    }
    .expect("failed to spawn a process-group leader");

    let pgid = child.id().to_string();
    std::thread::sleep(std::time::Duration::from_millis(100));

    // Increment 0 is a no-op any user may perform, so success means the option
    // was parsed, the ID was resolved as a *group*, and setpriority(PRIO_PGRP,
    // ...) was reached.
    renice_exit(vec!["-n", "0", "-g", &pgid], 0);

    let _ = child.kill();
    let _ = child.wait();
}

// A group ID with no processes must be diagnosed, not silently ignored — the
// counterpart that shows the success above is not vacuous.
#[test]
fn renice_unknown_group_fails() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("renice"),
            args: vec![
                String::from("-n"),
                String::from("0"),
                String::from("-g"),
                String::from("999999"),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("999999"),
                "the offending group should be named: {stderr:?}"
            );
        },
    );
}

// `-u` accepts a user *name*, not just a numeric uid (spec 113238-113240).
// `renice_unknown_user` only proves the lookup fails for a bad name; this
// proves it succeeds for a real one and reaches setpriority(PRIO_USER, ...).
#[test]
fn renice_user_id_type_accepts_a_name() {
    // Resolve our own login name via the same passwd database renice uses.
    // SAFETY: getpwuid returns a pointer into a static buffer; it is read
    // immediately and not retained.
    let name = unsafe {
        let pw = libc::getpwuid(libc::getuid());
        if pw.is_null() {
            return; // no passwd entry for this uid; nothing to test against
        }
        std::ffi::CStr::from_ptr((*pw).pw_name)
            .to_string_lossy()
            .into_owned()
    };

    // Increment 0 on our own uid: permitted, and only reachable if the name
    // resolved. Some of our processes may be unwritable, so tolerate either
    // outcome on the syscall itself and assert only that the *name* resolved.
    run_test_with_checker(
        TestPlan {
            cmd: String::from("renice"),
            args: vec![
                String::from("-n"),
                String::from("0"),
                String::from("-u"),
                name.clone(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                !stderr.contains("no such user"),
                "{name} must resolve in the passwd database: {stderr:?}"
            );
        },
    );
}
