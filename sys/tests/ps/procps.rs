//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! procps extensions that util-linux's test suite uses, as in
//! `until [[ $(ps --no-headers -ostat "${PID}") =~ S.* ]]` (lsfd) and
//! `ps --no-headers -o comm 1`: `--no-headers`, the `stat` field, and
//! process IDs given as operands.

use plib::testing::get_binary_path;
use std::process::{Child, Command, Output, Stdio};

fn ps(args: &[&str]) -> Output {
    Command::new(get_binary_path("ps"))
        .args(args)
        .output()
        .expect("run ps")
}

/// A child sleeping in `nanosleep`, so in state `S`, killed on drop.
struct Sleeper(Child);

impl Sleeper {
    fn new() -> Self {
        let child = Command::new("sleep")
            .arg("30")
            .stdin(Stdio::null())
            .spawn()
            .expect("spawn sleep");
        // Let it reach its sleep.
        std::thread::sleep(std::time::Duration::from_millis(200));
        Sleeper(child)
    }

    fn pid(&self) -> String {
        self.0.id().to_string()
    }
}

impl Drop for Sleeper {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn stdout_of(out: &Output) -> String {
    assert_eq!(out.status.code(), Some(0), "{out:?}");
    assert!(out.stderr.is_empty(), "{out:?}");
    String::from_utf8(out.stdout.clone()).unwrap()
}

/// lsfd's wait loop: the state of the process given as an operand, without
/// a header.
#[test]
fn ps_no_headers_stat_of_a_pid_operand() {
    let sleeper = Sleeper::new();
    let pid = sleeper.pid();
    let out = stdout_of(&ps(&["--no-headers", "-ostat", &pid]));
    assert!(out.starts_with('S'), "{out:?}");
    assert_eq!(out.lines().count(), 1, "{out:?}");
}

/// The `stat` field has the header `STAT`.
#[test]
fn ps_stat_field_header() {
    let sleeper = Sleeper::new();
    let out = stdout_of(&ps(&["-o", "stat", "-p", &sleeper.pid()]));
    let lines: Vec<_> = out.lines().collect();
    assert_eq!(lines.len(), 2, "{out:?}");
    assert_eq!(lines[0].trim_end(), "STAT");
    assert!(lines[1].starts_with('S'), "{out:?}");
}

/// `ps --no-headers -o comm 1`: operands select like `-p`, and
/// `--no-headers` drops the header.
#[test]
fn ps_pid_operands_select_like_p() {
    let a = Sleeper::new();
    let b = Sleeper::new();
    let out = stdout_of(&ps(&["--no-headers", "-o", "pid,comm", &a.pid(), &b.pid()]));
    let mut pids: Vec<_> = out
        .lines()
        .map(|l| l.split_whitespace().next().unwrap().to_string())
        .collect();
    pids.sort();
    let mut want = vec![a.pid(), b.pid()];
    want.sort();
    assert_eq!(pids, want, "{out:?}");
    assert!(out.lines().all(|l| l.ends_with("sleep")), "{out:?}");
}

/// An operand that is not a process ID is an error.
#[test]
fn ps_rejects_a_non_numeric_operand() {
    let out = ps(&["-o", "pid", "12x"]);
    assert_ne!(out.status.code(), Some(0), "{out:?}");
    assert!(out.stdout.is_empty(), "{out:?}");
    assert!(!out.stderr.is_empty(), "{out:?}");
}
