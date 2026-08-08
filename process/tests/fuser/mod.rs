//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::process::Output;

mod basic;
mod modes;
#[cfg(target_os = "linux")]
mod tcp;
#[cfg(target_os = "linux")]
mod udp;
#[cfg(target_os = "linux")]
mod unix;
mod with_user;

/// Parse a `fuser` stdout PID list, asserting it is in the POSIX format.
///
/// POSIX (fuser DESCRIPTION, 98794-98795) mandates that each process ID be
/// written to standard output as:
///
/// ```text
/// " %1d", <process ID>
/// ```
///
/// — exactly one `<blank>` before each PID, with no field padding.
///
/// These assertions deliberately do **not** compare against the host's
/// `fuser`. psmisc formats its PID list as `" %5d"` (right-aligned in five
/// columns), so a byte-for-byte comparison against it passed or failed purely
/// on how many digits the test process's PID happened to have: identical for a
/// 5-digit PID, off by one space for a 4-digit one. That made the test fail on
/// a freshly-booted machine and pass on one with some uptime. psmisc is also
/// simply the wrong reference here — it is the non-conforming side.
pub fn parse_posix_pid_list(stdout: &[u8]) -> Vec<u32> {
    let text = std::str::from_utf8(stdout).expect("fuser stdout must be valid UTF-8");
    assert!(
        !text.is_empty(),
        "fuser found no processes; expected at least one"
    );
    assert!(
        text.starts_with(' '),
        "POSIX format is \" %1d\": each PID is preceded by one blank, got {:?}",
        text
    );
    assert!(
        !text.contains("  "),
        "POSIX format is \" %1d\": no field padding permitted, got {:?}",
        text
    );
    assert!(
        !text.contains('\n'),
        "the PID list is written to stdout without a newline (the rest of the \
         line goes to stderr), got {:?}",
        text
    );
    text.split_whitespace()
        .map(|f| {
            f.parse::<u32>()
                .unwrap_or_else(|_| panic!("expected a decimal PID, got {:?} in {:?}", f, text))
        })
        .collect()
}

pub fn fuser_test(
    args: Vec<String>,
    expected_err: &str,
    expected_exit_code: i32,
    checker: impl FnMut(&TestPlan, &Output),
) {
    run_test_with_checker(
        TestPlan {
            cmd: "fuser".to_string(),
            args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: expected_err.to_string(),
            expected_exit_code,
        },
        checker,
    );
}
