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
// Every test in `modes` is Linux-gated, as are the only callers of the helper
// below; compiling either elsewhere just yields dead-code warnings.
#[cfg(target_os = "linux")]
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
#[cfg(target_os = "linux")]
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

/// A failure must read as one `fuser: <file>: <message>` line.
///
/// `main` returned `Result<(), Box<dyn Error>>`, so Rust's `Termination` impl
/// printed the `Debug` of the boxed error -- `Error: Os { code: 2, kind:
/// NotFound, message: "..." }` -- Rust struct syntax with no utility name and
/// no operand. GNU says which file it could not find.
#[test]
fn fuser_missing_operand_diagnostic_names_utility_and_file() {
    let out = std::process::Command::new(plib::testing::get_binary_path("fuser"))
        .arg("/nonexistent_fuser_probe_zz")
        .output()
        .expect("spawn fuser");
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();

    assert!(
        !stderr.contains("Os {"),
        "Rust's Debug form must not reach the user: {stderr:?}"
    );
    assert!(
        !stderr.starts_with("Error: "),
        "Rust's Debug form must not reach the user: {stderr:?}"
    );
    assert!(
        !stderr.contains("(os error"),
        "Rust's errno parenthetical must not reach the user: {stderr:?}"
    );
    assert!(
        stderr.starts_with("fuser: "),
        "every diagnostic must name the utility: {stderr:?}"
    );
    assert!(
        stderr.contains("/nonexistent_fuser_probe_zz"),
        "the diagnostic must name the operand it could not resolve: {stderr:?}"
    );
    assert_ne!(
        out.status.code(),
        Some(0),
        "a failed lookup must not exit 0"
    );
}

/// `--help` and `--version` are a successful request for information, not an
/// error. Every other utility here exits 0; `fuser` exited 1.
#[test]
fn fuser_help_and_version_exit_zero() {
    for flag in ["--help", "--version"] {
        let out = std::process::Command::new(plib::testing::get_binary_path("fuser"))
            .arg(flag)
            .output()
            .expect("spawn fuser");
        assert_eq!(
            out.status.code(),
            Some(0),
            "fuser {flag} must exit 0, got {:?}",
            out.status.code()
        );
    }
}
