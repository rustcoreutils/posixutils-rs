//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::run_test_base;
use std::process::Output;

fn run_cron_test(cmd: &str, args: &[String], stdin_data: &[u8]) -> Output {
    run_test_base(cmd, args, stdin_data)
}

#[test]
fn no_args() {
    let output = run_cron_test("crontab", &[], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn dash_e() {
    let output = run_cron_test("crontab", &["-e".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn dash_l() {
    let output = run_cron_test("crontab", &["-l".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn dash_r() {
    let output = run_cron_test("crontab", &["-r".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn too_many_args() {
    let output = run_cron_test("crontab", &["-erl".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

// Validation used by `crontab` before installing an entry (audit #C4). "Valid"
// means exactly "the daemon will load this crontab".
use cron::job::validate_user_crontab;

#[test]
fn validate_accepts_five_field_and_at_specs() {
    assert!(validate_user_crontab("15 3 * * 1-5 find /tmp -name core").is_ok());
    assert!(validate_user_crontab("*/15 * * * * echo hi").is_ok());
    assert!(validate_user_crontab("@daily echo hi\n@reboot echo boot").is_ok());
}

#[test]
fn validate_ignores_blank_comment_and_short_lines() {
    // Blank lines, comments, and structurally short lines are skipped by the
    // daemon, so they must not be flagged as errors.
    assert!(validate_user_crontab("\n# a comment\n   \nnotacron").is_ok());
}

#[test]
fn validate_rejects_bad_time_field_with_line_number() {
    assert_eq!(validate_user_crontab("0 0 * * * ok\nz * * * * bad"), Err(2));
}

#[test]
fn validate_rejects_unknown_at_spec() {
    assert_eq!(validate_user_crontab("@bogus echo hi"), Err(1));
}

// ============================================================================
// Diagnostics channel and spool round-trip -- audit #C1/#C2/#C3/#C7
// ============================================================================

/// True when the crontab spool is writable by this process, i.e. the
/// install/list/remove paths can actually be exercised.
///
/// `CRON_SPOOL_DIR` is a compile-time constant with no environment override,
/// so unlike the `at` spool these paths cannot be redirected into a tempdir.
/// Rather than add a test-only configuration surface to a security-sensitive
/// utility, the round-trip tests below are skipped unless the real spool is
/// writable (root, or a container). The behavior they cover is still exercised
/// by the `validate_*` unit tests and by crond's own suite.
fn spool_is_writable() -> bool {
    let dir = if cfg!(target_os = "macos") {
        "/var/at/tabs"
    } else {
        "/var/spool/cron"
    };
    std::fs::metadata(dir).is_ok()
        && tempfile::NamedTempFile::new_in(dir)
            .map(|f| {
                let _ = f.close();
            })
            .is_ok()
}

#[test]
fn diagnostics_go_to_stderr_not_stdout() {
    // #C3: every error arm used println!, so diagnostics landed on stdout.
    // STDOUT (90997-90998) is reserved for the `-l` listing; STDERR
    // (90999-91000) is "used only for diagnostic messages".
    //
    // Too many operands fails the same way for every user, privileged or not,
    // so this needs no spool access.
    let output = run_cron_test(
        "crontab",
        &["one".to_string(), "two".to_string(), "three".to_string()],
        b"",
    );
    assert_ne!(output.status.code(), Some(0), "this invocation must fail");
    assert!(
        !output.stderr.is_empty(),
        "a diagnostic must be written to stderr"
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "",
        "nothing may be written to stdout: {:?}",
        String::from_utf8_lossy(&output.stdout)
    );
}

#[test]
fn install_list_remove_round_trip() {
    // #C1/#C2: `crontab file` installs, `-l` lists exactly what was installed,
    // `-r` removes it. #C1 in particular was a data-loss bug -- `-e` truncated
    // the live crontab before the editor ever opened.
    if !spool_is_writable() {
        eprintln!("skipping: crontab spool is not writable by this user");
        return;
    }

    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("ct");
    let content = "*/5 * * * * echo hello\n";
    std::fs::write(&src, content).unwrap();

    let out = run_cron_test("crontab", &[src.to_string_lossy().to_string()], b"");
    assert_eq!(out.status.code(), Some(0), "install failed");

    let out = run_cron_test("crontab", &["-l".to_string()], b"");
    assert_eq!(out.status.code(), Some(0), "list failed");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        content,
        "-l must reproduce exactly what was installed"
    );

    let out = run_cron_test("crontab", &["-r".to_string()], b"");
    assert_eq!(out.status.code(), Some(0), "remove failed");
}

#[test]
fn no_operand_replaces_from_stdin() {
    // #C2: `crontab` with no option and no file operand reads the new crontab
    // from standard input (SYNOPSIS 90911, DESCRIPTION 90914-90917). It used
    // to print a usage error and exit 1.
    if !spool_is_writable() {
        eprintln!("skipping: crontab spool is not writable by this user");
        return;
    }

    let content = "0 3 * * * /usr/bin/backup\n";
    let out = run_cron_test("crontab", &[], content.as_bytes());
    assert_eq!(out.status.code(), Some(0), "stdin replacement failed");

    let out = run_cron_test("crontab", &["-l".to_string()], b"");
    assert_eq!(String::from_utf8_lossy(&out.stdout), content);

    let _ = run_cron_test("crontab", &["-r".to_string()], b"");
}
