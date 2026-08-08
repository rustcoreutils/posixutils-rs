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
/// so unlike the `at` spool these paths cannot be redirected into a tempdir,
/// and the round-trip tests below are skipped unless the real spool is writable
/// (root, or a container). The behavior they cover is still exercised by the
/// `validate_*` unit tests and by crond's own suite.
///
/// This is deliberately *not* the same call as the allow/deny files, which do
/// now take overrides (see the gating tests at the end of this file): a
/// redirected spool controls where jobs are written, whereas a redirected allow
/// file can only permit or refuse the invoking user's own crontab.
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

// ============================================================================
// cron.allow / cron.deny gating -- audit #C7
//
// These paths were compile-time constants with no environment override, which
// is why the gating decision had never been exercised: the *logic* was unit
// tested, but no test had ever run the utility and watched it refuse. They now
// take `CRON_ALLOW`/`CRON_DENY` overrides, guarded by the same
// real-uid == effective-uid check `at` uses for `AT_ALLOW`/`AT_DENY`, so a
// set-uid crontab cannot be pointed at attacker-chosen files.
//
// Note what these do *not* unlock: the install/list/remove round trip is gated
// on `CRON_SPOOL_DIR`, a separate constant deliberately left alone. A
// redirected allow file can only permit or refuse the invoking user's own
// crontab; a redirected spool controls where jobs are *written*.
// ============================================================================

/// The invoking user's login name, resolved **exactly as crontab resolves it**.
///
/// Identity comes from the real uid, never `$LOGNAME`, which is spoofable
/// (audit #X2). Reading the environment here would also be wrong for a second
/// reason: these tests share a process with `tests/crond`, which sets
/// `LOGNAME=root` for its own purposes, so an environment-derived name is not
/// even stable within one run.
fn whoami() -> String {
    cron::spool::User::current()
        .map(|u| u.name)
        .expect("the invoking user must resolve from the real uid")
}

/// Run crontab with the allow/deny pair redirected into a tempdir.
fn run_gated(
    args: &[&str],
    stdin_data: &[u8],
    allow: Option<&std::path::Path>,
    deny: Option<&std::path::Path>,
) -> std::process::Output {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut cmd = Command::new(plib::testing::get_binary_path("crontab"));
    cmd.args(args)
        .env("LC_ALL", "C")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    match allow {
        Some(p) => cmd.env("CRON_ALLOW", p),
        // A path that certainly does not exist, so the allow rule is inactive.
        None => cmd.env("CRON_ALLOW", "/nonexistent/cron.allow"),
    };
    match deny {
        Some(p) => cmd.env("CRON_DENY", p),
        None => cmd.env("CRON_DENY", "/nonexistent/cron.deny"),
    };

    let mut child = cmd.spawn().expect("spawn crontab");
    child.stdin.as_mut().unwrap().write_all(stdin_data).unwrap();
    child.wait_with_output().expect("crontab output")
}

#[test]
fn crontab_allow_file_permits_only_listed_users() {
    let dir = tempfile::tempdir().unwrap();
    let allow = dir.path().join("cron.allow");

    // A user absent from a present cron.allow is refused.
    std::fs::write(&allow, "definitely-not-this-user\n").unwrap();
    let out = run_gated(&["-l"], b"", Some(&allow), None);
    assert_ne!(
        out.status.code(),
        Some(0),
        "a user absent from cron.allow must be refused; stderr={:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // The same invocation with the user listed gets past the gate. It may still
    // fail for want of a crontab or spool access, but it must fail *later* --
    // with a different diagnostic than the refusal above.
    std::fs::write(&allow, format!("{}\n", whoami())).unwrap();
    let allowed = run_gated(&["-l"], b"", Some(&allow), None);
    let refused_msg = String::from_utf8_lossy(&out.stderr).to_string();
    let allowed_msg = String::from_utf8_lossy(&allowed.stderr).to_string();
    assert!(
        allowed.status.code() == Some(0) || allowed_msg != refused_msg,
        "a listed user must get past the allow gate, but got the same refusal: \
         {allowed_msg:?}"
    );
}

#[test]
fn crontab_deny_file_refuses_listed_users() {
    let dir = tempfile::tempdir().unwrap();
    let deny = dir.path().join("cron.deny");

    // With no cron.allow, a user named in cron.deny is refused...
    std::fs::write(&deny, format!("{}\n", whoami())).unwrap();
    let denied = run_gated(&["-l"], b"", None, Some(&deny));
    assert_ne!(
        denied.status.code(),
        Some(0),
        "a user listed in cron.deny must be refused"
    );

    // ...and an empty cron.deny permits everyone, so this must fail
    // differently, if at all.
    std::fs::write(&deny, "").unwrap();
    let permitted = run_gated(&["-l"], b"", None, Some(&deny));
    let denied_msg = String::from_utf8_lossy(&denied.stderr).to_string();
    let permitted_msg = String::from_utf8_lossy(&permitted.stderr).to_string();
    assert!(
        permitted.status.code() == Some(0) || permitted_msg != denied_msg,
        "an empty cron.deny permits all users, but got the same refusal: \
         {permitted_msg:?}"
    );
}

#[test]
fn crontab_unreadable_allow_file_fails_closed() {
    // An existing-but-unreadable cron.allow must refuse rather than fall
    // through to the deny rule: the safe reading of "cannot tell who is
    // allowed" is "nobody".
    let dir = tempfile::tempdir().unwrap();
    let allow = dir.path().join("cron.allow");
    std::fs::write(&allow, format!("{}\n", whoami())).unwrap();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&allow, std::fs::Permissions::from_mode(0o000)).unwrap();
    }

    // Root can read a mode-000 file, so the premise does not hold there.
    if std::fs::read_to_string(&allow).is_ok() {
        println!("Skipping: this process can read a mode-000 file (running as root)");
        return;
    }

    let out = run_gated(&["-l"], b"", Some(&allow), None);
    assert_ne!(
        out.status.code(),
        Some(0),
        "an unreadable cron.allow must fail closed; stderr={:?}",
        String::from_utf8_lossy(&out.stderr)
    );
}
