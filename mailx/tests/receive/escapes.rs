//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Input-mode command escapes.

use crate::common::{copy_test_data, create_temp_mailrc, create_temp_mbox, mailx, mailx_env};
use plib::tmp::NamedTempFile;

/// `~:set name` actually sets the variable.
#[test]
fn tilde_colon_set_applies() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("set debug\n");

    // Compose a message, set a variable via ~:, finish with ~., then
    // list variables back in command mode.
    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "mail recipient@example.com\n~:set composevar\nbody\n~.\nset\nquit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("composevar"),
        "~:set should set the variable: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `~w file` appends to an existing file rather than truncating it.
#[test]
fn tilde_w_appends() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("set debug\n");
    let target = create_temp_mbox("PREEXISTING CONTENT\n");
    let target_path = target.path().to_str().unwrap().to_string();

    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        format!(
            "mail recipient@example.com\nappended body\n~w {}\n~.\nquit\n",
            target_path
        ),
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let contents = std::fs::read_to_string(&target_path).unwrap_or_default();
    assert!(
        contents.contains("PREEXISTING CONTENT"),
        "~w must not truncate the existing file: {:?}",
        contents
    );
    assert!(
        contents.contains("appended body"),
        "~w must append the message body: {:?}",
        contents
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `set escape=` (null) disables command escaping, so `~x` is taken literally
/// and the message is still sent rather than aborted.
#[test]
fn escape_null_disables_escaping() {
    let mbox = copy_test_data("testdata.mbox");
    // dot terminates input; escape is null so ~x is not an escape.
    let mailrc = create_temp_mailrc("set debug\nset dot\nset escape=\n");

    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "mail recipient@example.com\n~x\nbody\n.\nquit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // With escaping disabled, ~x did not abort: the message is sent
    // (debug mode reports it).
    assert!(
        stderr.contains("Debug mode"),
        "escape= null should disable ~x abort and still send: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// With `ignoreeof` set, a lone "." terminates a receive-mode composition even
/// when `dot` is not set.
#[test]
fn ignoreeof_dot_terminates_compose() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("set debug\nset ignoreeof\n");

    // If "." terminates, the echo runs as a command afterwards.
    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "mail recipient@example.com\nbody line\n.\necho PHASEE_MARKER\nquit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("PHASEE_MARKER"),
        "ignoreeof should let '.' terminate compose so later commands run: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `~h` does not prompt (or consume input) when stdin is not a terminal:
/// a command after the composition still runs.
#[test]
fn tilde_h_gated_on_terminal() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("set debug\nset ignoreeof\n");

    // If ~h consumed the following lines (no tty gate), the echo would
    // never run; with the gate it consumes nothing.
    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "mail recipient@example.com\n~h\nbody line\n.\necho HMARKER\nquit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("HMARKER"),
        "~h must not consume input off a terminal: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// A multibyte `escape` character must not panic when slicing the input line.
#[test]
fn multibyte_escape_char_no_panic() {
    let mbox = copy_test_data("testdata.mbox");
    // escape is the 2-byte character 'é'; "é." finishes the message.
    let mailrc = create_temp_mailrc("set debug\nset escape=é\n");

    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "mail recipient@example.com\nbody\né.\nquit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    assert!(
        output.status.success(),
        "multibyte escape char must not panic: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "the message should compose and send: {}",
        stderr
    );
}

/// `~|` must not deadlock when the filter writes more than a pipe buffer.
///
/// The body was written to the child's stdin in full before its output was
/// read, so a filter producing more than one pipe buffer blocked writing while
/// mailx blocked writing. This test runs mailx under its own watchdog rather
/// than the shared harness, which has no timeout: a regression here hangs
/// forever instead of failing.
#[test]
fn tilde_pipe_survives_large_output() {
    use std::io::Write as _;
    use std::process::{Command, Stdio};
    use std::sync::mpsc;

    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    // A body far larger than the 64 KiB pipe buffer, read from a file so it
    // does not have to travel through stdin a kilobyte at a time.
    let big = NamedTempFile::new().expect("big body file");
    let big_path = big.path().to_str().unwrap().to_string();
    let line = "the quick brown fox jumps over the lazy dog\n";
    std::fs::write(&big_path, line.repeat(6000)).expect("write big body");

    let mut child = Command::new(plib::testing::get_binary_path("mailx"))
        .args(["-n", "-N", "-f", &mbox_path])
        .env("LC_ALL", "C")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn mailx");

    let script = format!(
        "set debug\nmail to@example.com\n~r {}\n~| cat\n~x\nexit\n",
        big_path
    );
    child
        .stdin
        .take()
        .expect("stdin")
        .write_all(script.as_bytes())
        .expect("write script");

    let (tx, rx) = mpsc::channel();
    let handle = child.id();
    std::thread::spawn(move || {
        let status = child.wait_with_output();
        let _ = tx.send(status);
    });

    match rx.recv_timeout(std::time::Duration::from_secs(30)) {
        Ok(Ok(output)) => {
            assert_eq!(
                output.status.code(),
                Some(0),
                "`~| cat` on a large body failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        Ok(Err(e)) => panic!("waiting for mailx failed: {}", e),
        Err(_) => {
            // SAFETY: `handle` is this child's pid; the process is still running
            // because the wait has not completed, so the pid has not been reused.
            unsafe { libc::kill(handle as libc::pid_t, libc::SIGKILL) };
            panic!("`~| cat` deadlocked on a body larger than one pipe buffer");
        }
    }
}

/// `~w` names a pathname (spec 105094); it is not a shell word.
#[test]
fn tilde_w_does_not_run_the_shell() {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let target = dir.path().join("my file.txt");
    let probe = dir.path().join("PWNED");
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &mbox_path],
        format!(
            "mail to@example.com\nbody text\n~w {}\n~w $(touch {})out.txt\n~x\nexit\n",
            target.display(),
            probe.display()
        ),
    );
    assert!(
        target.exists(),
        "~w did not write to the literal pathname, so the name was word-split"
    );
    assert!(
        !probe.exists(),
        "~w ran command substitution on the filename"
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `~w` resolves the mailx filename notations, just not the shell's.
#[test]
fn tilde_w_expands_the_home_prefix() {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let home = dir.path().to_str().unwrap().to_string();
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx_env(
        &["-n", "-N", "-f", &mbox_path],
        "mail to@example.com\nbody text\n~w ~/out.txt\n~x\nexit\n",
        &[("HOME", &home)],
    );
    assert!(
        dir.path().join("out.txt").exists(),
        "~w did not expand the ~ prefix"
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Headers read back after `~e` split their lists the same way.
#[test]
fn edited_headers_keep_quoted_commas() {
    let mbox = create_temp_mbox(
        "From a@example.com Fri Nov 29 10:00:00 2024\n\
         From: Bob <bob@example.com>\n\
         To: me@example.com, \"Smith, John\" <john@example.com>\n\
         Subject: s\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx_env(
        &["-n", "-N", "-f", &mbox_path],
        "set debug\nset EDITOR=/bin/true\nreply 1\nbody\n~e\n.\nexit\n",
        &[("USER", "me"), ("LOGNAME", "me")],
    );
    let err = String::from_utf8_lossy(&output.stderr);
    let envelope = err
        .lines()
        .find(|l| l.starts_with("Envelope:"))
        .unwrap_or_else(|| panic!("no envelope line: {}", err));
    assert_eq!(envelope, "Envelope: bob@example.com, john@example.com");
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `~t`, `~c` and `~b` take addresses as written in a header -- a quoted
/// display name may hold a comma or a blank -- as well as plain addresses
/// separated by blanks.
#[test]
fn recipient_escapes_split_by_address() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("set debug\n");
    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "mail first@example.com\n\
         ~t \"Smith, J\" <j@example.com>\n\
         ~c a@example.com b@example.com\n\
         body\n~.\nquit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("To: first@example.com, \"Smith, J\" <j@example.com>\n"),
        "~t should add one recipient: {}",
        stderr
    );
    assert!(
        stderr.contains("Cc: a@example.com, b@example.com\n"),
        "~c should add two recipients: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `mail` takes its recipients the same way.
#[test]
fn mail_command_splits_recipients_by_address() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("set debug\n");
    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "mail \"Smith, J\" <j@example.com> b@example.com\nbody\n~.\nquit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("To: \"Smith, J\" <j@example.com>, b@example.com\n"),
        "mail should take two recipients: {}",
        stderr
    );
}

/// An error in a command escape is diagnosed on standard error, and the
/// message is still sent (spec CONSEQUENCES OF ERRORS).
#[test]
fn escape_errors_go_to_standard_error() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("set debug\n");
    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "mail to@example.com\n~r /nonexistent/file\n~w /nonexistent/dir/out\n~j\nbody\n~.\nquit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    for diagnostic in [
        "/nonexistent/file:",
        "/nonexistent/dir/out:",
        "Unknown escape: ~j",
    ] {
        assert!(
            stderr.contains(diagnostic),
            "{} on stderr: {}",
            diagnostic,
            stderr
        );
        assert!(
            !stdout.contains(diagnostic),
            "{} on stdout: {}",
            diagnostic,
            stdout
        );
    }
    assert!(
        stderr.contains("Debug mode"),
        "the message was not sent: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}
