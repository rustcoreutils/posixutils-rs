//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Inputs that once aborted the process.

use crate::common::{assert_no_panic, copy_test_data, create_temp_mbox, mailx};

/// A `From:` whose display name contains `>` before `<` must not abort.
///
/// `extract_address` searched for `>` from the start of the string rather than
/// after the `<`, producing an inverted slice range.
#[test]
fn from_with_reversed_angle_brackets_no_panic() {
    let mbox = create_temp_mbox(
        "From bob@example.com Fri Nov 29 10:00:00 2024\n\
         From: Bob >_< Smith <bob@example.com>\n\
         Subject: reversed brackets\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(&["-n", "-N", "-f", &mbox_path], "followup 1\n.\nexit\n");
    assert_no_panic(&output, "followup on a reversed-bracket From:");
}

/// A `From:` whose parenthesised comment closes before it opens must not abort.
///
/// `short_from` had the same inverted-range bug, and it fires from the header
/// summary printed at start-up -- before the prompt is ever reached.
#[test]
fn from_with_reversed_parens_no_panic() {
    let mbox = create_temp_mbox(
        "From bob@example.com Fri Nov 29 10:00:00 2024\n\
         From: Smith) Jr (John\n\
         Subject: reversed parens\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(&["-n", "-f", &mbox_path], "headers\nexit\n");
    assert_no_panic(&output, "header summary for a reversed-paren From:");
}

/// A multibyte character straight after the escape character must not abort.
///
/// `handle_escape` sliced at byte 1 after reading a `char`, so `~élan` split a
/// UTF-8 sequence. The callers already slice by `len_utf8`; only this function
/// reintroduced the bug.
#[test]
fn multibyte_after_escape_char_no_panic() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &mbox_path],
        "mail someone@example.com\n~élan\n~x\nexit\n",
    );
    assert_no_panic(&output, "a multibyte character after the escape char");
}

/// `set screen=0` must not divide by zero in `headers` or `z`.
#[test]
fn screen_zero_no_panic() {
    for cmd in ["headers", "z", "z-"] {
        let mbox = copy_test_data("testdata.mbox");
        let mbox_path = mbox.path().to_str().unwrap().to_string();

        let output = mailx(
            &["-n", "-N", "-f", &mbox_path],
            format!("set screen=0\n{}\nexit\n", cmd),
        );
        assert_no_panic(&output, &format!("`set screen=0` then `{}`", cmd));
    }
}

/// A cyclic alias must be diagnosed, not recursed into until the stack dies.
#[test]
fn cyclic_alias_no_stack_overflow() {
    for defs in ["alias a b\nalias b a\n", "alias a a\n"] {
        let mbox = copy_test_data("testdata-single.mbox");
        let mbox_path = mbox.path().to_str().unwrap().to_string();

        let output = mailx(
            &["-n", "-N", "-f", &mbox_path],
            format!("{}mail a\n~x\nexit\n", defs),
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !stderr.contains("stack overflow"),
            "cyclic alias overflowed the stack: {}",
            stderr
        );
        assert_no_panic(&output, "a cyclic alias");
        assert_eq!(output.status.code(), Some(0), "exit status");
    }
}

/// A `From:` crafted to escape the current directory must not name a file.
///
/// `Save`, `Copy`, `followup`, and `Followup` derive a filename from the
/// author, and the value comes straight out of an attacker-controlled header.
#[test]
fn author_derived_filename_rejects_path_traversal() {
    let mbox = create_temp_mbox(
        "From bob@example.com Fri Nov 29 10:00:00 2024\n\
         From: ../../../../tmp/mailx-traversal-probe@example.com\n\
         Subject: traversal\n\
         \n\
         body\n\
         \n",
    );
    let mbox_path = mbox.path().to_str().unwrap().to_string();
    let probe = std::path::Path::new("/tmp/mailx-traversal-probe");
    let _ = std::fs::remove_file(probe);

    /// Removes the probe file when dropped, so a regression that creates it
    /// does not leave it behind after the assertion below fails.
    struct RemoveOnDrop<'a>(&'a std::path::Path);
    impl Drop for RemoveOnDrop<'_> {
        fn drop(&mut self) {
            let _ = std::fs::remove_file(self.0);
        }
    }
    let _cleanup = RemoveOnDrop(probe);

    let output = mailx(&["-n", "-N", "-f", &mbox_path], "followup 1\n.\nexit\n");
    assert_no_panic(&output, "a traversal-shaped author name");
    assert!(
        !probe.exists(),
        "an author-derived filename escaped the working directory"
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// The address space mailx is allowed by [`run_limited`].
#[cfg(target_os = "linux")]
const LIMIT: u64 = 24 * 1024 * 1024;

/// Run mailx with its address space limited to [`LIMIT`].
#[cfg(target_os = "linux")]
fn run_limited(args: &[&str], stdin: &str, env: &[(&str, &str)]) -> std::process::Output {
    use std::io::Write;
    use std::os::unix::process::CommandExt;
    use std::process::{Command, Stdio};

    let mut command = Command::new(plib::testing::get_binary_path("mailx"));
    command
        .args(args)
        .envs(env.iter().copied())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    // SAFETY: setrlimit is async-signal-safe and touches no shared state.
    unsafe {
        command.pre_exec(|| {
            let limit = libc::rlimit {
                rlim_cur: LIMIT,
                rlim_max: LIMIT,
            };
            if libc::setrlimit(libc::RLIMIT_AS, &limit) != 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
    let mut child = command.spawn().expect("spawn mailx");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();
    child.wait_with_output().unwrap()
}

/// `count` messages of about 64 KB each, the first numbered `first`.
#[cfg(target_os = "linux")]
fn big_mailbox(first: usize, count: usize) -> String {
    let body = format!("{}\n", "x".repeat(79)).repeat(800);
    let mut content = String::new();
    for n in first..first + count {
        content.push_str(&format!(
            "From u{n}@x Mon Jan  1 00:00:00 2024\nFrom: u{n}@x\nSubject: s{n}\n\n{body}\n"
        ));
    }
    content
}

/// A mailbox much larger than the memory mailx may use can still be listed,
/// read and written back: messages stay in the session's private copy, not
/// in memory.
#[cfg(target_os = "linux")]
#[test]
fn mailbox_larger_than_memory_limit() {
    let content = big_mailbox(0, 640);
    assert!(content.len() as u64 > LIMIT * 3 / 2);
    let mbox = create_temp_mbox(&content);

    let path = mbox.path().to_str().unwrap();
    let listed = run_limited(&["-n", "-H", "-f", path], "", &[]);
    assert!(
        listed.status.success(),
        "{}",
        String::from_utf8_lossy(&listed.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&listed.stdout).lines().count(), 640);

    let read = run_limited(&["-n", "-N", "-f", path], "print 2\nquit\n", &[]);
    assert!(
        read.status.success(),
        "{}",
        String::from_utf8_lossy(&read.stderr)
    );
    let after = std::fs::read_to_string(mbox.path()).unwrap();
    assert_eq!(after.matches("\nFrom u").count() + 1, 640, "messages lost");
    assert!(
        after.contains("Subject: s1\nStatus: RO\n"),
        "read state not written back"
    );
}

/// Read mail is put ahead of an mbox much larger than the memory mailx may
/// use, without reading that mbox into memory, and nothing of it is lost.
#[cfg(target_os = "linux")]
#[test]
fn prepend_to_mbox_larger_than_memory_limit() {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let spool = dir.path().join("spool");
    let secondary = dir.path().join("mbox");
    std::fs::write(&spool, big_mailbox(0, 2)).unwrap();
    std::fs::write(&secondary, big_mailbox(100, 640)).unwrap();
    std::fs::set_permissions(
        &secondary,
        std::os::unix::fs::PermissionsExt::from_mode(0o640),
    )
    .unwrap();

    let output = run_limited(
        &["-n", "-N"],
        "print 1\nquit\n",
        &[
            ("MAIL", spool.to_str().unwrap()),
            ("MBOX", secondary.to_str().unwrap()),
        ],
    );
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let saved = std::fs::read_to_string(&secondary).unwrap();
    assert!(
        saved.starts_with("From u0@x "),
        "read mail should come first"
    );
    assert_eq!(
        saved.matches("\nFrom u").count() + 1,
        641,
        "mbox lost messages"
    );
    let mode = std::os::unix::fs::PermissionsExt::mode(
        &std::fs::metadata(&secondary).unwrap().permissions(),
    );
    assert_eq!(mode & 0o777, 0o640, "mbox lost its mode");
    let names: Vec<_> = std::fs::read_dir(dir.path())
        .unwrap()
        .map(|e| e.unwrap().file_name())
        .collect();
    assert_eq!(
        names.len(),
        2,
        "a temporary file was left behind: {:?}",
        names
    );
}
