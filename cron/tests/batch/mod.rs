//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Integration tests for `batch`.
//!
//! `batch` had no tests at all, which is how #B9 (command duplication) and the
//! ungated prompts survived. Every test here drives the real binary with the
//! spool and allow-file redirected via `AT_JOB_DIR` / `AT_ALLOW`, so nothing
//! needs root or a writable `/var/spool`.

use plib::tmp::{tempdir, TempDir};
use std::fs;
use std::io::Write;
use std::process::{Command, Stdio};

/// The identity `batch` itself resolves: `getpwuid(getuid())`.
fn whoami() -> String {
    unsafe {
        let pw = libc::getpwuid(libc::getuid());
        assert!(!pw.is_null(), "no passwd entry for the test user");
        std::ffi::CStr::from_ptr((*pw).pw_name)
            .to_string_lossy()
            .to_string()
    }
}

struct Run {
    code: i32,
    stdout: String,
    stderr: String,
    dir: TempDir,
}

impl Run {
    /// The single job file written into the spool.
    fn job_name(&self) -> String {
        let mut names: Vec<String> = fs::read_dir(self.dir.path().join("spool"))
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().to_string())
            .collect();
        assert_eq!(names.len(), 1, "expected exactly one job file: {:?}", names);
        names.pop().unwrap()
    }

    fn job_body(&self) -> String {
        let name = self.job_name();
        fs::read_to_string(self.dir.path().join("spool").join(name)).unwrap()
    }
}

/// Run `bin` with `stdin_data`, spool redirected into a fresh tempdir.
fn run(bin: &str, args: &[&str], stdin_data: &str) -> Run {
    let dir = tempdir().unwrap();
    let spool = dir.path().join("spool");
    fs::create_dir_all(&spool).unwrap();
    let allow = dir.path().join("at.allow");
    fs::write(&allow, format!("{}\n", whoami())).unwrap();

    let mut child = Command::new(plib::testing::get_binary_path(bin))
        .args(args)
        .env("AT_JOB_DIR", &spool)
        .env("AT_ALLOW", &allow)
        .env_remove("AT_DENY")
        .env("TZ", "UTC")
        .env("LC_ALL", "C")
        .env("SHELL", "/bin/sh")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(stdin_data.as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    Run {
        code: out.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&out.stdout).to_string(),
        stderr: String::from_utf8_lossy(&out.stderr).to_string(),
        dir,
    }
}

#[test]
fn test_batch_does_not_duplicate_commands() {
    // #B9: batch read stdin into a buffer it never cleared, so `read_line`
    // appended and every earlier line was pushed again on each iteration --
    // a three-line job executed its first two lines twice. Silent, and
    // destructive for any non-idempotent command.
    let r = run("batch", &[], "echo one\necho two\necho three\n");
    assert_eq!(r.code, 0, "batch failed: {}", r.stderr);

    let body = r.job_body();
    for cmd in ["echo one", "echo two", "echo three"] {
        assert_eq!(
            body.matches(cmd).count(),
            1,
            "{:?} must appear exactly once in the job script:\n{}",
            cmd,
            body
        );
    }
}

#[test]
fn test_batch_uses_queue_b() {
    // POSIX: batch is `at -q b`. The queue letter is the first character of
    // the spool filename.
    let r = run("batch", &[], "true\n");
    assert_eq!(r.code, 0, "batch failed: {}", r.stderr);
    let name = r.job_name();
    assert!(
        name.starts_with('b'),
        "batch jobs belong in queue b, got {:?}",
        name
    );
}

#[test]
fn test_batch_submission_notice_goes_to_stderr() {
    // POSIX at STDERR (85018): "job %s at %s\n". stdout is reserved for the
    // -l listing, so nothing belongs there on submission.
    let r = run("batch", &[], "true\n");
    assert_eq!(r.code, 0, "batch failed: {}", r.stderr);
    assert!(
        r.stderr.starts_with("job "),
        "submission notice must be on stderr, got {:?}",
        r.stderr
    );
    assert_eq!(
        r.stdout, "",
        "nothing may be written to stdout on submission, got {:?}",
        r.stdout
    );
}

#[test]
fn test_batch_does_not_prompt_when_stdin_is_not_a_terminal() {
    // batch wrote the banner and an `at> ` prompt per line unconditionally,
    // so a pipeline got prompt noise interleaved into stdout -- and the banner
    // announced the wrong utility (#B8).
    let r = run("batch", &[], "echo one\necho two\n");
    assert!(!r.stdout.contains("at>"), "no prompts: {:?}", r.stdout);
    assert!(!r.stdout.contains("batch>"), "no prompts: {:?}", r.stdout);
    assert!(!r.stdout.contains("<EOT>"), "no EOT marker: {:?}", r.stdout);
}

#[test]
fn test_batch_equivalent_to_at_q_b() {
    // #B7 made batch a thin `at -q b -m now` wrapper. Nothing pinned that
    // equivalence, so a drift would have been silent.
    let via_batch = run("batch", &[], "echo hello\n");
    let via_at = run("at", &["-q", "b", "-m", "now"], "echo hello\n");
    assert_eq!(via_batch.code, 0, "batch: {}", via_batch.stderr);
    assert_eq!(via_at.code, 0, "at: {}", via_at.stderr);

    assert_eq!(
        via_batch.job_name().chars().next(),
        via_at.job_name().chars().next(),
        "both must land in the same queue"
    );

    // Compare the structural parts of the script only.
    //
    // The job embeds the submitter's entire environment, per POSIX, so the two
    // runs' env blocks differ by this run's tempdir -- and, because sibling
    // tests mutate process-global env via set_var, can differ under
    // parallelism for reasons that have nothing to do with batch. Dropping the
    // `NAME='value'; export NAME` lines and the atrun uid/gid banner leaves
    // exactly what this test is about: interpreter, umask, mail flag, working
    // directory and the command itself.
    let is_env_export = |l: &str| {
        l.split_once('=').is_some_and(|(name, _)| {
            !name.is_empty() && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        }) && l.ends_with(|c: char| c.is_ascii_alphanumeric() || c == '_')
            && l.contains("; export ")
    };
    let strip = |s: String| {
        s.lines()
            .filter(|l| !l.starts_with("# atrun") && !is_env_export(l))
            .collect::<Vec<_>>()
            .join("\n")
    };
    assert_eq!(
        strip(via_batch.job_body()),
        strip(via_at.job_body()),
        "batch must generate the same script as `at -q b -m now`"
    );
}

#[test]
fn test_batch_respects_the_allow_file() {
    // A user absent from at.allow must be refused, and no job filed.
    let dir = tempdir().unwrap();
    let spool = dir.path().join("spool");
    fs::create_dir_all(&spool).unwrap();
    let allow = dir.path().join("at.allow");
    fs::write(&allow, "definitely-not-this-user\n").unwrap();

    let mut child = Command::new(plib::testing::get_binary_path("batch"))
        .env("AT_JOB_DIR", &spool)
        .env("AT_ALLOW", &allow)
        .env_remove("AT_DENY")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn batch");
    child.stdin.as_mut().unwrap().write_all(b"true\n").unwrap();
    let out = child.wait_with_output().unwrap();

    assert_ne!(out.status.code(), Some(0), "a denied user must fail");
    assert_eq!(
        fs::read_dir(&spool).unwrap().count(),
        0,
        "no job may be filed for a denied user"
    );
}
