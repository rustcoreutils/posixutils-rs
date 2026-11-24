//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared helpers for the SCCS integration tests.
//!
//! The SCCS utilities are working-directory sensitive by specification: `get`
//! creates the g-file and l-file "in the current directory" (get, 99186-99194),
//! while the p-, z- and x-files live beside the s-file (99214, 99228).
//!
//! `plib::testing::run_test` cannot set a working directory — `TestPlan` has no
//! field for one — so a test using it has its working files created wherever
//! the test binary happens to be running. That was masked while `get` derived
//! the g-file's directory from the s-file; once the placement was corrected to
//! match the spec, a run of the suite scattered g-files across the `sccs/`
//! source directory.
//!
//! These wrappers therefore spawn the utility directly, deriving the working
//! directory from the plan's own s-file operand: the directory holding it, or
//! its parent when that directory is `SCCS/`. That is exactly the directory a
//! user would be standing in.

#![allow(dead_code)]

use plib::testing::TestPlan;
use std::ffi::OsStr;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};

/// Run a built SCCS utility with an explicit working directory.
pub fn run_in(cmd: &str, args: &[&str], cwd: &Path, stdin: &str) -> Output {
    let args: Vec<&OsStr> = args.iter().map(OsStr::new).collect();
    run_bytes(cmd, &args, cwd, stdin.as_bytes())
}

/// [`run_in`] with arguments and input that need not be UTF-8.
pub fn run_bytes(cmd: &str, args: &[&OsStr], cwd: &Path, stdin: &[u8]) -> Output {
    let mut child = Command::new(plib::testing::get_binary_path(cmd))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("spawn {cmd}: {e}"));
    // A utility that rejects its arguments exits without ever reading stdin,
    // which closes the read end and makes this write fail with EPIPE. That is
    // the child behaving correctly, not a test failure, so only a genuine I/O
    // error is worth panicking over.
    let mut sink = child.stdin.take().expect("piped stdin");
    match sink.write_all(stdin) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => {}
        Err(e) => panic!("write stdin to {cmd}: {e}"),
    }
    drop(sink);
    child.wait_with_output().expect("wait")
}

/// Run a utility in `cwd` and require success, reporting stderr on failure.
pub fn run_ok(cmd: &str, args: &[&str], cwd: &Path, stdin: &str) -> Output {
    let out = run_in(cmd, args, cwd, stdin);
    assert!(
        out.status.success(),
        "{cmd} {args:?} failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    out
}

/// The working directory implied by a plan's operands: the directory holding
/// the first `s.`-prefixed argument, or its parent when that is `SCCS/`.
fn cwd_for(plan: &TestPlan) -> PathBuf {
    for a in &plan.args {
        let p = Path::new(a);
        let is_sfile = p
            .file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.starts_with("s."))
            .unwrap_or(false);
        if !is_sfile {
            continue;
        }
        let dir = match p.parent() {
            Some(d) if !d.as_os_str().is_empty() => d,
            _ => continue,
        };
        if dir.file_name().map(|n| n == "SCCS").unwrap_or(false) {
            return dir.parent().unwrap_or(Path::new(".")).to_path_buf();
        }
        return dir.to_path_buf();
    }
    std::env::current_dir().expect("cwd")
}

fn run_plan(plan: &TestPlan) -> Output {
    let args: Vec<&str> = plan.args.iter().map(String::as_str).collect();
    run_in(&plan.cmd, &args, &cwd_for(plan), &plan.stdin_data)
}

/// Drop-in replacement for `plib::testing::run_test` that runs in the
/// directory implied by the plan's s-file operand.
pub fn run_test(plan: TestPlan) {
    let out = run_plan(&plan);

    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        plan.expected_out,
        "stdout mismatch for {} {:?}",
        plan.cmd,
        plan.args
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        plan.expected_err,
        "stderr mismatch for {} {:?}",
        plan.cmd,
        plan.args
    );
    assert_eq!(
        out.status.code(),
        Some(plan.expected_exit_code),
        "exit code mismatch for {} {:?}",
        plan.cmd,
        plan.args
    );
}

/// Drop-in replacement for `plib::testing::run_test_with_checker`.
pub fn run_test_with_checker<F>(plan: TestPlan, mut checker: F)
where
    F: FnMut(&TestPlan, &Output),
{
    let out = run_plan(&plan);
    checker(&plan, &out);
}

/// Write the s-file `dir/name` from `content` (everything after the `^Ah`
/// line), prepending the correct checksum line: for s-files no `admin`
/// invocation can produce, such as ones holding CRs or non-UTF-8 text.
pub fn write_sfile(dir: &Path, name: &str, content: &[u8]) -> PathBuf {
    let mut data = format!("\x01h{:05}\n", plib::sccsfile::compute_checksum(content)).into_bytes();
    data.extend_from_slice(content);
    let path = dir.join(name);
    std::fs::write(&path, data).expect("write s-file");
    path
}

/// An s-file (after its `^Ah` line) whose body, comment, `q` flag and
/// descriptive text hold Latin-1 bytes that are not valid UTF-8 -- as CSSC or historical
/// SCCS writes for a Latin-1 source file -- for [`write_sfile`].
pub const LATIN1_SFILE: &[u8] = b"\x01s 00002/00000/00000\n\
\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\
\x01c caf\xe9 comment\n\
\x01e\n\
\x01u\n\x01U\n\
\x01f q caf\xe9\n\
\x01t\n\xe9t\xe9\n\x01T\n\
\x01I 1\n\
caf\xe9 %I%\n\
line2\n\
\x01E 1\n";

/// Assert that the s-file at `path` still holds each of `records` (whole
/// lines, without their newline) byte for byte.
pub fn assert_sfile_holds(path: &Path, records: &[&[u8]]) {
    let data = std::fs::read(path).expect("read s-file");
    let lines = plib::sccsfile::split_lines(&data);
    for record in records {
        assert!(
            lines.contains(record),
            "{} lost {:?}:\n{}",
            path.display(),
            String::from_utf8_lossy(record),
            String::from_utf8_lossy(&data)
        );
    }
}

/// The Latin-1 records of [`LATIN1_SFILE`] that a rewrite must keep.
pub const LATIN1_RECORDS: &[&[u8]] = &[
    b"\x01c caf\xe9 comment",
    b"\x01f q caf\xe9",
    b"\xe9t\xe9",
    b"caf\xe9 %I%",
];

/// An s-file (after its `^Ah` line) whose blocks overlap rather than nest, as
/// CSSC and historical SCCS weave them: delta 3's delete block opens inside
/// delta 1's insert and ends inside delta 2's. 1.1 is `a b`, 1.2 is
/// `a b c d`, and 1.3 deleted `b` and `c`, leaving `a d`.
pub const OVERLAP_SFILE: &[u8] = b"\x01s 00000/00002/00002\n\
\x01d D 1.3 26/01/03 00:00:00 u 3 2\n\x01c three\n\x01e\n\
\x01s 00002/00000/00002\n\
\x01d D 1.2 26/01/02 00:00:00 u 2 1\n\x01c two\n\x01e\n\
\x01s 00002/00000/00000\n\
\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01c one\n\x01e\n\
\x01u\n\x01U\n\x01t\n\x01T\n\
\x01I 1\na\n\x01D 3\nb\n\x01E 1\n\x01I 2\nc\n\x01E 3\nd\n\x01E 2\n";

/// Assert that a command changed only what it was to change in an s-file:
/// the lines of `after`, less the checksum line and the lines `added`, are
/// the lines of `before`, less the checksum line and the lines `removed`,
/// in order; and the checksum line is right for the result.
#[track_caller]
pub fn assert_only_changed(before: &[u8], after: &[u8], removed: &[&[u8]], added: &[&[u8]]) {
    let rest = |data: &[u8], without: &[&[u8]]| -> Vec<String> {
        let mut without = without.to_vec();
        plib::sccsfile::split_lines(data)
            .into_iter()
            .skip(1)
            .filter(|line| match without.iter().position(|w| w == line) {
                Some(at) => {
                    without.remove(at);
                    false
                }
                None => true,
            })
            .map(|line| String::from_utf8_lossy(line).into_owned())
            .collect()
    };
    assert_eq!(
        rest(after, added),
        rest(before, removed),
        "the command touched lines it was not asked to"
    );
    let sum = plib::sccsfile::stored_body_checksum(after);
    assert!(
        after.starts_with(format!("\x01h{sum:05}\n").as_bytes()),
        "the checksum line is not the checksum of the file"
    );
}

/// The delta table of a three-delta trunk (1.1, 1.2, 1.3; serials 1, 2, 3)
/// with an empty user list and descriptive text, for hand-built weaves.
pub const THREE_DELTAS: &[u8] = b"\x01s 00001/00000/00000\n\
\x01d D 1.3 26/01/03 00:00:00 u 3 2\n\x01c three\n\x01e\n\
\x01s 00001/00000/00000\n\
\x01d D 1.2 26/01/02 00:00:00 u 2 1\n\x01c two\n\x01e\n\
\x01s 00001/00000/00000\n\
\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01c one\n\x01e\n\
\x01u\n\x01U\n\x01t\n\x01T\n";

/// [`write_sfile`] for a body under [`THREE_DELTAS`].
pub fn write_weave(dir: &Path, name: &str, body: &[u8]) -> PathBuf {
    write_sfile(dir, name, &[THREE_DELTAS, body].concat())
}
