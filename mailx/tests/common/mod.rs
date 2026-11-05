//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Fixtures and runners shared by the mailx test modules.

use plib::testing::run_test_base_with_env;
use plib::tmp::NamedTempFile;
use std::io::Write;
use std::path::PathBuf;
use std::process::Output;

/// Path to a static test data file in `tests/`:
///
/// - `testdata.mbox`: 3 messages (alice, bob, charlie)
/// - `testdata-single.mbox`: 1 message (alice)
/// - `testdata-5msg.mbox`: 5 messages (alice, bob, charlie, alice@different, dave)
pub fn test_data_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push(filename);
    path
}

/// Write `content` to a fresh temporary file.
pub fn temp_file_with(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().expect("failed to create temp file");
    file.write_all(content.as_bytes())
        .expect("failed to write temp file");
    file.flush().expect("failed to flush temp file");
    file
}

/// A temporary mbox holding `content`.
pub fn create_temp_mbox(content: &str) -> NamedTempFile {
    temp_file_with(content)
}

/// A temporary mailrc holding `content`.
pub fn create_temp_mailrc(content: &str) -> NamedTempFile {
    temp_file_with(content)
}

/// A writable copy of a static fixture.
///
/// Tests that reach `quit` may rewrite the mailbox, so they must never be
/// pointed at the checked-in file itself.
pub fn copy_test_data(filename: &str) -> NamedTempFile {
    let src = test_data_path(filename);
    let content = std::fs::read_to_string(&src)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", src.display(), e));
    temp_file_with(&content)
}

/// Run mailx with `args`, feeding it `stdin`.
pub fn mailx(args: &[&str], stdin: impl AsRef<[u8]>) -> Output {
    mailx_env(args, stdin, &[])
}

/// Run mailx with `args` and extra environment variables, feeding it `stdin`.
pub fn mailx_env(args: &[&str], stdin: impl AsRef<[u8]>, env: &[(&str, &str)]) -> Output {
    let args: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    run_test_base_with_env("mailx", &args, stdin.as_ref(), env)
}

/// Run mailx and require exactly this standard output, standard error and
/// exit status.
pub fn assert_mailx(args: &[&str], stdin: &str, stdout: &str, stderr: &str, status: i32) {
    let output = mailx(args, stdin);
    assert_eq!(String::from_utf8_lossy(&output.stdout), stdout, "stdout");
    assert_eq!(String::from_utf8_lossy(&output.stderr), stderr, "stderr");
    assert_eq!(output.status.code(), Some(status), "exit status");
}

/// Assert a run completed without a Rust panic.
pub fn assert_no_panic(output: &Output, what: &str) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("panicked"),
        "{} panicked: {}",
        what,
        stderr
    );
    assert_ne!(
        output.status.code(),
        Some(101),
        "{} aborted: {}",
        what,
        stderr
    );
}
