//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Fixtures shared by the mailx test modules.
//!
//! Each of these existed once per module, in four near-identical copies.

use plib::tmp::NamedTempFile;
use std::io::Write;
use std::path::PathBuf;

/// Path to a static test data file in `tests/`.
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

/// Assert a run completed without a Rust panic.
pub fn assert_no_panic(output: &std::process::Output, what: &str) {
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
