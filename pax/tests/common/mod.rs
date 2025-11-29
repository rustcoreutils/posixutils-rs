//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Common test helpers for pax integration tests

use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::process::{Command, Output};

/// Run pax with given arguments and return output
pub fn run_pax(args: &[&str]) -> Output {
    run_pax_with_stdin(args, None)
}

/// Run pax with given arguments and optional stdin, return output
pub fn run_pax_with_stdin(args: &[&str], stdin_data: Option<&str>) -> Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_pax"));
    cmd.args(args);

    if let Some(data) = stdin_data {
        use std::process::Stdio;
        cmd.stdin(Stdio::piped());
        let mut child = cmd.spawn().expect("Failed to spawn pax");
        if let Some(ref mut stdin) = child.stdin {
            stdin
                .write_all(data.as_bytes())
                .expect("Failed to write stdin");
        }
        child.wait_with_output().expect("Failed to wait for pax")
    } else {
        cmd.output().expect("Failed to run pax")
    }
}

/// Run pax with given arguments in a specific directory
pub fn run_pax_in_dir(args: &[&str], dir: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(args)
        .current_dir(dir)
        .output()
        .expect("Failed to run pax")
}

/// Run pax with stdin input in a specific directory
pub fn run_pax_in_dir_with_stdin(args: &[&str], dir: &Path, stdin_data: &str) -> Output {
    use std::process::Stdio;
    let mut child = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn pax");

    if let Some(ref mut stdin) = child.stdin {
        stdin
            .write_all(stdin_data.as_bytes())
            .expect("Failed to write stdin");
    }

    child.wait_with_output().expect("Failed to wait for pax")
}

/// Create a test directory with standard test files
pub fn create_test_files(dir: &Path) {
    // Create regular file
    let file_path = dir.join("file.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "Hello, world!").unwrap();

    // Create subdirectory
    let subdir = dir.join("subdir");
    fs::create_dir(&subdir).unwrap();

    // Create file in subdirectory
    let subfile = subdir.join("nested.txt");
    let mut f = File::create(&subfile).unwrap();
    writeln!(f, "Nested file content").unwrap();

    // Create symlink (Unix only)
    #[cfg(unix)]
    {
        let link_path = dir.join("link.txt");
        std::os::unix::fs::symlink("file.txt", &link_path).unwrap();
    }
}

/// Verify extracted files match original test files
pub fn verify_files_match(original: &Path, extracted: &Path) {
    // Check file.txt
    let orig_content = fs::read_to_string(original.join("file.txt")).unwrap();
    let extr_content = fs::read_to_string(extracted.join("file.txt")).unwrap();
    assert_eq!(orig_content, extr_content, "file.txt content mismatch");

    // Check subdir/nested.txt
    let orig_nested = fs::read_to_string(original.join("subdir/nested.txt")).unwrap();
    let extr_nested = fs::read_to_string(extracted.join("subdir/nested.txt")).unwrap();
    assert_eq!(orig_nested, extr_nested, "nested.txt content mismatch");

    // Check symlink (Unix only)
    #[cfg(unix)]
    {
        let orig_link = fs::read_link(original.join("link.txt")).unwrap();
        let extr_link = fs::read_link(extracted.join("link.txt")).unwrap();
        assert_eq!(orig_link, extr_link, "symlink target mismatch");
    }
}

/// Assert command succeeded
pub fn assert_success(output: &Output, context: &str) {
    assert!(
        output.status.success(),
        "{} failed with status {:?}\nstderr: {}",
        context,
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Assert command failed
pub fn assert_failure(output: &Output, context: &str) {
    assert!(
        !output.status.success(),
        "{} should have failed but succeeded\nstdout: {}",
        context,
        String::from_utf8_lossy(&output.stdout)
    );
}

/// Get stdout as string
pub fn stdout_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).to_string()
}

/// Get stderr as string
pub fn stderr_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).to_string()
}
