//
// Copyright (c) 2025-2026 Jeff Garzik
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

/// Run pax with extra environment variables set.
///
/// pax calls `tzset()` at startup so that `$TZ` selects the zone its `-v` and
/// `listopt` times are rendered in; a test that pins an hour has to pin the
/// zone too, or it only passes where local time happens to equal the one the
/// fixture was written in.
pub fn run_pax_with_env(args: &[&str], vars: &[(&str, &str)]) -> Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_pax"));
    cmd.args(args);
    for (name, value) in vars {
        cmd.env(name, value);
    }
    cmd.output().expect("Failed to run pax")
}

/// Run pax with given arguments and binary stdin data, return output
pub fn run_pax_with_stdin_bytes(args: &[&str], stdin_data: &[u8]) -> Output {
    use std::process::Stdio;
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_pax"));
    cmd.args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = cmd.spawn().expect("Failed to spawn pax");
    if let Some(ref mut stdin) = child.stdin {
        stdin.write_all(stdin_data).expect("Failed to write stdin");
    }

    child.wait_with_output().expect("Failed to wait for pax")
}

/// Run pax with raw stdin bytes, in a specific directory. Extraction targets
/// the working directory, so a test that feeds a hand-built archive to `-r`
/// needs both at once.
pub fn run_pax_with_stdin_bytes_in_dir(args: &[&str], stdin_data: &[u8], dir: &Path) -> Output {
    use std::process::Stdio;
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_pax"));
    cmd.args(args)
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = cmd.spawn().expect("Failed to spawn pax");
    if let Some(ref mut stdin) = child.stdin {
        stdin.write_all(stdin_data).expect("Failed to write stdin");
    }

    child.wait_with_output().expect("Failed to wait for pax")
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

/// Path of a compatibility front-end (`tar` or `cpio`).
///
/// Both are symlinks to the pax binary created by `pax/build.rs`, and the
/// binary picks its command-line parser from argv[0], so they live beside the
/// pax executable cargo built for this test run.
pub fn front_end(name: &str) -> std::path::PathBuf {
    let path = Path::new(env!("CARGO_BIN_EXE_pax")).with_file_name(name);
    assert!(
        path.exists(),
        "{} is missing; pax/build.rs should have symlinked it to pax",
        path.display()
    );
    path
}

/// Run `tar` or `cpio` in `dir`, feeding `stdin_data` if given.
pub fn run_front_end(name: &str, args: &[&str], dir: &Path, stdin_data: Option<&[u8]>) -> Output {
    use std::process::Stdio;
    let mut child = Command::new(front_end(name))
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("failed to spawn {}: {}", name, e));

    {
        // Dropping stdin closes it, so a child reading a name list from a pipe
        // sees EOF instead of blocking forever.
        let mut stdin = child.stdin.take().expect("stdin was piped");
        if let Some(data) = stdin_data {
            stdin.write_all(data).expect("failed to write stdin");
        }
    }

    child
        .wait_with_output()
        .unwrap_or_else(|e| panic!("failed to wait for {}: {}", name, e))
}

/// Run `tar` in `dir`.
pub fn run_tar(args: &[&str], dir: &Path) -> Output {
    run_front_end("tar", args, dir, None)
}

/// Run `cpio` in `dir` with `stdin_data` on standard input.
pub fn run_cpio(args: &[&str], dir: &Path, stdin_data: &[u8]) -> Output {
    run_front_end("cpio", args, dir, Some(stdin_data))
}

/// Whether a system tool of this name can be run, for the cross-tool checks.
pub fn have_tool(name: &str) -> bool {
    Command::new(name)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
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

/// Assert the command exited with exactly `code`. `assert_failure` only
/// distinguishes zero from non-zero; POSIX pins specific statuses, and a
/// process killed by a signal (a panic reaching abort) has no code at all --
/// which this reports as such rather than as a plain inequality.
pub fn assert_exit_code(output: &Output, code: i32, context: &str) {
    match output.status.code() {
        Some(actual) => assert_eq!(
            actual,
            code,
            "{} exited {} (wanted {})\nstderr: {}",
            context,
            actual,
            code,
            String::from_utf8_lossy(&output.stderr)
        ),
        None => panic!(
            "{} was killed by a signal rather than exiting {}\nstderr: {}",
            context,
            code,
            String::from_utf8_lossy(&output.stderr)
        ),
    }
}

/// Get stdout as string
pub fn stdout_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).to_string()
}

/// Get stderr as string
pub fn stderr_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).to_string()
}
