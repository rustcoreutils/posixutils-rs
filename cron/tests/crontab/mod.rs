//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Write;
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::Duration;

fn run_test_base(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> Output {
    let relpath = if cfg!(debug_assertions) {
        format!("target/debug/{}", cmd)
    } else {
        format!("target/release/{}", cmd)
    };
    let test_bin_path = std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap() // Move up to the workspace root from the current package directory
        .join(relpath); // Adjust the path to the binary

    let mut command = Command::new(test_bin_path);
    let mut child = command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|_| panic!("failed to spawn command {}", cmd));

    // Separate the mutable borrow of stdin from the child process
    if let Some(mut stdin) = child.stdin.take() {
        let chunk_size = 1024; // Arbitrary chunk size, adjust if needed
        for chunk in stdin_data.chunks(chunk_size) {
            // Write each chunk
            if let Err(e) = stdin.write_all(chunk) {
                eprintln!("Error writing to stdin: {}", e);
                break;
            }
            // Flush after writing each chunk
            if let Err(e) = stdin.flush() {
                eprintln!("Error flushing stdin: {}", e);
                break;
            }

            // Sleep briefly to avoid CPU spinning
            thread::sleep(Duration::from_millis(10));
        }
        // Explicitly drop stdin to close the pipe
        drop(stdin);
    }

    // Ensure we wait for the process to complete after writing to stdin

    child.wait_with_output().expect("failed to wait for child")
}

#[test]
fn no_args() {
    let output = run_test_base("crontab", &vec![], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn dash_e() {
    let output = run_test_base("crontab", &vec!["-e".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn dash_l() {
    let output = run_test_base("crontab", &vec!["-l".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn dash_r() {
    let output = run_test_base("crontab", &vec!["-r".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}

#[test]
fn too_many_args() {
    let output = run_test_base("crontab", &vec!["-erl".to_string()], b"");
    assert_eq!(output.status.code(), Some(1));
}
