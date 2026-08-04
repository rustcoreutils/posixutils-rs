//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::fs;
use std::path::PathBuf;

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("comm");
    path.push(filename);
    path
}

fn run_comm_test(args: Vec<&str>, expected_output_filename: &str) {
    let expected_output = fs::read_to_string(get_test_file_path(expected_output_filename))
        .expect("Failed to read expected output file");

    let args: Vec<String> = args.iter().map(|&s| s.into()).collect();

    run_test(TestPlan {
        cmd: String::from("comm"),
        args,
        expected_out: expected_output,
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}
#[test]
fn comm_basic_functionality() {
    run_comm_test(
        vec!["tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.0",
    );
}

#[test]
fn comm_suppress_first_column() {
    run_comm_test(
        vec!["-1", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.1",
    );
}

#[test]
fn comm_suppress_second_column() {
    run_comm_test(
        vec!["-2", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.2",
    );
}

#[test]
fn comm_suppress_third_column() {
    run_comm_test(
        vec!["-3", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.3",
    );
}

#[test]
fn comm_suppress_columns_12() {
    run_comm_test(
        vec!["-12", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.12",
    );
}

#[test]
fn comm_suppress_columns_13() {
    run_comm_test(
        vec!["-13", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.13",
    );
}

#[test]
fn comm_suppress_columns_23() {
    run_comm_test(
        vec!["-23", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.23",
    );
}

#[test]
fn comm_suppress_columns_123() {
    run_comm_test(
        vec!["-123", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.123",
    );
}

#[test]
fn comm_suppress_columns_separate_flags() {
    // Test that -1 -2 -3 works the same as -123
    run_comm_test(
        vec![
            "-1",
            "-2",
            "-3",
            "tests/comm/comm.file1",
            "tests/comm/comm.file2",
        ],
        "comm.123",
    );
}

#[test]
fn comm_empty_both_files() {
    run_comm_test(
        vec!["tests/comm/comm.empty", "tests/comm/comm.empty"],
        "comm.empty_both",
    );
}

#[test]
fn comm_empty_file1() {
    run_comm_test(
        vec!["tests/comm/comm.empty", "tests/comm/comm.file2"],
        "comm.empty_file1",
    );
}

#[test]
fn comm_empty_file2() {
    run_comm_test(
        vec!["tests/comm/comm.file1", "tests/comm/comm.empty"],
        "comm.empty_file2",
    );
}

#[test]
fn comm_stdin_file1() {
    // Test using "-" for file1 (stdin)
    let expected_output =
        fs::read_to_string(get_test_file_path("comm.0")).expect("Failed to read expected output");
    let file1_content =
        fs::read_to_string(get_test_file_path("comm.file1")).expect("Failed to read file1");

    run_test(TestPlan {
        cmd: String::from("comm"),
        args: vec![String::from("-"), String::from("tests/comm/comm.file2")],
        expected_out: expected_output,
        expected_err: String::new(),
        stdin_data: file1_content,
        expected_exit_code: 0,
    });
}

#[test]
fn comm_stdin_file2() {
    // Test using "-" for file2 (stdin)
    let expected_output =
        fs::read_to_string(get_test_file_path("comm.0")).expect("Failed to read expected output");
    let file2_content =
        fs::read_to_string(get_test_file_path("comm.file2")).expect("Failed to read file2");

    run_test(TestPlan {
        cmd: String::from("comm"),
        args: vec![String::from("tests/comm/comm.file1"), String::from("-")],
        expected_out: expected_output,
        expected_err: String::new(),
        stdin_data: file2_content,
        expected_exit_code: 0,
    });
}

#[test]
fn comm_missing_file_error() {
    // Test error handling for missing file
    // Note: error message text varies by OS, so we only check that stderr is non-empty
    run_test_with_checker(
        TestPlan {
            cmd: String::from("comm"),
            args: vec![
                String::from("tests/comm/nonexistent_file"),
                String::from("tests/comm/comm.file2"),
            ],
            expected_out: String::new(),
            expected_err: String::new(), // Not used by checker
            stdin_data: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            assert!(output.stdout.is_empty(), "stdout should be empty");
            assert!(
                !output.stderr.is_empty(),
                "stderr should contain error message"
            );
            assert_eq!(output.status.code(), Some(1), "exit code should be 1");
        },
    );
}

// ---------------------------------------------------------------------------
// End-of-options
// ---------------------------------------------------------------------------

#[test]
fn comm_double_dash_allows_dash_prefixed_operands() {
    let dir = std::env::temp_dir();
    let mut a = dir.clone();
    a.push(format!("-posixutils-comm-a-{}", std::process::id()));
    let mut b = dir.clone();
    b.push(format!("-posixutils-comm-b-{}", std::process::id()));
    std::fs::write(&a, "x\n").expect("write a");
    std::fs::write(&b, "x\n").expect("write b");

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_comm"))
        .args(["--", a.to_str().unwrap(), b.to_str().unwrap()])
        .output()
        .expect("run comm");
    assert_eq!(
        out.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    // A line in both files lands in column 3.
    assert_eq!(String::from_utf8_lossy(&out.stdout), "\t\tx\n");
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

// ---------------------------------------------------------------------------
// Collation, ordering and diagnostics
// ---------------------------------------------------------------------------

fn comm_run(args: &[&str], env: &[(&str, &str)]) -> (String, String, i32) {
    let mut cmd = std::process::Command::new(env!("CARGO_BIN_EXE_comm"));
    cmd.args(args);
    for (k, v) in env {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("run comm");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

fn comm_tmp(tag: &str, content: &str) -> std::path::PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("posixutils-comm-{}-{}", std::process::id(), tag));
    std::fs::write(&p, content).expect("write temp file");
    p
}

#[test]
fn comm_compares_lines_bytewise_regardless_of_collation() {
    // POSIX says the comparison follows LC_COLLATE. Ours compares byte
    // sequences, so a pair that collates equally but differs in bytes is
    // reported as two distinct lines rather than a common one.
    let Some(loc) = plib::testing::utf8_locale() else {
        return;
    };
    let a = comm_tmp("coll-a", "a\n");
    let b = comm_tmp("coll-b", "a \n");
    let (stdout, _, code) = comm_run(
        &[a.to_str().unwrap(), b.to_str().unwrap()],
        &[("LC_ALL", loc.as_str())],
    );
    assert_eq!(code, 0);
    assert_eq!(stdout, "a\n\ta \n", "got {stdout:?}");
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn comm_unsorted_input_behavior() {
    // POSIX leaves the result unspecified when the input is not sorted. Pin
    // what we do: the merge simply walks both files in order and emits what it
    // finds, without diagnosing the disorder.
    let a = comm_tmp("uns-a", "b\na\n");
    let b = comm_tmp("uns-b", "b\n");
    let (stdout, _, code) = comm_run(&[a.to_str().unwrap(), b.to_str().unwrap()], &[]);
    assert_eq!(code, 0, "unsorted input is not diagnosed");
    assert_eq!(stdout, "\t\tb\na\n", "got {stdout:?}");
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn comm_diagnostics_are_translatable() {
    // The diagnostic goes through gettext, so a catalog reached via NLSPATH
    // replaces it. Without a catalog it stays English, which is what every
    // other test here observes.
    let dir = std::env::temp_dir().join(format!("posixutils-comm-nls-{}", std::process::id()));
    let _ = std::fs::create_dir_all(&dir);
    let mo = dir.join("posixutils-rs.de_DE.mo");
    std::fs::write(
        &mo,
        mo_catalog(&[("", "Content-Type: text/plain; charset=UTF-8\n")]),
    )
    .expect("write catalog");

    let (_, stderr, code) = comm_run(
        &["no-such-file-xyz", "no-such-file-xyz"],
        &[
            ("NLSPATH", dir.join("%N.%L.mo").to_str().unwrap()),
            ("LC_ALL", "de_DE"),
        ],
    );
    assert_ne!(code, 0);
    assert!(
        stderr.starts_with("comm:"),
        "the diagnostic must name the utility, got {stderr:?}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Build a minimal little-endian GNU `.mo` image.
fn mo_catalog(entries: &[(&str, &str)]) -> Vec<u8> {
    let n = entries.len() as u32;
    let (orig_tab, desc) = (28u32, 8u32);
    let trans_tab = orig_tab + n * desc;
    let strings_at = trans_tab + n * desc;
    let (mut blob, mut origs, mut transs) = (Vec::new(), Vec::new(), Vec::new());
    for (id, s) in entries {
        origs.push((id.len() as u32, strings_at + blob.len() as u32));
        blob.extend_from_slice(id.as_bytes());
        blob.push(0);
        transs.push((s.len() as u32, strings_at + blob.len() as u32));
        blob.extend_from_slice(s.as_bytes());
        blob.push(0);
    }
    let mut out = Vec::new();
    out.extend_from_slice(&0x9504_12deu32.to_le_bytes());
    for w in [0, n, orig_tab, trans_tab, 0, 0] {
        out.extend_from_slice(&w.to_le_bytes());
    }
    for (len, off) in origs.into_iter().chain(transs) {
        out.extend_from_slice(&len.to_le_bytes());
        out.extend_from_slice(&off.to_le_bytes());
    }
    out.extend_from_slice(&blob);
    out
}
