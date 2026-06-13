//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::fs::File;
use std::io::Write;
use std::process::Output;
use tempfile::TempDir;

/// Create a minimal locale source file
fn create_minimal_locale(dir: &TempDir) -> std::path::PathBuf {
    let path = dir.path().join("minimal.locale");
    let content = r#"
comment_char %
escape_char /

LC_NUMERIC
decimal_point "."
thousands_sep ","
grouping 3;3
END LC_NUMERIC

LC_MESSAGES
yesexpr "^[yY]"
noexpr "^[nN]"
END LC_MESSAGES
"#;
    let mut file = File::create(&path).unwrap();
    write!(file, "{}", content).unwrap();
    path
}

/// Test localedef with minimal input
#[test]
fn test_localedef_minimal() {
    let temp_dir = TempDir::new().unwrap();
    let locale_src = create_minimal_locale(&temp_dir);
    let output_path = temp_dir.path().join("test_locale");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-i"),
                locale_src.to_str().unwrap().to_string(),
                output_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            assert!(
                output.status.success() || output.status.code() == Some(1),
                "localedef should succeed or return warnings"
            );
        },
    );
}

/// Test localedef with -c flag (continue despite warnings)
#[test]
fn test_localedef_continue_warnings() {
    let temp_dir = TempDir::new().unwrap();
    let locale_src = create_minimal_locale(&temp_dir);
    let output_path = temp_dir.path().join("test_locale_c");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-c"),
                String::from("-i"),
                locale_src.to_str().unwrap().to_string(),
                output_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            assert!(
                output.status.success(),
                "localedef -c should succeed with warnings"
            );
        },
    );
}

/// Test localedef with verbose mode
#[test]
fn test_localedef_verbose() {
    let temp_dir = TempDir::new().unwrap();
    let locale_src = create_minimal_locale(&temp_dir);
    let output_path = temp_dir.path().join("test_locale_v");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-v"),
                String::from("-c"),
                String::from("-i"),
                locale_src.to_str().unwrap().to_string(),
                output_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            // With -v, should have some output
            assert!(output.status.success(), "localedef -v should succeed");
        },
    );
}

/// LD-5/LD-9: a valid source exits 0 and reports the processed categories on
/// standard output.
#[test]
fn test_localedef_reports_categories() {
    let temp_dir = TempDir::new().unwrap();
    let src = temp_dir.path().join("ok.locale");
    let mut f = File::create(&src).unwrap();
    write!(
        f,
        "LC_NUMERIC\ndecimal_point \".\"\nthousands_sep \",\"\nEND LC_NUMERIC\n"
    )
    .unwrap();
    let out = temp_dir.path().join("loc");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-i"),
                src.to_str().unwrap().to_string(),
                out.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            assert_eq!(output.status.code(), Some(0));
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("LC_NUMERIC"), "stdout report: {stdout:?}");
        },
    );
}

/// LD-12: an `END` that does not match the open category is an error, so no
/// output is created and the exit status is > 3.
#[test]
fn test_localedef_mismatched_end_errors() {
    let temp_dir = TempDir::new().unwrap();
    let src = temp_dir.path().join("bad.locale");
    let mut f = File::create(&src).unwrap();
    write!(f, "LC_NUMERIC\ndecimal_point \".\"\nEND LC_MONETARY\n").unwrap();
    let out = temp_dir.path().join("loc");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-i"),
                src.to_str().unwrap().to_string(),
                out.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 4,
        },
        |_plan, output: &Output| {
            assert!(output.status.code().unwrap() > 3, "errors must exit > 3");
            assert!(!out.exists(), "no output on error");
        },
    );
}

/// LD-10/LD-11: an unknown keyword is a warning shown on stderr (even without
/// -v); without -c the warning prevents output and the exit status is > 3.
#[test]
fn test_localedef_unknown_keyword_warns() {
    let temp_dir = TempDir::new().unwrap();
    let src = temp_dir.path().join("warn.locale");
    let mut f = File::create(&src).unwrap();
    write!(f, "LC_NUMERIC\nbogus_keyword \"x\"\nEND LC_NUMERIC\n").unwrap();
    let out = temp_dir.path().join("loc");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-i"),
                src.to_str().unwrap().to_string(),
                out.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 4,
        },
        |_plan, output: &Output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("bogus_keyword"), "warning: {stderr:?}");
            assert!(output.status.code().unwrap() > 3);
        },
    );
}

/// LD-2: a `-f charmap` that cannot be read means the charset is unsupported;
/// exit 2 and no output.
#[test]
fn test_localedef_missing_charmap_exit2() {
    let temp_dir = TempDir::new().unwrap();
    let src = temp_dir.path().join("ok.locale");
    let mut f = File::create(&src).unwrap();
    write!(f, "LC_NUMERIC\ndecimal_point \".\"\nEND LC_NUMERIC\n").unwrap();
    let out = temp_dir.path().join("loc");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-f"),
                String::from("/nonexistent/charmap.xyz"),
                String::from("-i"),
                src.to_str().unwrap().to_string(),
                out.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 2,
        },
        |_plan, output: &Output| {
            assert_eq!(output.status.code(), Some(2), "unsupported charset -> 2");
            assert!(!out.exists(), "no output");
        },
    );
}

/// Test localedef with empty input
#[test]
fn test_localedef_empty_input() {
    let temp_dir = TempDir::new().unwrap();
    let empty_src = temp_dir.path().join("empty.locale");
    File::create(&empty_src).unwrap();
    let output_path = temp_dir.path().join("empty_locale");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-c"),
                String::from("-i"),
                empty_src.to_str().unwrap().to_string(),
                output_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            // Empty input should still work with -c
            assert!(
                output.status.success(),
                "localedef should handle empty input with -c"
            );
        },
    );
}
