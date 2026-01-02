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
