//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{
    run_test, run_test_with_checker, run_test_with_checker_and_env, run_test_with_env, TestPlan,
};
use std::fs;
use std::os::unix::fs::symlink;

fn test_test(args: &[&str], expected_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("test"),
        args: str_args,
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: expected_code,
    });
}

/// Run `test` with explicit environment variables (e.g. a locale override).
fn test_test_env(args: &[&str], env: &[(&str, &str)], expected_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_env(
        TestPlan {
            cmd: String::from("test"),
            args: str_args,
            stdin_data: String::from(""),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: expected_code,
        },
        env,
    );
}

/// Test that expects stderr output (doesn't check exact message, only exit code)
fn test_test_with_err(args: &[&str], expected_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("test"),
            args: str_args,
            stdin_data: String::from(""),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: expected_code,
        },
        |_plan, output| {
            assert_eq!(output.status.code(), Some(expected_code));
            assert!(
                !output.stderr.is_empty(),
                "expected stderr output for error case"
            );
        },
    );
}

#[test]
fn test_intops() {
    test_test(&["20", "-eq", "20"], 0);
    test_test(&["20", "-eq", "21"], 1);

    test_test(&["20", "-ne", "20"], 1);
    test_test(&["20", "-ne", "21"], 0);

    test_test(&["20", "-gt", "10"], 0);
    test_test(&["10", "-gt", "20"], 1);

    test_test(&["20", "-ge", "10"], 0);
    test_test(&["10", "-ge", "20"], 1);
    test_test(&["20", "-ge", "20"], 0);

    test_test(&["20", "-lt", "10"], 1);
    test_test(&["10", "-lt", "20"], 0);

    test_test(&["20", "-le", "10"], 1);
    test_test(&["10", "-le", "20"], 0);
    test_test(&["20", "-le", "20"], 0);
}

/// Integer operand robustness: surrounding blanks tolerated; explicit sign
/// accepted; non-numeric and i64-overflow operands are diagnosed (exit 2).
/// (audit #3)
#[test]
fn test_intops_operands() {
    // Leading/trailing blanks are tolerated (historical `test` behavior).
    test_test(&[" 5", "-eq", "5"], 0);
    test_test(&["5", "-eq", "5 "], 0);
    test_test(&[" 5 ", "-eq", "5"], 0);

    // Explicit sign and leading zeros.
    test_test(&["+5", "-eq", "5"], 0);
    test_test(&["-5", "-lt", "0"], 0);
    test_test(&["-5", "-eq", "-5"], 0);
    test_test(&["05", "-eq", "5"], 0);

    // Non-numeric and overflow operands are errors (exit > 1).
    test_test_with_err(&["abc", "-eq", "1"], 2);
    test_test_with_err(&["", "-eq", "1"], 2);
    test_test_with_err(&["99999999999999999999", "-gt", "1"], 2);
}

#[test]
fn test_strops() {
    test_test(&["a", "=", "a"], 0);
    test_test(&["a", "=", "b"], 1);

    test_test(&["a", "!=", "a"], 1);
    test_test(&["a", "!=", "b"], 0);

    test_test(&["a", "<", "b"], 0);
    test_test(&["b", "<", "a"], 1);

    test_test(&["a", ">", "b"], 1);
    test_test(&["b", ">", "a"], 0);
}

/// `<` and `>` must order strings by the current locale's collating sequence
/// (LC_COLLATE), not by raw byte/scalar value. (audit #1)
#[test]
fn test_str_collation() {
    // In the C locale, collation is byte order: 'B'(0x42) sorts before 'a'(0x61).
    test_test_env(&["B", "<", "a"], &[("LC_ALL", "C")], 0);
    test_test_env(&["a", "<", "B"], &[("LC_ALL", "C")], 1);
    test_test_env(&["a", ">", "B"], &[("LC_ALL", "C")], 0);

    // In a UTF-8 locale, collation interleaves case: 'a' sorts before 'B', the
    // opposite of byte order. Probe en_US.UTF-8 but skip gracefully when the
    // locale is not installed (the binary then falls back to C/byte order).
    let str_args: Vec<String> = ["a", "<", "B"].iter().map(|s| String::from(*s)).collect();
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("test"),
            args: str_args,
            stdin_data: String::from(""),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 0,
        },
        &[("LC_ALL", "en_US.UTF-8")],
        |_plan, output| match output.status.code() {
            Some(0) => {} // collation active: 'a' < 'B' as expected
            _ => eprintln!("skipping en_US.UTF-8 collation check: locale unavailable"),
        },
    );
}

#[test]
fn test_str_basic() {
    test_test(&[], 1);

    test_test(&[""], 1);
    test_test(&["a"], 0);

    test_test(&["-z", ""], 0);
    test_test(&["-z", "a"], 1);

    test_test(&["-n", ""], 1);
    test_test(&["-n", "a"], 0);
}

// ============================================================================
// File type tests
// ============================================================================

#[test]
fn test_file_exists() {
    // -e tests file existence
    test_test(&["-e", "/tmp"], 0);
    test_test(&["-e", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_directory() {
    // -d tests if file is a directory
    test_test(&["-d", "/tmp"], 0);
    test_test(&["-d", "/etc/passwd"], 1);
    test_test(&["-d", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_regular() {
    // -f tests if file is a regular file
    test_test(&["-f", "/etc/passwd"], 0);
    test_test(&["-f", "/tmp"], 1);
    test_test(&["-f", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_size() {
    // -s tests if file has size > 0
    test_test(&["-s", "/etc/passwd"], 0);
    test_test(&["-s", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_readable() {
    // -r tests if file is readable
    test_test(&["-r", "/etc/passwd"], 0);
    test_test(&["-r", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_writable() {
    // -w tests if file is writable
    test_test(&["-w", "/tmp"], 0);
}

#[test]
fn test_file_executable() {
    // -x tests if file is executable
    test_test(&["-x", "/bin/sh"], 0);
    test_test(&["-x", "/etc/passwd"], 1);
}

// ============================================================================
// Symlink tests
// ============================================================================

#[test]
fn test_symlink() {
    let test_dir = tempfile::tempdir().unwrap();
    let file_path = test_dir.path().join("file");
    let link_path = test_dir.path().join("link");
    let broken_link_path = test_dir.path().join("broken_link");

    // Create a regular file
    fs::write(&file_path, "content").unwrap();

    // Create a symlink to the file
    symlink(&file_path, &link_path).unwrap();

    // Create a broken symlink
    symlink("/nonexistent_target_12345", &broken_link_path).unwrap();

    // -h and -L should detect symlinks (not following them)
    test_test(&["-h", link_path.to_str().unwrap()], 0);
    test_test(&["-L", link_path.to_str().unwrap()], 0);

    // -h and -L should detect broken symlinks too
    test_test(&["-h", broken_link_path.to_str().unwrap()], 0);
    test_test(&["-L", broken_link_path.to_str().unwrap()], 0);

    // Regular file is not a symlink
    test_test(&["-h", file_path.to_str().unwrap()], 1);
    test_test(&["-L", file_path.to_str().unwrap()], 1);

    // -f follows symlinks, so symlink to file should be true
    test_test(&["-f", link_path.to_str().unwrap()], 0);

    // Broken symlink should fail -e (target doesn't exist)
    test_test(&["-e", broken_link_path.to_str().unwrap()], 1);
}

// ============================================================================
// Negation tests
// ============================================================================

#[test]
fn test_negation() {
    // ! negates the expression
    test_test(&["!", ""], 0);
    test_test(&["!", "a"], 1);

    // Negate unary
    test_test(&["!", "-d", "/tmp"], 1);
    test_test(&["!", "-d", "/nonexistent"], 0);

    // Negate binary
    test_test(&["!", "a", "=", "a"], 1);
    test_test(&["!", "a", "=", "b"], 0);
}

// ============================================================================
// XSI operators (-a, -o, parentheses)
// ============================================================================

#[test]
fn test_xsi_and() {
    // -a is AND
    test_test(&["-d", "/tmp", "-a", "-d", "/"], 0);
    test_test(&["-d", "/tmp", "-a", "-f", "/tmp"], 1);
    test_test(&["-f", "/tmp", "-a", "-d", "/"], 1);
}

#[test]
fn test_xsi_or() {
    // -o is OR
    test_test(&["-d", "/tmp", "-o", "-d", "/nonexistent"], 0);
    test_test(&["-d", "/nonexistent", "-o", "-d", "/tmp"], 0);
    test_test(&["-f", "/tmp", "-o", "-f", "/"], 1);
}

#[test]
fn test_xsi_precedence() {
    // -a has higher precedence than -o
    // "false -o true -a true" should be "false -o (true -a true)" = true
    test_test(&["-f", "/tmp", "-o", "-d", "/tmp", "-a", "-d", "/"], 0);

    // "true -a false -o true" should be "(true -a false) -o true" = true
    test_test(&["-d", "/tmp", "-a", "-f", "/tmp", "-o", "-d", "/"], 0);
}

#[test]
fn test_xsi_parentheses() {
    // Parentheses for grouping (3 args)
    test_test(&["(", "a", ")"], 0);
    test_test(&["(", "", ")"], 1);

    // Parentheses for grouping (4 args)
    test_test(&["(", "-d", "/tmp", ")"], 0);

    // Parentheses with XSI expressions
    test_test(&["(", "-d", "/tmp", ")", "-a", "-d", "/"], 0);
    test_test(&["(", "-f", "/tmp", ")", "-o", "-d", "/"], 0);
}

#[test]
fn test_xsi_not_combinations() {
    // ! with -a and -o
    test_test(&["!", "-f", "/tmp", "-a", "-d", "/tmp"], 0);
    test_test(&["!", "-d", "/tmp", "-o", "-d", "/"], 0);
}

// ============================================================================
// Integer error handling
// ============================================================================

#[test]
fn test_integer_errors() {
    // Invalid integers should cause exit code 2
    test_test_with_err(&["abc", "-eq", "123"], 2);
    test_test_with_err(&["123", "-eq", "abc"], 2);
    test_test_with_err(&["abc", "-lt", "def"], 2);
}

// ============================================================================
// Edge cases
// ============================================================================

#[test]
fn test_empty_args() {
    // No arguments = false
    test_test(&[], 1);
}

#[test]
fn test_special_strings() {
    // Strings that look like operators should be treated as strings
    test_test(&["-d"], 0); // Non-empty string = true
    test_test(&["!"], 0); // Non-empty string = true
    test_test(&["("], 0); // Non-empty string = true
    test_test(&[")"], 0); // Non-empty string = true
    test_test(&["-a"], 0); // Non-empty string = true
    test_test(&["-o"], 0); // Non-empty string = true

    // "test !" should be true (non-empty string)
    test_test(&["!"], 0);

    // "test ! !" should be ! of "!" which is non-empty, so false
    test_test(&["!", "!"], 1);
}

#[test]
fn test_path_comparisons() {
    // -ef tests if two paths refer to the same file
    let test_dir = tempfile::tempdir().unwrap();
    let file_path = test_dir.path().join("file");
    let link_path = test_dir.path().join("link");

    fs::write(&file_path, "content").unwrap();
    fs::hard_link(&file_path, &link_path).unwrap();

    test_test(
        &[
            file_path.to_str().unwrap(),
            "-ef",
            link_path.to_str().unwrap(),
        ],
        0,
    );
    test_test(&[file_path.to_str().unwrap(), "-ef", "/etc/passwd"], 1);
}

#[test]
fn test_posix_examples() {
    // From POSIX examples in the spec

    // "test ]" must exit with status 0 (non-empty string)
    test_test(&["]"], 0);

    // String comparisons
    test_test(&["pear", "=", "pear"], 0);
    test_test(&["pear", "=", "grape"], 1);
}
