//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};

fn xargs_test(test_data: &str, expected_output: &str, args: Vec<&str>) {
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: args.into_iter().map(String::from).collect(),
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_basic() {
    xargs_test("one two three\n", "one two three\n", vec!["echo"]);
}

#[test]
fn xargs_default_echo() {
    // When no utility is specified, echo should be used
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![],
        stdin_data: String::from("hello world\n"),
        expected_out: String::from("hello world\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_with_maxnum() {
    xargs_test(
        "one two three\n",
        "one\ntwo\nthree\n",
        vec!["-n", "1", "echo"],
    );
}

#[test]
fn xargs_with_maxsize() {
    xargs_test(
        "one two three four five\n",
        "one\ntwo\nthree\nfour\nfive\n",
        vec!["-s", "11", "echo"],
    );
}

#[test]
fn xargs_with_eofstr() {
    xargs_test(
        "one two three STOP four five\n",
        "one two three\n",
        vec!["-E", "STOP", "echo"],
    );
}

#[test]
fn xargs_with_null_delimiter() {
    xargs_test("one\0two\0three\0", "one two three\n", vec!["-0", "echo"]);
}

#[test]
fn xargs_with_null_delimiter_trailing_non_null() {
    xargs_test("one\0two\0three", "one two three\n", vec!["-0", "echo"]);
}

#[test]
fn xargs_trace() {
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-t".to_string(), "echo".to_string()],
        stdin_data: String::from("one two three\n"),
        expected_err: String::from("echo one two three\n"),
        expected_out: String::from("one two three\n"),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_insert_mode() {
    // -I replstr: replace {} with input in utility args
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-I".to_string(),
            "{}".to_string(),
            "echo".to_string(),
            "prefix-{}-suffix".to_string(),
        ],
        stdin_data: String::from("one\ntwo\nthree\n"),
        expected_out: String::from("prefix-one-suffix\nprefix-two-suffix\nprefix-three-suffix\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_line_mode() {
    // -L 2: execute for each 2 lines
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-L".to_string(), "2".to_string(), "echo".to_string()],
        stdin_data: String::from("one\ntwo\nthree\nfour\nfive\n"),
        expected_out: String::from("one two\nthree four\nfive\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_line_mode_single() {
    // -L 1: execute for each line
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-L".to_string(), "1".to_string(), "echo".to_string()],
        stdin_data: String::from("one two\nthree four\n"),
        expected_out: String::from("one two\nthree four\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_exit_255() {
    // When utility returns 255, xargs should terminate
    // Note: sh -c uses $0 for the first argument, not $1
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-n".to_string(),
            "1".to_string(),
            "sh".to_string(),
            "-c".to_string(),
            r#"case "$0" in stop) exit 255;; *) echo "$0";; esac"#.to_string(),
        ],
        stdin_data: String::from("one\ntwo\nstop\nthree\nfour\n"),
        expected_out: String::from("one\ntwo\n"),
        expected_err: String::from(""),
        expected_exit_code: 1,
    });
}

#[test]
fn xargs_utility_not_found() {
    // Non-existent utility should return 127
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["nonexistent_utility_xyz".to_string()],
        stdin_data: String::from("test\n"),
        expected_out: String::from(""),
        expected_err: String::from("xargs: nonexistent_utility_xyz: No such file or directory\n"),
        expected_exit_code: 127,
    });
}

#[test]
fn xargs_utility_failed() {
    // When utility returns non-zero (but not 255), xargs should return 1
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["false".to_string()],
        stdin_data: String::from("test\n"),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: 1,
    });
}

#[test]
fn xargs_exit_mode() {
    // -x: exit if command line too long
    // Using a very small size that can't fit the argument
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-x".to_string(),
            "-s".to_string(),
            "5".to_string(),
            "echo".to_string(),
        ],
        stdin_data: String::from("verylongargument\n"),
        expected_out: String::from(""),
        expected_err: String::from("xargs: argument line too long\n"),
        expected_exit_code: 1,
    });
}

#[test]
fn xargs_quoted_args() {
    // Test quoted arguments
    xargs_test(
        "\"hello world\" 'foo bar'\n",
        "hello world foo bar\n",
        vec!["echo"],
    );
}

#[test]
fn xargs_escaped_chars() {
    // Test escaped characters
    xargs_test("hello\\ world\n", "hello world\n", vec!["echo"]);
}

#[test]
fn xargs_single_quotes() {
    // Test single quote (apostrophe) handling
    xargs_test("'hello world'\n", "hello world\n", vec!["echo"]);
}

#[test]
fn xargs_mixed_quotes() {
    // Test mix of single and double quotes
    xargs_test(
        "'single' \"double\" plain\n",
        "single double plain\n",
        vec!["echo"],
    );
}

#[test]
fn xargs_empty_input() {
    // Empty input should not execute the utility
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["echo".to_string()],
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_line_continuation() {
    // -L with trailing blank should continue to next line
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-L".to_string(), "1".to_string(), "echo".to_string()],
        stdin_data: String::from("one \ntwo\nthree\n"),
        expected_out: String::from("one two\nthree\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_insert_multiple_replstr() {
    // -I with multiple occurrences of replstr in same argument
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-I".to_string(),
            "{}".to_string(),
            "echo".to_string(),
            "{}-{}-{}".to_string(),
        ],
        stdin_data: String::from("test\n"),
        expected_out: String::from("test-test-test\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_insert_five_args_with_replstr() {
    // -I with replstr in 5 arguments (POSIX requires at least 5)
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-I".to_string(),
            "{}".to_string(),
            "echo".to_string(),
            "{}".to_string(),
            "{}".to_string(),
            "{}".to_string(),
            "{}".to_string(),
            "{}".to_string(),
        ],
        stdin_data: String::from("x\n"),
        expected_out: String::from("x x x x x\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_combine_n_and_s() {
    // Combining -n and -s should work together
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-n".to_string(),
            "2".to_string(),
            "-s".to_string(),
            "20".to_string(),
            "echo".to_string(),
        ],
        stdin_data: String::from("a b c d e\n"),
        expected_out: String::from("a b\nc d\ne\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_insert_arg_size_limit() {
    // -I mode should enforce size limit on constructed arguments
    // Create a string that when repeated will exceed 4096 bytes
    let long_input = "x".repeat(5000);
    let test_plan = TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-I".to_string(),
            "{}".to_string(),
            "echo".to_string(),
            "{}".to_string(),
        ],
        stdin_data: format!("{}\n", long_input),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: 1,
    };

    // Run with custom checker since error format includes Rust error wrapper
    run_test_with_checker(test_plan, |plan, output| {
        assert_eq!(output.status.code(), Some(plan.expected_exit_code));
        assert_eq!(String::from_utf8_lossy(&output.stdout), plan.expected_out);
        // Just verify stderr contains the key message parts
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("constructed argument"),
            "Expected error about constructed argument"
        );
        assert!(stderr.contains("5000 bytes"), "Expected size in error");
        assert!(
            stderr.contains("4096 byte limit"),
            "Expected limit in error"
        );
    });
}
