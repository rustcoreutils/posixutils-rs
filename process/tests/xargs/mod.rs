//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::fs::{self, File};
use std::os::unix::fs::PermissionsExt;

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
        expected_err: String::from("xargs: sh: exited with status 255; aborting\n"),
        expected_exit_code: 1,
    });
}

/// An invocation killed by a signal stops xargs too, with a diagnostic, and
/// no further input is processed (POSIX xargs, CONSEQUENCES OF ERRORS).
#[test]
fn xargs_utility_killed_by_a_signal() {
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-n".to_string(),
            "1".to_string(),
            "sh".to_string(),
            "-c".to_string(),
            r#"case "$0" in stop) kill -TERM $$;; *) echo "$0";; esac"#.to_string(),
        ],
        stdin_data: String::from("one\nstop\nthree\n"),
        expected_out: String::from("one\n"),
        expected_err: String::from("xargs: sh: terminated by signal 15\n"),
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

// #X1: POSIX requires the utility to run exactly once on empty input when -r
// is not given. With `echo` and no args, that produces a single blank line.
#[test]
fn xargs_empty_input_runs_once() {
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["echo".to_string()],
        stdin_data: String::from(""),
        expected_out: String::from("\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// #X1: -r (--no-run-if-empty) suppresses the run on empty input.
#[test]
fn xargs_empty_input_no_run_if_empty() {
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-r".to_string(), "echo".to_string()],
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// #X1: blank-only input is also "no arguments" -> one run without -r.
#[test]
fn xargs_blank_input_runs_once() {
    xargs_test("   \n  \n", "\n", vec!["echo"]);
}

// #X2: a multibyte UTF-8 argument must survive intact (was mangled to "Ã©"
// by the per-byte `as char` cast).
#[test]
fn xargs_utf8_argument() {
    xargs_test("é\n", "é\n", vec!["echo"]);
}

#[test]
fn xargs_utf8_multiple() {
    xargs_test("café naïve\n", "café naïve\n", vec!["echo"]);
}

// #X3: a newline inside a quoted string is an error (exit non-zero).
#[test]
fn xargs_newline_in_quote_errors() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("xargs"),
            args: vec!["echo".to_string()],
            stdin_data: String::from("\"unterminated\n more\""),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            assert_eq!(output.status.code(), Some(1));
        },
    );
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

#[test]
fn xargs_escaped_newline() {
    // Backslash-newline should continue the argument
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["echo".to_string()],
        stdin_data: String::from("hello\\\nworld\n"),
        expected_out: String::from("helloworld\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_empty_quoted_strings() {
    // Empty quoted strings should produce empty arguments
    // Note: echo with empty args still produces a newline
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["echo".to_string()],
        stdin_data: String::from("\"\" '' normal\n"),
        expected_out: String::from("  normal\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_insert_mode_empty_lines() {
    // Empty lines in -I mode should be skipped
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-I".to_string(),
            "{}".to_string(),
            "echo".to_string(),
            "[{}]".to_string(),
        ],
        stdin_data: String::from("one\n\ntwo\n\n\nthree\n"),
        expected_out: String::from("[one]\n[two]\n[three]\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_eof_string_partial_match() {
    // EOF string as part of a larger argument should NOT trigger EOF
    // POSIX: only an argument consisting of JUST the EOF string triggers EOF
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-E".to_string(), "STOP".to_string(), "echo".to_string()],
        stdin_data: String::from("one STOPNOW two\n"),
        expected_out: String::from("one STOPNOW two\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_eof_string_quoted_becomes_match() {
    // A quoted argument that equals EOF string after unquoting DOES trigger EOF
    // POSIX: EOF detected after quote processing
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-E".to_string(), "STOP".to_string(), "echo".to_string()],
        stdin_data: String::from("one \"STOP\" two\n"),
        expected_out: String::from("one\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_line_mode_empty_lines() {
    // Empty lines should not count toward -L line count
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-L".to_string(), "2".to_string(), "echo".to_string()],
        stdin_data: String::from("one\n\ntwo\n\nthree\nfour\n"),
        expected_out: String::from("one two\nthree four\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_explicit_eof_disable() {
    // -E "" should explicitly disable EOF processing
    // The underscore should be treated as a normal argument
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-E".to_string(), "".to_string(), "echo".to_string()],
        stdin_data: String::from("one _ two\n"),
        expected_out: String::from("one _ two\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_multiple_batch_verification() {
    // Test that -n correctly batches multiple invocations
    // With -n 2, we should get 3 invocations for 5 args
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-n".to_string(),
            "2".to_string(),
            "echo".to_string(),
            "prefix".to_string(),
        ],
        stdin_data: String::from("a b c d e\n"),
        expected_out: String::from("prefix a b\nprefix c d\nprefix e\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_whitespace_only_input() {
    // #X1: whitespace-only input yields no arguments, so POSIX runs the
    // utility exactly once (without -r). With echo that is one blank line.
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["echo".to_string()],
        stdin_data: String::from("   \n\t\n  \t  \n"),
        expected_out: String::from("\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_insert_mode_preserves_internal_spaces() {
    // -I mode should preserve internal spaces, only strip leading blanks
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![
            "-I".to_string(),
            "{}".to_string(),
            "echo".to_string(),
            "[{}]".to_string(),
        ],
        stdin_data: String::from("  hello world  \n"),
        expected_out: String::from("[hello world  ]\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_exit_code_126_cannot_invoke() {
    // Create a file that exists but is not executable
    let test_dir = plib::tmp::tempdir().unwrap();
    let non_exec_file = test_dir.path().join("not_executable");

    // Create the file
    File::create(&non_exec_file).expect("Failed to create test file");

    // Make sure it's not executable (mode 0o644)
    let mut perms = fs::metadata(&non_exec_file)
        .expect("Failed to get metadata")
        .permissions();
    perms.set_mode(0o644);
    fs::set_permissions(&non_exec_file, perms).expect("Failed to set permissions");

    let test_plan = TestPlan {
        cmd: String::from("xargs"),
        args: vec![non_exec_file.to_string_lossy().to_string()],
        stdin_data: String::from("test\n"),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: 126,
    };

    run_test_with_checker(test_plan, |plan, output| {
        assert_eq!(
            output.status.code(),
            Some(plan.expected_exit_code),
            "Expected exit code 126 for non-executable file"
        );
        // Stderr should contain an error message
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!stderr.is_empty(), "Expected error message on stderr");
    });
}

// #X4: `-E` and `-I` together. The suite covers each flag alone
// (`xargs_with_eofstr`, `xargs_insert_mode`) but nothing combined them, and the
// EOF string used to be ignored once insert mode was active.
#[test]
fn xargs_eof_string_honored_in_insert_mode() {
    xargs_test(
        "a\nSTOP\nb\n",
        "[a]\n",
        vec!["-E", "STOP", "-I", "{}", "echo", "[{}]"],
    );
}

// The counterpart: with a different EOF string the STOP line is ordinary data,
// so all three lines are processed. Without this, the test above could pass
// because insert mode had stopped after one line for an unrelated reason.
#[test]
fn xargs_insert_mode_without_matching_eof_string_processes_all_lines() {
    xargs_test(
        "a\nSTOP\nb\n",
        "[a]\n[STOP]\n[b]\n",
        vec!["-E", "HALT", "-I", "{}", "echo", "[{}]"],
    );
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn option_argument_may_begin_with_hyphen() {
    for opt in ["-L", "-n", "-s", "-E", "-I"] {
        plib::testing::assert_hyphen_option_argument("xargs", &[opt, "-zq", "--help"]);
    }
}

// XBD 12.2, Guideline 9: xargs's options all come before the utility, so
// every word after the utility name belongs to the utility. `xargs touch -t
// STAMP` traced the command and ran `touch STAMP`, and `xargs -r0 rm -r` was
// refused as a repeated `-r`.
#[test]
fn options_after_utility_belong_to_the_utility() {
    // echo does not take -t, -r or -x as options, so it prints them.
    xargs_test("x\n", "-t x\n", vec!["echo", "-t"]);
    xargs_test("x\0", "-r x\n", vec!["-r0", "echo", "-r"]);
    xargs_test("x\n", "-x -L 1 x\n", vec!["echo", "-x", "-L", "1"]);
}

// `--` still ends xargs's options, and a `--` after the utility name is one of
// the utility's arguments.
#[test]
fn double_dash_ends_options_and_later_one_is_passed_through() {
    xargs_test("x\n", "-t x\n", vec!["--", "echo", "-t"]);
    xargs_test("x\n", "-- -t x\n", vec!["echo", "--", "-t"]);
}

// Input arguments are byte strings, as pathnames are.  Bytes that are not
// valid UTF-8 used to become U+FFFD, so `find . -print0 | xargs -0 rm` was
// handed names of files that do not exist.  The child prints each argument
// it received between brackets, so the expectation is the exact bytes.

/// Run xargs with `xargs_args` (each a byte string) on `stdin`, expecting
/// `expected` on stdout and success.
fn xargs_bytes(xargs_args: &[&[u8]], stdin: &[u8], expected: &[u8]) {
    plib::testing::run_test_os(plib::testing::TestPlanOs {
        cmd: String::from("xargs"),
        args: xargs_args
            .iter()
            .map(|a| plib::testing::os_bytes(a))
            .collect(),
        stdin_data: stdin.to_vec(),
        expected_out: expected.to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn non_utf8_bytes_pass_through_null_separated_input() {
    xargs_bytes(
        &[b"-0", b"printf", b"[%s]\n"],
        b"a\xffb\0c\xe9d\0",
        b"[a\xffb]\n[c\xe9d]\n",
    );
}

#[test]
fn non_utf8_bytes_pass_through_blank_separated_input() {
    xargs_bytes(
        &[b"printf", b"[%s]\n"],
        b"a\xffb c\xe9d\n'q\xff t'\n",
        b"[a\xffb]\n[c\xe9d]\n[q\xff t]\n",
    );
}

#[test]
fn non_utf8_bytes_substituted_by_insert_mode() {
    xargs_bytes(
        &[b"-I", b"{}", b"printf", b"[%s]\n", b"x{}y"],
        b"a\xffb\nc\xe9d\n",
        b"[xa\xffby]\n[xc\xe9dy]\n",
    );
}

#[test]
fn non_utf8_replstr_and_utility_argument() {
    xargs_bytes(
        &[b"-I", b"\xfe", b"printf", b"[%s]\n", b"x\xfey"],
        b"a\xffb\n",
        b"[xa\xffby]\n",
    );
}

#[test]
fn non_utf8_bytes_one_argument_per_invocation() {
    xargs_bytes(
        &[
            b"-n",
            b"1",
            b"sh",
            b"-c",
            b"printf '%s:[%s]\\n' $# \"$1\"",
            b"sh",
        ],
        b"a\xffb c\xe9d\n",
        b"1:[a\xffb]\n1:[c\xe9d]\n",
    );
}

#[test]
fn non_utf8_bytes_in_line_mode() {
    xargs_bytes(
        &[b"-L", b"1", b"printf", b"<%s>"],
        b"a\xffb c\n\xe9d\n",
        b"<a\xffb><c><\xe9d>",
    );
}

#[test]
fn non_utf8_eof_string_compared_as_bytes() {
    // "\xff" and "\xfe" were both U+FFFD, so each matched the other.
    xargs_bytes(
        &[b"-E", b"\xff", b"printf", b"[%s]\n"],
        b"a \xfe \xff b\n",
        b"[a]\n[\xfe]\n",
    );
}

#[test]
fn size_limit_counts_bytes() {
    // "\xff\xff\xff" is three bytes: with "echo" (5 bytes with its NUL) and
    // -s 13, two such arguments (4 + 4) fit in one command but three do not.
    xargs_bytes(
        &[b"-s", b"13", b"echo"],
        b"\xff\xff\xff \xff\xff\xff \xff\xff\xff\n",
        b"\xff\xff\xff \xff\xff\xff\n\xff\xff\xff\n",
    );
}

// Skipped where the filesystem refuses a name that is not valid UTF-8
// (macOS APFS).
#[test]
fn rm_removes_exactly_the_named_non_utf8_files() {
    use std::ffi::OsStr;
    use std::os::unix::ffi::OsStrExt;

    let dir = plib::tmp::tempdir().unwrap();
    let names: [&[u8]; 2] = [b"a\xffb", b"c\xe9d"];
    let mut stdin = Vec::new();
    for name in names {
        let created =
            plib::testing::create_non_utf8(dir.path(), name, |p| File::create(p).map(drop));
        let Some(path) = created else {
            return;
        };
        stdin.extend_from_slice(path.as_os_str().as_bytes());
        stdin.push(0);
    }
    // A decoy that the lossy name "a\u{FFFD}b" would have named.
    let decoy = dir.path().join("a\u{FFFD}b");
    File::create(&decoy).unwrap();

    xargs_bytes(&[b"-0", b"rm"], &stdin, b"");

    for name in names {
        assert!(!dir.path().join(OsStr::from_bytes(name)).exists());
    }
    assert!(decoy.exists(), "rm removed the U+FFFD decoy");
}

// An argument that cannot fit within -s even alone is an error, with or
// without -x, as in GNU xargs.  Without -x it used to run the utility with no
// arguments over and over, forever.
#[test]
fn argument_too_long_for_size_limit_is_an_error() {
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec![String::from("-s"), String::from("8"), String::from("echo")],
        stdin_data: String::from("aaaaaaaaaa bb\n"),
        expected_out: String::new(),
        expected_err: String::from("xargs: argument line too long\n"),
        expected_exit_code: 1,
    });
}

// The last argument, read at end of input, can overflow the batch built so
// far; it then goes in a command of its own.  Only one command was run for
// whatever remained at end of input, and the overflow was dropped.
#[test]
fn arguments_left_at_end_of_input_all_run() {
    xargs_test("aaa bbb ccc", "aaa bbb\nccc\n", vec!["-s", "13", "echo"]);
}

// By default a command line must leave room for the environment and stay well
// under {ARG_MAX}: xargs packed 200000 short arguments into one command and
// exec failed with E2BIG, because the default size was {ARG_MAX}-2048 with
// neither the environment nor the argument pointers counted.
#[test]
fn many_arguments_fit_the_default_size() {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let count = 200_000;
    let input: String = (1..=count).map(|n| format!("{n}\n")).collect();
    // Written in one go: the TestPlan runner feeds stdin in small paced chunks.
    let mut child = Command::new(plib::testing::get_binary_path("xargs"))
        .arg("echo")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdin = child.stdin.take().unwrap();
    let writer = std::thread::spawn(move || stdin.write_all(input.as_bytes()));
    let output = child.wait_with_output().unwrap();
    // A write error (xargs gone early) shows up in the assertions below.
    let _ = writer.join().unwrap();

    assert_eq!(output.status.code(), Some(0), "{:?}", output);
    let stdout = String::from_utf8(output.stdout).unwrap();
    let words: Vec<&str> = stdout.split_whitespace().collect();
    assert_eq!(words.len(), count);
    assert_eq!(words.last(), Some(&"200000"));
}

/// Runs xargs with `args` on `input`, expecting success with `out` and `err`.
fn xargs_test_err(input: &str, args: &[&str], out: &str, err: &str) {
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: String::from(input),
        expected_out: String::from(out),
        expected_err: String::from(err),
        expected_exit_code: 0,
    });
}

// -I, -L and -n are mutually exclusive; POSIX lets the last one specified
// take effect, and that one does, with a warning for each one it cancels.

/// `-n 1` after -I leaves -I in effect, silently: util-linux's tests/run.sh
/// runs `xargs -I '{}' -P N -n 1 ...`.
#[test]
fn xargs_insert_mode_with_n1_stays_insert_mode() {
    xargs_test_err(
        "a b\nc\n",
        &["-I", "{}", "-n", "1", "echo", "X{}Y"],
        "Xa bY\nXcY\n",
        "",
    );
}

#[test]
fn xargs_n_after_insert_mode_takes_effect() {
    xargs_test_err(
        "a\nb\nc\n",
        &["-I", "{}", "-n", "2", "echo", "X{}Y"],
        "X{}Y a b\nX{}Y c\n",
        "xargs: warning: options -I and -n are mutually exclusive; ignoring -I\n",
    );
}

#[test]
fn xargs_insert_mode_after_n_takes_effect() {
    xargs_test_err(
        "a\nb\n",
        &["-n", "1", "-I", "{}", "echo", "X{}Y"],
        "XaY\nXbY\n",
        "xargs: warning: options -n and -I are mutually exclusive; ignoring -n\n",
    );
}

#[test]
fn xargs_insert_mode_after_lines_takes_effect() {
    xargs_test_err(
        "a\nb\n",
        &["-L", "2", "-I", "{}", "echo", "X{}"],
        "Xa\nXb\n",
        "xargs: warning: options -L and -I are mutually exclusive; ignoring -L\n",
    );
}

#[test]
fn xargs_lines_after_n_takes_effect() {
    xargs_test_err(
        "a b\nc\n",
        &["-n", "1", "-L", "2", "echo"],
        "a b c\n",
        "xargs: warning: options -n and -L are mutually exclusive; ignoring -n\n",
    );
}
