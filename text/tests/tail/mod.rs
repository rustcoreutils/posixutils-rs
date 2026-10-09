//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_u8, TestPlan, TestPlanU8};

fn tail_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args = args.iter().map(|st| (*st).to_owned()).collect::<Vec<_>>();

    run_test(TestPlan {
        cmd: "tail".to_owned(),
        args: str_args,
        stdin_data: test_data.to_owned(),
        expected_out: expected_output.to_owned(),
        expected_err: String::new(),
        expected_exit_code: 0_i32,
    });
}

fn tail_test_failure(args: &[&str], expected_stderr: &str) {
    let str_args = args.iter().map(|st| (*st).to_owned()).collect::<Vec<_>>();

    run_test(TestPlan {
        cmd: "tail".to_owned(),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: expected_stderr.to_owned(),
        expected_exit_code: 1_i32,
    });
}

fn tail_test_binary(args: &[&str], test_data: &[u8], expected_output: &[u8]) {
    let str_args = args.iter().map(|st| (*st).to_owned()).collect::<Vec<_>>();

    run_test_u8(TestPlanU8 {
        cmd: "tail".to_owned(),
        args: str_args,
        stdin_data: test_data.to_vec(),
        expected_out: expected_output.to_vec(),
        expected_err: Vec::<u8>::new(),
        expected_exit_code: 0_i32,
    });
}

#[test]
fn test_tail() {
    tail_test(&["-n2"], "a\nb\nc\n", "b\nc\n");
}

#[test]
fn test_tail_1() {
    tail_test(&["-c+2"], "abcd", "bcd");
}

#[test]
fn test_tail_2() {
    tail_test(&["-c+8"], "abcd", "");
}

#[test]
fn test_tail_3() {
    tail_test(&["-c-1"], "abcd", "d");
}

#[test]
fn test_tail_4() {
    tail_test(&["-c-9"], "abcd", "abcd");
}

#[test]
fn test_tail_5() {
    tail_test(
        &["-c-12"],
        &("x".to_string() + &"y".repeat(12) + "z"),
        &("y".repeat(11) + "z"),
    );
}

#[test]
fn test_tail_6() {
    tail_test(&["-n-1"], "x\n", "x\n");
}

#[test]
fn test_tail_7() {
    tail_test(&["-n-1"], "x\ny\n", "y\n");
}

#[test]
fn test_tail_8() {
    tail_test(&["-n-1"], "x\ny\n", "y\n");
}

#[test]
fn test_tail_9() {
    tail_test(&["-n+1"], "x\ny\n", "x\ny\n");
}

#[test]
fn test_tail_10() {
    tail_test(&["-n+2"], "x\ny\n", "y\n");
}

#[test]
fn test_tail_11() {
    tail_test(
        &["-c+10"],
        &("x".to_string() + &"y".repeat(10) + "z\n"),
        "yyz\n",
    );
}

#[test]
fn test_tail_12() {
    tail_test(
        &["-n+10"],
        &("x\n".to_string() + &"y\n".repeat(10) + "z\n"),
        "y\ny\nz\n",
    );
}

#[test]
fn test_tail_13() {
    tail_test(
        &["-n-10"],
        &("x\n".to_string() + &"y\n".repeat(10) + "z\n"),
        &("y\n".repeat(9) + "z\n"),
    );
}

#[test]
fn test_tail_14() {
    let input = &("x\n".repeat(512 * 10 / 2 + 1));
    let expected_output = &("x\n".repeat(10));
    tail_test(&["-n-10"], input, expected_output);
}

#[test]
fn test_tail_15() {
    tail_test(&["-c2"], "abcd\n", "d\n");
}

#[test]
fn test_tail_16() {
    tail_test(
        &["-n-10"],
        &("x\n".to_string() + &"y\n".repeat(10) + "z\n"),
        &("y\n".repeat(9) + "z\n"),
    );
}

#[test]
fn test_tail_17() {
    tail_test(
        &["-n+10"],
        &("x\n".to_string() + &"y\n".repeat(10) + "z\n"),
        "y\ny\nz\n",
    );
}

#[test]
fn test_tail_18() {
    tail_test(&["-n+0"], &("y\n".repeat(5)), &("y\n".repeat(5)));
}

#[test]
fn test_tail_19() {
    tail_test(&["-n+1"], &("y\n".repeat(5)), &("y\n".repeat(5)));
}

#[test]
fn test_tail_20() {
    tail_test(&["-n-1"], &("y\n".repeat(5)), "y\n");
}

#[test]
fn test_tail_input_containing_non_utf_8() {
    const INPUT: &[u8] = b"\
\xFF
\xFF
0
1
2
3
4
5
6
7
8
9
";

    const EXPECTED_OUTPUT: &[u8] = b"\
0
1
2
3
4
5
6
7
8
9
";

    tail_test_binary(&[], INPUT, EXPECTED_OUTPUT);
}

#[test]
fn test_tail_zero_bytes_and_zero_lines() {
    const INPUT: &str = "\
0
1
2
3
4
5
6
7
8
9
";

    tail_test(&["-c", "0"], INPUT, "");
    tail_test(&["-c", "-0"], INPUT, "");

    tail_test(&["-n", "0"], INPUT, "");
    tail_test(&["-n", "-0"], INPUT, "");
}

// Finding #1: -r (reverse). BSD `tail -r` reverses all lines; `tail -r -n N`
// reverses only the last N lines. (GNU dropped -r, so BSD is the reference.)
#[test]
fn test_tail_reverse_all() {
    tail_test(&["-r"], "a\nb\nc\n", "c\nb\na\n");
}

#[test]
fn test_tail_reverse_last_n() {
    tail_test(&["-r", "-n", "2"], "a\nb\nc\n", "c\nb\n");
}

#[test]
fn test_tail_reverse_from_start() {
    // +2: reverse lines from the 2nd to the end.
    tail_test(&["-r", "-n", "+2"], "a\nb\nc\n", "c\nb\n");
}

#[test]
fn test_tail_reverse_no_trailing_newline() {
    // A missing final newline is normalized; each reversed line is terminated.
    tail_test(&["-r"], "a\nb\nc", "c\nb\na\n");
}

#[test]
fn test_tail_reverse_bytes() {
    // -r with -c reverses the selected bytes.
    tail_test(&["-r", "-c", "3"], "abcd", "dcb");
}

// Finding #6: `--` must still terminate option parsing despite
// allow_hyphen_values on -n/-c.
#[test]
fn test_tail_double_dash_with_count() {
    tail_test(&["-n", "2", "--"], "a\nb\nc\n", "b\nc\n");
}

#[test]
fn test_tail_double_dash_default() {
    tail_test(&["--"], "a\nb\nc\n", "a\nb\nc\n");
}

// Finding #3: `-n +0` matches GNU, which prints the WHOLE file (treats +0
// like +1). POSIX deems line/byte zero from start non-conforming, but GNU's
// choice is to emit everything; accepted as GNU-correct.
#[test]
fn test_tail_plus_zero_prints_all() {
    tail_test(&["-n", "+0"], "a\nb\nc\n", "a\nb\nc\n");
}

#[test]
fn test_tail_c_and_n() {
    tail_test_failure(
        &["-c", "1", "-n", "2"],
        "tail: options '-c' and '-n' cannot be used together\n",
    );
    tail_test_failure(
        &["-n", "3", "-c", "4"],
        "tail: options '-c' and '-n' cannot be used together\n",
    );
}

// ---------------------------------------------------------------------------
// Byte offsets from the start, and error paths
// ---------------------------------------------------------------------------

#[test]
fn test_tail_c_plus_one_is_the_whole_file() {
    // POSIX: `-c +number` counts from the beginning of the file, and +1 is the
    // first byte, so the entire file is copied.
    run_test(TestPlan {
        cmd: String::from("tail"),
        args: vec![String::from("-c"), String::from("+1")],
        stdin_data: String::from("abcdef"),
        expected_out: String::from("abcdef"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_tail_nonexistent_file_errors() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_tail"))
        .arg("no-such-file-xyz")
        .output()
        .expect("run tail");
    assert_ne!(out.status.code(), Some(0), "a missing operand must exit >0");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.starts_with("tail:"),
        "the diagnostic must name the utility, got {stderr:?}"
    );
}

/// `tail` must name the file it could not open.
///
/// `args.file` was moved into `tail(...)` before the diagnostic ran, so main
/// had nothing to name; the operand is in scope at the `File::open` origin.
#[test]
fn test_tail_error_names_the_file() {
    let out = std::process::Command::new(plib::testing::get_binary_path("tail"))
        .arg("/nonexistent_tail_probe_zz")
        .output()
        .expect("spawn tail");
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();

    assert!(
        stderr.starts_with("tail: "),
        "every diagnostic must name the utility: {stderr:?}"
    );
    assert!(
        stderr.contains("/nonexistent_tail_probe_zz"),
        "the diagnostic must name the file it could not open: {stderr:?}"
    );
    assert!(
        !stderr.contains("(os error"),
        "Rust's errno parenthetical must not reach the user: {stderr:?}"
    );
    assert_ne!(out.status.code(), Some(0));
}

// ---------------------------------------------------------------------------
// Historical `-number` / `+number` forms (withdrawn from POSIX in Issue 6)
// ---------------------------------------------------------------------------

fn tail_run(args: &[&str]) -> (String, String, i32) {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_tail"))
        .args(args)
        .output()
        .expect("run tail");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

fn tail_tmp(name: &str, content: &str) -> plib::testing::TempFile {
    plib::testing::TempFile::new(name, content)
}

#[test]
fn test_tail_historical_minus_number_stdin() {
    tail_test(&["-1"], "a\nb\nc\n", "c\n");
    tail_test(&["-0"], "a\nb\nc\n", "");
}

#[test]
fn test_tail_historical_plus_number_stdin() {
    tail_test(&["+2"], "a\nb\nc\n", "b\nc\n");
}

#[test]
fn test_tail_historical_bytes_stdin() {
    tail_test(&["-1c"], "abc\n", "\n");
    tail_test(&["-3c"], "abc\ndef\n", "ef\n");
    tail_test(&["+3c"], "abc\ndef\n", "c\ndef\n");
}

#[test]
fn test_tail_historical_forms_with_file() {
    let f = tail_tmp("hist", "1\n2\n3\n4\n5\n");
    let p = f.to_str().unwrap();
    let (stdout, stderr, code) = tail_run(&["-3", p]);
    assert_eq!(
        (stdout.as_str(), stderr.as_str(), code),
        ("3\n4\n5\n", "", 0)
    );
    let (stdout, _, code) = tail_run(&["+2", p]);
    assert_eq!((stdout.as_str(), code), ("2\n3\n4\n5\n", 0));
    let (stdout, _, code) = tail_run(&["-4c", p]);
    assert_eq!((stdout.as_str(), code), ("4\n5\n", 0));
}

#[test]
fn test_tail_historical_form_is_only_the_first_argument() {
    // As in GNU: once another option or `--` precedes it, the word is an
    // ordinary option or operand again.
    let (_, stderr, code) = tail_run(&["-n", "2", "-3"]);
    assert!(!stderr.is_empty());
    assert_ne!(code, 0);

    let dir = plib::tmp::tempdir().unwrap();
    std::fs::write(dir.path().join("-1"), "x\ny\n").unwrap();
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_tail"))
        .args(["--", "-1"])
        .current_dir(dir.path())
        .output()
        .expect("run tail");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "x\ny\n");
    assert_eq!(out.status.code(), Some(0));
}

#[test]
fn test_tail_historical_number_invalid_is_an_error() {
    for bad in ["-3x", "-99999999999999999999999", "+3cf"] {
        let (stdout, stderr, code) = tail_run(&[bad]);
        assert_eq!(stdout, "", "{bad}");
        assert!(!stderr.is_empty(), "{bad}: no diagnostic");
        assert_ne!(code, 0, "{bad}: must fail");
    }
}

/// Run tail in `dir` with `args` and no input, as GNU names its operand.
fn tail_in(dir: &std::path::Path, args: &[&str]) -> (String, String, Option<i32>) {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_tail"))
        .args(args)
        .current_dir(dir)
        .stdin(std::process::Stdio::null())
        .output()
        .expect("run tail");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

// -v (GNU) writes the `==> NAME <==` header even for a single file.
// debhelper runs `tail -v -n +0 config.log`.
#[test]
fn test_tail_verbose_header() {
    let dir = plib::tmp::tempdir().unwrap();
    std::fs::write(dir.path().join("config.log"), "a\nb\n").unwrap();
    std::fs::write(dir.path().join("partial"), "x\ny").unwrap();

    let ok = |out: &str| (out.to_string(), String::new(), Some(0));
    assert_eq!(
        tail_in(dir.path(), &["-v", "-n", "+0", "config.log"]),
        ok("==> config.log <==\na\nb\n")
    );
    assert_eq!(
        tail_in(dir.path(), &["--verbose", "-n1", "config.log"]),
        ok("==> config.log <==\nb\n")
    );
    assert_eq!(
        tail_in(dir.path(), &["-v", "-c1", "partial"]),
        ok("==> partial <==\ny")
    );
    assert_eq!(
        tail_in(dir.path(), &["-v", "-r", "config.log"]),
        ok("==> config.log <==\nb\na\n")
    );

    // An operand that cannot be opened gets no header.
    let (out, err, code) = tail_in(dir.path(), &["-v", "nosuch"]);
    assert_eq!(out, "");
    assert!(err.starts_with("tail: nosuch: "), "got {err:?}");
    assert_eq!(code, Some(1));
}

#[test]
fn test_tail_verbose_header_names_standard_input() {
    tail_test(&["-v", "-n1"], "a\nb\n", "==> standard input <==\nb\n");
    tail_test(&["-v", "-"], "a\n", "==> standard input <==\na\n");
}

// A write error on output that does not end in a <newline> is reported: that
// output sat in the line buffer until exit, where the error was lost.
#[test]
fn test_tail_reports_write_error_on_final_partial_line() {
    plib::testing::assert_write_error_on_full_device("tail", &[], b"x", 1);
    plib::testing::assert_write_error_on_full_device("tail", &["-c", "1"], b"x\ny", 1);
}
