//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use rand::{rng, seq::SliceRandom};

/* #region Normal tests */
fn head_test(n: Option<&str>, c: Option<&str>, test_data: &str, expected_output: &str) {
    fn generate_valid_arguments(n: Option<&str>, c: Option<&str>) -> Vec<Vec<String>> {
        let mut argument_forms = Vec::<Vec<(String, String)>>::new();

        let mut args_outer = Vec::<(String, String)>::new();

        for n_form in ["-n", "--lines"] {
            args_outer.clear();

            if let Some(n_str) = n {
                args_outer.push((n_form.to_owned(), n_str.to_owned()));
            };

            for c_form in ["-c", "--bytes"] {
                let mut args_inner = args_outer.clone();

                if let Some(c_str) = c {
                    args_inner.push((c_form.to_owned(), c_str.to_owned()));
                };

                argument_forms.push(args_inner);
            }
        }

        argument_forms.shuffle(&mut rng());

        let mut flattened = Vec::<Vec<String>>::with_capacity(argument_forms.len());

        for ve in argument_forms {
            let mut vec = Vec::<String>::new();

            for (st, str) in ve {
                vec.push(st);
                vec.push(str);
            }

            flattened.push(vec);
        }

        flattened
    }

    for ve in generate_valid_arguments(n, c) {
        run_test(TestPlan {
            cmd: "head".to_owned(),
            args: ve,
            stdin_data: test_data.to_owned(),
            expected_out: expected_output.to_owned(),
            expected_err: String::new(),
            expected_exit_code: 0_i32,
        });
    }
}

#[test]
fn test_head_basic() {
    head_test(None, None, "a\nb\nc\nd\n", "a\nb\nc\nd\n");

    head_test(
        None,
        None,
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n",
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n",
    );

    head_test(
        None,
        None,
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\na\n",
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n",
    );
}

#[test]
fn test_head_explicit_n() {
    head_test(
        Some("5"),
        None,
        "\
1
2
3
4
5
6
7
8
9
0
a
",
        "\
1
2
3
4
5
",
    );
}

#[test]
fn test_head_c() {
    head_test(None, Some("3"), "123456789", "123");
}
/* #endregion */

/* #region Property-based tests */
mod property_tests {
    use plib::testing::run_test_base;
    use proptest::{prelude::TestCaseError, prop_assert, test_runner::TestRunner};
    use std::{
        sync::mpsc::{self, RecvTimeoutError},
        thread::{self},
        time::Duration,
    };

    fn get_test_runner(cases: u32) -> TestRunner {
        TestRunner::new(proptest::test_runner::Config {
            cases,
            failure_persistence: None,

            ..proptest::test_runner::Config::default()
        })
    }

    fn run_head_and_verify_output(
        input: &[u8],
        true_if_lines_false_if_bytes: bool,
        count: usize,
    ) -> Result<(), TestCaseError> {
        let n_or_c = if true_if_lines_false_if_bytes {
            "-n"
        } else {
            "-c"
        };

        let output = run_test_base("head", &[n_or_c.to_owned(), count.to_string()], input);

        let stdout = &output.stdout;

        if true_if_lines_false_if_bytes {
            let new_lines_in_stdout = stdout.iter().filter(|&&ue| ue == b'\n').count();

            prop_assert!(new_lines_in_stdout <= count);

            prop_assert!(input.starts_with(stdout));
        } else {
            prop_assert!(stdout.len() <= count);

            prop_assert!(stdout.as_slice() == &input[..(input.len().min(count))]);
        }

        Ok(())
    }

    fn run_head_and_verify_output_with_timeout(
        true_if_lines_false_if_bytes: bool,
        count: usize,
        input: Vec<u8>,
    ) -> Result<(), TestCaseError> {
        let (sender, receiver) = mpsc::channel::<Result<(), TestCaseError>>();

        let input_len = input.len();

        thread::spawn(move || {
            sender.send(run_head_and_verify_output(
                input.as_slice(),
                true_if_lines_false_if_bytes,
                count,
            ))
        });

        match receiver.recv_timeout(Duration::from_secs(60_u64)) {
            Ok(result) => result,
            Err(RecvTimeoutError::Timeout) => {
                eprint!(
                        "\
head property test has been running for more than a minute. The spawned process will have to be killed manually.

true_if_lines_false_if_bytes: {true_if_lines_false_if_bytes}
count: {count}
input_len: {input_len}
"
                    );

                Err(TestCaseError::fail("Spawned process did not terminate"))
            }
            Err(RecvTimeoutError::Disconnected) => {
                unreachable!();
            }
        }
    }

    #[test]
    fn test_head_property_test_small_or_large() {
        get_test_runner(16_u32)
            .run(
                &(
                    proptest::bool::ANY,
                    (0_usize..16_384_usize),
                    proptest::collection::vec(proptest::num::u8::ANY, 0_usize..65_536_usize),
                ),
                |(true_if_lines_false_if_bytes, count, input)| {
                    run_head_and_verify_output_with_timeout(
                        true_if_lines_false_if_bytes,
                        count,
                        input,
                    )
                },
            )
            .unwrap();
    }

    #[test]
    fn test_head_property_test_small() {
        get_test_runner(128_u32)
            .run(
                &(
                    proptest::bool::ANY,
                    (0_usize..1_024_usize),
                    proptest::collection::vec(proptest::num::u8::ANY, 0_usize..16_384_usize),
                ),
                |(true_if_lines_false_if_bytes, count, input)| {
                    run_head_and_verify_output_with_timeout(
                        true_if_lines_false_if_bytes,
                        count,
                        input,
                    )
                },
            )
            .unwrap();
    }

    #[test]
    fn test_head_property_test_very_small() {
        get_test_runner(128_u32)
            .run(
                &(
                    proptest::bool::ANY,
                    (0_usize..512_usize),
                    proptest::collection::vec(proptest::num::u8::ANY, 0_usize..512_usize),
                ),
                |(true_if_lines_false_if_bytes, count, input)| {
                    run_head_and_verify_output_with_timeout(
                        true_if_lines_false_if_bytes,
                        count,
                        input,
                    )
                },
            )
            .unwrap();
    }
}
/* #endregion */

#[test]
fn head_dash_operand_reads_stdin() {
    // A "-" operand reads standard input rather than a file named "-".
    run_test(TestPlan {
        cmd: String::from("head"),
        args: vec![String::from("-")],
        stdin_data: String::from("a\nb\nc\n"),
        expected_out: String::from("a\nb\nc\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// ---------------------------------------------------------------------------
// Operands, headers and error paths
// ---------------------------------------------------------------------------

fn head_run(args: &[&str]) -> (String, String, i32) {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_head"))
        .args(args)
        .output()
        .expect("failed to run head");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

fn head_tmp(name: &str, content: &str) -> plib::testing::TempFile {
    plib::testing::TempFile::new(name, content)
}

#[test]
fn head_multiple_files_get_name_headers() {
    // POSIX STDOUT: with more than one file operand, each is preceded by
    // "==> <pathname> <==" and separated by a blank line.
    let a = head_tmp("h1", "a1\na2\n");
    let b = head_tmp("h2", "b1\nb2\n");
    let (stdout, _, code) = head_run(&["-n", "1", a.to_str().unwrap(), b.to_str().unwrap()]);
    assert_eq!(code, 0);
    assert_eq!(
        stdout,
        format!(
            "==> {} <==\na1\n\n==> {} <==\nb1\n",
            a.display(),
            b.display()
        ),
        "got {stdout:?}"
    );
}

#[test]
fn head_single_file_has_no_header() {
    let f = head_tmp("solo", "x\ny\n");
    let (stdout, _, code) = head_run(&["-n", "1", f.to_str().unwrap()]);
    assert_eq!(code, 0);
    assert_eq!(
        stdout, "x\n",
        "a lone operand gets no header, got {stdout:?}"
    );
}

#[test]
fn head_zero_count_writes_nothing_and_succeeds() {
    let f = head_tmp("zero", "a\nb\n");
    let p = f.to_str().unwrap();
    let (stdout, _, code) = head_run(&["-n", "0", p]);
    assert_eq!(stdout, "");
    assert_eq!(code, 0, "-n 0 is not an error");
    let (stdout, _, code) = head_run(&["-c", "0", p]);
    assert_eq!(stdout, "");
    assert_eq!(code, 0, "-c 0 is not an error");
}

#[test]
fn head_nonexistent_file_errors() {
    let (_, stderr, code) = head_run(&["-n", "1", "no-such-file-xyz"]);
    assert_ne!(code, 0);
    assert!(stderr.contains("no-such-file-xyz"), "got {stderr:?}");
}

#[test]
fn head_mixes_dash_with_named_files() {
    let f = head_tmp("mix", "fromfile\n");
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_head"))
        .args(["-n", "1", "-", f.to_str().unwrap()])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("spawn head");
    use std::io::Write;
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b"fromstdin\n")
        .unwrap();
    let out = child.wait_with_output().expect("wait head");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("fromstdin"), "got {stdout:?}");
    assert!(stdout.contains("fromfile"), "got {stdout:?}");
    assert!(
        stdout.contains("==>"),
        "multiple operands get headers: {stdout:?}"
    );
}

// ---------------------------------------------------------------------------
// Historical `-number` form (withdrawn from POSIX in Issue 6)
// ---------------------------------------------------------------------------

const TWENTY_LINES: &str =
    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n";

#[test]
fn head_historical_number_reads_stdin() {
    run_test(TestPlan {
        cmd: String::from("head"),
        args: vec![String::from("-1")],
        stdin_data: String::from("a\nb\nc\n"),
        expected_out: String::from("a\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn head_historical_number_with_file() {
    let f = head_tmp("hist5", TWENTY_LINES);
    let p = f.to_str().unwrap();
    let (stdout, stderr, code) = head_run(&["-5", p]);
    assert_eq!(
        (stdout.as_str(), stderr.as_str(), code),
        ("1\n2\n3\n4\n5\n", "", 0)
    );
    // An option may follow an operand, as `-n` may.
    let (stdout, _, code) = head_run(&[p, "-2"]);
    assert_eq!((stdout.as_str(), code), ("1\n2\n", 0));
}

#[test]
fn head_historical_number_last_count_wins() {
    let f = head_tmp("histlast", TWENTY_LINES);
    let p = f.to_str().unwrap();
    let (stdout, _, code) = head_run(&["-n", "3", "-5", p]);
    assert_eq!((stdout.as_str(), code), ("1\n2\n3\n4\n5\n", 0));
    let (stdout, _, code) = head_run(&["-5", "-n", "3", p]);
    assert_eq!((stdout.as_str(), code), ("1\n2\n3\n", 0));
    let (stdout, _, code) = head_run(&["-4", "-2", p]);
    assert_eq!((stdout.as_str(), code), ("1\n2\n", 0));
    let (stdout, _, code) = head_run(&["-n", "4", "-n", "1", p]);
    assert_eq!((stdout.as_str(), code), ("1\n", 0));
}

#[test]
fn head_historical_zero_writes_nothing() {
    let f = head_tmp("hist0", TWENTY_LINES);
    let (stdout, stderr, code) = head_run(&["-0", f.to_str().unwrap()]);
    assert_eq!((stdout.as_str(), stderr.as_str(), code), ("", "", 0));
}

#[test]
fn head_historical_number_is_not_an_operand_after_double_dash() {
    // `--` ends the options: a following "-1" is a file named "-1", and the
    // option-argument of a separate `-n` is never rewritten.
    let dir = plib::tmp::tempdir().unwrap();
    std::fs::write(dir.path().join("-1"), "x\ny\nz\n").unwrap();
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_head"))
        .args(["-2", "--", "-1"])
        .current_dir(dir.path())
        .output()
        .expect("run head");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "x\ny\n");
    assert_eq!(out.status.code(), Some(0));
}

#[test]
fn head_historical_number_invalid_is_an_error() {
    for bad in ["-5x", "-99999999999999999999999"] {
        let (stdout, stderr, code) = head_run(&[bad]);
        assert_eq!(stdout, "", "{bad}");
        assert!(!stderr.is_empty(), "{bad}: no diagnostic");
        assert_ne!(code, 0, "{bad}: must fail");
    }
    // `-c` is unaffected: it still needs its own option-argument.
    let (_, _, code) = head_run(&["-c", "-5"]);
    assert_ne!(code, 0, "-c -5 must not become -c with a -n 5");
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn option_argument_may_begin_with_hyphen() {
    for opt in ["-n", "-c"] {
        plib::testing::assert_hyphen_option_argument("head", &[opt, "-zq", "--help"]);
    }
}

// A write error on output that does not end in a <newline> is reported: that
// output sat in the line buffer until exit, where the error was lost.
#[test]
fn test_head_reports_write_error_on_final_partial_line() {
    plib::testing::assert_write_error_on_full_device("head", &[], b"x", 1);
    plib::testing::assert_write_error_on_full_device("head", &["-c", "1"], b"xy\n", 1);
}
