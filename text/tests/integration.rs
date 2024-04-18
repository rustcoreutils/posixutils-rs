//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{DateTime, Local};
use plib::{run_test, run_test_with_checker, TestPlan};
use regex::Regex;
use std::fs;
use std::io::Read;
const PR_DATE_TIME_FORMAT: &str = "%b %d %H:%M %Y";

fn expand_test_noargs(test_data: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("expand"),
        args: Vec::new(),
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn head_test(test_data: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("head"),
        args: Vec::new(),
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn wc_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("wc"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn nl_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("nl"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn pr_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("pr"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn pr_read_test_file(
    output_filename: &str,
    input_filename: &str,
    header: Option<&str>,
    date: Option<String>,
) -> String {
    let re = Regex::new(r"<DATE>|<FILENAME>").unwrap();

    let dt_string = date.unwrap_or_else(|| {
        let metadata = fs::metadata(input_filename).unwrap();
        let last_modified_time = metadata.modified().unwrap();
        let dt: DateTime<Local> = last_modified_time.into();
        dt.format(PR_DATE_TIME_FORMAT).to_string()
    });

    let mut file = fs::File::open(output_filename).unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();

    let s = re.replace_all(&buf, |captures: &regex::Captures<'_>| -> String {
        let marker = captures.get(0).unwrap();
        match marker.as_str() {
            "<DATE>" => dt_string.clone(),
            "<FILENAME>" => header.unwrap_or(input_filename).to_string(),
            _ => panic!("Unknown pattern"),
        }
    });

    s.to_string()
}

#[test]
fn test_expand_basic() {
    expand_test_noargs("", "");
    expand_test_noargs("a\tb\tc\n", "a       b       c\n");
}

#[test]
fn test_head_basic() {
    head_test("a\nb\nc\nd\n", "a\nb\nc\nd\n");
    head_test(
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n",
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n",
    );
    head_test(
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\na\n",
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n",
    );
}

#[test]
fn test_wc_empty() {
    wc_test(&["-c"], "", "0\n");
    wc_test(&["-l"], "", "0\n");
    wc_test(&["-w"], "", "0\n");
}

#[test]
fn test_wc_one() {
    wc_test(&["-c"], "x", "1\n");
    wc_test(&["-l"], "x", "0\n");
    wc_test(&["-w"], "x", "1\n");
}

#[test]
fn test_wc_two() {
    wc_test(&["-c"], "x y\n", "4\n");
    wc_test(&["-l"], "x y\n", "1\n");
    wc_test(&["-w"], "x y\n", "2\n");
}

#[test]
fn test_nl_justification() {
    nl_test(&["-n", "ln"], "a", "1     \ta\n");
    nl_test(&["-n", "rn"], "b", "     1\tb\n");
    nl_test(&["-n", "rz"], "c", "000001\tc\n");
}

#[test]
fn test_nl_newlines_at_end() {
    nl_test(&[], "a\n\n", "     1\ta\n       \n");
}

#[test]
fn test_nl_starting_number() {
    nl_test(&["-v", "2"], "a", "     2\ta\n");
}

#[test]
fn test_nl_number_increment() {
    let input = "\\:\\:\\:\nheader\n\\:\\:\nbody\n\\:\nfooter";
    // Without -p, the counter resets on delimiters
    nl_test(
        &["-h", "a", "-f", "a"],
        input,
        "\n     1\theader\n\n     1\tbody\n\n     1\tfooter\n",
    );

    // With -p, the counter increments even when encountering delimiters
    nl_test(
        &["-h", "a", "-f", "a", "-p"],
        input,
        "\n     1\theader\n\n     2\tbody\n\n     3\tfooter\n",
    );

    nl_test(
        &["-h", "a", "-f", "a", "-p", "-i", "2"],
        input,
        "\n     1\theader\n\n     3\tbody\n\n     5\tfooter\n",
    );
}

#[test]
fn test_nl_delimiter() {
    // Single character delimiter should be appended with the default second
    // character, ':'
    nl_test(
        &["-h", "a", "-f", "a", "-d", "?"],
        "?:?:?:\nheader\n?:?:\nbody\n?:\nfooter",
        "\n     1\theader\n\n     1\tbody\n\n     1\tfooter\n",
    );

    nl_test(
        &["-h", "a", "-f", "a", "-d", "?!"],
        "?!?!?!\nheader\n?!?!\nbody\n?!\nfooter",
        "\n     1\theader\n\n     1\tbody\n\n     1\tfooter\n",
    );
}

#[test]
fn test_nl_regex() {
    // NOTE: The implementation has better regex support than the reference.
    // `nl -b p.+ng` would fail to match the words ending with "ng" in the
    // original whereas it would in this Rust implementation. Might be
    // considered a bug?
    nl_test(
        &["-b", "p.*ng"],
        "something\nanything\neverything\ncat\ndog",
        "     1\tsomething\n     2\tanything\n     3\teverything\n       cat\n       dog\n",
    );
}

#[test]
fn test_pr_single_column() {
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file(
        "tests/pr/lorem_ipsum_output_single_column.txt",
        input,
        None,
        None,
    );
    pr_test(&[&input], "", &output);
}

#[test]
fn test_pr_multi_column() {
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file("tests/pr/lorem_ipsum_output_9_cols.txt", input, None, None);
    pr_test(&["-9", &input], "", &output);
}

#[test]
fn test_pr_multi_column_across() {
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file(
        "tests/pr/lorem_ipsum_output_2_cols_across.txt",
        input,
        None,
        None,
    );
    pr_test(&["-2", "-a", &input], "", &output);
}

#[test]
fn test_pr_multi_column_merge() {
    // This test requires the current timestamp.
    //
    // It's possible to inject the current timestamp to the expected output
    // before calling `pr_test` but that would cause spurious errors when the
    // minute portion changes in between now and when the process is actually
    // ran:
    //
    // Apr 18 14:12 2024
    // Apr 18 14:13 2024

    let input = "tests/pr/lorem_ipsum.txt";
    let args = &["+1:1", "-m", &input, &input, &input];
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    let test_plan = TestPlan {
        cmd: String::from("pr"),
        args: str_args,
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: 0,
    };

    run_test_with_checker(test_plan, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // MMM++++++++++YYYY
        let re = Regex::new(r"\w{3}.+\d{4}").unwrap();
        let captures = re.captures(&stdout).unwrap();
        let date = captures.get(0).unwrap().as_str();

        let expected_out = pr_read_test_file(
            "tests/pr/lorem_ipsum_output_merge.txt",
            input,
            None,
            Some(date.to_string()),
        );

        assert_eq!(stdout, expected_out);
    });
}

#[test]
fn test_pr_page_skip() {
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file(
        "tests/pr/numbers_output_9_cols_page15.txt",
        input,
        None,
        None,
    );
    pr_test(&["-9", "+15", &input], "", &output);
}

#[test]
fn test_pr_header_replacement() {
    let header = "custom";
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file(
        "tests/pr/lorem_ipsum_output_page_1.txt",
        input,
        Some(header),
        None,
    );
    pr_test(&["+1:1", "-h", header, &input], "", &output);
}

#[test]
fn test_pr_limit_lines() {
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file("tests/pr/numbers_output_l20.txt", input, None, None);
    pr_test(&["+1:1", "-l20", &input], "", &output);
}

#[test]
fn test_pr_limit_lines_trim() {
    // Lines <= 10 behave like -t is used
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file("tests/pr/numbers_output_l10.txt", input, None, None);
    pr_test(&["+1:1", "-l10", &input], "", &output);
}

#[test]
fn test_pr_omit_header() {
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file("tests/pr/numbers_output_omit_header.txt", input, None, None);
    pr_test(&["+1:1", "-l20", "-t", &input], "", &output);
}

#[test]
fn test_pr_offset() {
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file("tests/pr/numbers_output_offset.txt", input, None, None);
    pr_test(&["+1:1", "-o7", &input], "", &output);
}

#[test]
fn test_pr_width() {
    let input = "tests/pr/long_line.txt";
    let output = pr_read_test_file("tests/pr/long_line_output_w72.txt", input, None, None);
    pr_test(&["-2", "-t", "-w72", &input], "", &output);

    let output = pr_read_test_file("tests/pr/long_line_output_w200.txt", input, None, None);
    pr_test(&["-2", "-t", "-w200", &input], "", &output);

    // -s without -w causes the width to be 512
    let output = pr_read_test_file("tests/pr/long_line_output_s.txt", input, None, None);
    pr_test(&["-2", "-t", "-s", &input], "", &output);
}

#[test]
fn test_pr_number_line() {
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file(
        "tests/pr/lorem_ipsum_output_number_line.txt",
        input,
        None,
        None,
    );
    pr_test(&["-9", "-n3", &input], "", &output);
}

#[test]
fn test_pr_expand_and_replace() {
    let input = "tests/pr/spaces_and_tabs.txt";
    let output = pr_read_test_file(
        "tests/pr/spaces_and_tabs_expand_and_replace.txt",
        input,
        None,
        None,
    );
    pr_test(&["-i?3", "-e", "-t", &input], "", &output);
}
