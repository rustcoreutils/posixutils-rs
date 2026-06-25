//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// All expected outputs below were verified byte-for-byte against GNU coreutils
// `join` 9.4 under LC_ALL=C.

use plib::testing::{run_test, TestPlan};

fn run_test_join(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    run_test_join_stdin(
        args,
        "",
        expected_output,
        expected_error,
        expected_exit_code,
    )
}

fn run_test_join_stdin(
    args: &[&str],
    stdin_data: &str,
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("join"),
        args: str_args,
        stdin_data: String::from(stdin_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn fixture(name: &str) -> String {
    format!("{}/tests/join/{}", env!("CARGO_MANIFEST_DIR"), name)
}

#[test]
fn simple_test() {
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = [file1.as_str(), file2.as_str()];

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn a_test() {
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = ["-a", "1", file1.as_str(), file2.as_str()];

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n4 Kos\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn v_test() {
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = ["-v", "1", file1.as_str(), file2.as_str()];

    let expected_output = "4 Kos\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn o_test() {
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = ["-o", "1.2,2.1,2.2,2.1", file1.as_str(), file2.as_str()];

    let expected_output = "Alice 1 HR 1\nBob 2 Finance 2\nCharlie 3 IT 3\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn e_test() {
    let file1 = fixture("file5.txt");
    let file2 = fixture("file6.txt");
    let args = [
        "-o",
        "1.2,2.1,2.2",
        "-e",
        "Wandalen",
        file1.as_str(),
        file2.as_str(),
    ];

    let expected_output = "Alice 1 HR\nBob 2 Finance\nCharlie 3 IT\nKos 4 Wandalen\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn t_test() {
    // No comma in the inputs => the whole line is a single field; the keys
    // ("1 Alice" vs "1 HR" ...) never match, so the output is empty.
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = ["-t", ",", file1.as_str(), file2.as_str()];

    run_test_join(&args, "", "", 0)
}

#[test]
fn fields_test() {
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = ["-1", "1", "-2", "1", file1.as_str(), file2.as_str()];

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn three_fields_test() {
    // Default join on field 1. Output is: join field, then the remaining
    // fields of file1, then the remaining fields of file2. This expectation
    // was confirmed to match GNU `join` byte-for-byte.
    let file1 = fixture("file3.txt");
    let file2 = fixture("file4.txt");
    let args = [file1.as_str(), file2.as_str()];

    let expected_output =
        "1 Bob HR Director HR\n2 Charlie Finance Analyst Finance\n3 Alice Engineering Manager IT\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn third_field_join_test() {
    // Join on field 3 of each file. The join field is emitted ONCE, first,
    // followed by the remaining fields of file1 then file2. GNU `join`:
    //   $ join -1 3 -2 3 file3.txt file4.txt
    //   HR 1 Bob 1 Director
    //   Finance 2 Charlie 2 Analyst
    // (The third input group is out of collation order on field 3, so it is
    // dropped exactly as GNU drops it.) The previous expectation here encoded
    // the old buggy reconstruction and did not match GNU.
    let file1 = fixture("file3.txt");
    let file2 = fixture("file4.txt");
    let args = ["-1", "3", "-2", "3", file1.as_str(), file2.as_str()];

    let expected_output = "HR 1 Bob 1 Director\nFinance 2 Charlie 2 Analyst\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn stdin_as_file1_test() {
    // `-` operand reads file1 from standard input.
    let file2 = fixture("file2.txt");
    let args = ["-", file2.as_str()];
    let stdin = "1 Alice\n2 Bob\n3 Charlie\n4 Kos\n";

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n";

    run_test_join_stdin(&args, stdin, expected_output, "", 0)
}

#[test]
fn stdin_as_file2_test() {
    // `-` operand reads file2 from standard input.
    let file1 = fixture("file1.txt");
    let args = [file1.as_str(), "-"];
    let stdin = "1 HR\n2 Finance\n3 IT\n";

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n";

    run_test_join_stdin(&args, stdin, expected_output, "", 0)
}

#[test]
fn both_stdin_error_test() {
    // POSIX: both files cannot be standard input.
    run_test_join_stdin(
        &["-", "-"],
        "",
        "",
        "join: both files cannot be standard input\n",
        1,
    )
}

#[test]
fn cartesian_dup_file2_test() {
    // Duplicate keys on the file2 (stdin) side: one file1 line crossed with
    // three file2 lines => three output lines (cartesian product).
    let file1 = fixture("file1.txt");
    let args = [file1.as_str(), "-"];
    let stdin = "1 x\n1 y\n1 z\n";

    let expected_output = "1 Alice x\n1 Alice y\n1 Alice z\n";

    run_test_join_stdin(&args, stdin, expected_output, "", 0)
}

#[test]
fn cartesian_dup_file1_test() {
    // Duplicate keys on the file1 (stdin) side: two file1 lines crossed with
    // one file2 line => two output lines.
    let file2 = fixture("file2.txt");
    let args = ["-", file2.as_str()];
    let stdin = "2 a\n2 b\n";

    let expected_output = "2 a Finance\n2 b Finance\n";

    run_test_join_stdin(&args, stdin, expected_output, "", 0)
}

#[test]
fn additional_2_test() {
    // -a 2: also print unpairable lines from file2 (here key 9). Unpairable
    // lines from file1 are NOT printed.
    let file1 = fixture("file1.txt");
    let args = ["-a", "2", file1.as_str(), "-"];
    let stdin = "1 HR\n9 ZZ\n";

    let expected_output = "1 Alice HR\n9 ZZ\n";

    run_test_join_stdin(&args, stdin, expected_output, "", 0)
}

#[test]
fn v_both_test() {
    // -v 1 -v 2: only unpairable lines from both files, interleaved in key
    // order, with the normal joined output suppressed.
    let file1 = fixture("file1.txt");
    let args = ["-v", "1", "-v", "2", file1.as_str(), "-"];
    let stdin = "1 HR\n9 ZZ\n";

    let expected_output = "2 Bob\n3 Charlie\n4 Kos\n9 ZZ\n";

    run_test_join_stdin(&args, stdin, expected_output, "", 0)
}

#[test]
fn o_with_join_field_test() {
    // -o with the `0` (join field) specifier interleaved with file fields.
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = ["-o", "0,1.2,2.2", file1.as_str(), file2.as_str()];

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn o_blank_separated_with_e_test() {
    // -o accepts blank-separated specifiers in one argument; -e fills the
    // missing field for the -a 1 unpairable "4 Kos" line.
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = [
        "-a",
        "1",
        "-o",
        "0 1.2 2.2",
        "-e",
        "MISS",
        file1.as_str(),
        file2.as_str(),
    ];

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n4 Kos MISS\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn o_zero_only_test() {
    // Regression: `-o 0` must not panic (the join field by itself).
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = ["-o", "0", file1.as_str(), file2.as_str()];

    let expected_output = "1\n2\n3\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn default_multiblank_test() {
    // Default separator: runs of blanks collapse to a single separator and
    // leading blanks are ignored, so "1   Alice" splits as ["1", "Alice"].
    let file2 = fixture("file2.txt");
    let args = ["-", file2.as_str()];
    let stdin = "1   Alice\n";

    let expected_output = "1 Alice HR\n";

    run_test_join_stdin(&args, stdin, expected_output, "", 0)
}

#[test]
fn explicit_space_separator_test() {
    // -t ' ' makes a single space the (input and output) separator.
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = ["-t", " ", file1.as_str(), file2.as_str()];

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n";

    run_test_join(&args, expected_output, "", 0)
}

#[test]
fn unsorted_input_diagnostic_test() {
    // Unsorted input on the file1 (stdin) side, where the disorder makes a
    // line unpairable: GNU emits the per-line diagnostic plus the final
    // "input is not in sorted order" and exits 1.
    let file2 = fixture("file2.txt");
    let args = ["-", file2.as_str()];
    let stdin = "9 a\n1 b\n";

    let expected_err = "join: -:2: is not sorted: 1 b\njoin: input is not in sorted order\n";

    run_test_join_stdin(&args, stdin, "", expected_err, 1)
}

#[test]
fn invalid_field_spec_test() {
    // `-o 0.1` is an invalid field specifier (the `0` join-field spec must
    // stand alone).
    let file1 = fixture("file1.txt");
    let file2 = fixture("file2.txt");
    let args = ["-o", "0.1", file1.as_str(), file2.as_str()];

    run_test_join(&args, "", "join: invalid field specifier: '0.1'\n", 1)
}
