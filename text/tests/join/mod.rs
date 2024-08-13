//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

fn run_test_join(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("join"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

#[test]
fn simple_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/join/file1.txt", project_root);
    let file2 = format!("{}/tests/join/file2.txt", project_root);
    let args = [file1.as_str(), file2.as_str()];

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n";

    run_test_join(&args, &expected_output, "", 0)
}

#[test]
fn a_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/join/file1.txt", project_root);
    let file2 = format!("{}/tests/join/file2.txt", project_root);
    let args = ["-a", "1", file1.as_str(), file2.as_str()];

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n4 Kos\n";

    run_test_join(&args, &expected_output, "", 0)
}

#[test]
fn v_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/join/file1.txt", project_root);
    let file2 = format!("{}/tests/join/file2.txt", project_root);
    let args = ["-v", "1", file1.as_str(), file2.as_str()];

    let expected_output = "4 Kos\n";

    run_test_join(&args, &expected_output, "", 0)
}

#[test]
fn o_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/join/file1.txt", project_root);
    let file2 = format!("{}/tests/join/file2.txt", project_root);
    let args = ["-o", "1.2,2.1,2.2,2.1", file1.as_str(), file2.as_str()];

    let expected_output = "Alice 1 HR 1\nBob 2 Finance 2\nCharlie 3 IT 3\n";

    run_test_join(&args, &expected_output, "", 0)
}

#[test]
fn e_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/join/file5.txt", project_root);
    let file2 = format!("{}/tests/join/file6.txt", project_root);
    let args = [
        "-o",
        "1.2,2.1,2.2",
        "-e",
        "Wandalen",
        file1.as_str(),
        file2.as_str(),
    ];

    let expected_output = "Alice 1 HR\nBob 2 Finance\nCharlie 3 IT\nKos 4 Wandalen\n";

    run_test_join(&args, &expected_output, "", 0)
}

#[test]
fn t_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/join/file1.txt", project_root);
    let file2 = format!("{}/tests/join/file2.txt", project_root);
    let args = ["-t", ",", file1.as_str(), file2.as_str()];

    let expected_output = "";

    run_test_join(&args, &expected_output, "", 0)
}

#[test]
fn fields_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/join/file1.txt", project_root);
    let file2 = format!("{}/tests/join/file2.txt", project_root);
    let args = ["-1", "1", "-2", "1", file1.as_str(), file2.as_str()];

    let expected_output = "1 Alice HR\n2 Bob Finance\n3 Charlie IT\n";

    run_test_join(&args, &expected_output, "", 0)
}

#[test]
fn three_fields_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/join/file3.txt", project_root);
    let file2 = format!("{}/tests/join/file4.txt", project_root);
    let args = [file1.as_str(), file2.as_str()];

    let expected_output =
        "1 Bob HR Director HR\n2 Charlie Finance Analyst Finance\n3 Alice Engineering Manager IT\n";

    run_test_join(&args, &expected_output, "", 0)
}

#[test]
fn third_field_join_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/join/file3.txt", project_root);
    let file2 = format!("{}/tests/join/file4.txt", project_root);
    let args = ["-1", "3", "-2", "3", file1.as_str(), file2.as_str()];

    let expected_output = "1 Bob HR Director HR\n2 Charlie Finance Analyst Finance\n";

    run_test_join(&args, &expected_output, "", 0)
}
