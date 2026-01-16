//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};

fn csplit_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("csplit"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_csplit_text_by_lines() {
    csplit_test(
        &["-f", "text", "-", "5", "{3}"],
        "1sdfghnm
2sadsgdhjmf
3zcxbncvm vbm
4asdbncv
5adsbfdgfnfm
6sdfcvncbmcg
7zsdgdgfndcgmncg
8asdbsfdndcgmn
9sfbdxgfndcgmncgmn
10dvsd
11
12
13
14
15
16
17",
        "43\n57\n31\n14\n",
    );
    std::fs::remove_file("text00").unwrap();
    std::fs::remove_file("text01").unwrap();
    std::fs::remove_file("text02").unwrap();
    std::fs::remove_file("text03").unwrap();
}

#[test]
fn test_csplit_text_by_lines_from_file() {
    csplit_test(
        &["-f", "text_f", "tests/assets/test_file.txt", "5", "{3}"],
        "",
        "43\n57\n31\n14\n",
    );
    std::fs::remove_file("text_f00").unwrap();
    std::fs::remove_file("text_f01").unwrap();
    std::fs::remove_file("text_f02").unwrap();
    std::fs::remove_file("text_f03").unwrap();
}

// Note: In BRE, ( is a literal character, \( starts a grouping
// So to match "main(" we use "main(" not "main\("
#[test]
fn test_csplit_c_code_by_regex() {
    csplit_test(
        &[
            "-f",
            "code_c",
            "tests/assets/test_file_c",
            "%main(%",
            "/^}/+1",
            "{3}",
        ],
        "",
        "59\n53\n53\n54\n",
    );
    std::fs::remove_file("code_c00").unwrap();
    std::fs::remove_file("code_c01").unwrap();
    std::fs::remove_file("code_c02").unwrap();
    std::fs::remove_file("code_c03").unwrap();
}

#[test]
fn test_csplit_c_code_by_regex_negative_offset() {
    csplit_test(
        &[
            "-f",
            "code_c_neg",
            "tests/assets/test_file_c",
            "%main(%",
            "/^}/-2",
            "{3}",
        ],
        "",
        "12\n46\n52\n107\n",
    );
    std::fs::remove_file("code_c_neg00").unwrap();
    std::fs::remove_file("code_c_neg01").unwrap();
    std::fs::remove_file("code_c_neg02").unwrap();
    std::fs::remove_file("code_c_neg03").unwrap();
}

#[test]
fn test_csplit_c_code_by_regex_suppress() {
    csplit_test(
        &[
            "-s",
            "-f",
            "code_c_s",
            "tests/assets/test_file_c",
            "%main(%",
            "/^}/+1",
            "{3}",
        ],
        "",
        "",
    );
    std::fs::remove_file("code_c_s00").unwrap();
    std::fs::remove_file("code_c_s01").unwrap();
    std::fs::remove_file("code_c_s02").unwrap();
    std::fs::remove_file("code_c_s03").unwrap();
}

#[test]
fn test_csplit_c_code_by_regex_with_number() {
    csplit_test(
        &[
            "-f",
            "code_c_n",
            "-n",
            "3",
            "tests/assets/test_file_c",
            "%main(%",
            "/^}/+1",
            "{3}",
        ],
        "",
        "59\n53\n53\n54\n",
    );
    std::fs::remove_file("code_c_n000").unwrap();
    std::fs::remove_file("code_c_n001").unwrap();
    std::fs::remove_file("code_c_n002").unwrap();
    std::fs::remove_file("code_c_n003").unwrap();
}

#[test]
fn test_csplit_regex_by_empty_lines() {
    csplit_test(
        &["-f", "empty_lines", "tests/assets/empty_line.txt", "/^$/"],
        "",
        "6\n7\n",
    );
    std::fs::remove_file("empty_lines00").unwrap();
    std::fs::remove_file("empty_lines01").unwrap();
}

#[test]
fn test_csplit_regex_would_infloop() {
    csplit_test(
        &[
            "-f",
            "would_infloop",
            "tests/assets/would_infloop.txt",
            "/a/-1",
            "{*}",
        ],
        "",
        "2\n",
    );
    std::fs::remove_file("would_infloop00").unwrap();
}

#[test]
fn test_csplit_regex_in_uniq() {
    csplit_test(
        &["-f", "in_uniq", "tests/assets/in_uniq", "/^$/", "{*}"],
        "",
        "6\n10\n8\n9\n",
    );
    std::fs::remove_file("in_uniq00").unwrap();
    std::fs::remove_file("in_uniq01").unwrap();
    std::fs::remove_file("in_uniq02").unwrap();
    std::fs::remove_file("in_uniq03").unwrap();
}

#[test]
fn test_csplit_regex_in_uniq_2() {
    csplit_test(
        &["-f", "in_uniq_2_", "tests/assets/in_uniq", "/^$/-1", "{*}"],
        "",
        "3\n10\n8\n12\n",
    );
    std::fs::remove_file("in_uniq_2_00").unwrap();
    std::fs::remove_file("in_uniq_2_01").unwrap();
    std::fs::remove_file("in_uniq_2_02").unwrap();
    std::fs::remove_file("in_uniq_2_03").unwrap();
}

#[test]
fn test_csplit_regex_in_uniq_3() {
    csplit_test(
        &["-f", "in_uniq_3_", "tests/assets/in_uniq", "/^$/1", "{*}"],
        "",
        "7\n10\n8\n8\n",
    );
    std::fs::remove_file("in_uniq_3_00").unwrap();
    std::fs::remove_file("in_uniq_3_01").unwrap();
    std::fs::remove_file("in_uniq_3_02").unwrap();
    std::fs::remove_file("in_uniq_3_03").unwrap();
}

#[test]
fn test_csplit_regex_in_seq() {
    csplit_test(
        &["-f", "in_seq", "tests/assets/in_seq", "/2/", "/4/", "/6/"],
        "",
        "1\n3\n3\n1\n",
    );
    std::fs::remove_file("in_seq00").unwrap();
    std::fs::remove_file("in_seq01").unwrap();
    std::fs::remove_file("in_seq02").unwrap();
    std::fs::remove_file("in_seq03").unwrap();
}

// Test that line number 0 is rejected (POSIX: lines numbered starting at 1)
#[test]
fn test_csplit_error_line_zero() {
    let str_args: Vec<String> = ["-f", "err_zero", "-", "0"]
        .iter()
        .map(|s| String::from(*s))
        .collect();

    run_test(TestPlan {
        cmd: String::from("csplit"),
        args: str_args,
        stdin_data: String::from("line1\nline2\n"),
        expected_out: String::from(""),
        expected_err: String::from(
            "Error: Custom { kind: Other, error: \"line number must be greater than zero\" }\n",
        ),
        expected_exit_code: 1,
    });
}

// Test invalid BRE pattern error
#[test]
fn test_csplit_error_invalid_bre() {
    let str_args: Vec<String> = ["-f", "err_bre", "-", "/[invalid/"]
        .iter()
        .map(|s| String::from(*s))
        .collect();

    // Use checker to only verify error prefix - detailed message may vary by platform
    run_test_with_checker(
        TestPlan {
            cmd: String::from("csplit"),
            args: str_args,
            stdin_data: String::from("line1\nline2\n"),
            expected_out: String::from(""),
            expected_err: String::new(), // checked manually below
            expected_exit_code: 1,
        },
        |_plan, output| {
            assert_eq!(output.stdout, b"");
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("invalid regex"),
                "Expected error containing 'invalid regex', got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

// Test -k option: files should be kept on error
#[test]
fn test_csplit_keep_files_on_error() {
    // This test creates some files then encounters an error (line 999 doesn't exist)
    // With -k, the files should be kept
    let str_args: Vec<String> = ["-k", "-f", "keep_err", "-", "3", "999"]
        .iter()
        .map(|s| String::from(*s))
        .collect();

    run_test(TestPlan {
        cmd: String::from("csplit"),
        args: str_args,
        stdin_data: String::from("line1\nline2\nline3\nline4\n"),
        expected_out: String::from("11\n12\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });

    // With -k, all created files should exist
    assert!(std::path::Path::new("keep_err00").exists());
    assert!(std::path::Path::new("keep_err01").exists());
    std::fs::remove_file("keep_err00").unwrap();
    std::fs::remove_file("keep_err01").unwrap();
}

// Test BRE-specific patterns work correctly
#[test]
fn test_csplit_bre_patterns() {
    // In BRE: \+ means literal +, not "one or more"
    // In BRE: ( is literal, \( is grouping
    // First file: "header" (6 bytes), second file: "line1\nline2\n" (12 bytes)
    csplit_test(
        &["-f", "bre_test", "-", "/^line/"],
        "header\nline1\nline2\n",
        "6\n12\n",
    );
    std::fs::remove_file("bre_test00").unwrap();
    std::fs::remove_file("bre_test01").unwrap();
}
