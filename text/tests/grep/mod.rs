//
// Copyright (c) 2024-2025 Jeff Garzik
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};

mod context;
mod only_matching;
mod word;

const LINES_INPUT: &str =
    "line_{1}\np_line_{2}_s\n  line_{3}  \nLINE_{4}\np_LINE_{5}_s\nl_{6}\nline_{70}\n";
const EMPTY_LINES_INPUT: &str = "\n\n\n";
const BAD_INPUT: &str = "(some text)\n";

const INPUT_FILE_1: &str = "tests/grep/f_1";
const INPUT_FILE_2: &str = "tests/grep/f_2";
const INPUT_FILE_3: &str = "tests/grep/f_3";
const BAD_INPUT_FILE: &str = "tests/grep/inexisting_file";
const INVALID_LINE_INPUT_FILE: &str = "tests/grep/invalid_line";

/// What grep reports for `BAD_INPUT_FILE`: the system's own text for the
/// failed open, which differs between Unix and Windows.
fn bad_input_file_error() -> String {
    format!(
        "grep: {BAD_INPUT_FILE}: {}\n",
        plib::testing::open_error_text(BAD_INPUT_FILE)
    )
}

const BRE: &str = r#"line_{[0-9]\{1,\}}"#;
const ERE: &str = r#"line_\{[0-9]{1,}\}"#;
const FIXED: &str = "line_{";
const INVALID_BRE: &str = r#"\{1,3\}"#;
const INVALID_ERE: &str = r#"{1,3}"#;

const BRE_FILE_1: &str = "tests/grep/bre/p_1";
const BRE_FILE_2: &str = "tests/grep/bre/p_2";
const EMPTY_PATTERN_FILE: &str = "tests/grep/empty_pattern";

fn grep_test(
    args: &[&str],
    test_data: &str,
    expected_output: &str,
    expected_err: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("grep"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_err),
        expected_exit_code,
    });
}

/// Helper for tests that check regex error messages.
/// Only verifies the error contains "invalid regex" - detailed message may vary by platform.
fn grep_test_regex_error(args: &[&str], test_data: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("grep"),
            args: str_args,
            stdin_data: String::from(test_data),
            expected_out: String::new(),
            expected_err: String::new(), // checked manually below
            expected_exit_code,
        },
        |_plan, output| {
            assert_eq!(output.stdout, b"");
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("invalid regex"),
                "Expected error containing 'invalid regex', got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(expected_exit_code));
        },
    );
}

#[test]
fn test_incompatible_options() {
    grep_test(
        &["-cl"],
        "",
        "",
        "grep: Options \'-c\' and \'-l\' cannot be used together\n",
        2,
    );
    grep_test(
        &["-cq"],
        "",
        "",
        "grep: Options \'-c\' and \'-q\' cannot be used together\n",
        2,
    );
    grep_test(
        &["-lq"],
        "",
        "",
        "grep: Options \'-l\' and \'-q\' cannot be used together\n",
        2,
    );
}

#[test]
fn test_absent_pattern() {
    grep_test(
        &[],
        "",
        "",
        "grep: Required at least one pattern list or file\n",
        2,
    );
}

#[test]
fn test_inexisting_file_pattern() {
    grep_test(&["-f", BAD_INPUT_FILE], "", "", &bad_input_file_error(), 2);
}

#[test]
fn test_regexp_compiling_error() {
    // Error message comes from regerror() - detailed message may vary by platform
    grep_test_regex_error(&[INVALID_BRE], "", 2);
}

#[test]
fn test_basic_regexp_01() {
    grep_test(
        &[BRE],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_basic_regexp_02() {
    grep_test(&[BRE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_basic_regexp_03() {
    grep_test(
        &[BRE, INVALID_LINE_INPUT_FILE],
        "",
        "line_{1}\np_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_basic_regexp_count_01() {
    grep_test(&["-c", BRE], LINES_INPUT, "4\n", "", 0);
}

#[test]
fn test_basic_regexp_count_02() {
    grep_test(&["-c", BRE], BAD_INPUT, "0\n", "", 1);
}

#[test]
fn test_basic_regexp_files_with_matches_01() {
    grep_test(&["-l", BRE], LINES_INPUT, "(standard input)\n", "", 0);
}

#[test]
fn test_basic_regexp_files_with_matches_02() {
    grep_test(&["-l", BRE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_basic_regexp_quiet_without_error_01() {
    grep_test(&["-q", BRE], LINES_INPUT, "", "", 0);
}

#[test]
fn test_basic_regexp_quiet_without_error_02() {
    grep_test(&["-q", BRE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_basic_regexp_quiet_with_error_01() {
    grep_test(&["-q", BRE, "-", BAD_INPUT_FILE], LINES_INPUT, "", "", 0);
}

#[test]
fn test_basic_regexp_quiet_with_error_02() {
    grep_test(
        &["-q", BRE, BAD_INPUT_FILE, "-"],
        LINES_INPUT,
        "",
        &bad_input_file_error(),
        0,
    );
}

#[test]
fn test_basic_regexp_quiet_with_error_03() {
    grep_test(
        &["-q", BRE, "-", BAD_INPUT_FILE],
        BAD_INPUT,
        "",
        &bad_input_file_error(),
        2,
    );
}

#[test]
fn test_basic_regexp_ignore_case_01() {
    grep_test(
        &["-i", BRE],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nLINE_{4}\np_LINE_{5}_s\nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_basic_regexp_ignore_case_02() {
    grep_test(&["-i", BRE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_basic_regexp_line_number_01() {
    grep_test(
        &["-n", BRE],
        LINES_INPUT,
        "1:line_{1}\n2:p_line_{2}_s\n3:  line_{3}  \n7:line_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_basic_regexp_line_number_02() {
    grep_test(&["-n", BRE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_basic_regexp_line_number_03() {
    grep_test(
        &["-n", BRE, INVALID_LINE_INPUT_FILE],
        "",
        "1:line_{1}\n3:p_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_basic_regexp_no_messages_without_error_01() {
    grep_test(
        &["-s", BRE],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_basic_regexp_no_messages_without_error_02() {
    grep_test(&["-s", BRE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_basic_regexp_no_messages_with_error_01() {
    grep_test(&["-s", BRE, "-", BAD_INPUT_FILE], LINES_INPUT, "(standard input):line_{1}\n(standard input):p_line_{2}_s\n(standard input):  line_{3}  \n(standard input):line_{70}\n", "", 2);
}

#[test]
fn test_basic_regexp_no_messages_with_error_02() {
    grep_test(&["-s", BRE, "-", BAD_INPUT_FILE], BAD_INPUT, "", "", 2);
}

#[test]
fn test_basic_regexp_no_messages_with_error_03() {
    // Error message comes from regerror() - detailed message may vary by platform
    grep_test_regex_error(&["-s", INVALID_BRE, "-", BAD_INPUT_FILE], LINES_INPUT, 2);
}

#[test]
fn test_basic_regexp_no_messages_with_error_04() {
    grep_test(
        &["-q", "-s", BRE, BAD_INPUT_FILE, "-"],
        LINES_INPUT,
        "",
        "",
        0,
    );
}

#[test]
fn test_basic_regexp_no_messages_invalid_utf8_line_05() {
    grep_test(
        &["-s", BRE, INVALID_LINE_INPUT_FILE],
        "",
        "line_{1}\np_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_basic_regexp_invert_match_01() {
    grep_test(
        &["-v", BRE],
        LINES_INPUT,
        "LINE_{4}\np_LINE_{5}_s\nl_{6}\n",
        "",
        0,
    );
}

#[test]
fn test_basic_regexp_invert_match_02() {
    grep_test(&["-v", "."], LINES_INPUT, "", "", 1);
}

#[test]
fn test_basic_regexp_line_regexp_01() {
    grep_test(&["-x", BRE], LINES_INPUT, "line_{1}\nline_{70}\n", "", 0);
}

#[test]
fn test_basic_regexp_line_regexp_02() {
    grep_test(&["-x", BRE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_basic_regexp_option_combination() {
    grep_test(
        &["-insvx", BRE],
        LINES_INPUT,
        "2:p_line_{2}_s\n3:  line_{3}  \n5:p_LINE_{5}_s\n6:l_{6}\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_01() {
    grep_test(
        &["-E", ERE],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_02() {
    grep_test(&["-E", ERE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_extended_regexp_03() {
    grep_test(
        &["-E", ERE, INVALID_LINE_INPUT_FILE],
        "",
        "line_{1}\np_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_count_01() {
    grep_test(&["-E", "-c", ERE], LINES_INPUT, "4\n", "", 0);
}

#[test]
fn test_extended_regexp_count_02() {
    grep_test(&["-E", "-c", ERE], BAD_INPUT, "0\n", "", 1);
}

#[test]
fn test_extended_regexp_files_with_matches_01() {
    grep_test(&["-E", "-l", ERE], LINES_INPUT, "(standard input)\n", "", 0);
}

#[test]
fn test_extended_regexp_files_with_matches_02() {
    grep_test(&["-E", "-l", ERE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_extended_regexp_quiet_without_error_01() {
    grep_test(&["-E", "-q", ERE], LINES_INPUT, "", "", 0);
}

#[test]
fn test_extended_regexp_quiet_without_error_02() {
    grep_test(&["-E", "-q", ERE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_extended_regexp_quiet_with_error_01() {
    grep_test(
        &["-E", "-q", ERE, "-", BAD_INPUT_FILE],
        LINES_INPUT,
        "",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_quiet_with_error_02() {
    grep_test(
        &["-E", "-q", ERE, BAD_INPUT_FILE, "-"],
        LINES_INPUT,
        "",
        &bad_input_file_error(),
        0,
    );
}

#[test]
fn test_extended_regexp_quiet_with_error_03() {
    grep_test(
        &["-E", "-q", ERE, "-", BAD_INPUT_FILE],
        BAD_INPUT,
        "",
        &bad_input_file_error(),
        2,
    );
}

#[test]
fn test_extended_regexp_ignore_case_01() {
    grep_test(
        &["-E", "-i", ERE],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nLINE_{4}\np_LINE_{5}_s\nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_ignore_case_02() {
    grep_test(&["-E", "-i", ERE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_extended_regexp_line_number_01() {
    grep_test(
        &["-E", "-n", ERE],
        LINES_INPUT,
        "1:line_{1}\n2:p_line_{2}_s\n3:  line_{3}  \n7:line_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_line_number_02() {
    grep_test(&["-E", "-n", ERE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_extended_regexp_line_number_03() {
    grep_test(
        &["-E", "-n", ERE, INVALID_LINE_INPUT_FILE],
        "",
        "1:line_{1}\n3:p_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_no_messages_without_error_01() {
    grep_test(
        &["-E", "-s", ERE],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_no_messages_without_error_02() {
    grep_test(&["-E", "-s", ERE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_extended_regexp_no_messages_with_error_01() {
    grep_test(&["-E", "-s", ERE, "-", BAD_INPUT_FILE], LINES_INPUT, "(standard input):line_{1}\n(standard input):p_line_{2}_s\n(standard input):  line_{3}  \n(standard input):line_{70}\n", "", 2);
}

#[test]
fn test_extended_regexp_no_messages_with_error_02() {
    grep_test(
        &["-E", "-s", ERE, "-", BAD_INPUT_FILE],
        BAD_INPUT,
        "",
        "",
        2,
    );
}

#[test]
fn test_extended_regexp_no_messages_with_error_03() {
    // Error message comes from regerror() - detailed message may vary by platform
    grep_test_regex_error(
        &["-E", "-s", INVALID_ERE, "-", BAD_INPUT_FILE],
        LINES_INPUT,
        2,
    );
}

#[test]
fn test_extended_regexp_no_messages_with_error_04() {
    grep_test(
        &["-E", "-q", "-s", ERE, BAD_INPUT_FILE, "-"],
        LINES_INPUT,
        "",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_no_messages_invalid_utf8_line_05() {
    grep_test(
        &["-E", "-s", ERE, INVALID_LINE_INPUT_FILE],
        "",
        "line_{1}\np_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_invert_match_01() {
    grep_test(
        &["-E", "-v", ERE],
        LINES_INPUT,
        "LINE_{4}\np_LINE_{5}_s\nl_{6}\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_invert_match_02() {
    grep_test(&["-E", "-v", "."], LINES_INPUT, "", "", 1);
}

#[test]
fn test_extended_regexp_line_regexp_01() {
    grep_test(
        &["-E", "-x", ERE],
        LINES_INPUT,
        "line_{1}\nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_extended_regexp_line_regexp_02() {
    grep_test(&["-E", "-x", ERE], BAD_INPUT, "", "", 1);
}

#[test]
fn test_extended_regexp_option_combination() {
    grep_test(
        &["-E", "-insvx", ERE],
        LINES_INPUT,
        "2:p_line_{2}_s\n3:  line_{3}  \n5:p_LINE_{5}_s\n6:l_{6}\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_01() {
    grep_test(
        &["-F", FIXED],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_02() {
    grep_test(&["-F", FIXED], BAD_INPUT, "", "", 1);
}

#[test]
fn test_fixed_strings_03() {
    grep_test(
        &["-F", FIXED, INVALID_LINE_INPUT_FILE],
        "",
        "line_{1}\np_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_count_01() {
    grep_test(&["-F", "-c", FIXED], LINES_INPUT, "4\n", "", 0);
}

#[test]
fn test_fixed_strings_count_02() {
    grep_test(&["-F", "-c", FIXED], BAD_INPUT, "0\n", "", 1);
}

#[test]
fn test_fixed_strings_files_with_matches_01() {
    grep_test(
        &["-F", "-l", FIXED],
        LINES_INPUT,
        "(standard input)\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_files_with_matches_02() {
    grep_test(&["-F", "-l", FIXED], BAD_INPUT, "", "", 1);
}

#[test]
fn test_fixed_strings_quiet_without_error_01() {
    grep_test(&["-F", "-q", FIXED], LINES_INPUT, "", "", 0);
}

#[test]
fn test_fixed_strings_quiet_without_error_02() {
    grep_test(&["-F", "-q", FIXED], BAD_INPUT, "", "", 1);
}

#[test]
fn test_fixed_strings_quiet_with_error_01() {
    grep_test(
        &["-F", "-q", FIXED, "-", BAD_INPUT_FILE],
        LINES_INPUT,
        "",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_quiet_with_error_02() {
    grep_test(
        &["-F", "-q", FIXED, BAD_INPUT_FILE, "-"],
        LINES_INPUT,
        "",
        &bad_input_file_error(),
        0,
    );
}

#[test]
fn test_fixed_strings_quiet_with_error_03() {
    grep_test(
        &["-F", "-q", FIXED, "-", BAD_INPUT_FILE],
        BAD_INPUT,
        "",
        &bad_input_file_error(),
        2,
    );
}

#[test]
fn test_fixed_strings_ignore_case_01() {
    grep_test(
        &["-F", "-i", FIXED],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nLINE_{4}\np_LINE_{5}_s\nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_ignore_case_02() {
    grep_test(&["-F", "-i", FIXED], BAD_INPUT, "", "", 1);
}

#[test]
fn test_fixed_strings_line_number_01() {
    grep_test(
        &["-F", "-n", FIXED],
        LINES_INPUT,
        "1:line_{1}\n2:p_line_{2}_s\n3:  line_{3}  \n7:line_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_line_number_02() {
    grep_test(&["-F", "-n", FIXED], BAD_INPUT, "", "", 1);
}

#[test]
fn test_fixed_strings_line_number_03() {
    grep_test(
        &["-F", "-n", FIXED, INVALID_LINE_INPUT_FILE],
        "",
        "1:line_{1}\n3:p_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_no_messages_without_error_01() {
    grep_test(
        &["-E", "-s", ERE],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_no_messages_without_error_02() {
    grep_test(&["-F", "-s", FIXED], BAD_INPUT, "", "", 1);
}

#[test]
fn test_fixed_strings_no_messages_with_error_01() {
    grep_test(&["-F", "-s", FIXED, "-", BAD_INPUT_FILE], LINES_INPUT, "(standard input):line_{1}\n(standard input):p_line_{2}_s\n(standard input):  line_{3}  \n(standard input):line_{70}\n", "", 2);
}

#[test]
fn test_fixed_strings_no_messages_with_error_02() {
    grep_test(
        &["-F", "-s", FIXED, "-", BAD_INPUT_FILE],
        BAD_INPUT,
        "",
        "",
        2,
    );
}

#[test]
fn test_fixed_strings_no_messages_with_error_03() {
    grep_test(
        &["-F", "-cl", "-s", FIXED, "-", BAD_INPUT_FILE],
        LINES_INPUT,
        "",
        "grep: Options '-c' and '-l' cannot be used together\n",
        2,
    );
}

#[test]
fn test_fixed_strings_no_messages_with_error_04() {
    grep_test(
        &["-F", "-q", "-s", FIXED, BAD_INPUT_FILE, "-"],
        LINES_INPUT,
        "",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_no_messages_invalid_utf8_line_05() {
    grep_test(
        &["-F", "-s", FIXED, INVALID_LINE_INPUT_FILE],
        "",
        "line_{1}\np_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_invert_match_01() {
    grep_test(
        &["-F", "-v", FIXED],
        LINES_INPUT,
        "LINE_{4}\np_LINE_{5}_s\nl_{6}\n",
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_invert_match_02() {
    grep_test(
        &["-F", "-v", "some_bad_pattern"],
        LINES_INPUT,
        LINES_INPUT,
        "",
        0,
    );
}

#[test]
fn test_fixed_strings_invert_match_03() {
    grep_test(&["-F", "-v", ""], LINES_INPUT, "", "", 1);
}

#[test]
fn test_fixed_strings_line_regexp_01() {
    grep_test(&["-F", "-x", "line_{1}"], "line_{1}\n", "line_{1}\n", "", 0);
}

#[test]
fn test_fixed_strings_line_regexp_02() {
    grep_test(&["-F", "-x", "line_{1}"], BAD_INPUT, "", "", 1);
}

#[test]
fn test_fixed_strings_option_combination() {
    grep_test(
            &["-F", "-insvx", FIXED],
            LINES_INPUT,
            "1:line_{1}\n2:p_line_{2}_s\n3:  line_{3}  \n4:LINE_{4}\n5:p_LINE_{5}_s\n6:l_{6}\n7:line_{70}\n",
            "",
            0,
        );
}

#[test]
fn test_multiline_basic_regexes_01() {
    grep_test(
        &["line_{[0-9]\\{1,\\}}\nl_{[0-9]\\{1,\\}}"],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nl_{6}\nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_multiline_basic_regexes_02() {
    grep_test(
        &["line_{[0-9]\\{1,\\}}\nl_{[0-9]\\{1,\\}}"],
        BAD_INPUT,
        "",
        "",
        1,
    );
}

#[test]
fn test_multiline_basic_regexes_all_lines() {
    grep_test(&["some_pattern\n"], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_multiline_basic_extended_regexes_01() {
    grep_test(
        &["-E", "line_\\{[0-9]{1,}\\}\nl_\\{[0-9]{1,}\\}"],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nl_{6}\nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_multiline_extended_regexes_02() {
    grep_test(
        &["-E", "line_\\{[0-9]{1,}\\}\nl_\\{[0-9]{1,}\\}"],
        BAD_INPUT,
        "",
        "",
        1,
    );
}

#[test]
fn test_multiline_extended_regexes_all_lines() {
    grep_test(&["-E", "some_pattern\n"], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_multiline_fixed_strings_01() {
    grep_test(
        &["-F", "line_\nl_"],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nl_{6}\nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_multiline_fixed_strings_02() {
    grep_test(&["-F", "line_\nl_"], BAD_INPUT, "", "", 1);
}

#[test]
fn test_multiline_fixed_strings_all_lines() {
    grep_test(&["-E", "some pattern\n"], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_single_stdin() {
    grep_test(
        &[BRE, "-"],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_duplicate_stdin() {
    grep_test(
            &[BRE, "-", "-", "-"],
            LINES_INPUT,
            "(standard input):line_{1}\n(standard input):p_line_{2}_s\n(standard input):  line_{3}  \n(standard input):line_{70}\n",
            "",
            0,
        );
}

#[test]
fn test_duplicate_stdin_count() {
    grep_test(
        &["-c", BRE, "-", "-"],
        LINES_INPUT,
        "(standard input):4\n(standard input):0\n",
        "",
        0,
    );
}

#[test]
fn test_duplicate_stdin_files_with_matches() {
    grep_test(
        &["-l", BRE, "-", "-"],
        LINES_INPUT,
        "(standard input)\n",
        "",
        0,
    );
}

#[test]
fn test_duplicate_stdin_quiet() {
    grep_test(&["-q", BRE, "-", "-"], LINES_INPUT, "", "", 0);
}

#[test]
fn test_duplicate_stdin_line_number() {
    grep_test(
            &["-n", BRE, "-", "-", "-"],
            LINES_INPUT,
            "(standard input):1:line_{1}\n(standard input):2:p_line_{2}_s\n(standard input):3:  line_{3}  \n(standard input):7:line_{70}\n",
            "",
            0,
        );
}

#[test]
fn test_stdin_and_file_input() {
    grep_test(
            &[BRE, "-", INPUT_FILE_1, INPUT_FILE_2],
            LINES_INPUT,
            "(standard input):line_{1}\n(standard input):p_line_{2}_s\n(standard input):  line_{3}  \n(standard input):line_{70}\ntests/grep/f_1:line_{1}\ntests/grep/f_1:p_line_{2}_s\ntests/grep/f_1:  line_{3}  \ntests/grep/f_1:line_{70}\n",
            "",
            0,
        );
}

#[test]
fn test_stdin_and_input_files_count() {
    grep_test(
        &["-c", BRE, "-", INPUT_FILE_1, INPUT_FILE_2],
        LINES_INPUT,
        "(standard input):4\ntests/grep/f_1:4\ntests/grep/f_2:0\n",
        "",
        0,
    );
}

#[test]
fn test_stdin_and_input_files_files_with_matches() {
    grep_test(
        &["-l", BRE, "-", INPUT_FILE_1, INPUT_FILE_2],
        LINES_INPUT,
        "(standard input)\ntests/grep/f_1\n",
        "",
        0,
    );
}

#[test]
fn test_stdin_and_input_files_quiet() {
    grep_test(
        &["-q", BRE, "-", INPUT_FILE_1, INPUT_FILE_2],
        LINES_INPUT,
        "",
        "",
        0,
    );
}

#[test]
fn test_stdin_and_input_files_other_options() {
    grep_test(&["-insvx", BRE, "-", INPUT_FILE_1, BAD_INPUT_FILE], LINES_INPUT, "(standard input):2:p_line_{2}_s\n(standard input):3:  line_{3}  \n(standard input):5:p_LINE_{5}_s\n(standard input):6:l_{6}\ntests/grep/f_1:2:p_line_{2}_s\ntests/grep/f_1:3:  line_{3}  \ntests/grep/f_1:5:p_LINE_{5}_s\ntests/grep/f_1:6:l_{6}\n", "", 2);
}

#[test]
fn test_multiple_input_files() {
    grep_test(&[r#"2[[:punct:]]"#, INPUT_FILE_1, INPUT_FILE_2, INPUT_FILE_3], LINES_INPUT,
        "tests/grep/f_1:p_line_{2}_s\ntests/grep/f_2:void func2() {\ntests/grep/f_2:    printf(\"This is function 2\\n\");\n", "", 0);
}

#[test]
fn test_multiple_input_files_count() {
    grep_test(
        &[
            "-c",
            r#"2[[:punct:]]"#,
            INPUT_FILE_1,
            INPUT_FILE_2,
            INPUT_FILE_3,
        ],
        LINES_INPUT,
        "tests/grep/f_1:1\ntests/grep/f_2:2\ntests/grep/f_3:0\n",
        "",
        0,
    );
}

#[test]
fn test_multiple_input_files_files_with_matches() {
    grep_test(
        &[
            "-l",
            r#"2[[:punct:]]"#,
            INPUT_FILE_1,
            INPUT_FILE_2,
            INPUT_FILE_3,
        ],
        LINES_INPUT,
        "tests/grep/f_1\ntests/grep/f_2\n",
        "",
        0,
    );
}

#[test]
fn test_multiple_input_files_quiet() {
    grep_test(
        &[
            "-q",
            r#"2[[:punct:]]"#,
            INPUT_FILE_1,
            INPUT_FILE_2,
            INPUT_FILE_3,
        ],
        LINES_INPUT,
        "",
        "",
        0,
    );
}

#[test]
fn test_multiple_input_files_line_number() {
    grep_test(&["-n", r#"2[[:punct:]]"#, INPUT_FILE_1, INPUT_FILE_2, INPUT_FILE_3], LINES_INPUT,
        "tests/grep/f_1:2:p_line_{2}_s\ntests/grep/f_2:12:void func2() {\ntests/grep/f_2:13:    printf(\"This is function 2\\n\");\n", "", 0);
}

#[test]
fn test_duplicate_input_files() {
    grep_test(
        &["-n", r#"2[[:punct:]]"#, INPUT_FILE_1, INPUT_FILE_1],
        LINES_INPUT,
        "tests/grep/f_1:2:p_line_{2}_s\ntests/grep/f_1:2:p_line_{2}_s\n",
        "",
        0,
    );
}

#[test]
fn test_duplicate_input_files_count() {
    grep_test(
        &["-c", r#"2[[:punct:]]"#, INPUT_FILE_1, INPUT_FILE_1],
        LINES_INPUT,
        "tests/grep/f_1:1\ntests/grep/f_1:1\n",
        "",
        0,
    );
}

#[test]
fn test_duplicate_input_files_files_with_matches() {
    grep_test(
        &["-l", r#"2[[:punct:]]"#, INPUT_FILE_1, INPUT_FILE_1],
        LINES_INPUT,
        "tests/grep/f_1\ntests/grep/f_1\n",
        "",
        0,
    );
}

#[test]
fn test_duplicate_input_files_quiet() {
    grep_test(
        &["-q", r#"2[[:punct:]]"#, INPUT_FILE_1, INPUT_FILE_1],
        LINES_INPUT,
        "",
        "",
        0,
    );
}

#[test]
fn test_multiple_pattern_files_multiple_input_files() {
    grep_test(&["-f", BRE_FILE_1, "-f", BRE_FILE_2, INPUT_FILE_1, INPUT_FILE_2, INPUT_FILE_3], LINES_INPUT, "tests/grep/f_1:line_{1}\ntests/grep/f_1:p_line_{2}_s\ntests/grep/f_1:  line_{3}  \ntests/grep/f_1:line_{70}\ntests/grep/f_2:#include <stdio.h>\ntests/grep/f_2:void func1() {\ntests/grep/f_2:void func2() {\n", "", 0);
}

#[test]
fn test_multiple_pattern_files_multiple_input_files_count() {
    grep_test(
        &[
            "-c",
            "-f",
            BRE_FILE_1,
            "-f",
            BRE_FILE_2,
            INPUT_FILE_1,
            INPUT_FILE_2,
            INPUT_FILE_3,
        ],
        LINES_INPUT,
        "tests/grep/f_1:4\ntests/grep/f_2:3\ntests/grep/f_3:0\n",
        "",
        0,
    );
}

#[test]
fn test_multiple_pattern_files_multiple_input_files_files_with_matches() {
    grep_test(
        &[
            "-l",
            "-f",
            BRE_FILE_1,
            "-f",
            BRE_FILE_2,
            INPUT_FILE_1,
            INPUT_FILE_2,
            INPUT_FILE_3,
        ],
        LINES_INPUT,
        "tests/grep/f_1\ntests/grep/f_2\n",
        "",
        0,
    );
}

#[test]
fn test_multiple_pattern_files_multiple_input_files_quiet() {
    grep_test(
        &[
            "-q",
            "-f",
            BRE_FILE_1,
            "-f",
            BRE_FILE_2,
            INPUT_FILE_1,
            INPUT_FILE_2,
            INPUT_FILE_3,
        ],
        LINES_INPUT,
        "",
        "",
        0,
    );
}

#[test]
fn test_multiple_pattern_files_multiple_input_files_line_number() {
    grep_test(&["-n", "-f", BRE_FILE_1, "-f", BRE_FILE_2, INPUT_FILE_1, INPUT_FILE_2, INPUT_FILE_3], LINES_INPUT, "tests/grep/f_1:1:line_{1}\ntests/grep/f_1:2:p_line_{2}_s\ntests/grep/f_1:3:  line_{3}  \ntests/grep/f_1:7:line_{70}\ntests/grep/f_2:1:#include <stdio.h>\ntests/grep/f_2:8:void func1() {\ntests/grep/f_2:12:void func2() {\n", "", 0);
}

#[test]
fn test_empty_basic_regexp_01() {
    grep_test(&[""], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_empty_basic_regexp_02() {
    grep_test(&[""], EMPTY_LINES_INPUT, EMPTY_LINES_INPUT, "", 0);
}

#[test]
fn test_empty_basic_regexp_03() {
    grep_test(&["-e", ""], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_empty_basic_regexp_04() {
    grep_test(&["-f", EMPTY_PATTERN_FILE], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_empty_basic_regexp_05() {
    grep_test(&[""], "", "", "", 1);
}

#[test]
fn test_empty_basic_regexp_06() {
    grep_test(&["-e", ""], "", "", "", 1);
}

#[test]
fn test_empty_basic_regexp_07() {
    grep_test(&["-f", EMPTY_PATTERN_FILE], "", "", "", 1);
}

#[test]
fn test_empty_extended_regexp_01() {
    grep_test(&["-E", ""], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_empty_extended_regexp_02() {
    grep_test(&["-E", ""], EMPTY_LINES_INPUT, EMPTY_LINES_INPUT, "", 0);
}

#[test]
fn test_empty_extended_regexp_03() {
    grep_test(&["-E", "-e", ""], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_empty_extended_regexp_04() {
    grep_test(
        &["-E", "-f", EMPTY_PATTERN_FILE],
        LINES_INPUT,
        LINES_INPUT,
        "",
        0,
    );
}

#[test]
fn test_empty_extended_regexp_05() {
    grep_test(&["-E", ""], "", "", "", 1);
}

#[test]
fn test_empty_extended_regexp_06() {
    grep_test(&["-E", "-e", ""], "", "", "", 1);
}

#[test]
fn test_empty_extended_regexp_07() {
    grep_test(&["-E", "-f", EMPTY_PATTERN_FILE], "", "", "", 1);
}

#[test]
fn test_empty_fixed_strings_01() {
    grep_test(&["-F", ""], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_empty_fixed_strings_02() {
    grep_test(&["-E", ""], EMPTY_LINES_INPUT, EMPTY_LINES_INPUT, "", 0);
}

#[test]
fn test_empty_fixed_strings_03() {
    grep_test(&["-F", "-e", ""], LINES_INPUT, LINES_INPUT, "", 0);
}

#[test]
fn test_empty_fixed_strings_04() {
    grep_test(
        &["-F", "-f", EMPTY_PATTERN_FILE],
        LINES_INPUT,
        LINES_INPUT,
        "",
        0,
    );
}

#[test]
fn test_empty_fixed_strings_05() {
    grep_test(&["-F", ""], "", "", "", 1);
}

#[test]
fn test_empty_fixed_strings_06() {
    grep_test(&["-F", "-e", ""], "", "", "", 1);
}

#[test]
fn test_empty_fixed_strings_07() {
    grep_test(&["-F", "-f", EMPTY_PATTERN_FILE], "", "", "", 1);
}

#[test]
fn test_long_names_extended_regexp() {
    grep_test(
        &["--extended-regexp", ERE],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_long_names_fixed_strings() {
    grep_test(
        &["--fixed-strings", FIXED],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_long_names_count() {
    grep_test(&["--count", BRE], LINES_INPUT, "4\n", "", 0);
}

#[test]
fn test_long_names_files_with_matches() {
    grep_test(
        &["--files-with-matches", BRE],
        LINES_INPUT,
        "(standard input)\n",
        "",
        0,
    );
}

#[test]
fn test_long_names_quiet() {
    grep_test(&["--quiet", BRE], LINES_INPUT, "", "", 0);
}

#[test]
fn test_long_names_other_options() {
    grep_test(
        &[
            "--ignore-case",
            "--line-number",
            "--no-messages",
            "--invert-match",
            "--line-regexp",
            BRE,
        ],
        LINES_INPUT,
        "2:p_line_{2}_s\n3:  line_{3}  \n5:p_LINE_{5}_s\n6:l_{6}\n",
        "",
        0,
    );
}

#[test]
fn test_regexp_long_names_regexes() {
    grep_test(
        &["--regexp", BRE],
        LINES_INPUT,
        "line_{1}\np_line_{2}_s\n  line_{3}  \nline_{70}\n",
        "",
        0,
    );
}

#[test]
fn test_long_names_files() {
    grep_test(
            &[
                "--file",
                BRE_FILE_1,
                INPUT_FILE_1,
                INPUT_FILE_2,
            ],
            LINES_INPUT,
            "tests/grep/f_1:line_{1}\ntests/grep/f_1:p_line_{2}_s\ntests/grep/f_1:  line_{3}  \ntests/grep/f_1:line_{70}\n",
            "",
            0,
        );
}

#[test]
fn test_duplicate_patterns_are_all_used() {
    // Every specified pattern is kept (duplicates are not removed). Two
    // identical -e patterns still match the line exactly once.
    grep_test(&["-e", "foo", "-e", "foo"], "foo\nbar\n", "foo\n", "", 0);
}

// ---------------------------------------------------------------------------
// Pattern-file and input edge cases
// ---------------------------------------------------------------------------

fn grep_tmp(tag: &str, content: &str) -> plib::testing::TempFile {
    plib::testing::TempFile::new(tag, content)
}

fn grep_stdin(args: &[&str], stdin: &[u8]) -> (Vec<u8>, i32) {
    use std::io::Write;
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_grep"))
        .args(args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("spawn grep");
    child.stdin.as_mut().unwrap().write_all(stdin).unwrap();
    let out = child.wait_with_output().expect("wait grep");
    (out.stdout, out.status.code().unwrap_or(-1))
}

#[test]
fn grep_pattern_file_with_a_trailing_empty_line() {
    // A pattern file ending in a blank line contributes an empty pattern, and
    // an empty BRE matches every line.
    let one = grep_tmp("p1", "foo\n");
    let (out, code) = grep_stdin(&["-f", one.to_str().unwrap()], b"foo\nbar\n");
    assert_eq!(code, 0);
    assert_eq!(String::from_utf8_lossy(&out), "foo\n");

    let blank = grep_tmp("p2", "foo\n\n");
    let (out, code) = grep_stdin(&["-f", blank.to_str().unwrap()], b"foo\nbar\n");
    assert_eq!(code, 0);
    assert_eq!(
        String::from_utf8_lossy(&out),
        "foo\nbar\n",
        "an empty last pattern matches every line"
    );
}

#[test]
fn grep_input_containing_nul_bytes() {
    // POSIX requires grep's input to be text files, and a NUL byte makes a file
    // non-text, so the behavior here is unspecified. Pin what we actually do.
    //
    // Lines *without* a NUL still match normally even when another line has one,
    // so the file is not abandoned wholesale.
    let (out, code) = grep_stdin(&["plain"], b"a\x00b\nplain\n");
    assert_eq!(code, 0);
    assert_eq!(String::from_utf8_lossy(&out), "plain\n");

    // A line containing a NUL never matches: the match goes through POSIX
    // `regexec`, which takes a NUL-terminated string, so the conversion fails
    // and the line is skipped. GNU grep instead reports "binary file matches"
    // and exits 0. Both are within the latitude the spec allows for non-text
    // input. This comment is the record of that divergence: the text/ audit it
    // was written against is in git history.
    let (out, code) = grep_stdin(&["a"], b"a\x00b\n");
    assert_eq!(code, 1, "a NUL-bearing line does not match");
    assert!(out.is_empty());
}

#[test]
fn grep_crlf_line_endings() {
    // A <carriage-return> is part of the line's content, so `x$` does not match
    // a line ending in CRLF, while `x\r$` does.
    let (out, code) = grep_stdin(&["x$"], b"x\r\n");
    assert_eq!(code, 1, "the CR is data, so `x$` must not match");
    assert!(out.is_empty());

    let (out, code) = grep_stdin(&["x"], b"x\r\n");
    assert_eq!(code, 0);
    assert_eq!(out, b"x\r\n", "the CR is preserved in the output");
}

#[test]
fn grep_ere_repetition_modifier() {
    // POSIX.2024 *defines* a duplication symbol suffixed by `?`: "Each of the
    // duplication symbols ('+', '*', '?', and intervals) can be suffixed by the
    // repetition modifier '?' ... in which case matching behavior for that
    // repetition shall be changed from the leftmost longest possible match to
    // the leftmost shortest possible match" (XBD 9.4.6 rule 6, 6759-6765).
    // This is distinct from "multiple adjacent duplication symbols" such as
    // `a+*`, which remain undefined (6770-6771) and so are not tested here.
    //
    // grep reports only *whether* a line matched, never the extent of the
    // match, so leftmost-longest vs leftmost-shortest is unobservable through
    // this utility. What is observable is that the construct compiles and
    // matches. We delegate to libc regcomp: glibc implements the modifier,
    // while the BSD engine on macOS predates POSIX.2024 and rejects it as an
    // invalid ERE (exit 2). Closing that gap would mean shipping our own regex
    // engine, so accept either outcome -- but hold each to its own contract.
    let (out, code) = grep_stdin(&["-E", "a+?"], b"aaa\n");
    match code {
        0 => assert_eq!(
            String::from_utf8_lossy(&out),
            "aaa\n",
            "where the repetition modifier is supported, the line matches"
        ),
        2 => assert!(
            out.is_empty(),
            "a pattern the platform rejects must not also emit matches, got {:?}",
            String::from_utf8_lossy(&out)
        ),
        c => panic!("`a+?` must either match (0) or be rejected (2), got exit {c}"),
    }
}

// ---------------------------------------------------------------------------
// Locale-dependent behavior
// ---------------------------------------------------------------------------

#[test]
fn grep_case_insensitive_fixed_string_folding() {
    // -i folding follows LC_CTYPE. The interesting divergence is Turkish, where
    // `I` lowercases to the dotless `ı` rather than `i`, so `grep -i -F I`
    // should not match `i` there. Most hosts have no Turkish locale, so the
    // Turkish half runs only where one is installed; the ASCII half always
    // does.
    let (out, code) = grep_stdin(&["-i", "-F", "abc"], b"ABC\n");
    assert_eq!(code, 0);
    assert_eq!(String::from_utf8_lossy(&out), "ABC\n");

    let Some(tr_locale) = plib::testing::locale_matching(&["tr_TR.UTF-8", "tr_TR.utf8"]) else {
        return;
    };
    use std::io::Write;
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_grep"))
        .args(["-i", "-F", "I"])
        .env("LC_ALL", &tr_locale)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("spawn grep");
    let _ = child.stdin.as_mut().unwrap().write_all("i\n".as_bytes());
    let out = child.wait_with_output().expect("wait grep");
    // Whatever the folding rule, the run must terminate cleanly.
    assert!(
        matches!(out.status.code(), Some(0) | Some(1)),
        "got {:?}",
        out.status.code()
    );
}

#[test]
fn grep_diagnostics_name_the_utility() {
    // The diagnostic goes through gettext, so it is translatable; with no
    // catalog installed it stays English and carries the utility prefix.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_grep"))
        .args(["pattern", "no-such-file-xyz"])
        .output()
        .expect("run grep");
    assert_eq!(out.status.code(), Some(2), "an unreadable operand exits 2");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("no-such-file-xyz"), "got {stderr:?}");
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. autoconf's
// AC_PROG_GREP runs `grep -e 'GREP$' -e '-(cannot match)-'`; clap used to
// read the second pattern as an option and refuse it, failing configure.
#[test]
fn test_option_argument_begins_with_hyphen() {
    grep_test(
        &["-e", "GREP$", "-e", "-(cannot match)-"],
        "GREP\nnot this\n",
        "GREP\n",
        "",
        0,
    );
}

// Only the word after an option that takes a value is its argument: the
// options around it still parse as options.
#[test]
fn test_hyphen_pattern_then_option() {
    grep_test(&["-e", "-x", "-c"], "a-x\n-x\nb\n", "2\n", "", 0);
}

// A read error ends that input: grep reports it once and goes on to the next
// operand. A directory operand used to make grep print "error reading line N"
// for ever, hanging rpcsvc-proto's build (`grep -i GNU pkg ../*`).
#[cfg(unix)]
#[test]
fn test_unreadable_operand_is_reported_once() {
    use std::time::{Duration, Instant};
    let tmp = plib::tmp::TempDir::new().unwrap();
    let dir = tmp.path().join("d");
    std::fs::create_dir(&dir).unwrap();
    let file = tmp.path().join("f");
    std::fs::write(&file, "GNU here\nnot this\n").unwrap();

    let mut child = std::process::Command::new(plib::testing::get_binary_path("grep"))
        .args(["GNU"])
        .arg(&dir)
        .arg(&file)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    let start = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if start.elapsed() > Duration::from_secs(20) {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("grep never finished reading a directory operand");
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    let out = child.wait_with_output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(stdout, format!("{}:GNU here\n", file.display()));
    assert_eq!(stderr.lines().count(), 1, "{stderr:?}");
    assert!(stderr.contains(&*dir.to_string_lossy()), "{stderr:?}");
    assert_eq!(out.status.code(), Some(2));
}

/// Run grep with `env` added to its environment; returns stdout, stderr and
/// the exit status.
fn grep_bytes_with_env(
    args: &[&str],
    stdin: &[u8],
    env: &[(&str, &str)],
) -> (Vec<u8>, String, i32) {
    let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let out = plib::testing::run_test_base_with_env("grep", &args, stdin, env);
    (
        out.stdout,
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

// A line that is not valid UTF-8 is still a line. In the C locale every byte
// is a character, so it matches like any other; grep used to skip it with
// "error reading line N" and exit 2, even with no locale set at all.
#[test]
fn test_line_not_valid_utf8_is_searched_in_the_c_locale() {
    let c = [("LC_ALL", "C")];
    let input: &[u8] = b"x\xff foo\nbar\n\xc3\xa9\xe9 FOO\n";
    let cases: [(&[&str], &[u8]); 8] = [
        (&["foo"], b"x\xff foo\n"),
        (&["-F", "foo"], b"x\xff foo\n"),
        (&["-i", "foo"], b"x\xff foo\n\xc3\xa9\xe9 FOO\n"),
        (&["-F", "-i", "foo"], b"x\xff foo\n\xc3\xa9\xe9 FOO\n"),
        (&["-v", "-n", "bar"], b"1:x\xff foo\n3:\xc3\xa9\xe9 FOO\n"),
        (&["-c", "x. "], b"1\n"),
        // `.` is one byte: three of them before the blank on line 3.
        (&["^... "], b"\xc3\xa9\xe9 FOO\n"),
        (&["-F", "-x", "\u{e9}\u{fffd}"], b""),
    ];
    for (args, expected) in cases {
        let (out, err, code) = grep_bytes_with_env(args, input, &c);
        let want_code = if expected.is_empty() { 1 } else { 0 };
        assert_eq!(
            (out.as_slice(), err.as_str(), code),
            (expected, "", want_code),
            "{args:?}"
        );
    }
}

// The same with no locale variable set at all (`env -i grep`).
#[test]
fn test_line_not_valid_utf8_is_searched_with_no_locale() {
    let mut cmd = std::process::Command::new(plib::testing::get_binary_path("grep"));
    cmd.env_clear()
        .arg("foo")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    let mut child = cmd.spawn().unwrap();
    {
        use std::io::Write;
        child
            .stdin
            .take()
            .unwrap()
            .write_all(b"x\xff foo\n")
            .unwrap();
    }
    let out = child.wait_with_output().unwrap();
    assert_eq!(out.stdout, b"x\xff foo\n");
    assert_eq!(String::from_utf8_lossy(&out.stderr), "");
    assert_eq!(out.status.code(), Some(0));
}

// In a UTF-8 locale an invalid byte is no character at all: it is not matched
// by `.`, but the rest of its line is searched and the line is written as is.
#[test]
fn test_line_not_valid_utf8_is_searched_in_a_utf8_locale() {
    let Some(locale) = plib::testing::utf8_locale() else {
        return;
    };
    let env = [("LC_ALL", locale.as_str())];
    let input: &[u8] = b"x\xff foo\n\xc3\xa9 FOO\n";
    let cases: [(&[&str], &[u8]); 4] = [
        (&["foo"], b"x\xff foo\n"),
        (&["-F", "-i", "foo"], b"x\xff foo\n\xc3\xa9 FOO\n"),
        (&["-c", "^. "], b"1\n"),
        (&["-v", "foo"], b"\xc3\xa9 FOO\n"),
    ];
    for (args, expected) in cases {
        let (out, err, code) = grep_bytes_with_env(args, input, &env);
        assert_eq!(
            (out.as_slice(), err.as_str(), code),
            (expected, "", 0),
            "{args:?}"
        );
    }
}

// -H (GNU) names the file on every output line, even for a single input;
// --with-filename is its long spelling.
#[test]
fn test_with_filename() {
    let named = format!("{INPUT_FILE_1}:line_{{1}}\n{INPUT_FILE_1}:line_{{70}}\n");
    grep_test(&["-H", "^line", INPUT_FILE_1], "", &named, "", 0);
    grep_test(
        &["--with-filename", "^line", INPUT_FILE_1],
        "",
        &named,
        "",
        0,
    );
    grep_test(
        &["-H", "-n", "^line_{7", INPUT_FILE_1],
        "",
        &format!("{INPUT_FILE_1}:7:line_{{70}}\n"),
        "",
        0,
    );
    grep_test(
        &["-H", "-c", "^line", INPUT_FILE_1],
        "",
        &format!("{INPUT_FILE_1}:2\n"),
        "",
        0,
    );
    grep_test(
        &["-H", "^line_{7"],
        LINES_INPUT,
        "(standard input):line_{70}\n",
        "",
        0,
    );
}

// -h (GNU) never names the file, even for several inputs.  -h no longer means
// --help, and of -h and -H the last one given wins.
#[test]
fn test_no_filename() {
    grep_test(
        &["-h", "^line_{7", INPUT_FILE_1, "-"],
        LINES_INPUT,
        "line_{70}\nline_{70}\n",
        "",
        0,
    );
    grep_test(
        &["--no-filename", "-n", "^line_{7", INPUT_FILE_1, "-"],
        LINES_INPUT,
        "7:line_{70}\n7:line_{70}\n",
        "",
        0,
    );
    grep_test(
        &["-h", "-c", "^line_{7", INPUT_FILE_1, "-"],
        LINES_INPUT,
        "1\n1\n",
        "",
        0,
    );
    grep_test(&["-Hh", "^line_{7", INPUT_FILE_1], "", "line_{70}\n", "", 0);
    grep_test(
        &["-hH", "^line_{7", INPUT_FILE_1],
        "",
        &format!("{INPUT_FILE_1}:line_{{70}}\n"),
        "",
        0,
    );
    // -l writes names whatever -h says.
    grep_test(
        &["-h", "-l", "^line_{7", INPUT_FILE_1],
        "",
        &format!("{INPUT_FILE_1}\n"),
        "",
        0,
    );
}

// Help is reachable as --help only.
#[test]
fn test_help_is_long_only() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("grep"),
            args: vec![String::from("--help")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            let out = String::from_utf8_lossy(&output.stdout);
            assert!(out.contains("--help"), "got {out:?}");
            assert!(out.contains("-h, --no-filename"), "got {out:?}");
            assert_eq!(output.status.code(), Some(0));
        },
    );
}

// --label (GNU) names standard input in prefixes and in -l and -c output.
#[test]
fn test_label_names_standard_input() {
    grep_test(
        &["--label=LBL", "-H", "^line_{7", "-"],
        LINES_INPUT,
        "LBL:line_{70}\n",
        "",
        0,
    );
    grep_test(
        &["--label", "LBL", "-H", "^line_{7"],
        LINES_INPUT,
        "LBL:line_{70}\n",
        "",
        0,
    );
    grep_test(
        &["--label=LBL", "^line_{7", "-", INPUT_FILE_1],
        LINES_INPUT,
        &format!("LBL:line_{{70}}\n{INPUT_FILE_1}:line_{{70}}\n"),
        "",
        0,
    );
    grep_test(
        &["--label=LBL", "-c", "^line_{7", INPUT_FILE_1, "-"],
        LINES_INPUT,
        &format!("{INPUT_FILE_1}:1\nLBL:1\n"),
        "",
        0,
    );
    grep_test(
        &["--label=LBL", "-l", "^line_{7"],
        LINES_INPUT,
        "LBL\n",
        "",
        0,
    );
    // A label names standard input only, and only when it is printed.
    grep_test(
        &["--label=LBL", "^line_{7", INPUT_FILE_1],
        "",
        "line_{70}\n",
        "",
        0,
    );
    grep_test(
        &["--label=", "-H", "^line_{7"],
        LINES_INPUT,
        ":line_{70}\n",
        "",
        0,
    );
}
