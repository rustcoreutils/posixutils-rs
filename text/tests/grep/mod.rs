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

const LINES_INPUT: &str =
    "line_{1}\np_line_{2}_s\n  line_{3}  \nLINE_{4}\np_LINE_{5}_s\nl_{6}\nline_{70}\n";
const EMPTY_LINES_INPUT: &str = "\n\n\n";
const BAD_INPUT: &str = "(some text)\n";

const INPUT_FILE_1: &str = "tests/grep/f_1";
const INPUT_FILE_2: &str = "tests/grep/f_2";
const INPUT_FILE_3: &str = "tests/grep/f_3";
const BAD_INPUT_FILE: &str = "tests/grep/inexisting_file";
const INVALID_LINE_INPUT_FILE: &str = "tests/grep/invalid_line";

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

#[test]
fn test_incompatible_options() {
    grep_test(
        &["-cl"],
        "",
        "",
        "Options \'-c\' and \'-l\' cannot be used together\n",
        2,
    );
    grep_test(
        &["-cq"],
        "",
        "",
        "Options \'-c\' and \'-q\' cannot be used together\n",
        2,
    );
    grep_test(
        &["-lq"],
        "",
        "",
        "Options \'-l\' and \'-q\' cannot be used together\n",
        2,
    );
}

#[test]
fn test_absent_pattern() {
    grep_test(
        &[],
        "",
        "",
        "Required at least one pattern list or file\n",
        2,
    );
}

#[test]
fn test_inexisting_file_pattern() {
    grep_test(
        &["-f", BAD_INPUT_FILE],
        "",
        "",
        "tests/grep/inexisting_file: No such file or directory (os error 2)\n",
        2,
    );
}

#[test]
fn test_regexp_compiling_error() {
    grep_test(
        &[INVALID_BRE],
        "",
        "",
        "Error compiling regex '\\{1,3\\}'\n",
        2,
    );
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
        "tests/grep/invalid_line: Error reading line 2 (stream did not contain valid UTF-8)\n",
        2,
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
        "tests/grep/inexisting_file: No such file or directory (os error 2)\n",
        0,
    );
}

#[test]
fn test_basic_regexp_quiet_with_error_03() {
    grep_test(
        &["-q", BRE, "-", BAD_INPUT_FILE],
        BAD_INPUT,
        "",
        "tests/grep/inexisting_file: No such file or directory (os error 2)\n",
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
        "tests/grep/invalid_line: Error reading line 2 (stream did not contain valid UTF-8)\n",
        2,
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
    grep_test(
        &["-s", INVALID_BRE, "-", BAD_INPUT_FILE],
        LINES_INPUT,
        "",
        "Error compiling regex '\\{1,3\\}'\n",
        2,
    );
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
fn test_basic_regexp_no_messages_with_error_05() {
    grep_test(
        &["-s", BRE, INVALID_LINE_INPUT_FILE],
        "",
        "line_{1}\np_line_{2}_s\n",
        "",
        2,
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
        "tests/grep/invalid_line: Error reading line 2 (stream did not contain valid UTF-8)\n",
        2,
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
        "tests/grep/inexisting_file: No such file or directory (os error 2)\n",
        0,
    );
}

#[test]
fn test_extended_regexp_quiet_with_error_03() {
    grep_test(
        &["-E", "-q", ERE, "-", BAD_INPUT_FILE],
        BAD_INPUT,
        "",
        "tests/grep/inexisting_file: No such file or directory (os error 2)\n",
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
        "tests/grep/invalid_line: Error reading line 2 (stream did not contain valid UTF-8)\n",
        2,
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
    grep_test(
        &["-E", "-s", INVALID_ERE, "-", BAD_INPUT_FILE],
        LINES_INPUT,
        "",
        "Error compiling regex '{1,3}'\n",
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
fn test_extended_regexp_no_messages_with_error_05() {
    grep_test(
        &["-E", "-s", ERE, INVALID_LINE_INPUT_FILE],
        "",
        "line_{1}\np_line_{2}_s\n",
        "",
        2,
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
        "tests/grep/invalid_line: Error reading line 2 (stream did not contain valid UTF-8)\n",
        2,
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
        "tests/grep/inexisting_file: No such file or directory (os error 2)\n",
        0,
    );
}

#[test]
fn test_fixed_strings_quiet_with_error_03() {
    grep_test(
        &["-F", "-q", FIXED, "-", BAD_INPUT_FILE],
        BAD_INPUT,
        "",
        "tests/grep/inexisting_file: No such file or directory (os error 2)\n",
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
        "tests/grep/invalid_line: Error reading line 2 (stream did not contain valid UTF-8)\n",
        2,
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
        "Options '-c' and '-l' cannot be used together\n",
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
fn test_fixed_strings_no_messages_with_error_05() {
    grep_test(
        &["-F", "-s", FIXED, INVALID_LINE_INPUT_FILE],
        "",
        "line_{1}\np_line_{2}_s\n",
        "",
        2,
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
fn test_muptiple_pattern_files_multiple_input_files() {
    grep_test(&["-f", BRE_FILE_1, "-f", BRE_FILE_2, INPUT_FILE_1, INPUT_FILE_2, INPUT_FILE_3], LINES_INPUT, "tests/grep/f_1:line_{1}\ntests/grep/f_1:p_line_{2}_s\ntests/grep/f_1:  line_{3}  \ntests/grep/f_1:line_{70}\ntests/grep/f_2:#include <stdio.h>\ntests/grep/f_2:void func1() {\ntests/grep/f_2:void func2() {\n", "", 0);
}

#[test]
fn test_muptiple_pattern_files_multiple_input_files_count() {
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
fn test_muptiple_pattern_files_multiple_input_files_files_with_matches() {
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
fn test_muptiple_pattern_files_multiple_input_files_quiet() {
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
fn test_muptiple_pattern_files_multiple_input_files_line_number() {
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
