//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::Output;

use plib_testing::{run_test_with_checker, TestPlan};

fn test_checker_more(plan: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains(&plan.expected_out));

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(stderr, *plan.expected_err);

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

fn run_test_more(
    args: &[&str],
    stdin_data: &str,
    expected_out: &str,
    expected_err: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("more"),
            args: str_args,
            stdin_data: String::from(stdin_data),
            expected_out: String::from(expected_out),
            expected_err: String::from(expected_err),
            expected_exit_code,
        },
        test_checker_more,
    );
}

// base tests
#[test]
fn test_minus_files() {
    run_test_more(&["-p", "\":n\"", "-"], "ABC", "", "", 0);
}

#[test]
fn test_0_files() {
    run_test_more(&["-p", "\":n\""], "ABC", "", "", 0);
}

#[test]
fn test_0_files_error() {
    run_test_more(
        &["-p", "\":n\""],
        "",
        "Couldn't read from stdin\n",
        "Couldn't read from stdin\n",
        1,
    );
}

#[test]
fn test_1_file() {
    run_test_more(
        &["--test", "-p", "\":n\"", "test_files/README.md"],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_3_files() {
    run_test_more(
        &[
            "--test",
            "-p",
            ":n:n:n",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_styled_text() {
    run_test_more(
        &["--test", "-p", "jjjjjjjjjjj:n", "test_files/styled.txt"],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_styled_text_1() {
    run_test_more(
        &[
            "--test",
            "-p",
            ":n:njjjjjjjjjjj:n",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_styled_text_2() {
    run_test_more(
        &[
            "--test",
            "-p",
            ":n:nG:n",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_styled_text_3() {
    run_test_more(
        &["--test", "-p", "jjjjjjjjjjj:n", "test_files/styled1.txt"],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_styled_text_4() {
    run_test_more(
        &[
            "--test",
            "-p",
            ":n:njjjjjjjjjjj:n",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled1.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_styled_text_5() {
    run_test_more(
        &[
            "--test",
            "-p",
            ":n:nG:n",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled1.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

// commands tests
#[test]
fn test_help() {
    run_test_more(
        &["--test", "-p", "\"h:n:n\"", "test_files/README.md"],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_scroll_forward_screenful() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"f\x06f\x06f\x06 \"",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_scroll_backward_screenful() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"f\x06f\x06b\x02b\x02b\x02:n\"",
            "test_files/README.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_scroll_forward_one_line() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\" j\n j\n j\n j\n:n j\n j\n\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_scroll_backward_one_line() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"jjjjjkkkkkkkkk:n kkkjjjj\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_scroll_forward_halfscreen() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"d\x04d\x04d\x04:nd\x04\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_scroll_backward_halfscreen() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"d\x04d\x04d\x04u\x15u\x15u\x15u\x15:nu\x15d\x04\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_skip_lines() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"ssssssssssss:n sssss\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_goto_beggining() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"        g:nGg:n   \"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_goto_eof() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"G G G  \"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_refresh() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"r\x0Cr\x0Cr\x0Cr\x0C:nr\x0Cr\x0C:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_discard() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"RRRRRR:nRRRRRRRR:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_mark() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"maffmbffmcff:nmaffmbffmcff:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_goto_mark() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"ma'a:nma'a:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_goto_mark_error() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"'a:q\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "Couldn't find mark for 'a\n",
        "Couldn't find mark for 'a\n",
        1,
    );
}

#[test]
fn test_return_to_last() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"''fff'''':n''ffff'''':n\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_search_forward() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"15/goal\n :n15/test\n:gn \"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_search_backward() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"Gk?goal\n :nG?test\n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_search_forward_error() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"15/\\<sdfsdfsfewcwiu,lxnsb\\>\n :n \"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "Couldn't find '\\<sdfsdfsfewcwiu,lxnsb\\>' pattern\n",
        "Couldn't find '\\<sdfsdfsfewcwiu,lxnsb\\>' pattern\n",
        1,
    );
}

#[test]
fn test_search_backward_error() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"Gk?\\<sdfsdfsfewcwiu,lxnsb\\>\n :n \"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "Couldn't find '\\<sdfsdfsfewcwiu,lxnsb\\>' pattern\n",
        "Couldn't find '\\<sdfsdfsfewcwiu,lxnsb\\>' pattern\n",
        1,
    );
}

#[test]
fn test_search_repeat() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"/goal\n:n:n \"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_search_repeat_1() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"g/goal\n:nG15?test\ngn:n \"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_scroll_file() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\":p:n:p:n:p:n:n:p:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_examine_new_file() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\":e test_files/README.md\n q\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_examine_new_file_error() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\":e \n q\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "Couldn't read file \'\'\n",
        "Couldn't read file \'\'\n",
        1,
    );
}

#[test]
fn test_tag() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\":t SeekPositions\n q \"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_quit() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"\x03f\x04f\x1Cfq\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );

    run_test_more(
        &[
            "--test",
            "-p",
            "\":q\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );

    run_test_more(
        &[
            "--test",
            "-p",
            "\"ZZ\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_unknown_error() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\"aaaaa\"",
            "test_files/README.md",
            "test_files/TODO.md",
        ],
        ":n ",
        "Couldn't execute unknown command\n",
        "Couldn't execute unknown command\n",
        1,
    );
}

// with flags tests
#[test]
fn test_c() {
    run_test_more(
        &[
            "--test",
            "-c",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_e() {
    run_test_more(
        &[
            "--test",
            "-e",
            "-p",
            "\":n:n:n:nj\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_i() {
    run_test_more(
        &[
            "--test",
            "-i",
            "-p",
            "\"15/!\\<GOAL\\>\n:n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_n() {
    run_test_more(
        &[
            "--test",
            "-n",
            "18",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_s() {
    run_test_more(
        &[
            "--test",
            "-s",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_u() {
    run_test_more(
        &[
            "--test",
            "-u",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

// tag tests
#[test]
fn test_tag_1() {
    run_test_more(
        &[
            "--test",
            "-t",
            "SeekPositions",
            "-p",
            "\":t SeekPositions\n :n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_tag_2() {
    run_test_more(
        &[
            "--test",
            "-t",
            "SeekPositions",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_tag_3() {
    run_test_more(
        &[
            "--test",
            "-t",
            "[0123456789]",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "Couldn't find '[0123456789]' pattern\n",
        1,
    );
}

#[test]
fn test_tag_4() {
    run_test_more(
        &[
            "--test",
            "-t",
            "setlocale",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "Couldn't find 'setlocale' pattern\n",
        1,
    );
}

#[test]
fn test_tag_5() {
    run_test_more(
        &[
            "--test",
            "-t",
            "^struct",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "Couldn't find '^struct' pattern\n",
        1,
    );
}

#[test]
fn test_tag_6() {
    run_test_more(
        &[
            "--test",
            "-t",
            "\\<let\\>",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "Couldn't find '\\<let\\>' pattern\n",
        1,
    );
}

#[test]
fn test_tag_empty_error() {
    run_test_more(
        &[
            "--test",
            "-t",
            "",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "Couldn't read file ''\n",
        "Couldn't read file ''\n",
        1,
    );
}

#[test]
fn test_tag_not_found_error() {
    run_test_more(
        &[
            "--test",
            "-t",
            "\\<dflbowvwvwvdfbfd\\>",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "Couldn't find '\\<dflbowvwvwvdfbfd\\>' pattern\n",
        1,
    );
}

#[test]
fn test_command_tag_empty_error() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\":t \n :n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "Couldn't read file ''\n",
        1,
    );
}

#[test]
fn test_command_tag_not_found_error() {
    run_test_more(
        &[
            "--test",
            "-p",
            "\":t \\<dflbodfbfd\\>\n :n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "Couldn't find '\\<dflbodfbfd\\>' pattern\n",
        1,
    );
}

// tags with flags tests
#[test]
fn test_c_tag() {
    run_test_more(
        &[
            "--test",
            "-c",
            "-t",
            "SeekPositions",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_e_tag() {
    run_test_more(
        &[
            "--test",
            "-e",
            "-t",
            "SeekPositions",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_i_tag() {
    run_test_more(
        &[
            "--test",
            "-i",
            "-t",
            "SeekPositions",
            "-p",
            "\"15/!\\<GOAL\\>\n:n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_n_tag() {
    run_test_more(
        &[
            "--test",
            "-n",
            "18",
            "-t",
            "SeekPositions",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_s_tag() {
    run_test_more(
        &[
            "--test",
            "-s",
            "-t",
            "SeekPositions",
            "-p",
            "\"ffffffffffff:n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_u_tag() {
    run_test_more(
        &[
            "--test",
            "-u",
            "-t",
            "SeekPositions",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

// complex_tests
#[test]
fn test_flags_tag() {
    run_test_more(
        &[
            "--test",
            "-ceisu",
            "-t",
            "SeekPositions",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}

#[test]
fn test_flags_n_tag() {
    run_test_more(
        &[
            "--test",
            "-ceisu",
            "-n",
            "18",
            "-t",
            "SeekPositions",
            "-p",
            "\":n:n:n:n:n\"",
            "test_files/README.md",
            "test_files/TODO.md",
            "test_files/styled.txt",
        ],
        ":n ",
        "",
        "",
        0,
    );
}
