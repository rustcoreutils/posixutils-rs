//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::Output;

use plib::testing::{run_test_with_checker, run_test_with_checker_and_env, TestPlan};

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

fn run_test_more_with_env(
    args: &[&str],
    stdin_data: &str,
    expected_out: &str,
    expected_err: &str,
    expected_exit_code: i32,
    env_vars: &[(&str, &str)],
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("more"),
            args: str_args,
            stdin_data: String::from(stdin_data),
            expected_out: String::from(expected_out),
            expected_err: String::from(expected_err),
            expected_exit_code,
        },
        env_vars,
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

// POSIX: empty stdin is valid input, not an error. `cat /dev/null | more` must
// exit cleanly. (Earlier code returned MoreError::InputRead when
// `lines().next()` yielded None on empty stdin; the implicit-stdin path now
// uses `read_to_string`, which returns Ok on EOF.)
#[test]
fn test_0_files_empty_stdin() {
    run_test_more(&["-p", "\":n\""], "", "", "", 0);
}

// POSIX regression: with no file operands, all of stdin must be paginated,
// not just the first line. Earlier code used
// `BufReader::new(stdin).lines().next()`, so `printf 'a\nb\nc\n' | more`
// displayed only "a" and exited.  In filter mode (stdout is not a tty during
// `cargo test`), the full content must reach stdout.
#[test]
fn test_0_files_multiline_stdin_not_truncated() {
    run_test_more(
        &[],
        "alpha\nbravo\ncharlie\n",
        "alpha\nbravo\ncharlie\n",
        "",
        0,
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

// POSIX compliance tests: MORE environment variable
#[test]
fn test_more_env_variable() {
    // Test that MORE environment variable is parsed and applied
    // Using -s option from MORE env var
    run_test_more_with_env(
        &[
            "--test",
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
        &[("MORE", "-s")],
    );
}

#[test]
fn test_more_env_variable_with_n() {
    // Test that MORE environment variable with -n option is parsed
    run_test_more_with_env(
        &[
            "--test",
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
        &[("MORE", "-n 15")],
    );
}

#[test]
fn test_more_env_variable_combined() {
    // Test that MORE environment variable can combine multiple options
    run_test_more_with_env(
        &[
            "--test",
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
        &[("MORE", "-s -c")],
    );
}

// ---------------------------------------------------------------------------
// Audit regressions
// ---------------------------------------------------------------------------

/// Audit #7 / POSIX CONSEQUENCES OF ERRORS (107643-107646): "If an error is
/// encountered accessing a file when using the :n command, more shall attempt
/// to examine the next file in the argument list, but the final exit status
/// shall be affected."
#[test]
fn test_audit_next_file_error_affects_exit_status() {
    run_test_more(
        &[
            "--test",
            "-p",
            ":n",
            "test_files/README.md",
            "test_files/does_not_exist.md",
        ],
        "",
        "",
        "Couldn't read file 'test_files/does_not_exist.md'\n",
        1,
    );
}

// The success counterpart -- `:n` over readable files still exiting 0 -- is
// already asserted by `test_1_file` and `test_3_files` above; a dedicated
// test cannot be added here because `more --test` blocks in its interactive
// loop whenever a command lands mid-list rather than running past the end.

/// Audit #10/#11 / POSIX 107617-107620: the editor is chosen by the *last
/// pathname component* of EDITOR, and vi/ex are invoked with `-c linenumber`.
#[test]
fn test_audit_invoke_editor_arguments() {
    use std::io::Write as _;
    use std::os::unix::fs::PermissionsExt as _;

    let dir = std::env::temp_dir().join("posixutils_more_audit_editor");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();

    let record = dir.join("args");
    // Named `vi`, but reached through a full path: the basename is what
    // decides whether `-c linenumber` is passed.
    let editor = dir.join("vi");
    let mut script = std::fs::File::create(&editor).unwrap();
    writeln!(script, "#!/bin/sh").unwrap();
    writeln!(script, "printf '%s\\n' \"$@\" > {}", record.display()).unwrap();
    drop(script);
    std::fs::set_permissions(&editor, std::fs::Permissions::from_mode(0o755)).unwrap();

    run_test_more_with_env(
        &["--test", "-p", "vq", "test_files/README.md"],
        "",
        "",
        "",
        0,
        &[("EDITOR", editor.to_str().unwrap())],
    );

    let args = std::fs::read_to_string(&record).unwrap();
    let args: Vec<&str> = args.lines().collect();
    assert_eq!(
        args.first().copied(),
        Some("-c"),
        "vi must be invoked with -c linenumber, got {args:?}"
    );
    assert!(
        args.get(1).is_some_and(|n| n.parse::<usize>().is_ok()),
        "-c must be followed by a line number, got {args:?}"
    );
    assert_eq!(args.get(2).copied(), Some("--"), "got {args:?}");

    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------
// PTY render tests
//
// `more` only takes its interactive render path when standard output is a
// terminal, so these are the only tests that exercise it at all. They pin
// behavior that is already correct, so that changes to the render path show up
// as a clean diff.
// ---------------------------------------------------------------------------

use crate::common::MoreSession;

/// Write `content` to a fresh file under a per-test temporary directory.
fn render_fixture(name: &str, content: &[u8]) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join("posixutils_more_render");
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(name);
    std::fs::write(&path, content).unwrap();
    path
}

#[test]
fn test_pty_renders_plain_lines() {
    let path = render_fixture("plain.txt", b"alpha\nbravo\ncharlie\n");
    let Some(session) = MoreSession::spawn(&[path.to_str().unwrap()], &[], 10, 40) else {
        println!("Skipping PTY test: no pseudo-terminal available");
        return;
    };

    let grid = session.grid();
    assert_eq!(grid[0], "alpha");
    assert_eq!(grid[1], "bravo");
    assert_eq!(grid[2], "charlie");

    assert_eq!(session.quit(), Some(0));
}

#[test]
fn test_pty_folds_without_losing_content() {
    // A line longer than the display is folded, not truncated (POSIX
    // 107427-107429). The fold *column* is deliberately not asserted here:
    // the current parser gives back up to three columns per folded line (the
    // `buffer_str` backoff in `find_next_line_len_with_skip`), so a 20-column
    // terminal folds at 17. That is a defect, tracked separately; what must
    // hold either way is that no byte is dropped.
    const LINE: &str = "abcdefghijklmnopqrstuvwxyz0123";
    let path = render_fixture("fold.txt", format!("{LINE}\n").as_bytes());
    let Some(session) = MoreSession::spawn(&[path.to_str().unwrap()], &[], 10, 20) else {
        println!("Skipping PTY test: no pseudo-terminal available");
        return;
    };

    let grid = session.grid();
    assert!(
        LINE.starts_with(&grid[0]),
        "first row must be a prefix of the line, got {:?}",
        grid[0]
    );
    assert!(!grid[0].is_empty() && grid[0].len() <= 20);
    assert_eq!(
        format!("{}{}", grid[0], grid[1]),
        LINE,
        "folding must not drop content"
    );

    assert_eq!(session.quit(), Some(0));
}

#[test]
fn test_pty_scroll_forward_one_line() {
    let path = render_fixture("scroll.txt", b"l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8\n");
    let Some(mut session) = MoreSession::spawn(&[path.to_str().unwrap()], &[], 4, 20) else {
        println!("Skipping PTY test: no pseudo-terminal available");
        return;
    };

    assert_eq!(session.row(0), "l1");
    session.keys("j");
    assert_eq!(session.row(0), "l2");

    assert_eq!(session.quit(), Some(0));
}
