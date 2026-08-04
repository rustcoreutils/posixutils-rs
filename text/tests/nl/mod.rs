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
fn test_nl_pbre_is_basic_regex() {
    // The pBRE numbering style uses a Basic RE: `+` is a literal character
    // (not a quantifier), so only the line containing "a+" is numbered.
    run_test(TestPlan {
        cmd: String::from("nl"),
        args: vec!["-b".to_string(), "pa+".to_string()],
        stdin_data: String::from("a+b\nxyz\n"),
        expected_out: String::from("     1\ta+b\n       xyz\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_nl_pbre_interval() {
    // BRE interval \{3\}: number only lines containing three consecutive a's.
    run_test(TestPlan {
        cmd: String::from("nl"),
        args: vec!["-b".to_string(), r"pa\{3\}".to_string()],
        stdin_data: String::from("aaa\nbb\naaaa\n"),
        expected_out: String::from("     1\taaa\n       bb\n     2\taaaa\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// ---------------------------------------------------------------------------
// Error paths
// ---------------------------------------------------------------------------

#[test]
fn test_nl_nonexistent_file_reports_and_exits_nonzero() {
    // This exited 1 with *nothing* on stderr: `main` matched `Err(_)` and threw
    // the error away.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_nl"))
        .arg("no-such-file-xyz")
        .output()
        .expect("run nl");
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.starts_with("nl:"),
        "the diagnostic must name the utility, got {stderr:?}"
    );
    assert!(
        stderr.contains("no-such-file-xyz"),
        "the diagnostic must name the file, got {stderr:?}"
    );
}

#[test]
fn test_nl_rejects_an_over_long_section_delimiter() {
    // A >2-character -d argument was rejected silently.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_nl"))
        .args(["-d", "abc"])
        .output()
        .expect("run nl");
    assert_eq!(out.status.code(), Some(1));
    assert!(
        String::from_utf8_lossy(&out.stderr).starts_with("nl:"),
        "got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
}

// ---------------------------------------------------------------------------
// Option coverage
// ---------------------------------------------------------------------------

#[test]
fn test_nl_header_and_footer_with_pbre() {
    // -h/-f take the same style argument as -b, including the pBRE form.
    // The header section is delimited by the delimiter three times over.
    nl_test(
        &["-h", "pH", "-b", "a"],
        "\\:\\:\\:\nHDR\n\\:\\:\nBODY\n",
        "\n     1\tHDR\n\n     1\tBODY\n",
    );
    nl_test(&["-f", "pF", "-b", "a"], "\\:\nFOOT\n", "\n     1\tFOOT\n");
}

#[test]
fn test_nl_join_blank_lines_groups_them() {
    // -l N counts N consecutive blank lines as one numbered line.
    nl_test(
        &["-b", "a", "-l", "2"],
        "a\n\n\n\nb\n",
        "     1\ta\n       \n     2\t\n       \n     3\tb\n",
    );
}

#[test]
fn test_nl_numbering_restarts_at_each_logical_page() {
    // Each `\:\:` body delimiter starts a new logical page, so numbering
    // restarts unless -p is given.
    nl_test(&["-b", "a"], "a\n\\:\\:\nb\n", "     1\ta\n\n     1\tb\n");
    nl_test(
        &["-b", "a", "-p"],
        "a\n\\:\\:\nb\n",
        "     1\ta\n\n     2\tb\n",
    );
}

#[test]
fn test_nl_width_and_multi_character_separator() {
    nl_test(&["-b", "a", "-w", "10"], "x\n", "         1\tx\n");
    nl_test(&["-b", "a", "-s", "::"], "x\n", "     1::x\n");
    nl_test(&["-b", "a", "-w", "3", "-s", " | "], "x\n", "  1 | x\n");
}

#[test]
fn test_nl_negative_start_and_zero_increment() {
    // -v takes any integer; a `-5` argument needs `=` so it is not read as an
    // option. -i 0 leaves every line with the same number.
    nl_test(&["-b", "a", "-v=-5"], "a\n", "    -5\ta\n");
    nl_test(&["-b", "a", "-i", "0"], "a\nb\n", "     1\ta\n     1\tb\n");
}

#[test]
fn test_nl_line_number_overflow_is_diagnosed() {
    // The counter is i64; running past its maximum must be reported rather
    // than wrapping silently.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_nl"))
        .args(["-b", "a", "-v", "9223372036854775806", "-i", "4"])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .and_then(|mut c| {
            use std::io::Write;
            c.stdin.as_mut().unwrap().write_all(b"a\nb\n")?;
            c.wait_with_output()
        })
        .expect("run nl");
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("overflow"), "got {stderr:?}");
}
