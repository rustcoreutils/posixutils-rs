//
// Copyright (c) 2024-2026 Jeff Garzik
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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
        &["-f", "text", "-", "5", "{2}"],
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
        "44\n77\n19\n8\n",
    );
    std::fs::remove_file("text00").unwrap();
    std::fs::remove_file("text01").unwrap();
    std::fs::remove_file("text02").unwrap();
    std::fs::remove_file("text03").unwrap();
}

#[test]
fn test_csplit_text_by_lines_from_file() {
    csplit_test(
        &["-f", "text_f", "tests/assets/test_file.txt", "5", "{2}"],
        "",
        "44\n77\n19\n8\n",
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
        "60\n54\n54\n54\n0\n",
    );
    std::fs::remove_file("code_c00").unwrap();
    std::fs::remove_file("code_c01").unwrap();
    std::fs::remove_file("code_c02").unwrap();
    std::fs::remove_file("code_c03").unwrap();
    std::fs::remove_file("code_c04").unwrap();
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
        "13\n48\n54\n54\n53\n",
    );
    std::fs::remove_file("code_c_neg00").unwrap();
    std::fs::remove_file("code_c_neg01").unwrap();
    std::fs::remove_file("code_c_neg02").unwrap();
    std::fs::remove_file("code_c_neg03").unwrap();
    std::fs::remove_file("code_c_neg04").unwrap();
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
    std::fs::remove_file("code_c_s04").unwrap();
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
        "60\n54\n54\n54\n0\n",
    );
    std::fs::remove_file("code_c_n000").unwrap();
    std::fs::remove_file("code_c_n001").unwrap();
    std::fs::remove_file("code_c_n002").unwrap();
    std::fs::remove_file("code_c_n003").unwrap();
    std::fs::remove_file("code_c_n004").unwrap();
}

#[test]
fn test_csplit_regex_by_empty_lines() {
    csplit_test(
        &["-f", "empty_lines", "tests/assets/empty_line.txt", "/^$/"],
        "",
        "7\n7\n",
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
        "7\n11\n9\n9\n",
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
        "4\n11\n9\n12\n",
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
        "8\n11\n9\n8\n",
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
        "2\n4\n4\n1\n",
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

// Test -k option: files should be kept on error.
//
// POSIX OPERANDS: "An error shall be reported if an operand does not reference a
// line between the current position and the end of the file." Line 999 does not
// exist in a 4-line input, so this run fails -- but -k means the two files it
// did create must survive. Verified against GNU coreutils 9.4, which likewise
// prints both byte counts, diagnoses the operand, exits 1, and keeps the files.
#[test]
fn test_csplit_keep_files_on_error() {
    let str_args: Vec<String> = ["-k", "-f", "keep_err", "-", "3", "999"]
        .iter()
        .map(|s| String::from(*s))
        .collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("csplit"),
            args: str_args,
            stdin_data: String::from("line1\nline2\nline3\nline4\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(1),
                "out-of-range operand must fail"
            );
            assert_eq!(String::from_utf8_lossy(&output.stdout), "12\n12\n");
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("999") && stderr.contains("line number out of range"),
                "diagnostic should name the operand: {:?}",
                stderr
            );
        },
    );

    // With -k, all created files should exist
    assert!(std::path::Path::new("keep_err00").exists());
    assert!(std::path::Path::new("keep_err01").exists());
    std::fs::remove_file("keep_err00").unwrap();
    std::fs::remove_file("keep_err01").unwrap();
}

// The same run without -k must remove the files it created.
#[test]
fn test_csplit_removes_files_on_range_error() {
    let str_args: Vec<String> = ["-f", "rm_err", "-", "3", "999"]
        .iter()
        .map(|s| String::from(*s))
        .collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("csplit"),
            args: str_args,
            stdin_data: String::from("line1\nline2\nline3\nline4\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            assert_eq!(output.status.code(), Some(1));
        },
    );

    assert!(!std::path::Path::new("rm_err00").exists());
    assert!(!std::path::Path::new("rm_err01").exists());
}

// `{*}` is a GNU extension whose two operand kinds behave differently, verified
// against GNU coreutils 9.4: a pattern simply stops matching (success), while a
// line number eventually names a line past the end (error).
#[test]
fn test_csplit_star_repeat_semantics() {
    // Pattern + {*}: runs out of matches cleanly.
    csplit_test(
        &["-f", "star_rx", "-", "/^$/", "{*}"],
        "a\n\nb\n\nc\n",
        "2\n3\n3\n",
    );
    for i in 0..3 {
        let _ = std::fs::remove_file(format!("star_rx0{}", i));
    }

    // Line number + {*}: errors once the next multiple is past EOF.
    let str_args: Vec<String> = ["-k", "-f", "star_ln", "-", "2", "{*}"]
        .iter()
        .map(|s| String::from(*s))
        .collect();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("csplit"),
            args: str_args,
            stdin_data: String::from("1\n2\n3\n4\n5\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            assert_eq!(output.status.code(), Some(1));
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("line number out of range"),
                "got {:?}",
                stderr
            );
        },
    );
    for i in 0..6 {
        let _ = std::fs::remove_file(format!("star_ln0{}", i));
    }
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
        "7\n12\n",
    );
    std::fs::remove_file("bre_test00").unwrap();
    std::fs::remove_file("bre_test01").unwrap();
}

// ============================================================================
// ASYNCHRONOUS EVENTS (audit #4)
//
// POSIX csplit: "If the -k option is specified, created files shall be
// retained. Otherwise, the default action occurs." The -k clause only means
// anything if the default is to remove them.
// ============================================================================

/// Run csplit against a slow stdin, interrupt it once some output files exist,
/// and report which of them survived.
fn csplit_interrupted(prefix: &str, extra: &[&str]) -> Vec<String> {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let bin = plib::testing::get_binary_path("csplit");
    let mut args: Vec<String> = extra.iter().map(|s| s.to_string()).collect();
    args.extend([
        "-f".into(),
        prefix.into(),
        "-".into(),
        "2".into(),
        "{*}".into(),
    ]);

    let mut child = Command::new(bin)
        .args(&args)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn csplit");

    // Feed enough lines to create several output files, then hold the pipe open
    // so csplit is still running when the signal arrives.
    {
        let stdin = child.stdin.as_mut().expect("stdin");
        for i in 0..8 {
            writeln!(stdin, "line{}", i).unwrap();
        }
        stdin.flush().unwrap();
    }
    std::thread::sleep(std::time::Duration::from_millis(400));

    // `kill` rather than libc: this crate has no libc dev-dependency.
    let _ = Command::new("kill")
        .args(["-INT", &child.id().to_string()])
        .status();
    let _ = child.wait();
    std::thread::sleep(std::time::Duration::from_millis(200));

    let mut survivors = Vec::new();
    for i in 0..16 {
        let name = format!("{}{:02}", prefix, i);
        if std::path::Path::new(&name).exists() {
            survivors.push(name);
        }
    }
    survivors
}

#[test]
fn test_csplit_signal_removes_created_files() {
    let survivors = csplit_interrupted("sigrm", &[]);
    for name in &survivors {
        let _ = std::fs::remove_file(name);
    }
    assert!(
        survivors.is_empty(),
        "created files must be removed when interrupted without -k, found: {:?}",
        survivors
    );
}

#[test]
fn test_csplit_signal_keeps_created_files_with_k() {
    let survivors = csplit_interrupted("sigkeep", &["-k"]);
    let found = !survivors.is_empty();
    for name in &survivors {
        let _ = std::fs::remove_file(name);
    }
    assert!(
        found,
        "-k must retain created files when interrupted (POSIX ASYNCHRONOUS EVENTS)"
    );
}

// ============================================================================
// Regression coverage for behaviours the audit listed as untested
// ============================================================================

/// Audit #1/#2: multiple bare `line_no` operands are absolute positions, and a
/// bare line number fires exactly once. Verified against GNU coreutils 9.4.
#[test]
fn test_csplit_multiple_bare_line_numbers() {
    csplit_test(
        &["-f", "multiln", "-", "5", "10"],
        "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\nl10\nl11\nl12\n",
        "12\n15\n12\n",
    );
    for i in 0..3 {
        std::fs::remove_file(format!("multiln0{}", i)).unwrap();
    }
}

/// Audit #6: `\/` inside a `/rexp/` operand is a literal delimiter, not the end
/// of the pattern (POSIX OPERANDS: "The application shall use the sequence
/// \"\\/\" to specify a <slash> character within the rexp").
#[test]
fn test_csplit_escaped_delimiter_in_rexp() {
    csplit_test(
        &["-f", "escdel", "-", r"/proc\/sys/"],
        "aa\nproc/sys\nbb\n",
        "3\n12\n",
    );
    std::fs::remove_file("escdel00").unwrap();
    std::fs::remove_file("escdel01").unwrap();
}

/// Audit #3: the byte count printed for each file must equal the bytes actually
/// written, i.e. what `wc -c` would report.
#[test]
fn test_csplit_byte_counts_match_file_sizes() {
    csplit_test(&["-f", "bytecnt", "-", "3"], "aa\nbb\ncc\ndd\n", "6\n6\n");

    for name in ["bytecnt00", "bytecnt01"] {
        let len = std::fs::metadata(name).unwrap().len();
        assert_eq!(len, 6, "{} should be 6 bytes on disk", name);
        std::fs::remove_file(name).unwrap();
    }
}
