//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

fn run_test_helper(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("cmp"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

#[test]
fn cmp_same() {
    let mut files = vec![String::from("tests/cmp/lorem_ipsum.txt")];
    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];
    for i in indices {
        files.push(format!("tests/cmp/lorem_ipsum_{i}.txt"));
    }

    for file in &files {
        run_test_helper(&[file, file], "", "", 0);
    }
}

#[test]
fn cmp_different() {
    let original = "tests/cmp/lorem_ipsum.txt";

    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];
    let bytes = [1, 46, 91, 136, 181, 226, 271, 316, 361, 406, 451];
    let lines = [1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 7];

    for i in 0..indices.len() {
        let modified = format!("tests/cmp/lorem_ipsum_{}.txt", indices[i]);
        run_test_helper(
            &[original, &modified],
            &format!(
                "{original} {modified} differ: char {}, line {}\n",
                bytes[i], lines[i]
            ),
            "",
            1,
        );
    }
}

#[test]
fn cmp_different_silent() {
    let original = "tests/cmp/lorem_ipsum.txt";

    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];

    for index in indices {
        let modified = format!("tests/cmp/lorem_ipsum_{}.txt", index);
        run_test_helper(&["-s", original, &modified], "", "", 1);
    }
}

#[test]
fn cmp_different_less_verbose() {
    let original = "tests/cmp/lorem_ipsum.txt";

    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];
    let bytes = [1, 46, 91, 136, 181, 226, 271, 316, 361, 406, 451];
    let chars_original = ['L', 's', ' ', ' ', 'a', 'o', 'r', ' ', 'a', ' ', '.'];

    for i in 0..indices.len() {
        let modified = format!("tests/cmp/lorem_ipsum_{}.txt", indices[i]);
        run_test_helper(
            &["-l", original, &modified],
            &format!("{} {:o} {:o}\n", bytes[i], chars_original[i] as u8, b'?'),
            "",
            1,
        );
    }
}

// CMP-1: -s must suppress the "EOF on" diagnostic too (stdout AND stderr).
#[test]
fn cmp_eof_silent() {
    let original = "tests/cmp/lorem_ipsum.txt";
    let truncated = "tests/cmp/lorem_ipsum_trunc.txt";
    run_test_helper(&["-s", original, truncated], "", "", 1);
}

#[test]
fn cmp_eof() {
    let original = "tests/cmp/lorem_ipsum.txt";
    let truncated = "tests/cmp/lorem_ipsum_trunc.txt";

    // Status code must be 1. From the specification:
    //
    // "...this includes the case where one file is identical to the first part
    // of the other."
    run_test_helper(
        &[original, truncated],
        "",
        &format!("cmp: EOF on {truncated}\n"),
        1,
    );
}

// GNU's `-n count` and skip operands, which util-linux's mkswap test runs:
// `cmp -n "$offset" "$img.offset" /dev/zero` and
// `cmp "$img" "$img.offset" 0 "$offset"`.

/// Two scratch files holding `a` and `b`, and their paths as strings.
fn two_files(a: &[u8], b: &[u8]) -> (plib::tmp::TempDir, String, String) {
    let dir = plib::tmp::tempdir().unwrap();
    let (p1, p2) = (dir.path().join("f1"), dir.path().join("f2"));
    std::fs::write(&p1, a).unwrap();
    std::fs::write(&p2, b).unwrap();
    let s = |p: std::path::PathBuf| p.to_str().unwrap().to_string();
    (dir, s(p1), s(p2))
}

#[test]
fn cmp_n_limits_the_bytes_compared() {
    let (_dir, f1, f2) = two_files(b"abcdef", b"abcdeZ");
    run_test_helper(&["-n", "5", &f1, &f2], "", "", 0);
    run_test_helper(
        &["-n", "6", &f1, &f2],
        &format!("{f1} {f2} differ: char 6, line 1\n"),
        "",
        1,
    );
    // Nothing compared is no difference, an empty file's EOF included.
    let (_dir, e1, e2) = two_files(b"", b"x");
    run_test_helper(&["-n", "0", &e1, &e2], "", "", 0);
}

/// mkswap's check that the first bytes are zeros.
#[test]
fn cmp_n_against_dev_zero() {
    let (_dir, f1, _f2) = two_files(&[0u8; 100], b"");
    run_test_helper(&["-n", "100", &f1, "/dev/zero"], "", "", 0);
}

/// Skip operands: bytes and lines are counted from the first byte compared.
#[test]
fn cmp_skips_operands() {
    let (_dir, f1, f2) = two_files(b"..abc\nd", b"xxxxabc\nZ");
    run_test_helper(
        &[&f1, &f2, "2", "4"],
        &format!("{f1} {f2} differ: char 5, line 2\n"),
        "",
        1,
    );
    run_test_helper(&["-l", &f1, &f2, "2", "4"], "5 144 132\n", "", 1);
    // One skip operand skips the first file only.
    let (_dir, g1, g2) = two_files(b"..abc", b"abc");
    run_test_helper(&[&g1, &g2, "2"], "", "", 0);
    // With -n, after the skips.
    run_test_helper(&["-n", "3", &f1, &f2, "2", "4"], "", "", 0);
}

#[test]
fn cmp_rejects_a_bad_skip() {
    let (_dir, f1, f2) = two_files(b"a", b"a");
    let out = std::process::Command::new(plib::testing::get_binary_path("cmp"))
        .args([&f1, &f2, "x"])
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(2), "{out:?}");
    assert!(out.stdout.is_empty(), "{out:?}");
}
