//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_u8, TestPlan, TestPlanU8};
use std::fs;
use std::path::PathBuf;

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/paste");
    path.push(filename);
    path
}

fn run_paste_test(args: Vec<&str>, expected_output_filename: &str) {
    let expected_output_file_path = get_test_file_path(expected_output_filename);

    let expected_out = fs::read_to_string(expected_output_file_path).unwrap();

    let args = args.into_iter().map(ToOwned::to_owned).collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out,
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn paste_default_behavior() {
    run_paste_test(
        vec!["tests/paste/input1.txt", "tests/paste/input2.txt"],
        "output_default.txt",
    );
}

#[test]
fn paste_serial_mode() {
    run_paste_test(
        vec!["-s", "tests/paste/input1.txt", "tests/paste/input2.txt"],
        "output_serial.txt",
    );
}

#[test]
fn paste_custom_delimiter() {
    run_paste_test(
        vec![
            "-d",
            ",",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
        ],
        "output_custom_delim.txt",
    );
}

#[test]
fn paste_serial_custom_delimiter() {
    run_paste_test(
        vec![
            "-s",
            "-d",
            ",",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
        ],
        "output_serial_custom_delim.txt",
    );
}

// Test case for:
//
// thread 'main' panicked at text/./paste.rs:133:16:
// index out of bounds: the len is 0 but the index is 18446744073709551615
// note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
#[test]
fn paste_simple_multi_line_input() {
    let args = ["-d", "", "-s", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out: "Line 1Line 2Line 3\n".to_owned(),
        expected_err: String::new(),
        stdin_data: "\
Line 1
Line 2
Line 3
"
        .to_owned(),
        expected_exit_code: 0,
    });
}

#[test]
fn paste_multiple_stdin() {
    let args = ["-d", "ABC", "--", "-", "-", "-", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out: "\
Line 1ALine 2BLine 3CLine 4
Line 5ALine 6BLine 7CLine 8
Line 9ABC
"
        .to_owned(),
        expected_err: String::new(),
        stdin_data: "\
Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
"
        .to_owned(),
        expected_exit_code: 0,
    });
}

#[test]
fn paste_multiple_stdin_serial_test_one() {
    let args = ["-d", "ABC", "-s", "--", "-", "-", "-", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out: "\
Line 1ALine 2BLine 3CLine 4ALine 5BLine 6CLine 7ALine 8BLine 9



"
        .to_owned(),
        expected_err: String::new(),
        stdin_data: "\
Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
"
        .to_owned(),
        expected_exit_code: 0,
    });
}

// This implementation was improperly truncating the second "e"
#[test]
fn paste_multiple_stdin_serial_test_two() {
    let args = ["-d", "!", "-s", "--", "-", "-", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out: "\
O!K!1!234!here


"
        .to_owned(),
        expected_err: String::new(),
        stdin_data: "\
O
K
1
234
here"
            .to_owned(),
        expected_exit_code: 0,
    });
}

#[test]
fn paste_non_utf8_input() {
    const INPUT: &[u8] = b"String with non-UTF-8 bytes: \xFF\x00\xFF.\n";

    let args = ["--", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    run_test_u8(TestPlanU8 {
        args,
        cmd: "paste".to_owned(),
        expected_err: Vec::<u8>::new(),
        expected_exit_code: 0,
        expected_out: INPUT.to_vec(),
        stdin_data: INPUT.to_vec(),
    });
}

#[test]
fn paste_backslash_zero_delimiter() {
    let args = ["-d", r#"\0\!\0"#, "-s", "--", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    run_test(TestPlan {
        args,
        cmd: "paste".to_owned(),
        expected_err: String::new(),
        expected_exit_code: 0,
        expected_out: "\
AB!CDE!F
"
        .to_owned(),
        stdin_data: "\
A
B
C
D
E
F
"
        .to_owned(),
    });
}

#[test]
fn paste_trailing_unescaped_backslash_delimiter() {
    let args = ["-d", r#"\\\"#, "-s", "--", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    run_test(TestPlan {
        args,
        cmd: "paste".to_owned(),
        expected_err: r#"paste: delimiter list ends with an unescaped backslash: \\\
"#
        .to_owned(),
        expected_exit_code: 1,
        expected_out: String::new(),
        stdin_data: String::new(),
    });
}

#[test]
fn paste_trailing_escaped_backslash_delimiter() {
    let args = ["-d", r#"\\!\@\\"#, "-s", "--", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    run_test(TestPlan {
        args,
        cmd: "paste".to_owned(),
        expected_err: String::new(),
        expected_exit_code: 0,
        expected_out: r#"A\B!C@D\E\F!G@H\I\J
"#
        .to_owned(),
        stdin_data: "\
A
B
C
D
E
F
G
H
I
J
"
        .to_owned(),
    });
}

#[test]
fn paste_custom_delimiters() {
    run_paste_test(
        vec![
            "-d",
            "!@#",
            "--",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
            "tests/paste/input2.txt",
        ],
        "output_paste_custom_delimiters.txt",
    );
}

#[test]
fn paste_custom_delimiters_serial() {
    run_paste_test(
        vec![
            "-d",
            "!@#",
            "-s",
            "--",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
        ],
        "output_paste_custom_delimiters_serial.txt",
    );
}

// ---------------------------------------------------------------------------
// Operands, delimiters and error paths
// ---------------------------------------------------------------------------

fn paste_tmp(tag: &str, content: &str) -> std::path::PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("posixutils-paste-{}-{}", std::process::id(), tag));
    std::fs::write(&p, content).expect("write temp file");
    p
}

#[test]
fn paste_parallel_unequal_file_lengths() {
    // The shorter file contributes empty fields once exhausted; the delimiter
    // is still written.
    let a = paste_tmp("u1", "1\n2\n3\n");
    let b = paste_tmp("u2", "a\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_paste"))
        .args([a.to_str().unwrap(), b.to_str().unwrap()])
        .output()
        .expect("run paste");
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(String::from_utf8_lossy(&out.stdout), "1\ta\n2\t\n3\t\n");
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn paste_newline_delimiter() {
    let a = paste_tmp("n1", "1\n2\n");
    let b = paste_tmp("n2", "a\nb\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_paste"))
        .args(["-d", "\\n", a.to_str().unwrap(), b.to_str().unwrap()])
        .output()
        .expect("run paste");
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(String::from_utf8_lossy(&out.stdout), "1\na\n2\nb\n");
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn paste_null_then_ordinary_delimiter() {
    // `\0` is the empty delimiter; a character after it continues the cycling
    // list. POSIX leaves the combination unspecified, so this pins what we do.
    let a = paste_tmp("z1", "1\n2\n3\n");
    let b = paste_tmp("z2", "a\nb\nc\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_paste"))
        .args(["-d", "\\0x", a.to_str().unwrap(), b.to_str().unwrap()])
        .output()
        .expect("run paste");
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(String::from_utf8_lossy(&out.stdout), "1a\n2b\n3c\n");
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn paste_serial_mode_with_an_unopenable_file() {
    let good = paste_tmp("s1", "1\n2\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_paste"))
        .args(["-s", "no-such-file-xyz", good.to_str().unwrap()])
        .output()
        .expect("run paste");
    assert_eq!(out.status.code(), Some(1), "a missing operand must exit >0");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("no-such-file-xyz"), "got {stderr:?}");
    // The readable operand is still processed.
    assert_eq!(String::from_utf8_lossy(&out.stdout), "1\t2\n");
    let _ = std::fs::remove_file(good);
}

#[test]
fn paste_empty_file_operand_in_serial_mode() {
    let empty = paste_tmp("e1", "");
    let full = paste_tmp("e2", "a\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_paste"))
        .args(["-s", empty.to_str().unwrap(), full.to_str().unwrap()])
        .output()
        .expect("run paste");
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(String::from_utf8_lossy(&out.stdout), "\na\n");
    let _ = std::fs::remove_file(empty);
    let _ = std::fs::remove_file(full);
}

#[test]
fn paste_mixes_stdin_with_a_named_file() {
    use std::io::Write;
    let f = paste_tmp("m1", "a\n");
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_paste"))
        .args(["-", f.to_str().unwrap()])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("spawn paste");
    child.stdin.as_mut().unwrap().write_all(b"S\n").unwrap();
    let out = child.wait_with_output().expect("wait paste");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "S\ta\n");
    let _ = std::fs::remove_file(f);
}

#[test]
fn paste_many_dash_operands() {
    // Each `-` consumes the next line of the single standard input.
    use std::io::Write;
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_paste"))
        .args(["-", "-", "-", "-"])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("spawn paste");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b"a\nb\nc\nd\n")
        .unwrap();
    let out = child.wait_with_output().expect("wait paste");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "a\tb\tc\td\n");
}

#[test]
fn paste_multibyte_delimiter_under_lc_ctype() {
    // The delimiter list is a list of *characters*, so a multi-byte delimiter
    // is written whole rather than a byte at a time.
    let Some(loc) = plib::testing::utf8_locale() else {
        return;
    };
    let a = paste_tmp("mb1", "1\n2\n");
    let b = paste_tmp("mb2", "a\nb\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_paste"))
        .args(["-d", "\u{e9}", a.to_str().unwrap(), b.to_str().unwrap()])
        .env("LC_ALL", &loc)
        .output()
        .expect("run paste");
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(out.stdout, "1\u{e9}a\n2\u{e9}b\n".as_bytes());
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}
