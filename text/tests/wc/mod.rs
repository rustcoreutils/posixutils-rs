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

fn wc_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("wc"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn wc_empty() {
    wc_test(&["-c"], "", "0\n");
    wc_test(&["-l"], "", "0\n");
    wc_test(&["-w"], "", "0\n");
}

#[test]
fn wc_one() {
    wc_test(&["-c"], "x", "1\n");
    wc_test(&["-l"], "x", "0\n");
    wc_test(&["-w"], "x", "1\n");
}

#[test]
fn wc_two() {
    wc_test(&["-c"], "x y\n", "4\n");
    wc_test(&["-l"], "x y\n", "1\n");
    wc_test(&["-w"], "x y\n", "2\n");
}

#[test]
fn wc_dash_operand_reads_stdin() {
    // A "-" operand reads standard input rather than a file named "-", and the
    // operand name ("-") is shown (only no-operand stdin omits the name).
    wc_test(&["-l", "-"], "a\nb\n", "2 -\n");
}

#[test]
fn wc_single_file_shows_name() {
    // A single named file operand prints the filename (POSIX), not just counts.
    use std::io::Write;
    let dir = std::env::temp_dir();
    let path = dir.join("wc_single_file_shows_name.txt");
    std::fs::File::create(&path)
        .unwrap()
        .write_all(b"hello world\nfoo\n")
        .unwrap();
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_wc"))
        .arg("-c")
        .arg(&path)
        .output()
        .unwrap();
    let _ = std::fs::remove_file(&path);
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        format!("16 {}\n", path.display())
    );
}

fn utf8_locale() -> Option<String> {
    let avail = std::process::Command::new("locale")
        .arg("-a")
        .output()
        .ok()?;
    let list = String::from_utf8_lossy(&avail.stdout).to_lowercase();
    for name in ["C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"] {
        if list.contains(&name.to_lowercase()) {
            return Some(name.to_string());
        }
    }
    None
}

fn wc_locale(args: &[&str], input: &[u8], locale: &str) -> String {
    use std::io::Write;
    use std::process::{Command, Stdio};
    let mut child = Command::new(env!("CARGO_BIN_EXE_wc"))
        .args(args)
        .env("LC_ALL", locale)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.as_mut().unwrap().write_all(input).unwrap();
    String::from_utf8_lossy(&child.wait_with_output().unwrap().stdout).to_string()
}

#[test]
fn wc_chars_locale_aware() {
    // -m counts characters under LC_CTYPE: 世界 is two characters plus newline.
    let Some(loc) = utf8_locale() else {
        return;
    };
    assert_eq!(wc_locale(&["-m"], "世界\n".as_bytes(), &loc), "3\n");
}

#[test]
fn wc_words_locale_whitespace() {
    // U+3000 (ideographic space) is whitespace in a UTF-8 locale, so it splits
    // words — an ASCII-only whitespace table would miss it.
    let Some(loc) = utf8_locale() else {
        return;
    };
    assert_eq!(wc_locale(&["-w"], "a\u{3000}b\n".as_bytes(), &loc), "2\n");
}
