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

use plib::testing::utf8_locale;

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

// ---------------------------------------------------------------------------
// Operands, `--` and error paths
// ---------------------------------------------------------------------------

fn wc_run(args: &[&str]) -> (String, String, i32) {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_wc"))
        .args(args)
        .output()
        .expect("failed to run wc");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

fn wc_tmp(name: &str, content: &str) -> std::path::PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("posixutils-wc-{}-{}", std::process::id(), name));
    std::fs::write(&p, content).expect("write temp file");
    p
}

#[test]
fn wc_default_output_is_lines_words_bytes() {
    // With no options the order is <newline> <word> <byte>, per POSIX STDOUT
    // (the same order as `-l -w -c`), and the count columns precede the name.
    let f = wc_tmp("default", "a b\nc\n");
    let (stdout, _, code) = wc_run(&[f.to_str().unwrap()]);
    assert_eq!(code, 0);
    assert_eq!(stdout, format!("2 3 6 {}\n", f.display()), "got {stdout:?}");
    let _ = std::fs::remove_file(f);
}

#[test]
fn wc_multiple_files_report_each_and_a_total() {
    let a = wc_tmp("m1", "x\n");
    let b = wc_tmp("m2", "y\nz\n");
    let (stdout, _, code) = wc_run(&["-l", a.to_str().unwrap(), b.to_str().unwrap()]);
    assert_eq!(code, 0);
    assert_eq!(
        stdout,
        format!("1 {}\n2 {}\n3 total\n", a.display(), b.display()),
        "got {stdout:?}"
    );
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn wc_combined_flags_keep_column_order() {
    // Selected counts always print in <newline> <word> <byte> order regardless
    // of the order the options were given in.
    let f = wc_tmp("combo", "a b\nc\n");
    let p = f.to_str().unwrap();
    let (lw, _, _) = wc_run(&["-l", "-w", p]);
    assert_eq!(lw, format!("2 3 {}\n", f.display()), "got {lw:?}");
    let (wl, _, _) = wc_run(&["-w", "-l", p]);
    assert_eq!(wl, lw, "option order must not change the column order");
    let (lc, _, _) = wc_run(&["-l", "-c", p]);
    assert_eq!(lc, format!("2 6 {}\n", f.display()), "got {lc:?}");
    let _ = std::fs::remove_file(f);
}

#[test]
fn wc_nonexistent_file_errors_and_exits_nonzero() {
    let (_, stderr, code) = wc_run(&["-l", "no-such-file-xyz"]);
    assert_ne!(code, 0, "a missing operand must exit non-zero");
    assert!(
        stderr.contains("no-such-file-xyz"),
        "the diagnostic must name the file, got {stderr:?}"
    );
}

#[test]
fn wc_double_dash_ends_options() {
    let f = wc_tmp("-dashname", "one\n");
    let (stdout, _, code) = wc_run(&["-l", "--", f.to_str().unwrap()]);
    assert_eq!(code, 0);
    assert_eq!(stdout, format!("1 {}\n", f.display()), "got {stdout:?}");
    let _ = std::fs::remove_file(f);
}
