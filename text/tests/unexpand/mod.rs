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

fn unexpand_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("unexpand"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn unexpand_test_1() {
    // Input has no trailing newline; one is not fabricated (matches GNU).
    unexpand_test(
        &["-t", "4,8,12"],
        "    Apple\n        Banana\n            Cherry\n                Date",
        "\tApple\n\t\tBanana\n\t\t\tCherry\n\t\t\t    Date",
    );
}

#[test]
fn unexpand_test_2() {
    // Default 8-column stops repeat, so 16 leading spaces become two tabs
    // (previously the stop list did not repeat). No trailing newline added.
    unexpand_test(
        &["-"],
        "    Apple\n        Banana\n            Cherry\n                Date",
        "    Apple\n\tBanana\n\t    Cherry\n\t\tDate",
    );
}

#[test]
fn unexpand_test_3() {
    unexpand_test(
        &["-t", "8"],
        "        leading spaces\n",
        "\tleading spaces\n",
    );
}

#[test]
fn unexpand_test_4() {
    unexpand_test(&["-t", "4"], "    leading spaces\n", "\tleading spaces\n");
}

#[test]
fn unexpand_test_5() {
    // -t implies -a (POSIX), so the interior 4-space run from column 4 reaches
    // the stop at column 8 and becomes a tab.
    unexpand_test(&["-t", "8"], "text    with spaces\n", "text\twith spaces\n");
}

#[test]
fn unexpand_test_6() {
    // -a converts runs of 2+ spaces from their actual column position: the
    // 8-space run after "text" (columns 4..12) becomes a tab to column 8 plus
    // four spaces. No trailing newline added.
    unexpand_test(
        &["-a"],
        "text        with                spaces",
        "text\t    with\t\tspaces",
    );
}

#[test]
fn unexpand_dash_non_sole_operand() {
    // "-" reads stdin at its position; a second "-" sees EOF. Interior spaces
    // are preserved by default (only leading blanks are converted).
    unexpand_test(&["-", "-"], "x y\n", "x y\n");
}

#[test]
fn unexpand_rejects_zero_tabsize() {
    run_test(plib::testing::TestPlan {
        cmd: String::from("unexpand"),
        args: vec!["-t".to_string(), "0".to_string()],
        stdin_data: String::from("        x\n"),
        expected_out: String::new(),
        expected_err: String::from("tab size must be a positive integer\n"),
        expected_exit_code: 1,
    });
}

#[test]
fn unexpand_blank_separated_tablist() {
    // A blank-separated list is accepted (POSIX allows comma or blank).
    // 16 leading spaces with stops at 8 and 16 become two tabs.
    unexpand_test(&["-t", "8 16"], "                Q\n", "\t\tQ\n");
}

#[test]
fn unexpand_default_does_not_convert_interior() {
    // Without -a/-t, only leading blanks convert; interior runs are preserved.
    unexpand_test(&[], "        a        b\n", "\ta        b\n");
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

#[test]
fn unexpand_multibyte_column_width() {
    // Two 2-column CJK characters precede an 8-space run. With -a the run
    // begins at column 4 and reaches the stop at column 8 (a tab + 4 spaces),
    // proving column tracking uses display width, not byte length. Skipped
    // where no UTF-8 locale is available.
    let Some(loc) = utf8_locale() else {
        return;
    };
    use std::io::Write;
    use std::process::{Command, Stdio};
    let mut child = Command::new(env!("CARGO_BIN_EXE_unexpand"))
        .arg("-a")
        .env("LC_ALL", &loc)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    // 世界 = 4 columns, then 8 spaces, then "x".
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all("世界        x\n".as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap().stdout;
    assert_eq!(out, "世界\t    x\n".as_bytes());
}
