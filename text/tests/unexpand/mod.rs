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
        // Prefixed with the utility name since unexpand joined the tree's
        // shared diagnostic path.
        expected_err: String::from("unexpand: tab size must be a positive integer\n"),
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

use plib::testing::utf8_locale;

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

// ---------------------------------------------------------------------------
// Error paths
// ---------------------------------------------------------------------------

#[test]
fn unexpand_nonexistent_file_errors_naming_the_utility() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_unexpand"))
        .arg("no-such-file-xyz")
        .output()
        .expect("run unexpand");
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    // This printed a bare "No such file or directory" with no utility name.
    assert!(
        stderr.starts_with("unexpand:"),
        "the diagnostic must name the utility, got {stderr:?}"
    );
}

// ---------------------------------------------------------------------------
// Tab stops and control characters
// ---------------------------------------------------------------------------

#[test]
fn unexpand_default_stops_repeat_every_eight() {
    // Twelve leading blanks: eight become a <tab>, the remaining four stay.
    unexpand_test(&[], "            x\n", "\t    x\n");
    // Sixteen become two tabs.
    unexpand_test(&[], "                x\n", "\t\tx\n");
}

#[test]
fn unexpand_single_tabsize_repeats() {
    // A single -t value defines stops at every multiple of that value.
    unexpand_test(&["-t", "4"], "        x\n", "\t\tx\n");
    unexpand_test(&["-t", "2"], "    x\n", "\t\tx\n");
}

#[test]
fn unexpand_tabsize_implies_all_blanks() {
    // POSIX: -t implies -a, so interior blanks are converted too, not just the
    // leading run.
    unexpand_test(&["-t", "4"], "ab      cd      ef\n", "ab\t\tcd\t\tef\n");
}

#[test]
fn unexpand_backspace_in_input() {
    // The <backspace> is copied through; the leading blanks before it still
    // convert.
    unexpand_test(&[], "        \x08x\n", "\t\x08x\n");
}

#[test]
fn unexpand_leading_tab_then_blanks() {
    // A <tab> already at column 0 advances to column 8, so the eight blanks
    // that follow form the next full stop.
    unexpand_test(&[], "\t        x\n", "\t\tx\n");
}
