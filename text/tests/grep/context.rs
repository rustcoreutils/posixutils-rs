//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU context output: `-A NUM`, `-B NUM`, `-C NUM` and `-NUM`.  Context lines are marked with
//! `-` where a selected line has `:`, and `--` separates groups that do not touch.  gzip's
//! zgrep tests run `grep -15`.

use super::grep_test;

/// The lines `1` to `20`.
fn numbers() -> String {
    (1..=20).map(|n| format!("{n}\n")).collect()
}

#[test]
fn test_grep_after_and_before_context() {
    grep_test(&["-A1", "5"], &numbers(), "5\n6\n--\n15\n16\n", "", 0);
    grep_test(
        &["-B2", "-n", "1[05]"],
        &numbers(),
        "8-8\n9-9\n10:10\n--\n13-13\n14-14\n15:15\n",
        "",
        0,
    );
    grep_test(
        &["--after-context=1", "--before-context", "1", "^1[05]$"],
        &numbers(),
        "9\n10\n11\n--\n14\n15\n16\n",
        "",
        0,
    );
}

/// Groups that overlap or touch are joined; -A and -B win over -C whatever their order.
#[test]
fn test_grep_context() {
    grep_test(
        &["-C1", "-e", "3", "-e", "5"],
        &numbers(),
        "2\n3\n4\n5\n6\n--\n12\n13\n14\n15\n16\n",
        "",
        0,
    );
    grep_test(
        &["--context=2", "^1[05]$"],
        &numbers(),
        "8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n",
        "",
        0,
    );
    grep_test(&["-A1", "-B0", "-C3", "^9$"], &numbers(), "9\n10\n", "", 0);
    // A context of 0 still separates the groups.
    for opt in ["-C0", "-A0"] {
        grep_test(
            &[opt, "-e", "3", "-e", "5"],
            &numbers(),
            "3\n--\n5\n--\n13\n--\n15\n",
            "",
            0,
        );
    }
}

/// `-NUM` is `-C NUM`; a `-NUM` that is an option's argument is not.
#[test]
fn test_grep_numeric_context() {
    let tail: String = (5..=20).map(|n| format!("{n}\n")).collect();
    grep_test(&["-15", "^20$"], &numbers(), &tail, "", 0);
    grep_test(
        &["-2", "^1[05]$"],
        &numbers(),
        "8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n",
        "",
        0,
    );
    // Digits in a cluster of options are -NUM too.
    grep_test(&["-n1", "^3$"], &numbers(), "2-2\n3:3\n4-4\n", "", 0);
    grep_test(&["-e", "-1", "-A1"], &numbers(), "", "", 1);
    grep_test(&["-ve", "-1", "-A1"], "-1\n", "", "", 1);
    grep_test(&["-A1", "--", "-1"], "a-1\nb\nc\n", "a-1\nb\n", "", 0);
}

/// Context is a matter of output lines only: -c, -l and -q are unchanged, and with -v the
/// context lines are the selected-against ones.
#[test]
fn test_grep_context_other_modes() {
    grep_test(&["-c", "-A1", "5"], &numbers(), "2\n", "", 0);
    grep_test(&["-1", "-c", "5"], &numbers(), "2\n", "", 0);
    grep_test(&["-q", "-C3", "5"], &numbers(), "", "", 0);
    grep_test(
        &["-v", "-A1", "[02-9]"],
        &numbers(),
        "1\n2\n--\n11\n12\n",
        "",
        0,
    );
}

/// Across inputs, a group in a later file is separated from the last, and names its file with
/// `-` on context lines.
#[test]
fn test_grep_context_files() {
    let tmp = plib::tmp::tempdir().unwrap();
    let m = tmp.path().join("m");
    std::fs::write(&m, "x\ny\nx\nz\n").unwrap();
    let m = m.to_str().unwrap();
    grep_test(
        &["-1", "x", m, m],
        "",
        &format!("{m}:x\n{m}-y\n{m}:x\n{m}-z\n--\n{m}:x\n{m}-y\n{m}:x\n{m}-z\n"),
        "",
        0,
    );
    grep_test(
        &["-h", "-n", "-B1", "z", m, m],
        "",
        "3-x\n4:z\n--\n3-x\n4:z\n",
        "",
        0,
    );
}
