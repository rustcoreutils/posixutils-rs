//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `c` command: delete the pattern space and start the next cycle; with
//! no address or one, write the text each time, and with a range write it
//! once, at the range's last line.  Expectations are GNU sed 4.9's output.

use super::sed_test;

const INPUT: &str = "a\nb\nc\nd\n";

// The next line is a cycle of its own: commands before `c` still run on it.
#[test]
fn next_line_runs_the_whole_script() {
    sed_test(
        &["-e", "s/^/>/", "-e", "2c\\X"],
        INPUT,
        ">a\nX\n>c\n>d\n",
        "",
        0,
    );
    sed_test(
        &["-e", "2c\\X", "-e", "s/^/>/"],
        INPUT,
        ">a\nX\n>c\n>d\n",
        "",
        0,
    );
}

// Every line `!` selects is changed.
#[test]
fn negated_address() {
    sed_test(&["2!c\\X"], INPUT, "X\nb\nX\nX\n", "", 0);
    sed_test(&["/b/,/c/!c\\X"], INPUT, "X\nb\nc\nX\n", "", 0);
}

// A range ending at `$` writes the text once, at the last line.
#[test]
fn range_to_last_line() {
    sed_test(&["/b/,$c\\X"], INPUT, "a\nX\n", "", 0);
    sed_test(&["/b/,$c\\X"], "a\nb\nc\nd", "a\nX\n", "", 0);
}

// The text is written even under -n.
#[test]
fn quiet() {
    sed_test(&["-n", "2,3c\\X"], INPUT, "X\n", "", 0);
}

// A range that never ends writes nothing.
#[test]
fn unended_range() {
    sed_test(&["/b/,/nomatch/c\\X"], INPUT, "a\n", "", 0);
}
