//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! How the command line names the script: `-e` and `-f` in any of the forms
//! XBD 12.2 allows, in the order given.

use plib::testing::{run_test, TempFile, TestPlan};

fn sed_ok(args: &[&str], input: &str, output: &str) {
    run_test(TestPlan {
        cmd: String::from("sed"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: String::from(input),
        expected_out: String::from(output),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// `-e` may end a cluster of flags, as in the `sed -ne p` of countless build
// scripts. The script sources used to be found by a second scan of argv for
// the words "-e" and "-f" alone, which missed this form and the next.
#[test]
fn e_ends_a_cluster() {
    sed_ok(&["-ne", "p"], "a\n", "a\n");
}

#[test]
fn e_with_attached_script() {
    sed_ok(&["-n", "-es/a/X/p"], "a\nb\n", "X\n");
}

#[test]
fn e_and_f_keep_their_order() {
    let script = TempFile::new("script.sed", "s/b/c/\n");
    let f_arg = format!("-f{}", script.path().display());
    sed_ok(&["-e", "s/a/b/", &f_arg, "-e", "s/c/d/"], "a\n", "d\n");
}
