//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

fn pathchk_test(args: &[&str], stderr: &str, expected_code: i32) {
    run_test(TestPlan {
        cmd: String::from("pathchk"),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: stderr.to_string(),
        expected_exit_code: expected_code,
    });
}

// #P1: an existing path must pass the default (file-system) checks.
#[test]
fn pathchk_existing_path() {
    pathchk_test(&["/"], "", 0);
    pathchk_test(&["/etc"], "", 0);
}

// #P2: a creatable relative pathname must pass (its parent is the cwd).
#[test]
fn pathchk_creatable_relative() {
    pathchk_test(&["pathchk_nonexistent_file_xyz"], "", 0);
}

// #P3: -p and -P must be usable together.
#[test]
fn pathchk_p_and_pcap_combined() {
    pathchk_test(&["-p", "-P", "foo"], "", 0);
    pathchk_test(&["-p", "-P", "a/b/c"], "", 0);
}

// #P4: -p uses the portable filename character set, not is_ascii().
#[test]
fn pathchk_portable_charset() {
    pathchk_test(
        &["-p", "a b"],
        "pathchk: a b: component contains non-portable characters\n",
        1,
    );
    pathchk_test(
        &["-p", "a*b"],
        "pathchk: a*b: component contains non-portable characters\n",
        1,
    );
    // Portable characters pass.
    pathchk_test(&["-p", "a_b.c-d"], "", 0);
}

// -P: a component beginning with '-' is a diagnostic.
#[test]
fn pathchk_leading_hyphen_component() {
    pathchk_test(
        &["-P", "--", "-foo"],
        "pathchk: -foo: a component begins with '-'\n",
        1,
    );
    pathchk_test(
        &["-P", "--", "dir/-bar"],
        "pathchk: dir/-bar: a component begins with '-'\n",
        1,
    );
}

// -P: an empty pathname is a diagnostic.
#[test]
fn pathchk_empty_pathname() {
    pathchk_test(&["-P", ""], "pathchk: : empty pathname\n", 1);
}

// #P6: a component longer than {_POSIX_NAME_MAX} (14) fails under -p.
#[test]
fn pathchk_component_too_long() {
    let long_comp = "a".repeat(20);
    pathchk_test(
        &["-p", &long_comp],
        &format!("pathchk: {}: component too long\n", long_comp),
        1,
    );
}

// #P6: a pathname longer than {_POSIX_PATH_MAX} (256) fails under -p.
#[test]
fn pathchk_pathname_too_long() {
    let long_path = "aaa/".repeat(70); // 280 bytes, each component within NAME_MAX
    pathchk_test(
        &["-p", &long_path],
        &format!("pathchk: {}: pathname too long\n", long_path),
        1,
    );
}

// Multiple operands: the exit status reflects any failure, all are reported.
#[test]
fn pathchk_multiple_operands() {
    pathchk_test(
        &["-p", "good", "a b"],
        "pathchk: a b: component contains non-portable characters\n",
        1,
    );
}
