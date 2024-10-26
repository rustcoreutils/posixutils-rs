//
// Copyright (c) 2024 Ian McLinden
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

fn realpath_test(args: &[&str], stdout: &str, stderr: &str, expected_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("realpath"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: stdout.to_string(),
        expected_err: stderr.to_string(),
        expected_exit_code: expected_code,
    });
}

#[test]
fn realpath_current_directory() {
    let curr_dir = std::env::current_dir().unwrap();
    let out_str = &format!("{}\n", curr_dir.to_str().unwrap());

    realpath_test(&["."], out_str, "", 0);
    realpath_test(&["./"], out_str, "", 0);
    realpath_test(&["./."], out_str, "", 0);
}

#[test]
fn realpath_parent_directory() {
    let curr_dir = std::env::current_dir().unwrap();
    let parent_dir = curr_dir.parent().unwrap();
    let out_str = format!("{}\n", parent_dir.to_str().unwrap());

    realpath_test(&[".."], &out_str, "", 0);
    realpath_test(&["../"], &out_str, "", 0);
    realpath_test(&["../."], &out_str, "", 0);
    realpath_test(&["./.."], &out_str, "", 0);
    realpath_test(&["./../"], &out_str, "", 0);
    realpath_test(&["./../."], &out_str, "", 0);
}

#[test]
fn realpath_endless_parent() {
    realpath_test(
        &["../../../../../../../../../../../../../../../../../../../.."],
        "/\n",
        "",
        0,
    );
    realpath_test(
        &["../../../../../../../../../../../../../../../../../../../../"],
        "/\n",
        "",
        0,
    );
    realpath_test(
        &["../../../../../../../../../../../../../../../../../../../../."],
        "/\n",
        "",
        0,
    );
    realpath_test(
        &["./../../../../../../../../../../../../../../../../../../../.."],
        "/\n",
        "",
        0,
    );
    realpath_test(
        &["./../../../../../../../../../../../../../../../../../../../../"],
        "/\n",
        "",
        0,
    );
    realpath_test(
        &["./../../../../../../../../../../../../../../../../../../../../."],
        "/\n",
        "",
        0,
    );
}

#[test]
fn realpath_relative_directory() {
    let curr_dir = std::env::current_dir().unwrap();
    let curr_dir_name = curr_dir.file_name().unwrap();
    let out_str = &format!("{}\n", curr_dir.to_str().unwrap());

    let in_str = &format!("../{}", curr_dir_name.to_str().unwrap());
    realpath_test(&[in_str], out_str, "", 0);

    let in_str = &format!("./../{}", curr_dir_name.to_str().unwrap());
    realpath_test(&[in_str], out_str, "", 0);

    let in_str = &format!(
        "{}/../{}",
        curr_dir.to_str().unwrap(),
        curr_dir_name.to_str().unwrap()
    );
    realpath_test(&[in_str], out_str, "", 0);
}

#[test]
fn realpath_multiple_args() {
    let curr_dir = std::env::current_dir().unwrap();
    let parent_dir = curr_dir.parent().unwrap();
    let out_str = &format!(
        "{}\n{}\n/\n",
        curr_dir.to_str().unwrap(),
        parent_dir.to_str().unwrap()
    );
    realpath_test(&[".", "..", "/"], out_str, "", 0);
}

#[test]
fn realpath_no_args() {
    let curr_dir = std::env::current_dir().unwrap();
    let out_str = format!("{}\n", curr_dir.to_str().unwrap());

    realpath_test(&[], &out_str, "", 0);
}

#[test]
fn realpath_empty_path() {
    realpath_test(
        &[""],
        "",
        "error: a value is required for '[PATH]...' but none was supplied\n\nFor more information, try '--help'.\n",
        2,
    );
}

#[test]
fn realpath_args_canonicalization() {
    let curr_dir = std::env::current_dir().unwrap();
    let out_str = format!("{}/foobar\n", curr_dir.to_str().unwrap());

    // No canonicalization flags is the same as -E
    realpath_test(&["foobar"], &out_str, "", 0);
    realpath_test(&["-E", "foobar"], &out_str, "", 0);

    // Canonicalization flag must be explicitly supplied
    realpath_test(
        &["-e", "foobar"],
        "",
        "realpath: foobar: No such file or directory (os error 2)\n",
        1,
    );

    // POSIX-style overriding (last wins)
    realpath_test(&["-e", "-E", "foobar"], &out_str, "", 0);
    realpath_test(
        &["-E", "-e", "foobar"],
        "",
        "realpath: foobar: No such file or directory (os error 2)\n",
        1,
    );
}

#[test]
fn realpath_args_quiet() {
    realpath_test(&["-e", "-q", "foobar"], "", "", 1);
    realpath_test(&["-e", "--quiet", "foobar"], "", "", 1);
}
