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

use std::os::unix::fs::symlink;
use std::path::PathBuf;

fn fresh_dir(name: &str) -> PathBuf {
    let base = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(name);
    let _ = std::fs::remove_dir_all(&base);
    std::fs::create_dir_all(&base).unwrap();
    base
}

// #R1/#R2: default mode and -E must resolve symbolic-link components.
#[test]
fn realpath_resolves_symlink() {
    let base = fresh_dir("realpath_resolves_symlink");
    std::fs::create_dir(base.join("real")).unwrap();
    symlink("real", base.join("link")).unwrap();

    let expected = format!(
        "{}\n",
        std::fs::canonicalize(base.join("real"))
            .unwrap()
            .to_str()
            .unwrap()
    );
    let link = base.join("link");
    let link = link.to_str().unwrap();

    realpath_test(&[link], &expected, "", 0);
    realpath_test(&["-E", link], &expected, "", 0);

    let _ = std::fs::remove_dir_all(&base);
}

// #R2: -E expands a symlink to a missing target whose parent exists.
#[test]
fn realpath_e_dangling_with_existing_parent() {
    let base = fresh_dir("realpath_e_dangling_ok");
    let target = base.join("nofile");
    symlink(&target, base.join("dangling")).unwrap();

    let cbase = std::fs::canonicalize(&base).unwrap();
    let expected = format!("{}\n", cbase.join("nofile").to_str().unwrap());
    let dangling = base.join("dangling");

    realpath_test(&["-E", dangling.to_str().unwrap()], &expected, "", 0);

    let _ = std::fs::remove_dir_all(&base);
}

// #R2: -E errors when the expanded target's own parent does not exist.
#[test]
fn realpath_e_dangling_missing_parent_errors() {
    let base = fresh_dir("realpath_e_dangling_err");
    symlink(base.join("nofile/foo"), base.join("danglemd")).unwrap();

    let danglemd = base.join("danglemd");
    realpath_test(&["-E", "-q", danglemd.to_str().unwrap()], "", "", 1);

    let _ = std::fs::remove_dir_all(&base);
}

// #R2: -E with a trailing slash on a regular file is "Not a directory".
#[test]
fn realpath_e_trailing_slash_on_file_errors() {
    let base = fresh_dir("realpath_e_trailing_slash");
    std::fs::write(base.join("regfile"), b"x").unwrap();

    let p = format!("{}/", base.join("regfile").to_str().unwrap());
    realpath_test(&["-E", "-q", &p], "", "", 1);

    let _ = std::fs::remove_dir_all(&base);
}

// #R9: an embedded newline in the resolved path is treated as an error.
#[test]
fn realpath_newline_is_error() {
    let base = fresh_dir("realpath_newline");
    let nl_dir = base.join("a\nb");
    std::fs::create_dir(&nl_dir).unwrap();

    realpath_test(
        &[nl_dir.to_str().unwrap()],
        "",
        "realpath: result contains a newline character\n",
        1,
    );

    let _ = std::fs::remove_dir_all(&base);
}
