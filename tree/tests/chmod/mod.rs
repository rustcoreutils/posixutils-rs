//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use std::{
    fs,
    os::unix::{self, fs::PermissionsExt},
};

fn chmod_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("chmod"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

// Port of coreutils/tests/chmod/ignore-symlink.sh
#[test]
fn test_chmod_ignore_symlink() {
    let test_dir = &format!("{}/test_chmod_ignore_symlink", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");
    let l = &format!("{test_dir}/l");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();
    unix::fs::symlink(f, l).unwrap();

    chmod_test(&["u+w", "-R", test_dir], "", "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/chmod/inaccessible.sh
#[test]
fn test_chmod_inaccessible() {
    let test_dir = &format!("{}/test_chmod_inaccessible", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let d_e = &format!("{test_dir}/d/e");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir_all(d_e).unwrap();

    chmod_test(&["0", d_e, d], "", "", 0);
    chmod_test(&["u+rwx", d, d_e], "", "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/chmod/no-x.sh
#[test]
fn test_chmod_no_x() {
    let test_dir = &format!("{}/test_chmod_no_x", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let a_b = &format!("{test_dir}/a/b");
    let d = &format!("{test_dir}/d");
    let d_no_x_y = &format!("{test_dir}/d/no-x/y");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir_all(a_b).unwrap();
    fs::create_dir_all(d_no_x_y).unwrap();

    chmod_test(&["u=rw", d_no_x_y], "", "", 0);
    chmod_test(
        &["-R", "o=r", d],
        "",
        &format!("chmod: cannot access '{d_no_x_y}': Permission denied\n"),
        1,
    );

    chmod_test(
        &["a-x", a, a_b],
        "",
        &format!("chmod: cannot access '{a_b}': Permission denied\n"),
        1,
    );

    // Reset permission so it can be deleted
    fs::set_permissions(a, fs::Permissions::from_mode(0o777)).unwrap();

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/chmod/octal.sh
#[test]
fn test_chmod_octal() {
    let test_dir = &format!("{}/test_chmod_octal", env!("CARGO_TARGET_TMPDIR"));

    fs::create_dir(test_dir).unwrap();

    for perm in ["0-anything", "7-anything", "8"] {
        chmod_test(
            &[perm, test_dir],
            "",
            &format!("chmod: invalid mode: '{perm}'\n"),
            1,
        );
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/chmod/symlinks.sh
//
// Excluding non-standard options --L, -P, --no-dereference and --dereference
#[test]
fn test_chmod_symlinks() {
    let test_dir = &format!("{}/test_chmod_symlinks", env!("CARGO_TARGET_TMPDIR"));
    let a_b = &format!("{test_dir}/a/b");
    let a_dangle = &format!("{test_dir}/a/dangle");
    let a_dirlink = &format!("{test_dir}/a/dirlink");
    let a_b_file = &format!("{test_dir}/a/b/file");
    let a_c = &format!("{test_dir}/a/c");
    let a_c_file = &format!("{test_dir}/a/c/file");
    let a_c_link = &format!("{test_dir}/a/c/link");

    fs::create_dir(test_dir).unwrap();

    fs::create_dir_all(a_b).unwrap();
    fs::create_dir_all(a_c).unwrap();
    fs::File::create(a_b_file).unwrap();
    fs::File::create(a_c_file).unwrap();

    unix::fs::symlink("foo", a_dangle).unwrap();
    unix::fs::symlink("../b/file", a_c_link).unwrap();
    unix::fs::symlink("b", a_dirlink).unwrap();

    let reset_modes = || {
        chmod_test(&["777", a_b, a_c, a_b_file, a_c_file], "", "", 0);
    };

    fn count_755(files: &[&str], expected: usize) {
        let count = files
            .iter()
            .filter(|file| {
                let mode = fs::metadata(file).unwrap().permissions().mode();
                (mode & 0o777) == 0o755
            })
            .count();
        assert_eq!(count, expected)
    }

    reset_modes();

    chmod_test(&["755", "-R", a_c], "", "", 0);

    count_755(&[a_c, a_c_file, a_b_file], 2);

    reset_modes();

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/chmod/thru-dangling.sh
#[test]
fn test_chmod_thru_dangling() {
    let test_dir = &format!("{}/test_chmod_thru_dangling", env!("CARGO_TARGET_TMPDIR"));
    let non_existent = &format!("{test_dir}/non-existent");
    let dangle = &format!("{test_dir}/dangle");

    fs::create_dir(test_dir).unwrap();
    unix::fs::symlink(non_existent, dangle).unwrap();

    chmod_test(
        &["644", dangle],
        "",
        &format!("chmod: cannot operate on dangling symlink '{dangle}'\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}
