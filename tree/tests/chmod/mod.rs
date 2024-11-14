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

// TODO: isolate libc::umask
// Port of coreutils/tests/chmod/equals.sh
#[test]
fn test_chmod_equals() {
    let test_dir = &format!("{}/test_chmod_equals", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    let categories = ["u", "g", "o"];
    let expected_perms = [0o700, 0o070, 0o007];

    for src in categories {
        for (dest, expected) in categories.iter().copied().zip(expected_perms) {
            if src != dest {
                let opts = format!("a=,{src}=rwx,{dest}={src},{src}=");
                chmod_test(&[&opts, f], "", "", 0);

                let perm = fs::metadata(f).unwrap().permissions();
                let actual = perm.mode() & !libc::S_IFMT; // Mask out the file type
                assert_eq!(actual, expected);
            }
        }
    }

    // let m = unsafe { libc::umask(0o27) };

    // chmod_test(&["a=,u=rwx,=u", f], "", "", 0);

    // let perm = fs::metadata(f).unwrap().permissions();
    // let actual = perm.mode() & !libc::S_IFMT;
    // assert_eq!(actual, 0o750);

    // // Revert umask
    // unsafe {
    //     libc::umask(m);
    // }

    fs::remove_dir_all(test_dir).unwrap();
}
