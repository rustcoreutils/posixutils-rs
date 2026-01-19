//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// Casts are necessary for cross-platform compatibility (libc types differ by platform)
#![allow(clippy::unnecessary_cast)]

//! The use of `libc::umask` in these tests interferes with the permissions of newly created files
//! in `tree-tests.rs`. They are located here in `tree-tests-umask.rs` so that they can have a
//! different per-process umask than those in `tree-tests.rs`.

use plib::testing::{run_test, TestPlan};
use std::{
    fs,
    os::unix::fs::{DirBuilderExt, MetadataExt, PermissionsExt},
    path::Path,
    sync::Mutex,
};

static UMASK_SETTER: Mutex<UmaskSetter> = Mutex::new(UmaskSetter);

// Used to serialize changes to the process' umask
struct UmaskSetter;

impl UmaskSetter {
    fn umask(&self, mask: libc::mode_t) -> libc::mode_t {
        let original = unsafe { libc::umask(mask) };

        // Pessimistically makes sure that the umask is applied before
        // continuing execution
        unsafe { while libc::umask(mask) != mask {} }

        original
    }
}

fn base_test(
    cmd: &str,
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from(cmd),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn cp_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    base_test(
        "cp",
        args,
        expected_output,
        expected_error,
        expected_exit_code,
    )
}

fn rm_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    base_test(
        "rm",
        args,
        expected_output,
        expected_error,
        expected_exit_code,
    )
}

fn chmod_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    base_test(
        "chmod",
        args,
        expected_output,
        expected_error,
        expected_exit_code,
    )
}

// Port of coreutils/tests/cp/existing-perm-dir.sh
#[test]
fn test_cp_existing_perm_dir() {
    let test_dir = &format!("{}/test_cp_existing_perm_dir", env!("CARGO_TARGET_TMPDIR"));
    let src = &format!("{test_dir}/src");
    let dst = &format!("{test_dir}/dst");
    let src_dir = &format!("{test_dir}/src/dir");
    let dst_dir = &format!("{test_dir}/dst/dir");

    fs::create_dir(test_dir).unwrap();

    let umask_setter = UMASK_SETTER.lock().unwrap();
    let original_umask = umask_setter.umask(0o002);

    fs::DirBuilder::new()
        .mode(0o775)
        .recursive(true)
        .create(src_dir)
        .unwrap();
    fs::DirBuilder::new()
        .mode(0o700)
        .recursive(true)
        .create(dst_dir)
        .unwrap();

    cp_test(&["-R", &format!("{src}/."), &format!("{dst}/")], "", "", 0);

    let mode = fs::metadata(dst_dir).unwrap().mode();
    assert_eq!(mode & 0o777, 0o700); // Should be drwx-----

    umask_setter.umask(original_umask);
    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/cp/perm.sh
// Not including the `mv` tests.
#[test]
fn test_cp_perm() {
    let test_dir = &format!("{}/test_cp_perm", env!("CARGO_TARGET_TMPDIR"));
    let src = &format!("{test_dir}/src");
    let dest = &format!("{test_dir}/dest");

    fs::create_dir(test_dir).unwrap();

    let args_combination: Vec<Vec<&str>> = vec![
        vec![src, dest],
        vec!["-p", src, dest],
        vec!["-f", src, dest],
        vec!["-p", "-f", src, dest],
    ];

    let umask_setter = UMASK_SETTER.lock().unwrap();
    let original_umask = umask_setter.umask(0o31);

    for mask in [0o31, 0o37, 0o2] {
        umask_setter.umask(mask);
        for args in &args_combination {
            for existing_dest in [true, false] {
                for g_perm in 1..=7 {
                    for o_perm in 1..=7 {
                        // u=r,g=rx,o
                        let src_mode = 0o450;
                        // u=rw,g=$g_perm,o=$o_perm
                        let dest_mode = 0o600 | g_perm << 3 | o_perm;

                        fs::File::create(src).unwrap();
                        fs::set_permissions(src, fs::Permissions::from_mode(src_mode)).unwrap();

                        if existing_dest {
                            fs::File::create(dest).unwrap();
                            fs::set_permissions(dest, fs::Permissions::from_mode(dest_mode))
                                .unwrap();
                        }

                        cp_test(args, "", "", 0);
                        assert!(Path::new(src).exists());

                        if !args.contains(&"-p") {
                            let mode = fs::metadata(dest).unwrap().mode();
                            if existing_dest {
                                assert_eq!(mode & 0o777, dest_mode);
                            } else {
                                // Directly inspect the mode bits instead of
                                // going through `stat` and `sed` like in the
                                // original
                                if mask == 0o37 {
                                    // sed 's/.....$/-----/'
                                    // Converts `rwxrwxrwx` to `rwxr-----`
                                    assert_eq!(mode & 0o777, src_mode & 0b111100000);
                                } else if mask == 0o31 {
                                    // sed 's/..\(..\).$/--\1-/'
                                    // Converts `rwxrwxrwx` to `rwxr--rw-`
                                    assert_eq!(mode & 0o777, src_mode & 0b111100110);
                                }
                            }
                        }

                        // Cleanup for the next loop
                        fs::set_permissions(src, fs::Permissions::from_mode(0o777)).unwrap();
                        fs::remove_file(src).unwrap();
                        fs::set_permissions(dest, fs::Permissions::from_mode(0o777)).unwrap();
                        fs::remove_file(dest).unwrap();
                    }
                }
            }
        }
    }

    umask_setter.umask(original_umask);
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/deep-1.sh
#[test]
fn test_rm_deep_1() {
    let test_dir = &format!("{}/test_rm_deep_1", env!("CARGO_TARGET_TMPDIR"));
    let k20 = ["/k"; 20].join("");
    let k200 = [k20.as_str(); 10].join("");
    let t = &format!("{test_dir}/t");
    let k_deep = &format!("{t}{k200}{k200}");

    fs::create_dir(test_dir).unwrap();

    let umask_setter = UMASK_SETTER.lock().unwrap();
    let original_umask = umask_setter.umask(0o002);

    fs::create_dir_all(k_deep).unwrap();

    rm_test(&["-r", t], "", "", 0);

    assert!(!Path::new(t).exists());

    umask_setter.umask(original_umask);
    fs::remove_dir_all(test_dir).unwrap();
}

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
                let actual = perm.mode() & !(libc::S_IFMT as u32); // Mask out the file type
                assert_eq!(actual, expected);
            }
        }
    }

    let umask_setter = UMASK_SETTER.lock().unwrap();
    let original_umask = umask_setter.umask(0o27);

    chmod_test(&["a=,u=rwx,=u", f], "", "", 0);

    let perm = fs::metadata(f).unwrap().permissions();
    let actual = perm.mode() & !(libc::S_IFMT as u32);
    assert_eq!(actual, 0o750);

    // Revert umask
    umask_setter.umask(original_umask);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/chmod/equal-x.sh
#[test]
fn test_chmod_equal_x() {
    let test_dir = &format!("{}/test_chmod_equal_x", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    let umask_setter = UMASK_SETTER.lock().unwrap();
    let original_umask = umask_setter.umask(0o005);

    let expected_mode = 0o110;

    for mode in ["=x", "=xX", "=Xx", "=x,=X", "=X,=x"] {
        let opt = format!("a=r,{mode}");
        chmod_test(&[&opt, f], "", "", 0);

        let permissions = fs::metadata(f).unwrap().permissions();
        assert_eq!(permissions.mode() & 0o777, expected_mode,);
    }

    // Revert umask
    umask_setter.umask(original_umask);

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/chmod/setgid.sh
//
// Mixing of symbolic and octal mode operands are not supported in the standard:
// "For an octal integer mode operand, the file mode bits shall be set absolutely."
#[test]
fn test_chmod_setgid() {
    let test_dir = &format!("{}/test_chmod_setgid", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");

    fs::create_dir(test_dir).unwrap();

    let umask_setter = UMASK_SETTER.lock().unwrap();
    let original_umask = umask_setter.umask(0);

    fs::DirBuilder::new().mode(0o755).create(d).unwrap();

    chmod_test(&["g+s", d], "", "", 0);

    for mode in ["+", "-", "g-s", "00755", "000755", "755", "0755"] {
        chmod_test(&[mode, d], "", "", 0);

        let expected_mode = match mode {
            "g-s" | "00755" | "000755" => 0o755,
            _ => 0o2755,
        };

        let permissions = fs::metadata(d).unwrap().permissions();

        assert_eq!(permissions.mode() & 0o7777, expected_mode, "mode: {mode}");

        chmod_test(&["2755", d], "", "", 0);
    }

    umask_setter.umask(original_umask);

    fs::remove_dir_all(test_dir).unwrap();
}
