//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The use of `libc::umask` in these tests interferes with the permissions of
//! newly created files in `integration.rs`. They are located here in
//! `integration2.rs` because umask is a per-process.

use plib::testing::{run_test, TestPlan};
use std::fs;
use std::os::unix::fs::{DirBuilderExt, MetadataExt, PermissionsExt};
use std::path::Path;
use std::sync::Mutex;

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

fn cp_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("cp"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn rm_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("rm"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
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
