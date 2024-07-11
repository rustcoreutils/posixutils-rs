//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{env, path::PathBuf};

use plib::{run_test, TestPlan};

fn file_test(args: &[&str], expected_output: &str, expected_error: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("file"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code: 0,
    });
}

#[test]
fn file_doesnot_exist() {
    let file = "tests/file/this_file_doesnt_exist";

    file_test(&[file], &format!("{file}: cannot open\n"), "");
}

#[test]
fn file_is_a_directory() {
    let file = "tests/file";

    file_test(&[file], &format!("{file}: directory\n"), "");
}

#[test]
fn file_is_an_empty_file() {
    let file = "tests/file/empty_file.txt";

    file_test(&[file], &format!("{file}: empty\n"), "");
}

#[test]
fn file_with_i_flag_on_regular_file_with_no_further_classification() {
    let file = "tests/file/empty_file.txt";

    file_test(&[file, "-i"], &format!("{file}: regular file\n"), "");
}

#[test]
fn file_is_a_valid_sym_link() {
    use std::fs::{remove_file, symlink_metadata};
    use std::os::unix::fs::symlink;

    let cargo_man_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let mut valid_sym_link = cargo_man_dir.clone();
    valid_sym_link.push("tests/file/sym_link.txt");

    let mut file = cargo_man_dir.clone();
    file.push("tests/file/regular_file.txt");

    // Remove the symlink or file inside the tests/file folder, if it already exists
    if let Ok(_) = symlink_metadata(&valid_sym_link) {
        remove_file(&valid_sym_link).unwrap()
    }

    // Create the valid symlink
    let _ = symlink(&file, &valid_sym_link).unwrap();

    file_test(
        &[valid_sym_link.to_str().unwrap()],
        &format!(
            "{}: symbolic link to {}\n",
            valid_sym_link.to_str().unwrap(),
            file.to_str().unwrap()
        ),
        "",
    );

    // Delete the symlink after testing
    remove_file(valid_sym_link).unwrap()
}

#[test]
fn file_file_is_a_broken_sym_link() {
    use std::fs::{remove_file, symlink_metadata};
    use std::os::unix::fs::symlink;

    let cargo_man_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let mut broken_sym_link = cargo_man_dir.clone();
    broken_sym_link.push("tests/file/broken_sym_link.txt");

    let mut file = cargo_man_dir.clone();
    file.push("tests/file/this_file_does_not_exist.txt");

    // Remove the symlink or file inside the tests/file folder, if it already exists
    if let Ok(_) = symlink_metadata(&broken_sym_link) {
        remove_file(&broken_sym_link).unwrap()
    }

    // Create a broken sym link
    let _ = symlink(&file, &broken_sym_link).unwrap();

    file_test(
        &[broken_sym_link.to_str().unwrap()],
        &format!(
            "{}: broken symbolic link to {}\n",
            broken_sym_link.to_str().unwrap(),
            file.to_str().unwrap()
        ),
        "",
    );

    // Delete the symlink after testing
    remove_file(broken_sym_link).unwrap()
}

#[test]
fn file_is_a_character_special() {
    let file = PathBuf::from("/dev/null");

    file_test(
        &[file.to_str().unwrap()],
        &format!("{}: character special\n", file.to_str().unwrap()),
        "",
    );
}

#[test]
fn file_symlink_with_h_flag_for_both_valid_and_broken_symlink() {
    use std::env;
    use std::fs::{remove_file, symlink_metadata};
    use std::os::unix::fs::symlink;
    use std::path::PathBuf;

    // Get the directory of the Cargo project
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let valid_symlink = cargo_manifest_dir.join("tests/file/h_flag_valid_sym_link.txt");
    let broken_symlink = cargo_manifest_dir.join("tests/file/h_flag_broken_sym_link.txt");
    let regular_file = cargo_manifest_dir.join("tests/file/regular_file.txt");

    // Remove any existing symbolic links or files
    if let Ok(_) = symlink_metadata(&valid_symlink) {
        remove_file(&valid_symlink).unwrap();
    }
    if let Ok(_) = symlink_metadata(&broken_symlink) {
        remove_file(&broken_symlink).unwrap();
    }

    // Create symbolic links
    symlink(&regular_file, &valid_symlink).unwrap();
    symlink(&regular_file, &broken_symlink).unwrap();

    // test valid symbolic link
    file_test(
        &["-h", valid_symlink.to_str().unwrap()],
        &format!("{}: symbolic link\n", valid_symlink.to_str().unwrap(),),
        "",
    );

    file_test(
        &["-h", broken_symlink.to_str().unwrap()],
        &format!("{}: symbolic link\n", broken_symlink.to_str().unwrap(),),
        "",
    );

    // Remove the symbolic links after testing
    remove_file(valid_symlink).unwrap();
    remove_file(broken_symlink).unwrap();
}

#[test]
fn file_magic_file_priority_with_only_m_flag_using_cpio_archive() {
    use std::env;
    use std::path::PathBuf;

    // Get the directory of the Cargo project
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let magic_file_a = cargo_manifest_dir.join("tests/file/magic_file_a");
    let test_archive_file = cargo_manifest_dir.join("tests/file/test_archive.cpio");

    // test valid symbolic link
    file_test(
        &[
            "-m",
            magic_file_a.to_str().unwrap(),
            test_archive_file.to_str().unwrap(),
        ],
        &format!(
            "{}: cpio archive from magic_file_a\n",
            test_archive_file.to_str().unwrap(),
        ),
        "",
    );
}

#[allow(non_snake_case)]
#[test]
fn file_magic_file_priority_with_only_M_flag_using_cpio_archive() {
    use std::env;
    use std::path::PathBuf;

    // Get the directory of the Cargo project
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let magic_file_a = cargo_manifest_dir.join("tests/file/magic_file_a");
    let test_archive_file = cargo_manifest_dir.join("tests/file/test_archive.cpio");

    // test valid symbolic link
    file_test(
        &[
            "-M",
            magic_file_a.to_str().unwrap(),
            test_archive_file.to_str().unwrap(),
        ],
        &format!(
            "{}: cpio archive from magic_file_a\n",
            test_archive_file.to_str().unwrap(),
        ),
        "",
    );
}

#[allow(non_snake_case)]
#[test]
fn file_magic_file_priority_with_M_and_m_option_as_they_appear_using_cpio_archive() {
    use std::env;
    use std::path::PathBuf;

    // Get the directory of the Cargo project
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let magic_file_a = cargo_manifest_dir.join("tests/file/magic_file_a");
    let magic_file_b = cargo_manifest_dir.join("tests/file/magic_file_b");
    let test_archive_file = cargo_manifest_dir.join("tests/file/test_archive.cpio");

    // test valid symbolic link
    file_test(
        &[
            "-M",
            magic_file_a.to_str().unwrap(),
            "-m",
            magic_file_b.to_str().unwrap(),
            test_archive_file.to_str().unwrap(),
        ],
        &format!(
            "{}: cpio archive from magic_file_a\n",
            test_archive_file.to_str().unwrap(),
        ),
        "",
    );
}
