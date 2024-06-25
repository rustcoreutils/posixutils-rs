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

fn run_test_helper(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("cmp"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

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

fn od_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("od"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_cmp_same() {
    let mut files = vec![String::from("tests/cmp/lorem_ipsum.txt")];
    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];
    for i in indices {
        files.push(format!("tests/cmp/lorem_ipsum_{i}.txt"));
    }

    for file in &files {
        run_test_helper(&[file, file], "", "", 0);
    }
}

#[test]
fn test_cmp_different() {
    let original = "tests/cmp/lorem_ipsum.txt";

    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];
    let bytes = [1, 46, 91, 136, 181, 226, 271, 316, 361, 406, 451];
    let lines = [1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 7];

    for i in 0..indices.len() {
        let modified = format!("tests/cmp/lorem_ipsum_{}.txt", indices[i]);
        run_test_helper(
            &[original, &modified],
            &format!(
                "{original} {modified} differ: char {}, line {}\n",
                bytes[i], lines[i]
            ),
            "",
            1,
        );
    }
}

#[test]
fn test_cmp_different_silent() {
    let original = "tests/cmp/lorem_ipsum.txt";

    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];

    for i in 0..indices.len() {
        let modified = format!("tests/cmp/lorem_ipsum_{}.txt", indices[i]);
        run_test_helper(&["-s", original, &modified], "", "", 1);
    }
}

#[test]
fn test_cmp_different_less_verbose() {
    let original = "tests/cmp/lorem_ipsum.txt";

    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];
    let bytes = [1, 46, 91, 136, 181, 226, 271, 316, 361, 406, 451];
    let chars_original = ['L', 's', ' ', ' ', 'a', 'o', 'r', ' ', 'a', ' ', '.'];

    for i in 0..indices.len() {
        let modified = format!("tests/cmp/lorem_ipsum_{}.txt", indices[i]);
        run_test_helper(
            &["-l", original, &modified],
            &format!(
                "{} {:o} {:o}\n",
                bytes[i], chars_original[i] as u8, '?' as u8
            ),
            "",
            1,
        );
    }
}

#[test]
fn test_cmp_eof() {
    let original = "tests/cmp/lorem_ipsum.txt";
    let truncated = "tests/cmp/lorem_ipsum_trunc.txt";

    // Status code must be 1. From the specification:
    //
    // "...this includes the case where one file is identical to the first part
    // of the other."
    run_test_helper(
        &[original, truncated],
        "",
        &format!("cmp: EOF on {truncated}\n"),
        1,
    );
}

#[test]
fn test_file_doesnot_exist() {
    let file = "tests/file/this_file_doesnt_exist";

    file_test(&[file], &format!("{file}: cannot open\n"), "");
}

#[test]
fn test_file_is_a_directory() {
    let file = "tests/file";

    file_test(&[file], &format!("{file}: directory\n"), "");
}

#[test]
fn test_file_is_an_empty_file() {
    let file = "tests/file/empty_file.txt";

    file_test(&[file], &format!("{file}: empty\n"), "");
}

#[test]
fn test_file_with_i_flag_on_regular_file_with_no_further_classification() {
    let file = "tests/file/empty_file.txt";

    file_test(&[file, "-i"], &format!("{file}: regular file\n"), "");
}

#[test]
fn test_file_is_a_valid_sym_link() {
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
fn test_file_file_is_a_broken_sym_link() {
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
fn test_file_is_a_character_special() {
    let file = PathBuf::from("/dev/null");

    file_test(
        &[file.to_str().unwrap()],
        &format!("{}: character special\n", file.to_str().unwrap()),
        "",
    );
}

#[test]
fn test_file_symlink_with_h_flag_for_both_valid_and_broken_symlink() {
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
fn test_file_magic_file_priority_with_only_m_flag_using_cpio_archive() {
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
fn test_file_magic_file_priority_with_only_M_flag_using_cpio_archive() {
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
fn test_file_magic_file_priority_with_M_and_m_option_as_they_appear_using_cpio_archive() {
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

mod od_tests {
    use crate::od_test;

    #[test]
    fn test_od_0() {
        od_test(&["-tc", "-An"], "\x07", "\\a \n");
    }

    #[test]
    fn test_od_1() {
        od_test(&["-c", "-j1", "-An"], "a", "");
    }

    #[test]
    fn test_od_2() {
        od_test(&["-c", "-j3", "-An"], "abc", "");
    }

    #[test]
    fn test_od_3() {
        od_test(&["-tc", "-j3", "-An"], "abcd", "d \n");
    }

    #[test]
    fn test_od_4() {
        od_test(
            &[],
            "Hello, World!",
            "0000000 62510 66154 26157 53440 71157 62154 041 \n0000015 ",
        );
    }

    #[test]
    fn test_od_5() {
        od_test(
            &["-b"],
            "Hello, World!",
            "0000000 110 145 154 154 157 054 040 127 157 162 154 144 041 \n0000015 ",
        );
    }

    #[test]
    fn test_od_6() {
        od_test(
            &["-c"],
            "Hello, World!",
            "0000000 H e l l o ,   W o r l d ! \n0000015 ",
        );
    }

    #[test]
    fn test_od_7() {
        od_test(
            &["-d"],
            "Hello, World!",
            "0000000 25928 27756 11375 22304 29295 25708 33 \n0000015 ",
        );
    }

    #[test]
    fn test_od_8() {
        od_test(
            &["-Ad"],
            "Hello, World!",
            "0000000 62510 66154 26157 53440 71157 62154 041 \n0000013 ",
        );
    }

    #[test]
    fn test_od_9() {
        od_test(
            &["-An"],
            "Hello, World!",
            "62510 66154 26157 53440 71157 62154 041 \n",
        );
    }

    #[test]
    fn test_od_10() {
        od_test(
            &["-Ao"],
            "Hello, World!",
            "0000000 62510 66154 26157 53440 71157 62154 041 \n0000015 ",
        );
    }

    #[test]
    fn test_od_11() {
        od_test(
            &["-Ax"],
            "Hello, World!",
            "0000000 62510 66154 26157 53440 71157 62154 041 \n000000d ",
        );
    }

    #[test]
    fn test_od_12() {
        od_test(
            &["-j7"],
            "Hello, World!",
            "0000000 67527 66162 20544 \n0000006 ",
        );
    }

    #[test]
    fn test_od_13() {
        od_test(
            &["-N5"],
            "Hello, World!",
            "0000000 62510 66154 157 \n0000005 ",
        );
    }

    #[test]
    fn test_od_14() {
        od_test(
            &["-o"],
            "Hello, World!",
            "0000000 62510 66154 26157 53440 71157 62154 041 \n0000015 ",
        );
    }

    #[test]
    fn test_od_15() {
        od_test(
            &["-s"],
            "Hello, World!",
            "0000000 25928 27756 11375 22304 29295 25708 33 \n0000015 ",
        );
    }

    #[test]
    fn test_od_16() {
        od_test(
            &["-tf4"],
            "Hello, World!",
            "0000000 1.1431391224375825e27 1.7611270127616e14 1.7446709643352771e22 0e0 \n0000015 ",
        );
    }

    #[test]
    fn test_od_17() {
        od_test(
            &["-td2"],
            "Hello, World!",
            "0000000 25928 27756 11375 22304 29295 25708 33 \n0000015 ",
        );
    }

    #[test]
    fn test_od_18() {
        od_test(
            &["-v"],
            "Hello, World!",
            "0000000 62510 66154 26157 53440 71157 62154 041 \n0000015 ",
        );
    }

    #[test]
    fn test_od_19() {
        od_test(
            &["-x"],
            "Hello, World!",
            "0000000 6548 6c6c 2c6f 5720 726f 646c 0021 \n0000015 ",
        );
    }

    #[test]
    fn test_od_20() {
        od_test(
            &["-Ax", "-td4", "-v"],
            "Hello, World!",
            "0000000 1819043144 1461726319 1684828783 33 \n000000d ",
        );
    }

    #[test]
    fn test_od_21() {
        od_test(
            &["-j0x10"],
            "Hello, World!123456",
            "0000000 32464 066 \n0000003 ",
        );
    }

    #[test]
    fn test_od_22() {
        od_test(
            &["-tu4"],
            "Hello, World!1",
            "0000000 1819043144 1461726319 1684828783 12577 \n0000016 ",
        );
    }
}
