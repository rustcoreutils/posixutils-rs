//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{env, path::PathBuf};

use plib::testing::{run_test, run_test_with_checker, TestPlan};

/// Assert only on stdout and exit code. Used for the default-system-test cases,
/// which may emit a benign stderr note if the system magic file is absent.
fn file_test_stdout(args: &[&str], stdin: &str, expected_stdout: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("file"),
            args: str_args,
            stdin_data: String::from(stdin),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected_stdout);
            assert_eq!(output.status.code(), Some(0));
        },
    );
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
fn file_is_a_character_special() {
    let file = "/dev/null";

    file_test(&[file], &format!("{file}: character special\n"), "");
}

#[test]
fn file_is_an_empty_file() {
    let file = "tests/file/empty_file.txt";

    file_test(&[file], &format!("{file}: empty\n"), "");
}

#[test]
fn file_i_flag_regular_no_classify() {
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

    // Use an empty target so the default (follow-the-link) classification is
    // deterministic regardless of the system magic database.
    let mut file = cargo_man_dir.clone();
    file.push("tests/file/sym_link_empty_target_tmp");
    std::fs::write(&file, b"").unwrap();

    // Remove the symlink or file inside the tests/file folder, if it already exists
    if symlink_metadata(&valid_sym_link).is_ok() {
        remove_file(&valid_sym_link).unwrap()
    }

    // Create the valid symlink
    symlink(&file, &valid_sym_link).unwrap();

    // By default file resolves the link and reports the TARGET's type (here an
    // empty file), not "symbolic link to ...".
    file_test(
        &[valid_sym_link.to_str().unwrap()],
        &format!("{}: empty\n", valid_sym_link.to_str().unwrap()),
        "",
    );

    // Delete the symlink and target after testing
    remove_file(valid_sym_link).unwrap();
    remove_file(file).unwrap();
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
    if symlink_metadata(&broken_sym_link).is_ok() {
        remove_file(&broken_sym_link).unwrap()
    }

    // Create a broken sym link
    symlink(&file, &broken_sym_link).unwrap();

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
fn file_h_flag_symlinks() {
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
    if symlink_metadata(&valid_symlink).is_ok() {
        remove_file(&valid_symlink).unwrap();
    }
    if symlink_metadata(&broken_symlink).is_ok() {
        remove_file(&broken_symlink).unwrap();
    }

    // Create symbolic links
    symlink(&regular_file, &valid_symlink).unwrap();
    symlink(&regular_file, &broken_symlink).unwrap();

    // With -h, file identifies the link itself, including its target (POSIX
    // alternative output format "%s: %s %s"). Both links point to a real file.
    file_test(
        &["-h", valid_symlink.to_str().unwrap()],
        &format!(
            "{}: symbolic link to {}\n",
            valid_symlink.to_str().unwrap(),
            regular_file.to_str().unwrap()
        ),
        "",
    );

    file_test(
        &["-h", broken_symlink.to_str().unwrap()],
        &format!(
            "{}: symbolic link to {}\n",
            broken_symlink.to_str().unwrap(),
            regular_file.to_str().unwrap()
        ),
        "",
    );

    // Remove the symbolic links after testing
    remove_file(valid_symlink).unwrap();
    remove_file(broken_symlink).unwrap();
}

#[test]
fn file_magic_m_flag_cpio() {
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
fn file_magic_M_flag_cpio() {
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

#[test]
fn file_magic_hex_escape_jbig2() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let magic_file = cargo_manifest_dir.join("tests/file/jbig2.magic");
    let test_file = cargo_manifest_dir.join("tests/file/test.jb2");

    file_test(
        &[
            "-m",
            magic_file.to_str().unwrap(),
            test_file.to_str().unwrap(),
        ],
        &format!("{}: JBIG2 image data\n", test_file.to_str().unwrap(),),
        "",
    );
}

#[test]
fn file_malformed_magic_file_no_crash() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let magic_file = cargo_manifest_dir.join("tests/file/malformed.magic");
    let test_file = cargo_manifest_dir.join("tests/file/regular_file.txt");

    // The utility should not crash on a malformed magic file.
    // It should skip bad lines. The file has a valid "hello" entry that won't
    // match regular_file.txt, so the output should be "data".
    file_test(
        &[
            "-m",
            magic_file.to_str().unwrap(),
            test_file.to_str().unwrap(),
        ],
        &format!("{}: data\n", test_file.to_str().unwrap()),
        "",
    );
}

#[test]
fn file_magic_octal_escape_packed_data() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let magic_file = cargo_manifest_dir.join("tests/file/magic_file_a");

    // Create a temp file starting with \037\036 (octal for 0x1F 0x1E) = "Packed data"
    let tmp_dir = cargo_manifest_dir.join("tests/file");
    let tmp_path = tmp_dir.join("packed_data_test_tmp");
    std::fs::write(&tmp_path, [0o037, 0o036, 0x00, 0x00]).unwrap();

    let tmp_str = tmp_path.to_str().unwrap();

    file_test(
        &["-m", magic_file.to_str().unwrap(), tmp_str],
        &format!("{tmp_str}: Packed data\n"),
        "",
    );

    std::fs::remove_file(&tmp_path).unwrap();
}

// MAGIC-1: numeric `<` / `>` magic comparisons are "<file value> OP <field>".
#[test]
fn file_magic_numeric_less_greater() {
    let dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("tests/file");
    let magic = dir.join("cmp_test_tmp.magic");
    std::fs::write(
        &magic,
        "0\tuC\t<10\tLESS_THAN_10\n0\tuC\t>10\tGREATER_THAN_10\n",
    )
    .unwrap();
    let magic_s = magic.to_str().unwrap();

    let low = dir.join("cmp_low_tmp");
    std::fs::write(&low, [0x05u8]).unwrap();
    file_test(
        &["-m", magic_s, low.to_str().unwrap()],
        &format!("{}: LESS_THAN_10\n", low.to_str().unwrap()),
        "",
    );

    let high = dir.join("cmp_high_tmp");
    std::fs::write(&high, [0x14u8]).unwrap();
    file_test(
        &["-m", magic_s, high.to_str().unwrap()],
        &format!("{}: GREATER_THAN_10\n", high.to_str().unwrap()),
        "",
    );

    std::fs::remove_file(&magic).unwrap();
    std::fs::remove_file(&low).unwrap();
    std::fs::remove_file(&high).unwrap();
}

// FILE-2: context-sensitive default system tests (shell / C source).
#[test]
fn file_context_shell_script() {
    let dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("tests/file");
    let f = dir.join("ctx_shell_tmp");
    std::fs::write(&f, "#!/bin/sh\necho hi\n").unwrap();
    file_test_stdout(
        &[f.to_str().unwrap()],
        "",
        &format!("{}: commands text\n", f.to_str().unwrap()),
    );
    std::fs::remove_file(&f).unwrap();
}

#[test]
fn file_context_c_source() {
    let dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("tests/file");
    let f = dir.join("ctx_c_tmp");
    std::fs::write(&f, "#include <stdio.h>\nint main(){return 0;}\n").unwrap();
    file_test_stdout(
        &[f.to_str().unwrap()],
        "",
        &format!("{}: c program text\n", f.to_str().unwrap()),
    );
    std::fs::remove_file(&f).unwrap();
}

// FILE-4: a '-' operand classifies standard-input content.
#[test]
fn file_dash_classifies_stdin() {
    file_test_stdout(&["-"], "#!/bin/bash\n", "/dev/stdin: commands text\n");
}

#[test]
fn file_dash_empty_stdin() {
    file_test_stdout(&["-"], "", "/dev/stdin: empty\n");
}

// MAGIC-3: the magic message is a printf format taking the file value.
#[test]
fn file_magic_printf_message() {
    let dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("tests/file");
    let magic = dir.join("printf_msg_tmp.magic");
    std::fs::write(&magic, "0\tuC\t5\tvalue is %d here\n").unwrap();
    let data = dir.join("printf_msg_data_tmp");
    std::fs::write(&data, [0x05u8]).unwrap();

    file_test(
        &["-m", magic.to_str().unwrap(), data.to_str().unwrap()],
        &format!("{}: value is 5 here\n", data.to_str().unwrap()),
        "",
    );

    std::fs::remove_file(&magic).unwrap();
    std::fs::remove_file(&data).unwrap();
}

// MAGIC-4: a '>' continuation appends its message to the parent's.
#[test]
fn file_magic_continuation_appends() {
    let dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("tests/file");
    let magic = dir.join("contin_tmp.magic");
    std::fs::write(&magic, "0\tuC\t1\tParent\n>1\tuC\t&0x80\tChild\n").unwrap();
    let data = dir.join("contin_data_tmp");
    std::fs::write(&data, [0x01u8, 0x80u8]).unwrap();

    file_test(
        &["-m", magic.to_str().unwrap(), data.to_str().unwrap()],
        &format!("{}: Parent Child\n", data.to_str().unwrap()),
        "",
    );

    std::fs::remove_file(&magic).unwrap();
    std::fs::remove_file(&data).unwrap();
}

#[allow(non_snake_case)]
#[test]
fn file_magic_M_and_m_flag_cpio() {
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
