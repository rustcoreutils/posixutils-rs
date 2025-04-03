//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#[path = "../diff_util/constants.rs"]
mod constants;

use constants::{EXIT_STATUS_DIFFERENCE, EXIT_STATUS_NO_DIFFERENCE};
use plib::testing::{TestPlan, run_test};
use std::{collections::HashMap, path::PathBuf, process::Stdio, sync::LazyLock};

fn diff_test(args: &[&str], expected_output: &str, expected_diff_exit_status: u8) {
    let str_args = args.iter().cloned().map(str::to_owned).collect();

    run_test(TestPlan {
        cmd: String::from("diff"),
        args: str_args,
        stdin_data: String::from(""),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: i32::from(expected_diff_exit_status),
    });
}

fn diff_base_path() -> PathBuf {
    PathBuf::from("tests").join("diff")
}

fn f1_txt_path() -> String {
    diff_base_path()
        .join("f1.txt")
        .to_str()
        .expect("Could not unwrap f1_txt_path")
        .to_string()
}

fn f2_txt_path() -> String {
    diff_base_path()
        .join("f2.txt")
        .to_str()
        .expect("Could not unwrap f2_txt_path")
        .to_string()
}

fn f1_dir_path() -> String {
    diff_base_path()
        .join("f1")
        .to_str()
        .expect("Could not unwrap f1_dir_path")
        .to_string()
}

fn f2_dir_path() -> String {
    diff_base_path()
        .join("f2")
        .to_str()
        .expect("Could not unwrap f2_dir_path")
        .to_string()
}

fn f1_txt_with_eol_spaces_path() -> String {
    diff_base_path()
        .join("f1_with_eol_spaces.txt")
        .to_str()
        .expect("Could not unwrap f1_txt_with_eol_spaces_path")
        .to_string()
}

struct DiffTestHelper {
    content: String,
    file1_path: String,
    file2_path: String,
}

impl DiffTestHelper {
    fn new(options: &str, file1_path: String, file2_path: String) -> Self {
        let args = format!(
            "run --release --bin diff --{} {} {}",
            options, file1_path, file2_path
        );

        let args_list = args.split(' ').collect::<Vec<&str>>();

        let output = std::process::Command::new("cargo")
            .args(args_list)
            // .stdout(output_file)
            .stdout(Stdio::piped())
            .output()
            .expect("Could not run cargo command!");

        let content = String::from_utf8(output.stdout).expect("Failed to read output of Command!");

        Self {
            file1_path,
            file2_path,
            content,
        }
    }

    fn content(&self) -> &str {
        &self.content
    }

    fn file1_path(&self) -> &str {
        &self.file1_path
    }

    fn file2_path(&self) -> &str {
        &self.file2_path
    }
}

fn get_diff_test_helper_hash_map() -> HashMap<String, DiffTestHelper> {
    let diff_test_helper_init_data = [
        ("", f1_txt_path(), f2_txt_path(), "test_diff_normal"),
        (" -c", f1_txt_path(), f2_txt_path(), "test_diff_context3"),
        (" -C 1", f1_txt_path(), f2_txt_path(), "test_diff_context1"),
        (
            " -C 10",
            f1_txt_path(),
            f2_txt_path(),
            "test_diff_context10",
        ),
        (" -e", f1_txt_path(), f2_txt_path(), "test_diff_edit_script"),
        (
            " -f",
            f1_txt_path(),
            f2_txt_path(),
            "test_diff_forward_edit_script",
        ),
        (" -u", f1_txt_path(), f2_txt_path(), "test_diff_unified3"),
        (" -U 0", f1_txt_path(), f2_txt_path(), "test_diff_unified0"),
        (
            " -U 10",
            f1_txt_path(),
            f2_txt_path(),
            "test_diff_unified10",
        ),
        ("", f1_txt_path(), f2_dir_path(), "test_diff_file_directory"),
        ("", f1_dir_path(), f2_dir_path(), "test_diff_directories"),
        (
            " -r",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive",
        ),
        (
            " -r -c",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive_context",
        ),
        (
            " -r -e",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive_edit_script",
        ),
        (
            " -r -f",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive_forward_edit_script",
        ),
        (
            " -r -u",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive_unified",
        ),
        (
            "",
            f1_txt_path(),
            f1_txt_with_eol_spaces_path(),
            "test_diff_counting_eol_spaces",
        ),
        (
            " -b",
            f1_txt_path(),
            f1_txt_with_eol_spaces_path(),
            "test_diff_ignoring_eol_spaces",
        ),
        (
            " --label F1 --label2 F2 -u",
            f1_txt_path(),
            f1_txt_with_eol_spaces_path(),
            "test_diff_unified_two_labels",
        ),
    ];

    let mut diff_test_helper_hash_map =
        HashMap::<String, DiffTestHelper>::with_capacity(diff_test_helper_init_data.len());

    for (options, file1_path, file2_path, key) in diff_test_helper_init_data {
        let insert_option = diff_test_helper_hash_map.insert(
            key.to_owned(),
            DiffTestHelper::new(options, file1_path, file2_path),
        );

        assert!(insert_option.is_none());
    }

    diff_test_helper_hash_map
}

fn input_by_key(key: &str) -> &'static DiffTestHelper {
    static DIFF_TEST_INPUT: LazyLock<HashMap<String, DiffTestHelper>> =
        LazyLock::new(get_diff_test_helper_hash_map);

    // Initialized on first access
    DIFF_TEST_INPUT.get(key).unwrap()
}

#[test]
fn test_diff_normal() {
    let data = input_by_key("test_diff_normal");

    diff_test(
        &[data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_context3() {
    let data = input_by_key("test_diff_context3");

    diff_test(
        &["-c", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_context1() {
    let data = input_by_key("test_diff_context1");

    diff_test(
        &["-C", "1", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_context10() {
    let data = input_by_key("test_diff_context10");

    diff_test(
        &["-C", "10", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_edit_script() {
    let data = input_by_key("test_diff_edit_script");

    diff_test(
        &["-e", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_forward_edit_script() {
    let data = input_by_key("test_diff_forward_edit_script");

    diff_test(
        &["-f", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_unified3() {
    let data = input_by_key("test_diff_unified3");

    diff_test(
        &["-u", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_unified0() {
    let data = input_by_key("test_diff_unified0");

    diff_test(
        &["-U", "0", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_unified10() {
    let data = input_by_key("test_diff_unified10");

    diff_test(
        &["-U", "10", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_file_directory() {
    let data = input_by_key("test_diff_file_directory");

    diff_test(
        &[data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories() {
    let data = input_by_key("test_diff_directories");

    diff_test(
        &[data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive() {
    let data = input_by_key("test_diff_directories_recursive");

    diff_test(
        &["-r", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive_context() {
    let data = input_by_key("test_diff_directories_recursive_context");

    diff_test(
        &["-r", "-c", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive_edit_script() {
    let data = input_by_key("test_diff_directories_recursive_edit_script");

    diff_test(
        &["-r", "-e", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive_forward_edit_script() {
    let data = input_by_key("test_diff_directories_recursive_forward_edit_script");

    diff_test(
        &["-r", "-f", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive_unified() {
    let data = input_by_key("test_diff_directories_recursive_unified");

    diff_test(
        &["-r", "-u", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_counting_eol_spaces() {
    let data = input_by_key("test_diff_counting_eol_spaces");

    diff_test(
        &[data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_ignoring_eol_spaces() {
    let data = input_by_key("test_diff_ignoring_eol_spaces");

    diff_test(
        &["-b", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_NO_DIFFERENCE,
    );
}

#[test]
fn test_diff_unified_two_labels() {
    let data = input_by_key("test_diff_unified_two_labels");

    diff_test(
        &[
            "--label",
            "F1",
            "--label2",
            "F2",
            "-u",
            data.file1_path(),
            data.file2_path(),
        ],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}
