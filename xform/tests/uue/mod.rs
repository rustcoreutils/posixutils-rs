//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};
use std::{
    fs::{File, Permissions},
    io::Read,
    os::unix::fs::PermissionsExt,
};

const RWX: u32 = 0o7;
const UUCODE_PERMISSION_PLACEHOLDER: &str = "#PERM#";

fn get_permission_values(perm: Permissions) -> String {
    let perm_mode = perm.mode();

    let others_perm = perm_mode & RWX;
    let group_perm = (perm_mode >> 3) & RWX;
    let user_perm = (perm_mode >> 6) & RWX;

    format!("{user_perm}{group_perm}{others_perm}")
}

fn uuencode_test(args: &[&str], expected_output: &str, expected_error: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("uuencode"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code: 0,
    })
}

fn uudecode_test(args: &[&str], stdin_data: &str, expected_output: &str, expected_error: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("uudecode"),
        args: str_args,
        stdin_data: String::from(stdin_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code: 0,
    });
}

#[test]
fn uuencode_uudecode_with_historical_encoding_text_file() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let source_file_path = cargo_manifest_dir.join("tests/uucode/sample.txt");

    let source_file = File::open(&source_file_path).unwrap();
    let perm = source_file.metadata().unwrap().permissions();

    // the "sample_historical_encoded.txt" is generated from the GNU uuencode sharutils
    let encoded_file = cargo_manifest_dir.join("tests/uucode/sample_historical_encoded.txt");
    let mut encoded_file_content = String::new();

    File::open(&encoded_file)
        .unwrap()
        .read_to_string(&mut encoded_file_content)
        .unwrap();

    encoded_file_content = encoded_file_content.replacen(
        UUCODE_PERMISSION_PLACEHOLDER,
        &get_permission_values(perm),
        1,
    );

    uuencode_test(
        &[source_file_path.to_str().unwrap(), "/dev/stdout"],
        &encoded_file_content,
        "",
    );

    let mut source_file_content = Vec::new();
    File::open(&source_file_path)
        .unwrap()
        .read_to_end(&mut source_file_content)
        .unwrap();
    let source_file_content = String::from_utf8_lossy(&source_file_content);

    // Decode the encoded file using uudecode
    uudecode_test(&[], &encoded_file_content, &source_file_content, "");
}

#[test]
fn uuencode_uudecode_with_base64_encoding_text_file() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let source_file_path = cargo_manifest_dir.join("tests/uucode/sample.txt");

    let source_file = File::open(&source_file_path).unwrap();
    let perm = source_file.metadata().unwrap().permissions();

    let encoded_file = cargo_manifest_dir.join("tests/uucode/sample_base64_encoded.txt");
    let mut encoded_file_content = String::new();

    File::open(&encoded_file)
        .unwrap()
        .read_to_string(&mut encoded_file_content)
        .unwrap();

    encoded_file_content = encoded_file_content.replacen(
        UUCODE_PERMISSION_PLACEHOLDER,
        &get_permission_values(perm),
        1,
    );

    uuencode_test(
        &["-m", source_file_path.to_str().unwrap(), "/dev/stdout"],
        &encoded_file_content,
        "",
    );

    let mut source_file_content = Vec::new();
    File::open(&source_file_path)
        .unwrap()
        .read_to_end(&mut source_file_content)
        .unwrap();
    let source_file_content = String::from_utf8_lossy(&source_file_content);

    uudecode_test(&[], &encoded_file_content, &source_file_content, "");
}

#[test]
fn uuencode_uudecode_with_historical_encoding_jpg_file() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let source_file_path = cargo_manifest_dir.join("tests/uucode/image.jpg");

    let source_file = File::open(&source_file_path).unwrap();
    let perm = source_file.metadata().unwrap().permissions();

    let encoded_file = cargo_manifest_dir.join("tests/uucode/image_historical_encoded.txt");
    let mut encoded_file_content = String::new();

    File::open(&encoded_file)
        .unwrap()
        .read_to_string(&mut encoded_file_content)
        .unwrap();

    encoded_file_content = encoded_file_content.replacen(
        UUCODE_PERMISSION_PLACEHOLDER,
        &get_permission_values(perm),
        1,
    );

    uuencode_test(
        &[source_file_path.to_str().unwrap(), "/dev/stdout"],
        &encoded_file_content,
        "",
    );

    let mut source_file_content = Vec::new();
    File::open(&source_file_path)
        .unwrap()
        .read_to_end(&mut source_file_content)
        .unwrap();
    let source_file_content = String::from_utf8_lossy(&source_file_content);

    uudecode_test(&[], &encoded_file_content, &source_file_content, "");
}

#[test]
fn uuencode_uudecode_with_base64_encoding_jpg_file() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let source_file_path = cargo_manifest_dir.join("tests/uucode/image.jpg");

    let source_file = File::open(&source_file_path).unwrap();
    let perm = source_file.metadata().unwrap().permissions();

    let encoded_file = cargo_manifest_dir.join("tests/uucode/image_base64_encoded.txt");

    let mut encoded_file_content = String::new();
    File::open(&encoded_file)
        .unwrap()
        .read_to_string(&mut encoded_file_content)
        .unwrap();

    encoded_file_content = encoded_file_content.replacen(
        UUCODE_PERMISSION_PLACEHOLDER,
        &get_permission_values(perm),
        1,
    );

    uuencode_test(
        &["-m", source_file_path.to_str().unwrap(), "/dev/stdout"],
        &encoded_file_content,
        "",
    );

    let mut source_file_content = Vec::new();
    File::open(&source_file_path)
        .unwrap()
        .read_to_end(&mut source_file_content)
        .unwrap();
    let source_file_content = String::from_utf8_lossy(&source_file_content);

    uudecode_test(&[], &encoded_file_content, &source_file_content, "");
}
