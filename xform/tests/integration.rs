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
    fs::{remove_file, File, Permissions},
    io::Read,
    os::unix::fs::PermissionsExt,
    sync::Mutex,
};

const UUCODE_TEST_PERMISSION: u32 = 0o644;
static UUCODE_TXT_LOCK: Mutex<()> = Mutex::new(());
static UUCODE_JPG_LOCK: Mutex<()> = Mutex::new(());

fn cksum_test(test_data: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("cksum"),
        args: Vec::new(),
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn compress_test(args: &[&str], expected_output: &str, expected_error: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("compress"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code: 0,
    });
}

fn uncompress_test(args: &[&str], expected_output: &str, expected_error: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("uncompress"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code: 0,
    });
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

fn uudecode_test(args: &[&str], expected_output: &str, expected_error: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("uudecode"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code: 0,
    });
}

#[test]
fn test_cksum() {
    cksum_test("foo\n", "3915528286 4\n");
}

#[test]
fn test_magic_header_compress_file() {
    use std::env;
    use std::fs;
    use std::path::PathBuf;

    const MAGIC_HEADER: [u8; 2] = [0x1F, 0x9D];

    // Get the directory of the Cargo project
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let source_file = cargo_manifest_dir.join("tests/compress/lorem_ipsum.txt");
    let file = cargo_manifest_dir.join("tests/compress/magic_header_test_file.txt");

    if file.exists() {
        remove_file(&file).unwrap();
    }

    fs::copy(&source_file, &file).unwrap();

    let compressed_file_path =
        cargo_manifest_dir.join("tests/compress/magic_header_test_file.txt.Z");

    // Delete the compressed file if it exists(usually from last test failure)
    if compressed_file_path.exists() {
        remove_file(&compressed_file_path).unwrap();
    }

    // test valid symbolic link
    compress_test(&[file.to_str().unwrap()], "", "");

    let mut file = File::open(&compressed_file_path).unwrap();
    let mut buffer = vec![0; MAGIC_HEADER.len()];
    file.read_exact(&mut buffer).unwrap();

    assert_eq!(buffer, MAGIC_HEADER);

    // Delete the compressed file(if test is successful)
    if compressed_file_path.exists() {
        remove_file(&compressed_file_path).unwrap();
    }
}

#[test]
fn test_compression_compress_file() {
    use std::env;
    use std::fs;
    use std::path::PathBuf;

    // Get the directory of the Cargo project
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let source_file = cargo_manifest_dir.join("tests/compress/lorem_ipsum.txt");
    let file = cargo_manifest_dir.join("tests/compress/compression.txt");

    if file.exists() {
        remove_file(&file).unwrap();
    }

    fs::copy(&source_file, &file).unwrap();

    let mut buf = String::new();
    let _file_contents = File::open(&file).unwrap().read_to_string(&mut buf);

    let compressed_file_path = cargo_manifest_dir.join("tests/compress/compression.txt.Z");

    // Delete the compressed file if it exists(usually from last test failure)
    if compressed_file_path.exists() {
        remove_file(&compressed_file_path).unwrap();
    }

    compress_test(&[file.to_str().unwrap()], "", "");

    uncompress_test(&[compressed_file_path.to_str().unwrap()], &buf, "");

    // Delete the compressed file(if test is successful)
    if compressed_file_path.exists() {
        remove_file(&compressed_file_path).unwrap();
    }
}

#[test]
fn test_uuencode_uudecode_with_historical_encoding_text_file() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let source_file_path = cargo_manifest_dir.join("tests/uucode/sample.txt");

    let _txt_lock = UUCODE_TXT_LOCK.lock().unwrap();

    let source_file = File::open(&source_file_path).unwrap();
    let start_perm = source_file.metadata().unwrap().permissions().mode();
    let new_perm = Permissions::from_mode(((start_perm >> 9) << 9) | UUCODE_TEST_PERMISSION);

    source_file.set_permissions(new_perm.clone()).unwrap();

    // the "sample_historical_encoded.txt" is generated from the GNU uuencode sharutils
    let encoded_file = cargo_manifest_dir.join("tests/uucode/sample_historical_encoded.txt");

    let mut encoded_file_content = String::new();
    File::open(&encoded_file)
        .unwrap()
        .read_to_string(&mut encoded_file_content)
        .unwrap();

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
    uudecode_test(&[encoded_file.to_str().unwrap()], &source_file_content, "");

    source_file
        .set_permissions(Permissions::from_mode(start_perm))
        .unwrap();
}

#[test]
fn test_uuencode_uudecode_with_base64_encoding_text_file() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let source_file_path = cargo_manifest_dir.join("tests/uucode/sample.txt");

    let _txt_lock = UUCODE_TXT_LOCK.lock().unwrap();

    let source_file = File::open(&source_file_path).unwrap();
    let start_perm = source_file.metadata().unwrap().permissions();
    let new_perm = Permissions::from_mode(((start_perm.mode() >> 9) << 9) | UUCODE_TEST_PERMISSION);

    source_file.set_permissions(new_perm).unwrap();

    let encoded_file = cargo_manifest_dir.join("tests/uucode/sample_base64_encoded.txt");

    let mut encoded_file_content = String::new();
    File::open(&encoded_file)
        .unwrap()
        .read_to_string(&mut encoded_file_content)
        .unwrap();

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

    // Decode the encoded file using uudecode
    uudecode_test(&[encoded_file.to_str().unwrap()], &source_file_content, "");

    source_file.set_permissions(start_perm).unwrap();
}

#[test]
fn test_uuencode_uudecode_with_historical_encoding_jpg_file() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let source_file_path = cargo_manifest_dir.join("tests/uucode/image.jpg");

    let _jpg_lock = UUCODE_JPG_LOCK.lock().unwrap();

    let source_file = File::open(&source_file_path).unwrap();
    let start_perm = source_file.metadata().unwrap().permissions();
    let new_perm = Permissions::from_mode(((start_perm.mode() >> 9) << 9) | UUCODE_TEST_PERMISSION);

    source_file.set_permissions(new_perm).unwrap();
    let encoded_file = cargo_manifest_dir.join("tests/uucode/image_historical_encoded.txt");

    let mut encoded_file_content = String::new();
    File::open(&encoded_file)
        .unwrap()
        .read_to_string(&mut encoded_file_content)
        .unwrap();

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
    uudecode_test(&[encoded_file.to_str().unwrap()], &source_file_content, "");

    source_file.set_permissions(start_perm).unwrap();
}

#[test]
fn test_uuencode_uudecode_with_base64_encoding_jpg_file() {
    use std::env;
    use std::path::PathBuf;

    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let source_file_path = cargo_manifest_dir.join("tests/uucode/image.jpg");

    let _jpg_lock = UUCODE_JPG_LOCK.lock().unwrap();

    let source_file = File::open(&source_file_path).unwrap();
    let start_perm = source_file.metadata().unwrap().permissions();
    let new_perm = Permissions::from_mode(((start_perm.mode() >> 9) << 9) | UUCODE_TEST_PERMISSION);

    source_file.set_permissions(new_perm).unwrap();
    let encoded_file = cargo_manifest_dir.join("tests/uucode/image_base64_encoded.txt");

    let mut encoded_file_content = String::new();
    File::open(&encoded_file)
        .unwrap()
        .read_to_string(&mut encoded_file_content)
        .unwrap();

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

    // Decode the encoded file using uudecode
    uudecode_test(&[encoded_file.to_str().unwrap()], &source_file_content, "");

    source_file.set_permissions(start_perm).unwrap();
}
