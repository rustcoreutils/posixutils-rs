//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib_testing::{run_test, TestPlan};
use std::{
    fs::{remove_file, File},
    io::Read,
};

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

#[test]
fn magic_header_compress_file() {
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
fn compression_compress_file() {
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
