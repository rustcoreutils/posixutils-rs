//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test_u8, TestPlanU8};
use std::env;
use std::path::PathBuf;
use std::{fs::File, io::Read};

fn iconv_test(args: &[&str], input: Vec<u8>, expected_output: Vec<u8>, expected_error: Vec<u8>) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test_u8(TestPlanU8 {
        cmd: String::from("iconv"),
        args: str_args,
        stdin_data: input,
        expected_out: expected_output,
        expected_err: expected_error,
        expected_exit_code: 0,
    })
}

#[test]
fn iconv_no_flag_data_input() {
    let input = "Hello world".as_bytes().to_vec();
    iconv_test(&[], input.clone(), input.clone(), Vec::new());
}

#[test]
fn iconv_utf8_to_ascii_conversion_with_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf8");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf8_to_ascii_with_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-c", "-f", "UTF-8", "-t", "ASCII", "-"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-c", "-f", "UTF-8", "-t", "ASCII"],
        input,
        expected_output,
        Vec::new(),
    );
}
