//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#![allow(non_snake_case)]
use plib::testing::{run_test_u8, TestPlanU8};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

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
#[ignore]
fn iconv_no_flag_data_input() {
    let input = "Hello world".as_bytes().to_vec();
    iconv_test(&[], input.clone(), input.clone(), Vec::new());
}

#[test]
fn iconv_UTF8_to_ASCII_conversion_with_c_flag() {
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

#[test]
fn iconv_UTF8_to_UTF16LE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf8");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf8_to_utf16le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-8", "-t", "UTF-16LE", "-"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-8", "-t", "UTF-16LE"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
#[allow(non_snake_case)]
fn iconv_UTF8_to_UTF16BE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf8");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf8_to_utf16be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-8", "-t", "UTF-16BE", "-"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-8", "-t", "UTF-16BE"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
#[allow(non_snake_case)]
fn iconv_UTF8_to_UTF32BE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf8");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf8_to_utf32be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-8", "-t", "UTF-32BE", "-"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-8", "-t", "UTF-32BE"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
#[allow(non_snake_case)]
fn iconv_UTF8_to_UTF32LE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf8");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf8_to_utf32le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-8", "-t", "UTF-32LE", "-"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-8", "-t", "UTF-32LE"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_ASCII_to_UTF8_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_ascii");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file = cargo_manifest_dir.join("tests/iconv/test_data_ascii");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-8"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-8", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_ASCII_to_UTF16LE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_ascii");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_ascii_to_utf16le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-16LE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-16LE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_ASCII_to_UTF16BE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_ascii");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_ascii_to_utf16be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-16BE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-16BE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_ASCII_to_UTF32LE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_ascii");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_ascii_to_utf32le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-32LE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-32LE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_ASCII_to_UTF32BE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_ascii");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_ascii_to_utf32be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-32BE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "ASCII", "-t", "UTF-32BE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32LE_to_UTF32BE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32le_to_utf32be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-32LE", "-t", "UTF-32BE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-32LE", "-t", "UTF-32BE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32LE_to_ASCII_conversion_with_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32le_to_ascii_with_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-c", "-f", "UTF-32LE", "-t", "ASCII"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-c", "-f", "UTF-32LE", "-t", "ASCII", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32LE_to_UTF8_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32le_to_utf8_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-32LE", "-t", "UTF-8"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-32LE", "-t", "UTF-8", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32LE_to_UTF16LE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32le_to_utf16le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-32LE", "-t", "UTF-16LE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-32LE", "-t", "UTF-16LE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32LE_to_UTF16BE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32le_to_utf16be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-32LE", "-t", "UTF-16BE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-32LE", "-t", "UTF-16BE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32BE_to_UTF8_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32be_to_utf8_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-32BE", "-t", "UTF-8"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-32BE", "-t", "UTF-8", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32BE_to_UTF32LE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32be_to_utf32le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-32BE", "-t", "UTF-32LE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-32BE", "-t", "UTF-32LE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32BE_to_ASCII_conversion_with_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32be_to_ascii_with_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-c", "-f", "UTF-32BE", "-t", "ASCII"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-c", "-f", "UTF-32BE", "-t", "ASCII", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32BE_to_UTF16LE_conversion_with_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32be_to_utf16le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-32BE", "-t", "UTF-16LE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-32BE", "-t", "UTF-16LE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF32BE_to_UTF16BE_conversion_with_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf32be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf32be_to_utf16be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-32BE", "-t", "UTF-16BE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-32BE", "-t", "UTF-16BE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16LE_to_UTF16BE_conversion_with_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16le_to_utf16be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-16LE", "-t", "UTF-16BE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-16LE", "-t", "UTF-16BE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16LE_to_ASCII_conversion_with_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16le_to_ascii_with_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-c", "-f", "UTF-16LE", "-t", "ASCII"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-c", "-f", "UTF-16LE", "-t", "ASCII", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16LE_to_UTF32BE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16le_to_utf32be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-16LE", "-t", "UTF-32BE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-16LE", "-t", "UTF-32BE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16LE_to_UTF32LE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16le_to_utf32le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-16LE", "-t", "UTF-32LE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-16LE", "-t", "UTF-32LE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16LE_to_UTF8_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16le");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16le_to_utf8_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-16LE", "-t", "UTF-8"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-16LE", "-t", "UTF-8", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16BE_to_UTF16LE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16be_to_utf16le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-16BE", "-t", "UTF-16LE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-16BE", "-t", "UTF-16LE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16BE_to_ASCII_conversion_with_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16be_to_ascii_with_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-c", "-f", "UTF-16BE", "-t", "ASCII"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-c", "-f", "UTF-16BE", "-t", "ASCII", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16BE_to_UTF32BE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16be_to_utf32be_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-16BE", "-t", "UTF-32BE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-16BE", "-t", "UTF-32BE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16BE_to_UTF32LE_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16be_to_utf32le_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-16BE", "-t", "UTF-32LE"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-16BE", "-t", "UTF-32LE", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}

#[test]
fn iconv_UTF16BE_to_UTF8_conversion_without_c_flag() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let input_file = cargo_manifest_dir.join("tests/iconv/test_data_utf16be");
    let mut input: Vec<u8> = Vec::new();
    File::open(&input_file)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let expected_output_file =
        cargo_manifest_dir.join("tests/iconv/test_data_utf16be_to_utf8_without_c_flag");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_output_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    iconv_test(
        &["-f", "UTF-16BE", "-t", "UTF-8"],
        input.clone(),
        expected_output.clone(),
        Vec::new(),
    );

    iconv_test(
        &["-f", "UTF-16BE", "-t", "UTF-8", "-"],
        input,
        expected_output,
        Vec::new(),
    );
}
