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

fn gencat_test(args: &[&str], expected_output: Vec<u8>, expected_error: Vec<u8>) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test_u8(TestPlanU8 {
        cmd: String::from("gencat"),
        args: str_args,
        stdin_data: Vec::new(),
        expected_out: expected_output,
        expected_err: expected_error,
        expected_exit_code: 0,
    })
}

#[test]
fn gencat_empty_message_file() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let msg_file = cargo_manifest_dir.join("tests/gencat/empty_message_file.msg");

    #[cfg(not(target_os = "macos"))]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/empty_message_file_gnu_catfile.cat");

    #[cfg(target_os = "macos")]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/empty_message_file_osx_catfile.cat");

    //    let encoded_file = cargo_manifest_dir.join("tests/uucode/sample_historical_encoded.txt");
    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_cat_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    gencat_test(
        &["-", msg_file.to_str().unwrap()],
        expected_output,
        Vec::new(),
    );
}

#[test]
fn gencat_sets_and_messagess() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let msg_file = cargo_manifest_dir.join("tests/gencat/sets_and_messages.msg");

    #[cfg(not(target_os = "macos"))]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/sets_and_messages_gnu_catfile.cat");

    #[cfg(target_os = "macos")]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/sets_and_messages_osx_catfile.cat");

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_cat_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    gencat_test(
        &["-", msg_file.to_str().unwrap()],
        expected_output,
        Vec::new(),
    );
}
