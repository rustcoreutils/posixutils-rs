//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use plib::testing::{run_test_u8, TestPlanU8};

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

#[test]
fn gencat_sets_and_messagess_with_quote() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let msg_file = cargo_manifest_dir.join("tests/gencat/sets_and_messages_with_quotes.msg");

    #[cfg(not(target_os = "macos"))]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/sets_and_messages_with_quotes_gnu_catfile.cat");

    #[cfg(target_os = "macos")]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/sets_and_messages_with_quotes_osx_catfile.cat");

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
fn gencat_sets_and_messagess_with_quote_unset() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let msg_file = cargo_manifest_dir.join("tests/gencat/sets_and_messages_with_quote_unset.msg");

    #[cfg(not(target_os = "macos"))]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/sets_and_messages_with_quote_unset_gnu_catfile.cat");

    #[cfg(target_os = "macos")]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/sets_and_messages_with_quote_unset_osx_catfile.cat");

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
