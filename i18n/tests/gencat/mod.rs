//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_u8, TestPlanU8};
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

fn gencat_test_stdin(args: &[&str], stdin_data: Vec<u8>, expected_output: Vec<u8>) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test_u8(TestPlanU8 {
        cmd: String::from("gencat"),
        args: str_args,
        stdin_data,
        expected_out: expected_output,
        expected_err: Vec::new(),
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

// GC-12: a `-` msgfile operand reads the message source from standard input.
// Feeding the same source via stdin must produce the same catalog as the file.
#[test]
fn gencat_msgfile_from_stdin() {
    let cargo_manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let msg_file = cargo_manifest_dir.join("tests/gencat/sets_and_messages.msg");

    #[cfg(not(target_os = "macos"))]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/sets_and_messages_gnu_catfile.cat");
    #[cfg(target_os = "macos")]
    let expected_cat_file =
        cargo_manifest_dir.join("tests/gencat/sets_and_messages_osx_catfile.cat");

    let mut stdin_data: Vec<u8> = Vec::new();
    File::open(&msg_file)
        .unwrap()
        .read_to_end(&mut stdin_data)
        .unwrap();

    let mut expected_output: Vec<u8> = Vec::new();
    File::open(&expected_cat_file)
        .unwrap()
        .read_to_end(&mut expected_output)
        .unwrap();

    gencat_test_stdin(&["-", "-"], stdin_data, expected_output);
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

// ============================================================================
// LC_CTYPE-driven byte interpretation (cross-cutting theme 4)
// ============================================================================

/// A message file is a text file in whatever codeset `LC_CTYPE` describes, not
/// necessarily UTF-8. The parser used `read_to_string`, so a perfectly valid
/// Latin-1 (or EUC-JP, or Shift-JIS) source was rejected outright before a
/// single line had been looked at. The bytes must also reach the catalog
/// unchanged: nothing may re-encode them.
#[test]
fn gencat_preserves_non_utf8_message_bytes() {
    let temp = tempfile::TempDir::new().unwrap();
    let msg_path = temp.path().join("latin1.msg");
    let cat_path = temp.path().join("latin1.cat");

    // "caf<E9>" in Latin-1: 0xE9 is not valid UTF-8 on its own.
    let mut source = Vec::new();
    source.extend_from_slice(b"$set 1\n1 caf\xe9\n");
    std::fs::write(&msg_path, &source).unwrap();

    let status = std::process::Command::new(plib::testing::get_binary_path("gencat"))
        .arg(&cat_path)
        .arg(&msg_path)
        .status()
        .unwrap();
    assert!(status.success(), "gencat rejected a Latin-1 message file");

    let mut catalog = Vec::new();
    File::open(&cat_path)
        .unwrap()
        .read_to_end(&mut catalog)
        .unwrap();
    assert!(
        catalog.windows(4).any(|w| w == b"caf\xe9"),
        "the message bytes must reach the catalog verbatim, not re-encoded"
    );
    // The UTF-8 encoding of U+00E9 is the corruption this guards against.
    assert!(
        !catalog.windows(2).any(|w| w == b"\xc3\xa9"),
        "0xE9 was UTF-8 encoded into two bytes"
    );
}

/// `\ddd` names a *byte*, so a high octal escape must produce one byte.
#[test]
fn gencat_octal_escape_above_7f_is_one_byte() {
    let temp = tempfile::TempDir::new().unwrap();
    let msg_path = temp.path().join("octal.msg");
    let cat_path = temp.path().join("octal.cat");
    // \351 == 0xE9.
    std::fs::write(&msg_path, b"$set 1\n1 caf\\351\n").unwrap();

    let status = std::process::Command::new(plib::testing::get_binary_path("gencat"))
        .arg(&cat_path)
        .arg(&msg_path)
        .status()
        .unwrap();
    assert!(status.success());

    let mut catalog = Vec::new();
    File::open(&cat_path)
        .unwrap()
        .read_to_end(&mut catalog)
        .unwrap();
    assert!(
        catalog.windows(4).any(|w| w == b"caf\xe9"),
        r"\351 must expand to the single byte 0xE9"
    );
}

/// Two messages in one set must not overrun the catalog's hash array.
///
/// The sizing pass measured collision depth with `msg_id * set_id` while the
/// fill pass placed messages with `msg_id * (set_id + 1)`. The array is sized
/// `best_size * best_depth * 3` from the first hash, so the second could probe
/// past its end: `$set 1` with messages 1 and 2 both land in slot 0 at
/// `best_size` 2, needing depth 2 against an array built for depth 1.
/// Pre-existing, and reachable with the smallest catalog that has a collision.
#[test]
fn gencat_two_messages_in_a_set_do_not_overrun_the_array() {
    let temp = tempfile::TempDir::new().unwrap();
    let msg_path = temp.path().join("two.msg");
    let cat_path = temp.path().join("two.cat");
    std::fs::write(&msg_path, b"$set 1\n1 first\n2 second\n").unwrap();

    let out = std::process::Command::new(plib::testing::get_binary_path("gencat"))
        .arg(&cat_path)
        .arg(&msg_path)
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !stderr.contains("panicked"),
        "gencat panicked on a two-message set: {stderr}"
    );
    assert!(out.status.success(), "gencat failed: {stderr}");

    let mut catalog = Vec::new();
    File::open(&cat_path)
        .unwrap()
        .read_to_end(&mut catalog)
        .unwrap();
    assert!(catalog.windows(6).any(|w| w == b"first\0"));
    assert!(catalog.windows(7).any(|w| w == b"second\0"));
}
