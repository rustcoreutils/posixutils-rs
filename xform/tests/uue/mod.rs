//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_base, TestPlan};
use std::{
    fs::{File, Permissions},
    io::Read,
    os::unix::fs::PermissionsExt,
    path::PathBuf,
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

// =============================================================================
// uudecode robustness — malformed/binary input must diagnose + exit >0,
// never panic (exit 101).  Regression tests for audit #UD1.
// =============================================================================

fn uudecode_expect_graceful_failure(stdin_data: &[u8]) {
    let output = run_test_base(&String::from("uudecode"), &Vec::new(), stdin_data);
    let code = output.status.code();
    assert_ne!(
        code,
        Some(101),
        "uudecode panicked (exit 101) on malformed input; stderr={}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        code,
        Some(1),
        "expected exit 1 on malformed input, got {:?}; stderr={}",
        code,
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !output.stderr.is_empty(),
        "expected a diagnostic on stderr for malformed input"
    );
}

#[test]
fn uudecode_binary_input_no_panic() {
    // Non-UTF-8 / random binary input previously hit String::from_utf8().unwrap().
    let data: &[u8] = &[0x80, 0xFF, 0x00, 0x01, b'h', b'i', 0xC3, 0x28, 0xFE, 0x9D];
    uudecode_expect_graceful_failure(data);
}

#[test]
fn uudecode_empty_input_no_panic() {
    uudecode_expect_graceful_failure(b"");
}

#[test]
fn uudecode_missing_header_no_panic() {
    uudecode_expect_graceful_failure(b"this is not uuencoded data\nmore junk\n");
}

#[test]
fn uudecode_truncated_header_no_panic() {
    // "begin" with no mode / no pathname previously panicked on split[1]/split[2].
    uudecode_expect_graceful_failure(b"begin\n");
}

#[test]
fn uudecode_bad_mode_no_panic() {
    // Non-octal mode previously hit u32::from_str_radix(...).expect(...).
    uudecode_expect_graceful_failure(b"begin xyz out.txt\nM(2)Q\n");
}

// =============================================================================
// uudecode spec semantics — regression tests for audit #UD2..#UD6.
// Base64 of b"hi\n" (0x68,0x69,0x0a) is "aGkK".
// =============================================================================

fn unique_tmp(name: &str) -> PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("xform_uue_{}_{}", std::process::id(), name));
    p
}

fn running_as_root() -> bool {
    unsafe { libc::geteuid() == 0 }
}

#[test]
fn uudecode_dash_cookie_to_stdout() {
    // A decode pathname of '-' in the header means standard output (#UD2).
    run_test(TestPlan {
        cmd: String::from("uudecode"),
        args: vec![],
        stdin_data: String::from("begin-base64 644 -\naGkK\n====\n"),
        expected_out: String::from("hi\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn uudecode_o_dash_overrides_to_stdout() {
    // -o - means stdout and must not create the header's named file (#UD2).
    let bogus = unique_tmp("should_not_be_created");
    let _ = std::fs::remove_file(&bogus);
    let input = format!("begin-base64 644 {}\naGkK\n====\n", bogus.display());
    run_test(TestPlan {
        cmd: String::from("uudecode"),
        args: vec![String::from("-o"), String::from("-")],
        stdin_data: input,
        expected_out: String::from("hi\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
    assert!(!bogus.exists(), "-o - must not create the header's file");
}

#[test]
fn uudecode_scans_for_begin_line() {
    // The begin line need not be the first input line (#UD3).
    run_test(TestPlan {
        cmd: String::from("uudecode"),
        args: vec![],
        stdin_data: String::from("From: a@b\nSubject: x\n\nbegin-base64 644 -\naGkK\n====\n"),
        expected_out: String::from("hi\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn uudecode_base64_tolerates_crlf_and_whitespace() {
    // CRLF transport and stray whitespace must be ignored, not error (#UD4).
    run_test(TestPlan {
        cmd: String::from("uudecode"),
        args: vec![],
        stdin_data: String::from("begin-base64 644 -\r\naGkK \r\n====\r\n"),
        expected_out: String::from("hi\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn uudecode_overwrites_existing_writable_file() {
    // Existing writable target is overwritten in place (#UD5).
    let target = unique_tmp("existing_writable.txt");
    std::fs::write(&target, b"OLD CONTENT").unwrap();
    let output = run_test_base(
        &String::from("uudecode"),
        &vec![String::from("-o"), target.to_str().unwrap().to_string()],
        b"begin-base64 644 ignored\naGkK\n====\n",
    );
    let code = output.status.code();
    let got = std::fs::read(&target).unwrap();
    let _ = std::fs::remove_file(&target);
    assert_eq!(
        code,
        Some(0),
        "stderr={}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(got, b"hi\n");
}

#[test]
fn uudecode_readonly_target_errors() {
    // Existing non-writable target → error, file left unchanged (#UD5).
    if running_as_root() {
        return; // root bypasses W_OK
    }
    let target = unique_tmp("readonly.txt");
    std::fs::write(&target, b"KEEP").unwrap();
    let mut perm = std::fs::metadata(&target).unwrap().permissions();
    perm.set_mode(0o444);
    std::fs::set_permissions(&target, perm).unwrap();

    let output = run_test_base(
        &String::from("uudecode"),
        &vec![String::from("-o"), target.to_str().unwrap().to_string()],
        b"begin-base64 644 ignored\naGkK\n====\n",
    );

    // Restore perms so cleanup can remove it.
    let mut perm = std::fs::metadata(&target).unwrap().permissions();
    perm.set_mode(0o644);
    let _ = std::fs::set_permissions(&target, perm);
    let content = std::fs::read(&target).unwrap();
    let _ = std::fs::remove_file(&target);

    assert_eq!(output.status.code(), Some(1));
    assert_eq!(content, b"KEEP", "read-only target must be left unchanged");
}
