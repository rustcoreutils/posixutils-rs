//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test, run_test_with_checker};
use std::{
    fs::{self, File, remove_file},
    io::Read,
    path::PathBuf,
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

fn compress_test_with_exit(args: &[&str], expected_exit: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("compress"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: expected_exit,
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

fn compress_stdin_test(stdin_data: &str) -> Vec<u8> {
    let str_args: Vec<String> = vec![String::from("-c")];
    let mut result = Vec::new();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("compress"),
            args: str_args,
            stdin_data: String::from(stdin_data),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            result = output.stdout.clone();
            assert!(!output.stdout.is_empty(), "compress should produce output");
            // Check magic header
            assert!(
                output.stdout.len() >= 2,
                "compress output too short for magic header"
            );
            assert_eq!(output.stdout[0], 0x1F, "Wrong magic byte 0");
            assert_eq!(output.stdout[1], 0x9D, "Wrong magic byte 1");
        },
    );

    result
}

fn get_test_dir() -> PathBuf {
    PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap())
        .join("tests")
        .join("compress")
}

fn cleanup_file(path: &PathBuf) {
    if path.exists() {
        let _ = remove_file(path);
    }
}

// =============================================================================
// Basic functionality tests
// =============================================================================

#[test]
fn test_compress_magic_header() {
    const MAGIC_HEADER: [u8; 2] = [0x1F, 0x9D];

    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("magic_header_test_file.txt");
    let compressed_file = test_dir.join("magic_header_test_file.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    compress_test(&[test_file.to_str().unwrap()], "", "");

    let mut file = File::open(&compressed_file).unwrap();
    let mut buffer = vec![0; MAGIC_HEADER.len()];
    file.read_exact(&mut buffer).unwrap();

    assert_eq!(buffer, MAGIC_HEADER);

    cleanup_file(&compressed_file);
}

#[test]
fn test_compress_roundtrip() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("roundtrip_test.txt");
    let compressed_file = test_dir.join("roundtrip_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    compress_test(&[test_file.to_str().unwrap()], "", "");

    // File should be replaced by .Z version
    assert!(!test_file.exists(), "Original file should be deleted");
    assert!(compressed_file.exists(), "Compressed file should exist");

    // Uncompress with -c to stdout
    uncompress_test(
        &["-c", compressed_file.to_str().unwrap()],
        &original_content,
        "",
    );

    cleanup_file(&compressed_file);
}

// =============================================================================
// stdin/stdout tests
// =============================================================================

#[test]
fn test_compress_stdin_produces_output() {
    let compressed = compress_stdin_test("Hello World! This is a test of stdin compression.\n");
    assert!(
        compressed.len() >= 3,
        "Compressed output should have header"
    );
    assert_eq!(compressed[0], 0x1F);
    assert_eq!(compressed[1], 0x9D);
}

#[test]
fn test_compress_stdin_small_data() {
    // Even small data that expands should produce output when using stdin
    let compressed = compress_stdin_test("ab");
    assert!(
        !compressed.is_empty(),
        "Should produce output even for small data"
    );
}

#[test]
fn test_compress_stdout_flag() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");

    let mut expected = String::new();
    File::open(&source_file)
        .unwrap()
        .read_to_string(&mut expected)
        .unwrap();

    // Compress to stdout and pipe to uncompress
    let str_args: Vec<String> = vec![
        String::from("-c"),
        source_file.to_str().unwrap().to_string(),
    ];

    run_test_with_checker(
        TestPlan {
            cmd: String::from("compress"),
            args: str_args.clone(),
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(
                !output.stdout.is_empty(),
                "compress -c should produce output"
            );
        },
    );
}

// =============================================================================
// -b bits option tests
// =============================================================================

#[test]
fn test_compress_bits_9() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("bits9_test.txt");
    let compressed_file = test_dir.join("bits9_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    compress_test(&["-b", "9", test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "Compressed file should exist");

    // Verify header has correct bits value
    let mut file = File::open(&compressed_file).unwrap();
    let mut header = [0u8; 3];
    file.read_exact(&mut header).unwrap();
    assert_eq!(header[0], 0x1F);
    assert_eq!(header[1], 0x9D);
    // bits are stored in lower 5 bits of byte 2
    let stored_bits = header[2] & 0x1F;
    assert_eq!(stored_bits, 9);

    cleanup_file(&compressed_file);
}

#[test]
fn test_compress_bits_14() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("bits14_test.txt");
    let compressed_file = test_dir.join("bits14_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    compress_test(&["-b", "14", test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "Compressed file should exist");

    let mut file = File::open(&compressed_file).unwrap();
    let mut header = [0u8; 3];
    file.read_exact(&mut header).unwrap();
    let stored_bits = header[2] & 0x1F;
    assert_eq!(stored_bits, 14);

    cleanup_file(&compressed_file);
}

// =============================================================================
// Force mode tests
// =============================================================================

#[test]
fn test_compress_force_small_file() {
    let test_dir = get_test_dir();
    let test_file = test_dir.join("force_small_test.txt");
    let compressed_file = test_dir.join("force_small_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    // Create a very small file that would expand
    fs::write(&test_file, "ab").unwrap();

    // Without -f, should exit with code 2 (would expand)
    compress_test_with_exit(&[test_file.to_str().unwrap()], 2);

    // Original file should still exist
    assert!(test_file.exists(), "Original file should remain");
    assert!(!compressed_file.exists(), "No .Z file should be created");

    // With -f, should compress anyway
    compress_test(&["-f", test_file.to_str().unwrap()], "", "");
    assert!(
        compressed_file.exists(),
        "Compressed file should exist with -f"
    );

    cleanup_file(&compressed_file);
}

// =============================================================================
// File output mode tests (uncompress)
// =============================================================================

#[test]
fn test_uncompress_file_output() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("file_output_test.txt");
    let compressed_file = test_dir.join("file_output_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    compress_test(&[test_file.to_str().unwrap()], "", "");

    // Now uncompress to file (not stdout)
    let str_args: Vec<String> = vec![
        String::from("-f"),
        compressed_file.to_str().unwrap().to_string(),
    ];

    run_test(TestPlan {
        cmd: String::from("uncompress"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // .Z file should be removed, original recreated
    assert!(
        !compressed_file.exists(),
        "Compressed file should be deleted"
    );
    assert!(test_file.exists(), "Decompressed file should exist");

    let mut decompressed_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut decompressed_content)
        .unwrap();

    assert_eq!(original_content, decompressed_content);

    cleanup_file(&test_file);
}

#[test]
fn test_uncompress_finds_z_file() {
    // Test that uncompress finds .Z file when given filename without .Z
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("find_z_test.txt");
    let compressed_file = test_dir.join("find_z_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    compress_test(&[test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "Compressed file should exist");

    // Uncompress with filename without .Z - should find the .Z file
    let str_args: Vec<String> = vec![
        String::from("-f"),
        test_file.to_str().unwrap().to_string(), // Note: no .Z
    ];

    run_test(TestPlan {
        cmd: String::from("uncompress"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(test_file.exists(), "Decompressed file should exist");
    assert!(
        !compressed_file.exists(),
        "Compressed file should be removed"
    );

    cleanup_file(&test_file);
}

// =============================================================================
// Error handling tests
// =============================================================================

#[test]
fn test_compress_missing_file() {
    let str_args: Vec<String> = vec![String::from("/nonexistent/file/path.txt")];

    run_test_with_checker(
        TestPlan {
            cmd: String::from("compress"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            // Should have error output
            assert!(!output.stderr.is_empty(), "Should have error message");
            assert!(output.stdout.is_empty(), "Should have no stdout");
        },
    );
}

#[test]
fn test_uncompress_missing_file() {
    let str_args: Vec<String> = vec![String::from("/nonexistent/file/path.Z")];

    run_test_with_checker(
        TestPlan {
            cmd: String::from("uncompress"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            // Should have error output
            assert!(!output.stderr.is_empty(), "Should have error message");
            assert!(output.stdout.is_empty(), "Should have no stdout");
        },
    );
}
