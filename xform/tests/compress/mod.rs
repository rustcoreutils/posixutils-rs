//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::{
    fs::{self, remove_file, File},
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

fn compress_stdin_gzip_test(stdin_data: &str) -> Vec<u8> {
    let str_args: Vec<String> = vec![String::from("-c"), String::from("-g")];
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
            assert!(
                !output.stdout.is_empty(),
                "compress -g should produce output"
            );
            assert!(
                output.stdout.len() >= 2,
                "compress -g output too short for magic header"
            );
            // Check gzip magic header
            assert_eq!(output.stdout[0], 0x1F, "Wrong gzip magic byte 0");
            assert_eq!(output.stdout[1], 0x8B, "Wrong gzip magic byte 1");
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
// Basic LZW functionality tests
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

    // Decompress with -c -d to stdout
    let str_args: Vec<String> = vec![
        String::from("-c"),
        String::from("-d"),
        compressed_file.to_str().unwrap().to_string(),
    ];

    run_test(TestPlan {
        cmd: String::from("compress"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: original_content,
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    cleanup_file(&compressed_file);
}

// =============================================================================
// Gzip/DEFLATE tests
// =============================================================================

#[test]
fn test_compress_gzip_magic_header() {
    const GZIP_MAGIC: [u8; 2] = [0x1F, 0x8B];

    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("gzip_magic_test.txt");
    let compressed_file = test_dir.join("gzip_magic_test.txt.gz");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    compress_test(&["-g", test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "Compressed .gz file should exist");

    let mut file = File::open(&compressed_file).unwrap();
    let mut buffer = vec![0; GZIP_MAGIC.len()];
    file.read_exact(&mut buffer).unwrap();

    assert_eq!(buffer, GZIP_MAGIC);

    cleanup_file(&compressed_file);
}

#[test]
fn test_compress_gzip_roundtrip() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("gzip_roundtrip_test.txt");
    let compressed_file = test_dir.join("gzip_roundtrip_test.txt.gz");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    // Compress with gzip
    compress_test(&["-g", test_file.to_str().unwrap()], "", "");

    assert!(!test_file.exists(), "Original file should be deleted");
    assert!(compressed_file.exists(), "Compressed .gz file should exist");

    // Decompress with -c -d
    let str_args: Vec<String> = vec![
        String::from("-c"),
        String::from("-d"),
        compressed_file.to_str().unwrap().to_string(),
    ];

    run_test(TestPlan {
        cmd: String::from("compress"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: original_content,
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    cleanup_file(&compressed_file);
}

#[test]
fn test_compress_m_deflate() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("m_deflate_test.txt");
    let compressed_file = test_dir.join("m_deflate_test.txt.gz");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    // Use -m deflate
    compress_test(&["-m", "deflate", test_file.to_str().unwrap()], "", "");

    assert!(
        compressed_file.exists(),
        "-m deflate should create .gz file"
    );

    cleanup_file(&compressed_file);
}

#[test]
fn test_compress_m_gzip() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("m_gzip_test.txt");
    let compressed_file = test_dir.join("m_gzip_test.txt.gz");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    // Use -m gzip
    compress_test(&["-m", "gzip", test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "-m gzip should create .gz file");

    cleanup_file(&compressed_file);
}

#[test]
fn test_compress_m_lzw() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("m_lzw_test.txt");
    let compressed_file = test_dir.join("m_lzw_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    // Explicit -m lzw
    compress_test(&["-m", "lzw", test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "-m lzw should create .Z file");

    cleanup_file(&compressed_file);
}

// =============================================================================
// -d decompress option tests
// =============================================================================

#[test]
fn test_compress_d_option() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("d_option_test.txt");
    let compressed_file = test_dir.join("d_option_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    // Compress
    compress_test(&[test_file.to_str().unwrap()], "", "");
    assert!(compressed_file.exists());

    // Decompress using -d -f (file replacement)
    compress_test(&["-d", "-f", compressed_file.to_str().unwrap()], "", "");

    assert!(test_file.exists(), "Decompressed file should exist");
    assert!(!compressed_file.exists(), ".Z file should be removed");

    let mut decompressed_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut decompressed_content)
        .unwrap();

    assert_eq!(original_content, decompressed_content);

    cleanup_file(&test_file);
}

#[test]
fn test_decompress_finds_z_file() {
    // Test that decompress finds .Z file when given filename without .Z
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("find_z_test.txt");
    let compressed_file = test_dir.join("find_z_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    compress_test(&[test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "Compressed file should exist");

    // Decompress with filename without .Z - should find the .Z file
    compress_test(&["-d", "-f", test_file.to_str().unwrap()], "", "");

    assert!(test_file.exists(), "Decompressed file should exist");
    assert!(
        !compressed_file.exists(),
        "Compressed file should be removed"
    );

    cleanup_file(&test_file);
}

#[test]
fn test_decompress_finds_gz_file() {
    // Test that decompress finds .gz file when given filename without .gz
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("find_gz_test.txt");
    let compressed_file = test_dir.join("find_gz_test.txt.gz");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    // Compress with gzip
    compress_test(&["-g", test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "Compressed .gz file should exist");

    // Decompress with filename without .gz - should find the .gz file
    compress_test(&["-d", "-f", test_file.to_str().unwrap()], "", "");

    assert!(test_file.exists(), "Decompressed file should exist");
    assert!(
        !compressed_file.exists(),
        "Compressed .gz file should be removed"
    );

    cleanup_file(&test_file);
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
fn test_compress_gzip_stdin() {
    let compressed = compress_stdin_gzip_test("Hello World! This is a test of gzip compression.\n");
    assert!(compressed.len() >= 10, "Gzip output should have header");
    assert_eq!(compressed[0], 0x1F);
    assert_eq!(compressed[1], 0x8B);
}

#[test]
fn test_compress_stdout_flag() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");

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
// -b bits/level option tests
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

#[test]
fn test_compress_gzip_level() {
    // Test -b with gzip (compression level)
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("gzip_level_test.txt");
    let compressed_file = test_dir.join("gzip_level_test.txt.gz");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    // Use -g with -b 9 (maximum compression)
    compress_test(&["-g", "-b", "9", test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "Compressed .gz file should exist");

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
// Algorithm auto-detection tests
// =============================================================================

#[test]
fn test_auto_detect_lzw() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("auto_lzw_test.txt");
    let compressed_file = test_dir.join("auto_lzw_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    // Compress with LZW
    compress_test(&[test_file.to_str().unwrap()], "", "");

    // Decompress - should auto-detect LZW from magic bytes
    let str_args: Vec<String> = vec![
        String::from("-c"),
        String::from("-d"),
        compressed_file.to_str().unwrap().to_string(),
    ];

    run_test(TestPlan {
        cmd: String::from("compress"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: original_content,
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    cleanup_file(&compressed_file);
}

#[test]
fn test_auto_detect_gzip() {
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("auto_gzip_test.txt");
    let compressed_file = test_dir.join("auto_gzip_test.txt.gz");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    // Compress with gzip
    compress_test(&["-g", test_file.to_str().unwrap()], "", "");

    // Decompress - should auto-detect gzip from magic bytes
    let str_args: Vec<String> = vec![
        String::from("-c"),
        String::from("-d"),
        compressed_file.to_str().unwrap().to_string(),
    ];

    run_test(TestPlan {
        cmd: String::from("compress"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: original_content,
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    cleanup_file(&compressed_file);
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
fn test_decompress_missing_file() {
    let str_args: Vec<String> = vec![String::from("-d"), String::from("/nonexistent/file/path.Z")];

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
fn test_invalid_algorithm() {
    let str_args: Vec<String> = vec![
        String::from("-m"),
        String::from("invalid_algo"),
        String::from("/dev/null"),
    ];

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
            assert!(
                !output.stderr.is_empty(),
                "Should have error for invalid algorithm"
            );
        },
    );
}

// =============================================================================
// Warning tests
// =============================================================================

#[test]
fn test_warn_z_suffix_on_compress() {
    let test_dir = get_test_dir();
    let test_file = test_dir.join("already_compressed.Z");
    let double_compressed = test_dir.join("already_compressed.Z.Z");

    cleanup_file(&test_file);
    cleanup_file(&double_compressed);

    // Create a fake .Z file
    fs::write(&test_file, "test data").unwrap();

    let str_args: Vec<String> = vec![String::from("-f"), test_file.to_str().unwrap().to_string()];

    run_test_with_checker(
        TestPlan {
            cmd: String::from("compress"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // Should warn about .Z suffix
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("already has") && stderr.contains("suffix"),
                "Should warn about existing .Z suffix: {}",
                stderr
            );
        },
    );

    cleanup_file(&test_file);
    cleanup_file(&double_compressed);
}

// =============================================================================
// Symlink invocation tests (zcat, uncompress)
// =============================================================================

#[test]
fn test_zcat_lzw() {
    // Test zcat symlink with LZW (.Z) format
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("zcat_lzw_test.txt");
    let compressed_file = test_dir.join("zcat_lzw_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    // Compress with LZW (default)
    compress_test(&[test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "Compressed .Z file should exist");

    // Use zcat to decompress to stdout
    run_test(TestPlan {
        cmd: String::from("zcat"),
        args: vec![compressed_file.to_str().unwrap().to_string()],
        stdin_data: String::new(),
        expected_out: original_content,
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    cleanup_file(&compressed_file);
}

#[test]
fn test_zcat_gzip() {
    // Test zcat symlink with gzip (.gz) format
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("zcat_gzip_test.txt");
    let compressed_file = test_dir.join("zcat_gzip_test.txt.gz");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    // Compress with gzip
    compress_test(&["-g", test_file.to_str().unwrap()], "", "");

    assert!(compressed_file.exists(), "Compressed .gz file should exist");

    // Use zcat to decompress to stdout
    run_test(TestPlan {
        cmd: String::from("zcat"),
        args: vec![compressed_file.to_str().unwrap().to_string()],
        stdin_data: String::new(),
        expected_out: original_content,
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    cleanup_file(&compressed_file);
}

#[test]
fn test_uncompress_lzw() {
    // Test uncompress symlink with LZW (.Z) format - in-place decompression
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("uncompress_lzw_test.txt");
    let compressed_file = test_dir.join("uncompress_lzw_test.txt.Z");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    // Compress with LZW (default)
    compress_test(&[test_file.to_str().unwrap()], "", "");

    assert!(!test_file.exists(), "Original file should be deleted");
    assert!(compressed_file.exists(), "Compressed .Z file should exist");

    // Use uncompress to decompress in-place
    run_test(TestPlan {
        cmd: String::from("uncompress"),
        args: vec![compressed_file.to_str().unwrap().to_string()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Verify original file is restored
    assert!(test_file.exists(), "Decompressed file should exist");
    assert!(
        !compressed_file.exists(),
        "Compressed file should be removed"
    );

    let mut decompressed_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut decompressed_content)
        .unwrap();

    assert_eq!(original_content, decompressed_content);

    cleanup_file(&test_file);
}

#[test]
fn test_uncompress_gzip() {
    // Test uncompress symlink with gzip (.gz) format - in-place decompression
    let test_dir = get_test_dir();
    let source_file = test_dir.join("lorem_ipsum.txt");
    let test_file = test_dir.join("uncompress_gzip_test.txt");
    let compressed_file = test_dir.join("uncompress_gzip_test.txt.gz");

    cleanup_file(&test_file);
    cleanup_file(&compressed_file);

    fs::copy(&source_file, &test_file).unwrap();

    let mut original_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut original_content)
        .unwrap();

    // Compress with gzip
    compress_test(&["-g", test_file.to_str().unwrap()], "", "");

    assert!(!test_file.exists(), "Original file should be deleted");
    assert!(compressed_file.exists(), "Compressed .gz file should exist");

    // Use uncompress to decompress in-place
    run_test(TestPlan {
        cmd: String::from("uncompress"),
        args: vec![compressed_file.to_str().unwrap().to_string()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Verify original file is restored
    assert!(test_file.exists(), "Decompressed file should exist");
    assert!(
        !compressed_file.exists(),
        "Compressed file should be removed"
    );

    let mut decompressed_content = String::new();
    File::open(&test_file)
        .unwrap()
        .read_to_string(&mut decompressed_content)
        .unwrap();

    assert_eq!(original_content, decompressed_content);

    cleanup_file(&test_file);
}
