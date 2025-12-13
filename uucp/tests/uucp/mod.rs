//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::fs;
use std::io::Write;
use std::process::Output;

fn uucp_test(args: &[&str], expected_out: &str, expected_err: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("uucp"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_out),
        expected_err: String::from(expected_err),
        expected_exit_code,
    });
}

fn uucp_test_with_checker<F: FnMut(&TestPlan, &Output)>(args: &[&str], checker: F) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("uucp"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        checker,
    );
}

#[test]
fn test_uucp_no_args() {
    // No arguments should show usage and fail
    uucp_test_with_checker(&[], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("Usage:") || stderr.contains("usage:"),
            "Expected usage message in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uucp_one_arg() {
    // Only one argument should fail (need source and dest)
    uucp_test_with_checker(&["onlyfile.txt"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("Usage:") || stderr.contains("usage:"),
            "Expected usage message in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uucp_invalid_option() {
    uucp_test_with_checker(&["-Z", "src", "dst"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("unexpected argument") || stderr.contains("unknown option"),
            "Expected error about invalid option in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uucp_local_to_local() {
    let test_dir = &format!("{}/test_uucp_local_local", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/source.txt", test_dir);
    let dest = &format!("{}/dest.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    {
        let mut file = fs::File::create(source).unwrap();
        file.write_all(b"Hello, UUCP!").unwrap();
    }

    // Test
    uucp_test(&[source, dest], "", "", 0);

    // Verify
    assert!(std::path::Path::new(dest).exists());
    let content = fs::read_to_string(dest).unwrap();
    assert_eq!(content, "Hello, UUCP!");

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_local_to_local_with_j() {
    let test_dir = &format!("{}/test_uucp_local_j", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/source.txt", test_dir);
    let dest = &format!("{}/dest.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    {
        let mut file = fs::File::create(source).unwrap();
        file.write_all(b"test content").unwrap();
    }

    // Test with -j flag
    uucp_test_with_checker(&["-j", source, dest], |_, output| {
        assert!(output.status.success());
        // Should print a job ID
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(!stdout.trim().is_empty(), "Should print job ID with -j");
        // File should be copied
        assert!(std::path::Path::new(dest).exists());
    });

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_local_to_local_dir() {
    let test_dir = &format!("{}/test_uucp_local_dir", env!("CARGO_TARGET_TMPDIR"));
    let file1 = &format!("{}/file1.txt", test_dir);
    let file2 = &format!("{}/file2.txt", test_dir);
    let dest_dir = &format!("{}/destdir/", test_dir);
    let dest_file1 = &format!("{}/destdir/file1.txt", test_dir);
    let dest_file2 = &format!("{}/destdir/file2.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    fs::create_dir_all(dest_dir).unwrap();
    {
        let mut f1 = fs::File::create(file1).unwrap();
        f1.write_all(b"content 1").unwrap();
        let mut f2 = fs::File::create(file2).unwrap();
        f2.write_all(b"content 2").unwrap();
    }

    // Test
    uucp_test(&[file1, file2, dest_dir], "", "", 0);

    // Verify
    assert!(std::path::Path::new(dest_file1).exists());
    assert!(std::path::Path::new(dest_file2).exists());
    assert_eq!(fs::read_to_string(dest_file1).unwrap(), "content 1");
    assert_eq!(fs::read_to_string(dest_file2).unwrap(), "content 2");

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_create_dest_dir() {
    let test_dir = &format!("{}/test_uucp_create_dir", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/source.txt", test_dir);
    let dest = &format!("{}/newdir/subdir/dest.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    {
        let mut file = fs::File::create(source).unwrap();
        file.write_all(b"test").unwrap();
    }

    // Test - should create directories with -d default
    uucp_test(&[source, dest], "", "", 0);

    // Verify
    assert!(std::path::Path::new(dest).exists());

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_no_create_dir_with_f() {
    let test_dir = &format!("{}/test_uucp_no_create", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/source.txt", test_dir);
    let dest = &format!("{}/nonexistent/dest.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    {
        let mut file = fs::File::create(source).unwrap();
        file.write_all(b"test").unwrap();
    }

    // Test with -f, should fail if directory doesn't exist
    uucp_test_with_checker(&["-f", source, dest], |_, output| {
        assert!(!output.status.success());
    });

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_source_not_found() {
    let test_dir = &format!("{}/test_uucp_not_found", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/nonexistent.txt", test_dir);
    let dest = &format!("{}/dest.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();

    // Test
    uucp_test_with_checker(&[source, dest], |_, output| {
        assert!(!output.status.success());
    });

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_route_not_supported() {
    let test_dir = &format!("{}/test_uucp_route", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/source.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    {
        let mut file = fs::File::create(source).unwrap();
        file.write_all(b"test").unwrap();
    }

    // Test multi-hop routes
    uucp_test_with_checker(&[source, "host1!host2!dest.txt"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("route"),
            "Expected 'route' in stderr, got: {}",
            stderr
        );
    });

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_preserve_content() {
    let test_dir = &format!("{}/test_uucp_preserve", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/binary.dat", test_dir);
    let dest = &format!("{}/copy.dat", test_dir);

    // Setup with binary-like content
    fs::create_dir_all(test_dir).unwrap();
    let content = "Line 1\nLine 2\n\tTabbed\n";
    {
        let mut file = fs::File::create(source).unwrap();
        file.write_all(content.as_bytes()).unwrap();
    }

    // Test
    uucp_test(&[source, dest], "", "", 0);

    // Verify
    assert_eq!(fs::read_to_string(dest).unwrap(), content);

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_multiple_sources() {
    let test_dir = &format!("{}/test_uucp_multi_src", env!("CARGO_TARGET_TMPDIR"));
    let file_a = &format!("{}/a.txt", test_dir);
    let file_b = &format!("{}/b.txt", test_dir);
    let file_c = &format!("{}/c.txt", test_dir);
    let output_dir = &format!("{}/output/", test_dir);
    let output_a = &format!("{}/output/a.txt", test_dir);
    let output_b = &format!("{}/output/b.txt", test_dir);
    let output_c = &format!("{}/output/c.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    fs::create_dir_all(output_dir).unwrap();
    fs::write(file_a, "A").unwrap();
    fs::write(file_b, "B").unwrap();
    fs::write(file_c, "C").unwrap();

    // Test
    uucp_test(&[file_a, file_b, file_c, output_dir], "", "", 0);

    // Verify
    assert_eq!(fs::read_to_string(output_a).unwrap(), "A");
    assert_eq!(fs::read_to_string(output_b).unwrap(), "B");
    assert_eq!(fs::read_to_string(output_c).unwrap(), "C");

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_overwrite() {
    let test_dir = &format!("{}/test_uucp_overwrite", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/source.txt", test_dir);
    let dest = &format!("{}/dest.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    fs::write(source, "new content").unwrap();
    fs::write(dest, "old content").unwrap();

    // Test
    uucp_test(&[source, dest], "", "", 0);

    // Verify
    assert_eq!(fs::read_to_string(dest).unwrap(), "new content");

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_c_option_accepted() {
    let test_dir = &format!("{}/test_uucp_c_opt", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/source.txt", test_dir);
    let dest = &format!("{}/dest.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    fs::write(source, "test").unwrap();

    // Test -c (default, should be accepted)
    uucp_test(&["-c", source, dest], "", "", 0);

    // Verify
    assert!(std::path::Path::new(dest).exists());

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_uucp_combined_options() {
    let test_dir = &format!("{}/test_uucp_combined", env!("CARGO_TARGET_TMPDIR"));
    let source = &format!("{}/source.txt", test_dir);
    let dest = &format!("{}/newdir/dest.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();
    fs::write(source, "test").unwrap();

    // Test combined options
    uucp_test_with_checker(&["-jd", source, dest], |_, output| {
        assert!(output.status.success());
        // Should print job ID
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(!stdout.trim().is_empty());
        // Should create directory and copy
        assert!(std::path::Path::new(dest).exists());
    });

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}
