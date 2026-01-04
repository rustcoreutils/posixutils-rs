//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Compression integration tests for pax

use std::fs::{self, File};
use std::io::Write;
use std::process::{Command, Output};
use tempfile::TempDir;

fn pax_binary() -> String {
    env!("CARGO_BIN_EXE_pax").to_string()
}

fn assert_success(output: &Output, context: &str) {
    if !output.status.success() {
        eprintln!("=== STDOUT ===");
        eprintln!("{}", String::from_utf8_lossy(&output.stdout));
        eprintln!("=== STDERR ===");
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        panic!("{} failed with status: {:?}", context, output.status);
    }
}

fn assert_failure(output: &Output, context: &str) {
    if output.status.success() {
        eprintln!("=== STDOUT ===");
        eprintln!("{}", String::from_utf8_lossy(&output.stdout));
        eprintln!("=== STDERR ===");
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        panic!("{} should have failed but succeeded", context);
    }
}

#[test]
fn test_gzip_roundtrip() {
    let dir = TempDir::new().unwrap();
    let src_dir = dir.path().join("src");
    let dst_dir = dir.path().join("dst");
    let archive = dir.path().join("test.tar.gz");

    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&dst_dir).unwrap();

    // Create test files
    File::create(src_dir.join("hello.txt"))
        .unwrap()
        .write_all(b"Hello, World!")
        .unwrap();
    File::create(src_dir.join("test.txt"))
        .unwrap()
        .write_all(b"Test file content")
        .unwrap();

    // Create gzip archive with pax
    let output = Command::new(pax_binary())
        .args(["-w", "-z", "-x", "ustar", "-f"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax write gzip");

    // Verify the file is actually gzip (magic bytes)
    let archive_data = fs::read(&archive).unwrap();
    assert!(
        archive_data.len() >= 2 && archive_data[0] == 0x1f && archive_data[1] == 0x8b,
        "Archive should have gzip magic bytes"
    );

    // Extract with pax (auto-detect gzip)
    let output = Command::new(pax_binary())
        .args(["-r", "-f"])
        .arg(&archive)
        .current_dir(&dst_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax read gzip");

    // Verify files were extracted
    assert!(dst_dir.join("hello.txt").exists());
    assert!(dst_dir.join("test.txt").exists());
    assert_eq!(
        fs::read_to_string(dst_dir.join("hello.txt")).unwrap(),
        "Hello, World!"
    );
}

#[test]
fn test_gzip_auto_detect() {
    let dir = TempDir::new().unwrap();
    let src_dir = dir.path().join("src");
    let archive = dir.path().join("test.tar.gz");

    fs::create_dir_all(&src_dir).unwrap();

    File::create(src_dir.join("file.txt"))
        .unwrap()
        .write_all(b"Auto-detect test")
        .unwrap();

    // Create gzip archive
    let output = Command::new(pax_binary())
        .args(["-w", "-z", "-f"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax write gzip");

    // List without -z flag (should auto-detect)
    let output = Command::new(pax_binary())
        .args(["-f"])
        .arg(&archive)
        .output()
        .unwrap();
    assert_success(&output, "pax list auto-detect");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("file.txt"), "Should list file.txt");
}

#[test]
fn test_gzip_with_ustar() {
    let dir = TempDir::new().unwrap();
    let src_dir = dir.path().join("src");
    let archive = dir.path().join("test.tar.gz");

    fs::create_dir_all(&src_dir).unwrap();

    File::create(src_dir.join("ustar.txt"))
        .unwrap()
        .write_all(b"Ustar format test")
        .unwrap();

    // Create gzip archive with explicit ustar format
    let output = Command::new(pax_binary())
        .args(["-w", "-z", "-x", "ustar", "-f"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax write gzip ustar");

    // List and verify
    let output = Command::new(pax_binary())
        .args(["-f"])
        .arg(&archive)
        .output()
        .unwrap();
    assert_success(&output, "pax list gzip ustar");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("ustar.txt"));
}

#[test]
fn test_gzip_with_cpio() {
    let dir = TempDir::new().unwrap();
    let src_dir = dir.path().join("src");
    let archive = dir.path().join("test.cpio.gz");

    fs::create_dir_all(&src_dir).unwrap();

    File::create(src_dir.join("cpio.txt"))
        .unwrap()
        .write_all(b"Cpio format test")
        .unwrap();

    // Create gzip archive with cpio format
    let output = Command::new(pax_binary())
        .args(["-w", "-z", "-x", "cpio", "-f"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax write gzip cpio");

    // List and verify
    let output = Command::new(pax_binary())
        .args(["-f"])
        .arg(&archive)
        .output()
        .unwrap();
    assert_success(&output, "pax list gzip cpio");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("cpio.txt"));
}

#[test]
fn test_gzip_append_error() {
    let dir = TempDir::new().unwrap();
    let archive = dir.path().join("test.tar.gz");

    // Try to use -z with -a (should fail)
    let output = Command::new(pax_binary())
        .args(["-w", "-a", "-z", "-f"])
        .arg(&archive)
        .arg(".")
        .current_dir(dir.path())
        .output()
        .unwrap();
    assert_failure(&output, "pax -z -a");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("incompatible"),
        "Error should mention incompatibility"
    );
}

#[test]
fn test_gzip_verbose() {
    let dir = TempDir::new().unwrap();
    let src_dir = dir.path().join("src");
    let archive = dir.path().join("test.tar.gz");

    fs::create_dir_all(&src_dir).unwrap();

    File::create(src_dir.join("verbose.txt"))
        .unwrap()
        .write_all(b"Verbose test")
        .unwrap();

    // Create gzip archive
    let output = Command::new(pax_binary())
        .args(["-w", "-z", "-f"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax write gzip");

    // List with verbose flag
    let output = Command::new(pax_binary())
        .args(["-v", "-f"])
        .arg(&archive)
        .output()
        .unwrap();
    assert_success(&output, "pax list verbose");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("verbose.txt"));
    // Verbose output should have permissions, size, etc.
    assert!(stdout.contains("rw"), "Should show permissions");
}

#[test]
fn test_gzip_system_gunzip_compat() {
    let dir = TempDir::new().unwrap();
    let src_dir = dir.path().join("src");
    let archive = dir.path().join("test.tar.gz");

    fs::create_dir_all(&src_dir).unwrap();

    File::create(src_dir.join("compat.txt"))
        .unwrap()
        .write_all(b"System compatibility test")
        .unwrap();

    // Create gzip archive with pax
    let output = Command::new(pax_binary())
        .args(["-w", "-z", "-x", "ustar", "-f"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax write gzip");

    // Try to list with system tar (if available)
    let output = Command::new("tar").args(["-tzf"]).arg(&archive).output();

    if let Ok(output) = output {
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("compat.txt"),
                "System tar should read pax gzip archive"
            );
        } else {
            eprintln!("System tar -tzf failed, skipping compatibility check");
        }
    } else {
        eprintln!("System tar not available, skipping compatibility check");
    }
}

#[test]
fn test_gzip_read_system_tar_output() {
    let dir = TempDir::new().unwrap();
    let src_dir = dir.path().join("src");
    let archive = dir.path().join("sys.tar.gz");
    let dst_dir = dir.path().join("dst");

    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&dst_dir).unwrap();

    File::create(src_dir.join("sysfile.txt"))
        .unwrap()
        .write_all(b"Created by system tar")
        .unwrap();

    // Create gzip archive with system tar (if available)
    let output = Command::new("tar")
        .args(["-czf"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output();

    if output.is_err() || !output.as_ref().unwrap().status.success() {
        eprintln!("System tar not available or failed, skipping test");
        return;
    }

    // List with pax (should auto-detect gzip)
    let output = Command::new(pax_binary())
        .args(["-f"])
        .arg(&archive)
        .output()
        .unwrap();
    assert_success(&output, "pax list system tar gzip");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("sysfile.txt"),
        "pax should list system tar gzip archive"
    );

    // Extract with pax
    let output = Command::new(pax_binary())
        .args(["-r", "-f"])
        .arg(&archive)
        .current_dir(&dst_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax extract system tar gzip");

    assert!(dst_dir.join("sysfile.txt").exists());
    assert_eq!(
        fs::read_to_string(dst_dir.join("sysfile.txt")).unwrap(),
        "Created by system tar"
    );
}

#[test]
fn test_gzip_explicit_flag() {
    let dir = TempDir::new().unwrap();
    let src_dir = dir.path().join("src");
    let archive = dir.path().join("test.tar.gz");

    fs::create_dir_all(&src_dir).unwrap();

    File::create(src_dir.join("explicit.txt"))
        .unwrap()
        .write_all(b"Explicit -z flag test")
        .unwrap();

    // Create gzip archive
    let output = Command::new(pax_binary())
        .args(["-w", "-z", "-f"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax write gzip");

    // List with explicit -z flag
    let output = Command::new(pax_binary())
        .args(["-z", "-f"])
        .arg(&archive)
        .output()
        .unwrap();
    assert_success(&output, "pax list with -z");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("explicit.txt"));
}

#[test]
fn test_gzip_extract_explicit_flag() {
    let dir = TempDir::new().unwrap();
    let src_dir = dir.path().join("src");
    let dst_dir = dir.path().join("dst");
    let archive = dir.path().join("test.tar.gz");

    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&dst_dir).unwrap();

    File::create(src_dir.join("extract.txt"))
        .unwrap()
        .write_all(b"Extract with -z flag")
        .unwrap();

    // Create gzip archive
    let output = Command::new(pax_binary())
        .args(["-w", "-z", "-f"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax write gzip");

    // Extract with explicit -z flag
    let output = Command::new(pax_binary())
        .args(["-r", "-z", "-f"])
        .arg(&archive)
        .current_dir(&dst_dir)
        .output()
        .unwrap();
    assert_success(&output, "pax extract with -z");

    assert!(dst_dir.join("extract.txt").exists());
}
