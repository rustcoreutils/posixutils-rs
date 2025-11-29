//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Special file tests (FIFO, block device, character device)

use crate::common::*;
use std::ffi::CString;
use std::fs;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::FileTypeExt;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn test_fifo_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("fifo.tar");
    let dst_dir = temp.path().join("dest");

    // Create source directory with FIFO
    fs::create_dir(&src_dir).unwrap();
    let fifo_path = src_dir.join("myfifo");
    let path_cstr = CString::new(fifo_path.as_os_str().as_bytes()).unwrap();
    unsafe {
        let ret = libc::mkfifo(path_cstr.as_ptr(), 0o644);
        if ret != 0 {
            eprintln!("Skipping FIFO test: mkfifo failed");
            return;
        }
    }

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write fifo");

    // List archive and verify FIFO is present
    let output = run_pax(&["-v", "-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list fifo");

    let listing = stdout_str(&output);
    assert!(
        listing.contains("myfifo"),
        "FIFO should be in listing: {}",
        listing
    );
    // Verbose listing should show 'p' for FIFO
    assert!(
        listing.contains("p"),
        "Verbose listing should show 'p' for FIFO: {}",
        listing
    );

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read fifo");

    // Verify FIFO was extracted
    let extracted_fifo = dst_dir.join("myfifo");
    let meta = fs::symlink_metadata(&extracted_fifo).unwrap();
    assert!(
        meta.file_type().is_fifo(),
        "Extracted file should be a FIFO"
    );
}

/// Test block device archiving and listing (requires root for mknod and extraction)
#[cfg(all(unix, feature = "requires_root"))]
#[test]
fn test_block_device_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("blkdev.tar");
    let dst_dir = temp.path().join("dest");

    // Create source directory with block device
    fs::create_dir(&src_dir).unwrap();
    let dev_path = src_dir.join("myblock");
    let path_cstr = CString::new(dev_path.as_os_str().as_bytes()).unwrap();
    // makedev has different signatures on different platforms
    #[cfg(target_os = "macos")]
    let dev = libc::makedev(8i32, 0i32); // /dev/sda major=8, minor=0
    #[cfg(not(target_os = "macos"))]
    let dev = libc::makedev(8u32, 0u32); // /dev/sda major=8, minor=0
    unsafe {
        let ret = libc::mknod(path_cstr.as_ptr(), libc::S_IFBLK | 0o660, dev);
        if ret != 0 {
            let err = std::io::Error::last_os_error();
            eprintln!("Skipping block device test: mknod failed: {}", err);
            return;
        }
    }

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write block device");

    // List archive and verify device is present
    let output = run_pax(&["-v", "-f", archive.to_str().unwrap()]);
    let listing = stdout_str(&output);
    assert!(
        listing.contains("myblock"),
        "Block device should be in listing"
    );
    // Verbose listing should show 'b' for block device
    assert!(
        listing.contains("b"),
        "Verbose listing should show 'b' for block device"
    );

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read block device");

    // Verify block device was extracted
    let extracted_dev = dst_dir.join("myblock");
    let meta = fs::symlink_metadata(&extracted_dev).unwrap();
    assert!(
        meta.file_type().is_block_device(),
        "Extracted file should be a block device"
    );
}

/// Test character device archiving and listing (requires root for mknod and extraction)
#[cfg(all(unix, feature = "requires_root"))]
#[test]
fn test_char_device_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("chrdev.tar");
    let dst_dir = temp.path().join("dest");

    // Create source directory with character device
    fs::create_dir(&src_dir).unwrap();
    let dev_path = src_dir.join("mychar");
    let path_cstr = CString::new(dev_path.as_os_str().as_bytes()).unwrap();
    // makedev has different signatures on different platforms
    #[cfg(target_os = "macos")]
    let dev = libc::makedev(1i32, 3i32); // /dev/null major=1, minor=3
    #[cfg(not(target_os = "macos"))]
    let dev = libc::makedev(1u32, 3u32); // /dev/null major=1, minor=3
    unsafe {
        let ret = libc::mknod(path_cstr.as_ptr(), libc::S_IFCHR | 0o666, dev);
        if ret != 0 {
            let err = std::io::Error::last_os_error();
            eprintln!("Skipping char device test: mknod failed: {}", err);
            return;
        }
    }

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write char device");

    // List archive and verify device is present
    let output = run_pax(&["-v", "-f", archive.to_str().unwrap()]);
    let listing = stdout_str(&output);
    assert!(
        listing.contains("mychar"),
        "Char device should be in listing"
    );
    // Verbose listing should show 'c' for char device
    assert!(
        listing.contains("c"),
        "Verbose listing should show 'c' for char device"
    );

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read char device");

    // Verify char device was extracted
    let extracted_dev = dst_dir.join("mychar");
    let meta = fs::symlink_metadata(&extracted_dev).unwrap();
    assert!(
        meta.file_type().is_char_device(),
        "Extracted file should be a char device"
    );
}

#[cfg(unix)]
#[test]
fn test_read_special_files_from_system_tar() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("special.tar");

    // Create source directory with FIFO (mkfifo doesn't require root)
    fs::create_dir(&src_dir).unwrap();
    let fifo_path = src_dir.join("testfifo");
    let path_cstr = CString::new(fifo_path.as_os_str().as_bytes()).unwrap();
    unsafe {
        let ret = libc::mkfifo(path_cstr.as_ptr(), 0o644);
        if ret != 0 {
            eprintln!("Skipping test: mkfifo not supported");
            return;
        }
    }

    // Create archive with system tar
    let output = Command::new("tar")
        .args(["-cf"])
        .arg(&archive)
        .arg(".")
        .current_dir(&src_dir)
        .output();

    if output.is_err() || !output.as_ref().unwrap().status.success() {
        eprintln!("Skipping test: system tar not available");
        return;
    }

    // List with our pax
    let output = run_pax(&["-v", "-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list system tar");

    let listing = stdout_str(&output);
    assert!(listing.contains("testfifo"), "FIFO should be in listing");
    // Should show 'p' for FIFO
    assert!(
        listing.contains('p'),
        "Listing should show 'p' for FIFO: {}",
        listing
    );
}
