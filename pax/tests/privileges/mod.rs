//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Privilege/Preservation (-p) tests

use crate::common::*;
use std::fs::{self, File};
use std::io::Write;
use std::os::unix::fs::MetadataExt;
use std::os::unix::fs::PermissionsExt;
use std::time::{SystemTime, UNIX_EPOCH};
use tempfile::TempDir;

#[cfg(unix)]
#[test]
fn test_priv_preserve_perms() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file with specific permissions
    fs::create_dir(&src_dir).unwrap();
    let file_path = src_dir.join("test.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "test").unwrap();
    fs::set_permissions(&file_path, fs::Permissions::from_mode(0o754)).unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "test.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with -p p (preserve permissions)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-p", "p", "-f", archive.to_str().unwrap()],
        &dst_dir,
    );
    assert_success(&output, "pax read");

    // Verify permissions
    let extracted = dst_dir.join("test.txt");
    let mode = fs::metadata(&extracted).unwrap().permissions().mode() & 0o777;
    assert_eq!(mode, 0o754, "permissions should be preserved");
}

#[cfg(unix)]
#[test]
fn test_priv_preserve_mtime() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let file_path = src_dir.join("test.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "test").unwrap();

    // Set a specific mtime (2020-01-01 00:00:00 UTC)
    let mtime = 1577836800;
    let times = [
        libc::timeval {
            tv_sec: mtime,
            tv_usec: 0,
        },
        libc::timeval {
            tv_sec: mtime,
            tv_usec: 0,
        },
    ];
    let path_cstr = std::ffi::CString::new(file_path.to_str().unwrap()).unwrap();
    unsafe {
        libc::utimes(path_cstr.as_ptr(), times.as_ptr());
    }

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "test.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with default (preserves mtime)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    // Verify mtime is preserved
    let extracted = dst_dir.join("test.txt");
    let extracted_mtime = fs::metadata(&extracted).unwrap().mtime();
    assert_eq!(
        extracted_mtime, mtime,
        "mtime should be preserved by default"
    );
}

#[cfg(unix)]
#[test]
fn test_priv_no_preserve_mtime() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let file_path = src_dir.join("test.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "test").unwrap();

    // Set an old mtime (2020-01-01 00:00:00 UTC)
    let old_mtime = 1577836800i64;
    let times = [
        libc::timeval {
            tv_sec: old_mtime,
            tv_usec: 0,
        },
        libc::timeval {
            tv_sec: old_mtime,
            tv_usec: 0,
        },
    ];
    let path_cstr = std::ffi::CString::new(file_path.to_str().unwrap()).unwrap();
    unsafe {
        libc::utimes(path_cstr.as_ptr(), times.as_ptr());
    }

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "test.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with -p m (do NOT preserve mtime)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-p", "m", "-f", archive.to_str().unwrap()],
        &dst_dir,
    );
    assert_success(&output, "pax read");

    // Verify mtime is recent (not the old one)
    let extracted = dst_dir.join("test.txt");
    let extracted_mtime = fs::metadata(&extracted).unwrap().mtime();
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs() as i64;

    // Should be within last minute
    assert!(
        extracted_mtime > now - 60,
        "mtime should be current time when -p m is used: got {}, expected around {}",
        extracted_mtime,
        now
    );
}

#[cfg(unix)]
#[test]
fn test_priv_e_preserves_everything() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file with specific permissions
    fs::create_dir(&src_dir).unwrap();
    let file_path = src_dir.join("test.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "test").unwrap();
    fs::set_permissions(&file_path, fs::Permissions::from_mode(0o751)).unwrap();

    // Set a specific mtime
    let mtime = 1577836800;
    let times = [
        libc::timeval {
            tv_sec: mtime,
            tv_usec: 0,
        },
        libc::timeval {
            tv_sec: mtime,
            tv_usec: 0,
        },
    ];
    let path_cstr = std::ffi::CString::new(file_path.to_str().unwrap()).unwrap();
    unsafe {
        libc::utimes(path_cstr.as_ptr(), times.as_ptr());
    }

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "test.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with -p e (preserve everything, though owner needs root)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-p", "e", "-f", archive.to_str().unwrap()],
        &dst_dir,
    );
    // This might warn about owner on non-root, but should succeed
    assert_success(&output, "pax read");

    // Verify perms and mtime
    let extracted = dst_dir.join("test.txt");
    let meta = fs::metadata(&extracted).unwrap();
    let mode = meta.permissions().mode() & 0o777;
    assert_eq!(mode, 0o751, "permissions should be preserved with -p e");
    assert_eq!(meta.mtime(), mtime, "mtime should be preserved with -p e");
}

#[cfg(unix)]
#[test]
fn test_priv_precedence_last_wins() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let file_path = src_dir.join("test.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "test").unwrap();

    // Set an old mtime
    let old_mtime = 1577836800i64;
    let times = [
        libc::timeval {
            tv_sec: old_mtime,
            tv_usec: 0,
        },
        libc::timeval {
            tv_sec: old_mtime,
            tv_usec: 0,
        },
    ];
    let path_cstr = std::ffi::CString::new(file_path.to_str().unwrap()).unwrap();
    unsafe {
        libc::utimes(path_cstr.as_ptr(), times.as_ptr());
    }

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "test.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with -p me (m disables mtime, then e enables everything)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-p", "me", "-f", archive.to_str().unwrap()],
        &dst_dir,
    );
    assert_success(&output, "pax read");

    // With precedence, 'e' comes after 'm', so mtime should be preserved
    let extracted = dst_dir.join("test.txt");
    let extracted_mtime = fs::metadata(&extracted).unwrap().mtime();
    assert_eq!(
        extracted_mtime, old_mtime,
        "mtime should be preserved when 'e' follows 'm' in -p string"
    );

    // Now test the reverse: -p em (e enables, then m disables)
    let dst_dir2 = temp.path().join("dest2");
    fs::create_dir(&dst_dir2).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-p", "em", "-f", archive.to_str().unwrap()],
        &dst_dir2,
    );
    assert_success(&output, "pax read");

    // With precedence, 'm' comes after 'e', so mtime should NOT be preserved
    let extracted2 = dst_dir2.join("test.txt");
    let extracted_mtime2 = fs::metadata(&extracted2).unwrap().mtime();
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs() as i64;
    assert!(
        extracted_mtime2 > now - 60,
        "mtime should be current when 'm' follows 'e': got {}, expected around {}",
        extracted_mtime2,
        now
    );
}

#[cfg(unix)]
#[test]
fn test_priv_suid_cleared_without_owner() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file with SUID bit
    fs::create_dir(&src_dir).unwrap();
    let file_path = src_dir.join("test.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "test").unwrap();
    // Set mode 4755 (SUID + rwxr-xr-x)
    fs::set_permissions(&file_path, fs::Permissions::from_mode(0o4755)).unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "test.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract without -p o (owner not preserved)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-p", "p", "-f", archive.to_str().unwrap()],
        &dst_dir,
    );
    assert_success(&output, "pax read");

    // SUID should be cleared when owner is not preserved
    let extracted = dst_dir.join("test.txt");
    let mode = fs::metadata(&extracted).unwrap().permissions().mode();
    assert_eq!(
        mode & 0o4000,
        0,
        "SUID bit should be cleared when owner not preserved: mode={:o}",
        mode
    );
    assert_eq!(
        mode & 0o777,
        0o755,
        "base permissions should be preserved: mode={:o}",
        mode
    );
}

// Owner preservation requires root - gated by requires_root feature
#[cfg(all(unix, feature = "requires_root"))]
#[test]
fn test_priv_owner_preservation() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let file_path = src_dir.join("test.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "test").unwrap();

    // Get original uid/gid
    let orig_meta = fs::metadata(&file_path).unwrap();
    let orig_uid = orig_meta.uid();
    let orig_gid = orig_meta.gid();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "test.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with -p o (preserve owner)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-p", "o", "-f", archive.to_str().unwrap()],
        &dst_dir,
    );
    assert_success(&output, "pax read");

    // Verify owner
    let extracted = dst_dir.join("test.txt");
    let meta = fs::metadata(&extracted).unwrap();
    assert_eq!(meta.uid(), orig_uid, "uid should be preserved with -p o");
    assert_eq!(meta.gid(), orig_gid, "gid should be preserved with -p o");
}

#[cfg(all(unix, feature = "requires_root"))]
#[test]
fn test_priv_suid_preserved_with_owner() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file with SUID bit
    fs::create_dir(&src_dir).unwrap();
    let file_path = src_dir.join("test.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "test").unwrap();
    // Set mode 4755 (SUID + rwxr-xr-x)
    fs::set_permissions(&file_path, fs::Permissions::from_mode(0o4755)).unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "test.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with -p po (owner + perms preserved)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-p", "po", "-f", archive.to_str().unwrap()],
        &dst_dir,
    );
    assert_success(&output, "pax read");

    // SUID should be preserved when owner is also preserved
    let extracted = dst_dir.join("test.txt");
    let mode = fs::metadata(&extracted).unwrap().permissions().mode();
    assert_eq!(
        mode & 0o4000,
        0o4000,
        "SUID bit should be preserved when owner is preserved: mode={:o}",
        mode
    );
}
