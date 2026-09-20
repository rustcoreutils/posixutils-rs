//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Special file tests (FIFO, block device, character device)

use crate::common::*;
use plib::tmp::TempDir;
use std::ffi::CString;
use std::fs;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::FileTypeExt;
use std::process::Command;

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

/// Copy mode (-r -w) recreates a FIFO at the destination rather than reporting
/// it as an unsupported file type.
#[test]
fn test_copy_mode_recreates_fifo() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    fs::create_dir(&src_dir).unwrap();
    fs::create_dir(&dst_dir).unwrap();
    let fifo_path = src_dir.join("myfifo");
    let path_cstr = CString::new(fifo_path.as_os_str().as_bytes()).unwrap();
    unsafe {
        if libc::mkfifo(path_cstr.as_ptr(), 0o644) != 0 {
            eprintln!("Skipping copy-FIFO test: mkfifo failed");
            return;
        }
    }

    let output = run_pax_in_dir(&["-r", "-w", "myfifo", dst_dir.to_str().unwrap()], &src_dir);
    assert_success(&output, "pax copy fifo");

    let copied = dst_dir.join("myfifo");
    let meta = fs::symlink_metadata(&copied).unwrap();
    assert!(
        meta.file_type().is_fifo(),
        "copied entry should be a FIFO, got {:?}",
        meta.file_type()
    );
}

/// A trailing <space> is a legitimate ustar pathname character: it must survive
/// a write/list/extract round-trip (the name field is NUL-terminated, not
/// space-trimmed).
fn trailing_space_roundtrip(format: &str) {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    let archive = temp.path().join(format!("space-{}.tar", format));

    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("name "), b"hi").unwrap();

    let output = run_pax_in_dir(
        &["-w", "-x", format, "-f", archive.to_str().unwrap(), "name "],
        &src_dir,
    );
    assert_success(&output, "pax write trailing-space name");

    let listing = stdout_str(&run_pax(&["-f", archive.to_str().unwrap()]));
    assert!(
        listing.lines().any(|l| l == "name "),
        "-x {format}: listing should preserve the trailing space: {listing:?}"
    );

    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax extract trailing-space name");
    assert!(
        dst_dir.join("name ").exists(),
        "-x {format}: extracted file must keep its trailing space"
    );
}

#[test]
fn test_ustar_trailing_space_in_name_roundtrip() {
    trailing_space_roundtrip("ustar");
}

/// The pax reader parsed name/prefix/linkname with the space-trimming helper
/// used for uname/gname, so a trailing space was silently dropped even though
/// the ustar reader had already been fixed for exactly this.
#[test]
fn test_pax_trailing_space_in_name_roundtrip() {
    trailing_space_roundtrip("pax");
}

#[test]
fn test_cpio_trailing_space_in_name_roundtrip() {
    trailing_space_roundtrip("cpio");
}

/// A pathname read from the -w stdin file list keeps a leading space (the list
/// reader strips only the trailing newline, not surrounding whitespace).
#[test]
fn test_stdin_file_list_preserves_leading_space() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("lead.tar");

    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join(" lead"), b"hi").unwrap();

    let output = run_pax_in_dir_with_stdin(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap()],
        &src_dir,
        " lead\n",
    );
    assert_success(&output, "pax write from stdin list");

    let listing = stdout_str(&run_pax(&["-f", archive.to_str().unwrap()]));
    assert!(
        listing.lines().any(|l| l == " lead"),
        "listing should preserve the leading space: {listing:?}"
    );
}

/// `-o invalid=binary` must announce the member with hdrcharset=BINARY and
/// record its pathname as unencoded bytes. It used to run the name through
/// to_string_lossy first, replacing every invalid byte with U+FFFD
/// irreversibly, which made `binary` produce byte-identical output to `write`.
///
/// Linux-only: APFS and HFS+ reject a filename that is not well-formed UTF-8,
/// so the fixture cannot exist on macOS.
#[cfg(target_os = "linux")]
#[test]
fn test_pax_preserves_a_raw_name_without_being_asked() {
    // This used to need `-o invalid=binary`. A pathname is bytes, so keeping
    // the bytes is not an option to opt into: hdrcharset=BINARY is decided by
    // the writer from the name it was given.
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    let archive = temp.path().join("bin.pax");
    fs::create_dir(&src_dir).unwrap();
    fs::create_dir(&dst_dir).unwrap();

    let raw = b"na\xffme.txt";
    let name = std::ffi::OsStr::from_bytes(raw);
    fs::write(src_dir.join(name), b"payload").unwrap();

    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "write a member whose name is not UTF-8");

    let bytes = fs::read(&archive).unwrap();
    assert!(
        bytes
            .windows(b"hdrcharset=BINARY".len())
            .any(|w| w == b"hdrcharset=BINARY"),
        "the member must be announced as BINARY"
    );
    assert!(
        bytes.windows(raw.len()).any(|w| w == raw),
        "the pathname must appear in the archive as unencoded bytes"
    );
    // The ustar name field beside the record is a best-effort fallback for
    // readers that do not parse extended headers, so it may well be lossy; the
    // path= record is what has to be exact, and is asserted above.

    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "extract a BINARY member");
    assert_eq!(
        fs::read_to_string(dst_dir.join(name)).unwrap_or_default(),
        "payload",
        "the member must round-trip under its original byte name"
    );
}

// ---------------------------------------------------------------------------
// A pathname is a byte string. Every one of these used to come back with each
// invalid byte replaced by U+FFFD -- a different file, silently.
// ---------------------------------------------------------------------------

/// The member name a GNU-tar archive records must be the name pax creates.
///
/// Linux-only: APFS and HFS+ reject a filename that is not well-formed UTF-8
/// (`creat` returns EILSEQ), so neither the source file nor the extracted one
/// can exist on macOS. What is under test is that pax passes the bytes
/// through; a filesystem that refuses to hold them cannot show that either
/// way.
#[cfg(target_os = "linux")]
#[test]
fn test_non_utf8_name_round_trips_through_every_format() {
    let raw = b"na\xffme.txt";
    let name = std::ffi::OsStr::from_bytes(raw);

    for format in ["ustar", "pax", "cpio"] {
        let temp = TempDir::new().unwrap();
        let src = temp.path().join("src");
        let dst = temp.path().join("dst");
        fs::create_dir(&src).unwrap();
        fs::create_dir(&dst).unwrap();
        fs::write(src.join(name), b"payload").unwrap();

        let archive = temp.path().join("a.archive");
        assert_success(
            &run_pax_in_dir(
                &["-w", "-x", format, "-f", archive.to_str().unwrap(), "."],
                &src,
            ),
            &format!("archive a non-UTF-8 name as {format}"),
        );
        assert_success(
            &run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst),
            &format!("extract a non-UTF-8 name from {format}"),
        );

        assert_eq!(
            fs::read(dst.join(name)).unwrap_or_default(),
            b"payload",
            "{format}: the member did not come back under its own name"
        );
    }
}

/// Two members differing only in bytes that are not valid UTF-8 are two
/// members. Under a lossy conversion both became `a<U+FFFD>b` and the second
/// clobbered the first.
///
/// Linux-only: see the note above -- macOS will not hold either name.
#[cfg(target_os = "linux")]
#[test]
fn test_names_differing_only_in_invalid_bytes_do_not_collide() {
    let temp = TempDir::new().unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();

    let mut archive = crate::common::Ustar {
        name: b"a\xffb",
        body: b"first\n",
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &crate::common::Ustar {
            name: b"a\xfeb",
            body: b"second\n",
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&crate::common::ustar_trailer());

    crate::common::run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, &dst);

    assert_eq!(
        fs::read(dst.join(std::ffi::OsStr::from_bytes(b"a\xffb"))).unwrap_or_default(),
        b"first\n"
    );
    assert_eq!(
        fs::read(dst.join(std::ffi::OsStr::from_bytes(b"a\xfeb"))).unwrap_or_default(),
        b"second\n"
    );
}

/// A raw member name is listed as the bytes the archive recorded.
///
/// Portable: nothing is created, so this runs wherever pax does. The listing
/// is the half of the `pax -t` / `pax -r` agreement that does not need a
/// filesystem willing to hold the name.
#[test]
fn test_listing_reports_the_recorded_bytes() {
    let archive = crate::common::Ustar {
        name: b"na\xffme.txt",
        body: b"x\n",
        ..Default::default()
    }
    .archive();

    let listing = crate::common::run_pax_with_stdin_bytes(&[], &archive);
    assert_eq!(
        listing.stdout, b"na\xffme.txt\n",
        "the listing must be the recorded bytes, not a rendering of them"
    );
}

/// ...and it must be the name extraction actually creates. `pax -t` rendering
/// U+FFFD where `pax -r` writes a raw byte makes the two disagree about the
/// same archive.
///
/// Linux-only: macOS will not create the file, so there is no name to compare
/// the listing against.
#[cfg(target_os = "linux")]
#[test]
fn test_listing_reports_the_name_extraction_creates() {
    let temp = TempDir::new().unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();

    let archive = crate::common::Ustar {
        name: b"na\xffme.txt",
        body: b"x\n",
        ..Default::default()
    }
    .archive();

    let listing = crate::common::run_pax_with_stdin_bytes(&[], &archive);
    crate::common::run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, &dst);
    let created: Vec<_> = fs::read_dir(&dst)
        .unwrap()
        .map(|e| e.unwrap().file_name())
        .collect();
    assert_eq!(created.len(), 1);
    assert_eq!(
        created[0].as_bytes(),
        &listing.stdout[..listing.stdout.len() - 1],
        "`pax -t` and `pax -r` must agree about the name"
    );
}

/// `-o listopt=%F` is a second path to the same name and must not diverge from
/// the plain listing.
#[test]
fn test_listopt_renders_a_raw_name_exactly() {
    let archive = crate::common::Ustar {
        name: b"na\xffme.txt",
        body: b"x\n",
        ..Default::default()
    }
    .archive();

    let output = crate::common::run_pax_with_stdin_bytes(&["-o", "listopt=%F"], &archive);
    assert_eq!(output.stdout, b"na\xffme.txt\n");
}

/// For cpio the symbolic link target *is* the member data, so taking its
/// length from a lossy string made the header disagree with the bytes written
/// and desynchronised everything after it.
#[test]
fn test_non_utf8_symlink_target_round_trips_through_cpio() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let dst = temp.path().join("dst");
    fs::create_dir(&src).unwrap();
    fs::create_dir(&dst).unwrap();

    let target = std::ffi::OsStr::from_bytes(b"ta\xffrget");
    std::os::unix::fs::symlink(target, src.join("link")).unwrap();
    fs::write(src.join("after.txt"), b"still here\n").unwrap();

    let archive = temp.path().join("a.cpio");
    assert_success(
        &run_pax_in_dir(
            &["-w", "-x", "cpio", "-f", archive.to_str().unwrap(), "."],
            &src,
        ),
        "archive a non-UTF-8 symlink target",
    );
    assert_success(
        &run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst),
        "extract a non-UTF-8 symlink target",
    );

    assert_eq!(
        fs::read_link(dst.join("link"))
            .unwrap()
            .as_os_str()
            .as_bytes(),
        b"ta\xffrget",
        "the link target must come back byte-identical"
    );
    assert_eq!(
        fs::read_to_string(dst.join("after.txt")).unwrap_or_default(),
        "still here\n",
        "a wrong target length desynchronises every member after it"
    );
}
