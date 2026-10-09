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
use plib::testing::{create_non_utf8, non_utf8_names_supported};
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
    let Some(tar) = system_tool("tar") else {
        return;
    };
    run_system_ok(
        &tar,
        &["-cf", archive.to_str().unwrap(), "."],
        &src_dir,
        None,
    );

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
/// Skipped where the filesystem refuses a filename that is not well-formed
/// UTF-8 (macOS APFS and HFS+), since the fixture cannot exist there.
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
    if create_non_utf8(&src_dir, raw, |p| fs::write(p, b"payload")).is_none() {
        return;
    }

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
/// Skipped where the filesystem refuses a filename that is not well-formed
/// UTF-8 (macOS APFS and HFS+: `creat` returns EILSEQ), so neither the source
/// file nor the extracted one can exist. What is under test is that pax
/// passes the bytes through; a filesystem that refuses to hold them cannot
/// show that either way.
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
        if create_non_utf8(&src, raw, |p| fs::write(p, b"payload")).is_none() {
            return;
        }

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
/// Skipped where the filesystem will hold neither name; see the note above.
#[test]
fn test_names_differing_only_in_invalid_bytes_do_not_collide() {
    let temp = TempDir::new().unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();
    if !non_utf8_names_supported(&dst) {
        return;
    }

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
/// Skipped where the filesystem will not create the file (macOS APFS), as
/// there is then no name to compare the listing against.
#[test]
fn test_listing_reports_the_name_extraction_creates() {
    let temp = TempDir::new().unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();
    if !non_utf8_names_supported(&dst) {
        return;
    }

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

/// Escaping applies only to a terminal. Every test in this suite runs pax with
/// its output piped, so this asserts the guarantee the whole design rests on:
/// piped output is byte-identical, and nothing that parses `pax -t` changes
/// behaviour. It is also the regression test for escaping unconditionally by
/// accident.
#[test]
fn test_piped_output_keeps_control_bytes_verbatim() {
    let archive = crate::common::Ustar {
        name: b"x\x1b[31mred\tname",
        body: b"x\n",
        ..Default::default()
    }
    .archive();

    let plain = crate::common::run_pax_with_stdin_bytes(&[], &archive);
    assert_eq!(
        plain.stdout, b"x\x1b[31mred\tname\n",
        "a pipe must receive the recorded bytes, escape sequence and all"
    );

    let verbose = crate::common::run_pax_with_stdin_bytes(&["-v"], &archive);
    assert!(
        verbose.stdout.windows(4).any(|w| w == b"\x1b[31"),
        "the verbose listing must not escape to a pipe either"
    );

    let listopt = crate::common::run_pax_with_stdin_bytes(&["-o", "listopt=%F"], &archive);
    assert_eq!(listopt.stdout, b"x\x1b[31mred\tname\n");
}

/// A `-o listopt` format string is operator-supplied, so its own literal
/// characters must survive whatever the escaping rule does to the values
/// substituted into it.
#[test]
fn test_listopt_literal_tab_survives() {
    let archive = crate::common::Ustar {
        name: b"a.txt",
        body: b"x\n",
        ..Default::default()
    }
    .archive();

    let output = crate::common::run_pax_with_stdin_bytes(&["-o", "listopt=%F\t%s"], &archive);
    assert_eq!(
        output.stdout, b"a.txt\t2\n",
        "the tab is part of the format, not part of a name"
    );
}

/// A NUL inside a pathname read from standard input -- the common slip
/// `find -print0 | pax -w` -- can name no file. pax must diagnose that name and
/// go on with the rest, not panic after the archive was already created.
#[test]
fn test_write_list_with_nul_byte_is_diagnosed() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("good"), "G\n").unwrap();
    let archive = temp.path().join("a.tar");

    let output = run_pax_with_stdin_bytes_in_dir(
        &["-w", "-f", archive.to_str().unwrap()],
        b"a\0b\ngood\n",
        temp.path(),
    );
    assert_exit_code(&output, 1, "pax -w with a NUL in a listed name");

    let output = run_pax_in_dir(&["-f", archive.to_str().unwrap()], temp.path());
    assert_success(&output, "list");
    assert_eq!(stdout_str(&output), "good\n");
}

/// The same name list in copy mode.
#[test]
fn test_copy_list_with_nul_byte_is_diagnosed() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("good"), "G\n").unwrap();
    fs::create_dir(temp.path().join("out")).unwrap();

    let output = run_pax_with_stdin_bytes_in_dir(&["-rw", "out"], b"a\0b\ngood\n", temp.path());
    assert_exit_code(&output, 1, "pax -rw with a NUL in a listed name");
    assert!(temp.path().join("out/good").exists());
}

/// A write error on the archive is the archive's failure, not each source
/// file's. With the file-size limit exceeded (EFBIG), pax must stop and say so
/// once -- not go on reporting "File too large" against every remaining file
/// as though that file were at fault.
#[test]
fn test_archive_write_error_is_fatal_and_reported_once() {
    let temp = TempDir::new().unwrap();
    let mut names = Vec::new();
    for i in 0..20 {
        let name = format!("f{i:02}");
        fs::write(temp.path().join(&name), vec![b'x'; 4096]).unwrap();
        names.push(name);
    }

    // SIGXFSZ ignored so the over-limit write fails with EFBIG instead of
    // killing the process; a 1-block limit is exceeded by the first record.
    let script = format!(
        "trap '' XFSZ; ulimit -f 1; exec \"$0\" -w -f a.tar {}",
        names.join(" ")
    );
    let output = Command::new("sh")
        .arg("-c")
        .arg(&script)
        .arg(env!("CARGO_BIN_EXE_pax"))
        .current_dir(temp.path())
        .output()
        .unwrap();

    assert_exit_code(&output, 1, "pax -w past the file-size limit");
    let stderr = stderr_str(&output);
    assert_eq!(stderr.lines().count(), 1, "stderr:\n{stderr}");
    assert!(!stderr.contains("f0"), "blamed a source file:\n{stderr}");
}

/// On a terminal, nothing taken from an archive reaches it raw: not the name,
/// not a symbolic link's target, and not the owner and group names in the
/// `-v` columns, which were rendered without escaping.
#[test]
fn test_terminal_listing_escapes_every_archive_field() {
    let temp = TempDir::new().unwrap();
    let mut archive = crate::common::Ustar {
        name: b"n\x1b[31m",
        body: b"x\n",
        uname: b"u\x1b[32m",
        gname: b"g\x1b[33m",
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &crate::common::Ustar {
            name: b"l",
            typeflag: b'2',
            linkname: b"t\x1b[34m",
            ..Default::default()
        }
        .archive(),
    );
    fs::write(temp.path().join("a.tar"), &archive).unwrap();

    let tty = run_pax_on_terminal(
        &["-v", "-f", "a.tar"],
        temp.path(),
        std::time::Duration::from_secs(20),
    )
    .expect("pax -v did not finish");
    assert!(
        !tty.contains(&0x1b),
        "an escape sequence reached the terminal: {:?}",
        String::from_utf8_lossy(&tty)
    );
    assert!(
        tty.windows(2).any(|w| w == b"t?"),
        "{:?}",
        String::from_utf8_lossy(&tty)
    );
}

/// A diagnostic that ends the run is escaped like every per-file one: the
/// final message was printed as it was, archive bytes and all.
#[test]
fn test_terminal_fatal_diagnostic_is_escaped() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_ext_records(&pax_record("size", b"1\x1b[31m"));
    fs::write(temp.path().join("a.tar"), &archive).unwrap();

    let tty = run_pax_on_terminal(
        &["-f", "a.tar"],
        temp.path(),
        std::time::Duration::from_secs(20),
    )
    .expect("pax did not finish");
    assert!(
        tty.windows(2).any(|w| w == b"1?"),
        "the diagnostic should quote the value: {:?}",
        String::from_utf8_lossy(&tty)
    );
    assert!(
        !tty.contains(&0x1b),
        "an escape sequence reached the terminal: {:?}",
        String::from_utf8_lossy(&tty)
    );
}

/// A socket member (cpio can hold one) cannot be created: nothing makes a
/// listening socket out of an archive. Skipping it in silence, with exit
/// status 0, reported an extraction that left a file out as complete.
#[test]
fn test_socket_member_is_diagnosed_on_extract() {
    let temp = TempDir::new().unwrap();
    let mut archive = CpioNewc {
        name: b"sock",
        mode: 0o140644,
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &CpioNewc {
            name: b"after",
            body: b"after\n",
            ino: 2,
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert_exit_code(&output, 1, "extract a socket member");
    assert!(
        stderr_str(&output).contains("sock"),
        "the socket must be named: {}",
        stderr_str(&output)
    );
    assert!(!temp.path().join("sock").exists());
    assert_eq!(
        fs::read_to_string(temp.path().join("after")).unwrap(),
        "after\n"
    );
}

/// The root of macOS's sealed, read-only system volume, where creating a name
/// fails with EROFS (below it, SIP answers EPERM first). `None` where a probe
/// does not fail that way, so the tests below never write outside their
/// temporary directory.
#[cfg(target_os = "macos")]
fn read_only_dir() -> Option<&'static std::path::Path> {
    let dir = std::path::Path::new("/");
    let probe = dir.join("pax-erofs-probe");
    match fs::create_dir(&probe) {
        Err(e) if e.kind() == std::io::ErrorKind::ReadOnlyFilesystem => Some(dir),
        Ok(()) => {
            let _ = fs::remove_dir(&probe);
            None
        }
        Err(_) => None,
    }
}

/// POSIX CONSEQUENCES OF ERRORS: a file that cannot be created is diagnosed,
/// and processing continues. A read-only destination used to end the run at
/// the first member, with a message naming none of them.
#[cfg(target_os = "macos")]
#[test]
fn test_read_only_destination_is_diagnosed_per_member_on_extract() {
    let Some(root) = read_only_dir() else {
        return;
    };
    let mut archive = CpioNewc {
        name: b"pax-erofs-a",
        body: b"a\n",
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &CpioNewc {
            name: b"pax-erofs-b",
            body: b"b\n",
            ino: 2,
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, root);
    assert_exit_code(&output, 1, "extract onto a read-only filesystem");
    let stderr = stderr_str(&output);
    assert!(stderr.contains("pax-erofs-a"), "stderr:\n{stderr}");
    assert!(stderr.contains("pax-erofs-b"), "stderr:\n{stderr}");
}

/// The same in copy mode: every file is diagnosed, not just the first.
#[cfg(target_os = "macos")]
#[test]
fn test_read_only_destination_is_diagnosed_per_file_on_copy() {
    let Some(dest) = read_only_dir() else {
        return;
    };
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("pax-erofs-a"), "a\n").unwrap();
    fs::write(temp.path().join("pax-erofs-b"), "b\n").unwrap();

    let output = run_pax_in_dir(
        &["-rw", "pax-erofs-a", "pax-erofs-b", dest.to_str().unwrap()],
        temp.path(),
    );
    assert_exit_code(&output, 1, "copy onto a read-only filesystem");
    let stderr = stderr_str(&output);
    assert!(stderr.contains("pax-erofs-a"), "stderr:\n{stderr}");
    assert!(stderr.contains("pax-erofs-b"), "stderr:\n{stderr}");
}
