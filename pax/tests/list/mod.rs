//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! List mode tests

use crate::common::*;
use plib::tmp::TempDir;
use std::fs::{self, File};
use std::io::Write;
use std::process::Command;

#[test]
fn test_list_mode() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List archive contents
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list");

    let listing = stdout_str(&output);
    assert!(listing.contains("file.txt"), "Missing file.txt in listing");
    assert!(
        listing.contains("subdir/nested.txt") || listing.contains("subdir"),
        "Missing subdir in listing"
    );
}

#[test]
fn test_verbose_list() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List archive with verbose mode
    let output = run_pax(&["-v", "-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax verbose list");

    let listing = stdout_str(&output);
    // Verbose output should contain permission strings like "rw-"
    assert!(
        listing.contains("r") && listing.contains("-"),
        "Verbose listing missing permission info"
    );
}

#[test]
fn test_pattern_matching() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Extract only .txt files
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap(), "*.txt"], &dst_dir);
    assert_success(&output, "pax pattern extract");

    // file.txt should be extracted
    assert!(
        dst_dir.join("file.txt").exists(),
        "file.txt should be extracted"
    );
}

#[test]
fn test_no_clobber() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file.txt")).unwrap();
    writeln!(f, "Original content").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Create destination with existing file
    fs::create_dir(&dst_dir).unwrap();
    let mut f = File::create(dst_dir.join("file.txt")).unwrap();
    writeln!(f, "Existing content").unwrap();

    // Extract with -k (no clobber)
    let output = run_pax_in_dir(&["-r", "-k", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax no-clobber extract");

    // Original file should be preserved
    let content = fs::read_to_string(dst_dir.join("file.txt")).unwrap();
    assert!(
        content.contains("Existing"),
        "File was overwritten despite -k"
    );
}

#[test]
fn test_pax_list() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive using pax format
    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List contents
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list");

    let listing = stdout_str(&output);
    assert!(listing.contains("file.txt"), "Missing file.txt");
}

#[test]
fn test_pax_verbose_list() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive using pax format
    run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Verbose list
    let output = run_pax(&["-v", "-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax verbose list");

    let listing = stdout_str(&output);
    // Should have permissions
    assert!(listing.contains("r") || listing.contains("-"));
}

/// A directory-name pattern selects the whole subtree by default; `-d` restricts
/// the match to the directory member itself.
#[test]
fn test_list_directory_subtree_and_dash_d() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("c.tar");

    fs::create_dir_all(src_dir.join("dir/sub")).unwrap();
    File::create(src_dir.join("dir/sub/f"))
        .unwrap()
        .write_all(b"a")
        .unwrap();
    File::create(src_dir.join("dir/g"))
        .unwrap()
        .write_all(b"b")
        .unwrap();

    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "dir"],
        &src_dir,
    );

    // Default: the directory pattern selects the whole hierarchy.
    let out = run_pax(&["-f", archive.to_str().unwrap(), "dir"]);
    assert_success(&out, "list dir subtree");
    let listing = stdout_str(&out);
    assert!(
        listing.contains("dir/sub/f"),
        "subtree should be listed: {listing}"
    );
    assert!(
        listing.contains("dir/g"),
        "subtree should be listed: {listing}"
    );

    // With -d, only the directory member itself matches.
    let out = run_pax(&["-d", "-f", archive.to_str().unwrap(), "dir"]);
    assert_success(&out, "list dir with -d");
    let listing = stdout_str(&out);
    assert!(
        listing.lines().any(|l| l == "dir/" || l == "dir"),
        "the directory itself should be listed: {listing:?}"
    );
    assert!(
        !listing.contains("dir/sub/f") && !listing.contains("dir/g"),
        "-d must not list the subtree: {listing}"
    );
}

/// `pax -v` reports a link count, and ustar has no field to read one from.
///
/// `ustar::parse_header` finished with `..Default::default()`, which left
/// `nlink` at 0, so every member of every tar archive listed as having no
/// names at all. cpio floors the field at one for the same reason (see
/// `cpio::header_nlink`); a member read from a ustar header gets the same
/// treatment, because a member that exists has at least one name.
#[test]
fn test_verbose_list_reports_a_link_count_of_one() {
    let archive = Ustar {
        name: b"f.txt",
        body: b"hi\n",
        ..Default::default()
    }
    .archive();

    let output = run_pax_with_stdin_bytes(&["-v"], &archive);
    assert_success(&output, "pax -v over a ustar archive");

    let listing = stdout_str(&output);
    let line = listing.lines().next().expect("one member");
    // `print_verbose` writes mode, link count, owner, group, size, time, name.
    let nlink = line.split_whitespace().nth(1).expect("link count column");
    assert_eq!(
        nlink, "1",
        "a member read from a ustar header must list one link, not {} (line: {})",
        nlink, line
    );
}

/// -n selects the first member matching each pattern -- and a directory's
/// hierarchy comes with it, as without -n.
#[test]
fn test_n_keeps_the_matched_directory_hierarchy() {
    let mut a = Ustar {
        name: b"d/",
        typeflag: b'5',
        mode: 0o755,
        ..Default::default()
    }
    .member();
    a.extend_from_slice(
        &Ustar {
            name: b"d/x",
            body: b"X\n",
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes(&["-n", "d"], &a);
    assert_success(&output, "pax -n d");
    assert_eq!(stdout_str(&output), "d/\nd/x\n");
}

/// Bracket expressions follow XCU 2.13: character classes work, and a
/// bracket expression never matches the '/' of a pathname.
#[test]
fn test_pattern_bracket_classes_and_slash() {
    let mut a = Vec::new();
    for name in [&b"a1"[..], b"ab", b"a/b"] {
        a.extend_from_slice(
            &Ustar {
                name,
                body: b"X\n",
                ..Default::default()
            }
            .member(),
        );
    }
    a.extend_from_slice(&ustar_trailer());

    let output = run_pax_with_stdin_bytes(&["a[[:digit:]]"], &a);
    assert_eq!(stdout_str(&output), "a1\n");
    let output = run_pax_with_stdin_bytes(&["a[!x]b"], &a);
    assert_eq!(stdout_str(&output), "", "a bracket matched '/'");
}

/// An unterminated '[' is an ordinary character, not a fatal error.
#[test]
fn test_pattern_unterminated_bracket_is_literal() {
    let a = Ustar {
        name: b"x[y",
        body: b"X\n",
        ..Default::default()
    }
    .archive();
    let output = run_pax_with_stdin_bytes(&["x[y"], &a);
    assert_success(&output, "pattern with an unterminated bracket");
    assert_eq!(stdout_str(&output), "x[y\n");
}

/// A long run of '*' must not blow the stack or take exponential time. pax
/// is killed if it has not finished in time, so a regression fails the test
/// rather than hanging the suite.
#[test]
fn test_pattern_many_stars_is_fast() {
    let a = Ustar {
        name: "a".repeat(60).as_bytes(),
        body: b"X\n",
        ..Default::default()
    }
    .archive();
    let pattern = format!("{}b", "*".repeat(5000));
    let mut child = Command::new(env!("CARGO_BIN_EXE_pax"))
        .arg(&pattern)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap().write_all(&a).unwrap();

    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break Some(status);
        }
        if std::time::Instant::now() > deadline {
            child.kill().unwrap();
            child.wait().unwrap();
            break None;
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    };
    let status = status.expect("pax did not finish matching within 5 s");
    assert!(status.code().is_some(), "pax was killed by a signal");
}

/// A directory member replaces an existing non-directory of the same name
/// (unless -k), the way a regular-file member replaces a file.
#[test]
fn test_directory_member_replaces_regular_file() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("d"), "old\n").unwrap();
    let mut a = Ustar {
        name: b"d/",
        typeflag: b'5',
        mode: 0o755,
        ..Default::default()
    }
    .member();
    a.extend_from_slice(
        &Ustar {
            name: b"d/x",
            body: b"X\n",
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &a, temp.path());
    assert_success(&output, "pax -r over a file named like the directory");
    assert_eq!(fs::read_to_string(temp.path().join("d/x")).unwrap(), "X\n");
}

/// GNU tar writes base-256 numbers (high bit set) for values that do not fit
/// the octal field. pax should read them, not abandon the archive.
#[test]
fn test_gnu_base256_size_and_uid_are_read() {
    let mut member = Ustar {
        name: b"big-uid",
        body: b"X\n",
        ..Default::default()
    };
    member.uid = 0;
    let mut a = member.member();
    // uid field (108..116): base-256, value 3000000.
    let uid = 3_000_000u64;
    a[108] = 0x80;
    a[109..116].copy_from_slice(&uid.to_be_bytes()[1..]);
    // Recompute the checksum over the altered header.
    a[148..156].copy_from_slice(b"        ");
    let sum: u32 = a[..512].iter().map(|&b| b as u32).sum();
    a[148..156].copy_from_slice(format!("{:06o}\0 ", sum).as_bytes());
    a.extend_from_slice(
        &Ustar {
            name: b"next",
            body: b"N\n",
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes(&["-o", "listopt=%(uid)d %F"], &a);
    assert_success(&output, "list");
    assert_eq!(stdout_str(&output), "3000000 big-uid\n0 next\n");
}

/// Under the C locale a non-ASCII member name must not panic -s.
#[test]
fn test_subst_non_ascii_name_in_c_locale() {
    let a = Ustar {
        name: "café".as_bytes(),
        body: b"X\n",
        ..Default::default()
    }
    .archive();
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_pax"));
    cmd.args(["-s", ",f.,X,"])
        .env("LC_ALL", "C")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    let mut child = cmd.spawn().unwrap();
    child.stdin.take().unwrap().write_all(&a).unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(
        output.status.code().is_some_and(|c| c != 101),
        "{}",
        stderr_str(&output)
    );
}
