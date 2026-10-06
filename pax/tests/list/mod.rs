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

/// An archive of `n` empty members named `member-0000`, `member-0001`, ...
fn many_members(n: usize) -> Vec<u8> {
    let mut archive = Vec::new();
    for i in 0..n {
        let name = format!("member-{i:04}");
        archive.extend_from_slice(
            &Ustar {
                name: name.as_bytes(),
                ..Default::default()
            }
            .member(),
        );
    }
    archive.extend_from_slice(&ustar_trailer());
    archive
}

/// A listing on a terminal shows each member as it is read, not once the
/// whole archive is: here the archive arrives on a pipe, and its end is held
/// back until the first name has appeared.
#[test]
fn test_terminal_listing_is_not_held_back() {
    let temp = TempDir::new().unwrap();
    let archive = many_members(4);
    let (first, rest) = archive.split_at(2 * 512);

    let mut pax = PtyPax::spawn(&[], temp.path(), b"", PtyStdio::OutputOnly);
    let mut stdin = pax.child.stdin.take().unwrap();
    stdin.write_all(first).unwrap();
    let shown = pax.wait_for(b"member-0001", std::time::Duration::from_secs(5));
    let _ = stdin.write_all(rest);
    drop(stdin);
    let finished = pax.finish(std::time::Duration::from_secs(20));
    assert!(
        shown,
        "the first names were held back until the archive ended"
    );
    assert!(finished.is_some_and(|(out, _)| out.status.success()));
}

/// A listing standard output cannot hold is one error, diagnosed once, and
/// ends the run with a failure status -- not a diagnostic per remaining member
/// blaming each one in turn. The file-size limit stands in for a full disk;
/// a closed pipe needs no test, since SIGPIPE ends pax as it ends `cat`.
#[test]
fn test_listing_write_error_fails_once() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("a.tar"), many_members(3000)).unwrap();

    // SIGXFSZ ignored so the over-limit write fails with EFBIG instead of
    // killing the process; a 1-block limit is exceeded early in the listing.
    let output = Command::new("sh")
        .arg("-c")
        .arg("trap '' XFSZ; ulimit -f 1; exec \"$0\" -f a.tar > out")
        .arg(env!("CARGO_BIN_EXE_pax"))
        .current_dir(temp.path())
        .output()
        .unwrap();

    assert_exit_code(&output, 1, "pax listing past the file-size limit");
    let stderr = stderr_str(&output);
    assert_eq!(stderr.lines().count(), 1, "stderr:\n{stderr}");
    assert!(!stderr.contains("member-"), "blamed a member:\n{stderr}");
}

/// An archive of empty members by name; a name ending in '/' is a directory.
fn archive_of(names: &[&[u8]]) -> Vec<u8> {
    let mut a = Vec::new();
    for &name in names {
        let dir = name.ends_with(b"/");
        a.extend_from_slice(
            &Ustar {
                name,
                typeflag: if dir { b'5' } else { b'0' },
                mode: if dir { 0o755 } else { 0o644 },
                ..Default::default()
            }
            .member(),
        );
    }
    a.extend_from_slice(&ustar_trailer());
    a
}

/// List `archive` with `args`, returning the exit code, stdout and stderr.
fn list(archive: &[u8], args: &[&str]) -> (i32, String, String) {
    let output = run_pax_with_stdin_bytes(args, archive);
    (
        output.status.code().unwrap_or(-1),
        stdout_str(&output),
        stderr_str(&output),
    )
}

/// POSIX: a diagnostic is due only for a pattern "not matched by at least one
/// ... archive member". `d/x` matches d/x even though `d` selected it too.
#[test]
fn test_overlapping_patterns_are_all_matched() {
    let a = archive_of(&[b"d/", b"d/x", b"d/y"]);
    for args in [&["d", "d/x"][..], &["-n", "d", "d/x"], &["d/x", "d"]] {
        let (code, out, err) = list(&a, args);
        assert_eq!(
            (code, out.as_str(), err.as_str()),
            (0, "d/\nd/x\nd/y\n", ""),
            "{args:?}"
        );
    }
}

/// -n: "members of type directory shall still match the file hierarchy
/// rooted at that file" -- also when the archive names the directory after
/// its contents, as `find -depth` lists it.
#[test]
fn test_n_keeps_the_hierarchy_of_a_depth_first_archive() {
    let a = archive_of(&[b"d/x", b"d/y", b"d/", b"e"]);
    let (code, out, _) = list(&a, &["-n", "d"]);
    assert_eq!((code, out.as_str()), (0, "d/x\nd/y\nd/\n"));
}

/// -n with the pattern `.` selects the whole hierarchy of an archive made
/// from `.`.
#[test]
fn test_n_dot_keeps_its_hierarchy() {
    let a = archive_of(&[b"./", b"./x", b"./d/", b"./d/y"]);
    let (code, out, _) = list(&a, &["-n", "."]);
    assert_eq!((code, out.as_str()), (0, "./\n./x\n./d/\n./d/y\n"));
}

/// A pattern with a trailing slash names the directory, and so its hierarchy;
/// it does not name a file that is not a directory.
#[test]
fn test_pattern_trailing_slash_selects_the_hierarchy() {
    let a = archive_of(&[b"d/", b"d/x", b"f"]);
    let (code, out, _) = list(&a, &["d/"]);
    assert_eq!((code, out.as_str()), (0, "d/\nd/x\n"));
    let (code, out, _) = list(&a, &["f/"]);
    assert_eq!((code, out.as_str()), (1, ""));
}

/// `a/*` names what is in the directory a, not a itself (stored as "a/").
#[test]
fn test_pattern_star_after_slash_skips_the_directory() {
    let a = archive_of(&[b"a/", b"a/f"]);
    let (code, out, _) = list(&a, &["a/*"]);
    assert_eq!((code, out.as_str()), (0, "a/f\n"));
    let (_, out, _) = list(&a, &["-d", "a/*"]);
    assert_eq!(out, "a/f\n");
}

/// XCU 2.14.3, which pax patterns follow: "If a <slash> character is found
/// following an unescaped <left-square-bracket> character before a
/// corresponding <right-square-bracket> is found, the open bracket shall be
/// treated as an ordinary character."
#[test]
fn test_pattern_slash_in_bracket_makes_it_literal() {
    let a = archive_of(&[b"a[/]f", b"a/f"]);
    let (code, out, _) = list(&a, &["a[/]f"]);
    assert_eq!((code, out.as_str()), (0, "a[/]f\n"));
}

/// An absolute name's leading '/' is no empty directory a pattern can name:
/// neither `*` nor the empty pattern selects it. `/` does.
#[test]
fn test_pattern_does_not_match_the_empty_root_prefix() {
    let a = archive_of(&[b"/abs/k"]);
    for pattern in ["*", ""] {
        let (code, out, _) = list(&a, &[pattern]);
        assert_eq!((code, out.as_str()), (1, ""), "{pattern:?}");
    }
    let (code, out, _) = list(&a, &["/"]);
    assert_eq!((code, out.as_str()), (0, "/abs/k\n"));
}

/// -c: "Match all file or archive members except those specified by the
/// pattern or file operands." With none, nothing is excepted.
#[test]
fn test_c_without_patterns_selects_everything() {
    let a = archive_of(&[b"d/", b"d/x"]);
    let (code, out, _) = list(&a, &["-c"]);
    assert_eq!((code, out.as_str()), (0, "d/\nd/x\n"));
}

/// Bracket expressions read characters as `LC_CTYPE` encodes them, not as
/// UTF-8: in a Latin-1 locale the byte 0xE9 is 'é', a letter in the range
/// à-ÿ. Skipped where the locale is not installed.
#[test]
fn test_pattern_brackets_follow_a_single_byte_locale() {
    use std::os::unix::ffi::OsStrExt;
    let locale = ["en_US.ISO8859-1", "en_US.iso88591"].into_iter().find(|l| {
        Command::new("locale")
            .arg("-a")
            .output()
            .is_ok_and(|o| o.stdout.split(|&b| b == b'\n').any(|n| n == l.as_bytes()))
    });
    let Some(locale) = locale else {
        return;
    };
    let a = archive_of(&[b"\xe9"]);
    for pattern in [&b"[[:alpha:]]"[..], b"[\xe0-\xff]"] {
        let mut child = Command::new(env!("CARGO_BIN_EXE_pax"))
            .arg(std::ffi::OsStr::from_bytes(pattern))
            .env("LC_ALL", locale)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .unwrap();
        child.stdin.take().unwrap().write_all(&a).unwrap();
        let output = child.wait_with_output().unwrap();
        assert_eq!(
            output.stdout,
            b"\xe9\n",
            "{}",
            String::from_utf8_lossy(pattern)
        );
    }
}
