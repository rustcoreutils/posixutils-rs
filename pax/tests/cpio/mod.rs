//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Integration tests for the `cpio` compatibility front-end.

use crate::common::{
    assert_failure, assert_success, run_cpio, run_front_end, run_pax_with_stdin_bytes_in_dir,
    run_system_ok, stderr_str, stdout_str, system_tool, CpioNewc,
};
use plib::tmp::TempDir;
use std::fs;
use std::path::{Path, PathBuf};

/// The pathname list a real `find .` would produce for the tree below, in the
/// order cpio expects it on standard input.
const NAME_LIST: &str = ".\n./a.txt\n./sub\n./sub/b.txt\n./sub/c.o\n./link.txt\n";

fn setup(root: &Path) -> PathBuf {
    let src = root.join("src");
    fs::create_dir_all(src.join("sub")).unwrap();
    fs::write(src.join("a.txt"), "alpha\n").unwrap();
    fs::write(src.join("sub/b.txt"), "beta\n").unwrap();
    fs::write(src.join("sub/c.o"), "object\n").unwrap();
    #[cfg(unix)]
    std::os::unix::fs::symlink("a.txt", src.join("link.txt")).unwrap();
    src
}

/// Create an archive in `format` from the standard tree, returning its bytes.
fn copy_out(src: &Path, format: Option<&str>) -> Vec<u8> {
    let mut args = vec!["-o"];
    if let Some(fmt) = format {
        args.extend(["-H", fmt]);
    }
    let out = run_cpio(&args, src, NAME_LIST.as_bytes());
    assert_success(&out, "cpio -o");
    out.stdout
}

/// Sorted member names of an archive, as `cpio -it` reports them.
///
/// The listing is split on newlines, so this cannot describe a member whose own
/// name contains one; that case is checked against the extracted tree instead.
fn members(dir: &Path, archive: &[u8]) -> Vec<String> {
    let out = run_cpio(&["-it"], dir, archive);
    assert_success(&out, "cpio -it");
    let mut names: Vec<String> = stdout_str(&out)
        .lines()
        .map(|l| l.trim_start_matches("./").to_string())
        .filter(|l| !l.is_empty() && l != ".")
        .collect();
    names.sort();
    names
}

fn extract(dir: &Path, archive: &[u8]) {
    let out = run_cpio(&["-idm"], dir, archive);
    assert_success(&out, "cpio -idm");
}

fn assert_tree_extracted(dest: &Path) {
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
    assert_eq!(
        fs::read_to_string(dest.join("sub/b.txt")).unwrap(),
        "beta\n"
    );
    #[cfg(unix)]
    assert_eq!(
        fs::read_link(dest.join("link.txt")).unwrap(),
        Path::new("a.txt")
    );
}

#[test]
fn test_cpio_roundtrip_every_writable_format() {
    // "bin" is what -o writes with no -H, matching cpio's own default.
    for format in [None, Some("bin"), Some("odc"), Some("newc"), Some("crc")] {
        let temp = TempDir::new().unwrap();
        let src = setup(temp.path());
        let dest = temp.path().join("dest");
        fs::create_dir(&dest).unwrap();

        let archive = copy_out(&src, format);
        assert_eq!(
            members(temp.path(), &archive),
            vec![
                "a.txt".to_string(),
                "link.txt".to_string(),
                "sub".to_string(),
                "sub/b.txt".to_string(),
                "sub/c.o".to_string(),
            ],
            "member list for -H {:?}",
            format
        );

        extract(&dest, &archive);
        assert_tree_extracted(&dest);
    }
}

#[test]
fn test_cpio_default_format_is_old_binary() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let archive = copy_out(&src, None);
    // Old binary cpio's magic is the 16-bit value 070707 in host byte order.
    let magic = u16::from_ne_bytes([archive[0], archive[1]]);
    assert_eq!(magic, 0o070707, "cpio -o should default to the bin format");
}

#[test]
fn test_cpio_format_magics() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    for (format, magic) in [("odc", "070707"), ("newc", "070701"), ("crc", "070702")] {
        let archive = copy_out(&src, Some(format));
        assert_eq!(
            &archive[..6],
            magic.as_bytes(),
            "wrong magic for -H {}",
            format
        );
    }
}

#[test]
fn test_cpio_does_not_recurse() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    // cpio archives exactly the names it is handed. Naming only the directory
    // must not pull in its contents, or a `find | cpio -o` pipeline would store
    // every subtree twice.
    let out = run_cpio(&["-o", "-H", "newc"], &src, b"sub\n");
    assert_success(&out, "cpio -o with one directory name");
    assert_eq!(members(temp.path(), &out.stdout), vec!["sub".to_string()]);
}

#[test]
fn test_cpio_reports_block_count_and_quiet_suppresses_it() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    let out = run_cpio(&["-o", "-H", "newc"], &src, NAME_LIST.as_bytes());
    assert_success(&out, "cpio -o");
    assert!(
        stderr_str(&out).trim().ends_with("blocks") || stderr_str(&out).trim().ends_with("block"),
        "cpio -o should report a block count, got: {:?}",
        stderr_str(&out)
    );

    let out = run_cpio(&["-o", "-H", "newc", "--quiet"], &src, NAME_LIST.as_bytes());
    assert_success(&out, "cpio -o --quiet");
    assert_eq!(stderr_str(&out), "", "--quiet should silence the count");
}

#[test]
fn test_cpio_list_and_patterns() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let archive = copy_out(&src, Some("newc"));

    let out = run_cpio(&["-it", "./sub/*"], temp.path(), &archive);
    assert_success(&out, "cpio -it with a pattern");
    let listing = stdout_str(&out);
    let listed: Vec<&str> = listing.lines().collect();
    assert_eq!(listed, vec!["./sub/b.txt", "./sub/c.o"]);

    // -f inverts the selection.
    let out = run_cpio(&["-it", "-f", "./sub/*"], temp.path(), &archive);
    assert_success(&out, "cpio -it -f");
    let listed = stdout_str(&out);
    assert!(!listed.contains("b.txt"), "got {}", listed);
    assert!(listed.contains("a.txt"), "got {}", listed);
}

#[test]
fn test_cpio_extract_pattern_selects_subset() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    let archive = copy_out(&src, Some("newc"));

    let out = run_cpio(&["-idm", "./a.txt"], &dest, &archive);
    assert_success(&out, "cpio -idm with a pattern");
    assert!(dest.join("a.txt").exists());
    assert!(!dest.join("sub/b.txt").exists());
}

#[test]
fn test_cpio_null_separated_name_list() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    // Only a NUL-separated list can carry a name containing a newline.
    fs::write(src.join("odd\nname"), "odd\n").unwrap();

    let out = run_cpio(&["-o", "-0", "-H", "newc"], &src, b"./a.txt\0./odd\nname\0");
    assert_success(&out, "cpio -o -0");

    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    extract(&dest, &out.stdout);
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
    assert_eq!(
        fs::read_to_string(dest.join("odd\nname")).unwrap(),
        "odd\n",
        "the embedded newline should have survived"
    );
}

#[test]
fn test_cpio_pattern_file() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let archive = copy_out(&src, Some("newc"));
    fs::write(temp.path().join("pats"), "./a.txt\n./link.txt\n").unwrap();

    let out = run_cpio(&["-it", "-E", "pats"], temp.path(), &archive);
    assert_success(&out, "cpio -it -E");
    let listing = stdout_str(&out);
    let mut listed: Vec<&str> = listing.lines().collect();
    listed.sort();
    assert_eq!(listed, vec!["./a.txt", "./link.txt"]);
}

#[test]
fn test_cpio_pass_through() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    let out = run_front_end(
        "cpio",
        &["-pdm", dest.to_str().unwrap()],
        &src,
        Some(NAME_LIST.as_bytes()),
    );
    assert_success(&out, "cpio -pdm");
    assert_tree_extracted(&dest);
    // Pass-through moves no archive, so there is no block count to report.
    assert_eq!(stderr_str(&out), "");
}

#[test]
fn test_cpio_pass_through_requires_one_destination() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let out = run_cpio(&["-pdm", "one", "two"], &src, NAME_LIST.as_bytes());
    assert_failure(&out, "cpio -p with two destinations");
    assert!(stderr_str(&out).contains("exactly one destination"));
}

/// Rejecting a command line means exiting before the name list is read, which
/// closes the pipe the test is still writing to. Whether the write lands is a
/// race -- one Linux CI lost while macOS won -- so it is forced here with a
/// payload far larger than any pipe buffer: the diagnostic must still come
/// back rather than the write blowing up.
#[test]
fn test_cpio_rejects_a_bad_command_line_without_draining_stdin() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let flood = "./a.txt\n".repeat(200_000);

    let out = run_cpio(&["-pdm", "one", "two"], &src, flood.as_bytes());
    assert_failure(&out, "cpio -p with two destinations and a large stdin");
    assert!(stderr_str(&out).contains("exactly one destination"));
}

#[test]
fn test_cpio_preserves_mtime_only_with_dash_m() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let archive = copy_out(&src, Some("newc"));
    let source_mtime =
        filetime::FileTime::from_last_modification_time(&fs::metadata(src.join("a.txt")).unwrap());

    let kept = temp.path().join("kept");
    fs::create_dir(&kept).unwrap();
    assert_success(&run_cpio(&["-idm"], &kept, &archive), "cpio -idm");
    // cpio headers carry whole seconds, so only those can be compared.
    assert_eq!(
        filetime::FileTime::from_last_modification_time(&fs::metadata(kept.join("a.txt")).unwrap())
            .unix_seconds(),
        source_mtime.unix_seconds(),
        "-m must restore the archived modification time"
    );

    let fresh = temp.path().join("fresh");
    fs::create_dir(&fresh).unwrap();
    assert_success(&run_cpio(&["-id"], &fresh, &archive), "cpio -id");
    let extracted = filetime::FileTime::from_last_modification_time(
        &fs::metadata(fresh.join("a.txt")).unwrap(),
    );
    assert!(
        extracted.unix_seconds() >= source_mtime.unix_seconds(),
        "without -m the file takes the time of extraction"
    );
}

#[test]
fn test_cpio_unconditional_overwrite() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let archive = copy_out(&src, Some("newc"));
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    // A newer file on disk survives by default...
    fs::write(dest.join("a.txt"), "newer\n").unwrap();
    let future = filetime::FileTime::from_unix_time(
        filetime::FileTime::from_last_modification_time(&fs::metadata(src.join("a.txt")).unwrap())
            .unix_seconds()
            + 120,
        0,
    );
    filetime::set_file_mtime(dest.join("a.txt"), future).unwrap();
    run_cpio(&["-idm"], &dest, &archive);
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "newer\n");

    // ...but -u replaces it regardless.
    assert_success(&run_cpio(&["-idmu"], &dest, &archive), "cpio -idmu");
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
}

#[test]
fn test_cpio_archive_file_options() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    let out = run_cpio(
        &["-o", "-H", "newc", "-O", "../out.cpio"],
        &src,
        NAME_LIST.as_bytes(),
    );
    assert_success(&out, "cpio -o -O");
    assert!(temp.path().join("out.cpio").exists());
    assert!(out.stdout.is_empty(), "-O should keep stdout clean");

    let out = run_cpio(&["-idm", "-I", "../out.cpio"], &dest, b"");
    assert_success(&out, "cpio -i -I");
    assert_tree_extracted(&dest);
}

#[test]
fn test_cpio_rejects_unsupported_and_unknown_options() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    let out = run_cpio(&["-o", "-H", "hpodc"], &src, NAME_LIST.as_bytes());
    assert_failure(&out, "cpio -H hpodc");
    assert!(stderr_str(&out).contains("HP variants"));

    let out = run_cpio(&["-o", "-H", "nope"], &src, NAME_LIST.as_bytes());
    assert_failure(&out, "cpio -H nope");
    assert!(stderr_str(&out).contains("unknown archive format"));

    let out = run_cpio(&["-i", "--absolute-filenames"], &src, b"");
    assert_failure(&out, "cpio --absolute-filenames");
    assert!(stderr_str(&out).contains("below the current directory"));

    let out = run_cpio(&["-v"], &src, b"");
    assert_failure(&out, "cpio with no operation");
    assert!(stderr_str(&out).contains("is required"));

    let out = run_cpio(&["-o", "-i"], &src, b"");
    assert_failure(&out, "cpio -o -i");
    assert!(stderr_str(&out).contains("only one of"));
}

#[test]
fn test_cpio_help_and_version_exit_zero() {
    let temp = TempDir::new().unwrap();
    let out = run_cpio(&["--help"], temp.path(), b"");
    assert_success(&out, "cpio --help");
    assert!(stdout_str(&out).contains("Usage: cpio"));

    let out = run_cpio(&["--version"], temp.path(), b"");
    assert_success(&out, "cpio --version");
    assert!(stdout_str(&out).starts_with("cpio "));
}

#[test]
fn test_cpio_cross_tool_system_reads_our_newc_and_crc() {
    let Some(cpio) = system_tool("cpio") else {
        return;
    };

    // newc and crc are the formats this crate learned to write; the checksummed
    // one in particular is only proven correct by a reader that verifies it.
    for format in ["newc", "crc"] {
        let temp = TempDir::new().unwrap();
        let src = setup(temp.path());
        let dest = temp.path().join(format);
        fs::create_dir(&dest).unwrap();
        let archive = copy_out(&src, Some(format));

        let out = run_system_ok(&cpio, &["-idm"], &dest, Some(&archive));
        assert!(
            !String::from_utf8_lossy(&out.stderr).contains("checksum"),
            "system cpio reported a checksum problem in -H {}: {}",
            format,
            String::from_utf8_lossy(&out.stderr)
        );
        assert_tree_extracted(&dest);
    }
}

#[test]
fn test_cpio_cross_tool_we_read_system_newc() {
    let Some(cpio) = system_tool("cpio") else {
        return;
    };

    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    let made = run_system_ok(
        &cpio,
        &["-o", "-H", "newc"],
        &src,
        Some(NAME_LIST.as_bytes()),
    );

    assert_success(&run_cpio(&["-idm"], &dest, &made.stdout), "cpio -idm");
    assert_tree_extracted(&dest);
}

/// `cpio -t` alone lists the archive on standard input: -t implies -i.
#[test]
fn test_cpio_t_alone_lists() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let archive = copy_out(&src, Some("newc"));
    let out = run_cpio(&["-t"], temp.path(), &archive);
    assert_success(&out, "cpio -t");
    assert!(stdout_str(&out).contains("a.txt"), "{}", stdout_str(&out));
}

/// `find -depth` lists a directory after its contents, which is what lets
/// cpio restore a restrictive directory mode: the mode must arrive last and
/// stay, not be replaced by the 0755 created to hold the contents.
#[test]
fn test_cpio_depth_order_restores_directory_mode() {
    use std::os::unix::fs::PermissionsExt;
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d")).unwrap();
    fs::write(src.join("d/f"), "F\n").unwrap();
    fs::set_permissions(src.join("d"), fs::Permissions::from_mode(0o700)).unwrap();

    let out = run_cpio(&["-o", "-H", "newc"], &src, b"d/f\nd\n");
    assert_success(&out, "cpio -o");
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();
    let res = run_cpio(&["-idm"], &dst, &out.stdout);
    assert_success(&res, "cpio -idm");
    let mode = fs::metadata(dst.join("d")).unwrap().permissions().mode() & 0o7777;
    assert_eq!(mode, 0o700);
}

/// Pass mode copies a read-only directory's contents before applying its
/// mode; applying it first makes the directory unwritable and every file in
/// it fails with EACCES.
#[test]
fn test_cpio_pass_read_only_directory() {
    use std::os::unix::fs::PermissionsExt;
    if unsafe { libc::geteuid() } == 0 {
        return;
    }
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("ro")).unwrap();
    fs::write(src.join("ro/f"), "F\n").unwrap();
    fs::set_permissions(src.join("ro"), fs::Permissions::from_mode(0o555)).unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();

    let out = run_cpio(&["-pd", dst.to_str().unwrap()], &src, b"ro\nro/f\n");
    fs::set_permissions(src.join("ro"), fs::Permissions::from_mode(0o755)).unwrap();
    let copied = fs::read_to_string(dst.join("ro/f"));
    let _ = fs::set_permissions(dst.join("ro"), fs::Permissions::from_mode(0o755));
    assert_success(&out, "cpio -pd of a read-only directory");
    assert_eq!(copied.unwrap(), "F\n");
}

/// (st_dev, st_ino) of a name, not following a symbolic link.
fn file_id(path: &Path) -> (u64, u64) {
    use std::os::unix::fs::MetadataExt;
    let meta = fs::symlink_metadata(path).unwrap();
    (meta.dev(), meta.ino())
}

/// A newc link set of two names, `a` and `b`, sharing c_ino 5: each carries
/// the body given.
fn newc_pair(a: &[u8], b: &[u8]) -> Vec<u8> {
    let link = |name, body| CpioNewc {
        name,
        body,
        ino: 5,
        nlink: 2,
        ..Default::default()
    };
    let mut archive = link(b"a", a).member();
    archive.extend_from_slice(&link(b"b", b).archive());
    archive
}

/// Every unlinked member used to take a c_ino of its own, so once the field's
/// range had gone by -- 65535 members in the old binary format cpio writes by
/// default -- every later link set was written unlinked and came back as
/// separate files. Here one file named 65540 times uses the range up.
#[test]
fn test_cpio_links_survive_past_the_c_ino_range() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("f"), "f\n").unwrap();
    fs::write(src.join("zz_a"), "linked\n").unwrap();
    fs::hard_link(src.join("zz_a"), src.join("zz_b")).unwrap();

    let mut list = "f\n".repeat(65_540);
    list.push_str("zz_a\nzz_b\n");
    let out = run_cpio(&["-o", "--quiet"], &src, list.as_bytes());
    assert_success(&out, "cpio -o of 65542 names");

    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();
    assert_success(&run_cpio(&["-id", "zz_*"], &dst, &out.stdout), "cpio -id");
    assert_eq!(file_id(&dst.join("zz_a")), file_id(&dst.join("zz_b")));
    assert_eq!(fs::read_to_string(dst.join("zz_b")).unwrap(), "linked\n");
}

/// A name list can reach a file's names more often than its link count --
/// `find` output with a name repeated. The repeat must keep the set's c_ino,
/// and the reader must still know the set when it arrives, or the pair is
/// split on extraction.
#[test]
fn test_cpio_repeated_name_stays_linked() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("a"), "hello\n").unwrap();
    fs::hard_link(src.join("a"), src.join("b")).unwrap();

    let out = run_cpio(&["-o", "-H", "odc", "--quiet"], &src, b"a\nb\nb\n");
    assert_success(&out, "cpio -o naming b twice");

    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();
    let res = run_pax_with_stdin_bytes_in_dir(&["-r"], &out.stdout, &dst);
    assert_success(&res, "pax -r");
    assert_eq!(file_id(&dst.join("a")), file_id(&dst.join("b")));
    assert_eq!(fs::read_to_string(dst.join("b")).unwrap(), "hello\n");
}

/// Members sharing (c_dev, c_ino) with c_nlink 2 whose data differs are not
/// names of one file -- writers that truncate inode numbers make such
/// collisions. Each must keep its own data rather than the second becoming a
/// link to the first.
#[test]
fn test_cpio_colliding_inode_with_different_data_is_not_linked() {
    let temp = TempDir::new().unwrap();
    let archive = newc_pair(b"AAAA\n", b"BBBBBBBB\n");

    assert_success(&run_cpio(&["-id"], temp.path(), &archive), "cpio -id");
    assert_eq!(fs::read_to_string(temp.path().join("a")).unwrap(), "AAAA\n");
    assert_eq!(
        fs::read_to_string(temp.path().join("b")).unwrap(),
        "BBBBBBBB\n"
    );
    assert_ne!(
        file_id(&temp.path().join("a")),
        file_id(&temp.path().join("b"))
    );

    // The listing agrees: b is not shown as a link to a.
    let out = run_pax_with_stdin_bytes_in_dir(&["-v"], &archive, temp.path());
    assert_success(&out, "pax -v");
    assert!(!stdout_str(&out).contains("=="), "{}", stdout_str(&out));
}

/// newc stores a link set's data with its last name only. When that name is
/// not extracted -- not selected, kept by -k, renamed away by -s -- the data
/// must still reach the earlier names, not leave them empty.
#[test]
fn test_cpio_newc_set_data_reaches_earlier_names_when_last_is_skipped() {
    let archive = newc_pair(b"", b"hello\n");

    let temp = TempDir::new().unwrap();
    assert_success(
        &run_cpio(&["-id", "a"], temp.path(), &archive),
        "cpio -id a",
    );
    assert_eq!(
        fs::read_to_string(temp.path().join("a")).unwrap(),
        "hello\n"
    );
    assert!(!temp.path().join("b").exists());

    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("b"), "keep\n").unwrap();
    let out = run_pax_with_stdin_bytes_in_dir(&["-r", "-k"], &archive, temp.path());
    assert_success(&out, "pax -r -k");
    assert_eq!(
        fs::read_to_string(temp.path().join("a")).unwrap(),
        "hello\n"
    );
    assert_eq!(fs::read_to_string(temp.path().join("b")).unwrap(), "keep\n");

    let temp = TempDir::new().unwrap();
    let out = run_pax_with_stdin_bytes_in_dir(&["-r", "-s", ",^b$,,"], &archive, temp.path());
    assert_success(&out, "pax -r -s");
    assert_eq!(
        fs::read_to_string(temp.path().join("a")).unwrap(),
        "hello\n"
    );
    // Nothing is left behind but the extracted name.
    let names: Vec<_> = fs::read_dir(temp.path()).unwrap().collect();
    assert_eq!(names.len(), 1);
}

/// -I names the input archive; copy-out has none. GNU cpio refuses the
/// combination, and accepting it as the output silently truncated the file.
#[test]
fn test_cpio_copy_out_refuses_input_archive() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    fs::write(src.join("x"), "precious\n").unwrap();

    let out = run_cpio(&["-o", "-I", "x"], &src, NAME_LIST.as_bytes());
    assert_failure(&out, "cpio -o -I");
    assert_eq!(fs::read_to_string(src.join("x")).unwrap(), "precious\n");
}

/// -E is a file of patterns for copy-in. In copy-out and pass-through it used
/// to become the list of names, with the one on standard input ignored.
#[test]
fn test_cpio_pattern_file_only_in_copy_in() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    fs::write(src.join("pats"), "./sub/b.txt\n").unwrap();
    fs::create_dir(temp.path().join("dest")).unwrap();

    let out = run_cpio(&["-o", "-E", "pats"], &src, NAME_LIST.as_bytes());
    assert_failure(&out, "cpio -o -E");
    assert!(out.stdout.is_empty());

    let out = run_cpio(&["-p", "-E", "pats", "../dest"], &src, NAME_LIST.as_bytes());
    assert_failure(&out, "cpio -p -E");
    assert_eq!(fs::read_dir(temp.path().join("dest")).unwrap().count(), 0);
}

/// cpio without -u keeps a newer file -- the one at the name the member is
/// extracted under. With -r that is the name typed at the prompt: a newer file
/// there is kept, and one at the archived name does not stop the member.
#[test]
fn test_cpio_rename_keeps_newer_file_at_the_new_name() {
    use crate::common::{front_end, PtyPax, PtyStdio};
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    let mtime = |secs| filetime::FileTime::from_unix_time(secs, 0);
    fs::write(src.join("f"), "archived\n").unwrap();
    filetime::set_file_mtime(src.join("f"), mtime(1_000_000_000)).unwrap();
    let out = run_cpio(&["-o"], &src, b"f\n");
    assert_success(&out, "cpio -o");
    fs::write(temp.path().join("a.cpio"), &out.stdout).unwrap();

    let rename_to_g = |newer_at: &str| {
        let dest = temp.path().join(format!("dest-{newer_at}"));
        fs::create_dir(&dest).unwrap();
        fs::write(dest.join(newer_at), "newer\n").unwrap();
        filetime::set_file_mtime(dest.join(newer_at), mtime(2_000_000_000)).unwrap();
        let pty = PtyPax::spawn_program(
            &front_end("cpio"),
            &["-i", "-r", "-I", "../a.cpio"],
            &dest,
            b"g\n",
            PtyStdio::StdinOnly,
        );
        let (out, tty) = pty
            .finish(std::time::Duration::from_secs(20))
            .expect("cpio -i -r did not finish");
        assert_success(&out, "cpio -i -r");
        assert!(
            String::from_utf8_lossy(&tty).contains(" => "),
            "no prompt: {:?}",
            String::from_utf8_lossy(&tty)
        );
        dest
    };

    let dest = rename_to_g("g");
    assert_eq!(fs::read_to_string(dest.join("g")).unwrap(), "newer\n");

    let dest = rename_to_g("f");
    assert_eq!(fs::read_to_string(dest.join("g")).unwrap(), "archived\n");
    assert_eq!(fs::read_to_string(dest.join("f")).unwrap(), "newer\n");
}

/// Two archives one after the other on a standard input that is a file:
/// each `cpio -i` reads its own and leaves the file just past it, as POSIX
/// asks of a utility that stops before the end of a seekable input. Reading
/// ahead took the second archive with the first, and counted it in the first
/// one's block total too.
#[test]
fn test_cpio_reads_only_its_own_archive_from_standard_input() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    fs::write(src.join("a.txt"), vec![b'a'; 3000]).unwrap();
    let out = run_cpio(&["-o"], &src, NAME_LIST.as_bytes());
    assert_success(&out, "cpio -o");
    let written = stderr_str(&out);
    let one = out.stdout;
    assert!(one.len() > 512);

    let two = temp.path().join("two.cpio");
    fs::write(&two, [one.as_slice(), &one].concat()).unwrap();
    let out = std::process::Command::new("sh")
        .args(["-c", "\"$0\" -it && \"$0\" -it"])
        .arg(crate::common::front_end("cpio"))
        .stdin(fs::File::open(&two).unwrap())
        .output()
        .unwrap();
    assert_success(&out, "cpio -it twice");
    let names = stdout_str(&out);
    let once = names.lines().count() / 2;
    assert!(once > 0);
    assert_eq!(
        names.lines().take(once).collect::<Vec<_>>(),
        names.lines().skip(once).collect::<Vec<_>>()
    );
    // Each reports the size of one archive, which is what writing it reported.
    assert_eq!(stderr_str(&out), format!("{written}{written}"));
}

/// Under -n the extract loop stopped once every pattern was used, before the
/// last name of a newc link set -- the one that carries the data -- was read,
/// so the name extracted was left empty.
#[test]
fn test_cpio_first_match_reads_on_to_the_newc_data() {
    let archive = newc_pair(b"", b"hello world");
    let temp = TempDir::new().unwrap();
    let out = run_pax_with_stdin_bytes_in_dir(&["-r", "-n", "a"], &archive, temp.path());
    assert_success(&out, "pax -r -n a");
    assert_eq!(
        fs::read_to_string(temp.path().join("a")).unwrap(),
        "hello world"
    );
    assert!(!temp.path().join("b").exists());
}

/// A link set whose first name is replaced by an unrelated member of the same
/// name before its next name arrives: that next name is created empty, and
/// must not count as having the set's data, or the data on the last name is
/// skipped and every name is left linked to the empty file.
#[test]
fn test_cpio_newc_set_data_arrives_after_its_first_name_is_replaced() {
    let link = |name, body| CpioNewc {
        name,
        body,
        ino: 5,
        nlink: 3,
        ..Default::default()
    };
    let mut archive = link(b"a", b"").member();
    archive.extend_from_slice(
        &CpioNewc {
            name: b"a",
            body: b"OTHER",
            ino: 9,
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&link(b"b", b"").member());
    archive.extend_from_slice(&link(b"c", b"DATA").archive());

    let temp = TempDir::new().unwrap();
    let out = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert_success(&out, "pax -r");
    let read = |name| fs::read_to_string(temp.path().join(name)).unwrap();
    assert_eq!(read("a"), "OTHER");
    assert_eq!(read("b"), "DATA");
    assert_eq!(read("c"), "DATA");
    assert_eq!(
        file_id(&temp.path().join("b")),
        file_id(&temp.path().join("c"))
    );
}

/// An odc member: six-digit octal fields, the name, the body, no padding.
fn odc_member(name: &[u8], ino: u32, nlink: u32, body: &[u8]) -> Vec<u8> {
    let mut out = b"070707".to_vec();
    for v in [0, ino, 0o100644, 0, 0, nlink, 0] {
        out.extend_from_slice(format!("{v:06o}").as_bytes());
    }
    out.extend_from_slice(format!("{:011o}", 0).as_bytes()); // c_mtime
    out.extend_from_slice(format!("{:06o}", name.len() + 1).as_bytes());
    out.extend_from_slice(format!("{:011o}", body.len()).as_bytes());
    out.extend_from_slice(name);
    out.push(0);
    out.extend_from_slice(body);
    out
}

/// Run pax with `args` in `dir` allowed only `limit` open descriptors.
fn pax_with_fd_limit(args: &[&str], dir: &Path, limit: u64) -> std::process::Output {
    use std::os::unix::process::CommandExt;
    let mut command = std::process::Command::new(env!("CARGO_BIN_EXE_pax"));
    command.args(args).current_dir(dir);
    // SAFETY: setrlimit is async-signal-safe.
    unsafe {
        command.pre_exec(move || {
            let lim = libc::rlimit {
                rlim_cur: limit as libc::rlim_t,
                rlim_max: limit as libc::rlim_t,
            };
            if libc::setrlimit(libc::RLIMIT_NOFILE, &lim) != 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
    command.output().unwrap()
}

/// A link set of empty files never gets data, and the descriptor pinning its
/// file until the data came was never closed: one per set, so an archive of
/// more sets than the descriptor limit failed every member past it. In odc
/// every name carries the data, in newc the last one; neither has any to
/// wait for here.
#[test]
fn test_cpio_empty_link_sets_do_not_hold_a_descriptor_each() {
    const SETS: u32 = 200;
    for format in ["odc", "newc"] {
        let mut archive = Vec::new();
        for i in 0..SETS {
            for name in [format!("e{i}a"), format!("e{i}b")] {
                let member = match format {
                    "odc" => odc_member(name.as_bytes(), i + 1, 2, b""),
                    _ => CpioNewc {
                        name: name.as_bytes(),
                        ino: i + 1,
                        nlink: 2,
                        ..Default::default()
                    }
                    .member(),
                };
                archive.extend_from_slice(&member);
            }
        }
        archive.extend_from_slice(&odc_member(b"TRAILER!!!", 0, 1, b""));
        if format == "newc" {
            archive.truncate(archive.len() - odc_member(b"TRAILER!!!", 0, 1, b"").len());
            archive.extend_from_slice(
                &CpioNewc {
                    name: b"TRAILER!!!",
                    ..Default::default()
                }
                .member(),
            );
        }

        let temp = TempDir::new().unwrap();
        let path = temp.path().join("archive.cpio");
        fs::write(&path, &archive).unwrap();
        let dest = temp.path().join("dest");
        fs::create_dir(&dest).unwrap();
        let out = pax_with_fd_limit(&["-r", "-f", path.to_str().unwrap()], &dest, 64);
        assert_success(&out, &format!("pax -r of {SETS} empty {format} link sets"));
        for i in 0..SETS {
            let a = dest.join(format!("e{i}a"));
            let b = dest.join(format!("e{i}b"));
            assert_eq!(fs::read(&b).unwrap(), b"", "{format} e{i}b");
            assert_eq!(file_id(&a), file_id(&b), "{format} set {i} is linked");
        }
    }
}

/// -n stops reading once every pattern is used and no link set still waits
/// for data on a later name. An empty set waited for ever, so pax read the
/// rest of the archive: here, a damaged header it must never reach.
#[test]
fn test_cpio_first_match_stops_after_an_empty_link_set() {
    let link = |name| CpioNewc {
        name,
        ino: 5,
        nlink: 2,
        ..Default::default()
    };
    let mut archive = link(b"e1").member();
    archive.extend_from_slice(&link(b"e2").member());
    archive.extend_from_slice(
        &CpioNewc {
            name: b"sel",
            body: b"selected",
            ino: 6,
            ..Default::default()
        }
        .member(),
    );
    // Not a header: reading it is an error.
    archive.extend_from_slice(b"070701");
    archive.extend_from_slice(&[b'Z'; 200]);

    let temp = TempDir::new().unwrap();
    let path = temp.path().join("archive.cpio");
    fs::write(&path, &archive).unwrap();
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    let args = ["-r", "-n", "-f", path.to_str().unwrap(), "e1", "e2", "sel"];
    let out = crate::common::run_pax_in_dir(&args, &dest);
    assert_success(&out, "pax -r -n e1 e2 sel");
    assert_eq!(fs::read(dest.join("sel")).unwrap(), b"selected");
    assert_eq!(file_id(&dest.join("e1")), file_id(&dest.join("e2")));
}
