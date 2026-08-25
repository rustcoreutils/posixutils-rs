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
    assert_failure, assert_success, have_tool, run_cpio, run_front_end, stderr_str, stdout_str,
};
use plib::tmp::TempDir;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

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
    if !have_tool("cpio") {
        eprintln!("skipping cross-tool test: no system cpio");
        return;
    }

    // newc and crc are the formats this crate learned to write; the checksummed
    // one in particular is only proven correct by a reader that verifies it.
    for format in ["newc", "crc"] {
        let temp = TempDir::new().unwrap();
        let src = setup(temp.path());
        let dest = temp.path().join(format);
        fs::create_dir(&dest).unwrap();
        let archive = copy_out(&src, Some(format));

        let mut child = match Command::new("cpio")
            .args(["-idm"])
            .current_dir(&dest)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(c) => c,
            Err(_) => {
                eprintln!("skipping cross-tool test: system cpio would not run");
                return;
            }
        };
        child.stdin.take().unwrap().write_all(&archive).unwrap();
        let out = child.wait_with_output().unwrap();
        assert!(
            out.status.success(),
            "system cpio rejected our -H {} archive: {}",
            format,
            String::from_utf8_lossy(&out.stderr)
        );
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
    if !have_tool("cpio") {
        eprintln!("skipping cross-tool test: no system cpio");
        return;
    }

    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    let made = Command::new("sh")
        .args(["-c", "find . | cpio -o -H newc"])
        .current_dir(&src)
        .output();
    let Ok(made) = made else {
        eprintln!("skipping cross-tool test: system cpio would not run");
        return;
    };
    if !made.status.success() {
        eprintln!("skipping cross-tool test: system cpio failed to create");
        return;
    }

    assert_success(&run_cpio(&["-idm"], &dest, &made.stdout), "cpio -idm");
    assert_tree_extracted(&dest);
}
