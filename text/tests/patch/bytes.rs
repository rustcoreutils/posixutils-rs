//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Patches and patched files are bytes, not UTF-8. Debian's pcre2 .diff.gz
//! carries ISO-8859-1 text, and GNU patch applies it byte for byte.

use super::{cleanup_test_dir, setup_test_dir};
use plib::testing::run_test_base;
use std::fs;
use std::path::Path;

fn run_bytes(dir: &Path, args: &[&str], patch: &[u8]) -> (i32, String) {
    let mut full = vec![String::from("-d"), dir.to_str().unwrap().to_string()];
    full.extend(args.iter().map(|s| s.to_string()));
    let out = run_test_base("patch", &full, patch);
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stderr).to_string(),
    )
}

// ISO-8859-1 text in the patch and in the file: matched and written back byte
// for byte, including the untouched non-UTF-8 lines.
#[test]
fn test_patch_latin1_patch_and_file() {
    let dir = setup_test_dir("latin1");
    fs::write(dir.join("f.txt"), b"Ren\xe9\nold\n\xff\xfe end\n").unwrap();
    let patch =
        b"--- a/f.txt\n+++ b/f.txt\n@@ -1,3 +1,3 @@\n Ren\xe9\n-old\n+new \xe7a\n \xff\xfe end\n";
    let (code, err) = run_bytes(&dir, &["-p1"], patch);
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(
        fs::read(dir.join("f.txt")).unwrap(),
        b"Ren\xe9\nnew \xe7a\n\xff\xfe end\n"
    );
    cleanup_test_dir(&dir);
}

// A file created from a non-UTF-8 patch, and a reject file written from one,
// keep the bytes.
#[test]
fn test_patch_latin1_new_file_and_reject() {
    let dir = setup_test_dir("latin1_rej");
    fs::write(dir.join("g.txt"), b"other\n").unwrap();
    let patch = b"--- /dev/null\n+++ b/n.txt\n@@ -0,0 +1 @@\n+\xe9t\xe9\n--- a/g.txt\n+++ b/g.txt\n@@ -1 +1 @@\n-\xe0\n+\xe8\n";
    let (code, err) = run_bytes(&dir, &["-p1"], patch);
    assert_eq!(code, 1, "stderr: {}", err);
    assert_eq!(fs::read(dir.join("n.txt")).unwrap(), b"\xe9t\xe9\n");
    let rej = fs::read(dir.join("g.txt.rej")).unwrap();
    assert!(
        rej.windows(3).any(|w| w == b"- \xe0") && rej.windows(3).any(|w| w == b"+ \xe8"),
        "reject: {:?}",
        String::from_utf8_lossy(&rej)
    );
    cleanup_test_dir(&dir);
}

// -l lets <blank> runs differ, and nothing else: the byte 0xA0 (the tail of a
// UTF-8 "a with grave") is not a blank.
#[test]
fn test_patch_loose_whitespace_only_blanks() {
    let dir = setup_test_dir("loose_bytes");
    fs::write(dir.join("f.txt"), "x\u{e0} y\nold\n").unwrap();
    let patch = b"--- a/f.txt\n+++ b/f.txt\n@@ -1,2 +1,2 @@\n x\xc3 y\n-old\n+new\n";
    let (code, err) = run_bytes(&dir, &["-p1", "-l", "-F", "0"], patch);
    assert_eq!(code, 1, "stderr: {}", err);
    assert_eq!(
        fs::read_to_string(dir.join("f.txt")).unwrap(),
        "x\u{e0} y\nold\n"
    );
    cleanup_test_dir(&dir);
}

// A file name that is not UTF-8 names that file, byte for byte.
#[cfg(unix)]
#[test]
fn test_patch_non_utf8_file_name() {
    use std::ffi::OsStr;
    use std::os::unix::ffi::OsStrExt;
    let dir = setup_test_dir("latin1_name");
    let name = OsStr::from_bytes(b"caf\xe9.txt");
    if plib::testing::create_non_utf8(&dir, b"caf\xe9.txt", |p| fs::write(p, b"a\n")).is_none() {
        cleanup_test_dir(&dir);
        return;
    }
    let patch = b"--- a/caf\xe9.txt\n+++ b/caf\xe9.txt\n@@ -1 +1 @@\n-a\n+b\n";
    let (code, err) = run_bytes(&dir, &["-p1"], patch);
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(fs::read(dir.join(name)).unwrap(), b"b\n");
    cleanup_test_dir(&dir);
}
