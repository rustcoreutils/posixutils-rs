//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `-mindepth`, `-maxdepth` and `-printf`: GNU extensions that debhelper
//! runs for every Debian package (dh_update_autotools_config, dh_autoreconf,
//! dh_installdeb, dh_md5sums, dh_installgsettings, dh_movelibkdeinit).

use std::fs::{self, File};
use std::io::Write;
use std::os::unix::fs::symlink;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, UNIX_EPOCH};

use plib::testing::get_binary_path;

use super::{run_test_find, run_test_find_sorted, scratch_dir};

/// A tree with files at depths 1-2, a symlink, a hidden directory and a
/// regular file with a known size and nanosecond mtime:
///
/// ```text
/// a.txt (6 bytes, mtime 1577934245.123456789)   link -> a.txt
/// sub/b (2 bytes)   sub/deep/   .hid/b (1 byte)
/// ```
fn make_tree(tag: &str) -> PathBuf {
    let dir = scratch_dir(tag);
    let mut a = File::create(dir.join("a.txt")).unwrap();
    a.write_all(b"hello\n").unwrap();
    a.set_modified(UNIX_EPOCH + Duration::new(1_577_934_245, 123_456_789))
        .unwrap();
    fs::create_dir_all(dir.join("sub/deep")).unwrap();
    fs::write(dir.join("sub/b"), "x\n").unwrap();
    fs::create_dir(dir.join(".hid")).unwrap();
    fs::write(dir.join(".hid/b"), "y").unwrap();
    symlink("a.txt", dir.join("link")).unwrap();
    dir
}

/// Run find in `cwd` and return (stdout bytes, stderr, exit code).
fn find_in(cwd: &Path, args: &[&str]) -> (Vec<u8>, String, i32) {
    let output = Command::new(get_binary_path("find"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute find");
    (
        output.stdout,
        String::from_utf8_lossy(&output.stderr).into_owned(),
        output.status.code().unwrap_or(-1),
    )
}

fn sorted_lines(bytes: &[u8]) -> Vec<String> {
    let mut lines: Vec<String> = String::from_utf8_lossy(bytes)
        .lines()
        .map(String::from)
        .collect();
    lines.sort();
    lines
}

#[test]
fn find_mindepth_maxdepth() {
    let dir = make_tree("depth");
    let d = dir.to_str().unwrap();
    let at = |p: &str| format!("{d}/{p}");
    let (a, link, sub, hid) = (at("a.txt"), at("link"), at("sub"), at(".hid"));
    run_test_find_sorted(
        &[d, "-mindepth", "1", "-maxdepth", "1"],
        &[&a, &link, &sub, &hid],
        "",
        0,
    );
    run_test_find_sorted(&[d, "-maxdepth", "0"], &[d], "", 0);
    let (sub_b, deep, hid_b) = (at("sub/b"), at("sub/deep"), at(".hid/b"));
    run_test_find_sorted(&[d, "-mindepth", "2"], &[&sub_b, &deep, &hid_b], "", 0);
    // Global options: placed after a test they still apply to the whole walk.
    run_test_find_sorted(&[d, "-type", "f", "-maxdepth", "1"], &[&a], "", 0);
    // With -depth, the limits select the same entries.
    run_test_find_sorted(
        &[d, "-depth", "-mindepth", "2", "-maxdepth", "2"],
        &[&sub_b, &deep, &hid_b],
        "",
        0,
    );
    fs::remove_dir_all(&dir).unwrap();
}

/// dh_update_autotools_config: without `-mindepth 1` the starting point `.`
/// would match `-name '.*'` and be pruned.
#[test]
fn find_mindepth_dh_update_autotools_config() {
    let dir = make_tree("dh_autotools");
    let (out, err, code) = find_in(
        &dir,
        &[
            "-mindepth",
            "1",
            "(",
            "-type",
            "d",
            "-name",
            ".*",
            "-prune",
            ")",
            "-o",
            "-type",
            "f",
            "-name",
            "b",
            "-print",
        ],
    );
    assert_eq!(
        (sorted_lines(&out), err.as_str(), code),
        (vec!["./sub/b".to_string()], "", 0)
    );
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_maxdepth_bad_argument() {
    run_test_find(
        &[".", "-maxdepth", "x"],
        "",
        "find: invalid argument to -maxdepth: x\n",
        1,
    );
    run_test_find(
        &[".", "-mindepth"],
        "",
        "find: -mindepth requires an argument\n",
        1,
    );
}

/// dh_autoreconf's `timesize` and `md5` modes.
#[test]
fn find_printf_dh_autoreconf() {
    let dir = make_tree("dh_autoreconf");
    let (out, err, code) = find_in(
        &dir,
        &[
            "!",
            "-path",
            "./.hid/*",
            "-type",
            "f",
            "-printf",
            "%s|%T@  %p\\n",
            "-o",
            "-type",
            "l",
            "-printf",
            "symlink  %p\\n",
        ],
    );
    assert_eq!(
        (sorted_lines(&out), err.as_str(), code),
        (
            vec![
                "2|".to_string() + &mtime_at(&dir.join("sub/b")) + "  ./sub/b",
                "6|1577934245.1234567890  ./a.txt".to_string(),
                "symlink  ./link".to_string(),
            ],
            "",
            0
        )
    );
    fs::remove_dir_all(&dir).unwrap();
}

/// `%T@` as GNU find 4.9 prints it: seconds, a point, and ten digits.
fn mtime_at(path: &Path) -> String {
    let t = fs::metadata(path).unwrap().modified().unwrap();
    let d = t.duration_since(UNIX_EPOCH).unwrap();
    format!("{}.{:09}0", d.as_secs(), d.subsec_nanos())
}

/// dh_installdeb (`/etc/%P\n`), dh_md5sums (`%P\0`), dh_installgsettings
/// (`%P`): `%P` drops the starting point, and is empty for it.
#[test]
fn find_printf_relative_path_and_escapes() {
    let dir = make_tree("printf_rel");
    let d = dir.to_str().unwrap();
    let (out, _, code) = find_in(&dir, &[d, "-type", "f", "-printf", "/etc/%P\\n"]);
    assert_eq!(code, 0);
    assert_eq!(
        sorted_lines(&out),
        ["/etc/.hid/b", "/etc/a.txt", "/etc/sub/b"]
    );

    let (out, _, code) = find_in(&dir, &["-name", "a.txt", "-printf", "%P\\0"]);
    assert_eq!((out, code), (b"a.txt\0".to_vec(), 0));

    let (out, _, code) = find_in(&dir, &[".", "-maxdepth", "0", "-printf", "[%P]%%\\\\\\n"]);
    assert_eq!((out, code), (b"[]%\\\n".to_vec(), 0));
    fs::remove_dir_all(&dir).unwrap();
}

/// `-printf` is an action: no implicit `-print` is added.
#[test]
fn find_printf_suppresses_default_print() {
    let dir = make_tree("printf_action");
    let (out, _, code) = find_in(&dir, &["-name", "a.txt", "-printf", "x"]);
    assert_eq!((out, code), (b"x".to_vec(), 0));
    fs::remove_dir_all(&dir).unwrap();
}

/// Only the directives and escapes debhelper uses are implemented; any other
/// is refused before the walk rather than printed wrongly.
#[test]
fn find_printf_unsupported_is_an_error() {
    run_test_find(
        &[".", "-printf", "%u\\n"],
        "",
        "find: -printf: unsupported directive %u\n",
        1,
    );
    run_test_find(
        &[".", "-printf", "%-10p"],
        "",
        "find: -printf: unsupported directive %-\n",
        1,
    );
    run_test_find(
        &[".", "-printf", "a\\tb"],
        "",
        "find: -printf: unsupported escape \\t\n",
        1,
    );
    run_test_find(
        &[".", "-printf", "%"],
        "",
        "find: -printf: unsupported directive %\n",
        1,
    );
}

/// Output written before an `-exec` runs reaches stdout before the child's.
#[test]
fn find_printf_flushed_before_exec() {
    let dir = make_tree("printf_exec");
    let (out, _, code) = find_in(
        &dir,
        &[
            ".",
            "-maxdepth",
            "1",
            "-name",
            "a.txt",
            "-printf",
            "X",
            "-exec",
            "echo",
            "Y",
            ";",
        ],
    );
    assert_eq!((out, code), (b"XY\n".to_vec(), 0));
    let (out, _, code) = find_in(
        &dir,
        &[
            "sub", "-name", "b", "-printf", "X", "-exec", "echo", "Y", "{}", "+",
        ],
    );
    assert_eq!((out, code), (b"XY sub/b\n".to_vec(), 0));
    fs::remove_dir_all(&dir).unwrap();
}
