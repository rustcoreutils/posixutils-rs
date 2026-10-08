//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The GNU extensions `-i`/`--in-place[=SUFFIX]` and `-r`/`--regexp-extended`.
//! Each file edited in place is a stream of its own: its own line numbers and
//! its own `$`, as GNU sed does.

use plib::testing::{get_binary_path, run_test, TestPlan};
use plib::tmp::TempDir;
use std::fs;
use std::path::Path;
use std::process::{Command, Output};

/// Run sed in `dir` with `args`, stdin empty.
fn sed_in(dir: &Path, args: &[&str]) -> Output {
    Command::new(get_binary_path("sed"))
        .args(args)
        .current_dir(dir)
        .env("LC_ALL", "C")
        .stdin(std::process::Stdio::null())
        .output()
        .expect("run sed")
}

fn put(dir: &Path, name: &str, contents: &str) {
    fs::write(dir.join(name), contents).unwrap();
}

fn get(dir: &Path, name: &str) -> String {
    fs::read_to_string(dir.join(name)).unwrap()
}

/// The names in `dir`, sorted: no temporary may be left behind.
fn names(dir: &Path) -> Vec<String> {
    let mut v: Vec<String> = fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    v.sort();
    v
}

fn assert_ok(out: &Output) {
    assert_eq!(
        out.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(out.stdout.is_empty(), "stdout: {:?}", out.stdout);
}

#[test]
fn edits_the_file() {
    let dir = TempDir::new().unwrap();
    put(dir.path(), "f", "a\nb\n");
    assert_ok(&sed_in(dir.path(), &["-i", "s/a/A/", "f"]));
    assert_eq!(get(dir.path(), "f"), "A\nb\n");
    assert_eq!(names(dir.path()), ["f"]);
}

// The rule from a Debian package's debian/rules.
#[test]
fn debian_rules_substitution() {
    let dir = TempDir::new().unwrap();
    put(
        dir.path(),
        "substvars",
        "misc:Depends=debconf (>= 0.5) | debconf-2.0, foo\nmisc:Pre-Depends=\n",
    );
    assert_ok(&sed_in(
        dir.path(),
        &["-i", "/^misc:Depends=/s/debconf[^,]*[, ]*//", "substvars"],
    ));
    assert_eq!(
        get(dir.path(), "substvars"),
        "misc:Depends=foo\nmisc:Pre-Depends=\n"
    );
}

#[test]
fn each_file_has_its_own_line_numbers_and_last_line() {
    let dir = TempDir::new().unwrap();
    put(dir.path(), "f1", "a\nb\n");
    put(dir.path(), "f2", "c\nd\n");
    assert_ok(&sed_in(
        dir.path(),
        &["-i", "-e", "1d", "-e", "$s/$/!/", "f1", "f2"],
    ));
    assert_eq!(get(dir.path(), "f1"), "b!\n");
    assert_eq!(get(dir.path(), "f2"), "d!\n");
}

// Everything sed writes goes into the file: `=`, `i`, `p`.
#[test]
fn all_output_goes_to_the_file() {
    let dir = TempDir::new().unwrap();
    put(dir.path(), "f", "a\n");
    assert_ok(&sed_in(dir.path(), &["-i", "-n", "=;i\\\nbefore\np", "f"]));
    assert_eq!(get(dir.path(), "f"), "1\nbefore\na\n");
}

#[test]
fn attached_suffix_keeps_a_backup() {
    let dir = TempDir::new().unwrap();
    put(dir.path(), "f", "a\n");
    assert_ok(&sed_in(dir.path(), &["-i.bak", "s/a/b/", "f"]));
    assert_eq!(get(dir.path(), "f"), "b\n");
    assert_eq!(get(dir.path(), "f.bak"), "a\n");
    assert_eq!(names(dir.path()), ["f", "f.bak"]);
}

#[test]
fn long_option_with_suffix() {
    let dir = TempDir::new().unwrap();
    put(dir.path(), "f", "a\n");
    assert_ok(&sed_in(dir.path(), &["--in-place=.orig", "s/a/b/", "f"]));
    assert_eq!(get(dir.path(), "f"), "b\n");
    assert_eq!(get(dir.path(), "f.orig"), "a\n");
}

// `*` in the suffix stands for the file's name.
#[test]
fn star_in_suffix_is_the_file_name() {
    let dir = TempDir::new().unwrap();
    put(dir.path(), "f", "a\n");
    assert_ok(&sed_in(dir.path(), &["--in-place=old_*", "s/a/b/", "f"]));
    assert_eq!(get(dir.path(), "old_f"), "a\n");
}

// The `sed -ri '/^POT-Creation-Date:/ d' po/*.po` of Debian rules.
#[test]
fn r_and_i_in_one_cluster() {
    let dir = TempDir::new().unwrap();
    put(
        dir.path(),
        "x.po",
        "msgid \"\"\nPOT-Creation-Date: 2026\nab\n",
    );
    assert_ok(&sed_in(
        dir.path(),
        &["-ri", "/^POT-Creation-Date:/ d; s/(a)(b)/\\2\\1/", "x.po"],
    ));
    assert_eq!(get(dir.path(), "x.po"), "msgid \"\"\nba\n");
}

#[test]
fn i_after_n_in_a_cluster_takes_the_rest_as_suffix() {
    let dir = TempDir::new().unwrap();
    put(dir.path(), "f", "a\nb\n");
    assert_ok(&sed_in(dir.path(), &["-ni~", "2p", "f"]));
    assert_eq!(get(dir.path(), "f"), "b\n");
    assert_eq!(get(dir.path(), "f~"), "a\nb\n");
}

// `q` ends the run: the file being edited keeps what was written, the files
// after it are not touched.
#[test]
fn quit_stops_after_the_current_file() {
    let dir = TempDir::new().unwrap();
    put(dir.path(), "f1", "a\nb\n");
    put(dir.path(), "f2", "c\nd\n");
    assert_ok(&sed_in(dir.path(), &["-i", "1q", "f1", "f2"]));
    assert_eq!(get(dir.path(), "f1"), "a\n");
    assert_eq!(get(dir.path(), "f2"), "c\nd\n");
}

#[test]
fn not_a_regular_file_is_refused_and_the_rest_edited() {
    let dir = TempDir::new().unwrap();
    fs::create_dir(dir.path().join("d")).unwrap();
    put(dir.path(), "f", "a\n");
    let out = sed_in(dir.path(), &["-i", "s/a/b/", "d", "f"]);
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        "sed: couldn't edit d: not a regular file\n"
    );
    assert_ne!(out.status.code(), Some(0));
    assert_eq!(get(dir.path(), "f"), "b\n");
    assert_eq!(names(dir.path()), ["d", "f"]);
}

#[test]
fn missing_file_is_reported_and_the_rest_edited() {
    let dir = TempDir::new().unwrap();
    put(dir.path(), "f", "a\n");
    let out = sed_in(dir.path(), &["-i", "s/a/b/", "nope", "f"]);
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        "sed: can't read nope: No such file or directory\n"
    );
    assert_eq!(out.status.code(), Some(2));
    assert_eq!(get(dir.path(), "f"), "b\n");
}

#[test]
fn no_input_files() {
    run_test(TestPlan {
        cmd: String::from("sed"),
        args: vec!["-i".into(), "p".into()],
        stdin_data: String::from("a\n"),
        expected_out: String::new(),
        expected_err: String::from("sed: no input files\n"),
        expected_exit_code: 1,
    });
}

// -r is -E.
#[test]
fn r_selects_extended_regular_expressions() {
    for opt in ["-r", "--regexp-extended", "-E"] {
        run_test(TestPlan {
            cmd: String::from("sed"),
            args: vec![opt.into(), "s/(a|b)+/X/".into()],
            stdin_data: String::from("abba c\n"),
            expected_out: String::from("X c\n"),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

#[cfg(unix)]
mod unix {
    use super::*;
    use std::os::unix::fs::{symlink, PermissionsExt};

    #[test]
    fn mode_is_kept() {
        let dir = TempDir::new().unwrap();
        put(dir.path(), "f", "a\n");
        fs::set_permissions(dir.path().join("f"), fs::Permissions::from_mode(0o640)).unwrap();
        assert_ok(&sed_in(dir.path(), &["-i", "s/a/b/", "f"]));
        let mode = fs::metadata(dir.path().join("f"))
            .unwrap()
            .permissions()
            .mode();
        assert_eq!(mode & 0o7777, 0o640);
    }

    // As GNU sed without --follow-symlinks: the link is read through and
    // then replaced by a regular file; the file it pointed to is unchanged.
    #[test]
    fn symlink_is_replaced_not_written_through() {
        let dir = TempDir::new().unwrap();
        put(dir.path(), "target", "a\n");
        symlink("target", dir.path().join("link")).unwrap();
        assert_ok(&sed_in(dir.path(), &["-i", "s/a/b/", "link"]));
        let meta = fs::symlink_metadata(dir.path().join("link")).unwrap();
        assert!(meta.file_type().is_file());
        assert_eq!(get(dir.path(), "link"), "b\n");
        assert_eq!(get(dir.path(), "target"), "a\n");
    }

    // A FIFO is refused, not opened: opening one for reading would block.
    #[test]
    fn fifo_is_refused() {
        let dir = TempDir::new().unwrap();
        let fifo =
            std::ffi::CString::new(dir.path().join("p").into_os_string().into_string().unwrap())
                .unwrap();
        // SAFETY: a valid NUL-terminated path.
        assert_eq!(unsafe { libc::mkfifo(fifo.as_ptr(), 0o600) }, 0);
        let out = sed_in(dir.path(), &["-i", "p", "p"]);
        assert_eq!(
            String::from_utf8_lossy(&out.stderr),
            "sed: couldn't edit p: not a regular file\n"
        );
        assert_ne!(out.status.code(), Some(0));
    }
}
