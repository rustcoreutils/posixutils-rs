//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The command line: XBD 12.2 option syntax (ex 94191-94192), `-r`
//! (94198-94209, 94444-94451) and `-t` with file operands (94460-94469).

use plib::testing::get_binary_path;
use plib::tmp::TempDir;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

/// Run ex in `dir` with `args`, `script` on stdin; HOME and TMPDIR are
/// `dir`, so recovery files and `.exrc` stay inside it.
fn run(dir: &Path, args: &[&str], script: &str) -> (String, String, i32) {
    let mut child = Command::new(get_binary_path("ex"))
        .args(args)
        .current_dir(dir)
        .env("HOME", dir)
        .env("TMPDIR", dir)
        .env_remove("EXINIT")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ex");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(script.as_bytes())
        .unwrap();
    let out = child.wait_with_output().expect("wait for ex");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

fn with_file(content: &str) -> TempDir {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), content).unwrap();
    dir
}

#[test]
fn test_grouped_options_and_attached_arguments() {
    let dir = with_file("a\nb\nc\n");
    let d = dir.path();
    assert_eq!(run(d, &["-sc", "1d", "f"], "%p\nq!\n").0, "b\nc\n");
    assert_eq!(run(d, &["-sc1d", "f"], "%p\nq!\n").0, "b\nc\n");
    assert_eq!(run(d, &["-s", "-c2d", "f"], "%p\nq!\n").0, "a\nc\n");
    assert_eq!(run(d, &["-sRw5", "f"], "set window?\nq!\n").0, "window=5\n");
}

#[test]
fn test_double_dash_ends_options() {
    let dir = with_file("a\n");
    fs::write(dir.path().join("-R"), "dash\n").unwrap();
    let (out, _, code) = run(dir.path(), &["-s", "--", "-R"], "%p\nq!\n");
    assert_eq!((out.as_str(), code), ("dash\n", 0));
}

#[test]
fn test_several_c_options_run_in_order() {
    let dir = with_file("a\nb\nc\n");
    let (out, _, _) = run(dir.path(), &["-s", "-c", "1d", "-c", "1d", "f"], "%p\nq!\n");
    assert_eq!(out, "c\n");
    // `+cmd` is `-c cmd`.
    assert_eq!(run(dir.path(), &["-s", "+2d", "f"], "%p\nq!\n").0, "a\nc\n");
}

#[test]
fn test_bad_options_are_errors() {
    let dir = with_file("a\n");
    for args in [
        &["-x"][..],
        &["-w", "five"],
        &["-c"],
        &["-t", "a", "-t", "b"],
    ] {
        let (_, err, code) = run(dir.path(), args, "q!\n");
        assert_ne!(code, 0, "{args:?}");
        assert!(!err.is_empty(), "{args:?}");
    }
}

#[test]
fn test_recover_lists_before_reading_exrc() {
    // 94207-94209: .exrc is ignored; a command in it must not run.
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join(".exrc"), "!echo EXRC-RAN\n").unwrap();
    let (out, _, code) = run(dir.path(), &["-r"], "");
    assert_eq!(code, 0);
    assert!(!out.contains("EXRC-RAN"), "{out}");
    assert!(out.contains("No files to recover"), "{out}");
}

#[test]
fn test_recover_without_saved_data_edits_the_file() {
    // 94449-94451: a message, then the edit proceeds as usual.
    let dir = with_file("a\nb\n");
    let (out, err, code) = run(dir.path(), &["-s", "-r", "f"], "%p\nq!\n");
    assert_eq!((out.as_str(), code), ("a\nb\n", 0), "{err}");
}

#[test]
fn test_recover_finds_a_buffer_saved_under_another_spelling() {
    let dir = with_file("a\n");
    let d = dir.path();
    // Preserve a changed buffer, then recover it by `./f`.
    run(d, &["-s", "f"], "s/a/SAVED/\npreserve\nq!\n");
    let (out, err, code) = run(d, &["-s", "-r", "./f"], "%p\nq!\n");
    assert_eq!((out.as_str(), code), ("SAVED\n", 0), "{err}");
}

#[test]
fn test_tag_with_file_operands_keeps_them_in_the_argument_list() {
    let dir = with_file("a\n");
    let d = dir.path();
    fs::write(d.join("g"), "g1\n").unwrap();
    fs::write(d.join("tags"), "t\tf\t1\n").unwrap();
    let (out, err, code) = run(d, &["-s", "-t", "t", "g"], ".p\nn\n.p\nq!\n");
    assert_eq!((out.as_str(), code), ("a\ng1\n", 0), "{err}");
}

#[test]
fn test_recover_every_operand_on_its_first_edit() {
    let dir = with_file("a\n");
    let d = dir.path();
    fs::write(d.join("g"), "g\n").unwrap();
    run(d, &["-s", "f"], "s/a/A-SAVED/\npreserve\nq!\n");
    run(d, &["-s", "g"], "s/g/G-SAVED/\npreserve\nq!\n");
    let (out, err, _) = run(d, &["-s", "-r", "f", "g"], ".p\nn!\n.p\nq!\n");
    assert_eq!(out, "A-SAVED\nG-SAVED\n", "{err}");
}

#[test]
fn test_recover_command_uses_the_current_file() {
    let dir = with_file("a\n");
    let d = dir.path();
    fs::write(d.join("other"), "o\n").unwrap();
    run(d, &["-s", "other"], "s/o/OTHER/\npreserve\nq!\n");
    run(d, &["-s", "f"], "s/a/MINE/\npreserve\nq!\n");
    // Saved last is `f`'s, but `:recover` in `other` wants `other`'s.
    let (out, err, _) = run(d, &["-s", "other"], "recover!\n.p\nq!\n");
    assert_eq!(out, "OTHER\n", "{err}");
}

#[test]
fn test_recover_command_without_saved_data_edits_the_file() {
    let dir = with_file("a\n");
    let d = dir.path();
    fs::write(d.join("g"), "g\n").unwrap();
    let (out, err, code) = run(d, &["-s", "f"], "recover g\n.p\nq!\n");
    assert_eq!((out.as_str(), code), ("g\n", 0), "{err}");
}

#[test]
fn test_c_runs_from_the_last_line() {
    // 94454-94455, for a recovered file too.
    let dir = with_file("a\nb\nc\n");
    let d = dir.path();
    assert_eq!(run(d, &["-s", "-c", ".p", "f"], "q!\n").0, "c\n");
    run(d, &["-s", "f"], "1s/a/X/\npreserve\nq!\n");
    assert_eq!(run(d, &["-s", "-r", "-c", ".p", "f"], "q!\n").0, "c\n");
}

#[test]
fn test_c_runs_before_the_tag_movement() {
    // 94458-94459: -c from the last line, then the tag moves the cursor.
    let dir = with_file("a\nb\nc\n");
    let d = dir.path();
    fs::write(d.join("tags"), "t\tf\t/^b$/\n").unwrap();
    let (out, err, _) = run(d, &["-s", "-t", "t", "-c", ".p"], ".p\nq!\n");
    assert_eq!(out, "c\nb\n", "{err}");
}

#[test]
fn test_recover_with_tag_searches_the_recovered_text() {
    let dir = with_file("a\nb\n");
    let d = dir.path();
    fs::write(d.join("tags"), "t\tf\t/^b$/\n").unwrap();
    run(d, &["-s", "f"], "1i\nnew\n.\npreserve\nq!\n");
    let (out, err, _) = run(d, &["-s", "-r", "-t", "t"], ".=\nq!\n");
    assert_eq!(out, "3\n", "{err}");
}

#[test]
fn test_recover_listing_ignores_other_options() {
    let dir = TempDir::new().unwrap();
    let (out, _, code) = run(dir.path(), &["-r", "-w", "five"], "");
    assert_eq!(code, 0);
    assert!(out.contains("No files to recover"), "{out}");
}

#[test]
fn test_recovered_file_keeps_its_operand_name() {
    let dir = with_file("a\n");
    let d = dir.path();
    run(d, &["-s", "f"], "s/a/X/\npreserve\nq!\n");
    let (out, err, _) = run(d, &["-s", "-r", "f"], "!echo %\nq!\n");
    // `%` is the name as given, not the absolute one recovery records.
    assert!(out.lines().any(|l| l == "f"), "{out:?} {err}");
}
