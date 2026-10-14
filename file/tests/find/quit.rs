//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU `-quit`: stop at once, with the exit status so far.  util-linux's Debian rules test
//! whether a bash completion exists with `find "debian/tmp/$BCNAME" -quit`; without `-quit`
//! every completion was left out of the packages.

use std::fs::{create_dir, File};

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use plib::tmp::TempDir;

fn plan(args: &[&str]) -> TestPlan {
    TestPlan {
        cmd: String::from("find"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    }
}

/// util-linux's form: an operand that exists succeeds silently, one that does not fails.
#[test]
fn find_quit_tests_existence() {
    let dir = TempDir::new().unwrap();
    let file = dir.path().join("a");
    File::create(&file).unwrap();
    run_test(plan(&[file.to_str().unwrap(), "-quit"]));
    let missing = dir.path().join("nosuch");
    run_test_with_checker(plan(&[missing.to_str().unwrap(), "-quit"]), |_, out| {
        assert!(out.stdout.is_empty());
        assert!(!out.stderr.is_empty());
        assert_eq!(out.status.code(), Some(1));
    });
}

/// Primaries after `-quit` never run, and the walk stops at the first file that reaches it.
#[test]
fn find_quit_stops_the_walk() {
    let dir = TempDir::new().unwrap();
    create_dir(dir.path().join("d")).unwrap();
    for name in ["a", "b", "d/c"] {
        File::create(dir.path().join(name)).unwrap();
    }
    let root = dir.path().to_str().unwrap();
    run_test_with_checker(plan(&[root, "-type", "f", "-print", "-quit"]), |_, out| {
        assert_eq!(out.status.code(), Some(0));
        assert_eq!(String::from_utf8_lossy(&out.stdout).lines().count(), 1);
    });
    run_test(plan(&[root, "-quit", "-print"]));
}

/// The `-exec ... {} +` set gathered before `-quit` is still run, once; one after it is not.
#[test]
fn find_quit_runs_pending_exec_batch() {
    let dir = TempDir::new().unwrap();
    create_dir(dir.path().join("d")).unwrap();
    for name in ["a", "b", "d/c"] {
        File::create(dir.path().join(name)).unwrap();
    }
    let root = dir.path().to_str().unwrap();
    let args = [
        root, "-type", "f", "-exec", "echo", "batch", "{}", "+", "-quit",
    ];
    run_test_with_checker(plan(&args), |_, out| {
        assert_eq!(out.status.code(), Some(0));
        assert!(out.stderr.is_empty());
        let stdout = String::from_utf8_lossy(&out.stdout);
        let lines: Vec<&str> = stdout.lines().collect();
        assert_eq!(lines.len(), 1, "{stdout}");
        let words: Vec<&str> = lines[0].split_whitespace().collect();
        assert_eq!(words.len(), 2, "{stdout}");
        assert_eq!(words[0], "batch");
        assert!(words[1].starts_with(root), "{stdout}");
    });
    run_test(plan(&[root, "-quit", "-exec", "echo", "batch", "{}", "+"]));
}

/// Starting points after the one that reached `-quit` are not walked, and an error from one
/// before it still makes the exit status 1.
#[test]
fn find_quit_skips_later_operands() {
    let dir = TempDir::new().unwrap();
    let a = dir.path().join("a");
    let b = dir.path().join("b");
    File::create(&a).unwrap();
    File::create(&b).unwrap();
    let (a, b) = (a.to_str().unwrap(), b.to_str().unwrap());
    let missing = dir.path().join("nosuch");
    let missing = missing.to_str().unwrap();

    let mut p = plan(&[a, b, "-print", "-quit"]);
    p.expected_out = format!("{a}\n");
    run_test(p);

    run_test_with_checker(plan(&[missing, a, b, "-print", "-quit"]), |_, out| {
        assert_eq!(String::from_utf8_lossy(&out.stdout), format!("{a}\n"));
        assert!(String::from_utf8_lossy(&out.stderr).contains("nosuch"));
        assert_eq!(out.status.code(), Some(1));
    });
}

/// With `-depth` the first file acted on is a leaf; `-quit` there stops the walk, and the
/// directories still open are not acted on on the way out.
#[test]
fn find_quit_with_depth() {
    let dir = TempDir::new().unwrap();
    create_dir(dir.path().join("d")).unwrap();
    File::create(dir.path().join("d/c")).unwrap();
    let root = dir.path().to_str().unwrap();
    let mut p = plan(&[root, "-depth", "-print", "-quit"]);
    p.expected_out = format!("{root}/d/c\n");
    run_test(p);
}
