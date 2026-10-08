//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A patch is untrusted input: whatever its headers claim, applying it must
//! cost time bounded by the size of the patch and the file, and end cleanly.

use super::{cleanup_test_dir, run_patch_capture, setup_test_dir};
use std::fs;
use std::path::Path;
use std::time::{Duration, Instant};

const LIMIT: Duration = Duration::from_secs(5);

/// Run patch on `file` (content given) with `patch`, extra `args` first;
/// return the exit code and how long it took.
fn timed(dir: &Path, file: &str, patch: &str, args: &[&str]) -> (i32, Duration) {
    fs::write(dir.join("f.txt"), file).unwrap();
    let pf = dir.join("p");
    fs::write(&pf, patch).unwrap();
    let mut full: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    full.extend([
        String::from("-d"),
        dir.to_str().unwrap().to_string(),
        String::from("-i"),
        pf.to_str().unwrap().to_string(),
        String::from("f.txt"),
    ]);
    let start = Instant::now();
    let (code, _) = run_patch_capture(full);
    (code, start.elapsed())
}

fn lines(n: usize, f: impl Fn(usize) -> String) -> String {
    (0..n).map(|i| f(i) + "\n").collect()
}

// A hunk whose header names a line far beyond any file: the search starts
// from the end of the file, not from that line.
#[test]
fn test_patch_huge_line_number_terminates() {
    let dir = setup_test_dir("bounds_huge_line");
    for header in [
        "@@ -9999999999999,3 +9999999999999,3 @@",
        "@@ -18446744073709551615,3 +18446744073709551615,3 @@",
    ] {
        let patch = format!("--- f.txt\n+++ f.txt\n{}\n x\n-y\n+Y\n z\n", header);
        let (code, took) = timed(&dir, "a\nb\nc\n", &patch, &[]);
        assert_eq!(code, 1, "{}: no match is a reject", header);
        assert!(took < LIMIT, "{}: took {:?}", header, took);
        let (code, took) = timed(&dir, "a\nx\ny\nz\n", &patch, &[]);
        assert_eq!(code, 0, "{}: found by searching back", header);
        assert!(took < LIMIT, "{}: took {:?}", header, took);
        assert_eq!(
            fs::read_to_string(dir.join("f.txt")).unwrap(),
            "a\nx\nY\nz\n"
        );
    }
    cleanup_test_dir(&dir);
}

// Line counts in the header far larger than the hunk's body must not be
// taken as a size to allocate or to walk. (GNU patch 2.7.6 gives up here with
// "out of memory"; this patch applies the lines the hunk really carries.)
#[test]
fn test_patch_huge_line_counts_terminate() {
    let dir = setup_test_dir("bounds_huge_count");
    let patch = "--- f.txt\n+++ f.txt\n@@ -1,999999999999999 +1,999999999999999 @@\n-a\n+A\n";
    let (code, took) = timed(&dir, "a\nb\n", patch, &[]);
    assert!((0..=2).contains(&code), "exit {}", code);
    assert!(took < LIMIT, "took {:?}", took);
    cleanup_test_dir(&dir);
}

// -F far beyond the hunk's context, on a hunk that matches nowhere.
#[test]
fn test_patch_huge_fuzz_terminates() {
    let dir = setup_test_dir("bounds_huge_fuzz");
    let patch = "--- f.txt\n+++ f.txt\n@@ -1,3 +1,3 @@\n p\n-q\n+Q\n r\n";
    for fuzz in ["1000000", "18446744073709551615"] {
        let (code, took) = timed(&dir, "a\nb\nc\n", patch, &["-F", fuzz]);
        assert_eq!(code, 1, "-F {}", fuzz);
        assert!(took < LIMIT, "-F {}: took {:?}", fuzz, took);
    }
    cleanup_test_dir(&dir);
}

// Empty file, and a hunk longer than the file.
#[test]
fn test_patch_hunk_longer_than_file() {
    let dir = setup_test_dir("bounds_long_hunk");
    let patch = format!(
        "--- f.txt\n+++ f.txt\n@@ -1,50 +1,50 @@\n{}-x\n+X\n{}",
        lines(25, |i| format!(" c{}", i)),
        lines(24, |i| format!(" d{}", i))
    );
    for file in ["", "c0\n", "x\n"] {
        let (code, took) = timed(&dir, file, &patch, &[]);
        assert_eq!(code, 1, "file {:?}", file);
        assert!(took < LIMIT, "took {:?}", took);
    }
    cleanup_test_dir(&dir);
}

// An ed script deleting a range far past the end of the file.
#[test]
fn test_patch_ed_huge_range_terminates() {
    let dir = setup_test_dir("bounds_ed_range");
    let (code, took) = timed(&dir, "a\nb\nc\n", "2,18446744073709551615d\n", &["-e"]);
    assert!(took < LIMIT, "took {:?}", took);
    assert!(code == 0 || code == 1 || code == 2, "exit {}", code);
    cleanup_test_dir(&dir);
}

// A 100 000-line file and a hunk that matches nowhere, at -F 0, at the
// largest fuzz, and with -l: each is one pass over the file, not one per
// line of the hunk.
#[test]
fn test_patch_large_file_no_match_is_linear() {
    let dir = setup_test_dir("bounds_large");
    let file = lines(100_000, |i| format!("line {}", i % 7));
    let patch = format!(
        "--- f.txt\n+++ f.txt\n@@ -50000,7 +50000,7 @@\n{}-nowhere\n+X\n{}",
        lines(3, |i| format!(" line {}", i)),
        lines(3, |i| format!(" line {}", i + 4))
    );
    for args in [&["-F", "0"][..], &["-F", "3"], &["-l"]] {
        let (code, took) = timed(&dir, &file, &patch, args);
        assert_eq!(code, 1, "{:?}", args);
        assert!(took < LIMIT, "{:?}: took {:?}", args, took);
    }
    cleanup_test_dir(&dir);
}

// The adversarial case for a naive search: 100 000 identical lines and a
// hunk of 20 000 of them followed by a line that is nowhere. Comparing the
// hunk at every position would be two billion line comparisons per fuzz
// level.
#[test]
fn test_patch_repetitive_file_no_match_is_linear() {
    let dir = setup_test_dir("bounds_repetitive");
    let file = lines(100_000, |_| String::from("a"));
    let patch = format!(
        "--- f.txt\n+++ f.txt\n@@ -1,20003 +1,20003 @@\n a\n{}-b\n+B\n a\n a\n",
        lines(20_000, |_| String::from("-a"))
    );
    for args in [&["-F", "0"][..], &["-F", "2"], &["-l"]] {
        let (code, took) = timed(&dir, &file, &patch, args);
        assert_eq!(code, 1, "{:?}", args);
        assert!(took < LIMIT, "{:?}: took {:?}", args, took);
    }
    cleanup_test_dir(&dir);
}

// Many hunks that each apply where they say: no hunk pays for a search of
// the whole file.
#[test]
fn test_patch_many_hunks_in_place() {
    let dir = setup_test_dir("bounds_many_hunks");
    let n = 200_000;
    let file = lines(n, |i| format!("line {}", i));
    let mut patch = String::from("--- f.txt\n+++ f.txt\n");
    for i in (1..n - 1).step_by(5) {
        patch.push_str(&format!(
            "@@ -{},3 +{},3 @@\n line {}\n-line {}\n+LINE {}\n line {}\n",
            i,
            i,
            i - 1,
            i,
            i,
            i + 1
        ));
    }
    let (code, took) = timed(&dir, &file, &patch, &["-F", "0"]);
    assert_eq!(code, 0);
    assert!(took < LIMIT, "took {:?}", took);
    let out = fs::read_to_string(dir.join("f.txt")).unwrap();
    assert!(out.starts_with("line 0\nLINE 1\nline 2\n"));
    cleanup_test_dir(&dir);
}

// Line numbers at the top of the range, in every format: the arithmetic
// that turns a header into a hunk must not overflow (a panic in a debug
// build, a wrapped nonsense position otherwise).
#[test]
fn test_patch_max_line_numbers_do_not_overflow() {
    let dir = setup_test_dir("bounds_max_numbers");
    let m = usize::MAX;
    let cases = [
        ("-u", format!("--- f.txt\n+++ f.txt\n@@ -{m},0 +{m},0 @@\n")),
        ("-u", format!("--- f.txt\n+++ f.txt\n@@ -{m},0 +1 @@\n+x\n")),
        ("-u", format!("--- f.txt\n+++ f.txt\n@@ -1 +{m},0 @@\n-a\n")),
        (
            "-u",
            format!("--- f.txt\n+++ f.txt\n@@ -{m},3 +{m},3 @@\n x\n-y\n+Y\n z\n"),
        ),
        ("-n", format!("{m}a{m}\n> x\n")),
        ("-n", format!("{m}d{m}\n< a\n")),
        ("-e", format!("{m}a\nx\n.\n")),
        ("-e", format!("1,{m}d\n")),
        (
            "-c",
            format!("*** f.txt\n--- f.txt\n***************\n*** {m} ****\n--- 1 ----\n+ x\n"),
        ),
        (
            "-c",
            format!("*** f.txt\n--- f.txt\n***************\n*** 1 ****\n- a\n--- {m} ----\n"),
        ),
    ];
    for (format, patch) in cases {
        let (code, took) = timed(&dir, "a\nb\nc\n", &patch, &[format]);
        assert!(
            (0..=2).contains(&code),
            "{} {:?}: exit {}",
            format,
            patch,
            code
        );
        assert!(took < LIMIT, "{:?}: took {:?}", patch, took);
    }
    cleanup_test_dir(&dir);
}
