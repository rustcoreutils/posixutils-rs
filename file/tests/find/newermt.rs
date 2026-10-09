//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU `-newermt DATE`: the file was modified after DATE, which is read as `touch -d` and
//! `date -d` read a date.  binutils' Debian rules run
//! `find ... -depth -newermt '$(BUILD_DATE)' -print0`, the date taken from the changelog.

use std::fs::File;
use std::time::{Duration, UNIX_EPOCH};

use plib::testing::{run_test, TestPlan};
use plib::tmp::TempDir;

use super::run_test_find_sorted;

/// 2026-07-17 17:05:00 UTC: `Fri, 17 Jul 2026 19:05:00 +0200`.
const STAMP: u64 = 1_784_307_900;

/// A directory holding `old`, modified an hour before `STAMP`, `same`, modified at `STAMP`, and
/// `new`, an hour after.
fn tree() -> TempDir {
    let dir = TempDir::new().unwrap();
    for (name, secs) in [
        ("old", STAMP - 3600),
        ("same", STAMP),
        ("new", STAMP + 3600),
    ] {
        let file = File::create(dir.path().join(name)).unwrap();
        file.set_modified(UNIX_EPOCH + Duration::from_secs(secs))
            .unwrap();
    }
    dir
}

fn find_error(args: &[&str], expected_err: &str) {
    run_test(TestPlan {
        cmd: String::from("find"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::from(expected_err),
        expected_exit_code: 1,
    });
}

/// Strictly after DATE, in each form the shared date parser accepts: RFC 5322 (a Debian
/// changelog's), ISO 8601 with a zone, and `@SECONDS`.
#[test]
fn test_find_newermt() {
    let dir = tree();
    let d = dir.path().to_str().unwrap();
    let new = format!("{d}/new");
    for date in [
        "Fri, 17 Jul 2026 19:05:00 +0200",
        "2026-07-17T17:05:00Z",
        "2026-07-17 17:05:00 UTC",
        &format!("@{STAMP}"),
    ] {
        run_test_find_sorted(&[d, "-type", "f", "-newermt", date], &[&new], "", 0);
    }
    let same = format!("{d}/same");
    run_test_find_sorted(
        &[
            d,
            "-type",
            "f",
            "!",
            "-newermt",
            "@1784307900",
            "-name",
            "s*",
        ],
        &[&same],
        "",
        0,
    );
}

/// A date the parser does not accept, a missing one, and every other `-newerXY` form are
/// errors, before anything is walked.
#[test]
fn test_find_newermt_errors() {
    let dir = tree();
    let d = dir.path().to_str().unwrap();
    find_error(
        &[d, "-newermt", "yesterday"],
        "find: invalid date format: 'yesterday'\n",
    );
    find_error(&[d, "-newermt"], "find: -newermt requires an argument\n");
    for form in ["-newerma", "-newerat", "-newercm", "-newerBt", "-newermm"] {
        find_error(
            &[d, form, "x"],
            &format!("find: {form}: only -newermt is supported of the -newerXY forms\n"),
        );
    }
}

/// A DATE before the epoch with a fraction: `1969-12-31T23:59:59.5Z` is half a second before
/// the epoch, so a file modified 1.2 seconds before the epoch is not newer than it, and one
/// modified 0.3 seconds before the epoch is (as GNU find has it).
#[test]
fn test_find_newermt_before_epoch_fraction() {
    let dir = TempDir::new().unwrap();
    for (name, millis) in [("before", 1200), ("after", 300)] {
        let file = File::create(dir.path().join(name)).unwrap();
        file.set_modified(UNIX_EPOCH - Duration::from_millis(millis))
            .unwrap();
    }
    let d = dir.path().to_str().unwrap();
    let after = format!("{d}/after");
    run_test_find_sorted(
        &[d, "-type", "f", "-newermt", "1969-12-31T23:59:59.5Z"],
        &[&after],
        "",
        0,
    );
}
