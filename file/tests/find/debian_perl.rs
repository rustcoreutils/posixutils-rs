//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU extensions that Debian's perl packaging runs: its executable
//! `debian/perl.install` and `debian/perl-doc.install` list the manual pages
//! to ship with `find`, and an error there ships none.

use std::fs;

use super::{run_test_find_sorted, scratch_dir};

/// `-not` is `!`: `find usr/bin -type f -not -name perl -a -not -name perldoc`.
#[test]
fn find_not_is_bang() {
    let tmp = scratch_dir();
    let dir = tmp.path();
    for name in ["perl", "perldoc", "h2ph", "corelist"] {
        fs::write(dir.join(name), "").unwrap();
    }
    let d = dir.to_str().unwrap();
    let (h2ph, corelist) = (format!("{d}/h2ph"), format!("{d}/corelist"));
    run_test_find_sorted(
        &[
            d, "-type", "f", "-not", "-name", "perl", "-a", "-not", "-name", "perldoc",
        ],
        &[&h2ph, &corelist],
        "",
        0,
    );
    // `find usr/share/man/ -maxdepth 1 -mindepth 1 -not -name man1`
    fs::create_dir(dir.join("man1")).unwrap();
    fs::create_dir(dir.join("man3")).unwrap();
    let man3 = format!("{d}/man3");
    run_test_find_sorted(
        &[
            d,
            "-maxdepth",
            "1",
            "-mindepth",
            "1",
            "-type",
            "d",
            "-not",
            "-name",
            "man1",
        ],
        &[&man3],
        "",
        0,
    );
    // Like `!`, it needs an operand.
    run_test_find_sorted(
        &[d, "-not"],
        &[],
        "find: expected an expression after '-not'\n",
        1,
    );
}

/// `-printf %f` writes the basename: `find usr/share/man/man1 -type f -printf "%f\n"`.
#[test]
fn find_printf_basename() {
    let tmp = scratch_dir();
    let dir = tmp.path();
    fs::create_dir(dir.join("man1")).unwrap();
    fs::write(dir.join("man1/perl.1"), "").unwrap();
    fs::write(dir.join("man1/h2ph.1"), "").unwrap();
    let d = dir.to_str().unwrap();
    let man1 = format!("{d}/man1");
    run_test_find_sorted(
        &[&man1, "-type", "f", "-printf", "%f\\n"],
        &["h2ph.1", "perl.1"],
        "",
        0,
    );
    // A starting point's is its last component, any trailing slashes kept,
    // as in GNU find; `/` is its own.
    run_test_find_sorted(
        &[&format!("{man1}/"), "-maxdepth", "0", "-printf", "%f\\n"],
        &["man1/"],
        "",
        0,
    );
    run_test_find_sorted(&["/", "-maxdepth", "0", "-printf", "%f\\n"], &["/"], "", 0);
}
