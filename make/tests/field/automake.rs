//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shapes from automake-generated Makefiles (gzip 1.13), where this make's
//! `-n` output differed from GNU make's.

use crate::common::Dir;
use std::time::{Duration, SystemTime};

fn set_age(dir: &Dir, name: &str, secs: u64) {
    std::fs::File::options()
        .write(true)
        .open(dir.path(name))
        .unwrap()
        .set_modified(SystemTime::UNIX_EPOCH + Duration::from_secs(secs))
        .unwrap();
}

// automake names targets `$(srcdir)/file`, `./file` in the source tree, and
// depends on them as `file`: one file, so one target.
#[test]
fn dot_slash_names_the_same_target() {
    let dir = Dir::new("am_dot_slash");
    dir.write(
        "Makefile",
        "srcdir = .\nall: config.hin\n\t@echo all\n$(srcdir)/config.hin:\n\t@echo making $@\n",
    );
    dir.make(&[]).ok("making config.hin\nall\n");
}

// `$(srcdir)/version.texi: $(srcdir)/stamp-vti` has no commands. The stamp's
// commands rewrite version.texi, but version.texi ran no command of its own,
// so it is judged by the time it had before -- as in GNU make, which does not
// look again -- and the manual made from it is not remade: a build from a
// release needs no makeinfo.
#[test]
fn a_target_without_commands_keeps_its_time() {
    let dir = Dir::new("am_version_texi");
    dir.write(
        "Makefile",
        "gzip.info: version.texi\n\t@echo MAKEINFO\n\
         version.texi: stamp-vti\n\
         stamp-vti: gzip.texi\n\t@echo stamp; touch version.texi stamp-vti\n",
    );
    for (name, age) in [
        ("version.texi", 1_000_000),
        ("gzip.info", 2_000_000),
        ("stamp-vti", 1_500_000),
        ("gzip.texi", 3_000_000),
    ] {
        dir.write(name, "");
        set_age(&dir, name, age);
    }
    dir.make(&[]).ok("stamp\n");
}

// Under -n a command that runs anyway is shown even with an `@`, as every
// command is under -n: automake's recursion is `@fail=; ... $(MAKE) ...`.
#[test]
fn a_silent_recursive_command_is_shown_under_dash_n() {
    let dir = Dir::new("am_recursive_n");
    dir.write("Makefile", "all:\n\t@echo sub; $(MAKE) -f sub.mk\n");
    dir.write("sub.mk", "all:\n\t@echo in sub\n");
    let run = dir.make(&["-n"]);
    assert_eq!(run.code, Some(0), "{run:?}");
    let lines: Vec<&str> = run.stdout.lines().collect();
    assert!(lines[0].starts_with("echo sub; "), "{run:?}");
    assert_eq!(&lines[1..], ["sub", "echo in sub"], "{run:?}");
}
