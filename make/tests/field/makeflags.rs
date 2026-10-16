//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `MAKEFLAGS` as a GNU make parent writes it, and as this make writes it
//! for its own children (POSIX 105856-105865).
//!
//! A Debian build runs `debian/rules` under GNU make, and the make it starts
//! below inherits GNU's `MAKEFLAGS`. Flags this make does not implement are
//! passed over; the ones it does are honoured, and so are the macros.

use crate::common::Dir;

const SHOW: &str = "all:\n\t@echo \"[$(V)] [$$MAKEFLAGS]\"\n";

fn run(makeflags: &str, args: &[&str]) -> crate::common::Run {
    let dir = Dir::new(&format!(
        "makeflags_{}",
        makeflags
            .chars()
            .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
            .collect::<String>()
    ));
    dir.write("Makefile", SHOW);
    dir.make_env(args, &[("MAKEFLAGS", makeflags)])
}

// A parent's token pool whose descriptors this make was not given leaves it
// serial, with GNU make's warning; one of a kind it does not take part in,
// GNU make 4.4's named pipe, leaves it serial too. Either way it passes no
// `-j` on.
#[test]
fn a_gnu_jobserver_that_cannot_be_had_leaves_make_serial() {
    let out = run(" -j4 --jobserver-auth=3,4", &[]);
    out.ok("[] []\n");
    assert_eq!(
        out.stderr,
        "make: warning: jobserver unavailable: using -j1.  Add '+' to parent make rule.\n"
    );
    let out = run(" -j4 --jobserver-auth=fifo:/tmp/GMfifo123", &[]);
    out.ok("[] []\n");
    assert_eq!(out.stderr, "");
    run(" --jobserver-auth=fifo:/tmp/GMfifo123", &[]).ok("[] []\n");
}

// A `-j` in MAKEFLAGS with no pool, as a parent other than GNU make or
// this one may write it, gives a pool of the make's own.
#[test]
fn an_inherited_j_without_a_pool_makes_one() {
    let out = run(" -j3", &[]);
    assert_eq!(out.code, Some(0), "{out:?}");
    assert!(
        out.stdout.starts_with("[] [-j3 --jobserver-auth="),
        "{out:?}"
    );
}

#[test]
fn unknown_letters_and_long_options_are_passed_over() {
    run("w", &[]).ok("[] []\n");
    run("w --no-print-directory", &[]).ok("[] []\n");
    run("rw --warn-undefined-variables --debug=b -Onone", &[]).ok("[] [r]\n");
}

#[test]
fn known_letters_are_honoured() {
    let out = run("sw", &[]);
    out.ok("[] [s]\n");
    let out = run("kn", &[]);
    out.ok("echo \"[] [$MAKEFLAGS]\"\n");
}

// GNU puts command-line macros after `--`, a blank in a value escaped.
#[test]
fn macros_after_the_separator_are_command_line_macros() {
    run(" --no-print-directory -- V=1", &[]).ok("[1] [-- V=1]\n");
    run("w -- V=a\\ b", &[]).ok("[a b] [-- V=a\\ b]\n");
}

// POSIX 105856: command-line macros are added to MAKEFLAGS, quoted so that
// a child make recovers the value.
#[test]
fn command_line_macros_are_passed_to_children() {
    let dir = Dir::new("makeflags_children");
    dir.write("Makefile", "all:\n\t@$(MAKE) -f sub.mk\n");
    dir.write(
        "sub.mk",
        "V = makefile\nall:\n\t@echo \"[$(V)] [$$MAKEFLAGS]\"\n",
    );
    dir.make(&["-s", "V=a b"]).ok("[a b] [s -- V=a\\ b]\n");
}

// libzstd's lib/Makefile runs `$(MAKE) --no-print-directory $@ ...`; this
// make never prints directories, so the option has nothing to turn off.
#[test]
fn no_print_directory_is_accepted() {
    let dir = Dir::new("makeflags_no_print_directory");
    dir.write("Makefile", SHOW);
    dir.make(&["--no-print-directory"]).ok("[] []\n");
}

// `-j` with no number, as GNU make takes it: libzstd's `make test` runs
// `$(MAKE) -j -C programs allVariants`, and `-C` was read as the job count.
#[test]
fn a_bare_j_takes_no_operand() {
    let dir = Dir::new("bare_j");
    dir.write("sub/Makefile", "all: a b\na b:\n\t@echo $@ >$@.out\n");
    let run = dir.make(&["-j", "-C", "sub", "all"]);
    assert_eq!(run.code, Some(0), "{run:?}");
    assert!(dir.path("sub/a.out").exists() && dir.path("sub/b.out").exists());
    // A number after it is still its operand, and a target after it is not.
    dir.write("Makefile", "t:\n\t@echo t\n");
    dir.make(&["-j", "2", "t"]).ok("t\n");
    dir.make(&["-j", "t"]).ok("t\n");
    dir.make(&["t", "-j"]).ok("t\n");
}

// A flag given twice is given once, as GNU make takes it: a recursive build
// that runs `$(MAKE) -s` under `make -s` finds `s` in MAKEFLAGS as well as
// on its command line. POSIX processes MAKEFLAGS as if its options were on
// the command line, so the repeat must not be an error.
#[test]
fn a_flag_given_twice_is_given_once() {
    run("s", &["-s"]).ok("[] [s]\n");
    run("k", &["-k"]).ok("[] [k]\n");
    let dir = Dir::new("makeflags_twice");
    dir.write("Makefile", SHOW);
    dir.make(&["-s", "-s"]).ok("[] [s]\n");
    dir.make(&["-k", "-s", "-k", "-s"]).ok("[] [sk]\n");
    // -f still adds a makefile each time it is given.
    dir.write("one.mk", "all: two\n");
    dir.write("two.mk", "two:\n\t@echo two\n");
    dir.make(&["-f", "one.mk", "-f", "two.mk"]).ok("two\n");
}
