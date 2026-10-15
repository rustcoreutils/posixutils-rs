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

#[test]
fn a_gnu_jobserver_is_passed_over() {
    run(" -j4 --jobserver-auth=3,4", &[]).ok("[] []\n");
    run(" --jobserver-auth=fifo:/tmp/GMfifo123", &[]).ok("[] []\n");
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
