//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `\#` outside a command line is a literal `#`, not the start of a comment
//! (GNU make). libzstd writes `NUM_SYMBOL := \#`; build-essential's
//! debian/rules writes `$(shell grep -v '^\#' debian/cross-targets)`, where
//! the comment used to cut the reference in half (`UnexpectedEOF`).

use crate::common::Dir;

#[test]
fn an_escaped_hash_in_a_macro_value() {
    let dir = Dir::new("hash_value");
    dir.write(
        "Makefile",
        "NUM_SYMBOL := \\#\nX = a\\#b # a comment\nall:\n\t@echo '[$(NUM_SYMBOL)] [$(X)]'\n",
    );
    // The blank before the comment stays in the value, as in GNU make.
    dir.make(&[]).ok("[#] [a#b ]\n");
}

#[test]
fn an_escaped_hash_in_a_shell_call() {
    let dir = Dir::new("hash_shell");
    dir.write("targets", "# comment\namd64\n# another\narm64\n");
    dir.write(
        "Makefile",
        "cross_archs := $(shell grep -v '^\\#' targets)\nall:\n\t@echo $(cross_archs)\n",
    );
    dir.make(&[]).ok("amd64 arm64\n");
}

#[test]
fn an_escaped_hash_in_a_conditional_and_a_rule_line() {
    let dir = Dir::new("hash_rule");
    dir.write(
        "Makefile",
        "H = \\#\nifeq ($(H),\\#)\nOK = yes\nendif\nall: dep\\#1\n\t@echo $(OK) $^\ndep\\#1:\n\t@:\n",
    );
    dir.make(&[]).ok("yes dep#1\n");
}

// A command line goes to the shell as written, backslash and all.
#[test]
fn a_command_line_keeps_its_backslash() {
    let dir = Dir::new("hash_command");
    dir.write("Makefile", "all:\n\t@echo '\\#'\n");
    dir.make(&[]).ok("\\#\n");
}
