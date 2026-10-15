//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A pattern rule without commands cancels the built-in rule of that shape
//! (GNU make). automake 1.17 ends every Makefile with
//! `%:: %,v`, `%:: RCS/%,v`, `%:: RCS/%`, `%:: s.%` and `%:: SCCS/s.%`,
//! which cancel GNU's version-control rules; this make has none of those,
//! so there is nothing to cancel, and the lines must simply not stop it.

use crate::common::Dir;

const AUTOMAKE_FOOTER: &str = "\
# Tell GNU make to disable its built-in pattern rules.
%:: %,v
%:: RCS/%,v
%:: RCS/%
%:: s.%
%:: SCCS/s.%
";

#[test]
fn automakes_footer_is_accepted() {
    let dir = Dir::new("cancel_automake");
    dir.write(
        "Makefile",
        &format!("all: hello\n\t@echo built\nhello:\n\t@echo hello\n{AUTOMAKE_FOOTER}"),
    );
    dir.make(&[]).ok("hello\nbuilt\n");
}

// A cancelling rule must not make a target out of nothing: with no rule for
// `missing`, make still reports that it cannot make it.
#[test]
fn a_cancelling_rule_makes_nothing() {
    let dir = Dir::new("cancel_nothing");
    dir.write("Makefile", &format!("all: missing\n{AUTOMAKE_FOOTER}"));
    let run = dir.make(&[]);
    assert_ne!(run.code, Some(0), "{run:?}");
    assert!(run.stderr.contains("missing"), "{run:?}");
}

#[test]
fn a_single_colon_pattern_rule_without_commands_is_not_used() {
    let dir = Dir::new("cancel_single_colon");
    dir.write("Makefile", "all: x.o\n%.o: %.c\n");
    dir.write("x.c", "");
    let run = dir.make(&["-r"]);
    assert_ne!(run.code, Some(0), "{run:?}");
    assert!(run.stderr.contains("x.o"), "{run:?}");
}
