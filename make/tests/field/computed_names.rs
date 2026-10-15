//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX 105801-105803: an inner macro expansion in a macro name is
//! performed first, and its result names the macro.

use crate::common::Dir;

/// automake's silent-rules idiom: the name is a literal prefix and a
/// reference, `$(am__v_CC_$(V))`.
const SILENT_RULES: &str = "\
am__v_CC_ = @echo CC $@;
am__v_CC_0 = @echo quiet $@;
am__v_CC_1 =
AM_V_CC = $(am__v_CC_$(V))
all:
\t$(AM_V_CC) true
";

#[test]
fn a_partly_computed_name_resolves() {
    let dir = Dir::new("computed_partly");
    dir.write("Makefile", SILENT_RULES);
    dir.make(&[]).ok("CC all\n");
}

#[test]
fn a_partly_computed_name_follows_a_command_line_macro() {
    let dir = Dir::new("computed_cmdline");
    dir.write("Makefile", SILENT_RULES);
    dir.make(&["V=0"]).ok("quiet all\n");
    dir.make(&["V=1"]).ok("true\n");
}

#[test]
fn computed_names_nest() {
    let dir = Dir::new("computed_nested");
    dir.write(
        "Makefile",
        "c = 1\nb_1 = 2\na_2 = deep\nall:\n\
         \t@echo [$(a_$(b_$(c)))] [${a_${b_$(c)}}] [$(a_$(b_$(c))_x)]\n",
    );
    dir.make(&[]).ok("[deep] [deep] []\n");
}

#[test]
fn a_computed_name_takes_a_substitution() {
    let dir = Dir::new("computed_subst");
    dir.write(
        "Makefile",
        "V = 1\nSRC_1 = a.c b.c\nall:\n\t@echo $(SRC_$(V):.c=.o)\n",
    );
    dir.make(&[]).ok("a.o b.o\n");
}

#[test]
fn a_computed_name_in_a_rule_line() {
    let dir = Dir::new("computed_rule");
    dir.write(
        "Makefile",
        "K = x\ndeps_x = one two\nall: $(deps_$(K))\n\t@echo $^\none two:\n\t@:\n",
    );
    dir.make(&[]).ok("one two\n");
}
