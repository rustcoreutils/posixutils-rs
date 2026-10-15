//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A suffix may hold a dot. perl's Makefile declares
//! `.SUFFIXES: .c $(OBJ_EXT) .i .s .c.depends` and has a `.c.c.depends:`
//! rule; split at its second dot the name was a `.c.c` rule, making every
//! `.c` file from itself, and the build stopped with "recursive prerequisite
//! found trying to build 'op.c'".

use crate::common::Dir;

#[test]
fn a_rule_between_a_suffix_and_a_dotted_suffix() {
    let dir = Dir::new("dotted_suffix");
    dir.write(
        "Makefile",
        ".SUFFIXES: .c .o .c.depends\nall: op.o op.c.depends\n\
         .c.o:\n\t@echo cc $<\n.c.c.depends:\n\t@echo depend $< $@\n",
    );
    dir.write("op.c", "");
    dir.make(&[]).ok("cc op.c\ndepend op.c op.c.depends\n");
}

// Which known suffixes split the name decides the rule, not the dots.
#[test]
fn the_known_suffixes_split_the_name() {
    let dir = Dir::new("dotted_suffix_split");
    dir.write(
        "Makefile",
        ".SUFFIXES: .c.depends .o\nall: x.o\n.c.depends.o:\n\t@echo from $<\n",
    );
    dir.write("x.c.depends", "");
    dir.make(&[]).ok("from x.c.depends\n");
}
