//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX 105556-105562: an escaped newline in a command line stays in the
//! command executed -- the backslash, the newline and the next line, less
//! the next line's leading tab. Inside a macro expansion it is a single
//! space, as anywhere else in the makefile.

use crate::common::Dir;

#[test]
fn the_escaped_newline_is_shown_and_executed() {
    let dir = Dir::new("cont_shown");
    dir.write(
        "Makefile",
        "all:\n\techo one \\\n\ttwo\n\t@printf '%s\\n' 'a \\\n\tb'\n",
    );
    // `sh` removes an escaped newline outside quotes, and keeps it inside
    // single quotes.
    dir.make(&[]).ok("echo one \\\ntwo\none two\na \\\nb\n");
    dir.make(&["-n"])
        .ok("echo one \\\ntwo\nprintf '%s\\n' 'a \\\nb'\n");
}

#[test]
fn only_a_leading_tab_is_removed_from_the_next_line() {
    let dir = Dir::new("cont_tab");
    dir.write("Makefile", "all:\n\t@echo 'x \\\n\t\ty \\\n  z'\n");
    dir.make(&[]).ok("x \\\n\ty \\\n  z\n");
}

// Inside a function call the escaped newline and the blanks after it are
// one space, so the call sees ordinary arguments (libzstd's
// `$(if $(filter ...),\ <newline> @echo ..., ...)`).
#[test]
fn inside_a_reference_it_is_a_space() {
    let dir = Dir::new("cont_reference");
    dir.write(
        "Makefile",
        "X = 1\nall:\n\t$(if $(X),\\\n    @echo yes,\\\n    @echo no)\n\t@echo $(subst a,\\\n  b,cat)\n",
    );
    dir.make(&[]).ok("yes\nc bt\n");
    dir.make(&["-n"]).ok("echo yes\necho c bt\n");
}
