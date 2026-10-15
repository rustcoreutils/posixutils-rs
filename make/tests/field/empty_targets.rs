//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A rule whose target list expands to nothing names no target. automake
//! writes `$(EXTRA_PROGRAMS): ...` and `$(am__depfiles_remade):` whatever
//! the lists hold.

use crate::common::Dir;

#[test]
fn an_empty_target_list_is_ignored_with_its_commands() {
    let dir = Dir::new("empty_targets");
    dir.write(
        "Makefile",
        "E =\nall:\n\t@echo ok\n$(E): foo\n\techo never\n$(E):\n\techo never\n",
    );
    dir.make(&[]).ok("ok\n");
}

#[test]
fn an_empty_first_rule_does_not_become_the_default() {
    let dir = Dir::new("empty_targets_first");
    dir.write(
        "Makefile",
        "E =\n$(E): foo\n\techo never\nall:\n\t@echo ok\n",
    );
    dir.make(&[]).ok("ok\n");
}
