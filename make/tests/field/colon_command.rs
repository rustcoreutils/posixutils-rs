//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A command line that is only `:` does nothing, so make does not start
//! the standard shell to run it; it is still shown and still succeeds. A
//! shell the makefile chose is another matter: whatever it does with `:`
//! is its own business, and it is run.

use crate::common::Dir;
use std::os::unix::fs::PermissionsExt;

#[test]
fn a_colon_command_is_shown_and_succeeds() {
    let dir = Dir::new("colon_shown");
    dir.write("Makefile", "all: sub\n\t@echo all\nsub:\n\t:\n\t@ : \n");
    dir.make(&[]).ok(":\nall\n");
}

#[test]
fn a_makefile_shell_is_run_for_a_colon_command() {
    let dir = Dir::new("colon_custom_shell");
    dir.write("trace.sh", "#!/bin/sh\necho \"shell $*\"\n");
    let script = dir.path("trace.sh");
    let mut mode = std::fs::metadata(&script).unwrap().permissions();
    mode.set_mode(0o755);
    std::fs::set_permissions(&script, mode).unwrap();
    dir.write("Makefile", "SHELL = ./trace.sh\nall:\n\t@:\n");
    dir.make(&[]).ok("shell -e -c :\n");
}
