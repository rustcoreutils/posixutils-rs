//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A command that fails inside `$(shell ...)` or `!=` gives its output and
//! nothing else: the status is not make's business (POSIX 105744-105748 says
//! nothing of it for `!=`, and GNU make ignores it for both).

use crate::common::Dir;

#[test]
fn a_failing_shell_command_is_not_fatal() {
    let dir = Dir::new("shell_status");
    dir.write(
        "Makefile",
        "X := $(shell false)\nY != false\nZ = $(shell echo out; exit 3)\n\
         all:\n\t@echo [$(X)] [$(Y)] [$(Z)] [$(shell exit 1)]\n",
    );
    dir.make(&[]).ok("[] [] [out] []\n");
}

#[test]
fn a_failing_shell_command_in_a_conditional_is_not_fatal() {
    let dir = Dir::new("shell_status_cond");
    dir.write(
        "Makefile",
        "ifeq ($(shell test 5 -lt 8; echo $$?), 0)\nR = yes\nendif\n\
         ifeq ($(shell md5-no-such-command < /dev/null 2>/dev/null; echo $$?), 0)\nR = no\nendif\n\
         all:\n\t@echo $(R)\n",
    );
    dir.make(&[]).ok("yes\n");
}

/// POSIX 105744-105746: the command runs "as if it were passed as the
/// argument to the system() interface", and only its standard output is
/// captured, so what it writes to standard error reaches make's. libselinux
/// runs `$(shell $(PKG_CONFIG) --cflags $(PCRE_MODULE))`, and pkg-config's
/// "Package ... was not found" was lost. GNU make agrees for both forms.
#[test]
fn a_shell_command_writes_its_diagnostics_to_standard_error() {
    let dir = Dir::new("shell_stderr");
    dir.write(
        "Makefile",
        "X != echo one >&2; echo x\nY := $(shell echo two >&2; echo y)\n\
         all:\n\t@echo [$(X)] [$(Y)]\n",
    );
    let run = dir.make(&[]);
    run.ok("[x] [y]\n");
    assert_eq!(run.stderr, "one\ntwo\n");
}
