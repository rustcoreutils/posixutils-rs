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
