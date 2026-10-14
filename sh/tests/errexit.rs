//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `set -e`: which failures make the shell exit (XCU `set`, -e).

use plib::testing::{run_test, TestPlan};

fn sh_c(script: &str, expected_out: &str, expected_exit_code: i32) {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), script.to_string()],
        stdin_data: String::new(),
        expected_out: expected_out.to_string(),
        expected_err: String::new(),
        expected_exit_code,
    });
}

// "If the exit status of a compound command other than a subshell command
// was the result of a failure while -e was being ignored, then -e shall not
// apply to this command." libzstd's cli-tests loop with
// `for file in ...; do zstd -t $file && die ...; done` under `set -e`.
#[test]
fn compound_command_ending_in_ignored_failure_does_not_exit() {
    sh_c(
        "set -e; for f in a b; do false && echo x; done; echo after",
        "after\n",
        0,
    );
    sh_c("set -e; { false && echo x; }; echo after", "after\n", 0);
    sh_c(
        "set -e; if true; then false && echo x; fi; echo after",
        "after\n",
        0,
    );
    sh_c(
        "set -e; case a in a) false && echo x;; esac; echo after",
        "after\n",
        0,
    );
    sh_c("set -e; { ! true; }; echo after", "after\n", 0);
    sh_c(
        "set -e; while true; do false && echo x; break; done; echo after",
        "after\n",
        0,
    );
}

// Where the failure was not ignored, or the command is a subshell, a
// function call or a command substitution's assignment, -e still applies.
#[test]
fn failures_that_still_exit() {
    sh_c(
        "set -e; for f in a; do false || false; done; echo after",
        "",
        1,
    );
    sh_c("set -e; (false && echo x); echo after", "", 1);
    sh_c("set -e; f() { false && echo x; }; f; echo after", "", 1);
    sh_c("set -e; x=$(false && echo); echo after", "", 1);
    sh_c("set -e; { false; }; echo after", "", 1);
}

// The flag goes with the status through nested compound commands, and a
// loop's status (its body's) keeps it even though the condition ran last.
// Checked against dash and bash.
#[test]
fn ignored_failure_propagates_through_compounds() {
    sh_c("set -e; { { false && :; }; }; echo after", "after\n", 0);
    sh_c(
        "set -e; for f in a; do { false && :; } >/dev/null; done; echo after",
        "after\n",
        0,
    );
    sh_c(
        "set -e; case x in x) { false && :; };; esac; echo after",
        "after\n",
        0,
    );
    sh_c(
        "set -e; i=0; while [ $i -lt 1 ]; do i=1; false && :; done; echo after",
        "after\n",
        0,
    );
    sh_c(
        "set -e; i=0; until [ $i -ge 1 ]; do i=1; ! true; done; echo after",
        "after\n",
        0,
    );
    sh_c("set -e; if true; then ! true; fi; echo after", "after\n", 0);
    sh_c(
        "set -e; { false && :; } || { false && :; }; echo after",
        "after\n",
        0,
    );
    // The status itself is still the failure's.
    sh_c("set -e; { false && :; }; echo $?", "1\n", 0);
}

// A real failure after an ignored one, a subshell, a function call around a
// compound command, a pipeline of several commands and eval all still exit.
#[test]
fn ignored_failure_does_not_leak() {
    sh_c("set -e; { { false && :; } || false; }; echo after", "", 1);
    sh_c("set -e; { false && :; }; false; echo after", "", 1);
    sh_c("set -e; ( { false && :; } ); echo after", "", 1);
    sh_c(
        "set -e; f() { for f in a; do false && :; done; }; f; echo after",
        "",
        1,
    );
    sh_c("set -e; true | { false && :; }; echo after", "", 1);
    sh_c("set -e; eval 'false && :'; echo after", "", 1);
}
