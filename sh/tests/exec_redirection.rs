//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `exec` with redirections and no command changes the shell's own file
//! descriptors (XCU `exec`), wherever it runs in the current shell
//! environment: inside a compound command, a function or `eval` too.

use plib::testing::{run_test, TestPlan};

fn sh_c(script: &str, expected_out: &str) {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), script.to_string()],
        stdin_data: String::new(),
        expected_out: expected_out.to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// ncurses' autoconf-dickey opens its output with
// `case $outfile in -) exec 4>&1 ;; *) exec 4>$outfile;; esac`.
#[test]
fn exec_in_compound_command_persists() {
    sh_c("case x in *) exec 4>&1;; esac; echo case >&4", "case\n");
    sh_c("if true; then exec 4>&1; fi; echo if >&4", "if\n");
    sh_c("{ exec 4>&1; }; echo brace >&4", "brace\n");
    sh_c("for i in 1; do exec 4>&1; done; echo for >&4", "for\n");
    sh_c(
        "while :; do exec 4>&1; break; done; echo while >&4",
        "while\n",
    );
    sh_c("case x in *) exec >/dev/null;; esac; echo hidden", "");
}

#[test]
fn exec_in_function_and_eval_persists() {
    sh_c("f() { exec 4>&1; }; f; echo func >&4", "func\n");
    sh_c("eval 'exec 4>&1'; echo eval >&4", "eval\n");
}

// The descriptor made by `exec` is a copy of what fd 1 was at that moment;
// the compound command's own redirection is undone afterwards, the copy
// stays.
#[test]
fn exec_copy_outlives_compound_redirection() {
    sh_c("{ exec 4>&1; } >/dev/null; echo b2 >&4; echo out", "out\n");
    sh_c(
        "f() { exec 4>&1; } >/dev/null; f; echo fr >&4; echo out",
        "out\n",
    );
}

// A subshell's `exec` is its own.
#[test]
fn exec_in_subshell_does_not_persist() {
    sh_c(
        "(exec 4>&1); { echo sub >&4; } 2>/dev/null || echo subshell-ok",
        "subshell-ok\n",
    );
}
