//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Signal dispositions the shell passes to the commands it runs, and the
//! process `$!` names.

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

// `$!` is the process of the background command itself, so `kill $!`
// reaches it. guile's test-guild-compile runs `guild compile ... &` and
// sends it SIGINT through `$!`.
#[test]
fn background_pid_is_the_command() {
    sh_c("sleep 30 & p=$!; ps -o comm= -p $p; kill $p", "sleep\n");
    sh_c("sleep 30 & p=$!; kill $p; wait $p; echo $?", "143\n");
    // A subshell or brace group whose last command is the utility: killing
    // `$!` still ends the job, and nothing is left running.
    sh_c("(sleep 30) & p=$!; ps -o comm= -p $p; kill $p", "sleep\n");
    sh_c("(sleep 30) & p=$!; kill $p; wait $p; echo $?", "143\n");
    sh_c("{ sleep 30; } & p=$!; kill $p; wait $p; echo $?", "143\n");
}

// Running the last command in place must not lose what the shell still has
// to do afterwards: an EXIT trap, the status inversion of `!`, the commands
// after it, or the status a command substitution reports.
#[test]
fn last_command_runs_in_place_only_when_nothing_follows() {
    sh_c("(trap 'echo bye' EXIT; /bin/echo x)", "x\nbye\n");
    sh_c("trap 'echo bye' EXIT; /bin/echo x", "x\nbye\n");
    sh_c("x=$(trap 'echo cs' EXIT; /bin/echo x); echo $x", "x cs\n");
    sh_c("(! /bin/false); echo $?", "0\n");
    sh_c("(/bin/false || /bin/echo or); echo $?", "or\n0\n");
    sh_c("(for i in 1 2; do /bin/echo $i; done)", "1\n2\n");
    sh_c("x=$(/bin/sh -c 'exit 3'); echo $?", "3\n");
    sh_c("(/bin/echo a | /bin/cat; /bin/echo b)", "a\nb\n");
    sh_c("( (/bin/echo a); /bin/echo b)", "a\nb\n");
    sh_c("(f() { /bin/echo f; }; f; /bin/echo after)", "f\nafter\n");
    sh_c("/bin/echo a\n/bin/echo b # comment\n", "a\nb\n");
}
