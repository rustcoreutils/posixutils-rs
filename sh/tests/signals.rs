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

const SH: &str = env!("CARGO_BIN_EXE_sh");

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

// A signal ignored when the shell starts stays ignored, for the shell and
// for every command it runs (XCU 2.11): `trap '' HUP; sh -c cmd` keeps cmd
// safe from SIGHUP, as nohup-style wrappers rely on.
#[test]
fn signals_ignored_on_entry_stay_ignored_for_children() {
    sh_c(
        &format!("trap '' HUP; {SH} -c \"{SH} -c 'kill -HUP \\$\\$; echo survived'\""),
        "survived\n",
    );
    sh_c(
        &format!("trap '' TERM; {SH} -c \"{SH} -c 'kill -TERM \\$\\$; echo survived'\""),
        "survived\n",
    );
    // gzip's pipe-output test: gunzip and zcat are #!/bin/sh scripts that
    // exec gzip, which must see EPIPE (exit 1 or 2), not die of SIGPIPE.
    sh_c(
        &format!("trap '' PIPE; {SH} -c \"{SH} -c 'kill -PIPE \\$\\$; echo survived'\""),
        "survived\n",
    );
    // The same when the middle shell forks for the command instead of
    // replacing itself with it.
    sh_c(
        &format!("trap '' HUP; {SH} -c \"{SH} -c 'kill -HUP \\$\\$; echo survived'; :\""),
        "survived\n",
    );
    sh_c(
        &format!("trap '' INT; {SH} -c \"({SH} -c 'kill -INT \\$\\$; echo survived'); :\""),
        "survived\n",
    );
}

// A non-interactive shell cannot trap or reset a signal ignored on entry, and
// says nothing when asked to (bash, dash). `trap` lists it as ignored (XCU
// `trap`: "including signals that were ignored on entry").
#[test]
fn signals_ignored_on_entry_cannot_be_trapped_or_reset() {
    sh_c(
        &format!(
            "trap '' HUP; {SH} -c 'trap \"echo caught\" HUP; kill -HUP $$; trap - HUP; \
             kill -HUP $$; trap 1 HUP; kill -HUP $$; echo survived; trap'"
        ),
        "survived\ntrap -- '' HUP\n",
    );
}

// The shell must be able to wait for its children even when SIGCHLD was
// ignored when it started, which would have the system reap them first.
#[test]
fn sigchld_ignored_on_entry_does_not_break_wait() {
    sh_c(
        &format!("trap '' CHLD; {SH} -c '/bin/false; echo $?; sleep 0 & wait $!; echo $?'"),
        "1\n0\n",
    );
}

// Without job control (`set -m` off, as in every script), the commands of a
// pipeline stay in the shell's process group and the shell never touches the
// terminal's foreground group (XCU 2.11). The shell used to put each
// pipeline in a group of its own and hand it the terminal, and panicked
// when tcsetpgrp failed: DejaGnu runs binutils' tests under expect on a pty
// that goes away, and ~40 strip/objcopy/ar tests failed that way.
#[test]
fn pipeline_without_job_control_stays_in_the_shell_process_group() {
    sh_c(
        &format!(
            "a=$(ps -o pgid= -p $$); b=$({SH} -c 'ps -o pgid= -p $$' | cat); \
             [ $a = $b ] && echo same || echo \"differ $a $b\""
        ),
        "same\n",
    );
}
