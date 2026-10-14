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
    sh_c(
        "sleep 30 & p=$!; ps -o comm= -p $p | sed 's|.*/||'; kill $p",
        "sleep\n",
    );
    sh_c("sleep 30 & p=$!; kill $p; wait $p; echo $?", "143\n");
    // A subshell or brace group whose last command is the utility: killing
    // `$!` still ends the job, and nothing is left running.
    sh_c(
        "(sleep 30) & p=$!; ps -o comm= -p $p | sed 's|.*/||'; kill $p",
        "sleep\n",
    );
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

/// Runs `sh -c script`, failing the test instead of hanging it when the
/// shell does not finish within two minutes.
fn sh_c_bounded(script: &str) -> std::process::Output {
    use std::process::{Command, Stdio};
    use std::sync::mpsc;
    use std::time::Duration;

    let child = Command::new(SH)
        .args(["-c", script])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn sh");
    let pid = child.id() as libc::pid_t;
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        let _ = tx.send(child.wait_with_output());
    });
    match rx.recv_timeout(Duration::from_secs(120)) {
        Ok(output) => output.expect("wait for sh"),
        Err(_) => {
            unsafe { libc::kill(pid, libc::SIGKILL) };
            panic!("the shell hung running {script:?}");
        }
    }
}

// A trap runs as soon as the command during which its signal arrived has
// finished, with `$?` that command's status, as in dash and bash; not at
// the next external command, or never if none follows. A trapped TERM sent
// to a loop of built-ins could not stop it at all.
#[test]
fn trap_runs_after_the_command_that_received_the_signal() {
    sh_c(
        "trap 'echo got' USR1; kill -USR1 $$; echo after",
        "got\nafter\n",
    );
    sh_c("trap 'echo got' USR1; echo hi; kill -USR1 $$", "hi\ngot\n");
    sh_c(
        "trap 'echo trap $?; false' USR1; kill -USR1 $$; echo $?",
        "trap 0\n0\n",
    );
    sh_c(
        "f() { kill -USR1 $$; echo inf; }; trap 'echo got' USR1; f; echo after",
        "got\ninf\nafter\n",
    );
    sh_c(
        "trap 'echo got' USR1; x=$(kill -USR1 $$); echo after",
        "got\nafter\n",
    );
    let output =
        sh_c_bounded("trap 'echo got; exit 3' TERM; (kill -TERM $$) & while :; do :; done");
    assert_eq!(String::from_utf8_lossy(&output.stdout), "got\n");
    assert_eq!(output.status.code(), Some(3));
}

// XCU 2.11: a trapped signal received while the shell waits for a
// foreground command runs its trap only after the command has completed,
// with `$?` the command's status, as in dash and bash. The shell ran the
// trap in the middle of the wait, before the command's output.
#[test]
fn trap_waits_for_the_foreground_command() {
    sh_c(
        &format!(
            "trap 'echo t' USR1; {SH} -c 'kill -USR1 $PPID; sleep 0.1; echo child'; echo after"
        ),
        "child\nt\nafter\n",
    );
    sh_c(
        &format!("trap 'echo t $?' USR1; {SH} -c 'kill -USR1 $PPID; exit 3'; echo $?"),
        "t 3\n3\n",
    );
}

// A signal that arrives while a trap action runs waits for it to finish
// instead of running a nested trap: a trap that sends its own signal again
// tens of thousands of times overflowed the stack once signals were checked
// after every command. dash and bash count to the end.
#[test]
fn trap_actions_do_not_nest() {
    let output = sh_c_bounded(
        "n=0; trap 'n=$((n+1)); [ $n -lt 50000 ] && kill -USR1 $$' USR1; kill -USR1 $$; echo $n",
    );
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
    assert_eq!(String::from_utf8_lossy(&output.stdout), "50000\n");
    assert!(output.status.success());
}

// A flood of trapped signals while the shell is blocked must neither hang it
// nor lose a signal. The shell is blocked reading a command substitution,
// whose reads are retried after each interruption without running traps,
// while the substitution's command sends it tens of thousands of USR1s and
// then one USR2. Every delivery used to write a record to a blocking
// self-pipe, which filled up, and the next signal handler blocked in write()
// forever. The same signal may be coalesced (XCU 2.11), so USR1 must run at
// least once and USR2 exactly once, as in dash and bash.
#[test]
fn signal_flood_neither_hangs_nor_loses_a_signal() {
    let script = format!(
        "n=0; trap 'n=$((n+1))' USR1; trap 'm=$((m+1))' USR2; \
         x=$({SH} -c 'i=0; while [ $i -lt 30000 ]; do kill -USR1 $1; i=$((i+1)); done; \
         kill -USR2 $1; echo done' flood $$); \
         echo $x; [ $n -ge 1 ] && echo usr1; echo usr2=$m"
    );
    let output = sh_c_bounded(&script);
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "done\nusr1\nusr2=1\n"
    );
    assert!(output.status.success());
}

// A shell started with no descriptors to spare cannot create its signal
// pipe, and must run (traps included) without it rather than panic, as dash
// and bash run. The limit leaves room for the dynamic loader's one file at a
// time, which is Linux's loader's need.
#[cfg(target_os = "linux")]
#[test]
fn shell_runs_without_descriptors_for_its_signal_pipe() {
    use std::os::unix::process::CommandExt;
    use std::process::{Command, Stdio};

    let mut command = Command::new(SH);
    command
        .args([
            "-c",
            "trap 'echo got' USR1; echo hi; kill -USR1 $$; /bin/sh -c :",
        ])
        .stdin(Stdio::null());
    unsafe {
        command.pre_exec(|| {
            let limit = libc::rlimit {
                rlim_cur: 4,
                rlim_max: 4,
            };
            if libc::setrlimit(libc::RLIMIT_NOFILE, &limit) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
    let output = command.output().expect("run sh");
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
    assert_eq!(String::from_utf8_lossy(&output.stdout), "hi\ngot\n");
    assert!(output.status.success());
}
