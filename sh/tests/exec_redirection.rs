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

// A here-document is standard input like any other, so `. /dev/stdin`
// reads it.
#[test]
fn heredoc_is_readable_as_dev_stdin() {
    sh_c(". /dev/stdin <<EOF\necho dot\nEOF", "dot\n");
    sh_c(". /dev/fd/3 3<<EOF\necho fd3\nEOF", "fd3\n");
    sh_c("{ . /dev/stdin; } <<EOF\necho braced\nEOF", "braced\n");
    sh_c("echo 'echo piped' | . /dev/stdin", "piped\n");
    // A script need not be valid text.
    sh_c("printf 'echo ok\\n#\\377\\n' | . /dev/stdin", "ok\n");
}

// A here-document is input: the descriptor a command is given for one is
// open only for reading, so writing to it fails with EBADF, as in dash
// (a pipe) and bash (a temporary file opened read-only).
#[test]
fn heredoc_descriptor_is_read_only() {
    sh_c("echo x 1<<EOF 2>/dev/null\ndoc\nEOF\necho st=$?", "st=1\n");
    sh_c(
        "exec 3<<EOF\ndoc\nEOF\nexec 4>&3; echo x 1>&4 2>/dev/null; echo st=$?; cat <&3",
        "st=1\ndoc\n",
    );
    sh_c("cat <<EOF\nbody\nEOF", "body\n");
}

// `n>&m` and `n<&m` duplicate descriptor m whatever it was opened for
// (XCU 2.7.5, 2.7.6); the direction only says which way the shell expects
// it to be used. sed's testsuite/bsd.sh saves and restores its standard
// descriptors with `exec 3>&0 4>&1 5>&2` ... `exec 0>&3 1>&4 2>&5`.
#[test]
fn duplication_ignores_direction() {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec![
            "-c".to_string(),
            "exec 3>&0 4>&1 5>&2; exec 0</dev/null 1>/dev/null 2>/dev/null; echo hidden; \
             exec 0>&3 1>&4 2>&5; echo back; read x; echo \"[$x]\""
                .to_string(),
        ],
        stdin_data: "in\n".to_string(),
        expected_out: "back\n[in]\n".to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
    sh_c("exec 3<&1; echo viadup >&3", "viadup\n");
    sh_c("{ echo inner; } 3<&1 1>&3", "inner\n");
    sh_c(
        "exec 3>/dev/null; exec 4<&3; echo four >&4; echo st=$?",
        "st=0\n",
    );
    // A copy used the other way fails where it is used, as the descriptor
    // does: reading a write-only one, or writing a read-only one.
    sh_c(
        "exec 3>/dev/null; read v <&3 2>/dev/null; echo st=$?",
        "st=1\n",
    );
    sh_c(
        "exec 3>/dev/null; exec 0<&3; read v 2>/dev/null; echo st=$?",
        "st=1\n",
    );
    sh_c(
        "exec 3</dev/null; exec 4>&3; echo x 1>&4 2>/dev/null; echo st=$?",
        "st=1\n",
    );
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), "exec 3>/dev/null; read v <&3".to_string()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: "read: read error: Bad file descriptor\n".to_string(),
        expected_exit_code: 1,
    });
}

// Running a command must not need descriptors at or above some floor the
// shell picks for itself: after `ulimit -n 10` with 3..9 closed, external
// commands still run (dash and bash agree). tar's extrac11 test checks
// that tar works with few descriptors exactly this way.
#[test]
fn commands_run_under_a_low_descriptor_limit() {
    sh_c(
        "(exec 3<&- 4<&- 5<&- 6<&- 7<&- 8<&- 9<&- && ulimit -n 10 && /bin/echo ok && /bin/echo ok2)",
        "ok\nok2\n",
    );
    sh_c("(ulimit -n 10; x=$(/bin/echo cs); echo $x)", "cs\n");
    // Redirections of an external command, including a swap that needs a
    // spare descriptor and a here-document, still work there too. bash
    // agrees; dash does not, because it saves descriptors at 10 and above.
    sh_c(
        "(exec 3<&- 4<&- 5<&- 6<&- 7<&- 8<&- 9<&-; ulimit -n 10; \
         /bin/echo a >/dev/null; { /bin/echo x 3>&1 1>&2 2>&3; } 2>&1; \
         /bin/cat 5<<EOF 0<&5
h5
EOF
)",
        "x\nh5\n",
    );
}

/// Runs `script` with standard output and standard error on pipes and
/// standard input on a pipe nobody writes to, so that it ends waiting in
/// `read`, and asserts that both output pipes reach end-of-file while it
/// waits: the shell process no longer holds either of them.
fn assert_output_released(script: &str, expected_out: &[u8], expected_err: &[u8]) {
    use std::io::Read;
    use std::process::{Command, Stdio};
    use std::sync::mpsc;
    use std::time::Duration;

    let mut child = Command::new(env!("CARGO_BIN_EXE_sh"))
        .args(["-c", script])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        let mut out = Vec::new();
        let mut err = Vec::new();
        stdout.read_to_end(&mut out).unwrap();
        stderr.read_to_end(&mut err).unwrap();
        tx.send((out, err)).unwrap();
    });
    // Both pipes reach EOF while the shell still waits in `read`.
    let got = rx.recv_timeout(Duration::from_secs(5));
    drop(child.stdin.take());
    let _ = child.wait();
    let (out, err) = got.expect("stdout/stderr never reached EOF while the shell ran");
    assert_eq!(out, expected_out);
    assert_eq!(err, expected_err);
}

// `exec` with redirections changes the shell's own descriptors, not just
// what later commands are given: after `exec 1>/dev/null` the process no
// longer holds the pipe it was started with, so a reader sees end-of-file
// while the shell is still running. guile's popen test reads a child's
// stdout to EOF after the child runs exactly this script, and hung.
#[test]
fn exec_redirection_releases_the_original_descriptor() {
    assert_output_released(
        "exec 1>/dev/null; echo closed 1>&2; exec 2>/dev/null; read REPLY",
        b"",
        b"closed\n",
    );
}

// `exec n>&-` really closes descriptor n, releasing the pipe it held. So
// does undoing a compound command's redirection: the copy of fd 1 it saved
// is closed once fd 1 is put back.
#[test]
fn exec_close_releases_a_pipe() {
    assert_output_released(
        "exec 3>&1; exec 1>/dev/null 2>&1; echo x >&3; exec 3>&-; read REPLY",
        b"x\n",
        b"",
    );
    assert_output_released(
        "{ exec 3>&1 4>&2; } >/dev/null; exec 1>&- 2>&- 4>&-; read REPLY",
        b"",
        b"",
    );
}

// The descriptor `exec` opens is the shell's own descriptor: a subshell, a
// utility, a compound command and `/dev/fd/3` all reach the same file.
#[test]
fn exec_descriptor_is_seen_by_subshells_and_utilities() {
    sh_c(
        "d=$(mktemp -d) && exec 3>\"$d/f\" && echo a >&3 && (echo b >&3) && \
         /bin/sh -c 'echo c >&3' && { echo e; } >&3 && echo d >>/dev/fd/3 && \
         cat \"$d/f\"; rm -rf \"$d\"",
        "a\nb\nc\ne\nd\n",
    );
    sh_c(
        "exec 3>&1; (echo sub >&3); ( (echo nested >&3) ); /bin/sh -c 'echo utility >&3'",
        "sub\nnested\nutility\n",
    );
}

// A descriptor `exec` closed is closed for everything that follows, and a
// command's redirection of a closed descriptor leaves it closed once the
// command completes.
#[test]
fn exec_closed_descriptor_is_closed_for_everything() {
    sh_c(
        "exec 3>/dev/null; exec 3>&-; { echo x >&3; } 2>/dev/null || echo builtin-closed; \
         (echo x >&3) 2>/dev/null || echo subshell-closed; \
         /bin/sh -c 'echo x >&3' 2>/dev/null || echo utility-closed",
        "builtin-closed\nsubshell-closed\nutility-closed\n",
    );
    sh_c(
        "exec 3>&-; : 3>/dev/null; { echo x >&3; } 2>/dev/null || echo still-closed",
        "still-closed\n",
    );
}

// A here-document opened by `exec` is a descriptor like any other: each
// `read` takes the next line, a subshell's included, and `cat` the rest.
#[test]
fn exec_here_document_on_a_descriptor() {
    sh_c(
        "exec 3<<EOF\nl1\nl2\nl3\nEOF\nread x <&3; read y <&3; echo \"$x/$y\"; cat <&3",
        "l1/l2\nl3\n",
    );
    sh_c(
        "exec 3<<EOF\na\nb\nEOF\n(read x <&3; echo \"sub $x\"); read y <&3; echo \"main $y\"",
        "sub a\nmain b\n",
    );
}

// The copies the shell keeps of descriptors a redirection replaced are its
// own: a utility run meanwhile inherits exactly the descriptors it would
// have inherited without them.
#[test]
fn saved_descriptors_do_not_leak_into_utilities() {
    sh_c(
        "p='for fd in 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do \
             (: <&$fd) 2>/dev/null && printf \"%s \" $fd; done; echo'
         a=$(/bin/sh -c \"$p\")
         b=$({ /bin/sh -c \"$p\"; } 2>/dev/null </dev/null)
         c=$(f() { /bin/sh -c \"$p\"; }; f 2>/dev/null)
         exec 5>/dev/null; exec 5>&-
         d=$(/bin/sh -c \"$p\")
         [ \"$a\" = \"$b\" ] && [ \"$a\" = \"$c\" ] && [ \"$a\" = \"$d\" ] && echo same",
        "same\n",
    );
}

// The process itself holds what `exec` opened, and no longer holds what it
// closed or replaced.
#[cfg(target_os = "linux")]
#[test]
fn exec_changes_the_process_descriptors() {
    sh_c(
        "d=$(mktemp -d); f=$(cd \"$d\" && pwd -P)/f; exec 3>\"$f\"; \
         [ \"$(readlink /proc/$$/fd/3)\" = \"$f\" ] && echo holds; \
         exec 3>&-; [ -e /proc/$$/fd/3 ] || echo released; \
         exec 5>&1 1>/dev/null; one=$(readlink /proc/$$/fd/1); echo \"$one\" >&5; rm -rf \"$d\"",
        "holds\nreleased\n/dev/null\n",
    );
}

// Descriptors above 9 are the user's too: the shell's own (here, its signal
// pipe) move out of the way, and keep working. bash agrees; dash accepts
// only single-digit descriptors.
#[test]
fn redirections_above_nine_leave_the_shell_working() {
    sh_c(
        "exec 10>/dev/null 11>/dev/null 12>/dev/null; trap 'echo trapped' USR1; \
         kill -USR1 $$; echo after; exec 10>&- 11>&- 12>&-; kill -USR1 $$; echo end",
        "trapped\nafter\ntrapped\nend\n",
    );
    sh_c(
        "{ echo ten >&10; echo eleven >&11; } 10>&1 11>&1",
        "ten\neleven\n",
    );
}

// POSIX 2.11: without job control an asynchronous list reads /dev/null
// unless its standard input is redirected explicitly. An enclosing
// command's redirection is explicit (bash; dash reads /dev/null there too);
// an earlier `exec` is not (dash and bash).
#[test]
fn background_standard_input_after_exec() {
    sh_c("{ cat & wait; } <<EOF\nbraced\nEOF", "braced\n");
    sh_c("exec 0<<EOF\nexecd\nEOF\ncat & wait", "");
}
