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
