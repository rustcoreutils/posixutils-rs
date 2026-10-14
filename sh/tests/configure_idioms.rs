//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shell constructs that autoconf's `configure`, the `config.status` it
//! writes, and libtool's `libtoolize` rely on. Each was found by building
//! Debian packages with this shell as `/bin/sh`.

use plib::testing::{run_test, TestPlan};

fn sh(script: &str, expected_out: &str) {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), script.to_string()],
        stdin_data: String::new(),
        expected_out: expected_out.to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// configure starts with `exec 7<&0 </dev/null`; every pipeline after it
// must still connect its commands' stdin to the pipe.
#[test]
fn pipeline_reads_its_pipe_after_exec_redirects_stdin() {
    sh("exec </dev/null; printf 'a\\n' | cat", "a\n");
    sh("exec 7<&0 </dev/null; printf 'a\\n' | sed s/a/b/", "b\n");
    sh(
        "exec </dev/null; x=`printf 'a\\n' | cat`; echo \"[$x]\"",
        "[a]\n",
    );
    sh(
        "exec </dev/null; x=$(printf 'a\\n' | cat); echo \"[$x]\"",
        "[a]\n",
    );
}

// Likewise for standard output: a pipe or a command substitution replaces
// whatever `exec` redirected it to, and a closed descriptor too.
#[test]
fn pipe_and_substitution_replace_exec_redirected_stdout() {
    sh(
        "exec 3>&1 >/dev/null; x=$(echo a); y=`echo b`; echo \"[$x$y]\" >&3",
        "[ab]\n",
    );
    sh("exec 3>&1 >/dev/null; echo a | cat >&3", "a\n");
    sh("exec 3>&1 >&-; x=$(echo a); echo \"[$x]\" >&3", "[a]\n");
    sh("exec <&-; echo a | cat", "a\n");
}

// A descriptor that `exec` made a copy of standard input or output keeps
// referring to that file inside a pipeline or command substitution;
// configure writes its messages to `exec 6>&1` this way.
#[test]
fn exec_copy_of_standard_fd_survives_pipe() {
    sh("exec 3>&1; x=$(echo hi >&3); echo \"[$x]\"", "hi\n[]\n");
    sh(
        "exec 3>&1 >/dev/null; x=$(echo a; echo b >&3); echo \"[$x]\" >&3",
        "b\n[a]\n",
    );
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec![
            "-c".to_string(),
            "exec 7<&0 </dev/null; echo a | cat; cat <&7".to_string(),
        ],
        stdin_data: "in\n".to_string(),
        expected_out: "a\nin\n".to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// The redirection made by `exec` stays in effect for commands that are not
// in a pipeline.
#[test]
fn exec_redirection_of_stdin_persists() {
    sh("exec </dev/null; cat; echo done", "done\n");
}

// Inside backquotes, `\\` stands for `\` and `\$` for `$` (XCU 2.6.3), so
// libtoolize's `` `eval echo "\\\$$my_dirvar"` `` reads the named variable.
#[test]
fn backquote_backslash_dollar_escapes() {
    sh(
        "v=foo foo=bar; x=`eval echo \"\\\\\\$$v\"`; echo \"[$x]\"",
        "[bar]\n",
    );
    sh(
        "x=`echo \\\\$HOME_UNSET_X`; echo \"[$x]\"",
        "[$HOME_UNSET_X]\n",
    );
    sh("x=`echo '\\$'`; echo \"[$x]\"", "[$]\n");
    sh("x=`echo \"\\`echo in\\`\"`; echo \"[$x]\"", "[in]\n");
    // `\\` is one escaped backslash, so the backquote after it ends the
    // substitution.
    sh("x=`echo \\\\`; echo \"[$x]\"", "[\\]\n");
    sh("echo `echo \\\\` `echo b`", "\\ b\n");
    // Inside double quotes, `\"` in the backquotes also stands for `"`.
    sh("echo \"`echo \\\"hi\\\"`\"", "hi\n");
    sh("echo `echo \\\"hi\\\"`", "\"hi\"\n");
}

// Unquoted `$*` where no field splitting happens (an assignment) joins the
// parameters with the first character of IFS (XCU 2.5.2). configure splits
// the canonical host with IFS=- and rebuilds host_os with `host_os=$*`.
#[test]
fn unquoted_star_in_assignment_joins_with_ifs() {
    sh(
        "IFS=-; set x a b-c; shift; v=$*; echo \"[$v]\"",
        "[a-b-c]\n",
    );
    sh("set a b c; v=$*; echo \"[$v]\"", "[a b c]\n");
    sh("IFS=; set a b c; v=$*; echo \"[$v]\"", "[abc]\n");
    sh(
        "set a b; case $* in 'a b') echo yes;; *) echo no;; esac",
        "yes\n",
    );
}

// In a here-document with an unquoted delimiter, quotes are ordinary
// characters and a backslash is special only before $, `, \ and newline
// (XCU 2.7.4). configure writes config.status this way.
#[test]
fn heredoc_body_keeps_quotes_literal() {
    sh(
        "v=X; cat <<EOF\na='$v' b=\"$v\" c=\\$v d=\\\" e=\\\\\nEOF",
        "a='X' b=\"X\" c=$v d=\\\" e=\\\n",
    );
    sh("v=X; cat <<EOF\n'${v}' \"`echo $v`\"\nEOF", "'X' \"X\"\n");
    sh("cat <<'EOF'\na='$v' \\$\nEOF", "a='$v' \\$\n");
    // `$'...'` is not special there either, and a backslash before anything
    // else stays.
    sh("cat <<EOF\n$'a\\tb' \\x\nEOF", "$'a\\tb' \\x\n");
    // The word of `${v-word}` is parsed as inside double quotes: `"` quotes,
    // `'` is ordinary. A pattern's quotes are still quotes.
    sh(
        "u= v=X; cat <<EOF\n${u:-'a'} ${u:-\"b\"} ${u:-\\\"c\\\"} [${v#'X'}] [${v#\"X\"}]\nEOF",
        "'a' b \"c\" [] []\n",
    );
}

// Inside double quotes, the word of `${v-word}` is double-quoted too, so a
// single quote in it is an ordinary character (dash and bash agree), while
// the pattern of `${v#pattern}` keeps its quoting.
#[test]
fn single_quotes_in_parameter_word_inside_double_quotes() {
    sh("u=; echo \"${u:-'a'}\"", "'a'\n");
    sh(
        "u=; echo \"${u:-\"x\"'y'\"z\"}\" ${u:-'p'\"q\"}",
        "x'y'z pq\n",
    );
    sh(
        "u=; printf '%s\\n' \"${u:-\\'d\\'}\" \"${u:-a\\\\b}\"",
        "\\'d\\'\na\\b\n",
    );
    sh("v=X; echo \"[${v#'X'}]\"", "[]\n");
}

// "not found" belongs to the command, so its own `2>/dev/null` hides it;
// libtool probes for `+=` that way.
#[test]
fn command_not_found_message_obeys_redirection() {
    sh("V+=x 2>/dev/null; echo $?", "127\n");
    sh(
        "no_such_command_xyz 2>/dev/null || echo missing",
        "missing\n",
    );
    sh("{ no_such_command_xyz; } 2>/dev/null; echo $?", "127\n");
}

// A PATH assignment in front of a command is the PATH the command is
// searched with (XCU 2.9.1), and does not leave a remembered location
// behind for later commands.
#[test]
fn path_prefix_assignment_is_used_for_search() {
    sh(
        "env >/dev/null; { PATH=/nonexistent env >/dev/null; } 2>/dev/null; echo $?",
        "127\n",
    );
    sh(
        "p=$PATH; PATH=/nonexistent; PATH=$p env >/dev/null; a=$?; { env >/dev/null; } 2>/dev/null; b=$?; PATH=$p; echo $a $b",
        "0 127\n",
    );
}

// LINENO is the line of the command being executed (XCU 2.5.3), also when
// it is expanded in a here-document or a command substitution.
#[test]
fn lineno_in_heredoc_and_command_substitution() {
    sh("echo $LINENO\n\necho $LINENO", "1\n3\n");
    sh(":\ncat <<EOF\n$LINENO\nEOF", "2\n");
    sh(":\n:\necho `echo $LINENO` $(echo $LINENO)", "3 3\n");
    // A command is numbered by the line it starts on, not the line after
    // its here-document body or its last continuation line.
    sh(":\necho $LINENO <<EOF\nx\nEOF\necho $LINENO", "2\n5\n");
    sh(":\necho a \\\n$LINENO $(echo $LINENO)", "a 2 2\n");
}
