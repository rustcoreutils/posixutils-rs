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
    sh("unset IFS; set a b c; v=$*; echo \"[$v]\"", "[a b c]\n");
    sh("IFS=-; set a b c; v=x$*y; echo \"[$v]\"", "[xa-b-cy]\n");
    sh(
        "IFS=-; set a b c; case $* in a-b-c) echo yes;; *) echo no;; esac",
        "yes\n",
    );
    sh(
        "IFS=-; set a b c; case a-b-c in $*) echo yes;; *) echo no;; esac",
        "yes\n",
    );
    // The word of `${u:-word}`, `${u+word}` and `${x=word}` is expanded in the
    // context of the whole expansion.
    sh(
        "IFS=-; set a b c; v=${u:-$*}; w=${v+$*}; echo \"[$v][$w]\"",
        "[a-b-c][a-b-c]\n",
    );
    sh(
        "IFS=-; set a 'b c'; v=${x=$*}; echo \"[$v][$x]\"",
        "[a-b c][a-b c]\n",
    );
    sh("IFS=-; set a b c; v=${*#a}; echo \"[$v]\"", "[-b-c]\n");
    // Where field splitting is performed, `$*` still splits.
    sh("IFS=-; set a b c; echo $*; echo ${u:-$*}", "a b c\na b c\n");
}

// Unquoted `$@` in an assignment is unspecified by POSIX; it joins as `$*`
// does, as in dash.
#[test]
fn unquoted_at_in_assignment_joins_like_star() {
    sh("IFS=-; set a b c; v=$@; echo \"[$v]\"", "[a-b-c]\n");
    sh("IFS=; set a b c; v=$@; echo \"[$v]\"", "[abc]\n");
    sh("unset IFS; set a b c; v=$@; echo \"[$v]\"", "[a b c]\n");
    sh("IFS=-; set a b c; v=${@%c}; echo \"[$v]\"", "[a-b-]\n");
}

// The word of a double-quoted `${p:-word}` (also `=`, `?`, `+`) is within
// those double quotes (XCU 2.2.3): not field split, not globbed, no tilde
// expansion, single quotes literal, and `\}` is a `}` (XCU 2.6.2). The
// pattern of `${p#word}` and its siblings is not. configure is full of
// `"${CONFIG_SHELL-/bin/sh}"` and `"${var+set}"`.
#[test]
fn double_quoted_parameter_word_is_quoted() {
    sh("x='a  b'; echo \"${u:-$x}\" \"${x+$x}\"", "a  b a  b\n");
    sh("IFS=-; set a b c; echo \"${u:-$*}\"", "a-b-c\n");
    sh(
        "set -- a 'b  c'; for i in \"${u:-$@}\"; do echo \"[$i]\"; done",
        "[a]\n[b  c]\n",
    );
    sh("echo \"${u:-/*}\" \"${u:-~}\"", "/* ~\n");
    sh("y=Y; echo \"${y:+'$y'}\" \"${u:-a\\}b}\"", "'Y' a}b\n");
    sh("echo \"${u=a  b}\"; echo \"$u\"", "a  b\na  b\n");
    // `\"` in a pattern is a literal `"`, so `%\"c\"` removes nothing.
    sh(
        "x=abc; echo \"${x#*}\" \"${x#'a'}\" \"${x%\\\"c\\\"}\" \"${x%\"c\"}\"",
        "abc bc abc ab\n",
    );
    // Unquoted, the word is still split and globbed.
    sh("x='a  b'; echo ${u:-$x} ${u:-/}", "a b /\n");
}

// `\}` in the word of `${p-word}` is a literal `}` under every operator,
// within double quotes, nested double quotes and a here-document alike.
#[test]
fn backslash_brace_in_quoted_parameter_word() {
    sh(
        "u=; printf '%s\\n' \"${u:-a\\}b}\" \"${u-a\\}b}\" \"${u:=a\\}b}\" \"$u\"",
        "a}b\n\na}b\na}b\n",
    );
    sh(
        "u=; v=x; printf '%s\\n' \"${v:+a\\}b}\" \"${u+a\\}b}\" \"${u:-a\\}}\"",
        "a}b\na}b\na}\n",
    );
    sh("u=; printf '%s\\n' \"${u:-\"a\\}b\"}\"", "a}b\n");
    sh("u=; printf '%s\\n' \"${u:-a\\\\}b}\"", "a\\b}\n");
    sh("u=; cat <<EOF\n${u:-a\\}b}\nEOF", "a}b\n");
    sh("x='a}b'; printf '%s\\n' \"${x%\\}b}\"", "a\n");
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
    sh(
        "exec 2>&1; no_such_command_xyz 2>/dev/null; echo $?",
        "127\n",
    );
    // Likewise a utility that is found but cannot be executed.
    sh("exec 2>&1; /dev/null 2>/dev/null; echo $?", "126\n");
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

// `read` drops NUL bytes from its input, as dash and bash do (the user's
// choice, 2026-10-14; POSIX leaves input with NULs unspecified). A shell
// variable cannot hold a NUL, and keeping it let `read x <f; cd "$x"` reach
// a system call with an embedded NUL.
#[test]
fn read_drops_nul_bytes() {
    plib::testing::run_test_u8(plib::testing::TestPlanU8 {
        cmd: "sh".to_string(),
        args: vec![
            "-c".to_string(),
            "read x y; printf '[%s][%s]\\n' \"$x\" \"$y\"".to_string(),
        ],
        stdin_data: b"a\0b c\0d\n".to_vec(),
        expected_out: b"[ab][cd]\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

/// Runs `script` with `input` as standard input, expecting `expected_out`.
fn sh_with_input(script: &str, input: &[u8], expected_out: &str) {
    plib::testing::run_test_u8(plib::testing::TestPlanU8 {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), script.to_string()],
        stdin_data: input.to_vec(),
        expected_out: expected_out.as_bytes().to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

// The NUL goes before anything else looks at the input: with `-r` too, a
// backslash before a NUL escapes the byte after it (dash), a line of NULs
// is an empty line, and a NUL delimiter (`read -d ''`) is still the
// delimiter, as in bash.
#[test]
fn read_drops_nul_bytes_before_backslashes_and_splitting() {
    let show = "printf '[%s]\\n' \"$x\"";
    sh_with_input(&format!("read -r x; {show}"), b"a\0b\\c\n", "[ab\\c]\n");
    sh_with_input(&format!("read x; {show}"), b"a\\\0b\n", "[ab]\n");
    sh_with_input(&format!("read x; {show}"), b"a\0\\\nb c\n", "[ab c]\n");
    sh_with_input(&format!("read x; echo $?; {show}"), b"\0\0\n", "0\n[]\n");
    sh_with_input(&format!("read -d '' x; {show}"), b"a\0b", "[a]\n");
    sh_with_input(&format!("read -d b x; {show}"), b"a\0b", "[a]\n");
    sh_with_input(
        "while read l; do printf '<%s>' \"$l\"; done; echo",
        b"x\0y\nz\n",
        "<xy><z>\n",
    );
}

// NUL bytes in a script are dropped too, from its words, here-documents
// and assignments alike, as in dash (bash drops them after the first line).
#[test]
fn script_drops_nul_bytes() {
    plib::testing::run_test_u8(plib::testing::TestPlanU8 {
        cmd: "sh".to_string(),
        args: Vec::new(),
        stdin_data: b"echo \"a\0b\" c\0d\ncat <<EOF\nh\0i\nEOF\ncat <<'EOF'\nq\0r\nEOF\nx=\"v\0w\"; echo \"$x\" ${#x}\n".to_vec(),
        expected_out: b"ab cd\nhi\nqr\nvw 2\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}
