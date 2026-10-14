//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `set` special built-in: options, and the positional parameters it
//! replaces only when given operands.

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

// Options alone leave the positional parameters as they are (XCU `set`:
// they are replaced by the arguments, and only when arguments are given).
// autoconf's `as_fn_exit () { set +e; as_fn_set_status $1; exit $1; }`
// lost its $1 to this, and every failing configure exited 0.
#[test]
fn options_alone_keep_positional_parameters() {
    sh_c("set -- a b; set +e; echo $#", "2\n");
    sh_c("set -- a b; set -e +e -f +f; echo $# $1 $2", "2 a b\n");
    sh_c("set -- a b; set -o errexit +o errexit; echo $#", "2\n");
    sh_c("f() { set +e; echo \"$#:$1\"; }; f a b", "2:a\n");
    sh_c("f() { set +e; exit $1; }; (f 77); echo $?", "77\n");
}

#[test]
fn exit_status_survives_set_in_function() {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec![
            "-c".to_string(),
            "as_fn_exit () { set +e; exit $1; }; as_fn_exit 77".to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 77,
    });
}

#[test]
fn double_dash_and_operands_still_replace() {
    sh_c("set -- a b; set --; echo $#", "0\n");
    sh_c("set -- a b; set -f c; echo $# $1", "1 c\n");
    sh_c("set -- a b; set -- -x; echo $# $1", "1 -x\n");
}

// `set -v` writes the shell's input to stderr as it is read, from the
// point where it is turned on, comments included (libzstd's cli-tests
// compare this output). Both a script file and standard input.
#[test]
fn verbose_echoes_input_as_read() {
    let script = "set -v\necho one\n# c\nset +v\necho two\n";
    let path = concat!(env!("CARGO_TARGET_TMPDIR"), "/set_verbose.sh");
    std::fs::write(path, script).unwrap();
    for (args, stdin) in [(vec![path.to_string()], ""), (vec![], script)] {
        run_test(TestPlan {
            cmd: "sh".to_string(),
            args,
            stdin_data: stdin.to_string(),
            expected_out: "one\ntwo\n".to_string(),
            expected_err: "echo one\n# c\nset +v\n".to_string(),
            expected_exit_code: 0,
        });
    }
}

// Each command's text, here-document bodies and multi-line compound
// commands included, is echoed whole; the commands of a command
// substitution are not input read by the shell and are not echoed again.
#[test]
fn verbose_echoes_whole_commands() {
    let script = "set -v\nx=$(echo s)\ncat <<E\n$x\nE\nif true; then\n  :\nfi\n# end\n";
    let path = concat!(env!("CARGO_TARGET_TMPDIR"), "/set_verbose_whole.sh");
    std::fs::write(path, script).unwrap();
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec![path.to_string()],
        stdin_data: String::new(),
        expected_out: "s\n".to_string(),
        expected_err: script["set -v\n".len()..].to_string(),
        expected_exit_code: 0,
    });
}
