//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `for` loop: its word list is expanded once, before the first
//! iteration, into one list of fields (XCU 2.9.4.2).

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

// `continue` goes on with the next field, also when that field came from
// the same word. libtoolize's `for file in $pkgaux_files` stopped after its
// first `continue`, so ltmain.sh was never installed.
#[test]
fn continue_moves_to_next_field_of_same_word() {
    sh_c(
        "L='a b c'; for f in $L; do test $f = a && continue; echo $f; done; echo end",
        "b\nc\nend\n",
    );
    sh_c(
        "set -- a b c; for f in \"$@\"; do test $f = a && continue; echo $f; done",
        "b\nc\n",
    );
    sh_c(
        "L='a b'; for f in $L; do case $f in a) continue;; esac; echo $f; done",
        "b\n",
    );
    sh_c("for f in *; do continue; done; echo end", "end\n");
}

#[test]
fn continue_n_and_break_with_expanded_list() {
    sh_c(
        "L='a b'; for o in 1 2; do for f in $L; do test $f = a && continue 2; echo $f; done; echo no; done; echo end",
        "end\n",
    );
    sh_c(
        "L='a b c'; for f in $L; do test $f = b && break; echo $f; done; echo end",
        "a\nend\n",
    );
}

// The list is expanded before the body runs, so the body cannot change the
// words still to come.
#[test]
fn list_is_expanded_before_the_first_iteration() {
    sh_c("x=1; for i in a $x; do x=9; echo $i; done", "a\n1\n");
}

// `for name` with no `in` list is `for name in "$@"`: each positional
// parameter is one field, unsplit. autoconf parses its options with
// `for ac_option`, so binutils' `--with-pkgversion="GNU Binutils for Debian"`
// became four arguments and "Binutils" the target.
#[test]
fn for_without_in_iterates_positional_parameters_unsplit() {
    sh_c(
        "set -- 'a b' c; for x; do echo \"[$x]\"; done",
        "[a b]\n[c]\n",
    );
    sh_c(
        "set -- 'a b' c; for x\ndo echo \"[$x]\"; done",
        "[a b]\n[c]\n",
    );
    sh_c(
        "f() { for x; do echo \"[$x]\"; done; }; f '*' ''",
        "[*]\n[]\n",
    );
    sh_c("set --; for x; do echo y; done; echo end", "end\n");
}

// `do` may follow the name directly, with no `;` or newline (XCU 2.9.4.2
// grammar: `For name linebreak do_group`); it is still the reserved word.
#[test]
fn for_name_do_without_separator() {
    sh_c(
        "set -- 'a b' c; for x do echo \"[$x]\"; done",
        "[a b]\n[c]\n",
    );
    sh_c("for do in a; do echo $do; done", "a\n");
}
