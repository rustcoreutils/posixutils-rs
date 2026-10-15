//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! What autoconf, automake and libtoolize need from m4.  Every expected
//! output below was produced by GNU m4 1.4.19 from the same input.

use plib::testing::{run_test, run_test_base, TestPlan};

/// Run m4 with `args` on `stdin` and require exactly this stdout, stderr and
/// exit status.
fn m4(args: &[&str], stdin: &str, stdout: &str, stderr: &str, status: i32) {
    run_test(TestPlan {
        cmd: String::from("m4"),
        args: args.iter().map(|a| a.to_string()).collect(),
        stdin_data: String::from(stdin),
        expected_out: String::from(stdout),
        expected_err: String::from(stderr),
        expected_exit_code: status,
    });
}

/// Run m4 and hand back stdout and stderr, for checks that cannot spell out
/// the whole of either.
fn m4_output(args: &[&str], stdin: &str) -> (String, String, Option<i32>) {
    let args: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    let out = run_test_base("m4", &args, stdin.as_bytes());
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

// ---------------------------------------------------------------------------
// POSIX: dumpdef and defn
// ---------------------------------------------------------------------------

// POSIX: "dumpdef ... or, if no arguments are specified, for all macros".  A
// bare `dumpdef` used to be left as text because the built-in demanded an
// argument list.  autom4te learns the names of the built-ins this way.
#[test]
fn bare_dumpdef_dumps_every_macro_sorted() {
    let (out, err, status) = m4_output(&[], "define(`foo',`bar')dumpdef\n");
    assert_eq!(out, "\n");
    assert_eq!(status, Some(0));
    let lines: Vec<&str> = err.lines().collect();
    assert!(lines.contains(&"foo:\tbar"), "{err}");
    assert!(lines.contains(&"define:\t<define>"), "{err}");
    assert!(lines.contains(&"dumpdef:\t<dumpdef>"), "{err}");
    let mut sorted = lines.clone();
    sorted.sort();
    assert_eq!(lines, sorted, "dumpdef output is sorted by name");
}

#[test]
fn dumpdef_sorts_named_macros_and_reports_undefined_ones() {
    m4(
        &[],
        "define(`b',`B')define(`a',`A')dumpdef(`b',`nosuch',`a')\n",
        "\n",
        "m4:stdin:1: undefined macro `nosuch'\na:\tA\nb:\tB\n",
        0,
    );
}

// POSIX: defn yields "the quoted definition ... of its arguments": all of
// them, not just the first.
#[test]
fn defn_concatenates_every_named_definition() {
    m4(
        &[],
        "define(`a',`A')define(`b',`B')[defn(`a',`zz',`b')]\n",
        "[AB]\n",
        "",
        0,
    );
}

#[test]
fn defn_of_a_builtin_among_several_names_is_dropped_with_a_warning() {
    m4(
        &[],
        "define(`a',`A')define(`d',defn(`define'))define(`f',defn(`d',`a'))dumpdef(`f')\n",
        "\n",
        "m4:stdin:1: Warning: cannot concatenate builtin `d'\nf:\tA\n",
        0,
    );
}

// A built-in copied under a new name stays that built-in: copying the copy
// works, and dumpdef names the built-in, not the alias.
#[test]
fn copied_builtin_keeps_its_identity() {
    m4(
        &[],
        "define(`d',defn(`define'))d(`e',defn(`d'))dumpdef(`d',`e')e(`x',`X')x\n",
        "X\n",
        "d:\t<define>\ne:\t<define>\n",
        0,
    );
}
