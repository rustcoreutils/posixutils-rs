//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `export` special built-in, and its `-n` extension.

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

// `export -n NAME` takes the export attribute away and keeps the value, as
// in bash, ksh and busybox ash. Not POSIX: binutils' top-level Makefile
// runs `export -n ELF_PACKAGE_METADATA` under /bin/sh before its tests.
#[test]
fn export_n_removes_export_attribute() {
    sh_c(
        "X=1; export X; export -n X; echo \"[$X]\"; sh -c 'echo \"[${X-unset}]\"'",
        "[1]\n[unset]\n",
    );
    sh_c(
        "export -n W=5; echo \"[$W]\"; sh -c 'echo \"[${W-unset}]\"'",
        "[5]\n[unset]\n",
    );
    sh_c("export -n NEVER_SET_X; echo $?", "0\n");
    sh_c(
        "X=1; export X; export -n X; export -p | grep -c ' X=' || :",
        "0\n",
    );
}

// Each expectation below was checked against bash.
#[test]
fn export_n_edge_cases() {
    // The attribute stays off when the variable is assigned again later,
    // also for a name exported before it had a value.
    sh_c(
        "X=1; export X; export -n X; X=3; sh -c 'echo \"[${X-u}]\"'",
        "[u]\n",
    );
    sh_c(
        "export N; export -n N; N=1; sh -c 'echo \"[${N-u}]\"'",
        "[u]\n",
    );
    // A readonly variable may lose the attribute; its value is kept.
    sh_c(
        "readonly R=1; export R; export -n R; echo \"$? [$R]\"; sh -c 'echo \"[${R-u}]\"'",
        "0 [1]\n[u]\n",
    );
    // Assigning with -n, even under set -a, leaves the variable unexported.
    sh_c("set -a; export -n W=5; sh -c 'echo \"[${W-u}]\"'", "[u]\n");
    // Several operands, `--`, and a repeated letter.
    sh_c(
        "X=1; export X; export -n X Y=2 Z; echo $X $Y; sh -c 'echo \"[${X-u}${Y-u}]\"'",
        "1 2\n[uu]\n",
    );
    sh_c("export -n -- X=3; echo $X", "3\n");
    sh_c(
        "X=1; export X; export -nn X; sh -c 'echo \"[${X-u}]\"'",
        "[u]\n",
    );
    // With an operand, -p beside -n is ignored; without one, -n beside -p
    // is ignored and the exported variables are listed.
    sh_c(
        "X=1; export X; export -p -n X; export -p | grep -c ' X=' || :",
        "0\n",
    );
    sh_c("export X=1; export -pn | grep -c ' X='", "1\n");
    sh_c("export X=1; export -n -p | grep -c ' X='", "1\n");
    sh_c("export X=1; export -n | grep -c ' X='", "1\n");
}

#[test]
fn export_n_rejects_invalid_names() {
    for script in ["export -n 1x", "export -n 1x=3"] {
        run_test(TestPlan {
            cmd: "sh".to_string(),
            args: vec!["-c".to_string(), script.to_string()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: "export: '1x' is not a valid name\n".to_string(),
            expected_exit_code: 1,
        });
    }
    // A readonly variable still cannot be assigned through -n.
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), "readonly R=1; export -n R=2".to_string()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: "sh: cannot set readonly variable R\n".to_string(),
        expected_exit_code: 1,
    });
}
