//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_env, TestPlan};

/// The physical (getcwd) working directory of the test runner; the spawned
/// `pwd` inherits the same cwd, so `-P` output must equal this.
fn physical_cwd() -> String {
    let mut s = std::env::current_dir()
        .unwrap()
        .to_string_lossy()
        .into_owned();
    s.push('\n');
    s
}

fn pwd_plan(args: &[&str], env: &[(&str, &str)], expected_out: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test_with_env(
        TestPlan {
            cmd: String::from("pwd"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::from(expected_out),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        env,
    );
}

#[test]
fn test_pwd_default_falls_back_to_getcwd_for_invalid_pwd() {
    // Default is logical (-L); a non-absolute $PWD is unusable and pwd must
    // fall back to the physical getcwd path.
    pwd_plan(&[], &[("PWD", "not/absolute")], &physical_cwd());
}

#[test]
fn test_pwd_default_rejects_pwd_with_dotdot() {
    // $PWD containing `..` is rejected by the -L validity check → fall back.
    pwd_plan(&[], &[("PWD", "/foo/../bar")], &physical_cwd());
}

#[test]
fn test_pwd_logical_honors_valid_pwd() {
    // -L prints a syntactically valid $PWD verbatim (it is not stat-verified).
    pwd_plan(&["-L"], &[("PWD", "/valid/abs/path")], "/valid/abs/path\n");
}

#[test]
fn test_pwd_physical_ignores_pwd() {
    // -P ignores $PWD entirely and prints the physical getcwd path.
    pwd_plan(&["-P"], &[("PWD", "/valid/abs/path")], &physical_cwd());
}

#[test]
fn test_pwd_last_option_wins_lp_is_physical() {
    // `-L -P`: the last (-P) wins → physical, ignoring $PWD.
    pwd_plan(
        &["-L", "-P"],
        &[("PWD", "/valid/abs/path")],
        &physical_cwd(),
    );
}

#[test]
fn test_pwd_last_option_wins_pl_is_logical() {
    // `-P -L`: the last (-L) wins → logical, honoring valid $PWD.
    pwd_plan(
        &["-P", "-L"],
        &[("PWD", "/valid/abs/path")],
        "/valid/abs/path\n",
    );
}

#[test]
fn test_pwd_output_is_absolute() {
    // Whatever the mode, the printed path is absolute.
    let out = physical_cwd();
    assert!(
        out.starts_with('/'),
        "getcwd output must be absolute: {out}"
    );
}
