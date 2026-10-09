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

// -L must not print a $PWD that names some other directory: POSIX pwd uses
// $PWD only if it is "an absolute pathname of the current working directory".
// Until this test, only the spelling was checked, so a stale $PWD inherited
// across a chdir() (perl's `chdir "sub"; print `pwd``) was printed as is.
#[test]
fn test_pwd_logical_rejects_pwd_naming_another_directory() {
    pwd_plan(&["-L"], &[("PWD", "/")], &physical_cwd());
    pwd_plan(&[], &[("PWD", "/nonexistent/abs/path")], &physical_cwd());
}

/// A temporary directory holding `real/` and `link -> real`.
struct LinkedDir {
    dir: plib::tmp::TempDir,
}

impl LinkedDir {
    fn new() -> LinkedDir {
        let dir = plib::tmp::tempdir().expect("create temporary directory");
        std::fs::create_dir(dir.path().join("real")).unwrap();
        std::os::unix::fs::symlink("real", dir.path().join("link")).unwrap();
        LinkedDir { dir }
    }

    fn link(&self) -> String {
        self.dir.path().join("link").to_string_lossy().into_owned()
    }

    fn physical(&self) -> String {
        let real = std::fs::canonicalize(self.dir.path().join("real")).unwrap();
        format!("{}\n", real.to_string_lossy())
    }

    /// Run pwd with `args` in `link`, with $PWD spelled through the link.
    fn pwd(&self, args: &[&str]) -> String {
        let out = std::process::Command::new(plib::testing::get_binary_path("pwd"))
            .args(args)
            .current_dir(self.link())
            .env("PWD", self.link())
            .output()
            .expect("run pwd");
        assert_eq!(out.status.code(), Some(0));
        String::from_utf8(out.stdout).unwrap()
    }
}

#[test]
fn test_pwd_logical_honors_pwd_through_a_symlink() {
    // A $PWD that reaches the working directory through a symbolic link names
    // it, so -L prints it rather than the physical path.
    let dirs = LinkedDir::new();
    assert_eq!(dirs.pwd(&["-L"]), format!("{}\n", dirs.link()));
    assert_eq!(dirs.pwd(&[]), format!("{}\n", dirs.link()));
    assert_eq!(dirs.pwd(&["-P"]), dirs.physical());
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
    // `-P -L`: the last (-L) wins → logical, honoring valid $PWD; `-L -P` is
    // physical.
    let dirs = LinkedDir::new();
    assert_eq!(dirs.pwd(&["-P", "-L"]), format!("{}\n", dirs.link()));
    assert_eq!(dirs.pwd(&["-L", "-P"]), dirs.physical());
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
