//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared helpers for the SCCS integration tests.
//!
//! The SCCS utilities are working-directory sensitive by specification: `get`
//! creates the g-file and l-file "in the current directory" (get, 99186-99194),
//! while the p-, z- and x-files live beside the s-file (99214, 99228).
//!
//! `plib::testing::run_test` cannot set a working directory — `TestPlan` has no
//! field for one — so a test using it has its working files created wherever
//! the test binary happens to be running. That was masked while `get` derived
//! the g-file's directory from the s-file; once the placement was corrected to
//! match the spec, a run of the suite scattered g-files across the `sccs/`
//! source directory.
//!
//! These wrappers therefore spawn the utility directly, deriving the working
//! directory from the plan's own s-file operand: the directory holding it, or
//! its parent when that directory is `SCCS/`. That is exactly the directory a
//! user would be standing in.

#![allow(dead_code)]

use plib::testing::TestPlan;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};

/// Run a built SCCS utility with an explicit working directory.
pub fn run_in(cmd: &str, args: &[&str], cwd: &Path, stdin: &str) -> Output {
    let mut child = Command::new(plib::testing::get_binary_path(cmd))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("spawn {cmd}: {e}"));
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();
    child.wait_with_output().expect("wait")
}

/// Run a utility in `cwd` and require success, reporting stderr on failure.
pub fn run_ok(cmd: &str, args: &[&str], cwd: &Path, stdin: &str) -> Output {
    let out = run_in(cmd, args, cwd, stdin);
    assert!(
        out.status.success(),
        "{cmd} {args:?} failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    out
}

/// The working directory implied by a plan's operands: the directory holding
/// the first `s.`-prefixed argument, or its parent when that is `SCCS/`.
fn cwd_for(plan: &TestPlan) -> PathBuf {
    for a in &plan.args {
        let p = Path::new(a);
        let is_sfile = p
            .file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.starts_with("s."))
            .unwrap_or(false);
        if !is_sfile {
            continue;
        }
        let dir = match p.parent() {
            Some(d) if !d.as_os_str().is_empty() => d,
            _ => continue,
        };
        if dir.file_name().map(|n| n == "SCCS").unwrap_or(false) {
            return dir.parent().unwrap_or(Path::new(".")).to_path_buf();
        }
        return dir.to_path_buf();
    }
    std::env::current_dir().expect("cwd")
}

fn run_plan(plan: &TestPlan) -> Output {
    let args: Vec<&str> = plan.args.iter().map(String::as_str).collect();
    run_in(&plan.cmd, &args, &cwd_for(plan), &plan.stdin_data)
}

/// Drop-in replacement for `plib::testing::run_test` that runs in the
/// directory implied by the plan's s-file operand.
pub fn run_test(plan: TestPlan) {
    let out = run_plan(&plan);

    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        plan.expected_out,
        "stdout mismatch for {} {:?}",
        plan.cmd,
        plan.args
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        plan.expected_err,
        "stderr mismatch for {} {:?}",
        plan.cmd,
        plan.args
    );
    assert_eq!(
        out.status.code(),
        Some(plan.expected_exit_code),
        "exit code mismatch for {} {:?}",
        plan.cmd,
        plan.args
    );
}

/// Drop-in replacement for `plib::testing::run_test_with_checker`.
pub fn run_test_with_checker<F>(plan: TestPlan, mut checker: F)
where
    F: FnMut(&TestPlan, &Output),
{
    let out = run_plan(&plan);
    checker(&plan, &out);
}
