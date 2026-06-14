//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::get_binary_path;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use tempfile::TempDir;

/// Run a built binary with the given args, cwd, env, and stdin.
fn run_env(
    cmd: &str,
    args: &[&str],
    cwd: &Path,
    stdin: &str,
    envs: &[(&str, &str)],
    clear_env: bool,
) -> std::process::Output {
    let bin = get_binary_path(cmd);
    let mut c = Command::new(bin);
    c.args(args)
        .current_dir(cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    if clear_env {
        c.env_clear();
    }
    for (k, v) in envs {
        c.env(k, v);
    }
    let mut child = c.spawn().expect("spawn");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();
    child.wait_with_output().expect("wait")
}

fn run(cmd: &str, args: &[&str], cwd: &Path, stdin: &str) -> std::process::Output {
    run_env(cmd, args, cwd, stdin, &[], false)
}

/// Lay out a project tree under `tmp`: an SCCS/ dir with an s-file created
/// from `content`, plus the g-file removed so `get -e` can run cleanly.
fn setup_project(tmp: &TempDir, name: &str, content: &str) {
    std::fs::create_dir_all(tmp.path().join("SCCS")).unwrap();
    let sname = format!("SCCS/s.{}", name);
    let out = run("admin", &["-i", &sname], tmp.path(), content);
    assert!(
        out.status.success(),
        "admin failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn sccs_info_reports_full_pfile_fields() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "g.txt", "hello\nworld\n");

    // Check out for editing to create a p-file.
    let out = run("sccs", &["edit", "g.txt"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs edit failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // `sccs info` must print old-SID, new-SID, user, date and time.
    let out = run("sccs", &["info"], tmp.path(), "");
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    // Expected form: "g.txt: being edited: 1.1 1.2 <user> <date> <time>"
    assert!(
        stdout.contains("g.txt: being edited:"),
        "info missing prefix: {stdout}"
    );
    assert!(
        stdout.contains("1.1 1.2"),
        "info missing old+new SID: {stdout}"
    );
    // A date/time field separated by whitespace must be present: count tokens
    // after the colon prefix.
    let detail = stdout.split("being edited:").nth(1).unwrap_or("").trim();
    let tokens: Vec<&str> = detail.split_whitespace().collect();
    assert!(
        tokens.len() >= 5,
        "info should have >=5 detail fields (old new user date time), got {:?}",
        tokens
    );
}

#[test]
fn sccs_edit_resolves_siblings_without_path() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "h.txt", "one\ntwo\n");

    // Run `sccs edit` with a cleared environment and a PATH that does NOT
    // contain the SCCS siblings.  The front-end must still find `get` next
    // to its own executable.
    let out = run_env(
        "sccs",
        &["edit", "h.txt"],
        tmp.path(),
        "",
        &[("PATH", "/nonexistent-dir")],
        true,
    );
    assert!(
        out.status.success(),
        "sccs edit must work without siblings on PATH: stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );
    // p-file and g-file should now exist.
    assert!(
        tmp.path().join("SCCS/p.h.txt").exists(),
        "p-file should be created"
    );
    assert!(
        tmp.path().join("h.txt").exists(),
        "g-file should be created"
    );
}

#[test]
fn sccs_unknown_command_fails() {
    let tmp = TempDir::new().unwrap();
    let out = run("sccs", &["boguscmd"], tmp.path(), "");
    assert!(!out.status.success(), "unknown sccs subcommand should fail");
}
