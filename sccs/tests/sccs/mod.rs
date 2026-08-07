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

/// The driver resolves `SCCS/s.<name>` from a bare module name, and `-p`
/// overrides the subdirectory while `-d` supplies a root prefix
/// (spec 113889-113899).
#[test]
fn sccs_resolves_prefix_and_subdirectory_options() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "mod1", "content\n");

    // Bare module name, resolved through the default SCCS/ subdirectory.
    let out = run("sccs", &["get", "mod1"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs get <module> failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(tmp.path().join("mod1").exists(), "g-file should be created");
    std::fs::remove_file(tmp.path().join("mod1")).unwrap();

    // -d supplies the root; run from elsewhere so a relative path cannot work.
    let elsewhere = TempDir::new().unwrap();
    let out = run(
        "sccs",
        &["-d", tmp.path().to_str().unwrap(), "get", "mod1"],
        elsewhere.path(),
        "",
    );
    assert!(
        out.status.success(),
        "sccs -d <dir> get failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // -p names the subdirectory holding the s-files. A fresh working directory
    // is used because `get` writes a read-only g-file, so reusing `elsewhere`
    // would fail on the overwrite rather than on the resolution being tested.
    let elsewhere2 = TempDir::new().unwrap();
    let out = run(
        "sccs",
        &[
            "-d",
            tmp.path().to_str().unwrap(),
            "-p",
            "SCCS",
            "get",
            "mod1",
        ],
        elsewhere2.path(),
        "",
    );
    assert!(
        out.status.success(),
        "sccs -p failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// #SC4: options belonging to the sub-utility are passed through rather than
/// being consumed by the driver — `sccs get -e` must reach `get` as `-e`, so a
/// p-file appears.
#[test]
fn sccs_passes_subcommand_options_through() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "opt", "content\n");

    let out = run("sccs", &["get", "-e", "opt"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs get -e failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        tmp.path().join("SCCS/p.opt").exists(),
        "-e must reach get, creating a p-file"
    );
}

/// `edit` is "Equivalent to get -e" (113937) and `unedit` is its opposite
/// (113949-113951).
#[test]
fn sccs_edit_and_unedit_pseudo_commands() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "ed", "content\n");

    let out = run("sccs", &["edit", "ed"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs edit failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        tmp.path().join("SCCS/p.ed").exists(),
        "edit must be equivalent to get -e"
    );

    let out = run("sccs", &["unedit", "ed"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs unedit failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !tmp.path().join("SCCS/p.ed").exists(),
        "unedit must cancel the edit"
    );
}

/// `create` makes an SCCS file from a like-named file and then renames the
/// original "by prefixing the basenames with a comma" (113921-113924).
#[test]
fn sccs_create_renames_the_original_with_a_comma() {
    let tmp = TempDir::new().unwrap();
    std::fs::create_dir_all(tmp.path().join("SCCS")).unwrap();
    std::fs::write(tmp.path().join("newmod"), b"fresh content\n").unwrap();

    let out = run("sccs", &["create", "newmod"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs create failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(
        tmp.path().join("SCCS/s.newmod").exists(),
        "the s-file should be created"
    );
    assert!(
        tmp.path().join(",newmod").exists(),
        "the original must be renamed with a leading comma"
    );
    assert!(
        !tmp.path().join("newmod").exists(),
        "the original name must no longer hold the pre-create file"
    );
}

/// `tell` writes "a <newline>-separated list of the files being edited"
/// (113947-113948); `info` lists them verbosely (113942); `clean` removes what
/// can be recreated but leaves files being edited alone (113917-113920).
#[test]
fn sccs_tell_info_and_clean_pseudo_commands() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "t1", "a\n");
    setup_project(&tmp, "t2", "b\n");

    // Nothing checked out yet.
    let out = run("sccs", &["tell"], tmp.path(), "");
    assert!(out.status.success());
    assert!(
        String::from_utf8_lossy(&out.stdout).trim().is_empty(),
        "tell must be empty when nothing is being edited"
    );

    let out = run("sccs", &["edit", "t1"], tmp.path(), "");
    assert!(out.status.success());

    let out = run("sccs", &["tell"], tmp.path(), "");
    assert!(out.status.success());
    let told = String::from_utf8_lossy(&out.stdout);
    assert!(told.contains("t1"), "tell should list t1: {told:?}");
    assert!(!told.contains("t2"), "t2 is not being edited: {told:?}");

    let out = run("sccs", &["info"], tmp.path(), "");
    assert!(out.status.success());
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("t1"),
        "info should mention the edited file"
    );

    // clean must not remove a file that is being edited.
    let out = run("sccs", &["clean"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs clean failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        tmp.path().join("t1").exists(),
        "clean must leave files being edited in place"
    );
}

/// `print` is "equivalent to sccs prs" (113946).
#[test]
fn sccs_print_pseudo_command() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "pr", "a\n");

    let out = run("sccs", &["print", "pr"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs print failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("s.pr"),
        "print should name the s-file: {stdout:?}"
    );
}

/// `delget` performs a delta and then a fresh, non-editable get
/// (113925-113928).
#[test]
fn sccs_delget_pseudo_command() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "dg", "v1\n");

    let out = run("sccs", &["edit", "dg"], tmp.path(), "");
    assert!(out.status.success());
    std::fs::write(tmp.path().join("dg"), b"v2\n").unwrap();

    // SCCS option-arguments are *attached*: `-ymsg`, not `-y msg`. With the
    // separated form the driver (and CSSC 1.4.1, identically) takes `msg` as a
    // file operand and looks for SCCS/s.msg.
    let out = run("sccs", &["delget", "-ymsg", "dg"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs delget failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // The edit is committed (no p-file) and a fresh g-file is present.
    assert!(
        !tmp.path().join("SCCS/p.dg").exists(),
        "delget must complete the delta"
    );
    assert_eq!(
        std::fs::read(tmp.path().join("dg")).unwrap(),
        b"v2\n",
        "delget must leave the new version checked out"
    );
}

/// The pseudo-command set is the one POSIX lists (113901-113951). `enter` is a
/// historical BSD extension and is *not* in it, so rejecting it is correct;
/// this pins that the rejection is a clean diagnostic rather than a panic.
#[test]
fn sccs_rejects_a_non_posix_pseudo_command() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "x", "a\n");

    let out = run("sccs", &["enter", "x"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "unknown command must exit 1");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("enter"),
        "the rejected name should be echoed: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
}
