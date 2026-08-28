//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::get_binary_path;
use plib::tmp::TempDir;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

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

/// `sccs diffs` compares the working file against the retrieved version, using
/// a temporary file for the latter.
///
/// That temporary used to be `$TMPDIR/sccs_diff.<pid>.<n>` written with a
/// plain create-and-truncate, so its name was guessable and the write followed
/// a symlink left in its place — in a binary that carries `drop_privileges()`
/// because it may be installed setuid. It is now created with mkstemp, which
/// picks an unpredictable name and refuses to open an existing path. This test
/// covers the functional half: the diff is still right, and nothing is left
/// behind in TMPDIR.
#[test]
fn sccs_diffs_reports_changes_and_leaves_no_temporary() {
    let tmp = TempDir::new().unwrap();
    let tmpdir = TempDir::new().unwrap();
    setup_project(&tmp, "d.txt", "one\ntwo\n");

    let out = run("sccs", &["edit", "d.txt"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs edit failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    std::fs::write(tmp.path().join("d.txt"), "one\ntwo\nthree\n").unwrap();

    let out = run_env(
        "sccs",
        &["diffs", "d.txt"],
        tmp.path(),
        "",
        &[("TMPDIR", tmpdir.path().to_str().unwrap())],
        false,
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("three"),
        "diffs should report the added line, got {stdout:?} / {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    let leftovers: Vec<String> = std::fs::read_dir(tmpdir.path())
        .unwrap()
        .flatten()
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .collect();
    assert!(
        leftovers.is_empty(),
        "sccs diffs left temporaries behind: {leftovers:?}"
    );
}

/// A detached option-argument is not a file operand.
///
/// The front end partitioned argv on a leading '-', so the value token of a
/// separated option fell into the file list and was rewritten through
/// to_sfile: `sccs get -r 1.1 f` reached get as `-r SCCS/s.f SCCS/s.1.1`,
/// which it rejected as an invalid SID. Inventing a bogus operand is a worse
/// failure than diagnosing a missing one, and CSSC mishandles this too.
#[test]
fn sccs_keeps_a_detached_option_argument_out_of_the_file_list() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "r.txt", "one\ntwo\n");

    let out = run("sccs", &["get", "-r", "1.1", "r.txt"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs get -r 1.1 failed: {} / {}",
        String::from_utf8_lossy(&out.stderr),
        String::from_utf8_lossy(&out.stdout)
    );
    assert_eq!(
        std::fs::read_to_string(tmp.path().join("r.txt")).unwrap(),
        "one\ntwo\n"
    );
    assert!(
        !String::from_utf8_lossy(&out.stderr).contains("s.1.1"),
        "the option-argument must not become an operand: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// The same shape with a comment that looks nothing like a filename.
#[test]
fn sccs_keeps_a_detached_comment_out_of_the_file_list() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "c.txt", "one\n");

    let out = run("sccs", &["edit", "c.txt"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs edit: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    std::fs::write(tmp.path().join("c.txt"), "one\ntwo\n").unwrap();

    let out = run(
        "sccs",
        &["delta", "-y", "some comment", "c.txt"],
        tmp.path(),
        "",
    );
    assert!(
        out.status.success(),
        "sccs delta -y <comment> failed: {} / {}",
        String::from_utf8_lossy(&out.stderr),
        String::from_utf8_lossy(&out.stdout)
    );
    assert!(
        !String::from_utf8_lossy(&out.stderr).contains("some comment"),
        "the comment must not become an operand: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // The comment reached delta: prs reports it.
    let out = run("prs", &["-d:C:", "-r1.2", "SCCS/s.c.txt"], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "some comment");
}

/// With no SCCS/ directory, an s-file beside the working file is the s-file.
///
/// to_sfile always inserted SCCS/, where plib's sfile_from_gfile falls back to
/// a sibling, so the front end could not address a flat directory that the
/// utilities themselves handle.
#[test]
fn sccs_finds_a_sibling_sfile_when_there_is_no_sccs_directory() {
    let tmp = TempDir::new().unwrap();
    let out = run("admin", &["-i", "s.flat"], tmp.path(), "flat\n");
    assert!(
        out.status.success(),
        "admin: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = run("sccs", &["get", "flat"], tmp.path(), "");
    assert!(
        out.status.success(),
        "sccs get on a flat directory failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        std::fs::read_to_string(tmp.path().join("flat")).unwrap(),
        "flat\n"
    );
}

/// `sccs info` must report a p-file line that carries no time field.
///
/// get_editing_info hand-parsed p-files and required at least five
/// whitespace-separated fields, where PfileEntry::parse requires four and
/// defaults the time. A line every other utility accepts was silently dropped
/// here, so `info`, `tell`, `check` and `clean` disagreed with `sact`.
#[test]
fn sccs_info_reports_a_pfile_line_without_a_time_field() {
    let tmp = TempDir::new().unwrap();
    setup_project(&tmp, "t.txt", "body\n");

    // What a p-file written by an older SCCS looks like: no time.
    std::fs::write(
        tmp.path().join("SCCS/p.t.txt"),
        format!("1.1 1.2 {} 26/08/28\n", plib::sccsfile::real_login_name()),
    )
    .unwrap();

    let out = run("sccs", &["info"], tmp.path(), "");
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("t.txt") && stdout.contains("1.1 1.2"),
        "info dropped a p-file line that sact accepts: {stdout:?}"
    );
}

/// Under `-d`, `sccs diffs` must compare the same working file that `get`
/// wrote and `delta` will read.
///
/// get places the g-file as a bare name in the current directory; the front
/// end computed `root_dir.join(file)` instead, so with -d it diffed a file at
/// a path no other utility uses -- and reported no differences no matter what
/// the user had edited.
#[test]
fn sccs_diffs_under_d_compares_the_file_get_actually_wrote() {
    let tmp = TempDir::new().unwrap();
    let project = tmp.path().join("project");
    std::fs::create_dir_all(project.join("SCCS")).unwrap();

    let out = run("admin", &["-i", "SCCS/s.d.txt"], &project, "one\ntwo\n");
    assert!(
        out.status.success(),
        "admin: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // Work from a sibling directory, addressing the project through -d.
    let work = tmp.path().join("work");
    std::fs::create_dir_all(&work).unwrap();
    let root = project.to_str().unwrap().to_string();

    let out = run("sccs", &["-d", &root, "edit", "d.txt"], &work, "");
    assert!(
        out.status.success(),
        "sccs -d edit: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // get wrote the g-file into the current directory, not under -d.
    let gfile = work.join("d.txt");
    assert!(
        gfile.exists(),
        "get -e must write the g-file in the current directory"
    );
    std::fs::write(&gfile, "one\ntwo\nthree\n").unwrap();

    let out = run("sccs", &["-d", &root, "diffs", "d.txt"], &work, "");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("three"),
        "diffs must compare the g-file get wrote, got {stdout:?} / {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
}
