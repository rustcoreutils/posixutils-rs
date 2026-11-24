//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::common::{run_test, run_test_with_checker};
use plib::testing::TestPlan;
use plib::tmp::TempDir;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Output;

fn setup_sccs_file(tmp: &TempDir, name: &str, content: &str) -> (PathBuf, PathBuf, PathBuf) {
    let sfile = tmp.path().join(format!("s.{}", name));
    let pfile = tmp.path().join(format!("p.{}", name));
    let gfile = tmp.path().join(name);

    // Create SCCS file. admin warns "No id keywords." on stderr when the body
    // has no %X% keyword (these fixtures use plain content), so tolerate stderr.
    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into()],
        stdin_data: String::from(content),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(output.status.success(), "admin create should succeed");
    });

    (sfile, pfile, gfile)
}

// Helper to run get for editing that ignores stderr
fn get_for_editing(sfile: &Path) {
    let plan = TestPlan {
        cmd: String::from("get"),
        args: vec!["-e".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(output.status.success(), "get -e should succeed");
    });
}

// Helper to run get -p -s (silent) which has exact output
fn get_version_silent(sfile: &Path, expected: &str) {
    run_test(TestPlan {
        cmd: String::from("get"),
        args: vec!["-p".into(), "-s".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::from(expected),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

fn get_version_silent_sid(sfile: &Path, sid: &str, expected: &str) {
    run_test(TestPlan {
        cmd: String::from("get"),
        args: vec![
            "-p".into(),
            "-s".into(),
            format!("-r{}", sid),
            sfile.to_string_lossy().into(),
        ],
        stdin_data: String::new(),
        expected_out: String::from(expected),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// Helper to run delta that ignores stderr (has version info)
fn run_delta(sfile: &Path, comment: &str) {
    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec![format!("-y{}", comment), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(output.status.success(), "delta should succeed");
    });
}

#[test]
fn delta_basic_commit() {
    let tmp = TempDir::new().unwrap();
    let (sfile, pfile, gfile) = setup_sccs_file(&tmp, "basic", "original\n");

    // Get for editing
    get_for_editing(&sfile);

    // Modify the g-file
    fs::write(&gfile, "original\nmodified\n").unwrap();

    // Commit the delta
    run_delta(&sfile, "Test commit");

    // Verify g-file is removed (default behavior)
    assert!(!gfile.exists(), "g-file should be removed after delta");

    // Verify p-file is removed
    assert!(!pfile.exists(), "p-file should be removed after delta");

    // Verify new version is accessible
    get_version_silent(&sfile, "original\nmodified\n");
}

#[test]
fn delta_keep_gfile() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "keepg", "content\n");

    // Get for editing
    get_for_editing(&sfile);

    // Modify
    fs::write(&gfile, "content\nmore\n").unwrap();

    // Commit with -n (keep g-file)
    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec![
            "-n".into(),
            "-yKeep file".into(),
            sfile.to_string_lossy().into(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(output.status.success(), "delta -n should succeed");
    });

    // Verify g-file still exists
    assert!(gfile.exists(), "g-file should be kept with -n");
}

#[test]
fn delta_old_version_accessible() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "oldver", "v1\n");

    // Get for editing
    get_for_editing(&sfile);

    // Modify
    fs::write(&gfile, "v2\n").unwrap();

    // Commit
    run_delta(&sfile, "Version 2");

    // Verify old version is still accessible
    get_version_silent_sid(&sfile, "1.1", "v1\n");

    // Verify new version
    get_version_silent_sid(&sfile, "1.2", "v2\n");
}

#[test]
fn delta_error_no_pending_edit() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, _gfile) = setup_sccs_file(&tmp, "noedit", "content\n");

    // Try to commit without pending edit
    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec![sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "delta should fail without p-file");
    });
}

#[test]
fn delta_error_missing_gfile() {
    let tmp = TempDir::new().unwrap();
    let (sfile, pfile, gfile) = setup_sccs_file(&tmp, "nogfile", "content\n");

    // Get for editing
    get_for_editing(&sfile);

    // Remove g-file
    fs::remove_file(&gfile).unwrap();

    // Try to commit - should fail because g-file is missing
    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec![sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "delta should fail without g-file");
    });

    // Clean up - remove p-file manually since delta failed
    fs::remove_file(&pfile).ok();
}

#[test]
fn delta_multiple_commits() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "multi", "line1\n");

    // First edit cycle
    get_for_editing(&sfile);
    fs::write(&gfile, "line1\nline2\n").unwrap();
    run_delta(&sfile, "Added line2");

    // Second edit cycle
    get_for_editing(&sfile);
    fs::write(&gfile, "line1\nline2\nline3\n").unwrap();
    run_delta(&sfile, "Added line3");

    // Verify all versions
    get_version_silent_sid(&sfile, "1.1", "line1\n");
    get_version_silent_sid(&sfile, "1.2", "line1\nline2\n");
    get_version_silent_sid(&sfile, "1.3", "line1\nline2\nline3\n");
}

#[test]
fn delta_comment_in_file() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "comment", "test\n");

    // Get and modify
    get_for_editing(&sfile);
    fs::write(&gfile, "test\nmore\n").unwrap();

    // Commit with comment
    run_delta(&sfile, "My test comment");

    // Verify comment is in the SCCS file
    let content = fs::read_to_string(&sfile).unwrap();
    assert!(
        content.contains("My test comment"),
        "Comment should be stored in SCCS file"
    );
}

// Run admin to set a flag on an existing s-file, ignoring stderr/stdout.
fn admin_set_flag(sfile: &Path, flag: &str) {
    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec![format!("-f{}", flag), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(output.status.success(), "admin -f{} should succeed", flag);
    });
}

#[test]
fn delta_records_mr_with_v_flag() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "mr", "a\n");
    admin_set_flag(&sfile, "v");

    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\n").unwrap();

    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec![
            "-ydelta".into(),
            "-mbug123 bug456".into(),
            sfile.to_string_lossy().into(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(output.status.success(), "delta -m should succeed");
    });

    let content = fs::read_to_string(&sfile).unwrap();
    assert!(content.contains("\x01m bug123"), "should record ^Am bug123");
    assert!(content.contains("\x01m bug456"), "should record ^Am bug456");
}

#[test]
fn delta_v_flag_requires_mr() {
    let tmp = TempDir::new().unwrap();
    let (sfile, pfile, gfile) = setup_sccs_file(&tmp, "needmr", "a\n");
    admin_set_flag(&sfile, "v");

    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\n").unwrap();

    // No -m and empty stdin: must fail without committing.
    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec!["-ydelta".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            !output.status.success(),
            "delta must fail when v flag set and no MR supplied"
        );
    });

    // Edit must NOT have been committed: p-file still present, still SID 1.1.
    assert!(pfile.exists(), "p-file should remain after aborted delta");
    let content = fs::read_to_string(&sfile).unwrap();
    let delta_lines = content.matches("\x01d ").count();
    assert_eq!(delta_lines, 1, "no new delta should be recorded");
}

#[test]
fn delta_comment_from_stdin() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "stdincmt", "a\n");

    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\n").unwrap();

    // No -y: comment is read from (non-tty) stdin.
    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec![sfile.to_string_lossy().into()],
        stdin_data: String::from("stdin comment line\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(output.status.success(), "delta should succeed");
    });

    let content = fs::read_to_string(&sfile).unwrap();
    assert!(
        content.contains("\x01c stdin comment line"),
        "comment should come from stdin"
    );
}

#[test]
fn delta_g_records_ignored() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "ignore", "l1\n");

    // Build up deltas 1.2 (serial 2) and 1.3 (serial 3).
    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\n").unwrap();
    run_delta(&sfile, "c1");
    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\nl3\n").unwrap();
    run_delta(&sfile, "c2");

    // New delta ignoring SID 1.2 (serial 2).
    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\nl3\nl4\n").unwrap();
    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec![
            "-yc3".into(),
            "-g1.2".into(),
            sfile.to_string_lossy().into(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(output.status.success(), "delta -g should succeed");
    });

    let content = fs::read_to_string(&sfile).unwrap();
    assert!(content.contains("\x01g 2"), "should record ^Ag 2");
}

/// `-g list` takes "a list (see get for the definition of list)" (92229), so
/// an element may be an inclusive SID range, not just a single SID.
#[test]
fn delta_g_accepts_a_sid_range() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "grange", "l1\n");

    // Build 1.2 (serial 2) and 1.3 (serial 3).
    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\n").unwrap();
    run_delta(&sfile, "c1");
    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\nl3\n").unwrap();
    run_delta(&sfile, "c2");

    // A range names both of them at once.
    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\nl3\nl4\n").unwrap();
    let out = super::common::run_in(
        "delta",
        &["-yc3", "-g1.2-1.3", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert!(
        out.status.success(),
        "delta -g with a range failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let content = fs::read_to_string(&sfile).unwrap();
    assert!(
        content.contains("\x01g 2 3") || content.contains("\x01g 3 2"),
        "the range must record both serials, got the ignore line: {:?}",
        content
            .lines()
            .find(|l| l.starts_with("\x01g"))
            .unwrap_or("<none>")
    );
}

/// A range whose first SID is not an ancestor of the second is rejected before
/// anything is written: baking a silently wrong ignore-list into the history
/// is not recoverable by re-running delta.
#[test]
fn delta_g_rejects_a_range_that_is_not_an_ancestor_chain() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "gbad", "l1\n");

    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\n").unwrap();
    run_delta(&sfile, "c1");

    let before = fs::read_to_string(&sfile).unwrap();
    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\nl3\n").unwrap();
    let out = super::common::run_in(
        "delta",
        &["-yc2", "-g1.2-1.1", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert_eq!(out.status.code(), Some(1), "a reversed range must fail");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("ancestor"),
        "expected the ancestry diagnostic, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        fs::read_to_string(&sfile).unwrap(),
        before,
        "a rejected -g list must leave the s-file untouched"
    );
}

#[test]
fn delta_p_normal_diff_format() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "pdiff", "l1\nl2\nl3\n");

    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nL2\nl3\n").unwrap();

    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec![
            "-yc".into(),
            "-p".into(),
            "-s".into(),
            sfile.to_string_lossy().into(),
        ],
        stdin_data: String::new(),
        // With -s the only stdout is the -p diff in normal format.
        expected_out: String::from("2c2\n< l2\n---\n> L2\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test(plan);
}

#[test]
fn delta_p_append_does_not_panic() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "pappend", "l1\nl2\n");

    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\nl3\nl4\n").unwrap();

    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec![
            "-yc".into(),
            "-p".into(),
            "-s".into(),
            sfile.to_string_lossy().into(),
        ],
        stdin_data: String::new(),
        expected_out: String::from("2a3,4\n> l3\n> l4\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test(plan);
}

// z-file (per-command lock) is removed after a successful delta, and a
// pre-existing z-file blocks the mutation (#X8 / #D5).
#[test]
fn delta_zfile_removed_after_command() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "zclean", "a\n");

    let zfile = tmp.path().join("z.zclean");

    get_for_editing(&sfile);
    assert!(
        !zfile.exists(),
        "z-file must not persist after get -e returns"
    );

    fs::write(&gfile, "a\nb\n").unwrap();
    run_delta(&sfile, "add");
    assert!(
        !zfile.exists(),
        "z-file must be removed after delta returns"
    );
}

#[test]
fn delta_blocked_by_existing_zfile() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "zlock", "a\n");

    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\n").unwrap();

    // Simulate another command holding the per-command z-file lock.
    let zfile = tmp.path().join("z.zlock");
    fs::write(&zfile, "99999\n").unwrap();

    let plan = TestPlan {
        cmd: String::from("delta"),
        args: vec!["-yblocked".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            !output.status.success(),
            "delta must fail while z-file lock is held"
        );
    });

    // Our manually-created lock must be left intact (delta must not remove a
    // lock it did not create).
    assert!(zfile.exists(), "foreign z-file must not be removed");
}

/// A directory operand expands to the SCCS files inside it, and `-` reads the
/// pathname list from standard input (92258+). The g-file is resolved against
/// the current directory in both cases, matching `get`'s placement rule.
#[test]
fn delta_directory_and_stdin_operands() {
    let tmp = TempDir::new().unwrap();
    std::fs::create_dir(tmp.path().join("proj")).unwrap();
    super::common::run_in("admin", &["-i", "proj/s.mod"], tmp.path(), "body\n");

    // Directory operand.
    super::common::run_in("get", &["-e", "proj/s.mod"], tmp.path(), "");
    fs::write(tmp.path().join("mod"), "body2\n").unwrap();
    let out = super::common::run_in("delta", &["-yviadir", "proj"], tmp.path(), "");
    assert!(
        out.status.success(),
        "delta <dir> failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("1.2"),
        "expected the new SID, got {:?}",
        String::from_utf8_lossy(&out.stdout)
    );

    // '-' operand: the list comes from stdin, so -y is required (92255).
    super::common::run_in("get", &["-e", "proj/s.mod"], tmp.path(), "");
    fs::write(tmp.path().join("mod"), "body3\n").unwrap();
    let out = super::common::run_in("delta", &["-yviastdin", "-"], tmp.path(), "proj/s.mod\n");
    assert!(
        out.status.success(),
        "delta - failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(String::from_utf8_lossy(&out.stdout).contains("1.3"));
}

/// "-r ... shall be necessary only if two or more outstanding get commands for
/// editing on the same SCCS file were done by the same person" (92219-92224).
/// Without it the ambiguity has to be diagnosed rather than guessed at.
#[test]
fn delta_r_selects_among_multiple_pending_edits() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "multi", "l1\n");

    // Joint edit plus branching, so one user can hold two locks at once.
    super::common::run_in(
        "admin",
        &["-fj", "-fb", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );
    super::common::run_in("get", &["-e", sfile.to_str().unwrap()], tmp.path(), "");
    // -g: the first edit's g-file is there, writable, and is not replaced.
    super::common::run_in(
        "get",
        &["-e", "-b", "-g", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );

    let pfile = fs::read_to_string(tmp.path().join("p.multi")).unwrap();
    assert_eq!(
        pfile.lines().count(),
        2,
        "fixture needs two pending edits, got {pfile:?}"
    );

    // Ambiguous without -r.
    fs::write(&gfile, "l1\nEDIT\n").unwrap();
    let out = super::common::run_in(
        "delta",
        &["-yambig", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert_eq!(
        out.status.code(),
        Some(1),
        "two pending edits must be ambiguous"
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("-r"),
        "the diagnostic must name the option that resolves it, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // -r names the branch edit, and that is the SID committed.
    let out = super::common::run_in(
        "delta",
        &["-ypick", "-r1.1.1.1", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert!(
        out.status.success(),
        "delta -r failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("1.1.1.1"),
        "expected the branch SID, got {:?}",
        String::from_utf8_lossy(&out.stdout)
    );

    // The trunk edit is still pending afterwards.
    let pfile = fs::read_to_string(tmp.path().join("p.multi")).unwrap();
    assert_eq!(pfile.lines().count(), 1, "the other edit must survive");
    assert!(pfile.contains("1.2"), "the trunk edit remains: {pfile:?}");
}

/// SIGINT during a delta must leave no transient files behind. delta takes the
/// z-file lock before it reads the comment from standard input, so leaving the
/// pipe open parks it with the lock held and the x-file temporary in play.
///
/// Without the cleanup registry a stale z.<name> survives the interrupt and
/// every later get -e / delta on that file fails with "being edited" until
/// somebody removes it by hand.
#[test]
fn delta_sigint_removes_transient_files() {
    use std::process::{Command, Stdio};

    let tmp = TempDir::new().unwrap();
    let (sfile, _pfile, gfile) = setup_sccs_file(&tmp, "sigint", "l1\n");
    get_for_editing(&sfile);
    fs::write(&gfile, "l1\nl2\n").unwrap();

    // No -y, and stdin is a pipe we never write to or close: delta blocks
    // reading the comment.
    let mut child = Command::new(plib::testing::get_binary_path("delta"))
        .arg(sfile.to_str().unwrap())
        .current_dir(tmp.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn delta");

    // Wait for the lock to appear rather than sleeping a fixed amount.
    let zfile = tmp.path().join("z.sigint");
    let mut waited = 0;
    while !zfile.exists() && waited < 5000 {
        std::thread::sleep(std::time::Duration::from_millis(10));
        waited += 10;
    }
    assert!(
        zfile.exists(),
        "delta should hold the z-file lock while blocked"
    );

    unsafe { libc::kill(child.id() as libc::pid_t, libc::SIGINT) };
    let _ = child.wait().expect("wait for delta");

    assert!(
        !zfile.exists(),
        "SIGINT must remove the z-file lock, not strand it"
    );
    let strays: Vec<String> = fs::read_dir(tmp.path())
        .unwrap()
        .flatten()
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .filter(|n| n.starts_with("x.") || n.starts_with("z."))
        .collect();
    assert!(
        strays.is_empty(),
        "SIGINT left transient files behind: {strays:?}"
    );

    // The s-file must be untouched: the interrupted delta was never recorded.
    let out = super::common::run_in(
        "get",
        &["-p", "-s", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "l1\n");
}

/// `admin -m` split its MR list on whitespace and `delta -m` split on
/// whitespace *and* commas, so the same option-argument produced one MR from
/// one utility and two from the other. CSSC 1.4.1 records one MR named `1,2`
/// in both cases: a comma is an ordinary character inside an MR number, not a
/// separator.
#[test]
fn delta_m_treats_a_comma_as_part_of_the_mr_number() {
    assert_eq!(mrs_of_a_delta_made_with("mrcomma", "-m1,2"), vec!["1,2"]);
}

/// Blanks, by contrast, do separate MR numbers — in both utilities.
#[test]
fn delta_m_splits_the_mr_list_on_blanks() {
    assert_eq!(mrs_of_a_delta_made_with("mrblank", "-m3 4"), vec!["3", "4"]);
}

/// Build a one-delta file with the `v` flag set, commit a second delta passing
/// `mopt`, and report the `:MR:` lines recorded for it.
fn mrs_of_a_delta_made_with(name: &str, mopt: &str) -> Vec<String> {
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join(format!("s.{}", name));
    let sfile_s = sfile.to_string_lossy().to_string();

    // The v flag is what makes MR numbers acceptable at all.
    let out = super::common::run_in(
        "admin",
        &["-i", "-fv", "-minit", &sfile_s],
        tmp.path(),
        "a\n",
    );
    assert!(
        out.status.success(),
        "admin: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = super::common::run_in("get", &["-e", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -e: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    std::fs::write(tmp.path().join(name), "a\nb\n").unwrap();

    let out = super::common::run_in("delta", &[mopt, "-ychanged", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "delta: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = super::common::run_in("prs", &["-d:MR:", "-r1.2", &sfile_s], tmp.path(), "");
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .filter(|l| !l.is_empty())
        .map(str::to_string)
        .collect()
}

/// Deleting two or more trailing lines panicked: the diff's lookahead read
/// `new_lines[j]` in the branch it entered precisely because the new text was
/// exhausted. CSSC records this as 0 inserted / 3 deleted / 1 unchanged.
#[test]
fn delta_deleting_trailing_lines_does_not_panic() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _p, gfile) = setup_sccs_file(&tmp, "trail", "a\nb\nc\nd\n");
    get_for_editing(&sfile);
    fs::write(&gfile, "a\n").unwrap();

    let out = super::common::run_in(
        "delta",
        &["-ytrim", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert_eq!(
        out.status.code(),
        Some(0),
        "delta failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("0 inserted"), "got {stdout:?}");
    assert!(stdout.contains("3 deleted"), "got {stdout:?}");
    assert!(stdout.contains("1 unchanged"), "got {stdout:?}");

    get_version_silent(&sfile, "a\n");
}

/// Deleting the whole body is the same shape, with nothing left to anchor on.
#[test]
fn delta_deleting_every_line_does_not_panic() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _p, gfile) = setup_sccs_file(&tmp, "wipe", "a\nb\nc\n");
    get_for_editing(&sfile);
    fs::write(&gfile, "").unwrap();

    let out = super::common::run_in(
        "delta",
        &["-ywipe", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert_eq!(
        out.status.code(),
        Some(0),
        "delta failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(String::from_utf8_lossy(&out.stdout).contains("3 deleted"));
}

/// The edit script must be minimal. Inserting twelve lines between two
/// unchanged ones was recorded as thirteen insertions and one deletion,
/// because the anchor line sat past the old ten-line lookahead window. CSSC
/// records 12 inserted / 0 deleted / 2 unchanged.
#[test]
fn delta_records_a_minimal_edit_script() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _p, gfile) = setup_sccs_file(&tmp, "wide", "a\nz\n");
    get_for_editing(&sfile);
    fs::write(&gfile, "a\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\nz\n").unwrap();

    let out = super::common::run_in(
        "delta",
        &["-ywide", sfile.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert!(
        out.status.success(),
        "delta failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("12 inserted"), "got {stdout:?}");
    assert!(stdout.contains("0 deleted"), "got {stdout:?}");
    assert!(stdout.contains("2 unchanged"), "got {stdout:?}");
}

/// An encoded (binary) s-file could be created and retrieved but never
/// updated: `delta` read the g-file's raw bytes as UTF-8 lines and diffed them
/// against the uuencoded text held in the s-file, failing with "stream did not
/// contain valid UTF-8". CSSC segfaults on this case, so it is not an oracle
/// here; the invariant under test is the round trip itself.
#[test]
fn delta_round_trips_a_binary_file() {
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.bin");
    let sfile_s = sfile.to_string_lossy().to_string();

    let original: &[u8] = b"A\x00B\xffC\n";
    let edited: &[u8] = b"A\x00B\xffC\nD\x80E\n";
    fs::write(tmp.path().join("seed.bin"), original).unwrap();

    let out = super::common::run_in("admin", &["-iseed.bin", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "admin: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = super::common::run_in("get", &["-e", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -e: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        fs::read(tmp.path().join("bin")).unwrap(),
        original,
        "get -e must write the raw bytes back"
    );

    fs::write(tmp.path().join("bin"), edited).unwrap();
    let out = super::common::run_in("delta", &["-ybin", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "delta on an encoded file: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = super::common::run_in("get", &["-p", "-s", &sfile_s], tmp.path(), "");
    assert_eq!(
        out.stdout, edited,
        "the re-gotten binary must match byte for byte"
    );
}

/// `get -e -x` records the exclusion in the p-file, and `delta` must both diff
/// against the version the user actually edited and record the exclusion on
/// the new delta.
///
/// It did neither: `reconstruct_base` called compute_applied_set on the base
/// serial alone, so the base still contained the excluded delta's line and the
/// diff charged its absence to the user as a deletion; and the new DeltaEntry
/// hardcoded empty included/excluded lists, losing the provenance that POSIX
/// requires prs :Dn:/:Dx: to report. CSSC reports 1 inserted / 0 deleted /
/// 2 unchanged and records ^Ax 2.
#[test]
fn delta_honors_and_records_the_pfile_exclude_list() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _p, gfile) = setup_sccs_file(&tmp, "excl", "a\n");
    let sfile_s = sfile.to_str().unwrap().to_string();

    // 1.2 adds b, 1.3 adds c.
    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\n").unwrap();
    run_delta(&sfile, "two");
    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\nc\n").unwrap();
    run_delta(&sfile, "three");

    // Edit 1.3 with 1.2 forced out: the working file is "a\nc\n".
    let out = super::common::run_in("get", &["-e", "-x1.2", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -e -x1.2: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        fs::read_to_string(&gfile).unwrap(),
        "a\nc\n",
        "the g-file must omit the excluded delta's line"
    );

    // Append one line: exactly one insertion and nothing else.
    fs::write(&gfile, "a\nc\nd\n").unwrap();
    let out = super::common::run_in("delta", &["-yfour", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "delta: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("1 inserted"), "got {stdout:?}");
    assert!(
        stdout.contains("0 deleted"),
        "the exclusion must not be charged to the user as a deletion: {stdout:?}"
    );
    assert!(stdout.contains("2 unchanged"), "got {stdout:?}");

    // The exclusion is recorded on the new delta, by serial.
    let out = super::common::run_in("prs", &["-d:Dx:", "-r1.4", &sfile_s], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "2",
        "the new delta must record the excluded serial"
    );

    // And the retrieved 1.4 is what the user edited.
    get_version_silent(&sfile, "a\nc\nd\n");
}

/// The include side of the same rule: `get -e -i` forces a delta in, and the
/// new delta records it.
#[test]
fn delta_honors_and_records_the_pfile_include_list() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _p, gfile) = setup_sccs_file(&tmp, "incl", "a\n");
    let sfile_s = sfile.to_str().unwrap().to_string();

    // 1.2 adds b, then a branch-free 1.3 removes it again.
    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\n").unwrap();
    run_delta(&sfile, "two");
    get_for_editing(&sfile);
    fs::write(&gfile, "a\n").unwrap();
    run_delta(&sfile, "three");

    // Retrieve 1.1 with 1.2 forced back in.
    let out = super::common::run_in("get", &["-e", "-r1.1", "-i1.2", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -e -i1.2: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        fs::read_to_string(&gfile).unwrap(),
        "a\nb\n",
        "the g-file must contain the forced-in delta's line"
    );

    fs::write(&gfile, "a\nb\nz\n").unwrap();
    let out = super::common::run_in("delta", &["-yfour", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "delta: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("1 inserted") && stdout.contains("0 deleted"),
        "the inclusion must not be charged to the user as an edit: {stdout:?}"
    );

    let sid = stdout.lines().next().unwrap_or("").trim().to_string();
    let out = super::common::run_in(
        "prs",
        &["-d:Dn:", &format!("-r{sid}"), &sfile_s],
        tmp.path(),
        "",
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "2",
        "the new delta must record the included serial"
    );
}

/// The authorized-user list is checked again when the delta is committed, not
/// only when the edit starts.
///
/// The window is real: `admin -a` can narrow the list while an edit is
/// outstanding, and the p-file lock taken earlier is no evidence of present
/// permission. CSSC refuses here too, with the same message.
#[test]
fn delta_refuses_a_user_removed_from_the_authorized_list_mid_edit() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _p, gfile) = setup_sccs_file(&tmp, "revoked", "a\n");
    let sfile_s = sfile.to_str().unwrap().to_string();

    // Start the edit while still permitted...
    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\n").unwrap();

    // ...then hand the file to somebody else.
    let out = super::common::run_in("admin", &["-asomeoneelse", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "admin -a: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = super::common::run_in("delta", &["-ynope", &sfile_s], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "delta must re-check the user list; stdout {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("not authorized"),
        "got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // Nothing was recorded: the trunk head is still 1.1.
    let out = super::common::run_in("prs", &["-d:I:", "-r", &sfile_s], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "1.1");
}

/// An exclusion recorded on a delta must survive into that delta's successors.
///
/// compute_applied_set honored `included` for every delta it walked -- they
/// are pushed onto the traversal stack -- but removed `excluded` only for the
/// target. So 1.4, made with `get -e -x1.2`, retrieved correctly while 1.5,
/// its own successor, resurrected the excluded line. The asymmetry was latent
/// only for as long as delta recorded no exclusions at all.
#[test]
fn delta_exclusion_survives_into_the_next_delta() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _p, gfile) = setup_sccs_file(&tmp, "surv", "a\n");
    let sfile_s = sfile.to_str().unwrap().to_string();

    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\n").unwrap();
    run_delta(&sfile, "two");
    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\nc\n").unwrap();
    run_delta(&sfile, "three");

    // 1.4 excludes 1.2.
    let out = super::common::run_in("get", &["-e", "-x1.2", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -e -x1.2: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    fs::write(&gfile, "a\nc\nd\n").unwrap();
    run_delta(&sfile, "four");

    let out = super::common::run_in("get", &["-p", "-s", "-r1.4", &sfile_s], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "a\nc\nd\n");

    // 1.5 is an ordinary edit on top of 1.4 and must inherit the exclusion.
    get_for_editing(&sfile);
    fs::write(&gfile, "a\nc\nd\ne\n").unwrap();
    run_delta(&sfile, "five");

    let out = super::common::run_in("get", &["-p", "-s", "-r1.5", &sfile_s], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "a\nc\nd\ne\n",
        "the excluded line must not resurrect in a successor delta"
    );
}

/// `get -e` and `delta` must gate on the same release, or an edit that `get`
/// allowed can never be committed.
///
/// get checked the release being retrieved; delta checked the release being
/// created. With release 2 locked, `get -e -r2` succeeded and announced the
/// new delta 2.1, then delta refused forever -- the work stranded behind a
/// p-file lock nothing would clear. CSSC gates the retrieved release.
#[test]
fn delta_commits_an_edit_get_allowed_against_a_locked_target_release() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _p, gfile) = setup_sccs_file(&tmp, "gate", "a\n");
    let sfile_s = sfile.to_str().unwrap().to_string();

    // Lock release 2, which does not exist yet.
    let out = super::common::run_in("admin", &["-fl2", &sfile_s], tmp.path(), "");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-e", "-r2", &sfile_s], tmp.path(), "");
    let allowed = out.status.success();
    fs::write(&gfile, "a\nz\n").unwrap();
    let out = super::common::run_in("delta", &["-ygate", &sfile_s], tmp.path(), "");

    assert_eq!(
        allowed,
        out.status.success(),
        "get -e and delta must agree: get {} the edit, delta {} it ({})",
        if allowed { "allowed" } else { "refused" },
        if out.status.success() {
            "accepted"
        } else {
            "refused"
        },
        String::from_utf8_lossy(&out.stderr)
    );
}

/// `-i` and `-x` naming the same delta must resolve the same way at retrieval
/// and at re-retrieval, or the delta re-reads as content the user never had.
#[test]
fn delta_round_trips_an_overlapping_include_and_exclude() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _p, gfile) = setup_sccs_file(&tmp, "olap", "a\n");
    let sfile_s = sfile.to_str().unwrap().to_string();

    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\n").unwrap();
    run_delta(&sfile, "two");
    get_for_editing(&sfile);
    fs::write(&gfile, "a\nb\nc\n").unwrap();
    run_delta(&sfile, "three");

    let out = super::common::run_in("get", &["-e", "-i1.2", "-x1.2", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -e -i -x: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let handed_out = fs::read_to_string(&gfile).unwrap();
    assert_eq!(handed_out, "a\nb\nc\n", "-i outranks -x per POSIX 99084");

    fs::write(&gfile, format!("{handed_out}z\n")).unwrap();
    run_delta(&sfile, "four");

    let out = super::common::run_in("get", &["-p", "-s", "-r1.4", &sfile_s], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "a\nb\nc\nz\n",
        "re-retrieval must agree with what get -e handed out"
    );
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn option_argument_may_begin_with_hyphen() {
    for opt in ["-r", "-g"] {
        plib::testing::assert_hyphen_option_argument("delta", &[opt, "-zq", "--help"]);
    }
}

/// A delta to a CRLF file changes only the lines that changed: the CR is part
/// of each line on both sides of the diff, so the untouched lines are kept
/// and every line keeps its CR.
#[test]
fn delta_keeps_carriage_returns() {
    let tmp = TempDir::new().unwrap();
    super::common::write_sfile(
        tmp.path(),
        "s.crlf",
        b"\x01s 00002/00000/00000\n\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01e\n\
\x01u\n\x01U\n\x01t\n\x01T\n\x01I 1\na\r\nb\r\n\x01E 1\n",
    );
    let run = |cmd: &str, args: &[&str]| {
        let out = super::common::run_in(cmd, args, tmp.path(), "");
        assert!(out.status.success(), "{cmd} {args:?}: {out:?}");
        out
    };
    run("get", &["-e", "-s", "s.crlf"]);
    fs::write(tmp.path().join("crlf"), b"a\r\nB\r\n").unwrap();
    run("delta", &["-s", "-ychange", "s.crlf"]);
    let out = run("prs", &["-d:Li:/:Ld:/:Lu:", "-r1.2", "s.crlf"]);
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "00001/00001/00001"
    );
    let out = run("get", &["-p", "-k", "-s", "s.crlf"]);
    assert_eq!(out.stdout, b"a\r\nB\r\n");
}

/// A delta to a file whose text is not UTF-8 diffs and records it byte for
/// byte, and `delta -p` prints the changed lines as they are.
#[test]
fn delta_handles_non_utf8_text() {
    let tmp = TempDir::new().unwrap();
    super::common::write_sfile(tmp.path(), "s.l1", super::common::LATIN1_SFILE);
    let run = |cmd: &str, args: &[&str]| {
        let out = super::common::run_in(cmd, args, tmp.path(), "");
        assert!(out.status.success(), "{cmd} {args:?}: {out:?}");
        out
    };
    run("get", &["-e", "-s", "s.l1"]);
    fs::write(tmp.path().join("l1"), b"caf\xe9 %I%\nna\xefve\n").unwrap();
    let out = run("delta", &["-s", "-p", "-ychange", "s.l1"]);
    assert_eq!(out.stdout, b"2c2\n< line2\n---\n> na\xefve\n");
    let out = run("get", &["-p", "-k", "-s", "s.l1"]);
    assert_eq!(out.stdout, b"caf\xe9 %I%\nna\xefve\n");
}

/// delta and rmdel rewrite the whole s-file; text another delta stored that
/// is not UTF-8 -- comments, flag values, descriptive text -- survives both.
#[test]
fn delta_and_rmdel_keep_non_utf8_text() {
    use super::common::{assert_sfile_holds, write_sfile, LATIN1_RECORDS, LATIN1_SFILE};
    let tmp = TempDir::new().unwrap();
    let s = write_sfile(tmp.path(), "s.l1", LATIN1_SFILE);
    let run = |cmd: &str, args: &[&str]| {
        let out = super::common::run_in(cmd, args, tmp.path(), "");
        assert!(out.status.success(), "{cmd} {args:?}: {out:?}");
    };
    run("get", &["-e", "-s", "s.l1"]);
    fs::write(tmp.path().join("l1"), b"caf\xe9 %I%\nline2\nline3\n").unwrap();
    run("delta", &["-s", "-ymore", "s.l1"]);
    assert_sfile_holds(&s, LATIN1_RECORDS);
    run("rmdel", &["-r1.2", "s.l1"]);
    assert_sfile_holds(&s, LATIN1_RECORDS);
}

/// A `-y` comment of several lines is stored as several comment lines.
#[test]
fn delta_multi_line_comment_is_kept() {
    let tmp = TempDir::new().unwrap();
    let (sfile, _, gfile) = setup_sccs_file(&tmp, "ml", "one\n");
    get_for_editing(&sfile);
    fs::write(&gfile, "two\n").unwrap();
    let dir = sfile.parent().unwrap();
    let name = sfile.file_name().unwrap().to_str().unwrap();
    let out = super::common::run_in("delta", &["-s", "-yline1\nline2", name], dir, "");
    assert!(out.status.success(), "{out:?}");
    let out = super::common::run_in("prs", &["-d:C:", "-r1.2", name], dir, "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "line1\nline2\n\n");
    let out = super::common::run_in("val", &[name], dir, "");
    assert_eq!(out.status.code(), Some(0), "{out:?}");
}

/// A text line beginning with SOH cannot be stored in a text s-file: it would
/// read back as a control line, and one such as `^AE 1` would end a block
/// early. delta refuses it and changes nothing, rather than store a file
/// that reads back differently.
#[test]
fn delta_refuses_a_line_beginning_with_soh() {
    let tmp = TempDir::new().unwrap();
    let (sfile, pfile, gfile) = setup_sccs_file(&tmp, "soh", "one\ntwo\n");
    get_for_editing(&sfile);
    let before = fs::read(&sfile).unwrap();
    fs::write(&gfile, b"one\n\x01E 1\ntwo\n").unwrap();
    let dir = sfile.parent().unwrap();
    let out = super::common::run_in("delta", &["-s", "-yc", "s.soh"], dir, "");
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("line 2"),
        "the line should be named: {out:?}"
    );
    assert_eq!(
        fs::read(&sfile).unwrap(),
        before,
        "the s-file must be untouched"
    );
    assert!(
        pfile.exists() && gfile.exists(),
        "the edit is still pending"
    );
}

/// delta and rmdel reweave a body whose blocks overlap without disturbing
/// the versions already in it.
#[test]
fn delta_and_rmdel_keep_overlapping_blocks_intact() {
    let tmp = TempDir::new().unwrap();
    super::common::write_sfile(tmp.path(), "s.o", super::common::OVERLAP_SFILE);
    let run = |cmd: &str, args: &[&str]| {
        let out = super::common::run_in(cmd, args, tmp.path(), "");
        assert!(out.status.success(), "{cmd} {args:?}: {out:?}");
        String::from_utf8_lossy(&out.stdout).into_owned()
    };
    let versions = |with_14: bool| {
        assert_eq!(run("get", &["-p", "-k", "-s", "-r1.1", "s.o"]), "a\nb\n");
        assert_eq!(
            run("get", &["-p", "-k", "-s", "-r1.2", "s.o"]),
            "a\nb\nc\nd\n"
        );
        assert_eq!(run("get", &["-p", "-k", "-s", "-r1.3", "s.o"]), "a\nd\n");
        if with_14 {
            assert_eq!(
                run("get", &["-p", "-k", "-s", "-r1.4", "s.o"]),
                "a\nx\nd\ne\n"
            );
        }
    };

    run("get", &["-e", "-s", "s.o"]);
    fs::write(tmp.path().join("o"), b"a\nx\nd\ne\n").unwrap();
    run("delta", &["-s", "-yfour", "s.o"]);
    versions(true);

    run("rmdel", &["-r1.4", "s.o"]);
    versions(false);
    // And the delta whose blocks overlap can itself be removed.
    run("rmdel", &["-r1.3", "s.o"]);
    assert_eq!(run("get", &["-p", "-k", "-s", "s.o"]), "a\nb\nc\nd\n");
    assert_eq!(run("get", &["-p", "-k", "-s", "-r1.1", "s.o"]), "a\nb\n");
}

/// What a text s-file cannot hold, delta does not store: a file whose last
/// line has no newline, or one holding a NUL. Input must be a text file
/// (POSIX delta 92274), and historical SCCS refuses both ("No newline at end
/// of file (de18)", and de14 for the NUL); storing them anyway gave back a
/// file one byte longer, or an s-file no other implementation can read. The
/// s-file is untouched and the edit stays pending, to be fixed and retried.
#[test]
fn delta_refuses_what_is_not_a_text_file() {
    for (name, content, expect) in [
        ("nonl", &b"one\ntwo"[..], "newline"),
        ("nul", &b"one\nt\x00wo\n"[..], "NUL"),
    ] {
        let tmp = TempDir::new().unwrap();
        let (sfile, pfile, gfile) = setup_sccs_file(&tmp, name, "one\n");
        get_for_editing(&sfile);
        let before = fs::read(&sfile).unwrap();
        fs::write(&gfile, content).unwrap();
        let dir = sfile.parent().unwrap();
        let sname = format!("s.{name}");
        let out = super::common::run_in("delta", &["-s", "-yc", &sname], dir, "");
        assert_eq!(out.status.code(), Some(1), "{name}: {out:?}");
        assert!(
            String::from_utf8_lossy(&out.stderr).contains(expect),
            "{name}: the reason should be given: {out:?}"
        );
        assert_eq!(
            fs::read(&sfile).unwrap(),
            before,
            "{name}: s-file untouched"
        );
        assert!(
            pfile.exists() && gfile.exists(),
            "{name}: the edit is still pending"
        );

        // Fixed, the same edit goes in.
        fs::write(&gfile, b"one\ntwo\n").unwrap();
        let out = super::common::run_in("delta", &["-s", "-yc", &sname], dir, "");
        assert!(out.status.success(), "{name}: {out:?}");
    }
}

/// An encoded s-file holds any bytes: there a missing final newline is kept
/// exactly, through a delta.
#[test]
fn delta_of_an_encoded_file_keeps_a_missing_final_newline() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("in"), b"no newline").unwrap();
    let run = |cmd: &str, args: &[&str]| {
        let out = super::common::run_in(cmd, args, tmp.path(), "");
        assert!(out.status.success(), "{cmd} {args:?}: {out:?}");
        out.stdout
    };
    run("admin", &["-iin", "s.enc"]);
    run("get", &["-e", "-s", "s.enc"]);
    fs::write(tmp.path().join("enc"), b"still\nno newline").unwrap();
    run("delta", &["-s", "-yc", "s.enc"]);
    assert_eq!(run("get", &["-p", "-s", "s.enc"]), b"still\nno newline");
}

/// delta weaves a change the way SCCS and CSSC lay it out: one delete block
/// around each run of deleted lines, opened right before the first of them
/// and closed at the next line the version keeps, and the lines inserted
/// there in a block of their own just before that line. (It used to wrap
/// every deleted line in a block of its own.) The body expected here is the
/// one CSSC writes for the same six steps, byte for byte.
#[test]
fn delta_weaves_runs_as_sccs_does() {
    let tmp = TempDir::new().unwrap();
    let run = |cmd: &str, args: &[&str], stdin: &str| {
        let out = super::common::run_in(cmd, args, tmp.path(), stdin);
        assert!(out.status.success(), "{cmd} {args:?}: {out:?}");
    };
    run("admin", &["-i", "-fb", "s.f"], "a\nb\nc\nd\ne\nf\n");
    let edit = |get_args: &[&str], text: &str, comment: &str| {
        let mut argv = vec!["-e", "-s"];
        argv.extend_from_slice(get_args);
        argv.push("s.f");
        run("get", &argv, "");
        fs::write(tmp.path().join("f"), text).unwrap();
        run("delta", &["-s", &format!("-y{comment}"), "s.f"], "");
    };
    edit(&[], "a\nB1\nB2\nc\nd\ne\nf\n", "replace one line with two");
    edit(&["-r1.1"], "a\nb\nc\nf\n", "delete a run of two");
    edit(&[], "a\nB1\nc\nd\nX\ne\nf\nZ\n", "delete, insert, append");
    edit(
        &["-r1.1.1.1"],
        "f\n",
        "delete a run across other deltas' records",
    );
    edit(
        &[],
        "P\na\nB1\nc\nf\nZ\n",
        "prepend, and delete across an insert",
    );

    let data = fs::read(tmp.path().join("s.f")).unwrap();
    let at = data.windows(3).position(|w| w == b"\x01T\n").unwrap();
    let body = String::from_utf8_lossy(&data[at + 3..]).replace('\x01', "^A");
    let cssc = "^AI 1\n^AD 5\n^AI 6\nP\n^AE 6\na\n^AD 2\nb\n^AE 2\n^AI 2\nB1\n^AD 4\nB2\n\
^AE 2\n^AE 4\nc\n^AD 3\n^AD 6\nd\n^AI 4\nX\n^AE 4\ne\n^AE 3\n^AE 5\n^AE 6\nf\n^AE 1\n\
^AI 4\nZ\n^AE 4\n";
    assert_eq!(body, cssc);

    // Every version still reads as it was checked in.
    for (sid, text) in [
        ("1.1", "a\nb\nc\nd\ne\nf\n"),
        ("1.2", "a\nB1\nB2\nc\nd\ne\nf\n"),
        ("1.1.1.1", "a\nb\nc\nf\n"),
        ("1.3", "a\nB1\nc\nd\nX\ne\nf\nZ\n"),
        ("1.1.1.2", "f\n"),
        ("1.4", "P\na\nB1\nc\nf\nZ\n"),
    ] {
        let out = super::common::run_in(
            "get",
            &["-p", "-k", "-s", &format!("-r{sid}"), "s.f"],
            tmp.path(),
            "",
        );
        assert_eq!(String::from_utf8_lossy(&out.stdout), text, "version {sid}");
    }
}

/// The MR list and the comment read from standard input end at the first
/// newline that is not escaped; "the escape character is <backslash>"
/// (POSIX delta, -m and -y). An escaped newline separates MRs, and in a
/// comment it begins the next line. Reading one line and no more, as this
/// did, kept the backslash and took the rest of the reply for the answer to
/// the next question.
#[test]
fn delta_reads_escaped_newlines_in_its_replies() {
    let tmp = TempDir::new().unwrap();
    let run =
        |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, tmp.path(), stdin);
    assert!(run("admin", &["-i", "-fv", "-mfirst", "s.r"], "one\n")
        .status
        .success());
    assert!(run("get", &["-e", "-s", "s.r"], "").status.success());
    std::fs::write(tmp.path().join("r"), "two\n").unwrap();

    // MRs first, then the comment; each reply may run over several lines.
    let out = run(
        "delta",
        &["-s", "s.r"],
        "mr1 mr2\\\nmr3\nwhy: \\ a backslash\\\nand a second line\\\n\nnot read\n",
    );
    assert!(out.status.success(), "{out:?}");
    let out = run("prs", &["-d:MR:|:C:|", "-r1.2", "s.r"], "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "mr1\nmr2\nmr3\n|why: \\ a backslash\nand a second line\n|\n"
    );

    // A reply cut short by the end of the input is taken as far as it goes.
    assert!(run("get", &["-e", "-s", "s.r"], "").status.success());
    std::fs::write(tmp.path().join("r"), "three\n").unwrap();
    let out = run("delta", &["-s", "-mmr9", "s.r"], "unfinished\\\n");
    assert!(out.status.success(), "{out:?}");
    let out = run("prs", &["-d:C:|", "-r1.3", "s.r"], "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "unfinished\n|\n");
}

/// The comment and the MR list read from standard input are read once, and
/// are those of every file named: delta of several files asks one question.
/// It read standard input afresh for each file, so the first got the comment
/// and the rest got the end of the input, an empty one.
#[test]
fn delta_gives_every_file_the_reply_it_read() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    for name in ["a", "b", "c"] {
        let s = format!("s.{name}");
        assert!(run("admin", &["-i", "-fv", "-mfirst", &s], "one\n")
            .status
            .success());
        assert!(run("get", &["-e", "-s", &s], "").status.success());
        std::fs::write(dir.join(name), "two\n").unwrap();
    }
    let out = run("delta", &["-s", "s.a", "s.b", "s.c"], "mr7\nthe comment\n");
    assert!(out.status.success(), "{out:?}");
    for name in ["a", "b", "c"] {
        let out = run("prs", &["-d:MR:|:C:|", "-r1.2", &format!("s.{name}")], "");
        assert_eq!(
            String::from_utf8_lossy(&out.stdout),
            "mr7\n|the comment\n|\n",
            "s.{name}"
        );
    }
}

/// A delta is made once. Should the p-file still hold an edit whose delta
/// is already in the s-file -- delta stopped between writing the one and
/// the other -- running delta again does not make a second delta with the
/// same SID; it did, and the file then had two live deltas named 1.2.
#[test]
fn delta_does_not_make_a_sid_twice() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "s.d"], "one\n").status.success());
    assert!(run("get", &["-e", "-s", "s.d"], "").status.success());
    let pending = fs::read(dir.join("p.d")).unwrap();
    fs::write(dir.join("d"), "two\n").unwrap();
    assert!(run("delta", &["-s", "-n", "-yc", "s.d"], "")
        .status
        .success());

    // As if the p-file had not been updated.
    fs::write(dir.join("p.d"), &pending).unwrap();
    let out = run("delta", &["-s", "-yagain", "s.d"], "");
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("1.2"),
        "{out:?}"
    );
    let out = run("prs", &["-e", "-d:I:", "s.d"], "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "1.2\n1.1\n");
}

/// With several edits of one delta pending (the `j` flag), the SID they
/// were made from does not say which is meant: `-r` must name the SID to be
/// made. The first such edit was taken, whichever one the g-file was.
#[test]
fn delta_r_naming_several_edits_is_ambiguous() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "-fj", "s.a"], "one\n")
        .status
        .success());
    assert!(run("get", &["-e", "-s", "s.a"], "").status.success());
    assert!(run("get", &["-e", "-s", "-g", "s.a"], "").status.success());
    fs::write(dir.join("a"), "two\n").unwrap();

    let out = run("delta", &["-s", "-yc", "-r1.1", "s.a"], "");
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert_eq!(
        fs::read_to_string(dir.join("p.a")).unwrap().lines().count(),
        2
    );
    // The SID to be made names one edit.
    let out = run("delta", &["-s", "-yc", "-r1.1.1.1", "s.a"], "");
    assert!(out.status.success(), "{out:?}");
    let out = run("prs", &["-e", "-d:I:", "s.a"], "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "1.1.1.1\n1.1\n");
}
