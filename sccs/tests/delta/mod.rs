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
    super::common::run_in(
        "get",
        &["-e", "-b", sfile.to_str().unwrap()],
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
