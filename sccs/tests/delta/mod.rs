//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Output;
use tempfile::TempDir;

fn setup_sccs_file(tmp: &TempDir, name: &str, content: &str) -> (PathBuf, PathBuf, PathBuf) {
    let sfile = tmp.path().join(format!("s.{}", name));
    let pfile = tmp.path().join(format!("p.{}", name));
    let gfile = tmp.path().join(name);

    // Create SCCS file
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into()],
        stdin_data: String::from(content),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
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
