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
use std::path::PathBuf;
use std::process::Output;
use tempfile::TempDir;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(name)
}

// Helper to run get tests that check stdout only, allowing any stderr
fn run_get_test_stdout_only(args: Vec<String>, expected_out: &str) {
    let plan = TestPlan {
        cmd: String::from("get"),
        args,
        stdin_data: String::new(),
        expected_out: String::from(expected_out),
        expected_err: String::new(), // ignored by checker
        expected_exit_code: 0,
    };

    run_test_with_checker(plan, |plan: &TestPlan, output: &Output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(stdout, plan.expected_out, "stdout mismatch");
        assert!(output.status.success(), "command should succeed");
    });
}

#[test]
fn get_simple_to_stdout() {
    // Test -p (print to stdout)
    let fixture = fixture_path("s.simple");
    run_get_test_stdout_only(
        vec!["-p".into(), fixture.to_string_lossy().into()],
        "line1\nline2\nline3\n",
    );
}

#[test]
fn get_multi_latest_version() {
    // Test getting latest version of multi-delta file
    let fixture = fixture_path("s.multi");
    run_get_test_stdout_only(
        vec!["-p".into(), fixture.to_string_lossy().into()],
        "line1\nmodified-line2\nline3\nline4\n",
    );
}

#[test]
fn get_specific_version() {
    // Test -r (specific SID)
    let fixture = fixture_path("s.multi");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-r1.1".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\n",
    );
}

#[test]
fn get_version_1_2() {
    // Test getting version 1.2
    let fixture = fixture_path("s.multi");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-r1.2".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\nline4\n",
    );
}

#[test]
fn get_keywords_expanded() {
    // Test keyword expansion
    let fixture = fixture_path("s.keywords");
    run_get_test_stdout_only(
        vec!["-p".into(), fixture.to_string_lossy().into()],
        "@(#)keywords\t1.1\n1.1\nkeywords\n@(#)\n",
    );
}

#[test]
fn get_keywords_suppressed() {
    // Test -k (suppress keyword expansion)
    let fixture = fixture_path("s.keywords");
    run_get_test_stdout_only(
        vec!["-p".into(), "-k".into(), fixture.to_string_lossy().into()],
        "%W%\n%I%\n%M%\n%Z%\n",
    );
}

#[test]
fn get_branched_trunk() {
    // Test getting trunk version from branched file
    let fixture = fixture_path("s.branched");
    run_get_test_stdout_only(
        vec!["-p".into(), fixture.to_string_lossy().into()],
        "line1\nline2\nline3\ntrunk-1.2\n",
    );
}

#[test]
fn get_branched_version() {
    // Test getting branch version
    let fixture = fixture_path("s.branched");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-r1.1.1.1".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\nbranch-1.1.1.1\n",
    );
}

#[test]
fn get_for_editing() {
    // Test -e (get for editing) - creates p-file
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.editme");
    let pfile = tmp.path().join("p.editme");
    let gfile = tmp.path().join("editme");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into()],
        stdin_data: String::from("content\n"),
        expected_out: String::new(),
        // admin warns when the body has no %X% id keyword.
        expected_err: format!("admin: warning: {}: No id keywords.\n", sfile.display()),
        expected_exit_code: 0,
    });

    // Get for editing - use checker to verify g-file and p-file exist
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

    // Verify g-file was created
    assert!(gfile.exists(), "g-file should be created");

    // Verify p-file was created
    assert!(pfile.exists(), "p-file should be created");
}

#[test]
fn get_silent_mode() {
    // Test -s (silent mode) - no version info in stderr
    let fixture = fixture_path("s.simple");
    run_test(TestPlan {
        cmd: String::from("get"),
        args: vec!["-p".into(), "-s".into(), fixture.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::from("line1\nline2\nline3\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn get_error_file_not_found() {
    // Test error when file doesn't exist
    let plan = TestPlan {
        cmd: String::from("get"),
        args: vec!["-p".into(), "s.nonexistent".into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            !output.status.success(),
            "get on nonexistent file should fail"
        );
    });
}

#[test]
fn get_version_1_1_from_branched() {
    // Test getting base version from branched file
    let fixture = fixture_path("s.branched");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-r1.1".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\n",
    );
}

#[test]
fn get_exclude_top_delta() {
    // -x1.3 excludes the most recent delta; the result matches version 1.2.
    let fixture = fixture_path("s.multi");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-x1.3".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\nline4\n",
    );
}

#[test]
fn get_include_excluded_notation() {
    // -x writes an "Excluded:" notation to the info stream (stderr under -p).
    let fixture = fixture_path("s.multi");
    let plan = TestPlan {
        cmd: String::from("get"),
        args: vec![
            "-p".into(),
            "-x1.3".into(),
            fixture.to_string_lossy().into(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("Excluded:\n1.3"),
            "stderr should contain Excluded notation, got: {stderr}"
        );
    });
}

#[test]
fn get_cutoff_excludes_newer_deltas() {
    // -c cutoff between delta 1.2 (22:24:57) and 1.3 (22:25:05) drops 1.3.
    let fixture = fixture_path("s.multi");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-c251212222500".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\nline4\n",
    );
}

#[test]
fn get_cutoff_invalid_field_rejected() {
    // An out-of-range cutoff field (month 13) must be rejected as an error
    // rather than silently filtering nonsensically.
    let fixture = fixture_path("s.multi");
    let plan = TestPlan {
        cmd: String::from("get"),
        args: vec![
            "-p".into(),
            "-c251312".into(),
            fixture.to_string_lossy().into(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            !output.status.success(),
            "invalid -c cutoff should fail, exit was {:?}",
            output.status.code()
        );
        let err = String::from_utf8_lossy(&output.stderr);
        assert!(
            err.contains("Invalid cutoff date"),
            "expected 'Invalid cutoff date' diagnostic, got: {err}"
        );
    });
}

#[test]
fn get_lfile_to_stdout() {
    // -L writes the delta summary table to standard output.
    let fixture = fixture_path("s.multi");
    let plan = TestPlan {
        cmd: String::from("get"),
        args: vec!["-L".into(), fixture.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Status line: three status columns, a space, the SID, a tab, then
        // date-time and login.
        assert!(
            stdout.contains("    1.3\t25/12/12 22:25:05 jgarzik\n"),
            "l-file summary line mismatch, got: {stdout}"
        );
        assert!(
            stdout.contains("\tmodified line2\n"),
            "l-file comment line missing, got: {stdout}"
        );
    });
}

#[test]
fn get_top_delta_in_release() {
    // -t accesses the most recently created delta in release 1 (1.3).
    let fixture = fixture_path("s.multi");
    let plan = TestPlan {
        cmd: String::from("get"),
        args: vec![
            "-p".into(),
            "-t".into(),
            "-r1".into(),
            fixture.to_string_lossy().into(),
        ],
        stdin_data: String::new(),
        expected_out: String::from("line1\nmodified-line2\nline3\nline4\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |plan: &TestPlan, output: &Output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(stdout, plan.expected_out, "stdout mismatch");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("1.3"),
            "top SID should be 1.3, got: {stderr}"
        );
    });
}

/// The g-file "shall be created in the current directory" (spec 99186-99187),
/// and "only the real user need have write permission in the current
/// directory" (99190).
///
/// `get` used to derive the g-file's directory from the s-file — the parent of
/// `SCCS/`, else the s-file's own directory. For the usual
/// `cd project && get SCCS/s.foo` that gives the same answer, which is why it
/// went unnoticed; but retrieving an s-file that lives elsewhere wrote the
/// working copy into *that* tree instead of here. Verified against CSSC 1.4.1,
/// which writes to the current directory.
#[test]
fn get_writes_the_gfile_into_the_current_directory() {
    let project = TempDir::new().unwrap();
    let work = TempDir::new().unwrap();

    std::fs::create_dir(project.path().join("SCCS")).unwrap();
    let sfile = project.path().join("SCCS/s.remote");
    let out = super::common::run_in(
        "admin",
        &["-i", sfile.to_str().unwrap()],
        project.path(),
        "remote content\n",
    );
    assert!(
        out.status.success(),
        "admin failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // Retrieve it from an unrelated directory.
    let out = super::common::run_in("get", &[sfile.to_str().unwrap()], work.path(), "");
    assert!(
        out.status.success(),
        "get failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(
        work.path().join("remote").exists(),
        "the g-file belongs in the current directory"
    );
    assert!(
        !project.path().join("remote").exists(),
        "the g-file must not be written beside the SCCS directory"
    );
    assert_eq!(
        std::fs::read(work.path().join("remote")).unwrap(),
        b"remote content\n"
    );
}

/// The l-file follows the same rule: "the l-file shall be created in the
/// current directory if the -l option is used" (99192-99194).
#[test]
fn get_writes_the_lfile_into_the_current_directory() {
    let project = TempDir::new().unwrap();
    let work = TempDir::new().unwrap();

    std::fs::create_dir(project.path().join("SCCS")).unwrap();
    let sfile = project.path().join("SCCS/s.withl");
    let out = super::common::run_in(
        "admin",
        &["-i", sfile.to_str().unwrap()],
        project.path(),
        "body\n",
    );
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-l", sfile.to_str().unwrap()], work.path(), "");
    assert!(
        out.status.success(),
        "get -l failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(
        work.path().join("l.withl").exists(),
        "the l-file belongs in the current directory"
    );
    assert!(
        !project.path().join("SCCS/l.withl").exists() && !project.path().join("l.withl").exists(),
        "the l-file must not be written beside the s-file"
    );
}

/// The p-file, by contrast, is "created in the directory containing the SCCS
/// file" (99214) — so the two rules really are different, and fixing the
/// g-file must not have moved the p-file too.
#[test]
fn get_writes_the_pfile_beside_the_sfile() {
    let project = TempDir::new().unwrap();
    let work = TempDir::new().unwrap();

    std::fs::create_dir(project.path().join("SCCS")).unwrap();
    let sfile = project.path().join("SCCS/s.locked");
    let out = super::common::run_in(
        "admin",
        &["-i", sfile.to_str().unwrap()],
        project.path(),
        "body\n",
    );
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-e", sfile.to_str().unwrap()], work.path(), "");
    assert!(
        out.status.success(),
        "get -e failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(
        project.path().join("SCCS/p.locked").exists(),
        "the p-file belongs beside the s-file"
    );
    assert!(
        !work.path().join("p.locked").exists(),
        "the p-file must not follow the g-file into the current directory"
    );
    assert!(
        work.path().join("locked").exists(),
        "the g-file still belongs in the current directory"
    );
}
