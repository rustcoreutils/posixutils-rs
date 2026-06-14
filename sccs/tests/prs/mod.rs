//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::process::Output;
use tempfile::TempDir;

fn create_sccs_file(tmp: &TempDir, name: &str, content: &str) -> std::path::PathBuf {
    let sfile = tmp.path().join(format!("s.{}", name));

    run_test_with_checker(
        TestPlan {
            cmd: String::from("admin"),
            args: vec![sfile.to_string_lossy().into(), "-i".into()],
            stdin_data: String::from(content),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "admin should succeed");
        },
    );

    sfile
}

#[test]
fn prs_default_output() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec![sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should contain filename, delta info, MRs, COMMENTS
            assert!(stdout.contains("s.test"), "should show filename");
            assert!(stdout.contains("D 1.1"), "should show delta type and SID");
            assert!(stdout.contains("MRs:"), "should show MRs label");
            assert!(stdout.contains("COMMENTS:"), "should show COMMENTS label");
        },
    );
}

#[test]
fn prs_dataspec() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-d:I:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::from("1.1"),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.trim() == "1.1", "should show SID");
        },
    );
}

#[test]
fn prs_delta_type() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-d:DT:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::from("D"),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.trim() == "D", "delta type should be D");
        },
    );
}

#[test]
fn prs_line_counts() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "line1\nline2\nline3\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-d:Li:/:Ld:/:Lu:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            // First delta has 3 lines inserted, 0 deleted, 0 unchanged
            assert!(
                stdout.contains("3/0/0") || stdout.contains("00003/00000/00000"),
                "should show line counts"
            );
        },
    );
}

#[test]
fn prs_specific_sid() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec![
                "-r1.1".into(),
                "-d:I:".into(),
                sfile.to_string_lossy().into(),
            ],
            stdin_data: String::new(),
            expected_out: String::from("1.1"),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
        },
    );
}

/// Build a multi-delta s-file: initial admin -i, then one get -e + delta.
fn create_multi_delta_file(tmp: &TempDir) -> std::path::PathBuf {
    let sfile = tmp.path().join("s.multi");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("admin"),
            args: vec![
                sfile.to_string_lossy().into(),
                "-i".into(),
                "-yinitial".into(),
            ],
            stdin_data: String::from("a\nb\nc\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_p: &TestPlan, o: &Output| assert!(o.status.success()),
    );

    let gfile = tmp.path().join("multi");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("get"),
            args: vec!["-e".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_p: &TestPlan, o: &Output| assert!(o.status.success()),
    );
    std::fs::write(&gfile, "a\nb\nc\nd\n").unwrap();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("delta"),
            args: vec!["-ysecond".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_p: &TestPlan, o: &Output| assert!(o.status.success()),
    );

    sfile
}

/// #P1: each delta's :I: must be on its own line; output ends with a newline.
#[test]
fn prs_trailing_newline_per_delta() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_multi_delta_file(&tmp);

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec![
                "-e".into(),
                "-r".into(),
                "-d:I:".into(),
                sfile.to_string_lossy().into(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_p: &TestPlan, o: &Output| {
            assert!(o.status.success());
            let stdout = String::from_utf8_lossy(&o.stdout);
            assert_eq!(stdout, "1.2\n1.1\n", "each SID newline-terminated");
        },
    );
}

/// #P2: bare -r selects the most recent delta (trunk head).
#[test]
fn prs_r_optional_argument() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_multi_delta_file(&tmp);

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-r".into(), "-d:I:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_p: &TestPlan, o: &Output| {
            assert!(o.status.success());
            let stdout = String::from_utf8_lossy(&o.stdout);
            assert_eq!(stdout, "1.2\n", "bare -r selects trunk head");
        },
    );
}

/// #P4: :GB: reconstructs the gotten body of the latest delta.
#[test]
fn prs_gotten_body() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_multi_delta_file(&tmp);

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-d:GB:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_p: &TestPlan, o: &Output| {
            assert!(o.status.success());
            let stdout = String::from_utf8_lossy(&o.stdout);
            assert_eq!(stdout, "a\nb\nc\nd\n\n", "latest body, M-format");
        },
    );
}

/// #P7: "::" prints verbatim; only recognized keywords are substituted.
#[test]
fn prs_literal_double_colon() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-da::b".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_p: &TestPlan, o: &Output| {
            assert!(o.status.success());
            let stdout = String::from_utf8_lossy(&o.stdout);
            assert_eq!(stdout, "a::b\n", "double colon is literal");
        },
    );
}

/// #P6: unset boundary/locked keywords print "none".
#[test]
fn prs_unset_keywords_none() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    for kw in ["LK", "FB", "CB", "Ds"] {
        run_test_with_checker(
            TestPlan {
                cmd: String::from("prs"),
                args: vec![format!("-d:{}:", kw), sfile.to_string_lossy().into()],
                stdin_data: String::new(),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_p: &TestPlan, o: &Output| {
                assert!(o.status.success());
                let stdout = String::from_utf8_lossy(&o.stdout);
                assert_eq!(stdout, "none\n", "unset keyword prints none");
            },
        );
    }
}

#[test]
fn prs_filename() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-d:F:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("s.test"), "should show filename");
        },
    );
}
