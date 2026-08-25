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
use std::process::Output;

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

fn get_for_editing(sfile: &std::path::Path) {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("get"),
            args: vec!["-e".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "get -e should succeed");
        },
    );
}

#[test]
fn sact_no_pending_edit() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // No pending edit - should produce no output
    run_test(TestPlan {
        cmd: String::from("sact"),
        args: vec![sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn sact_with_pending_edit() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // Get for editing to create p-file
    get_for_editing(&sfile);

    // sact should show the pending edit
    run_test_with_checker(
        TestPlan {
            cmd: String::from("sact"),
            args: vec![sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "sact should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("1.1"), "should show old SID");
            assert!(stdout.contains("1.2"), "should show new SID");
        },
    );
}

#[test]
fn sact_corrupt_pfile_nonzero_exit() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // Write a corrupt p-file (alongside the s-file as p.<name>).
    let pfile = tmp.path().join("p.test");
    fs::write(&pfile, "this is not a valid p-file line\n").unwrap();

    // A corrupt p-file must surface as a non-zero exit status.
    run_test_with_checker(
        TestPlan {
            cmd: String::from("sact"),
            args: vec![sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(
                !output.status.success(),
                "corrupt p-file should yield non-zero exit"
            );
        },
    );
}

fn sact_run(args: &[&str], cwd: &std::path::Path) -> Output {
    std::process::Command::new(plib::testing::get_binary_path("sact"))
        .args(args)
        .current_dir(cwd)
        .output()
        .expect("run sact")
}

/// Multiple operands are each prefixed with the pathname header
/// `"\n%s:\n"` (spec 113793-113794). The existing tests use a single operand,
/// so the header form was never exercised, and they assert with `contains`
/// rather than pinning the exact bytes.
#[test]
fn sact_multi_file_uses_the_pathname_header() {
    let tmp = TempDir::new().unwrap();
    let a = create_sccs_file(&tmp, "alpha", "a\n");
    let b = create_sccs_file(&tmp, "beta", "b\n");
    get_for_editing(&a);
    get_for_editing(&b);

    let out = sact_run(&[a.to_str().unwrap(), b.to_str().unwrap()], tmp.path());
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);

    // Exact shape: a leading newline, "<path>:", then one activity line each.
    let want_a = format!("\n{}:\n", a.display());
    let want_b = format!("\n{}:\n", b.display());
    assert!(stdout.starts_with(&want_a), "got {stdout:?}");
    assert!(stdout.contains(&want_b), "got {stdout:?}");

    // Each activity line is "<old-sid> <new-sid> <user> <date> <time>"
    // (113787-113792): five blank-separated fields.
    for line in stdout
        .lines()
        .filter(|l| !l.is_empty() && !l.ends_with(':'))
    {
        let fields: Vec<&str> = line.split_whitespace().collect();
        assert_eq!(fields.len(), 5, "activity line shape: {line:?}");
        assert_eq!(fields[0], "1.1", "old SID");
        assert_eq!(fields[1], "1.2", "new SID");
    }
}

/// A single operand gets no header, so the header really is conditional rather
/// than unconditional.
#[test]
fn sact_single_file_has_no_header() {
    let tmp = TempDir::new().unwrap();
    let a = create_sccs_file(&tmp, "solo", "a\n");
    get_for_editing(&a);

    let out = sact_run(&[a.to_str().unwrap()], tmp.path());
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    // The header is "\n<path>:\n"; the activity line itself contains colons in
    // the time field, so match the header shape rather than any colon.
    assert!(
        !stdout.contains(&format!("{}:", a.display())),
        "a lone operand must not print a pathname header, got {stdout:?}"
    );
    assert!(
        !stdout.starts_with('\n'),
        "no leading blank line without a header, got {stdout:?}"
    );
}

/// A directory operand expands to the SCCS files it contains, and (being more
/// than one file) uses the header form.
#[test]
fn sact_directory_operand_expands() {
    let tmp = TempDir::new().unwrap();
    std::fs::create_dir_all(tmp.path().join("SCCS")).unwrap();

    // create_sccs_file puts s-files at the top level; make a directory of them.
    let dir = tmp.path().join("proj");
    std::fs::create_dir(&dir).unwrap();
    for name in ["one", "two"] {
        let src = create_sccs_file(&tmp, name, "x\n");
        get_for_editing(&src);
        std::fs::rename(&src, dir.join(format!("s.{name}"))).unwrap();
        let p = src.with_file_name(format!("p.{name}"));
        if p.exists() {
            std::fs::rename(&p, dir.join(format!("p.{name}"))).unwrap();
        }
    }

    let out = sact_run(&[dir.to_str().unwrap()], tmp.path());
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("s.one:"), "got {stdout:?}");
    assert!(stdout.contains("s.two:"), "got {stdout:?}");
}

/// #S1: EXIT STATUS is "0 Successful completion / >0 An error occurred"
/// (113803-113805), and STDERR carries diagnostics (113796-113797).
///
/// A named operand that does not exist used to be silently ignored with exit
/// 0, making `sact s.typo` byte-for-byte identical to `sact` on a real file
/// with nothing checked out. The distinction is the whole point of the exit
/// status here.
#[test]
fn sact_missing_operand_is_an_error() {
    let tmp = TempDir::new().unwrap();

    let out = sact_run(&["s.does-not-exist"], tmp.path());
    assert_eq!(out.status.code(), Some(1), "a missing operand is an error");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("s.does-not-exist"),
        "the operand must be named in the diagnostic: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // An operand that is not an SCCS file at all is likewise an error.
    let plain = tmp.path().join("plain.txt");
    std::fs::write(&plain, b"not sccs\n").unwrap();
    let out = sact_run(&[plain.to_str().unwrap()], tmp.path());
    assert_eq!(out.status.code(), Some(1));
    assert!(!out.stderr.is_empty(), "a diagnostic is required");
}

/// The counterpart: a real file with no pending edit stays silent and exits 0.
/// Without this, the test above could be satisfied by an implementation that
/// simply errors on everything.
#[test]
fn sact_no_pending_edit_is_silent_success() {
    let tmp = TempDir::new().unwrap();
    let a = create_sccs_file(&tmp, "quiet", "a\n");

    let out = sact_run(&[a.to_str().unwrap()], tmp.path());
    assert_eq!(out.status.code(), Some(0));
    assert!(out.stdout.is_empty(), "no impending delta means no output");
}
