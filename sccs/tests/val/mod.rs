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

#[test]
fn val_valid_file() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "valid", "test content\n");

    run_test(TestPlan {
        cmd: String::from("val"),
        args: vec![sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn val_invalid_sid() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // SID 2.1 doesn't exist
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec!["-r2.1".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0, // Non-zero, but we check below
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(
                !output.status.success(),
                "val should fail for non-existent SID"
            );
        },
    );
}

#[test]
fn val_valid_sid() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // SID 1.1 exists
    run_test(TestPlan {
        cmd: String::from("val"),
        args: vec!["-r1.1".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn val_not_sccs_file() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("notsccs");
    fs::write(&file, "not an sccs file\n").unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec![file.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(
                !output.status.success(),
                "val should fail for non-SCCS file"
            );
        },
    );
}

#[test]
fn val_no_file_argument() {
    // Missing file argument must yield exit bit 0x80 (128), not clap's exit 2.
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec![],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert_eq!(output.status.code(), Some(0x80), "missing file => 0x80");
        },
    );
}

#[test]
fn val_unknown_keyletter() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // An unknown keyletter must yield exit bit 0x40 (64), not clap's exit 2.
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec!["-Q".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert_eq!(output.status.code(), Some(0x40), "unknown option => 0x40");
        },
    );
}

#[test]
fn val_duplicate_keyletter() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // A duplicated keyletter must yield exit bit 0x40 (64).
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec![
                "-r1.1".into(),
                "-r1.1".into(),
                sfile.to_string_lossy().into(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert_eq!(output.status.code(), Some(0x40), "duplicate option => 0x40");
        },
    );
}

#[test]
fn val_stdin_discrepancy_format() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "x", "content\n");
    let line = format!("-y nomatch {}", sfile.to_string_lossy());

    // POSIX stdin format: "%s\n\n %s: %s\n", <input>, <pathname>, <string>
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec!["-".into()],
            stdin_data: format!("{}\n", line),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let expected = format!("{}\n\n {}: %Y%, -y mismatch\n", line, sfile.display());
            assert_eq!(stdout, expected, "stdin discrepancy format");
            assert_eq!(output.status.code(), Some(0x02), "-y mismatch => 0x02");
        },
    );
}

#[test]
fn val_silent_mode() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("notsccs");
    fs::write(&file, "not an sccs file\n").unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec!["-s".into(), file.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(!output.status.success(), "val should fail");
            // Silent mode - no output
            assert!(output.stdout.is_empty(), "no stdout in silent mode");
        },
    );
}

fn val_run_stdin(args: &[&str], cwd: &std::path::Path, stdin: &str) -> Output {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut child = Command::new(plib::testing::get_binary_path("val"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn val");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();
    child.wait_with_output().expect("wait val")
}

fn val_run(args: &[&str], cwd: &std::path::Path) -> Output {
    val_run_stdin(args, cwd, "")
}

/// Damage a byte of the delta body so the stored checksum no longer matches.
fn corrupt_body(sfile: &std::path::Path, needle: &[u8]) {
    let mut perms = fs::metadata(sfile).unwrap().permissions();
    let orig = perms.clone();
    #[allow(clippy::permissions_set_readonly_false)]
    perms.set_readonly(false);
    fs::set_permissions(sfile, perms).unwrap();

    let mut data = fs::read(sfile).unwrap();
    let at = data
        .windows(needle.len())
        .position(|w| w == needle)
        .expect("needle must appear in the s-file body");
    for b in data[at..at + needle.len()].iter_mut() {
        *b = b'X';
    }
    fs::write(sfile, &data).unwrap();
    fs::set_permissions(sfile, orig).unwrap();
}

/// The exit status is "a bit string where set bits are interpreted as follows"
/// (spec 120325-120335). 0x20 (corrupted SCCS file) and 0x10 (cannot open file
/// or file not SCCS) are distinct conditions and must not be conflated.
#[test]
fn val_distinguishes_corrupted_from_unopenable() {
    let tmp = TempDir::new().unwrap();

    // 0x20: a well-formed s-file whose body no longer matches its checksum.
    let corrupt = create_sccs_file(&tmp, "corrupt", "content zzz\n");
    corrupt_body(&corrupt, b"content");
    let out = val_run(&[corrupt.to_str().unwrap()], tmp.path());
    assert_eq!(
        out.status.code(),
        Some(0x20),
        "a checksum mismatch is 0x20, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // 0x10: a file that exists but is not an SCCS file.
    let plain = tmp.path().join("plain.txt");
    fs::write(&plain, b"just text\n").unwrap();
    let out = val_run(&[plain.to_str().unwrap()], tmp.path());
    assert_eq!(out.status.code(), Some(0x10), "not-an-SCCS-file is 0x10");

    // 0x10 also covers a file that cannot be opened at all.
    let out = val_run(&["s.does-not-exist"], tmp.path());
    assert_eq!(out.status.code(), Some(0x10), "unopenable is also 0x10");
}

/// A missing file operand sets 0x80 (120328), which is why APPLICATION USAGE
/// warns that `$?` cannot distinguish it from a signal.
#[test]
fn val_missing_file_argument_sets_high_bit() {
    let tmp = TempDir::new().unwrap();
    let out = val_run(&[], tmp.path());
    assert_eq!(out.status.code(), Some(0x80));
}

/// "an aggregate code shall be returned: a logical OR of the codes generated
/// for each command line and file processed" (120336-120338). Each existing
/// test checks one bit in isolation; this checks that two conditions in one
/// invocation OR together rather than the last one winning.
#[test]
fn val_exit_status_ors_the_codes_of_all_operands() {
    let tmp = TempDir::new().unwrap();

    let x = create_sccs_file(&tmp, "x", "content x\n");
    let corrupt = create_sccs_file(&tmp, "corrupt", "content zzz\n");
    corrupt_body(&corrupt, b"content");

    // 0x02 (%Y%, -y mismatch) | 0x10 (unopenable)
    let out = val_run(
        &["-y", "source", x.to_str().unwrap(), "s.does-not-exist"],
        tmp.path(),
    );
    assert_eq!(
        out.status.code(),
        Some(0x02 | 0x10),
        "expected 0x12; stderr={:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // 0x20 (corrupted) | 0x10 (not SCCS)
    let plain = tmp.path().join("plain.txt");
    fs::write(&plain, b"just text\n").unwrap();
    let out = val_run(
        &[corrupt.to_str().unwrap(), plain.to_str().unwrap()],
        tmp.path(),
    );
    assert_eq!(out.status.code(), Some(0x20 | 0x10), "expected 0x30");
}

/// The spec's EXAMPLE (120345-120355): `val -` reads command lines from
/// standard input, each line being a fresh argument list. The erroring lines
/// are echoed with their diagnostic, and the final status is the OR across all
/// of them.
///
/// Worth recording: CSSC 1.4.1 does *not* implement this form — it treats each
/// whole input line as a single filename — so this is a case where the
/// reference implementation is the less conforming one and cannot be used as
/// an oracle.
#[test]
fn val_reads_command_lines_from_standard_input() {
    let tmp = TempDir::new().unwrap();

    let _x = create_sccs_file(&tmp, "x", "content x\n");
    let _y = create_sccs_file(&tmp, "y", "content y\n");
    let corrupt = create_sccs_file(&tmp, "corrupt", "content zzz\n");
    corrupt_body(&corrupt, b"content");

    // Mirrors the spec example: a -y mismatch, a matching -m (no error), and a
    // corrupted file.
    let script = "-y source s.x\n-m y s.y\ns.corrupt\n";
    let out = val_run_stdin(&["-"], tmp.path(), script);

    assert_eq!(
        out.status.code(),
        Some(0x02 | 0x20),
        "the aggregate must OR every command line's code; stdout={:?} stderr={:?}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );

    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        combined.contains("-y source s.x"),
        "the erroring command line is echoed: {combined:?}"
    );
    assert!(
        combined.contains("s.corrupt"),
        "the corrupted file's line is echoed: {combined:?}"
    );
    assert!(
        !combined.contains("-m y s.y"),
        "a command line with no error is not echoed: {combined:?}"
    );
}
