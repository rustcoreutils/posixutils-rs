//
// Copyright (c) 2024-2025 Jeff Garzik
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_u8, run_test_with_checker, TestPlan, TestPlanU8};

const EXIT_STATUS_NO_DIFFERENCE: i32 = 0;
const EXIT_STATUS_DIFFERENCE: i32 = 1;
const EXIT_STATUS_TROUBLE: i32 = 2;
use std::io::Write as _;
use std::{collections::HashMap, path::PathBuf, process::Stdio, sync::LazyLock};

/// Write `content` to a uniquely named temp file and return its path. The
/// `tag` must be unique per test to avoid collisions under parallel runs.
fn write_tmp(tag: &str, content: &[u8]) -> String {
    let path = std::env::temp_dir().join(format!("pu_difftest_{}_{}", std::process::id(), tag));
    let mut f = std::fs::File::create(&path).expect("create temp file");
    f.write_all(content).expect("write temp file");
    path.to_str().unwrap().to_string()
}

/// Run `diff` with `args`, asserting stdout, stderr, and exit code.
fn diff_test_full(args: &[&str], out: &str, err: &str, code: i32) {
    run_test(TestPlan {
        cmd: String::from("diff"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::from(out),
        expected_err: String::from(err),
        expected_exit_code: code,
    });
}

/// Run `diff` with `args`, asserting stdout and stderr byte for byte.
///
/// `run_test` compares through `String::from_utf8_lossy`, which replaces every
/// invalid byte with U+FFFD on both sides -- turning exactly the corruption
/// these tests exist to catch into a match. A `TestPlan` also cannot express a
/// non-UTF-8 expectation in the first place.
fn diff_test_bytes(args: &[&str], out: &[u8], err: &[u8], code: i32) {
    run_test_u8(TestPlanU8 {
        cmd: String::from("diff"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: Vec::new(),
        expected_out: out.to_vec(),
        expected_err: err.to_vec(),
        expected_exit_code: code,
    });
}

fn diff_test(args: &[&str], expected_output: &str, expected_exit_code: i32) {
    let str_args = args.iter().cloned().map(str::to_owned).collect();

    run_test(TestPlan {
        cmd: String::from("diff"),
        args: str_args,
        stdin_data: String::from(""),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code,
    });
}

fn diff_base_path() -> PathBuf {
    PathBuf::from("tests").join("diff")
}

fn f1_txt_path() -> String {
    diff_base_path()
        .join("f1.txt")
        .to_str()
        .expect("Could not unwrap f1_txt_path")
        .to_string()
}

fn f2_txt_path() -> String {
    diff_base_path()
        .join("f2.txt")
        .to_str()
        .expect("Could not unwrap f2_txt_path")
        .to_string()
}

fn f1_dir_path() -> String {
    diff_base_path()
        .join("f1")
        .to_str()
        .expect("Could not unwrap f1_dir_path")
        .to_string()
}

fn f2_dir_path() -> String {
    diff_base_path()
        .join("f2")
        .to_str()
        .expect("Could not unwrap f2_dir_path")
        .to_string()
}

fn f1_txt_with_eol_spaces_path() -> String {
    diff_base_path()
        .join("f1_with_eol_spaces.txt")
        .to_str()
        .expect("Could not unwrap f1_txt_with_eol_spaces_path")
        .to_string()
}

struct DiffTestHelper {
    content: String,
    file1_path: String,
    file2_path: String,
}

impl DiffTestHelper {
    fn new(options: &str, file1_path: String, file2_path: String) -> Self {
        // Use the test binary directly instead of cargo run for performance
        let binary = env!("CARGO_BIN_EXE_diff");

        let mut args: Vec<&str> = Vec::new();
        if !options.is_empty() {
            args.extend(options.split_whitespace());
        }
        args.push(&file1_path);
        args.push(&file2_path);

        let output = std::process::Command::new(binary)
            .args(&args)
            .stdout(Stdio::piped())
            .output()
            .expect("Could not run diff binary!");

        let content = String::from_utf8(output.stdout).expect("Failed to read output of Command!");

        Self {
            file1_path,
            file2_path,
            content,
        }
    }

    fn content(&self) -> &str {
        &self.content
    }

    fn file1_path(&self) -> &str {
        &self.file1_path
    }

    fn file2_path(&self) -> &str {
        &self.file2_path
    }
}

fn get_diff_test_helper_hash_map() -> HashMap<String, DiffTestHelper> {
    let diff_test_helper_init_data = [
        ("", f1_txt_path(), f2_txt_path(), "test_diff_normal"),
        (" -c", f1_txt_path(), f2_txt_path(), "test_diff_context3"),
        (" -C 1", f1_txt_path(), f2_txt_path(), "test_diff_context1"),
        (
            " -C 10",
            f1_txt_path(),
            f2_txt_path(),
            "test_diff_context10",
        ),
        (" -e", f1_txt_path(), f2_txt_path(), "test_diff_edit_script"),
        (
            " -f",
            f1_txt_path(),
            f2_txt_path(),
            "test_diff_forward_edit_script",
        ),
        (" -u", f1_txt_path(), f2_txt_path(), "test_diff_unified3"),
        (" -U 0", f1_txt_path(), f2_txt_path(), "test_diff_unified0"),
        (
            " -U 10",
            f1_txt_path(),
            f2_txt_path(),
            "test_diff_unified10",
        ),
        ("", f1_txt_path(), f2_dir_path(), "test_diff_file_directory"),
        ("", f1_dir_path(), f2_dir_path(), "test_diff_directories"),
        (
            " -r",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive",
        ),
        (
            " -r -c",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive_context",
        ),
        (
            " -r -e",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive_edit_script",
        ),
        (
            " -r -f",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive_forward_edit_script",
        ),
        (
            " -r -u",
            f1_dir_path(),
            f2_dir_path(),
            "test_diff_directories_recursive_unified",
        ),
        (
            "",
            f1_txt_path(),
            f1_txt_with_eol_spaces_path(),
            "test_diff_counting_eol_spaces",
        ),
        (
            " -b",
            f1_txt_path(),
            f1_txt_with_eol_spaces_path(),
            "test_diff_ignoring_eol_spaces",
        ),
        (
            " --label F1 --label2 F2 -u",
            f1_txt_path(),
            f1_txt_with_eol_spaces_path(),
            "test_diff_unified_two_labels",
        ),
    ];

    let mut diff_test_helper_hash_map =
        HashMap::<String, DiffTestHelper>::with_capacity(diff_test_helper_init_data.len());

    for (options, file1_path, file2_path, key) in diff_test_helper_init_data {
        let insert_option = diff_test_helper_hash_map.insert(
            key.to_owned(),
            DiffTestHelper::new(options, file1_path, file2_path),
        );

        assert!(insert_option.is_none());
    }

    diff_test_helper_hash_map
}

fn input_by_key(key: &str) -> &'static DiffTestHelper {
    static DIFF_TEST_INPUT: LazyLock<HashMap<String, DiffTestHelper>> =
        LazyLock::new(get_diff_test_helper_hash_map);

    // Initialized on first access
    DIFF_TEST_INPUT.get(key).unwrap()
}

#[test]
fn test_diff_normal() {
    let data = input_by_key("test_diff_normal");

    diff_test(
        &[data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_context3() {
    let data = input_by_key("test_diff_context3");

    diff_test(
        &["-c", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_context1() {
    let data = input_by_key("test_diff_context1");

    diff_test(
        &["-C", "1", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_context10() {
    let data = input_by_key("test_diff_context10");

    diff_test(
        &["-C", "10", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_edit_script() {
    let data = input_by_key("test_diff_edit_script");

    diff_test(
        &["-e", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_forward_edit_script() {
    let data = input_by_key("test_diff_forward_edit_script");

    diff_test(
        &["-f", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_unified3() {
    let data = input_by_key("test_diff_unified3");

    diff_test(
        &["-u", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_unified0() {
    let data = input_by_key("test_diff_unified0");

    diff_test(
        &["-U", "0", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_unified10() {
    let data = input_by_key("test_diff_unified10");

    diff_test(
        &["-U", "10", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_file_directory() {
    let data = input_by_key("test_diff_file_directory");

    diff_test(
        &[data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories() {
    let data = input_by_key("test_diff_directories");

    diff_test(
        &[data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive() {
    let data = input_by_key("test_diff_directories_recursive");

    diff_test(
        &["-r", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive_context() {
    let data = input_by_key("test_diff_directories_recursive_context");

    diff_test(
        &["-r", "-c", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive_edit_script() {
    let data = input_by_key("test_diff_directories_recursive_edit_script");

    diff_test(
        &["-r", "-e", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive_forward_edit_script() {
    let data = input_by_key("test_diff_directories_recursive_forward_edit_script");

    diff_test(
        &["-r", "-f", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_directories_recursive_unified() {
    let data = input_by_key("test_diff_directories_recursive_unified");

    diff_test(
        &["-r", "-u", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_counting_eol_spaces() {
    let data = input_by_key("test_diff_counting_eol_spaces");

    diff_test(
        &[data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_ignoring_eol_spaces() {
    let data = input_by_key("test_diff_ignoring_eol_spaces");

    diff_test(
        &["-b", data.file1_path(), data.file2_path()],
        data.content(),
        EXIT_STATUS_NO_DIFFERENCE,
    );
}

#[test]
fn test_diff_unified_two_labels() {
    let data = input_by_key("test_diff_unified_two_labels");

    diff_test(
        &[
            "--label",
            "F1",
            "--label2",
            "F2",
            "-u",
            data.file1_path(),
            data.file2_path(),
        ],
        data.content(),
        EXIT_STATUS_DIFFERENCE,
    );
}

// --- POSIX-conformance regression tests (outputs verified against GNU diff) ---

/// `-b`: a sequence of blanks compares equal to any other non-empty sequence,
/// and trailing blanks are ignored, so interior/trailing whitespace differences
/// are not reported.
#[test]
fn test_diff_b_interior_blanks_equal() {
    let f1 = write_tmp("b_interior_1", b"a  b\tc \n");
    let f2 = write_tmp("b_interior_2", b"a b c\n");
    diff_test_full(&["-b", &f1, &f2], "", "", EXIT_STATUS_NO_DIFFERENCE);
}

/// `-b`: the presence vs. absence of leading blanks IS significant (a leading
/// blank run does not compare equal to no leading blank).
#[test]
fn test_diff_b_leading_blanks_significant() {
    let f1 = write_tmp("b_leading_1", b"   foo\n");
    let f2 = write_tmp("b_leading_2", b"foo\n");
    diff_test_full(
        &["-b", &f1, &f2],
        "1c1\n<    foo\n---\n> foo\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// `-f`: multi-line ranges are space-separated (`c2 4`), not comma-separated.
#[test]
fn test_diff_forward_multiline_range() {
    let f1 = write_tmp("fed_range_1", b"a\nb\nc\nd\ne\n");
    let f2 = write_tmp("fed_range_2", b"a\nX\nY\nZ\nW\ne\n");
    diff_test_full(
        &["-f", &f1, &f2],
        "c2 4\nX\nY\nZ\nW\n.\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// `-C 0`: zero context must be accepted, and a single-line context range is
/// printed with one number (`*** 2 ****`), not two.
#[test]
fn test_diff_context_zero_single_line_range() {
    let f1 = write_tmp("c0_range_1", b"a\nb\nc\nd\ne\n");
    let f2 = write_tmp("c0_range_2", b"a\nB\nc\nd\ne\n");
    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-C0", &f1, &f2],
        "*** L1\n--- L2\n***************\n*** 2 ****\n! b\n--- 2 ----\n! B\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// `-U 0`: zero context must be accepted, and a single-line unified range is
/// printed with one number (`@@ -2 +2 @@`).
#[test]
fn test_diff_unified_zero_single_line_range() {
    let f1 = write_tmp("u0_range_1", b"a\nb\nc\nd\ne\n");
    let f2 = write_tmp("u0_range_2", b"a\nB\nc\nd\ne\n");
    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-U0", &f1, &f2],
        "--- L1\n+++ L2\n@@ -2 +2 @@\n-b\n+B\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// Default format: the "no newline" marker is emitted for an appended last line
/// that lacks a trailing newline (Insert hunk at end of file).
#[test]
fn test_diff_default_no_newline_append() {
    let f1 = write_tmp("nonl_app_1", b"a\nb\n");
    let f2 = write_tmp("nonl_app_2", b"a\nb\nc");
    diff_test_full(
        &[&f1, &f2],
        "2a3\n> c\n\\ No newline at end of file\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// Default format: the marker follows the file1 line that lacks a trailing
/// newline; file2 (which has one) gets no marker.
#[test]
fn test_diff_default_no_newline_substitute() {
    let f1 = write_tmp("nonl_sub_1", b"a\nb\nc");
    let f2 = write_tmp("nonl_sub_2", b"a\nb\nC\n");
    diff_test_full(
        &[&f1, &f2],
        "3c3\n< c\n\\ No newline at end of file\n---\n> C\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// `-e`: the "no newline" diagnostic must never corrupt the ed script on
/// stdout; it goes to stderr, and a missing trailing newline yields exit 2.
#[test]
fn test_diff_edit_script_no_newline_stderr() {
    let f1 = write_tmp("ed_nonl_1", b"a\nb\nc");
    let f2 = write_tmp("ed_nonl_2", b"a\nb\nC");
    let err = format!(
        "diff: {}: No newline at end of file\n\ndiff: {}: No newline at end of file\n\n",
        f1, f2
    );
    diff_test_full(&["-e", &f1, &f2], "3c\nC\n.\n", &err, EXIT_STATUS_TROUBLE);
}

/// Unified header timestamps carry fractional seconds and a timezone offset,
/// e.g. `2024-01-01 12:00:00.000000000 +0000`.
#[test]
fn test_diff_unified_header_timestamp_format() {
    let f1 = write_tmp("uhdr_1", b"a\n");
    let f2 = write_tmp("uhdr_2", b"b\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("diff"),
            args: vec![String::from("-u"), f1.clone(), f2.clone()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: EXIT_STATUS_DIFFERENCE,
        },
        |_plan, output| {
            assert_eq!(output.status.code(), Some(EXIT_STATUS_DIFFERENCE));
            let stdout = String::from_utf8_lossy(&output.stdout);
            let header = stdout.lines().next().expect("missing header line");
            let ts = header.split('\t').nth(1).expect("missing timestamp field");
            // e.g. "2026-06-25 19:16:19.521127253 +0000"
            let (datetime, offset) = ts.rsplit_once(' ').expect("missing tz offset");
            assert!(
                offset.len() == 5
                    && (offset.starts_with('+') || offset.starts_with('-'))
                    && offset[1..].chars().all(|c| c.is_ascii_digit()),
                "bad tz offset: {ts:?}"
            );
            let frac = datetime
                .rsplit_once('.')
                .expect("missing fractional seconds")
                .1;
            assert!(
                frac.len() == 9 && frac.chars().all(|c| c.is_ascii_digit()),
                "bad fractional seconds: {ts:?}"
            );
        },
    );
}

// ---------------------------------------------------------------------------
// Operands, exit status and boundary hunk ranges
// ---------------------------------------------------------------------------

/// Write `content` to a uniquely named temporary file and return its path.
fn diff_tmp(tag: &str, content: &str) -> PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("posixutils-diff-{}-{}", std::process::id(), tag));
    std::fs::write(&p, content).expect("write temp file");
    p
}

#[test]
fn test_diff_identical_files_are_silent_and_exit_zero() {
    let a = diff_tmp("same-a", "x\ny\n");
    let b = diff_tmp("same-b", "x\ny\n");
    diff_test(&[a.to_str().unwrap(), b.to_str().unwrap()], "", 0);
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn test_diff_exit_status_is_zero_one_two() {
    // POSIX EXIT STATUS: 0 no differences, 1 differences, >1 an error.
    let a = diff_tmp("st-a", "x\n");
    let b = diff_tmp("st-b", "y\n");
    let code = |args: &[&str]| {
        std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
            .args(args)
            .output()
            .expect("run diff")
            .status
            .code()
    };
    assert_eq!(code(&[a.to_str().unwrap(), a.to_str().unwrap()]), Some(0));
    assert_eq!(code(&[a.to_str().unwrap(), b.to_str().unwrap()]), Some(1));
    assert_eq!(
        code(&[a.to_str().unwrap(), "no-such-file-xyz"]),
        Some(2),
        "an inaccessible operand is an error, not a difference"
    );
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn test_diff_stdin_operand() {
    // A `-` operand reads standard input as one of the two files.
    use std::io::Write;
    let a = diff_tmp("stdin-a", "a\nb\nc\n");
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
        .args([a.to_str().unwrap(), "-"])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("spawn diff");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b"a\nX\nc\n")
        .unwrap();
    let out = child.wait_with_output().expect("wait diff");
    assert_eq!(out.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("2c2"), "got {stdout:?}");
    assert!(
        stdout.contains("< b") && stdout.contains("> X"),
        "got {stdout:?}"
    );
    let _ = std::fs::remove_file(a);
}

#[test]
fn test_diff_context_empty_range_header() {
    // An empty first file has no LCS entries, so building the trailing hunk
    // indexed `lcs_indices[len - 1]` and underflowed: `diff -c` panicked
    // outright on this input. POSIX/GNU render the empty side as `*** 0 ****`.
    let empty = diff_tmp("empty", "");
    let one = diff_tmp("one", "x\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
        .args(["-c", empty.to_str().unwrap(), one.to_str().unwrap()])
        .output()
        .expect("run diff");
    assert_eq!(out.status.code(), Some(1), "must not crash");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("*** 0 ****"), "got {stdout:?}");
    assert!(stdout.contains("--- 1 ----"), "got {stdout:?}");
    assert!(stdout.contains("+ x"), "got {stdout:?}");

    // And the same pair the other way round.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
        .args(["-c", one.to_str().unwrap(), empty.to_str().unwrap()])
        .output()
        .expect("run diff");
    assert_eq!(out.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("--- 0 ----"), "got {stdout:?}");
    let _ = std::fs::remove_file(empty);
    let _ = std::fs::remove_file(one);
}

#[test]
fn test_diff_no_trailing_newline_in_context_and_unified() {
    let a = diff_tmp("nonl-a", "a");
    let b = diff_tmp("nonl-b", "b");
    for mode in ["-c", "-u"] {
        let out = std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
            .args([mode, a.to_str().unwrap(), b.to_str().unwrap()])
            .output()
            .expect("run diff");
        assert_eq!(out.status.code(), Some(1));
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert_eq!(
            stdout.matches("\\ No newline at end of file").count(),
            2,
            "{mode} must mark both sides, got {stdout:?}"
        );
    }
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn test_diff_edit_script_escapes_a_lone_period() {
    // In an `-e` script a line consisting of a single `.` would terminate the
    // input mode, so it must be escaped to survive being fed to `ed`.
    let a = diff_tmp("dot-a", "x\n");
    let b = diff_tmp("dot-b", ".\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
        .args(["-e", a.to_str().unwrap(), b.to_str().unwrap()])
        .output()
        .expect("run diff");
    assert_eq!(out.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&out.stdout);
    // A bare "." would end ed's input mode early and drop the rest of the
    // block, so the content line must be escaped and repaired afterwards.
    let lines: Vec<&str> = stdout.lines().collect();
    let bare_dots = lines.iter().filter(|l| **l == ".").count();
    assert_eq!(
        bare_dots, 1,
        "only the terminator may be a lone period, got {stdout:?}"
    );
    assert!(
        lines.contains(&".."),
        "the content line must be escaped, got {stdout:?}"
    );
    assert!(
        lines.iter().any(|l| l.contains("s/^\\.\\.$/./")),
        "the escape must be repaired, got {stdout:?}"
    );
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn test_diff_directory_with_a_fifo() {
    // A non-regular file in a recursive comparison must not make diff block on
    // opening it, nor abort the walk over the regular files beside it.
    let base = std::env::temp_dir().join(format!("posixutils-diff-fifo-{}", std::process::id()));
    let (a, b) = (base.join("a"), base.join("b"));
    std::fs::create_dir_all(&a).expect("mkdir a");
    std::fs::create_dir_all(&b).expect("mkdir b");
    std::fs::write(a.join("f"), "x\n").expect("write a/f");
    std::fs::write(b.join("f"), "y\n").expect("write b/f");
    let made_fifo = std::process::Command::new("mkfifo")
        .arg(a.join("pipe"))
        .status()
        .map(|s| s.success())
        .unwrap_or(false);
    if made_fifo {
        std::fs::write(b.join("pipe"), "z\n").expect("write b/pipe");
    }

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
        .args(["-r", a.to_str().unwrap(), b.to_str().unwrap()])
        .output()
        .expect("run diff -r");
    assert_eq!(
        out.status.code(),
        Some(1),
        "the regular files differ, so the walk must report differences"
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("< x") && stdout.contains("> y"),
        "got {stdout:?}"
    );
    let _ = std::fs::remove_dir_all(&base);
}

#[test]
fn test_diff_recursive_symlink_cycle_terminates() {
    // A directory containing a symlink back to itself must not send the
    // recursive walk into an infinite loop.
    let base = std::env::temp_dir().join(format!("posixutils-diff-loop-{}", std::process::id()));
    let dir = base.join("d");
    // A crashed earlier run can leave this behind, and PIDs are recycled; the
    // symlink below would then fail with EEXIST.
    let _ = std::fs::remove_dir_all(&base);
    std::fs::create_dir_all(&dir).expect("mkdir d");
    std::fs::write(dir.join("f"), "x\n").expect("write d/f");
    // The cycle is the whole point of the test: without it `diff -r` has
    // nothing to loop on and the run below would pass without proving
    // anything. Both supported platforms symlink here, so this is a hard
    // precondition rather than a best-effort one.
    std::os::unix::fs::symlink("..", dir.join("up")).expect("symlink d/up -> ..");

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
        .args(["-r", dir.to_str().unwrap(), dir.to_str().unwrap()])
        .output()
        .expect("run diff -r");
    assert_eq!(
        out.status.code(),
        Some(0),
        "a directory compared with itself has no differences"
    );
    let _ = std::fs::remove_dir_all(&base);
}

/// Lines inserted before the first line the two files have in common are a
/// hunk like any other. The hunk builder used to suppress the whole `i == 0`
/// case to dodge an index underflow in its "insertion, no deletion" branch,
/// so `diff` reported the two files identical and exited 0.
#[test]
fn test_diff_insertion_before_first_common_line() {
    let f1 = write_tmp("head_ins_1", b"x\ny\n");
    let f2 = write_tmp("head_ins_2", b"new\nx\ny\n");
    diff_test_full(&[&f1, &f2], "0a1\n> new\n", "", EXIT_STATUS_DIFFERENCE);
}

/// The same insertion in every output format, since the defect was in hunk
/// construction and so was invisible to whichever printer ran afterwards.
#[test]
fn test_diff_insertion_before_first_common_line_all_formats() {
    let f1 = write_tmp("head_ins_fmt_1", b"x\ny\n");
    let f2 = write_tmp("head_ins_fmt_2", b"new\nx\ny\n");
    diff_test_full(
        &["-u", "--label", "L1", "--label2", "L2", &f1, &f2],
        "--- L1\n+++ L2\n@@ -1,2 +1,3 @@\n+new\n x\n y\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
    diff_test_full(
        &["-e", &f1, &f2],
        "0a\nnew\n.\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
    diff_test_full(
        &["-f", &f1, &f2],
        "a0\nnew\n.\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// Several lines inserted at the head, with the tail also changed, so the
/// head hunk is followed by another one rather than being the whole diff.
#[test]
fn test_diff_multi_line_head_insertion_with_later_change() {
    let f1 = write_tmp("head_ins_multi_1", b"a\nb\nc\n");
    let f2 = write_tmp("head_ins_multi_2", b"p\nq\na\nb\nZ\n");
    diff_test_full(
        &[&f1, &f2],
        "0a1,2\n> p\n> q\n3c5\n< c\n---\n> Z\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// A first line that already matches must NOT produce an empty leading hunk:
/// the guard being removed exists to keep `lcs_indices[0] == 0` quiet.
#[test]
fn test_diff_matching_first_line_emits_no_empty_hunk() {
    let f1 = write_tmp("no_empty_hunk_1", b"a\nb\n");
    let f2 = write_tmp("no_empty_hunk_2", b"a\nZ\n");
    diff_test_full(
        &[&f1, &f2],
        "2c2\n< b\n---\n> Z\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// Output must not depend on the process-random iteration order of the
/// histogram map. Ties on the pivot's occurrence count are the common case,
/// so a run-to-run difference here is the norm, not an edge case.
#[test]
fn test_diff_output_is_deterministic() {
    // A small alphabet makes almost every line a tie candidate.
    let alphabet = ["a", "b", "c", "d"];
    let mut s1 = String::new();
    let mut s2 = String::new();
    let mut state: u64 = 0x2545F491_4F6CDD1D;
    let mut next = || {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        state
    };
    for _ in 0..300 {
        s1.push_str(alphabet[(next() % 4) as usize]);
        s1.push('\n');
        s2.push_str(alphabet[(next() % 4) as usize]);
        s2.push('\n');
    }
    let f1 = write_tmp("determinism_1", s1.as_bytes());
    let f2 = write_tmp("determinism_2", s2.as_bytes());

    let once = || {
        std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
            .args([&f1, &f2])
            .env("LC_ALL", "C")
            .output()
            .expect("run diff")
            .stdout
    };
    // Compare as text: a mismatch on 300 lines of byte vectors is unreadable.
    let first = String::from_utf8_lossy(&once()).into_owned();
    assert!(!first.is_empty(), "the two files must actually differ");
    for run in 1..12 {
        let again = String::from_utf8_lossy(&once()).into_owned();
        assert_eq!(
            again, first,
            "run {run} produced different output from run 0"
        );
    }
}

/// A context section whose second-file pane is a pure insertion used to be
/// tested for the `-` prefix that pane can never carry, so its body was
/// dropped -- and because only the printing path cleared the buffer, the
/// dropped lines were then flushed into the *next* section. `patch` rejects
/// the result as "context mangled in hunk".
#[test]
fn test_diff_context_pure_insertion_section_is_not_swallowed() {
    let a: String = (1..=40).map(|n| format!("{n}\n")).collect();
    let b: String = (1..=4)
        .map(|n| format!("{n}\n"))
        .chain(std::iter::once("INS\n".to_string()))
        .chain((5..=29).map(|n| format!("{n}\n")))
        .chain(std::iter::once("CHANGED\n".to_string()))
        .chain((31..=40).map(|n| format!("{n}\n")))
        .collect();
    let f1 = write_tmp("sect_1", a.as_bytes());
    let f2 = write_tmp("sect_2", b.as_bytes());

    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-c", &f1, &f2],
        concat!(
            "*** L1\n--- L2\n",
            "***************\n",
            "*** 2,7 ****\n",
            "--- 2,8 ----\n  2\n  3\n  4\n+ INS\n  5\n  6\n  7\n",
            "***************\n",
            "*** 27,33 ****\n  27\n  28\n  29\n! 30\n  31\n  32\n  33\n",
            "--- 28,34 ----\n  27\n  28\n  29\n! CHANGED\n  31\n  32\n  33\n",
        ),
        "",
        EXIT_STATUS_DIFFERENCE,
    );
    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-u", &f1, &f2],
        concat!(
            "--- L1\n+++ L2\n",
            "@@ -2,6 +2,7 @@\n 2\n 3\n 4\n+INS\n 5\n 6\n 7\n",
            "@@ -27,7 +28,7 @@\n 27\n 28\n 29\n-30\n+CHANGED\n 31\n 32\n 33\n",
        ),
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// A head insertion leaves the first file's cursor at line 0, which both
/// context printers used as their "no hunk seen yet" sentinel. The second
/// hunk then re-ran first-hunk setup, discarding the position already reached
/// and emitting a section header for the wrong lines.
#[test]
fn test_diff_head_insertion_does_not_displace_later_sections() {
    let f1 = write_tmp("headsect_1", b"x\ny\n");
    let f2 = write_tmp("headsect_2", b"new\nx\ny\n");
    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-c", &f1, &f2],
        "*** L1\n--- L2\n***************\n*** 1,2 ****\n--- 1,3 ----\n+ new\n  x\n  y\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );

    // Two hunks, the first of them a head insertion: the second section's
    // ranges must still describe the lines it actually prints.
    let a: String = (1..=40).map(|n| format!("{n}\n")).collect();
    let b: String = std::iter::once("HEAD\n".to_string())
        .chain((1..=29).map(|n| format!("{n}\n")))
        .chain(std::iter::once("CHANGED\n".to_string()))
        .chain((31..=40).map(|n| format!("{n}\n")))
        .collect();
    let g1 = write_tmp("headsect_3", a.as_bytes());
    let g2 = write_tmp("headsect_4", b.as_bytes());
    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-u", &g1, &g2],
        concat!(
            "--- L1\n+++ L2\n",
            "@@ -1,3 +1,4 @@\n+HEAD\n 1\n 2\n 3\n",
            "@@ -27,7 +28,7 @@\n 27\n 28\n 29\n-30\n+CHANGED\n 31\n 32\n 33\n",
        ),
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// An empty second file leaves nothing to take trailing context from, so the
/// range arithmetic must clamp rather than underflow, and the empty side
/// renders as a zero-width range.
#[test]
fn test_diff_empty_second_file_context_ranges() {
    let f1 = write_tmp("empty2_1", b"a\nb\n");
    let f2 = write_tmp("empty2_2", b"");
    diff_test_full(&[&f1, &f2], "1,2d0\n< a\n< b\n", "", EXIT_STATUS_DIFFERENCE);
    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-c", &f1, &f2],
        "*** L1\n--- L2\n***************\n*** 1,2 ****\n- a\n- b\n--- 0 ----\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-u", &f1, &f2],
        "--- L1\n+++ L2\n@@ -1,2 +0,0 @@\n-a\n-b\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// The real acceptance property: context and unified output must be
/// applicable. Feeding our own patch to our own `patch` with the first file
/// has to reproduce the second, byte for byte, at every context width.
#[test]
fn test_diff_context_and_unified_round_trip_through_patch() {
    let a: String = (1..=60).map(|n| format!("line {n}\n")).collect();
    let b: String = std::iter::once("HEAD\n".to_string())
        .chain((1..=9).map(|n| format!("line {n}\n")))
        .chain(std::iter::once("CHANGED\n".to_string()))
        .chain((11..=29).map(|n| format!("line {n}\n")))
        .chain((40..=60).map(|n| format!("line {n}\n")))
        .chain(std::iter::once("TAIL\n".to_string()))
        .collect();
    let f1 = write_tmp("rt_1", a.as_bytes());
    let f2 = write_tmp("rt_2", b.as_bytes());

    // Unified at every width, context from 2 up. The two excluded context
    // widths are not diff defects -- our output is byte-identical to GNU's at
    // both -- but limits of the appliers: GNU patch rejects a zero-context
    // context diff, including one GNU diff produced itself, and our own patch
    // cannot parse -C 1, again including GNU's own output.
    let cases: Vec<(&str, &str)> = ["0", "1", "2", "3", "5"]
        .iter()
        .map(|w| ("-U", *w))
        .chain(["2", "3", "5"].iter().map(|w| ("-C", *w)))
        .collect();
    {
        for (flag, width) in cases {
            let patch_text = std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
                .args([flag, width, &f1, &f2])
                .env("LC_ALL", "C")
                .output()
                .expect("run diff");
            assert_eq!(
                patch_text.status.code(),
                Some(EXIT_STATUS_DIFFERENCE),
                "{flag} {width}: the two files differ"
            );

            let work = write_tmp(&format!("rt_work_{flag}_{width}"), a.as_bytes());
            let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_patch"))
                .args(["-f", &work])
                .env("LC_ALL", "C")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .expect("spawn patch");
            child
                .stdin
                .take()
                .expect("patch stdin")
                .write_all(&patch_text.stdout)
                .expect("feed patch");
            let done = child.wait_with_output().expect("wait for patch");
            assert!(
                done.status.success(),
                "{flag} {width}: patch failed: {}",
                String::from_utf8_lossy(&done.stderr)
            );
            assert_eq!(
                std::fs::read(&work).expect("read patched file"),
                b.as_bytes(),
                "{flag} {width}: patched file does not match the second file"
            );
            let _ = std::fs::remove_file(&work);
        }
    }
}

/// POSIX INPUT FILES: "The input files may be of any type." A Latin-1 file
/// has no NUL byte, so it is not binary, but reading it as UTF-8 failed and
/// diff exited 2 with "stream did not contain valid UTF-8".
#[test]
fn test_diff_latin1_lines_are_compared_as_bytes() {
    let f1 = write_tmp("latin1_1", b"caf\xe9\nbar\n");
    let f2 = write_tmp("latin1_2", b"caf\xe9\nbaz\n");
    diff_test_bytes(
        &[&f1, &f2],
        b"2c2\n< bar\n---\n> baz\n",
        b"",
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_latin1_identical_files_exit_zero() {
    let f1 = write_tmp("latin1_same_1", b"caf\xe9\nbar\n");
    let f2 = write_tmp("latin1_same_2", b"caf\xe9\nbar\n");
    diff_test_bytes(&[&f1, &f2], b"", b"", EXIT_STATUS_NO_DIFFERENCE);
}

/// The bytes of the file have to survive into the patch, or the patch does not
/// apply to the file it came from.
#[test]
fn test_diff_unified_and_edit_script_preserve_non_utf8_bytes() {
    let f1 = write_tmp("latin1_u_1", b"caf\xe9\nbar\n");
    let f2 = write_tmp("latin1_u_2", b"caf\xe9\nbaz\n");
    diff_test_bytes(
        &["--label", "L1", "--label2", "L2", "-u", &f1, &f2],
        b"--- L1\n+++ L2\n@@ -1,2 +1,2 @@\n caf\xe9\n-bar\n+baz\n",
        b"",
        EXIT_STATUS_DIFFERENCE,
    );
    diff_test_bytes(
        &["-e", &f1, &f2],
        b"2c\nbaz\n.\n",
        b"",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// A `\r` before the newline is data. Stripping it made a CRLF file compare
/// equal to the same text with LF endings, and made `diff -u` of two CRLF
/// files emit LF-only bodies, so applying that patch silently converted every
/// line ending in the target.
#[test]
fn test_diff_crlf_and_lf_files_differ() {
    let f1 = write_tmp("crlf_1", b"a\r\nb\r\n");
    let f2 = write_tmp("crlf_2", b"a\nb\n");
    diff_test_full(
        &[&f1, &f2],
        "1,2c1,2\n< a\r\n< b\r\n---\n> a\n> b\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

#[test]
fn test_diff_crlf_identical_files_exit_zero() {
    let f1 = write_tmp("crlf_same_1", b"a\r\nb\r\n");
    let f2 = write_tmp("crlf_same_2", b"a\r\nb\r\n");
    diff_test_full(&[&f1, &f2], "", "", EXIT_STATUS_NO_DIFFERENCE);
}

#[test]
fn test_diff_context_formats_keep_carriage_returns() {
    let f1 = write_tmp("crlf_ctx_1", b"a\r\nb\r\nc\r\n");
    let f2 = write_tmp("crlf_ctx_2", b"a\r\nB\r\nc\r\n");
    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-u", &f1, &f2],
        "--- L1\n+++ L2\n@@ -1,3 +1,3 @@\n a\r\n-b\r\n+B\r\n c\r\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
    diff_test_full(
        &["--label", "L1", "--label2", "L2", "-c", &f1, &f2],
        concat!(
            "*** L1\n--- L2\n***************\n",
            "*** 1,3 ****\n  a\r\n! b\r\n  c\r\n",
            "--- 1,3 ----\n  a\r\n! B\r\n  c\r\n",
        ),
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// The terminated and unterminated branches of the line reader have to agree
/// about `\r`: the final line of a file with no trailing newline used to keep
/// its carriage return while every other line lost one.
#[test]
fn test_diff_final_carriage_return_without_newline() {
    let f1 = write_tmp("crlf_final_1", b"a\r");
    let f2 = write_tmp("crlf_final_2", b"a");
    diff_test_full(
        &[&f1, &f2],
        "1c1\n< a\r\n\\ No newline at end of file\n---\n> a\n\\ No newline at end of file\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
}

/// POSIX defines -b over white space "not including <newline>", so `\r` is in
/// the set and -b remains the way to compare a CRLF file with an LF one. GNU
/// agrees.
#[test]
fn test_diff_b_ignores_carriage_return() {
    let f1 = write_tmp("crlf_b_1", b"a\r\nb\r\n");
    let f2 = write_tmp("crlf_b_2", b"a\nb\n");
    diff_test_full(&["-b", &f1, &f2], "", "", EXIT_STATUS_NO_DIFFERENCE);
}

/// Whether the last line ends with a newline is part of what that line is.
/// It was only consulted when printing the marker, so two files differing in
/// nothing else were reported identical.
#[test]
fn test_diff_trailing_newline_only_difference_is_reported() {
    let f1 = write_tmp("nonl_1", b"a\nb\n");
    let f2 = write_tmp("nonl_2", b"a\nb");
    diff_test_full(
        &[&f1, &f2],
        "2c2\n< b\n---\n> b\n\\ No newline at end of file\n",
        "",
        EXIT_STATUS_DIFFERENCE,
    );
    // -b treats trailing white space, the newline included, as insignificant,
    // so it does not see this difference. GNU behaves the same way.
    diff_test_full(&["-b", &f1, &f2], "", "", EXIT_STATUS_NO_DIFFERENCE);
}

/// An empty file has no incomplete final line, so an edit script can
/// represent it. It used to be classed as lacking a trailing newline, which
/// made `diff -e` report trouble instead of producing the script.
#[test]
fn test_diff_edit_script_from_empty_file() {
    let f1 = write_tmp("emptyed_1", b"");
    let f2 = write_tmp("emptyed_2", b"a\n");
    diff_test_full(&["-e", &f1, &f2], "0a\na\n.\n", "", EXIT_STATUS_DIFFERENCE);
}

/// Binary detection is a NUL byte, as in GNU diff. The old control-byte table
/// classified an ordinary text file carrying one 0x01 as binary and refused to
/// diff it.
#[test]
fn test_diff_binary_detection_is_nul_only() {
    let t1 = write_tmp("ctl_1", b"a\x01b\nq\n");
    let t2 = write_tmp("ctl_2", b"a\x01b\nr\n");
    diff_test_bytes(
        &[&t1, &t2],
        b"2c2\n< q\n---\n> r\n",
        b"",
        EXIT_STATUS_DIFFERENCE,
    );

    let n1 = write_tmp("nul_1", b"a\0b\nq\n");
    let n2 = write_tmp("nul_2", b"a\0b\nr\n");
    let expected = format!("Binary files {n1} and {n2} differ\n");
    diff_test_full(&[&n1, &n2], &expected, "", EXIT_STATUS_DIFFERENCE);

    let n3 = write_tmp("nul_3", b"a\0b\nq\n");
    diff_test_full(&[&n1, &n3], "", "", EXIT_STATUS_NO_DIFFERENCE);
}

/// The property the byte-oriented line store exists for: a patch generated
/// from a file must reproduce that file's bytes exactly when applied. Uses our
/// own `patch`, so it also pins the two utilities to one another -- after
/// 5e8eb888 they agree that a carriage return is line content.
#[test]
fn test_diff_patch_round_trip_preserves_bytes() {
    // No non-UTF-8 case here: our `patch` still rejects such a patch with
    // "stream did not contain valid UTF-8", including one GNU diff generated,
    // so it is a patch-side gap rather than a diff one. That diff's own output
    // is right is pinned instead by
    // test_diff_unified_and_edit_script_preserve_non_utf8_bytes, whose expected
    // bytes are GNU's, and GNU patch applies that output byte-exactly.
    let cases: [(&str, &[u8], &[u8]); 2] = [
        ("crlf", b"a\r\nb\r\nc\r\nd\r\n", b"a\r\nB\r\nc\r\nd\r\n"),
        ("mixed", b"one\ntwo\r\nthree\n", b"one\ntwo\r\nTHREE\n"),
    ];

    for (tag, before, after) in cases {
        let f1 = write_tmp(&format!("rtb_{tag}_1"), before);
        let f2 = write_tmp(&format!("rtb_{tag}_2"), after);

        let patch_text = std::process::Command::new(env!("CARGO_BIN_EXE_diff"))
            .args(["-u", &f1, &f2])
            .env("LC_ALL", "C")
            .output()
            .expect("run diff -u");
        assert_eq!(
            patch_text.status.code(),
            Some(EXIT_STATUS_DIFFERENCE),
            "{tag}: the two files differ"
        );

        let work = write_tmp(&format!("rtb_{tag}_work"), before);
        let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_patch"))
            .args(["-f", &work])
            .env("LC_ALL", "C")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn patch");
        child
            .stdin
            .take()
            .expect("patch stdin")
            .write_all(&patch_text.stdout)
            .expect("feed patch");
        let done = child.wait_with_output().expect("wait for patch");
        assert!(
            done.status.success(),
            "{tag}: patch failed: {}",
            String::from_utf8_lossy(&done.stderr)
        );
        assert_eq!(
            std::fs::read(&work).expect("read patched file"),
            after,
            "{tag}: patched file does not match the second file byte for byte"
        );
        let _ = std::fs::remove_file(&work);
    }
}
