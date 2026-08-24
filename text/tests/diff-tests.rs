//
// Copyright (c) 2024-2025 Jeff Garzik
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};

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
