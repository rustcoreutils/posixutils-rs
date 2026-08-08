//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::fs::File;
use std::io::Write;
use std::process::Output;
use tempfile::TempDir;

/// Create a minimal locale source file
fn create_minimal_locale(dir: &TempDir) -> std::path::PathBuf {
    let path = dir.path().join("minimal.locale");
    let content = r#"
comment_char %
escape_char /

LC_NUMERIC
decimal_point "."
thousands_sep ","
grouping 3;3
END LC_NUMERIC

LC_MESSAGES
yesexpr "^[yY]"
noexpr "^[nN]"
END LC_MESSAGES
"#;
    let mut file = File::create(&path).unwrap();
    write!(file, "{}", content).unwrap();
    path
}

/// Test localedef with minimal input
#[test]
fn test_localedef_minimal() {
    let temp_dir = TempDir::new().unwrap();
    let locale_src = create_minimal_locale(&temp_dir);
    let output_path = temp_dir.path().join("test_locale");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-i"),
                locale_src.to_str().unwrap().to_string(),
                output_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 3,
        },
        |_plan, output: &Output| {
            assert_eq!(
                output.status.code(),
                Some(3),
                "a clean source parses, but creating locales is unsupported (102796)"
            );
        },
    );
}

/// Test localedef with -c flag (continue despite warnings)
#[test]
fn test_localedef_continue_warnings() {
    let temp_dir = TempDir::new().unwrap();
    let locale_src = create_minimal_locale(&temp_dir);
    let output_path = temp_dir.path().join("test_locale_c");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-c"),
                String::from("-i"),
                locale_src.to_str().unwrap().to_string(),
                output_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 3,
        },
        |_plan, output: &Output| {
            assert_eq!(
                output.status.code(),
                Some(3),
                "-c tolerates warnings, but exit 1 asserts a locale was created"
            );
        },
    );
}

/// Test localedef with verbose mode
#[test]
fn test_localedef_verbose() {
    let temp_dir = TempDir::new().unwrap();
    let locale_src = create_minimal_locale(&temp_dir);
    let output_path = temp_dir.path().join("test_locale_v");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-v"),
                String::from("-c"),
                String::from("-i"),
                locale_src.to_str().unwrap().to_string(),
                output_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 3,
        },
        |_plan, output: &Output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert_eq!(
                output.status.code(),
                Some(3),
                "-v does not change the outcome"
            );
            // -v is otherwise inert since LD-11 made warnings unconditional; it
            // now reports that the source itself was fine, which is the useful
            // fact alongside the creation-unsupported status.
            assert!(
                stderr.contains("parsed and validated successfully"),
                "-v should say the source validated: {stderr:?}"
            );
        },
    );
}

/// LD-5/LD-9: a valid source exits 0 and reports the processed categories on
/// standard output.
#[test]
fn test_localedef_reports_categories() {
    let temp_dir = TempDir::new().unwrap();
    let src = temp_dir.path().join("ok.locale");
    let mut f = File::create(&src).unwrap();
    write!(
        f,
        "LC_NUMERIC\ndecimal_point \".\"\nthousands_sep \",\"\nEND LC_NUMERIC\n"
    )
    .unwrap();
    let out = temp_dir.path().join("loc");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-i"),
                src.to_str().unwrap().to_string(),
                out.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 3,
        },
        |_plan, output: &Output| {
            // 102766 asks for the categories successfully *processed*, so the
            // report survives even though no locale is created.
            assert_eq!(output.status.code(), Some(3));
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("LC_NUMERIC"), "stdout report: {stdout:?}");
        },
    );
}

/// LD-12: an `END` that does not match the open category is an error, so no
/// output is created and the exit status is > 3.
#[test]
fn test_localedef_mismatched_end_errors() {
    let temp_dir = TempDir::new().unwrap();
    let src = temp_dir.path().join("bad.locale");
    let mut f = File::create(&src).unwrap();
    write!(f, "LC_NUMERIC\ndecimal_point \".\"\nEND LC_MONETARY\n").unwrap();
    let out = temp_dir.path().join("loc");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-i"),
                src.to_str().unwrap().to_string(),
                out.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 4,
        },
        |_plan, output: &Output| {
            assert!(output.status.code().unwrap() > 3, "errors must exit > 3");
            assert!(!out.exists(), "no output on error");
        },
    );
}

/// LD-10/LD-11: an unknown keyword is a warning shown on stderr (even without
/// -v); without -c the warning prevents output and the exit status is > 3.
#[test]
fn test_localedef_unknown_keyword_warns() {
    let temp_dir = TempDir::new().unwrap();
    let src = temp_dir.path().join("warn.locale");
    let mut f = File::create(&src).unwrap();
    write!(f, "LC_NUMERIC\nbogus_keyword \"x\"\nEND LC_NUMERIC\n").unwrap();
    let out = temp_dir.path().join("loc");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-i"),
                src.to_str().unwrap().to_string(),
                out.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 4,
        },
        |_plan, output: &Output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("bogus_keyword"), "warning: {stderr:?}");
            assert!(output.status.code().unwrap() > 3);
        },
    );
}

/// LD-2: a `-f charmap` that cannot be read means the charset is unsupported;
/// exit 2 and no output.
#[test]
fn test_localedef_missing_charmap_exit2() {
    let temp_dir = TempDir::new().unwrap();
    let src = temp_dir.path().join("ok.locale");
    let mut f = File::create(&src).unwrap();
    write!(f, "LC_NUMERIC\ndecimal_point \".\"\nEND LC_NUMERIC\n").unwrap();
    let out = temp_dir.path().join("loc");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-f"),
                String::from("/nonexistent/charmap.xyz"),
                String::from("-i"),
                src.to_str().unwrap().to_string(),
                out.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 2,
        },
        |_plan, output: &Output| {
            assert_eq!(output.status.code(), Some(2), "unsupported charset -> 2");
            assert!(!out.exists(), "no output");
        },
    );
}

/// Test localedef with empty input
#[test]
fn test_localedef_empty_input() {
    let temp_dir = TempDir::new().unwrap();
    let empty_src = temp_dir.path().join("empty.locale");
    File::create(&empty_src).unwrap();
    let output_path = temp_dir.path().join("empty_locale");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("localedef"),
            args: vec![
                String::from("-c"),
                String::from("-i"),
                empty_src.to_str().unwrap().to_string(),
                output_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 3,
        },
        |_plan, output: &Output| {
            // Empty input is still a clean parse, so it reaches the same
            // creation-unsupported answer rather than an error.
            assert_eq!(
                output.status.code(),
                Some(3),
                "localedef should handle empty input with -c"
            );
        },
    );
}

/// A locale source is a text file in whatever codeset `LC_CTYPE` describes, so
/// `read_to_string` rejected a valid non-UTF-8 source before a single line had
/// been looked at. The character data is validated rather than preserved, which
/// is all this implementation can do: it does not compile locales (LD-1) and
/// says so with exit 3.
#[test]
fn localedef_accepts_a_non_utf8_source() {
    let temp = tempfile::TempDir::new().unwrap();
    let src = temp.path().join("src");
    let mut source = Vec::new();
    source.extend_from_slice(b"LC_IDENTIFICATION\n");
    // A comment holding a lone 0xE9, which is not valid UTF-8.
    source.extend_from_slice(b"# caf\xe9\n");
    source.extend_from_slice(b"title \"test\"\n");
    source.extend_from_slice(b"END LC_IDENTIFICATION\n");
    std::fs::write(&src, &source).unwrap();

    let out = std::process::Command::new(plib::testing::get_binary_path("localedef"))
        .arg("-i")
        .arg(&src)
        .arg(temp.path().join("out"))
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !stderr.contains("invalid utf-8") && !stderr.contains("stream did not contain"),
        "a non-UTF-8 source must not be rejected for its encoding: {stderr}"
    );
}

/// A clean source must report "creating locales is not supported" and leave
/// nothing behind.
///
/// POSIX 102796 defines exit 3 as "The capability to create new locales is not
/// supported by the implementation", and 102680-102684 makes that capability
/// implementation-defined. This implementation parses and validates a locale
/// source but cannot compile one, so 3 is the honest answer.
///
/// It used to drop an `LC_IDENTIFICATION` marker holding the single line
/// `locale: <name>` and exit **0**, whose defined meaning is "No errors occurred
/// and the locales were successfully created" — telling a script testing `$?`
/// that everything worked when nothing usable existed.
#[test]
fn localedef_reports_creation_unsupported_and_writes_nothing() {
    let temp_dir = TempDir::new().unwrap();
    let locale_src = create_minimal_locale(&temp_dir);
    let out = temp_dir.path().join("built_locale");

    let output = std::process::Command::new(plib::testing::get_binary_path("localedef"))
        .arg("-i")
        .arg(&locale_src)
        .arg(&out)
        .output()
        .expect("run localedef");

    assert_eq!(
        output.status.code(),
        Some(3),
        "a clean source must report creation-unsupported; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !out.exists(),
        "no locale is created, so nothing may be written to {}",
        out.display()
    );
    // 102766: "report all categories successfully processed" -- processed, not
    // created, so the report still belongs on stdout.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("LC_NUMERIC") && stdout.contains("LC_MESSAGES"),
        "the processed categories must still be reported: {stdout:?}"
    );
    // An unexplained exit 3 would be unhelpful.
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not supported"),
        "the status must be explained on stderr: {stderr:?}"
    );
}

/// Warnings accepted with `-c` reach the same answer: nothing is created, so
/// exit 1 ("warnings occurred and the locales were successfully created") is
/// unreachable and 3 is correct.
#[test]
fn localedef_creation_unsupported_applies_with_c_too() {
    let temp_dir = TempDir::new().unwrap();
    let src = temp_dir.path().join("warn.locale");
    let mut file = File::create(&src).unwrap();
    write!(
        file,
        "LC_NUMERIC\ndecimal_point \".\"\nbogus_keyword \"x\"\nEND LC_NUMERIC\n"
    )
    .unwrap();
    drop(file);
    let out = temp_dir.path().join("warn_locale");

    let output = std::process::Command::new(plib::testing::get_binary_path("localedef"))
        .arg("-c")
        .arg("-i")
        .arg(&src)
        .arg(&out)
        .output()
        .expect("run localedef");

    assert_eq!(output.status.code(), Some(3), "-c with warnings -> 3");
    assert!(!out.exists(), "nothing may be written");
}
