//
// Copyright (c) 2025-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::common::run_test_with_checker;
use plib::testing::TestPlan;
use std::path::PathBuf;
use std::process::Output;

fn test_file_path(file_name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push(file_name);
    path
}

#[test]
fn single_identification() {
    let file_path = test_file_path("single_identification.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            output.status.success(),
            "what should succeed when matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("single_identification.txt:"),
            "should show filename"
        );
        assert!(
            stdout.contains("\tsingle_identification\n"),
            "should show identification with tab"
        );
    });
}

#[test]
fn multiple_identifications() {
    let file_path = test_file_path("multiple_identifications.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            output.status.success(),
            "what should succeed when matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("multiple_identifications.txt:"),
            "should show filename"
        );
        assert!(
            stdout.contains("\tfirst_identification\n"),
            "should show first identification"
        );
        assert!(
            stdout.contains("\tsecond_identification\n"),
            "should show second identification"
        );
    });
}

#[test]
fn single_identification_flag() {
    let file_path = test_file_path("single_identification_flag.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec!["-s".into(), file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            output.status.success(),
            "what should succeed when matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("single_identification_flag.txt:"),
            "should show filename"
        );
        assert!(
            stdout.contains("\tfirst_identification\n"),
            "should show first identification"
        );
        // With -s, should only show one match
        assert!(
            !stdout.contains("second_identification"),
            "should not show second with -s"
        );
    });
}

#[test]
fn no_identification() {
    let file_path = test_file_path("no_identification.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1, // No matches found
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            !output.status.success(),
            "what should fail when no matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should still print filename even with no matches
        assert!(
            stdout.contains("no_identification.txt:"),
            "should show filename"
        );
    });
}

#[test]
fn empty_file() {
    let file_path = test_file_path("empty_file.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1, // No matches found
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            !output.status.success(),
            "what should fail when no matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("empty_file.txt:"), "should show filename");
    });
}

#[test]
fn special_characters() {
    let file_path = test_file_path("special_characters.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            output.status.success(),
            "what should succeed when matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("special_characters.txt:"),
            "should show filename"
        );
        assert!(stdout.contains("\tspecial\n"), "should show special");
        assert!(stdout.contains("\tanother\n"), "should show another");
        assert!(stdout.contains("\tback\n"), "should show back");
        assert!(stdout.contains("\tnull\n"), "should show null");
    });
}

#[test]
fn what_binary_input_does_not_abort() {
    // Regression for the UTF-8 abort bug: `what` must scan arbitrary binary
    // input (its primary use case) and never error out on non-UTF-8 bytes.
    use std::io::Write;
    let dir = tempfile::TempDir::new().unwrap();
    let path = dir.path().join("bin.o");
    // Non-UTF-8 bytes (0xff, 0xfe) surrounding a valid @(#) identification.
    let mut data = vec![0xffu8, 0x00, 0xfe, b'x'];
    data.extend_from_slice(b"@(#)ident-here\" trailing");
    data.extend_from_slice(&[0x80, 0x81, 0xff]);
    std::fs::File::create(&path)
        .unwrap()
        .write_all(&data)
        .unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("what"),
            args: vec![path.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(
                output.status.success(),
                "what must exit 0 on a binary file containing an ident"
            );
            let out = String::from_utf8_lossy(&output.stdout);
            assert!(
                out.contains("\tident-here"),
                "expected the @(#) ident, got: {out:?}"
            );
        },
    );
}

/// Write a scratch file under a caller-owned temp dir.
fn scratch(dir: &tempfile::TempDir, name: &str, bytes: &[u8]) -> PathBuf {
    let p = dir.path().join(name);
    std::fs::write(&p, bytes).unwrap();
    p
}

fn what_run(args: &[&str]) -> Output {
    std::process::Command::new(plib::testing::get_binary_path("what"))
        .args(args)
        .output()
        .expect("run what")
}

/// `@(#)` is not itself a terminator, so two patterns on one line yield a
/// single identification string running to the newline. The terminator set is
/// exactly `"`, `>`, <newline>, `\`, <NUL>, EOF (spec 122786-122788).
#[test]
fn two_patterns_on_one_line_are_one_identification() {
    let td = tempfile::tempdir().unwrap();
    let f = scratch(&td, "two.txt", b"x@(#)first@(#)second\n");

    let out = what_run(&[f.to_str().unwrap()]);
    assert!(out.status.success());
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        format!("{}:\n\tfirst@(#)second\n", f.display()),
        "the ident runs to the newline; @(#) does not end it"
    );
}

/// After a terminator, the search resumes looking for the next `@(#)`
/// (122788-122789), so two idents on separate lines are both reported.
#[test]
fn search_resumes_after_a_terminator() {
    let td = tempfile::tempdir().unwrap();
    let f = scratch(&td, "multi.txt", b"@(#)one\n@(#)two\n");

    let out = what_run(&[f.to_str().unwrap()]);
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        format!("{}:\n\tone\n\ttwo\n", f.display())
    );
}

/// `-s` writes "at most one identification string for each file" and then
/// stops reading *that* file, recommencing at the next operand
/// (122793-122795). The existing `-s` test uses a single operand, so the
/// per-file (rather than per-run) scope was never shown.
#[test]
fn silent_stops_per_file_not_per_run() {
    let td = tempfile::tempdir().unwrap();
    let a = scratch(&td, "a.txt", b"@(#)alpha\n@(#)alpha2\n");
    let b = scratch(&td, "b.txt", b"@(#)beta\n@(#)beta2\n");

    let out = what_run(&["-s", a.to_str().unwrap(), b.to_str().unwrap()]);
    assert!(out.status.success());
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        format!("{}:\n\talpha\n{}:\n\tbeta\n", a.display(), b.display()),
        "-s must yield one ident per file, not one for the whole run"
    );
}

/// STDIN is "Not used" (122800-122801), so `-` is an ordinary pathname and
/// must be reported as an unopenable file rather than read from stdin.
#[test]
fn dash_operand_is_a_filename_not_stdin() {
    let out = what_run(&["-"]);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        output_names_dash(&out),
        "the `-` operand should be diagnosed as a file: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
}

fn output_names_dash(out: &Output) -> bool {
    String::from_utf8_lossy(&out.stderr).contains('-')
}

/// Exit status is 0 if any match was found anywhere, 1 otherwise
/// (122822-122826) — it is not per-file. A run mixing a matching and a
/// non-matching file therefore exits 0, and the non-matching file still gets
/// its header line.
#[test]
fn exit_status_is_zero_when_any_file_matches() {
    let td = tempfile::tempdir().unwrap();
    let hit = scratch(&td, "hit.txt", b"@(#)alpha\n");
    let miss = scratch(&td, "miss.txt", b"nothing here\n");

    let mixed = what_run(&[hit.to_str().unwrap(), miss.to_str().unwrap()]);
    assert_eq!(mixed.status.code(), Some(0), "any match means exit 0");
    assert_eq!(
        String::from_utf8_lossy(&mixed.stdout),
        format!("{}:\n\talpha\n{}:\n", hit.display(), miss.display()),
        "a file with no match still gets its header"
    );

    let none = what_run(&[miss.to_str().unwrap()]);
    assert_eq!(
        none.status.code(),
        Some(1),
        "no match anywhere means exit 1"
    );
}

/// "The input files shall be of any file type" (122803). An ELF object is the
/// case `what` exists for; the ident is embedded via a string literal so the
/// fixture is deterministic rather than depending on incidental bytes.
#[test]
fn finds_identification_in_an_elf_object() {
    let td = tempfile::tempdir().unwrap();
    let c = td.path().join("ident.c");
    std::fs::write(
        &c,
        "const char sccsid[] = \"@(#)elf_fixture_1.7\";\nint unused(void){return 0;}\n",
    )
    .unwrap();
    let o = td.path().join("ident.o");

    let compiled = std::process::Command::new("cc")
        .args(["-c", "-o", o.to_str().unwrap(), c.to_str().unwrap()])
        .status();
    match compiled {
        Ok(s) if s.success() => {}
        _ => return, // no working cc here; nothing to assert against
    }

    let out = what_run([o.to_str().unwrap()].as_ref());
    assert!(out.status.success(), "what must scan a binary successfully");
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("elf_fixture_1.7"),
        "the embedded ident must be found in the .o, got {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
}
