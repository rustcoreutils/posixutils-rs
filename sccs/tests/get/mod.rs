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
use std::path::PathBuf;
use std::process::Output;

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

/// An -i/-x option-argument is a comma-separated list whose elements may be
/// SID ranges: `<list> ::= <range> | <list> , <range>`, `<range> ::= SID |
/// SID - SID` (99085-99087). A SID never contains '-', so the separator is
/// unambiguous.
#[test]
fn get_include_accepts_a_sid_range() {
    let tmp = TempDir::new().unwrap();
    std::fs::copy(fixture_path("s.multi"), tmp.path().join("s.multi")).unwrap();

    // Baseline: excluding 1.2 and 1.3 reverts both of their changes.
    let out = super::common::run_in("get", &["-p", "-s", "-x1.2,1.3", "s.multi"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "line1\nline2\nline3\n",
        "-x must drop both named deltas"
    );

    // -i names the same two deltas as a range and wins: "forced to be
    // applied" (99084) outranks "forced not to be applied".
    let out = super::common::run_in(
        "get",
        &["-p", "-s", "-x1.2,1.3", "-i1.2-1.3", "s.multi"],
        tmp.path(),
        "",
    );
    assert!(
        out.status.success(),
        "get -i range failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "line1\nmodified-line2\nline3\nline4\n",
        "an -i range must force its whole span back in"
    );

    // A range spanning the entire history behaves the same way.
    let out = super::common::run_in(
        "get",
        &["-p", "-s", "-x1.2,1.3", "-i1.1-1.3", "s.multi"],
        tmp.path(),
        "",
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "line1\nmodified-line2\nline3\nline4\n"
    );
}

/// "A diagnostic message shall be written if the first SID in the range is not
/// an ancestor of the second SID in the range" (99090-99092). Retrieving some
/// other version instead would silently hand back text nobody asked for, so
/// the run stops.
#[test]
fn get_rejects_a_range_that_is_not_an_ancestor_chain() {
    let tmp = TempDir::new().unwrap();
    std::fs::copy(fixture_path("s.multi"), tmp.path().join("s.multi")).unwrap();

    let out = super::common::run_in("get", &["-p", "-s", "-i1.3-1.1", "s.multi"], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "a reversed range must not be retrieved anyway"
    );
    assert!(
        out.stdout.is_empty(),
        "no text may be written for a rejected range, got {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("ancestor"),
        "expected the ancestry diagnostic, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // A syntactically broken range is rejected the same way.
    let out = super::common::run_in("get", &["-p", "-s", "-ix-y", "s.multi"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1));
    assert!(out.stdout.is_empty());
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

/// The cutoff's omitted trailing units default to their *maximum*, not their
/// minimum: "-c 7502 is equivalent to -c 750228235959" (99063-99065). Getting
/// this backwards would silently exclude every delta in the named period
/// rather than including them.
///
/// The fixture's deltas are 1.1 @ 25/12/12 22:24:47, 1.2 @ 22:24:57 and
/// 1.3 @ 22:25:05, so a cutoff can separate them at second granularity.
#[test]
fn get_cutoff_omitted_units_default_to_maximum() {
    let tmp = TempDir::new().unwrap();
    std::fs::copy(fixture_path("s.multi"), tmp.path().join("s.multi")).unwrap();

    let text = |args: &[&str]| -> String {
        let out = super::common::run_in("get", args, tmp.path(), "");
        assert!(
            out.status.success(),
            "get {args:?} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        String::from_utf8_lossy(&out.stdout).into_owned()
    };

    const ALL: &str = "line1\nmodified-line2\nline3\nline4\n";
    // Year alone means the end of that year, so every delta is in.
    assert_eq!(text(&["-p", "-s", "-c25", "s.multi"]), ALL);
    assert_eq!(text(&["-p", "-s", "-c2512", "s.multi"]), ALL);
    // Down to the minute: seconds default to 59, so 22:24:57 is in and
    // 22:25:05 is out.
    assert_eq!(
        text(&["-p", "-s", "-c2512122224", "s.multi"]),
        "line1\nline2\nline3\nline4\n",
        "seconds must default to 59, keeping the 22:24:57 delta"
    );

    // "Any number of non-numeric characters may separate the various 2-digit
    // pieces" (99066-99068).
    assert_eq!(
        text(&["-p", "-s", "-c25/12/12 22:24:57", "s.multi"]),
        "line1\nline2\nline3\nline4\n",
        "a punctuated cutoff must parse the same as the bare digits"
    );

    // Two-digit years in [69,99] are 19xx (99057-99058), so 1969 predates
    // every delta and nothing is retrieved.
    assert_eq!(
        text(&["-p", "-s", "-c69", "s.multi"]),
        "",
        "-c69 means 1969, which is before the whole history"
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

/// `-m` prefixes each line with the SID of the delta that inserted it, `-n`
/// with the module name, and the two combine (module name, tab, SID, tab).
#[test]
fn get_line_prefix_options() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_in("admin", &["-i", "s.pfx"], tmp.path(), "line one\n");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-p", "-s", "-m", "s.pfx"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.1\tline one\n",
        "-m prefixes the inserting delta's SID"
    );

    let out = super::common::run_in("get", &["-p", "-s", "-n", "s.pfx"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "pfx\tline one\n",
        "-n prefixes the module name"
    );

    let out = super::common::run_in("get", &["-p", "-s", "-m", "-n", "s.pfx"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "pfx\t1.1\tline one\n",
        "-n -m prefixes the module name then the SID"
    );
}

/// `-g` suppresses retrieval: the SID is reported but no g-file is written.
/// Useful for verifying a SID exists without materializing it.
#[test]
fn get_g_suppresses_retrieval() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_in("admin", &["-i", "s.nog"], tmp.path(), "body\n");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-g", "s.nog"], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -g failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !tmp.path().join("nog").exists(),
        "-g must not create a g-file"
    );

    // A SID that does not exist is still an error under -g.
    let out = super::common::run_in("get", &["-g", "-r9.9", "s.nog"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "-g must still validate the SID");
}

/// #G8: `get -e` takes the z-file lock while it works and releases it
/// afterwards; the p-file, by contrast, persists to record the pending edit.
#[test]
fn get_edit_leaves_pfile_but_no_zfile() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_in("admin", &["-i", "s.lock"], tmp.path(), "body\n");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-e", "s.lock"], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -e failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(
        tmp.path().join("p.lock").exists(),
        "the p-file records the pending edit"
    );
    assert!(
        !tmp.path().join("z.lock").exists(),
        "the z-file lock must be released when get finishes"
    );

    // Absence after the fact is also what "the lock is never taken" looks
    // like, so prove the lock is real from the other side: a z-file left by
    // another get must block this one.
    std::fs::write(tmp.path().join("z.lock"), "1\n").unwrap();
    let out = super::common::run_in("get", &["-e", "s.lock"], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "an existing z-file must block get -e"
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("being edited"),
        "expected the lock diagnostic, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        tmp.path().join("z.lock").exists(),
        "get must not remove a z-file it did not create"
    );
}

/// #G9: a body with no `%X%` keyword draws the "No id keywords" warning, which
/// is a warning rather than an error — unless the `i` flag makes it fatal.
#[test]
fn get_no_id_keywords_warning() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_in("admin", &["-i", "s.nokw"], tmp.path(), "plain text\n");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["s.nokw"], tmp.path(), "");
    assert!(out.status.success(), "the warning must not be fatal");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("No id keywords"),
        "expected the warning, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // A body containing a keyword draws no such warning. Assert the retrieval
    // actually succeeded too: on its own, "stderr lacks this substring" is
    // equally true of a get that died before it ever looked for keywords.
    let out = super::common::run_in("admin", &["-i", "s.withkw"], tmp.path(), "%W%\ntext\n");
    assert!(out.status.success());
    let out = super::common::run_in("get", &["s.withkw"], tmp.path(), "");
    assert!(
        out.status.success(),
        "get s.withkw failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !String::from_utf8_lossy(&out.stderr).contains("No id keywords"),
        "a body with %W% must not warn: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    let g = std::fs::read_to_string(tmp.path().join("withkw")).expect("g-file");
    assert!(
        g.starts_with("@(#)withkw\t1.1") && g.ends_with("text\n"),
        "%W% must expand in the g-file, got {g:?}"
    );

    // The `i` flag turns the warning into a fatal error: exit 1, no g-file.
    let out = super::common::run_in("admin", &["-i", "-fi", "s.ikw"], tmp.path(), "plain text\n");
    assert!(
        out.status.success(),
        "admin -fi failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let out = super::common::run_in("get", &["s.ikw"], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "the i flag must make a keyword-less body fatal; stderr={:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("No id keywords"),
        "expected the fatal diagnostic, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !tmp.path().join("ikw").exists(),
        "the fatal path must not leave a g-file"
    );
}

/// Directory and `-` (stdin list) operands both expand.
#[test]
fn get_directory_and_stdin_operands() {
    // Directory operand: the s-file inside is retrieved, and the g-file lands
    // in the current directory (99186-99187), not inside the directory.
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path().join("proj");
    std::fs::create_dir(&dir).unwrap();
    let out = super::common::run_in("admin", &["-i", "proj/s.indir"], tmp.path(), "body\n");
    assert!(
        out.status.success(),
        "admin failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = super::common::run_in("get", &["proj"], tmp.path(), "");
    assert!(
        out.status.success(),
        "get <dir> failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        tmp.path().join("indir").exists(),
        "the g-file belongs in the current directory"
    );
    assert!(
        !dir.join("indir").exists(),
        "no second g-file beside the s-file"
    );
    // Expanding a directory yields a named file, so the pathname header is
    // required even though one operand was given.
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("proj/s.indir:"),
        "a directory operand must print the pathname header, got {:?}",
        String::from_utf8_lossy(&out.stdout)
    );

    // `-` reads the pathname list from stdin.
    let tmp2 = TempDir::new().unwrap();
    let out = super::common::run_in("admin", &["-i", "s.viastdin"], tmp2.path(), "body\n");
    assert!(out.status.success());
    let out = super::common::run_in("get", &["-"], tmp2.path(), "s.viastdin\n");
    assert!(
        out.status.success(),
        "get - failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(tmp2.path().join("viastdin").exists());

    // "Non-SCCS files and unreadable files shall be silently ignored" (99128).
    // A list piped from `ls`/`find` names ordinary files as a matter of
    // course; they must not produce a diagnostic or spoil the exit status.
    std::fs::write(tmp2.path().join("README"), "notes\n").unwrap();
    std::fs::remove_file(tmp2.path().join("viastdin")).unwrap();
    let out = super::common::run_in("get", &["-"], tmp2.path(), "README\ns.viastdin\n");
    assert!(
        out.status.success(),
        "a non-SCCS line must not fail the run: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !String::from_utf8_lossy(&out.stderr).contains("README"),
        "a non-SCCS line must be ignored silently, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        tmp2.path().join("viastdin").exists(),
        "the real s-file in the list is still retrieved"
    );
}

/// A partial SID names a release, and `get -r<release>` retrieves that
/// release's most recent delta — the multi-release case the single-release
/// fixtures cannot show.
#[test]
fn get_partial_sid_selects_within_a_release() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_in("admin", &["-i", "s.rel"], tmp.path(), "r1\n");
    assert!(out.status.success());

    // Build 1.2, then start release 2 to get 2.1.
    for (body, args) in [
        ("r2\n", vec!["-e", "s.rel"]),
        ("r3\n", vec!["-e", "-r2", "s.rel"]),
    ] {
        let out = super::common::run_in("get", &args, tmp.path(), "");
        assert!(
            out.status.success(),
            "get {args:?} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        std::fs::write(tmp.path().join("rel"), body).unwrap();
        let out = super::common::run_in("delta", &["-ynext", "s.rel"], tmp.path(), "");
        assert!(
            out.status.success(),
            "delta failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }

    // -r1 selects the newest delta in release 1, not the newest overall.
    let out = super::common::run_in("get", &["-p", "-s", "-r1", "s.rel"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "r2\n",
        "-r1 must select the last delta of release 1"
    );

    // No -r selects the newest overall, which is in release 2.
    let out = super::common::run_in("get", &["-p", "-s", "s.rel"], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "r3\n");

    // A release above the newest one retrieves the trunk head rather than
    // failing: mR.mL is the text, and it is release R that gets created.
    let out = super::common::run_in("get", &["-p", "-s", "-r9", "s.rel"], tmp.path(), "");
    assert!(
        out.status.success(),
        "-r9 must fall back to the trunk head: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "r3\n");
}

/// The "SID of Delta to be Created" column of the POSIX get SID table
/// (99233-99253). Each row picks between extending the retrieved delta's line
/// and branching off it, and the two are not interchangeable: minting a trunk
/// successor where the table calls for a branch silently collides with a SID
/// that already exists.
#[test]
fn get_edit_new_sid_follows_the_posix_table() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_in("admin", &["-i", "s.tbl"], tmp.path(), "r1\n");
    assert!(out.status.success());

    // Build trunk 1.1, 1.2, 2.1.
    for (body, args) in [
        ("r2\n", vec!["-e", "s.tbl"]),
        ("r3\n", vec!["-e", "-r2", "s.tbl"]),
    ] {
        super::common::run_ok("get", &args, tmp.path(), "");
        std::fs::write(tmp.path().join("tbl"), body).unwrap();
        super::common::run_ok("delta", &["-ynext", "s.tbl"], tmp.path(), "");
    }

    // `get -e -g` announces the new SID without retrieving, so each row can be
    // probed without disturbing the history — but it must still take the lock,
    // so drop the p-file between probes.
    let announced = |args: &[&str]| -> String {
        let out = super::common::run_in("get", args, tmp.path(), "");
        assert!(
            out.status.success(),
            "get {args:?} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        assert!(
            tmp.path().join("p.tbl").exists(),
            "get -e -g must still record the pending edit in the p-file"
        );
        let _ = std::fs::remove_file(tmp.path().join("p.tbl"));
        String::from_utf8_lossy(&out.stdout)
            .trim()
            .replace('\n', " ")
    };

    // R = mR: extend the trunk (99238).
    assert_eq!(
        announced(&["-e", "-g", "-r2", "s.tbl"]),
        "2.1 new delta 2.2"
    );
    // R > mR: force the first delta of a new release (99237).
    assert_eq!(
        announced(&["-e", "-g", "-r5", "s.tbl"]),
        "2.1 new delta 5.1"
    );
    // R < mR with a trunk successor in a higher release: branch (99243).
    assert_eq!(
        announced(&["-e", "-g", "-r1", "s.tbl"]),
        "1.2 new delta 1.2.1.1"
    );
    // R.L with a trunk successor: branch, not the already-taken 1.2 (99247).
    assert_eq!(
        announced(&["-e", "-g", "-r1.1", "s.tbl"]),
        "1.1 new delta 1.1.1.1"
    );
    // R.L with no trunk successor: extend the trunk (99245).
    assert_eq!(
        announced(&["-e", "-g", "-r2.1", "s.tbl"]),
        "2.1 new delta 2.2"
    );
    // No SID at all: the trunk head of the newest release (99235).
    assert_eq!(announced(&["-e", "-g", "s.tbl"]), "2.1 new delta 2.2");
}

/// A delta withdrawn by `rmdel` keeps its SID but has no text, so it must not
/// be selected as the target of a release-only `-r`.
#[test]
fn get_skips_a_removed_delta() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_in("admin", &["-i", "s.rm"], tmp.path(), "r1\n");
    assert!(out.status.success());

    super::common::run_ok("get", &["-e", "s.rm"], tmp.path(), "");
    std::fs::write(tmp.path().join("rm"), "r2\n").unwrap();
    super::common::run_ok("delta", &["-ynext", "s.rm"], tmp.path(), "");
    super::common::run_ok("rmdel", &["-r1.2", "s.rm"], tmp.path(), "");

    // -r1 must fall back to 1.1, the newest delta in release 1 that still has
    // text; selecting the removed 1.2 retrieves an empty file.
    let out = super::common::run_in("get", &["-p", "-s", "-r1", "s.rm"], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -r1 failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "r1\n",
        "a removed delta must not be retrieved"
    );
}

/// A second `get` of the same file must succeed.
///
/// `get` writes the g-file mode 0444, and then reopened that read-only file
/// with `File::create` on the next run, which fails with EACCES. Every file
/// was effectively get-once.
#[test]
fn get_can_retrieve_the_same_file_twice() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_ok("admin", &["-i", "s.twice"], tmp.path(), "one\ntwo\n");
    assert!(out.status.success());

    let first = super::common::run_in("get", &["s.twice"], tmp.path(), "");
    assert!(
        first.status.success(),
        "first get failed: {}",
        String::from_utf8_lossy(&first.stderr)
    );

    let second = super::common::run_in("get", &["s.twice"], tmp.path(), "");
    assert!(
        second.status.success(),
        "second get of the same file failed: {}",
        String::from_utf8_lossy(&second.stderr)
    );
    assert_eq!(
        std::fs::read_to_string(tmp.path().join("twice")).unwrap(),
        "one\ntwo\n",
        "the re-gotten g-file must hold the retrieved text"
    );
}

/// The same defect, for the l-file: `get -l` could only ever succeed once in a
/// given directory.
#[test]
fn get_l_can_write_the_lfile_twice() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_ok("admin", &["-i", "s.ltwice"], tmp.path(), "body\n");
    assert!(out.status.success());

    // -p keeps the g-file out of the way so only the l-file is under test.
    let first = super::common::run_in("get", &["-p", "-l", "s.ltwice"], tmp.path(), "");
    assert!(
        first.status.success(),
        "first get -l failed: {}",
        String::from_utf8_lossy(&first.stderr)
    );

    let second = super::common::run_in("get", &["-p", "-l", "s.ltwice"], tmp.path(), "");
    assert!(
        second.status.success(),
        "second get -l failed: {}",
        String::from_utf8_lossy(&second.stderr)
    );
    assert!(tmp.path().join("l.ltwice").exists(), "l-file should exist");
}

/// `get -e` acquires the z-file lock and registers it for cleanup, but never
/// installed the SIGINT handler that consults the registry, so the registry
/// was inert and ^C stranded `z.<name>`.
///
/// The block is deterministic rather than timed: `-p` writes the retrieved
/// text to standard output, and with a pipe nobody reads and a body larger
/// than the pipe buffer, `get` blocks in `write` while holding the lock.
#[test]
fn get_sigint_removes_the_zfile_lock() {
    use std::process::{Command, Stdio};

    let tmp = TempDir::new().unwrap();
    let body: String = (0..20000).map(|i| format!("line {i}\n")).collect();
    let out = super::common::run_ok("admin", &["-i", "s.sigint"], tmp.path(), &body);
    assert!(out.status.success());

    let mut child = Command::new(plib::testing::get_binary_path("get"))
        .args(["-e", "-p", "s.sigint"])
        .current_dir(tmp.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn get");

    let zfile = tmp.path().join("z.sigint");
    let mut waited = 0;
    while !zfile.exists() && waited < 5000 {
        std::thread::sleep(std::time::Duration::from_millis(10));
        waited += 10;
    }
    assert!(
        zfile.exists(),
        "get -e should hold the z-file lock while blocked writing"
    );

    unsafe { libc::kill(child.id() as libc::pid_t, libc::SIGINT) };
    let _ = child.wait().expect("wait for get");

    assert!(
        !zfile.exists(),
        "SIGINT must remove the z-file lock, not strand it"
    );
}

/// Build a 1.1/1.2/1.3 file in `dir` and return its s-file path as a string.
fn three_delta_file(dir: &std::path::Path, name: &str) -> String {
    let sfile = dir.join(format!("s.{name}"));
    let sfile_s = sfile.to_string_lossy().to_string();
    let out = super::common::run_in("admin", &["-i", &sfile_s], dir, "a\n");
    assert!(
        out.status.success(),
        "admin: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    for (n, body) in [("two", "a\nb\n"), ("three", "a\nb\nc\n")] {
        let out = super::common::run_in("get", &["-e", &sfile_s], dir, "");
        assert!(
            out.status.success(),
            "get -e: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        std::fs::write(dir.join(name), body).unwrap();
        let out = super::common::run_in("delta", &[&format!("-y{n}"), &sfile_s], dir, "");
        assert!(
            out.status.success(),
            "delta: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
    let _ = std::fs::remove_file(dir.join(name));
    sfile_s
}

/// The `d` flag names "the default delta number (SID) to be used by a get
/// command" (POSIX 84036). It was stored by admin, reported by prs, and
/// ignored by get, which always retrieved the trunk head.
#[test]
fn get_uses_the_default_sid_flag() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "dflag");

    let out = super::common::run_in("admin", &["-fd1.1", &sfile], tmp.path(), "");
    assert!(
        out.status.success(),
        "admin -fd1.1: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = super::common::run_in("get", &["-p", "-s", &sfile], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "a\n",
        "a bare get must honor the d flag"
    );

    // An explicit -r still outranks the default.
    let out = super::common::run_in("get", &["-p", "-s", "-r1.3", &sfile], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "a\nb\nc\n");
}

/// "get -e against one of these locked releases fails" (POSIX 84047-84048).
#[test]
fn get_e_refuses_a_locked_release() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "locked");

    let out = super::common::run_in("admin", &["-fl1", &sfile], tmp.path(), "");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-e", &sfile], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "get -e on a locked release must fail; stdout {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("locked"),
        "got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !tmp.path().join("p.locked").exists(),
        "a refused get -e must not leave an edit lock"
    );

    // Retrieval without -e is unaffected: the protection is on editing.
    let out = super::common::run_in("get", &["-p", "-s", &sfile], tmp.path(), "");
    assert!(out.status.success(), "plain get must still work");
}

/// `l a` locks every release, not none.
#[test]
fn get_e_refuses_every_release_when_all_are_locked() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "alll");

    let out = super::common::run_in("admin", &["-fla", &sfile], tmp.path(), "");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-e", &sfile], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "l a must lock release 1 too");
}

/// The ceiling is "the highest release ... which may be retrieved by a get
/// command for editing" (POSIX 84030-84032), enforced for -e per 99077-99078.
#[test]
fn get_e_refuses_a_release_above_the_ceiling() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "ceil");

    // Move the head to release 2, then set the ceiling below it.
    let out = super::common::run_in("get", &["-e", "-r2", &sfile], tmp.path(), "");
    assert!(
        out.status.success(),
        "get -e -r2: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    std::fs::write(tmp.path().join("ceil"), "a\nb\nc\nd\n").unwrap();
    let out = super::common::run_in("delta", &["-yrel2", &sfile], tmp.path(), "");
    assert!(
        out.status.success(),
        "delta: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = super::common::run_in("admin", &["-fc1", &sfile], tmp.path(), "");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-e", &sfile], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "release 2 is above a ceiling of 1; stdout {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
    let out = super::common::run_in("get", &["-p", "-s", &sfile], tmp.path(), "");
    assert!(
        out.status.success(),
        "plain get is not gated by the ceiling"
    );
}

/// The floor is the lowest release retrievable for editing (84033-84035).
#[test]
fn get_e_refuses_a_release_below_the_floor() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "floor");

    let out = super::common::run_in("admin", &["-ff2", &sfile], tmp.path(), "");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-e", &sfile], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "release 1 is below a floor of 2; stdout {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
}

/// "SCCS file protection specified via the ceiling, floor, and authorized user
/// list stored in the SCCS file shall be enforced when the -e option is used"
/// (POSIX 99077-99078). The list was written by admin -a, reported by prs, and
/// consulted by nobody.
#[test]
fn get_e_refuses_a_user_not_on_the_authorized_list() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "auth");

    let out = super::common::run_in("admin", &["-asomeoneelse", &sfile], tmp.path(), "");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-e", &sfile], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "an unlisted user must not be able to start an edit; stdout {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("not authorized"),
        "got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !tmp.path().join("p.auth").exists(),
        "a refused get -e must not leave an edit lock"
    );

    let out = super::common::run_in("get", &["-p", "-s", &sfile], tmp.path(), "");
    assert!(
        out.status.success(),
        "plain get is not gated by the user list"
    );
}

/// "If login or group ID is preceded by a '!', the users so specified shall be
/// denied permission to make deltas" (POSIX 84089-84090).
#[test]
fn get_e_refuses_a_user_denied_with_a_bang() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "bang");
    let me = plib::sccsfile::real_login_name();

    let out = super::common::run_in("admin", &[&format!("-a!{me}"), &sfile], tmp.path(), "");
    assert!(out.status.success());

    let out = super::common::run_in("get", &["-e", &sfile], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "an explicitly denied user must be refused; stdout {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
}

/// A user named on the list may edit, and the numeric-group-ID form means
/// every login in that group (POSIX 84085-84086).
#[test]
fn get_e_allows_a_listed_user_and_a_listed_group() {
    let tmp = TempDir::new().unwrap();
    let me = plib::sccsfile::real_login_name();

    let sfile = three_delta_file(tmp.path(), "byname");
    let out = super::common::run_in("admin", &[&format!("-a{me}"), &sfile], tmp.path(), "");
    assert!(out.status.success());
    let out = super::common::run_in("get", &["-e", &sfile], tmp.path(), "");
    assert!(
        out.status.success(),
        "a listed user must be allowed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let gid = unsafe { libc::getgid() };
    let sfile = three_delta_file(tmp.path(), "bygroup");
    let out = super::common::run_in("admin", &[&format!("-a{gid}"), &sfile], tmp.path(), "");
    assert!(out.status.success());
    let out = super::common::run_in("get", &["-e", &sfile], tmp.path(), "");
    assert!(
        out.status.success(),
        "a listed group must admit its members: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// In `-i`/`-x`, a bare number is a SID, not a serial number.
///
/// Sharing one list resolver between `get -i`/`-x` and `delta -g` brought
/// delta's historical bare-serial convenience with it, so `get -x2` silently
/// excluded delta 1.2 -- serial 2 -- and handed back a different version than
/// the user asked for, with no diagnostic. CSSC treats `-x2` as naming no
/// delta.
#[test]
fn get_x_with_a_bare_number_names_a_sid_not_a_serial() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "bare");

    let out = super::common::run_in("get", &["-p", "-s", "-x2", &sfile], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "a\nb\nc\n",
        "-x2 names no delta, so nothing is excluded"
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("no such delta"),
        "an unresolvable token must be diagnosed, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // The SID form still works.
    let out = super::common::run_in("get", &["-p", "-s", "-x1.2", &sfile], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "a\nc\n");
}

/// `delta -g` keeps the bare-serial form it has always accepted.
#[test]
fn delta_g_still_accepts_a_bare_serial_number() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "gbare");

    let out = super::common::run_in("get", &["-e", &sfile], tmp.path(), "");
    assert!(out.status.success());
    std::fs::write(tmp.path().join("gbare"), "a\nb\nc\nd\n").unwrap();

    let out = super::common::run_in("delta", &["-g2", "-yg", &sfile], tmp.path(), "");
    assert!(
        out.status.success(),
        "delta -g2 should accept a serial: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let out = super::common::run_in("prs", &["-d:Dg:", "-r1.4", &sfile], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "2");
}

/// `get` must replace the g-file, never write through a symlink standing in
/// its place.
///
/// The original `File::create` followed one, so anyone who could plant
/// `./f` as a symlink could have `get f` overwrite the target with the
/// retrieved text -- and `sccs` may be installed setuid. Replacing by rename
/// leaves the pointed-at file alone.
#[test]
fn get_replaces_a_gfile_symlink_instead_of_following_it() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_ok("admin", &["-i", "s.link"], tmp.path(), "retrieved\n");
    assert!(out.status.success());

    let victim = tmp.path().join("victim");
    std::fs::write(&victim, "ORIGINAL\n").unwrap();
    std::os::unix::fs::symlink(&victim, tmp.path().join("link")).unwrap();

    let out = super::common::run_in("get", &["s.link"], tmp.path(), "");
    assert!(
        out.status.success(),
        "get failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert_eq!(
        std::fs::read_to_string(&victim).unwrap(),
        "ORIGINAL\n",
        "get must not write through the symlink"
    );
    assert_eq!(
        std::fs::read_to_string(tmp.path().join("link")).unwrap(),
        "retrieved\n",
        "the g-file itself holds the retrieved text"
    );
    assert!(
        !tmp.path()
            .join("link")
            .symlink_metadata()
            .unwrap()
            .is_symlink(),
        "the symlink is replaced by the g-file"
    );
}
