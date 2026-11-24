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
    // -L writes the delta summary table to standard output. The g-file is
    // still retrieved, so the fixture is read from a temporary directory:
    // run beside the fixture, the g-file landed in the source tree.
    let tmp = TempDir::new().unwrap();
    std::fs::copy(fixture_path("s.multi"), tmp.path().join("s.multi")).unwrap();
    let out = super::common::run_in("get", &["-L", "s.multi"], tmp.path(), "");
    assert!(out.status.success(), "{out:?}");
    let stdout = String::from_utf8_lossy(&out.stdout);
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
    assert!(
        tmp.path().join("multi").exists(),
        "the g-file is retrieved too"
    );
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
    let me = plib::sccsfile::real_login_name().unwrap();

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
    let me = plib::sccsfile::real_login_name().unwrap();

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
/// the user asked for, with no diagnostic. `-x2` names no delta, and a list
/// that names no delta is refused.
#[test]
fn get_x_with_a_bare_number_names_a_sid_not_a_serial() {
    let tmp = TempDir::new().unwrap();
    let sfile = three_delta_file(tmp.path(), "bare");

    let out = super::common::run_in("get", &["-p", "-s", "-x2", &sfile], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(out.stdout.is_empty(), "-x2 names no delta: {out:?}");
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

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn option_argument_may_begin_with_hyphen() {
    for opt in ["-r", "-c", "-i", "-x"] {
        plib::testing::assert_hyphen_option_argument("get", &[opt, "-zq", "--help"]);
    }
}

// A failed write of the retrieved text to standard output is "get: write
// error: REASON", not an error against the s-file.
#[cfg(target_os = "linux")]
#[test]
fn get_write_error_on_full_device() {
    let sfile = fixture_path("s.simple");
    let sfile = sfile.to_str().unwrap();
    plib::testing::assert_write_error_message_on_full_device("get", &["-p", "-s", sfile], b"", 1);
    plib::testing::assert_write_error_message_on_full_device(
        "get",
        &["-p", "-s", sfile, sfile],
        b"",
        1,
    );
}

// An SCCS file that cannot be read is reported with the system's text,
// without Rust's "(os error N)" suffix.
#[test]
fn missing_sccs_file_names_the_system_error() {
    let dir = plib::tmp::tempdir().unwrap();
    let sfile = dir.path().join("s.missing");
    let sfile = sfile.to_str().unwrap();
    let reason = plib::diag::io_error_text(&std::fs::File::open(sfile).unwrap_err());
    let out = plib::testing::run_test_base("get", &[sfile.to_string()], b"");
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!("get: {sfile}: I/O error: {reason}\n")
    );
}

/// A body line ending in CR (a CRLF text file under SCCS) is retrieved with
/// its CR: the line ends at the newline, and the CR before it is text.
#[test]
fn get_keeps_carriage_returns() {
    let tmp = TempDir::new().unwrap();
    super::common::write_sfile(
        tmp.path(),
        "s.crlf",
        b"\x01s 00002/00000/00000\n\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01e\n\
\x01u\n\x01U\n\x01t\n\x01T\n\x01I 1\na %I%\r\nb\r\n\x01E 1\n",
    );
    let out = super::common::run_in("get", &["-p", "-k", "-s", "s.crlf"], tmp.path(), "");
    assert!(out.status.success(), "{out:?}");
    assert_eq!(out.stdout, b"a %I%\r\nb\r\n");
}

/// A body line that is not UTF-8 is retrieved byte for byte, keywords and
/// all.
#[test]
fn get_retrieves_non_utf8_text() {
    let tmp = TempDir::new().unwrap();
    super::common::write_sfile(tmp.path(), "s.l1", super::common::LATIN1_SFILE);
    let out = super::common::run_in("get", &["-p", "-s", "s.l1"], tmp.path(), "");
    assert!(out.status.success(), "{out:?}");
    assert_eq!(out.stdout, b"caf\xe9 1.1\nline2\n");
}

/// Keywords expand in one pass: a keyword's value is not itself expanded,
/// so an `m` flag of `mod%R%` yields `mod%R%` for `%M%`, as CSSC writes it.
#[test]
fn get_expands_keywords_in_one_pass() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_in(
        "admin",
        &["-i", "-fmmod%R%", "-fq%I%", "s.kw"],
        tmp.path(),
        "m=%M% q=%Q% w=%W%\n",
    );
    assert!(out.status.success(), "{out:?}");
    let out = super::common::run_in("get", &["-p", "-s", "s.kw"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "m=mod%R% q=%I% w=@(#)mod%R%\t1.1\n"
    );
}

/// Blocks in an s-file need not nest: a delete may open in one delta's
/// insert and end in another's, and `^AE n` ends block n, not whichever block
/// opened last. Every version of such a file -- CSSC writes them -- is
/// retrieved as it was stored.
#[test]
fn get_reads_overlapping_blocks() {
    let tmp = TempDir::new().unwrap();
    super::common::write_sfile(tmp.path(), "s.o", super::common::OVERLAP_SFILE);
    for (rev, want) in [
        ("-r1.1", "a\nb\n"),
        ("-r1.2", "a\nb\nc\nd\n"),
        ("-r1.3", "a\nd\n"),
    ] {
        let out = super::common::run_in("get", &["-p", "-k", "-s", rev, "s.o"], tmp.path(), "");
        assert!(out.status.success(), "{rev}: {out:?}");
        assert_eq!(String::from_utf8_lossy(&out.stdout), want, "get {rev}");
    }
    // -m names the delta that inserted each line.
    let out = super::common::run_in(
        "get",
        &["-p", "-k", "-s", "-m", "-r1.2", "s.o"],
        tmp.path(),
        "",
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.1\ta\n1.1\tb\n1.2\tc\n1.2\td\n"
    );
    let out = super::common::run_in("val", &["s.o"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(0), "{out:?}");
}

/// Of the blocks open around a line, the one with the highest serial that
/// has a say decides whether the line is in the version: an applied insert
/// keeps it, an insert not applied or an applied delete drops it, and a
/// delete not applied has no say. That is historical SCCS's rule, and
/// CSSC's; ours was "every open insert applied and no open delete applied",
/// which gives another answer as soon as blocks of several deltas are open
/// at once. The expected text and inserting SIDs are CSSC's.
#[test]
fn get_keeps_a_line_by_the_newest_open_block() {
    let tmp = TempDir::new().unwrap();
    let get = |file: &str, args: &[&str]| -> String {
        let mut argv = vec!["-p", "-k", "-s"];
        argv.extend_from_slice(args);
        argv.push(file);
        let out = super::common::run_in("get", &argv, tmp.path(), "");
        assert!(out.status.success(), "get {args:?} {file}: {out:?}");
        String::from_utf8_lossy(&out.stdout).replace('\n', ",")
    };

    // Delta 3 inserted a line inside a block delta 2 deleted.
    super::common::write_weave(
        tmp.path(),
        "s.b",
        b"\x01I 1\na\n\x01D 2\nb\n\x01I 3\nc\n\x01E 3\nd\n\x01E 2\ne\n\x01E 1\n",
    );
    assert_eq!(get("s.b", &["-r1.3"]), "a,c,e,");
    assert_eq!(get("s.b", &["-r1.3", "-m"]), "1.1\ta,1.3\tc,1.1\te,");
    assert_eq!(get("s.b", &["-r1.2"]), "a,e,");
    assert_eq!(get("s.b", &["-r1.3", "-x1.2"]), "a,b,c,d,e,");
    assert_eq!(get("s.b", &["-r1.3", "-x1.1"]), "c,");

    // Delta 2 inserted a line inside delta 1's block; with 1 excluded, the
    // line delta 2 inserted is still there.
    super::common::write_weave(
        tmp.path(),
        "s.a",
        b"\x01I 1\na\n\x01I 2\nb\n\x01E 2\nc\n\x01E 1\n",
    );
    assert_eq!(get("s.a", &["-r1.3"]), "a,b,c,");
    assert_eq!(get("s.a", &["-r1.3", "-x1.1"]), "b,");
    assert_eq!(get("s.a", &["-r1.3", "-x1.1", "-m"]), "1.2\tb,");
    assert_eq!(get("s.a", &["-r1.3", "-x1.2"]), "a,c,");
}

/// `-i` applies the deltas it names and no others: including 1.3 into 1.1
/// does not bring 1.2 with it. (It used to pull in the whole chain of
/// ancestors of each included delta.) The expected text is CSSC's.
#[test]
fn get_i_includes_only_the_deltas_named() {
    let tmp = TempDir::new().unwrap();
    let get = |file: &str, args: &[&str]| -> String {
        let mut argv = vec!["-p", "-k", "-s"];
        argv.extend_from_slice(args);
        argv.push(file);
        let out = super::common::run_in("get", &argv, tmp.path(), "");
        assert!(out.status.success(), "get {args:?} {file}: {out:?}");
        String::from_utf8_lossy(&out.stdout).replace('\n', ",")
    };
    // 1.2 inserted b inside 1.1's block; 1.3 changed nothing.
    super::common::write_weave(
        tmp.path(),
        "s.a",
        b"\x01I 1\na\n\x01I 2\nb\n\x01E 2\nc\n\x01E 1\n",
    );
    assert_eq!(get("s.a", &["-r1.1", "-i1.3"]), "a,c,");
    assert_eq!(get("s.a", &["-r1.1", "-i1.2"]), "a,b,c,");

    // 1.2 deleted b..d; 1.3 inserted c inside that.
    super::common::write_weave(
        tmp.path(),
        "s.b",
        b"\x01I 1\na\n\x01D 2\nb\n\x01I 3\nc\n\x01E 3\nd\n\x01E 2\ne\n\x01E 1\n",
    );
    assert_eq!(get("s.b", &["-r1.1", "-i1.3"]), "a,b,c,d,e,");
    assert_eq!(get("s.b", &["-r1.1", "-i1.2"]), "a,e,");
    // Named by both, a delta is applied: -i outranks -x.
    assert_eq!(get("s.b", &["-r1.3", "-x1.2", "-i1.2"]), "a,c,e,");
}

/// A body whose block structure is broken cannot be read reliably: which
/// lines belong to a version depends on which blocks are open. Historical
/// SCCS treats it as a format error in every command that reads the body,
/// and so do get, prs :GB:, delta and rmdel here, instead of reading the
/// text under whatever blocks happened to be open.
#[test]
fn a_damaged_weave_is_refused_by_every_reader() {
    let tmp = TempDir::new().unwrap();
    let bodies: [(&str, &[u8]); 7] = [
        (
            "unknown control line",
            b"\x01I 1\nbody\n\x01X junk\n\x01E 1\n",
        ),
        ("marker without a serial", b"\x01I x\nbody\n\x01E x\n"),
        (
            "end of a block not open",
            b"\x01I 1\nbody\n\x01E 2\n\x01E 1\n",
        ),
        ("block never closed", b"\x01I 1\nbody\n"),
        ("block closed twice", b"\x01I 1\nbody\n\x01E 1\n\x01E 1\n"),
        (
            "block opened twice",
            b"\x01I 1\n\x01I 1\nbody\n\x01E 1\n\x01E 1\n",
        ),
        (
            "serial of no delta",
            b"\x01I 1\nbody\n\x01D 9\nx\n\x01E 9\n\x01E 1\n",
        ),
    ];
    for (what, body) in bodies {
        let s = super::common::write_weave(tmp.path(), "s.w", body);
        let refused = |cmd: &str, args: &[&str]| {
            let out = super::common::run_in(cmd, args, tmp.path(), "");
            assert_eq!(
                out.status.code(),
                Some(1),
                "{what}: {cmd} {args:?}: {out:?}"
            );
            assert!(!out.stderr.is_empty(), "{what}: {cmd} must say why");
        };
        refused("get", &["-p", "-s", "s.w"]);
        refused("get", &["-p", "-s", "-m", "s.w"]);
        refused("prs", &["-d:GB:", "s.w"]);
        let before = std::fs::read(&s).unwrap();
        refused("rmdel", &["-r1.3", "s.w"]);
        assert_eq!(
            std::fs::read(&s).unwrap(),
            before,
            "{what}: rmdel wrote the file"
        );
        // What does not read the body is not affected.
        let out = super::common::run_in("prs", &["-d:I:", "s.w"], tmp.path(), "");
        assert!(
            out.status.success(),
            "{what}: prs without the body: {out:?}"
        );
        std::fs::remove_file(&s).unwrap();
    }
}

/// `get -e` takes the lock before it opens the s-file, as every command that
/// changes SCCS state does: what it decides (the new SID, whether the edit
/// is allowed) must come from the file as it is while it holds the lock. It
/// used to read the file first and lock afterwards, so a delta committed in
/// between was never seen.
///
/// The order shows when the lock is held by someone else and the s-file
/// cannot be parsed: a command that locks first reports the lock.
#[test]
fn get_e_locks_before_it_reads_the_s_file() {
    let tmp = TempDir::new().unwrap();
    std::fs::write(tmp.path().join("s.f"), b"not an s-file at all\n").unwrap();
    std::fs::write(tmp.path().join("z.f"), b"held").unwrap();
    let out = super::common::run_in("get", &["-e", "s.f"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("being edited"),
        "the lock should be reported, not the file: {out:?}"
    );
    assert!(
        tmp.path().join("z.f").exists(),
        "someone else's lock is left alone"
    );

    // A plain get changes nothing, takes no lock, and reads the file.
    let out = super::common::run_in("get", &["-p", "s.f"], tmp.path(), "");
    assert!(
        !String::from_utf8_lossy(&out.stderr).contains("being edited"),
        "{out:?}"
    );
}

/// The `-i` and `-x` lists of an edit are recorded in the p-file for delta,
/// one line per edit. A list is SIDs and ranges separated by commas (blanks
/// or a newline are accepted too); it is recorded with commas, so that what
/// delta reads back is what get resolved, on one line: recorded as given, a
/// newline cut the p-file line in two and left the p-file unreadable to
/// every command. A list holding anything else is refused, and nothing is
/// recorded.
#[test]
fn get_e_records_only_a_well_formed_list() {
    let tmp = TempDir::new().unwrap();
    let run =
        |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, tmp.path(), stdin);
    assert!(run("admin", &["-i", "-fj", "s.l"], "one\n")
        .status
        .success());
    for text in ["two\n", "three\n"] {
        assert!(run("get", &["-e", "-s", "s.l"], "").status.success());
        std::fs::write(tmp.path().join("l"), text).unwrap();
        assert!(run("delta", &["-s", "-yc", "s.l"], "").status.success());
    }

    for bad in ["1.2;rm", "", "1.2 -x1.1", "1.2\n-junk"] {
        let out = run("get", &["-e", "-s", "-g", "-x", bad, "s.l"], "");
        assert_eq!(out.status.code(), Some(1), "-x {bad:?}: {out:?}");
        assert!(!out.stderr.is_empty(), "-x {bad:?} must say why");
        assert!(
            !tmp.path().join("p.l").exists(),
            "-x {bad:?} recorded an edit"
        );
    }

    // SIDs separated by a blank and a newline are one list, recorded with
    // commas.
    let out = run("get", &["-e", "-s", "-g", "-x", "1.1 1.2\n1.3", "s.l"], "");
    assert!(out.status.success(), "{out:?}");
    let pfile = std::fs::read_to_string(tmp.path().join("p.l")).unwrap();
    assert_eq!(pfile.lines().count(), 1, "{pfile:?}");
    assert!(pfile.trim_end().ends_with(" -x1.1,1.2,1.3"), "{pfile:?}");
    // And every command still reads the p-file.
    let out = run("sact", &["s.l"], "");
    assert!(out.status.success(), "{out:?}");
}

/// An edit that cannot be given its g-file is not left pending. `get -e`
/// records the edit in the p-file before it retrieves the text; when the
/// retrieval then failed -- a damaged body, a g-file that cannot be made --
/// the entry stayed, with no g-file to go with it, and every later `get -e`
/// was refused until someone ran `unget`.
#[test]
fn get_e_that_fails_leaves_no_pending_edit() {
    use std::os::unix::fs::PermissionsExt;
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |args: &[&str], cwd: &std::path::Path| super::common::run_in("get", args, cwd, "");

    // A body whose first block is never closed.
    super::common::write_weave(dir, "s.bad", b"\x01I 1\none\n");
    let out = run(&["-e", "s.bad"], dir);
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(out.stdout.is_empty(), "no new delta is announced: {out:?}");
    assert!(!dir.join("p.bad").exists(), "no edit is pending");
    assert!(!dir.join("bad").exists() && !dir.join("z.bad").exists());

    // A sound file, in a directory where the g-file cannot be created.
    super::common::write_weave(dir, "s.ok", b"\x01I 1\none\n\x01E 1\n");
    let cwd = dir.join("ro");
    std::fs::create_dir(&cwd).unwrap();
    std::fs::set_permissions(&cwd, std::fs::Permissions::from_mode(0o555)).unwrap();
    // (Root creates files anywhere; the case cannot arise for it.)
    if std::fs::write(cwd.join("probe"), b"").is_err() {
        let out = run(&["-e", "-s", "../s.ok"], &cwd);
        assert_eq!(out.status.code(), Some(1), "{out:?}");
        assert!(!dir.join("p.ok").exists(), "no edit is pending: {out:?}");
    }
    std::fs::set_permissions(&cwd, std::fs::Permissions::from_mode(0o755)).unwrap();

    // The same edit, where it can be made, is pending afterwards.
    assert!(run(&["-e", "-s", "s.ok"], dir).status.success());
    assert!(dir.join("p.ok").exists());
}

/// A g-file that is writable holds someone's work: `get` does not replace
/// it. (SCCS: "writable `f' exists".) It used to be overwritten in silence,
/// by a plain `get` and by `get -e` alike. `-p` and `-g` write no g-file and
/// are not affected; a read-only g-file is a previous retrieval and is
/// replaced.
#[test]
fn get_does_not_replace_a_writable_g_file() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "-fj", "s.w"], "one\n")
        .status
        .success());

    assert!(run("get", &["-s", "s.w"], "").status.success());
    assert!(
        run("get", &["-s", "s.w"], "").status.success(),
        "read-only: replaced"
    );
    assert!(run("get", &["-e", "-s", "s.w"], "").status.success());
    std::fs::write(dir.join("w"), "my edits\n").unwrap();

    for args in [
        &["-s", "s.w"][..],
        &["-e", "-s", "s.w"],
        &["-k", "-s", "s.w"],
    ] {
        let out = run("get", args, "");
        assert_eq!(out.status.code(), Some(1), "{args:?}: {out:?}");
        assert!(
            String::from_utf8_lossy(&out.stderr).contains("writable"),
            "{out:?}"
        );
        assert_eq!(
            std::fs::read(dir.join("w")).unwrap(),
            b"my edits\n",
            "{args:?}"
        );
    }
    let pfile = std::fs::read_to_string(dir.join("p.w")).unwrap();
    assert_eq!(pfile.lines().count(), 1, "the refused edit is not pending");

    let out = run("get", &["-p", "-s", "s.w"], "");
    assert_eq!(out.stdout, b"one\n");
    assert!(run("get", &["-e", "-g", "-s", "s.w"], "").status.success());
    assert_eq!(std::fs::read(dir.join("w")).unwrap(), b"my edits\n");
}

/// With the `j` flag several edits of one delta may be pending, and each
/// is given a SID of its own: the first the next on the line, the others
/// branches (as SCCS and CSSC do, "creating a branch due to concurrent
/// edit"). They were all given the same SID; the first delta then removed
/// every one of the p-file's identical entries, and the other edits were no
/// longer pending.
#[test]
fn joint_edits_of_one_delta_get_different_sids() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "-fj", "s.j"], "one\n")
        .status
        .success());

    let mut announced = Vec::new();
    for _ in 0..3 {
        let out = run("get", &["-e", "-g", "s.j"], "");
        assert!(out.status.success(), "{out:?}");
        announced.push(String::from_utf8(out.stdout).unwrap());
    }
    assert_eq!(
        announced,
        [
            "1.1\nnew delta 1.2\n",
            "1.1\nnew delta 1.1.1.1\n",
            "1.1\nnew delta 1.1.2.1\n"
        ]
    );
    let pending = |dir: &std::path::Path| -> Vec<String> {
        std::fs::read_to_string(dir.join("p.j"))
            .unwrap()
            .lines()
            .map(|l| l.split(' ').nth(1).unwrap().to_string())
            .collect()
    };
    assert_eq!(pending(dir), ["1.2", "1.1.1.1", "1.1.2.1"]);

    // One delta settles one edit; the others stay pending.
    std::fs::write(dir.join("j"), "two\n").unwrap();
    let out = run("delta", &["-s", "-r1.1.1.1", "-yc", "s.j"], "");
    assert!(out.status.success(), "{out:?}");
    assert_eq!(pending(dir), ["1.2", "1.1.2.1"]);
}

/// A list that names a delta the file does not have is an error, for get's
/// `-i` and `-x` and for delta's `-g`: the version asked for cannot be
/// made, and nothing is retrieved, recorded or committed. Each said "no such
/// delta" and went on with the rest of the list, exit status 0 -- `get -e`
/// recording in the p-file a list that delta would then refuse for good.
#[test]
fn a_list_naming_no_delta_is_an_error() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "s.m"], "one\n").status.success());
    let refused = |out: std::process::Output, what: &str| {
        assert_eq!(out.status.code(), Some(1), "{what}: {out:?}");
        assert!(
            String::from_utf8_lossy(&out.stderr).contains("9.9"),
            "{what}: {out:?}"
        );
        assert!(out.stdout.is_empty(), "{what}: {out:?}");
    };

    refused(run("get", &["-p", "-s", "-i9.9", "s.m"], ""), "get -i");
    refused(run("get", &["-p", "-s", "-x1.1,9.9", "s.m"], ""), "get -x");
    refused(run("get", &["-e", "-i9.9", "s.m"], ""), "get -e -i");
    assert!(!dir.join("p.m").exists() && !dir.join("m").exists());

    assert!(run("get", &["-e", "-s", "s.m"], "").status.success());
    std::fs::write(dir.join("m"), "two\n").unwrap();
    refused(run("delta", &["-yc", "-g9.9", "s.m"], ""), "delta -g");
    assert!(dir.join("p.m").exists(), "the edit is still pending");
    let out = run("prs", &["-e", "-d:I:", "s.m"], "");
    assert_eq!(out.stdout, b"1.1\n");
}

/// Without the `j` flag one edit of a delta may be pending: get -e
/// "shall prevent further gets for editing on the same SID until delta is
/// executed or the j (joint edit) flag is set" (POSIX get, -e). Only an
/// edit that would come to the same *new* SID was refused, so `-b` or a
/// higher `-r` got a second and third edit of the same delta through.
#[test]
fn a_delta_being_edited_is_not_edited_again_without_j() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "-fb", "s.e"], "one\n")
        .status
        .success());
    assert!(run("get", &["-e", "-g", "-s", "s.e"], "").status.success());
    for args in [
        &["-e", "-g", "s.e"][..],
        &["-e", "-g", "-b", "s.e"],
        &["-e", "-g", "-r2", "s.e"],
    ] {
        let out = run("get", args, "");
        assert_eq!(out.status.code(), Some(1), "{args:?}: {out:?}");
        assert!(
            String::from_utf8_lossy(&out.stderr).contains("being edited"),
            "{args:?}: {out:?}"
        );
    }
    let pfile = std::fs::read_to_string(dir.join("p.e")).unwrap();
    assert_eq!(pfile.lines().count(), 1, "{pfile}");
}

/// `-t` is the most recently created delta "in a given release (for example,
/// -r1) or release and level (for example, -r1.2)" (POSIX get). The level
/// was ignored: `-t -r1.2` gave the newest delta anywhere in release 1.
#[test]
fn get_t_keeps_to_the_release_and_level_named() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "-fb", "s.t"], "1.1\n")
        .status
        .success());
    // 1.2, 1.3 on the trunk; then, later, 1.1.1.1 and 1.2.1.1 on branches.
    for (from, text) in [
        ("1.1", "1.2"),
        ("1.2", "1.3"),
        ("1.1", "1.1.1.1"),
        ("1.2", "1.2.1.1"),
    ] {
        let r = format!("-r{from}");
        assert!(
            run("get", &["-e", "-s", &r, "s.t"], "").status.success(),
            "{from}"
        );
        std::fs::write(dir.join("t"), format!("{text}\n")).unwrap();
        assert!(run("delta", &["-s", "-yc", "s.t"], "").status.success());
    }
    let top = |args: &[&str]| {
        let out = run("get", &[&["-p", "-s", "-t"], args, &["s.t"]].concat(), "");
        String::from_utf8(out.stdout).unwrap()
    };
    assert_eq!(top(&[]), "1.2.1.1\n");
    assert_eq!(top(&["-r1"]), "1.2.1.1\n");
    assert_eq!(top(&["-r1.3"]), "1.3\n");
    assert_eq!(top(&["-r1.2"]), "1.2.1.1\n");
    assert_eq!(top(&["-r1.1"]), "1.1.1.1\n");
}

/// `-t` is for a release, or a release and level. A SID that names a
/// branch or a delta on one is that branch or delta, with -t as without:
/// `-t -r1.1.1.1` retrieved, and with -e edited, the newest delta of some
/// other branch of 1.1.
#[test]
fn get_t_does_not_leave_the_branch_named() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "-fb", "s.b"], "1.1\n")
        .status
        .success());
    for (args, text) in [
        (&["-r1.1"][..], "1.2"),
        (&["-r1.1", "-b"], "1.1.1.1"),
        (&["-r1.1", "-b"], "1.1.2.1"),
    ] {
        let args = [&["-e", "-s"], args, &["s.b"]].concat();
        assert!(run("get", &args, "").status.success(), "{args:?}");
        std::fs::write(dir.join("b"), format!("{text}\n")).unwrap();
        assert!(run("delta", &["-s", "-yc", "s.b"], "").status.success());
    }
    for sid in ["-r1.1.1.1", "-r1.1.1"] {
        let out = run("get", &["-p", "-s", "-t", sid, "s.b"], "");
        assert_eq!(String::from_utf8_lossy(&out.stdout), "1.1.1.1\n", "{sid}");
    }
    let out = run("get", &["-e", "-g", "-t", "-r1.1.1.1", "s.b"], "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.1.1.1\nnew delta 1.1.1.2\n"
    );
}

/// `-e -rR` where release R does not exist and a higher one does: the
/// delta retrieved is hR.mL and the one to be created hR.mL.(mB+1).1, a
/// branch (POSIX get, the table of SIDs). The first delta of release R
/// was created instead, on the trunk between two releases that exist.
#[test]
fn get_e_of_a_skipped_release_branches() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "s.s"], "1.1\n").status.success());
    assert!(run("get", &["-e", "-s", "-r3", "s.s"], "").status.success());
    std::fs::write(dir.join("s"), "3.1\n").unwrap();
    assert!(run("delta", &["-s", "-yc", "s.s"], "").status.success());

    let out = run("get", &["-e", "-g", "-r2", "s.s"], "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.1\nnew delta 1.1.1.1\n"
    );
    assert!(run("unget", &["-s", "s.s"], "").status.success());
    // A release above the newest still begins that release.
    let out = run("get", &["-e", "-g", "-r4", "s.s"], "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "3.1\nnew delta 4.1\n");
}

/// A g-file made with `-k` is writable by its owner, like one made for
/// editing (POSIX get: read-only "unless -k is used or implied"): it is
/// what one edits after losing an edit's g-file. It was made read-only.
#[test]
fn get_k_makes_a_writable_g_file() {
    use std::os::unix::fs::PermissionsExt;
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run("admin", &["-i", "s.k"], "%I%\n").status.success());
    let mode = |name: &str| {
        std::fs::metadata(dir.join(name))
            .unwrap()
            .permissions()
            .mode()
            & 0o777
    };
    assert!(run("get", &["-s", "s.k"], "").status.success());
    assert_eq!(mode("k"), 0o444);
    assert!(run("get", &["-s", "-k", "s.k"], "").status.success());
    assert_eq!(mode("k"), 0o644);
    assert_eq!(std::fs::read(dir.join("k")).unwrap(), b"%I%\n");
}

/// The l-file lists each delta with "the comments and MR data ... on
/// subsequent lines, indented one <tab>" (POSIX get). The MRs were left out.
#[test]
fn get_l_lists_the_mrs() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str], stdin: &str| super::common::run_in(cmd, args, dir, stdin);
    assert!(run(
        "admin",
        &["-i", "-fv", "-mMR7 MR8", "-yinit", "s.l"],
        "one\n"
    )
    .status
    .success());
    let out = run("get", &["-s", "-L", "-g", "s.l"], "");
    let text = String::from_utf8(out.stdout).unwrap();
    let lines: Vec<&str> = text.lines().collect();
    assert_eq!(&lines[1..], ["\tMR7", "\tMR8", "\tinit", ""], "{text:?}");
}
