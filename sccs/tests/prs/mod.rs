//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::common::run_test_with_checker;
use plib::testing::TestPlan;
use plib::tmp::TempDir;
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

use super::common::run_in;

/// Build an s-file with `n` deltas in `dir`, returning its path.
fn prs_multi_delta(dir: &std::path::Path, name: &str, n: usize) -> std::path::PathBuf {
    let sname = format!("s.{name}");
    let out = run_in("admin", &["-i", &sname], dir, "v1\n");
    assert!(
        out.status.success(),
        "admin failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    for i in 2..=n {
        let out = run_in("get", &["-e", &sname], dir, "");
        assert!(out.status.success(), "get -e failed");
        std::fs::write(dir.join(name), format!("v{i}\n")).unwrap();
        let out = run_in("delta", &["-ynext", &sname], dir, "");
        assert!(
            out.status.success(),
            "delta failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
    dir.join(sname)
}

/// #P1: with `-e` each delta's data specification is terminated by a newline,
/// so the output of `-d:I:` over three deltas is exactly three lines. The
/// default fixture elsewhere is single-delta, which cannot show this.
#[test]
fn prs_emits_one_newline_terminated_record_per_delta() {
    let tmp = TempDir::new().unwrap();
    prs_multi_delta(tmp.path(), "chain", 3);

    let out = run_in("prs", &["-e", "-d:I:", "s.chain"], tmp.path(), "");
    assert!(out.status.success());
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.3\n1.2\n1.1\n",
        "one newline-terminated record per delta, newest first"
    );

    // Without -e only the single default (most recent) delta is reported.
    let out = run_in("prs", &["-d:I:", "s.chain"], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "1.3\n");
}

/// `-e` selects the given delta "and earlier", `-l` the given delta "and
/// later" — the pair only distinguishable on a multi-delta file.
#[test]
fn prs_earlier_and_later_selectors() {
    let tmp = TempDir::new().unwrap();
    prs_multi_delta(tmp.path(), "sel", 3);

    let out = run_in("prs", &["-e", "-r1.2", "-d:I:", "s.sel"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.2\n1.1\n",
        "-e must report the named delta and earlier ones"
    );

    let out = run_in("prs", &["-l", "-r1.2", "-d:I:", "s.sel"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.3\n1.2\n",
        "-l must report the named delta and later ones"
    );
}

/// #P2: `-r` with no SID defaults to the most recently created delta and
/// prints the default report format.
#[test]
fn prs_r_without_a_sid_uses_the_most_recent_delta() {
    let tmp = TempDir::new().unwrap();
    prs_multi_delta(tmp.path(), "nosid", 3);

    let out = run_in("prs", &["-r", "s.nosid"], tmp.path(), "");
    assert!(
        out.status.success(),
        "prs -r with no SID failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("1.3"),
        "the default is the newest delta: {stdout:?}"
    );
    assert!(
        stdout.contains("MRs:") && stdout.contains("COMMENTS:"),
        "the default report format is used: {stdout:?}"
    );
}

/// `-a` includes deltas that `rmdel` marked removed; without it they are
/// omitted.
#[test]
fn prs_a_includes_removed_deltas() {
    let tmp = TempDir::new().unwrap();
    prs_multi_delta(tmp.path(), "rem", 3);

    let out = run_in("rmdel", &["-r", "1.3", "s.rem"], tmp.path(), "");
    assert!(out.status.success(), "rmdel failed");

    let out = run_in("prs", &["-e", "-d:I:", "s.rem"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.2\n1.1\n",
        "a removed delta is omitted by default"
    );

    let out = run_in("prs", &["-e", "-a", "-d:I:", "s.rem"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.3\n1.2\n1.1\n",
        "-a must include the removed delta"
    );
}

/// Line statistics are zero-padded to five digits, and `:DL:` is the
/// slash-joined triple of them.
#[test]
fn prs_line_statistics_are_five_digit_padded() {
    let tmp = TempDir::new().unwrap();
    prs_multi_delta(tmp.path(), "stats", 2);

    for kw in [":Li:", ":Ld:", ":Lu:"] {
        let out = run_in("prs", &["-d", kw, "s.stats"], tmp.path(), "");
        let value = String::from_utf8_lossy(&out.stdout).trim_end().to_string();
        assert_eq!(value.len(), 5, "{kw} must be five digits, got {value:?}");
        assert!(
            value.chars().all(|c| c.is_ascii_digit()),
            "{kw} must be numeric, got {value:?}"
        );
    }

    let out = run_in("prs", &["-d", ":DL:", "s.stats"], tmp.path(), "");
    let dl = String::from_utf8_lossy(&out.stdout).trim_end().to_string();
    let parts: Vec<&str> = dl.split('/').collect();
    assert_eq!(parts.len(), 3, ":DL: is inserted/deleted/unchanged: {dl:?}");
    for p in parts {
        assert_eq!(p.len(), 5, ":DL: components are five digits: {dl:?}");
    }
}

/// The `what`-string keywords. `:A:` is `:Z::Y: :M: :I::Z:` (spec 112350) and
/// `:Z:` is the literal `@(#)` (112351).
///
/// *Recorded divergence:* when the module-type flag is unset, CSSC 1.4.1
/// renders `:Y:` inside `:A:` as the literal `none` (`@(#)none p1 1.3@(#)`)
/// while we render it empty. The spec does not mandate a "none" spelling for
/// an unset keyword, and embedding the word into a what-string is the less
/// useful reading, so ours is kept. With the flag set the two agree exactly.
#[test]
fn prs_what_string_keywords() {
    let tmp = TempDir::new().unwrap();
    prs_multi_delta(tmp.path(), "wh", 2);

    let out = run_in("prs", &["-d", ":Z:", "s.wh"], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "@(#)\n");

    // With the module type flag set, :A: is fully determined.
    let out = run_in("admin", &["-fttextfile", "s.wh"], tmp.path(), "");
    assert!(out.status.success());

    let out = run_in("prs", &["-d", ":A:", "s.wh"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "@(#)textfile wh 1.2@(#)\n",
        ":A: is :Z::Y: :M: :I::Z:"
    );

    let out = run_in("prs", &["-d", ":W:", "s.wh"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "@(#)wh\t1.2\n",
        ":W: is :Z::M:\\t:I:"
    );
}

/// Multiple operands, a directory operand and a `-` stdin list all work, and a
/// missing operand is an error.
#[test]
fn prs_operand_forms_and_exit_status() {
    let tmp = TempDir::new().unwrap();
    prs_multi_delta(tmp.path(), "one", 1);
    prs_multi_delta(tmp.path(), "two", 1);

    let out = run_in("prs", &["-d:M:", "s.one", "s.two"], tmp.path(), "");
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("one") && stdout.contains("two"));

    // Directory operand.
    let dir = tmp.path().join("proj");
    std::fs::create_dir(&dir).unwrap();
    std::fs::copy(tmp.path().join("s.one"), dir.join("s.one")).unwrap();
    let out = run_in("prs", &["-d:M:", "proj"], tmp.path(), "");
    assert!(out.status.success());
    assert!(String::from_utf8_lossy(&out.stdout).contains("one"));

    // `-` reads the pathname list from stdin.
    let out = run_in("prs", &["-d:M:", "-"], tmp.path(), "s.two\n");
    assert!(out.status.success());
    assert!(String::from_utf8_lossy(&out.stdout).contains("two"));

    // A missing operand is an error.
    let out = run_in("prs", &["s.does-not-exist"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1));
    assert!(!out.stderr.is_empty(), "a diagnostic is required");
}

/// The `-c` cutoff applies whether or not `-e`/`-l` is given: "No changes
/// (deltas) to the SCCS file that were created after the specified cutoff
/// date-time shall be included in the output" (112241-112243) is unconditional,
/// and the synopsis at 112215 makes `[-e|-l]` optional in the `-c` form.
///
/// Uses the checked-in fixture, whose deltas are pinned at 25/12/12 22:24:47
/// (1.1), 22:24:57 (1.2) and 22:25:05 (1.3), so the expectations do not depend
/// on when the suite runs.
#[test]
fn prs_cutoff_applies_without_e_or_l() {
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.multi");
    std::fs::copy(
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/s.multi"),
        &sfile,
    )
    .unwrap();

    let sids = |args: &[&str]| -> String {
        let out = run_in("prs", args, tmp.path(), "");
        assert!(
            out.status.success(),
            "prs {args:?} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        String::from_utf8_lossy(&out.stdout).into_owned()
    };

    // Cutoff after the whole history: the newest delta is still reported.
    assert_eq!(sids(&["-c25", "-d:I:", "s.multi"]), "1.3\n");

    // Cutoff before the whole history: nothing survives it. Without the fix
    // this printed 1.3, because -c was parsed and then ignored.
    assert_eq!(
        sids(&["-c2412", "-d:I:", "s.multi"]),
        "",
        "a cutoff before every delta must exclude them all"
    );

    // -e and -l keep working, and agree on the same boundary.
    assert_eq!(
        sids(&["-e", "-c2512122224", "-d:I:", "s.multi"]),
        "1.2\n1.1\n",
        "-e takes everything up to and including the cutoff"
    );
    assert_eq!(
        sids(&["-l", "-c251212222500", "-d:I:", "s.multi"]),
        "1.3\n",
        "-l takes everything from the cutoff onward"
    );
}

/// The cutoff's year is always two digits with the [69,99]=19xx / [00,68]=20xx
/// pivot (112236-112237). There is no 4-digit-year form to infer.
#[test]
fn prs_cutoff_year_is_always_two_digits() {
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.multi");
    std::fs::copy(
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/s.multi"),
        &sfile,
    )
    .unwrap();

    let sids = |args: &[&str]| -> String {
        let out = run_in("prs", args, tmp.path(), "");
        String::from_utf8_lossy(&out.stdout).into_owned()
    };

    // "2512" is December 2025, not the year 2512. Reading it as a 4-digit year
    // put the cutoff five centuries out and let every delta through.
    assert_eq!(
        sids(&["-e", "-c2512", "-d:I:", "s.multi"]),
        "1.3\n1.2\n1.1\n",
        "2512 means December 2025, which is after the whole fixture"
    );
    assert_eq!(
        sids(&["-e", "-c2511", "-d:I:", "s.multi"]),
        "",
        "November 2025 precedes the fixture's deltas"
    );

    // 69-99 select the 20th century, so these precede everything.
    assert_eq!(sids(&["-e", "-c99", "-d:I:", "s.multi"]), "");
    assert_eq!(sids(&["-e", "-c69", "-d:I:", "s.multi"]), "");
}

/// An out-of-range cutoff component is rejected rather than ignored. Dropping
/// an unparseable `-c` would report every delta, which reads as a successful
/// query rather than a rejected one.
#[test]
fn prs_cutoff_invalid_field_rejected() {
    let tmp = TempDir::new().unwrap();
    std::fs::copy(
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/s.multi"),
        tmp.path().join("s.multi"),
    )
    .unwrap();

    for bad in ["-c251312", "-c25121225", "-c2512122260"] {
        let out = run_in("prs", &["-e", bad, "-d:I:", "s.multi"], tmp.path(), "");
        assert_eq!(
            out.status.code(),
            Some(1),
            "{bad} must be rejected, got stdout {:?}",
            String::from_utf8_lossy(&out.stdout)
        );
        assert!(out.stdout.is_empty(), "{bad} must not report deltas anyway");
        assert!(!out.stderr.is_empty(), "{bad} needs a diagnostic");
    }
}
