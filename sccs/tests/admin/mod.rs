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
use std::fs;
use std::path::PathBuf;
use std::process::Output;
use tempfile::TempDir;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(name)
}

#[test]
fn admin_check_valid_file() {
    // Test -h (check) on a valid SCCS file
    let fixture = fixture_path("s.simple");
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-h".into(), fixture.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn admin_check_all_fixtures() {
    // Test -h on all fixtures
    for name in &["s.simple", "s.multi", "s.keywords", "s.branched"] {
        let fixture = fixture_path(name);
        run_test(TestPlan {
            cmd: String::from("admin"),
            args: vec!["-h".into(), fixture.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

#[test]
fn admin_create_new_file_stdin() {
    // Test creating new SCCS file from stdin
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.newfile");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into()],
        stdin_data: String::from("hello\nworld\n"),
        expected_out: String::new(),
        expected_err: format!("admin: warning: {}: No id keywords.\n", sfile.display()),
        expected_exit_code: 0,
    });

    // Verify file was created
    assert!(sfile.exists(), "SCCS file should be created");

    // Verify with -h
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-h".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn admin_create_new_file_from_file() {
    // Test creating new SCCS file from an existing file
    let tmp = TempDir::new().unwrap();
    let input = tmp.path().join("input.txt");
    let sfile = tmp.path().join("s.fromfile");

    // Create input file
    fs::write(&input, "line one\nline two\nline three\n").unwrap();

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![
            sfile.to_string_lossy().into(),
            format!("-i{}", input.to_string_lossy()),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: format!("admin: warning: {}: No id keywords.\n", sfile.display()),
        expected_exit_code: 0,
    });

    // Verify file was created
    assert!(sfile.exists(), "SCCS file should be created");
}

#[test]
fn admin_error_file_exists() {
    // Test error when creating file that already exists
    let fixture = fixture_path("s.simple");

    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec!["-n".into(), fixture.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "should fail when file exists");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("file already exists"),
            "error message should mention file exists"
        );
    });
}

#[test]
fn admin_error_invalid_name() {
    // Test error for invalid SCCS file name (not starting with s.)
    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec!["-n".into(), "invalid.txt".into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "should fail for invalid name");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("s."),
            "error message should mention s. requirement"
        );
    });
}

#[test]
fn admin_recompute_checksum() {
    // Test -z (recompute checksum)
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.checksum");

    // Create a new SCCS file first
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into()],
        stdin_data: String::from("test content\n"),
        expected_out: String::new(),
        expected_err: format!("admin: warning: {}: No id keywords.\n", sfile.display()),
        expected_exit_code: 0,
    });

    // Make file writable for -z
    let perms = std::os::unix::fs::PermissionsExt::from_mode(0o644);
    fs::set_permissions(&sfile, perms).ok();

    // Recompute checksum
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-z".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Verify with -h
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-h".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn admin_create_with_initial_sid() {
    // Test creating file with initial SID
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.withsid");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into(), "-r2.1".into()],
        stdin_data: String::from("content\n"),
        expected_out: String::new(),
        expected_err: format!("admin: warning: {}: No id keywords.\n", sfile.display()),
        expected_exit_code: 0,
    });

    assert!(sfile.exists());
}

#[test]
fn admin_create_with_comment() {
    // Test creating file with custom comment
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.withcomment");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![
            sfile.to_string_lossy().into(),
            "-i".into(),
            "-yMy custom comment".into(),
        ],
        stdin_data: String::from("content\n"),
        expected_out: String::new(),
        expected_err: format!("admin: warning: {}: No id keywords.\n", sfile.display()),
        expected_exit_code: 0,
    });

    assert!(sfile.exists());

    // Verify comment is in file
    let content = fs::read_to_string(&sfile).unwrap();
    assert!(
        content.contains("My custom comment"),
        "Comment should be in file"
    );
}

#[test]
fn admin_bare_i_then_operand() {
    // #A1: bare -i must NOT consume the following operand; the operand names
    // the s-file and the body is read from stdin.
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.bare");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-i".into(), sfile.to_string_lossy().into()],
        stdin_data: String::from("%I%\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(sfile.exists(), "s-file should be created from stdin");
}

#[test]
fn admin_reject_invalid_flag() {
    // #A2: an unrecognized -f flag letter is rejected (non-zero exit).
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.badflag");

    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec!["-i".into(), "-fZ".into(), sfile.to_string_lossy().into()],
        stdin_data: String::from("%I%\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "invalid flag must fail");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("Unrecognized flag"),
            "stderr should report unrecognized flag, got: {stderr}"
        );
        assert!(!sfile.exists(), "s-file must not be created on flag error");
    });
}

#[test]
fn admin_ceiling_out_of_range() {
    // #A8: ceiling/floor flag values above 9999 are rejected.
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.ceil");

    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec![
            "-i".into(),
            "-fc10000".into(),
            sfile.to_string_lossy().into(),
        ],
        stdin_data: String::from("%I%\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "out-of-range ceiling must fail");
    });
}

#[test]
fn admin_no_id_keyword_warning() {
    // #A9: a text body with no %X% id keyword triggers a warning to stderr.
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.noid");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-i".into(), sfile.to_string_lossy().into()],
        stdin_data: String::from("plain text\n"),
        expected_out: String::new(),
        expected_err: format!("admin: warning: {}: No id keywords.\n", sfile.display()),
        expected_exit_code: 0,
    });
}

#[test]
fn admin_id_keyword_no_warning() {
    // #A9: a body containing %I% suppresses the warning.
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.withkw");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-i".into(), sfile.to_string_lossy().into()],
        stdin_data: String::from("%I%\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn admin_m_without_v_flag() {
    // #A7: -m without the v flag is a diagnostic error.
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.m");

    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec!["-i".into(), "-mBUG1".into(), sfile.to_string_lossy().into()],
        stdin_data: String::from("%I%\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "-m without v must fail");
    });
}

#[test]
fn admin_v_flag_requires_m() {
    // #A7: v flag set on create without -m is a diagnostic error.
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.v");

    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec!["-i".into(), "-fv".into(), sfile.to_string_lossy().into()],
        stdin_data: String::from("%I%\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "v flag without -m must fail");
    });
}

#[test]
fn admin_m_with_v_flag() {
    // #A7: -m together with -fv succeeds and records the MR.
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.mv");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![
            "-i".into(),
            "-fv".into(),
            "-mBUG42".into(),
            sfile.to_string_lossy().into(),
        ],
        stdin_data: String::from("%I%\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&sfile).unwrap();
    assert!(content.contains("BUG42"), "MR number should be recorded");
}

use super::common::run_in;

/// Create `s.<name>` in `dir` from `content`, returning its path.
fn admin_create(dir: &std::path::Path, name: &str, content: &str) -> PathBuf {
    let sname = format!("s.{name}");
    let out = run_in("admin", &["-i", &sname], dir, content);
    assert!(
        out.status.success(),
        "admin -i failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    dir.join(sname)
}

/// #A1: SCCS option-arguments are **attached**, so `-i` takes its value as
/// `-iname`. In the separated form `-i name`, `name` is an operand — and an
/// operand that is not an s-file is an error.
///
/// CSSC 1.4.1 reads it the same way (it reports "The 'i' keyletter can't be
/// used with multiple files"), so this pins the interpretation rather than a
/// particular wording.
#[test]
fn admin_i_option_argument_must_be_attached() {
    let tmp = TempDir::new().unwrap();
    std::fs::write(tmp.path().join("src.txt"), b"seed content\n").unwrap();

    // Attached: the file supplies the initial body.
    let out = run_in("admin", &["-isrc.txt", "s.attached"], tmp.path(), "");
    assert!(
        out.status.success(),
        "admin -isrc.txt failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(tmp.path().join("s.attached").exists());

    let out = run_in("get", &["-p", "-s", "s.attached"], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "seed content\n",
        "the attached form must take the body from the named file"
    );

    // Separated: `src.txt` becomes an operand and is rejected as a non-s-file.
    let out = run_in("admin", &["-i", "src.txt", "s.separated"], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "the separated form must be an error"
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("src.txt"),
        "the offending operand should be named: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// #A2: an unrecognized `-f` flag letter is rejected. `Z` is not an SCCS flag
/// and `4` is not a flag at all, so both must be diagnosed rather than
/// silently stored — otherwise a typo would set a flag nothing consults.
#[test]
fn admin_rejects_unrecognized_flags() {
    let tmp = TempDir::new().unwrap();
    let s = admin_create(tmp.path(), "flags", "body\n");
    let sname = s.file_name().unwrap().to_str().unwrap();

    for bad in ["-fZ", "-f4"] {
        let out = run_in("admin", &[bad, sname], tmp.path(), "");
        assert_eq!(out.status.code(), Some(1), "{bad} must be rejected");
        assert!(!out.stderr.is_empty(), "a diagnostic is required for {bad}");
    }

    // Real flags still work, so the check is not simply rejecting every -f.
    for good in ["-fb", "-fj", "-fn"] {
        let out = run_in("admin", &[good, sname], tmp.path(), "");
        assert!(
            out.status.success(),
            "{good} should be accepted: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

/// #A3/#A4: a directory operand expands to the s-files it contains, and a lone
/// `-` reads the pathname list from standard input.
#[test]
fn admin_directory_and_stdin_operands() {
    // Directory.
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path().join("proj");
    std::fs::create_dir(&dir).unwrap();
    let s = admin_create(tmp.path(), "indir", "body\n");
    std::fs::rename(&s, dir.join("s.indir")).unwrap();

    let out = run_in("admin", &["-fb", "proj"], tmp.path(), "");
    assert!(
        out.status.success(),
        "admin <dir> failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let out = run_in("prs", &["-d:FL:", "proj/s.indir"], tmp.path(), "");
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("branch"),
        "the flag should have been set on the file inside the directory: {:?}",
        String::from_utf8_lossy(&out.stdout)
    );

    // `-` stdin list.
    let tmp2 = TempDir::new().unwrap();
    admin_create(tmp2.path(), "viastdin", "body\n");
    let out = run_in("admin", &["-fj", "-"], tmp2.path(), "s.viastdin\n");
    assert!(
        out.status.success(),
        "admin - failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let out = run_in("prs", &["-d:FL:", "s.viastdin"], tmp2.path(), "");
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("joint edit"),
        "the flag should have been set via the stdin list: {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
}

/// `-a` adds an authorized user, `-e` erases one, and `-d` deletes a flag —
/// a round trip through the s-file's header.
#[test]
fn admin_user_list_and_flag_round_trip() {
    let tmp = TempDir::new().unwrap();
    let s = admin_create(tmp.path(), "rt", "body\n");
    let sname = s.file_name().unwrap().to_str().unwrap();

    let read_kw = |kw: &str| -> String {
        let out = run_in("prs", &[&format!("-d:{kw}:"), sname], tmp.path(), "");
        String::from_utf8_lossy(&out.stdout).trim().to_string()
    };

    // Empty user list reads as "none".
    assert_eq!(read_kw("UN"), "none", "a fresh file has no user list");

    let out = run_in("admin", &["-atestuser", sname], tmp.path(), "");
    assert!(out.status.success());
    assert_eq!(read_kw("UN"), "testuser", "-a must add the user");

    let out = run_in("admin", &["-etestuser", sname], tmp.path(), "");
    assert!(out.status.success());
    assert_eq!(read_kw("UN"), "none", "-e must remove the user");

    // Flags: set then delete.
    let out = run_in("admin", &["-fn", sname], tmp.path(), "");
    assert!(out.status.success());
    assert!(
        read_kw("FL").contains("null delta"),
        "-fn must set the flag"
    );

    let out = run_in("admin", &["-dn", sname], tmp.path(), "");
    assert!(out.status.success());
    assert!(
        !read_kw("FL").contains("null delta"),
        "-dn must delete the flag"
    );
}

/// `-h` audits the stored checksum and `-z` recomputes it. Deliberately
/// corrupting the body must make `-h` fail, and `-z` must then make it pass
/// again — the pair is what makes either meaningful.
///
/// Cross-checked against CSSC 1.4.1, which reports the identical stored and
/// computed values for the same corruption.
#[test]
fn admin_checksum_audit_and_recompute() {
    let tmp = TempDir::new().unwrap();
    let s = admin_create(tmp.path(), "sum", "body line\n");
    let sname = s.file_name().unwrap().to_str().unwrap();

    // A freshly written file passes the audit.
    let out = run_in("admin", &["-h", sname], tmp.path(), "");
    assert!(
        out.status.success(),
        "-h on an intact file should pass: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // Corrupt the body without touching the stored checksum.
    let mut perms = fs::metadata(&s).unwrap().permissions();
    #[allow(clippy::permissions_set_readonly_false)]
    perms.set_readonly(false);
    fs::set_permissions(&s, perms).unwrap();
    let mut data = fs::read(&s).unwrap();
    let at = data
        .windows(4)
        .position(|w| w == b"body")
        .expect("body text must be present");
    data[at..at + 4].copy_from_slice(b"XXXX");
    fs::write(&s, &data).unwrap();

    let out = run_in("admin", &["-h", sname], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "-h must detect the corruption");
    assert!(!out.stderr.is_empty(), "a checksum diagnostic is required");

    // -z recomputes, after which the audit passes again.
    let out = run_in("admin", &["-z", sname], tmp.path(), "");
    assert!(
        out.status.success(),
        "-z failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let out = run_in("admin", &["-h", sname], tmp.path(), "");
    assert!(
        out.status.success(),
        "-h must pass after -z recomputes the checksum"
    );
}

/// A body with no `%X%` keyword draws the "No id keywords" warning on stderr,
/// but is not an error.
#[test]
fn admin_warns_when_the_body_has_no_id_keywords() {
    let tmp = TempDir::new().unwrap();

    let out = run_in("admin", &["-i", "s.nokw"], tmp.path(), "plain text\n");
    assert!(out.status.success(), "the warning must not be fatal");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("No id keywords"),
        "expected the warning, got {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    // With a keyword present there is no such warning.
    let out = run_in("admin", &["-i", "s.withkw"], tmp.path(), "%W%\ntext\n");
    assert!(out.status.success());
    assert!(
        !String::from_utf8_lossy(&out.stderr).contains("No id keywords"),
        "a body containing %W% must not draw the warning: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
}
