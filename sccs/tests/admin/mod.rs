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
use std::path::PathBuf;
use std::process::Output;

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

/// "The level number is optional, and defaults to 1" (POSIX 84017-84019).
///
/// `admin -r2` stored the parsed SID verbatim, and `"2"` parses to a release
/// with level 0. A level-0 SID is not a valid delta number, so the file was
/// born unusable: every later get and delta works from it.
#[test]
fn admin_r_defaults_the_level_to_one() {
    let tmp = plib::tmp::TempDir::new().unwrap();
    let sfile = tmp.path().join("s.rel");
    let sfile_s = sfile.to_string_lossy().to_string();

    let out = super::common::run_in("admin", &["-i", "-r2", &sfile_s], tmp.path(), "body\n");
    assert!(
        out.status.success(),
        "admin -r2: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let out = super::common::run_in("prs", &["-d:I:", "-r", &sfile_s], tmp.path(), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "2.1",
        "a release-only -r must default the level to 1"
    );

    // And the file is usable: the SID it minted can be retrieved and edited.
    let out = super::common::run_in("get", &["-e", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "the created SID must be usable: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// An explicit level is still honored.
#[test]
fn admin_r_keeps_an_explicit_level() {
    let tmp = plib::tmp::TempDir::new().unwrap();
    let sfile = tmp.path().join("s.rel2");
    let sfile_s = sfile.to_string_lossy().to_string();

    let out = super::common::run_in("admin", &["-i", "-r3.4", &sfile_s], tmp.path(), "body\n");
    assert!(out.status.success());

    let out = super::common::run_in("prs", &["-d:I:", "-r", &sfile_s], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "3.4");
}

// An argument that is not valid UTF-8 is reported, not a panic.
#[test]
fn admin_non_utf8_argument_is_reported() {
    plib::testing::assert_non_utf8_argument_rejected("admin", &[]);
}

/// An -i file that cannot be read is reported by name, with the system's
/// text for the error and without Rust's "(os error N)" suffix.
#[test]
fn admin_unreadable_input_names_the_system_error() {
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.noinput");
    let missing = "/nonexistent/admin-input";
    let open_error = plib::diag::io_error_text(&fs::File::open(missing).unwrap_err());
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), format!("-i{missing}")],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: format!("admin: {missing}: {open_error}\n"),
        expected_exit_code: 1,
    });
}

/// Every rewrite of an s-file keeps text that is not UTF-8 byte for byte:
/// body, comments, flag values, descriptive text and the user list. It was
/// decoded as UTF-8 with a fallback of "", so one `admin -fb` or `admin -z`
/// blanked all of it, under a valid checksum.
#[test]
fn admin_rewrite_keeps_non_utf8_text() {
    use super::common::{assert_sfile_holds, write_sfile, LATIN1_RECORDS, LATIN1_SFILE};
    let tmp = TempDir::new().unwrap();
    for args in [&["-fb"][..], &["-z"], &["-dq"], &["-afred"]] {
        let s = write_sfile(tmp.path(), "s.l1", LATIN1_SFILE);
        let mut argv = args.to_vec();
        argv.push("s.l1");
        let out = super::common::run_in("admin", &argv, tmp.path(), "");
        assert!(out.status.success(), "admin {args:?}: {out:?}");
        let kept: Vec<&[u8]> = LATIN1_RECORDS
            .iter()
            .copied()
            .filter(|r| !(args == ["-dq"] && r.starts_with(b"\x01f q")))
            .collect();
        assert_sfile_holds(&s, &kept);
        fs::remove_file(&s).unwrap();
    }

    // A user list entry that is not UTF-8, from another implementation.
    let mut content = LATIN1_SFILE.to_vec();
    let at = content
        .windows(4)
        .position(|w| w == b"\x01U\n\x01")
        .unwrap();
    content.splice(at..at, b"\xe9ric\n".iter().copied());
    let s = write_sfile(tmp.path(), "s.u", &content);
    let out = super::common::run_in("admin", &["-afred", "s.u"], tmp.path(), "");
    assert!(out.status.success(), "{out:?}");
    assert_sfile_holds(&s, &[b"\xe9ric", b"fred"]);
}

/// The x-file is created afresh, never opened through what is at its name: a
/// symlink planted at `x.<name>` -- by anyone who can write the directory --
/// must not lead admin, delta or rmdel to overwrite, or chmod, its target.
#[test]
fn x_file_symlink_is_not_followed() {
    use std::os::unix::fs::{symlink, PermissionsExt};
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let victim = dir.join("victim");
    let plant = |name: &str| {
        let _ = fs::remove_file(dir.join(name));
        fs::write(&victim, b"precious\n").unwrap();
        fs::set_permissions(&victim, fs::Permissions::from_mode(0o644)).unwrap();
        symlink(&victim, dir.join(name)).unwrap();
    };
    let check = |what: &str| {
        assert_eq!(
            fs::read(&victim).unwrap(),
            b"precious\n",
            "{what} wrote through"
        );
        let mode = fs::metadata(&victim).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode, 0o644, "{what} chmodded the target");
        let meta = fs::symlink_metadata(dir.join("s.f")).unwrap();
        assert!(
            meta.file_type().is_file(),
            "{what}: s.f is not a regular file"
        );
    };
    let run = |cmd: &str, args: &[&str]| {
        let out = super::common::run_in(cmd, args, dir, "");
        assert!(out.status.success(), "{cmd} {args:?}: {out:?}");
    };

    plant("x.f");
    run("admin", &["-n", "s.f"]);
    check("admin -n");

    plant("x.f");
    run("admin", &["-fb", "s.f"]);
    check("admin -fb");

    run("get", &["-e", "-s", "s.f"]);
    fs::write(dir.join("f"), b"one\n").unwrap();
    plant("x.f");
    run("delta", &["-s", "-yc", "s.f"]);
    check("delta");

    plant("x.f");
    run("rmdel", &["-r1.2", "s.f"]);
    check("rmdel");
}

/// Lines no s-file record accounts for -- in the delta table, inside a delta
/// entry, in the flags -- are structural corruption. `admin -h` and `val`
/// check the structure (POSIX admin 84107) and must report them, though the
/// checksum is right; `get` still reads around them. `admin -z` recomputes
/// the checksum and changes nothing else (84111): it used to re-serialize
/// the file, silently deleting such lines.
#[test]
fn admin_h_checks_structure_and_z_changes_only_the_checksum() {
    let tmp = TempDir::new().unwrap();
    let strays: [&[u8]; 3] = [
        // In the delta table, between entries.
        b"\x01s 00001/00000/00000\n\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01e\n\
STRAY\n\x01u\n\x01U\n\x01t\n\x01T\n\x01I 1\nbody %I%\n\x01E 1\n",
        // Inside a delta entry: a second comment line lost its ^Ac.
        b"\x01s 00001/00000/00000\n\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01c one\n\
two\n\x01e\n\x01u\n\x01U\n\x01t\n\x01T\n\x01I 1\nbody %I%\n\x01E 1\n",
        // In the flags section.
        b"\x01s 00001/00000/00000\n\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01e\n\
\x01u\n\x01U\n\x01f q a\nb\n\x01t\n\x01T\n\x01I 1\nbody %I%\n\x01E 1\n",
    ];
    for content in strays {
        let s = super::common::write_sfile(tmp.path(), "s.bad", content);
        let shown = String::from_utf8_lossy(content).into_owned();

        let out = super::common::run_in("admin", &["-h", "s.bad"], tmp.path(), "");
        assert_eq!(out.status.code(), Some(1), "admin -h passed {shown:?}");
        assert!(!out.stderr.is_empty());
        let out = super::common::run_in("val", &["s.bad"], tmp.path(), "");
        assert_eq!(out.status.code(), Some(0x20), "val passed {shown:?}");
        let out = super::common::run_in("get", &["-p", "-s", "s.bad"], tmp.path(), "");
        assert!(out.status.success(), "get must read around it: {out:?}");

        // Break the checksum, then -z: only line 1 may change.
        let mut data = fs::read(&s).unwrap();
        data[6] = if data[6] == b'0' { b'1' } else { b'0' };
        let mut perms = fs::metadata(&s).unwrap().permissions();
        #[allow(clippy::permissions_set_readonly_false)]
        perms.set_readonly(false);
        fs::set_permissions(&s, perms).unwrap();
        fs::write(&s, &data).unwrap();
        let out = super::common::run_in("admin", &["-z", "s.bad"], tmp.path(), "");
        assert!(out.status.success(), "{out:?}");
        let after = fs::read(&s).unwrap();
        let rest = |d: &[u8]| d[d.iter().position(|&b| b == b'\n').unwrap()..].to_vec();
        assert_eq!(rest(&after), rest(&data), "-z changed more than line 1");
        assert!(plib::sccsfile::stored_checksum_matches(
            &after,
            plib::sccsfile::stored_body_checksum(&after)
        ));
        assert!(after.starts_with(
            format!("\x01h{:05}\n", plib::sccsfile::stored_body_checksum(&after)).as_bytes()
        ));
        fs::remove_file(&s).unwrap();
    }
}

/// "Non-SCCS files and unreadable files shall be silently ignored" when a
/// directory, or a list on standard input, names them (POSIX admin
/// 84116-84123): an s-file the user cannot read is skipped without a
/// diagnostic, and the others are still processed.
#[test]
fn unreadable_members_are_silently_ignored() {
    use std::os::unix::fs::PermissionsExt;
    if unsafe { libc::geteuid() } == 0 {
        return; // root can read anything
    }
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path().join("d");
    fs::create_dir(&dir).unwrap();
    for name in ["s.a", "s.b"] {
        let out = super::common::run_in("admin", &["-n", name], &dir, "");
        assert!(out.status.success(), "{out:?}");
    }
    fs::set_permissions(dir.join("s.b"), fs::Permissions::from_mode(0o000)).unwrap();

    let check = |out: Output, what: &str| {
        assert!(out.status.success(), "{what}: {out:?}");
        assert!(out.stderr.is_empty(), "{what}: {out:?}");
    };
    check(
        super::common::run_in("admin", &["-fb", "d"], tmp.path(), ""),
        "directory",
    );
    check(
        super::common::run_in("admin", &["-fj", "-"], tmp.path(), "d/s.a\nd/s.b\n"),
        "stdin list",
    );
    check(
        super::common::run_in("prs", &["-d:I:", "d"], tmp.path(), ""),
        "prs directory",
    );
    let out = super::common::run_in("prs", &["-d:FL:", "d/s.a"], tmp.path(), "");
    let flags = String::from_utf8_lossy(&out.stdout);
    assert!(
        flags.contains("branch") && flags.contains("joint edit"),
        "{flags}"
    );
}

/// "If this file contains more than 99 999 lines, the number of lines
/// recorded in the header for this file shall be 99 999 for this delta"
/// (POSIX admin 84132-84133). The text itself is all kept.
#[test]
fn admin_records_at_most_99999_lines() {
    let tmp = TempDir::new().unwrap();
    let input: String = (0..100_001).map(|i| format!("{i} %I%\n")).collect();
    let out = super::common::run_in("admin", &["-i", "s.big"], tmp.path(), &input);
    assert!(out.status.success(), "{out:?}");
    let data = fs::read(tmp.path().join("s.big")).unwrap();
    assert!(
        data.windows(21).any(|w| w == b"\x01s 99999/00000/00000\n"),
        "the header must record 99999 lines"
    );
    let out = super::common::run_in("get", &["-p", "-k", "-s", "s.big"], tmp.path(), "");
    assert_eq!(out.stdout, input.as_bytes());
}

/// A `-y` comment of several lines is stored as several comment lines, as
/// delta stores one (POSIX admin 84095-84096). Written as one record, the
/// lines after the first were bare text in the delta table, which every
/// reader skipped and the next rewrite deleted.
#[test]
fn admin_multi_line_comment_is_kept() {
    let tmp = TempDir::new().unwrap();
    let out = super::common::run_in("admin", &["-n", "-yline1\nline2", "s.c"], tmp.path(), "");
    assert!(out.status.success(), "{out:?}");
    let out = super::common::run_in("prs", &["-d:C:", "s.c"], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "line1\nline2\n\n");
    let out = super::common::run_in("val", &["s.c"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(0), "{out:?}");
}

/// A flag value or a login holding a newline cannot be stored: it would put
/// a line of its own into the header. It is refused, and nothing is written.
#[test]
fn admin_rejects_values_that_would_break_the_header() {
    let tmp = TempDir::new().unwrap();
    let s = admin_create(tmp.path(), "inj", "body\n");
    let before = fs::read(&s).unwrap();
    for bad in [
        &["-fqa\nb"][..],
        &["-fmname\n\x01e"],
        &["-auser\nother"],
        &["-euser\nother"],
        &["-a\x01U"],
    ] {
        let mut argv = bad.to_vec();
        argv.push("s.inj");
        let out = super::common::run_in("admin", &argv, tmp.path(), "");
        assert_eq!(out.status.code(), Some(1), "admin {bad:?}: {out:?}");
        assert!(!out.stderr.is_empty(), "admin {bad:?} must say why");
        assert_eq!(
            fs::read(&s).unwrap(),
            before,
            "admin {bad:?} wrote the file"
        );

        argv.pop();
        argv.extend(["-n", "s.new"]);
        let out = super::common::run_in("admin", &argv, tmp.path(), "");
        assert_eq!(out.status.code(), Some(1), "admin -n {bad:?}: {out:?}");
        assert!(
            !tmp.path().join("s.new").exists(),
            "admin -n {bad:?} created it"
        );
    }
}

/// When the `v` flag names a validation program, admin runs it on the MRs of
/// the initial delta, and "a diagnostic message shall be written if ... MR
/// validation fails" (POSIX admin 84102-84106): no file is created.
#[test]
fn admin_runs_the_mr_validation_program() {
    let tmp = TempDir::new().unwrap();
    let out = run_in(
        "admin",
        &["-n", "-fv/bin/false", "-mMR1", "s.no"],
        tmp.path(),
        "",
    );
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("MR validation failed"),
        "{out:?}"
    );
    assert!(!tmp.path().join("s.no").exists());

    let out = run_in(
        "admin",
        &["-n", "-fv/bin/true", "-mMR1", "s.yes"],
        tmp.path(),
        "",
    );
    assert!(out.status.success(), "{out:?}");
    let out = run_in("prs", &["-d:MR:", "s.yes"], tmp.path(), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "MR1");
}

/// With the `v` flag set on create, "the m option is also used even if its
/// value is null" (POSIX admin 84077-84079): `-m ''` satisfies it. And on an
/// existing file, `-m` is diagnosed by whether *that file* has the `v` flag,
/// not by whether this command line sets it.
#[test]
fn admin_m_follows_the_v_flag_of_the_file() {
    let tmp = TempDir::new().unwrap();
    let out = run_in("admin", &["-n", "-fv", "-m", "", "s.null"], tmp.path(), "");
    assert!(out.status.success(), "a null -m must satisfy -fv: {out:?}");

    // The file has v: -m is accepted (it has no initial delta left to mark).
    let out = run_in("admin", &["-mMR2", "s.null"], tmp.path(), "");
    assert!(out.status.success(), "{out:?}");

    // A file without v: -m is diagnosed, and the file is untouched.
    let s = admin_create(tmp.path(), "plain", "body\n");
    let before = fs::read(&s).unwrap();
    let out = run_in("admin", &["-mMR2", "-fb", "s.plain"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(String::from_utf8_lossy(&out.stderr).contains("'v' flag"));
    assert_eq!(fs::read(&s).unwrap(), before);
}

/// What `admin -i` stores comes back from `get -k` byte for byte, whatever it
/// holds. Text is stored as text -- lines split at newlines only, so a CR
/// stays; bytes that are not UTF-8 are text too (the locale decides what a
/// character is, POSIX admin 84141) -- and so has its keywords expanded. Only
/// what the line format cannot hold is encoded: a NUL, a line beginning with
/// SOH, or a missing final newline.
#[test]
fn admin_i_round_trips_every_input() {
    let tmp = TempDir::new().unwrap();
    let cases: [(&str, &[u8], bool); 7] = [
        ("crlf", b"a %I%\r\nb\r\n", false),
        ("latin1", b"caf\xe9 %I%\n", false),
        ("midsoh", b"mid\x01soh %I%\n", false),
        ("blank", b"\n\na\n\n", false),
        ("leadsoh", b"\x01lead\n", true),
        ("nul", b"nul\x00x\n", true),
        ("nonl", b"no newline", true),
    ];
    for (name, content, encoded) in cases {
        let input = tmp.path().join(name);
        fs::write(&input, content).unwrap();
        let sname = format!("s.{name}");
        let out = run_in("admin", &[&format!("-i{name}"), &sname], tmp.path(), "");
        assert!(out.status.success(), "{name}: {out:?}");

        let data = fs::read(tmp.path().join(&sname)).unwrap();
        let has_e = data.windows(7).any(|w| w == b"\x01f e 1\n");
        assert_eq!(has_e, encoded, "{name}: encoded flag");

        let out = run_in("get", &["-p", "-k", "-s", &sname], tmp.path(), "");
        assert_eq!(out.stdout, content, "{name}: get -k");
        let out = run_in("val", &[&sname], tmp.path(), "");
        assert_eq!(out.status.code(), Some(0), "{name}: {out:?}");
    }

    // Text that is not UTF-8 is still text: its keywords are expanded.
    let out = run_in("get", &["-p", "-s", "s.latin1"], tmp.path(), "");
    assert_eq!(out.stdout, b"caf\xe9 1.1\n");
}

/// The `-t` file is text like any other: bytes that are not UTF-8 and CRs
/// are stored as they are. A line that would read as a control line is
/// refused, and an unreadable file is reported by name.
#[test]
fn admin_t_stores_descriptive_text_as_bytes() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("d.txt"), b"d\xe9sc\r\nline2\n").unwrap();
    let out = run_in("admin", &["-n", "-td.txt", "s.t"], tmp.path(), "");
    assert!(out.status.success(), "{out:?}");
    let out = run_in("prs", &["-d:FD:", "s.t"], tmp.path(), "");
    assert_eq!(out.stdout, b"d\xe9sc\r\nline2\n\n");

    // Replacing it on an existing file goes the same way.
    fs::write(tmp.path().join("d2.txt"), b"\xe9\n").unwrap();
    let out = run_in("admin", &["-td2.txt", "s.t"], tmp.path(), "");
    assert!(out.status.success(), "{out:?}");
    let out = run_in("prs", &["-d:FD:", "s.t"], tmp.path(), "");
    assert_eq!(out.stdout, b"\xe9\n\n");

    let before = fs::read(tmp.path().join("s.t")).unwrap();
    fs::write(tmp.path().join("bad.txt"), b"ok\n\x01T\n").unwrap();
    let out = run_in("admin", &["-tbad.txt", "s.t"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(String::from_utf8_lossy(&out.stderr).contains("bad.txt"));
    assert_eq!(fs::read(tmp.path().join("s.t")).unwrap(), before);

    let out = run_in("admin", &["-tmissing.txt", "s.t"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(
        String::from_utf8_lossy(&out.stderr).starts_with("admin: missing.txt: "),
        "{out:?}"
    );
}

/// `-i` names the text of one new file (POSIX admin 83983-83984): with more
/// than one, the second would be created from an input already read. It is
/// refused before anything is created.
#[test]
fn admin_i_takes_one_new_file() {
    let tmp = TempDir::new().unwrap();
    let out = run_in("admin", &["-i", "s.a", "s.b"], tmp.path(), "x %I%\n");
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(!out.stderr.is_empty());
    assert!(!tmp.path().join("s.a").exists() && !tmp.path().join("s.b").exists());
}
