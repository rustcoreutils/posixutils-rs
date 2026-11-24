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

/// `admin -h` and `val` both verify the stored checksum, and they must agree.
///
/// They open-coded the check separately and guarded it differently — `admin`
/// with `content_start < data.len()`, `val` with `<=`. The difference turns
/// out to be unreachable, because even an `admin -n` file with no body still
/// has a delta table after the checksum line, so both guards fire. Both now
/// call one helper, and this test pins the agreement so a future edit to
/// either side cannot reintroduce a gap that is only accidentally harmless.
#[test]
fn admin_h_and_val_agree_on_a_corrupt_header_only_file() {
    let tmp = plib::tmp::TempDir::new().unwrap();
    let sfile = tmp.path().join("s.headeronly");
    let sfile_s = sfile.to_string_lossy().to_string();

    // -n creates the file with control information but no file data.
    let out = super::common::run_in("admin", &["-n", &sfile_s], tmp.path(), "");
    assert!(
        out.status.success(),
        "admin -n: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // Intact, both accept it.
    let admin_ok = super::common::run_in("admin", &["-h", &sfile_s], tmp.path(), "");
    let val_ok = super::common::run_in("val", &[&sfile_s], tmp.path(), "");
    assert_eq!(
        admin_ok.status.success(),
        val_ok.status.success(),
        "admin -h and val must agree on an intact header-only file"
    );
    assert!(admin_ok.status.success(), "an intact file should pass");

    // Corrupt the stored checksum on the first line, leaving everything else
    // byte-identical. The s-file is read-only, as SCCS keeps it.
    let mut perms = fs::metadata(&sfile).unwrap().permissions();
    let orig = perms.clone();
    #[allow(clippy::permissions_set_readonly_false)]
    perms.set_readonly(false);
    fs::set_permissions(&sfile, perms).unwrap();

    let data = fs::read(&sfile).unwrap();
    let nl = data.iter().position(|&b| b == b'\n').unwrap();
    let mut corrupt = b"\x01h99999".to_vec();
    corrupt.extend_from_slice(&data[nl..]);
    fs::write(&sfile, &corrupt).unwrap();
    fs::set_permissions(&sfile, orig).unwrap();

    let admin_bad = super::common::run_in("admin", &["-h", &sfile_s], tmp.path(), "");
    let val_bad = super::common::run_in("val", &[&sfile_s], tmp.path(), "");
    assert_eq!(
        admin_bad.status.success(),
        val_bad.status.success(),
        "admin -h and val must agree on a corrupt header-only file: \
         admin said {:?}, val said {:?}",
        admin_bad.status.code(),
        val_bad.status.code()
    );
    assert!(
        !admin_bad.status.success(),
        "a corrupt checksum must be diagnosed, not passed"
    );
}

// A read error on standard input ends `val -`: it is reported once, not
// retried. A directory as standard input (EISDIR) used to spin for ever.
#[cfg(unix)]
#[test]
fn val_stdin_read_error_ends_input() {
    use std::process::{Command, Stdio};
    use std::time::{Duration, Instant};

    let tmp = TempDir::new().unwrap();
    let dir = std::fs::File::open(tmp.path()).unwrap();
    let mut child = Command::new(plib::testing::get_binary_path("val"))
        .arg("-")
        .stdin(Stdio::from(dir))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn val");
    let start = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if start.elapsed() > Duration::from_secs(20) {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("val - never finished reading a directory as standard input");
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    let out = child.wait_with_output().unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(stderr.lines().count(), 1, "{stderr:?}");
    assert!(stderr.starts_with("val: "), "{stderr:?}");
    assert!(out.stdout.is_empty());
    assert_ne!(out.status.code(), Some(0));
}

// A failed write of the help or version text is a write error, not a silent
// success. admin's own --help wants a file operand, so its version is used.
#[cfg(target_os = "linux")]
#[test]
fn help_write_error_on_full_device() {
    for (cmd, opt) in [("prs", "--help"), ("val", "--help"), ("admin", "--version")] {
        plib::testing::assert_write_error_message_on_full_device(cmd, &[opt], b"", 1);
    }
}

/// A file holding bytes above 0x7f validates whether its checksum is the
/// signed sum CSSC and historical SCCS write or the unsigned sum earlier
/// versions of these utilities wrote; and what admin writes is the signed one,
/// so CSSC accepts it too.
#[test]
fn val_accepts_the_signed_and_unsigned_checksum() {
    let tmp = TempDir::new().unwrap();
    let content = super::common::LATIN1_SFILE;
    let unsigned = content.iter().fold(0u16, |s, &b| s.wrapping_add(b as u16));
    let signed = content
        .iter()
        .fold(0u16, |s, &b| s.wrapping_add(b as i8 as i16 as u16));
    for sum in [signed, unsigned] {
        let mut data = format!("\x01h{:05}\n", sum).into_bytes();
        data.extend_from_slice(content);
        fs::write(tmp.path().join("s.l1"), &data).unwrap();
        let out = super::common::run_in("val", &["s.l1"], tmp.path(), "");
        assert_eq!(out.status.code(), Some(0), "sum {sum}: {out:?}");
        let out = super::common::run_in("admin", &["-h", "s.l1"], tmp.path(), "");
        assert_eq!(out.status.code(), Some(0), "sum {sum}: {out:?}");
    }

    let out = super::common::run_in("admin", &["-z", "s.l1"], tmp.path(), "");
    assert!(out.status.success(), "{out:?}");
    let data = fs::read(tmp.path().join("s.l1")).unwrap();
    assert!(
        data.starts_with(format!("\x01h{:05}\n", signed).as_bytes()),
        "admin must write the signed sum"
    );
}

/// Every command that reads an s-file checks its checksum first and refuses
/// a file that fails it, as SCCS does ("Corrupted file"). Only `admin -z`,
/// whose job is to set the checksum, reads past it. Before, only val and
/// admin -h looked: get returned text from a damaged file, and an edit by
/// admin, delta or rmdel gave it a fresh, valid checksum.
#[test]
fn a_bad_checksum_is_refused_by_every_reader() {
    let tmp = TempDir::new().unwrap();
    let s = create_sccs_file(&tmp, "sum", "one %I%\n");
    let dir = tmp.path();
    let run = |cmd: &str, args: &[&str]| super::common::run_in(cmd, args, dir, "");

    // An edit is pending, so that delta has something to do.
    assert!(run("get", &["-e", "-s", "s.sum"]).status.success());
    fs::write(dir.join("sum"), b"two\n").unwrap();

    // Damage the body without touching the checksum line.
    let mut data = fs::read(&s).unwrap();
    let at = data.windows(3).position(|w| w == b"one").unwrap();
    data[at..at + 3].copy_from_slice(b"ONE");
    let mut perms = fs::metadata(&s).unwrap().permissions();
    #[allow(clippy::permissions_set_readonly_false)]
    perms.set_readonly(false);
    fs::set_permissions(&s, perms).unwrap();
    fs::write(&s, &data).unwrap();

    for (cmd, args) in [
        ("get", &["-p", "-s", "s.sum"][..]),
        ("prs", &["s.sum"]),
        ("delta", &["-s", "-yc", "s.sum"]),
        ("rmdel", &["-r1.1", "s.sum"]),
        ("admin", &["-fb", "s.sum"]),
        ("admin", &["-h", "s.sum"]),
    ] {
        let out = run(cmd, args);
        assert_eq!(out.status.code(), Some(1), "{cmd} {args:?}: {out:?}");
        assert!(
            String::from_utf8_lossy(&out.stderr)
                .to_lowercase()
                .contains("checksum"),
            "{cmd} {args:?} should name the checksum: {out:?}"
        );
        assert_eq!(fs::read(&s).unwrap(), data, "{cmd} {args:?} wrote the file");
    }
    assert_eq!(run("val", &["s.sum"]).status.code(), Some(0x20));
    assert!(dir.join("p.sum").exists(), "the pending edit is untouched");

    // admin -z is the repair; after it everything reads the file again.
    assert!(run("admin", &["-z", "s.sum"]).status.success());
    let out = run("get", &["-p", "-k", "-s", "s.sum"]);
    assert_eq!(out.stdout, b"ONE %I%\n");
    assert!(run("delta", &["-s", "-yc", "s.sum"]).status.success());
}

/// A delta's predecessor is an older delta: its serial is smaller. A file
/// that says otherwise is corrupt, and is refused by every command when it
/// is read. Nothing checked, so a table whose predecessors formed a cycle
/// passed `val` and `admin -h`, and `get -i`/`-x`, which follow predecessors,
/// never returned.
#[test]
fn a_predecessor_that_is_not_older_is_corruption() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    // 1.2 (serial 2) names serial 3 as its predecessor: 2 -> 3 -> 2.
    let table = String::from_utf8(super::common::THREE_DELTAS.to_vec())
        .unwrap()
        .replace("26/01/02 00:00:00 u 2 1", "26/01/02 00:00:00 u 2 3");
    let body = "\x01I 1\none\n\x01E 1\n";
    super::common::write_sfile(dir, "s.cyc", format!("{table}{body}").as_bytes());
    let run = |cmd: &str, args: &[&str]| super::common::run_in(cmd, args, dir, "");

    assert_eq!(run("val", &["s.cyc"]).status.code(), Some(0x20));
    for (cmd, args) in [
        ("admin", &["-h", "s.cyc"][..]),
        ("get", &["-p", "-s", "-i1.1-1.3", "s.cyc"]),
        ("get", &["-p", "-s", "-x1.1", "s.cyc"]),
        ("prs", &["s.cyc"]),
    ] {
        let out = run(cmd, args);
        assert_eq!(out.status.code(), Some(1), "{cmd} {args:?}: {out:?}");
        assert!(
            String::from_utf8_lossy(&out.stderr).contains("predecessor"),
            "{cmd} {args:?}: {out:?}"
        );
    }
}
