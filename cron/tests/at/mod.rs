//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::sync::Mutex;

static TEST_MUTEX: Mutex<()> = Mutex::new(());

use std::{fs, path::Path};

use plib::testing::{run_test, TestPlan};
use plib::tmp::{tempdir, TempDir};

fn setup_test_env() -> (TempDir, String) {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let dir_path = temp_dir.path().join("testdir");
    (temp_dir, dir_path.to_str().unwrap().to_string())
}

fn run_test_at(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    // Pin the timezone and locale so the `-l` listing (formatted via libc
    // strftime in the user's timezone) is deterministic regardless of the host.
    std::env::set_var("TZ", "UTC");
    std::env::set_var("LC_ALL", "C");
    // Make the allow/deny gate permit the test user regardless of the host's
    // /etc/at.{allow,deny}: no allow file, an empty deny file (/dev/null).
    std::env::set_var("AT_ALLOW", "/nonexistent/posixutils/at.allow");
    std::env::set_var("AT_DENY", "/dev/null");

    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("at"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

#[test]
fn test1() {
    let _lock = TEST_MUTEX.lock().unwrap();

    let (_temp_dir, dir_path) = setup_test_env();

    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();
    let args = ["05:53amNOV4,2100", "-f", &file];

    // The submission notice goes to standard error (audit #A3).
    let expected_error = "job 1 at Thu Nov  4 05:53:00 2100\n";

    run_test_at(&args, "", expected_error, 0);

    let res_file = Path::new(&dir_path).join("a00001041a0e81");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test2() {
    let _lock = TEST_MUTEX.lock().unwrap();

    let (_temp_dir, dir_path) = setup_test_env();

    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["05:53amNOV4,2100+30minutes", "-f", &file];

    let expected_error = "job 1 at Thu Nov  4 06:23:00 2100\n";

    run_test_at(&args, "", expected_error, 0);

    let res_file = Path::new(&dir_path).join("a00001041a0e9f");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test3() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["05:53amNOV4,2100+1day", "-f", &file];

    let expected_error = "job 1 at Fri Nov  5 05:53:00 2100\n";

    run_test_at(&args, "", expected_error, 0);

    let res_file = Path::new(&dir_path).join("a00001041a1421");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test4() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file];

    let expected_error = "job 1 at Thu Nov  4 00:00:00 2100\n";

    run_test_at(&args, "", expected_error, 0);

    let res_file = Path::new(&dir_path).join("a00001041a0d20");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test5() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["05:53pmNOV4,2100+1day", "-f", &file];

    let expected_error = "job 1 at Fri Nov  5 17:53:00 2100\n";

    run_test_at(&args, "", expected_error, 0);

    let res_file = Path::new(&dir_path).join("a00001041a16f1");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test6() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["15:53NOV4,2100+1day", "-f", &file];

    let expected_error = "job 1 at Fri Nov  5 15:53:00 2100\n";

    run_test_at(&args, "", expected_error, 0);

    let res_file = Path::new(&dir_path).join("a00001041a1679");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test7() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];

    let expected_error = "job 1 at Thu Nov  4 00:00:00 2100\n";

    run_test_at(&args, "", expected_error, 0);

    let res_file = Path::new(&dir_path).join("b00001041a0d20");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test8() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["-t", "210012131200", "-f", &file];

    let expected_error = "job 1 at Mon Dec 13 12:00:00 2100\n";

    run_test_at(&args, "", expected_error, 0);

    let res_file = Path::new(&dir_path).join("a00001041aeb50");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test9() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];

    let expected_error = "job 1 at Thu Nov  4 00:00:00 2100\n";

    run_test_at(&args, "", expected_error, 0);

    // Remove the job by id; nothing on stdout/stderr.
    let args2 = ["-r", "1"];
    run_test_at(&args2, "", "", 0);
}

#[test]
fn test10() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];
    run_test_at(&args, "", "job 1 at Thu Nov  4 00:00:00 2100\n", 0);

    let args2 = ["midnightNOV4,2099", "-f", &file];
    run_test_at(&args2, "", "job 2 at Wed Nov  4 00:00:00 2099\n", 0);

    // POSIX -l format: "%s\t%s\n", at_job_id, <date> (audit #A4).
    let args3 = ["-l", "-q", "b"];
    let expected_output3 = "1\tThu Nov  4 00:00:00 2100\n";
    run_test_at(&args3, expected_output3, "", 0);
}

#[test]
fn test11() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];
    run_test_at(&args, "", "job 1 at Thu Nov  4 00:00:00 2100\n", 0);

    let args2 = ["midnightNOV4,2099", "-f", &file];
    run_test_at(&args2, "", "job 2 at Wed Nov  4 00:00:00 2099\n", 0);

    let args3 = ["-l"];
    let expected_output3 = "1\tThu Nov  4 00:00:00 2100\n2\tWed Nov  4 00:00:00 2099\n";
    run_test_at(&args3, expected_output3, "", 0);
}

#[test]
fn test12() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];
    run_test_at(&args, "", "job 1 at Thu Nov  4 00:00:00 2100\n", 0);

    let args2 = ["midnightNOV4,2099", "-f", &file];
    run_test_at(&args2, "", "job 2 at Wed Nov  4 00:00:00 2099\n", 0);

    let args3 = ["-l", "2"];
    let expected_output3 = "2\tWed Nov  4 00:00:00 2099\n";
    run_test_at(&args3, expected_output3, "", 0);
}

// `at -r` requires at least one at_job_id operand; with none it is a usage
// error (POSIX synopsis `at -r at_job_id...`).
#[test]
fn remove_requires_operand() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");
    std::env::set_var("AT_JOB_DIR", &dir_path);

    let out = plib::testing::run_test_base("at", &["-r".to_string()], b"");
    assert_eq!(out.status.code(), Some(1));
}

// A timespec spread across multiple operands is concatenated and parsed as one,
// per the grammar where white space merely delimits tokens (audit #A1). This is
// equivalent to test1's single-operand "05:53amNOV4,2100".
#[test]
fn test_multi_operand_timespec() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["05:53am", "NOV4,2100", "-f", &file];
    run_test_at(&args, "", "job 1 at Thu Nov  4 05:53:00 2100\n", 0);

    let res_file = Path::new(&dir_path).join("a00001041a0e81");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

// ============================================================================
// SHELL / TZ environment semantics -- audit #B6
// ============================================================================

/// Submit one job with the given env and return the generated script's first
/// line (the `#!` interpreter line) plus the spool filename.
fn submit_and_read_script(env: &[(&str, &str)], args: &[&str]) -> (String, String) {
    let _guard = TEST_MUTEX.lock().unwrap_or_else(|e| e.into_inner());
    let dir = tempdir().expect("tempdir");
    let spool = dir.path().join("spool");
    fs::create_dir_all(&spool).unwrap();
    let allow = dir.path().join("at.allow");
    fs::write(&allow, format!("{}\n", whoami())).unwrap();

    let mut cmd = std::process::Command::new(plib::testing::get_binary_path("at"));
    cmd.args(args)
        .env("AT_JOB_DIR", &spool)
        .env("AT_ALLOW", &allow)
        .env_remove("AT_DENY")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    for (k, v) in env {
        if v.is_empty() && *k == "__UNSET_SHELL" {
            cmd.env_remove("SHELL");
        } else {
            cmd.env(k, v);
        }
    }
    let mut child = cmd.spawn().expect("spawn at");
    use std::io::Write;
    child.stdin.as_mut().unwrap().write_all(b"true\n").unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(
        out.status.success(),
        "at failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let entry = fs::read_dir(&spool)
        .unwrap()
        .next()
        .expect("a job file must be written")
        .unwrap();
    let name = entry.file_name().to_string_lossy().to_string();
    let body = fs::read_to_string(entry.path()).unwrap();
    let first = body.lines().next().unwrap_or_default().to_string();
    (first, name)
}

fn whoami() -> String {
    // Same identity `at` itself resolves: getpwuid(getuid()).
    unsafe {
        let pw = libc::getpwuid(libc::getuid());
        assert!(!pw.is_null(), "no passwd entry for the test user");
        std::ffi::CStr::from_ptr((*pw).pw_name)
            .to_string_lossy()
            .to_string()
    }
}

#[test]
fn test_at_shell_env_selects_the_interpreter() {
    // #B6: POSIX (batch.md 86991-86993) makes SHELL authoritative for the
    // command interpreter, and mandates `sh` when it is unset or null. The
    // passwd shell used to win, so SHELL was consulted only when the passwd
    // entry had none -- and an unset SHELL ran the login shell rather than sh.
    let (shebang, _) =
        submit_and_read_script(&[("SHELL", "/bin/zsh")], &["-m", "now", "+", "1", "hour"]);
    assert_eq!(shebang, "#!/bin/zsh", "SHELL must select the interpreter");
}

#[test]
fn test_at_unset_shell_falls_back_to_sh() {
    // "If the variable is unset or null, sh shall be used" (86992).
    let (shebang, _) =
        submit_and_read_script(&[("__UNSET_SHELL", "")], &["-m", "now", "+", "1", "hour"]);
    assert_eq!(shebang, "#!/bin/sh", "an unset SHELL must yield sh");

    let (shebang, _) = submit_and_read_script(&[("SHELL", "")], &["-m", "now", "+", "1", "hour"]);
    assert_eq!(shebang, "#!/bin/sh", "a null SHELL must yield sh");
}

#[test]
fn test_at_tz_determines_the_absolute_execution_time() {
    // #B6/#A5 (batch.md 86996-87000): the same wall-clock timespec submitted
    // under different TZ values must resolve to different absolute instants.
    // The spool filename encodes the execution minute, so it is the only
    // observable that distinguishes them -- the submission notice is printed
    // in local time and reads identically in every zone.
    fn minute_of(tz: &str) -> u64 {
        let (_, name) = submit_and_read_script(&[("TZ", tz)], &["-t", "202701011200.00"]);
        u64::from_str_radix(&name[6..14], 16).expect("filename encodes the minute in hex")
    }

    let utc = minute_of("UTC");
    let ny = minute_of("America/New_York");
    let tokyo = minute_of("Asia/Tokyo");

    // January: New York is UTC-5, so noon there is 5 hours later in absolute
    // terms; Tokyo is UTC+9, so noon there is 9 hours earlier.
    assert_eq!(
        ny - utc,
        5 * 60,
        "America/New_York must be UTC-5 in January"
    );
    assert_eq!(utc - tokyo, 9 * 60, "Asia/Tokyo must be UTC+9");
}
