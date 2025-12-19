//
// Copyright (c) 2024 Hemi Labs, Inc.
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
use tempfile::{tempdir, TempDir};

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
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();
    let args = ["05:53amNOV4,2100", "-f", &file];

    let expected_output = "job 1 at Thu Nov 04 05:53:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

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
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["05:53amNOV4,2100+30minutes", "-f", &file];

    let expected_output = "job 1 at Thu Nov 04 06:23:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

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
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["05:53amNOV4,2100+1day", "-f", &file];

    let expected_output = "job 1 at Fri Nov 05 05:53:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

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
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file];

    let expected_output = "job 1 at Thu Nov 04 00:00:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

    let res_file = Path::new(&dir_path).join("a00001041a0d20");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test6() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["05:53pmNOV4,2100+1day", "-f", &file];

    let expected_output = "job 1 at Fri Nov 05 17:53:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

    let res_file = Path::new(&dir_path).join("a00001041a16f1");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test7() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["15:53NOV4,2100+1day", "-f", &file];

    let expected_output = "job 1 at Fri Nov 05 15:53:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

    let res_file = Path::new(&dir_path).join("a00001041a1679");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test8() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];

    let expected_output = "job 1 at Thu Nov 04 00:00:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

    let res_file = Path::new(&dir_path).join("b00001041a0d20");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test9() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["-t", "210012131200", "-f", &file];

    let expected_output = "job 1 at Mon Dec 13 12:00:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

    let res_file = Path::new(&dir_path).join("a00001041aeb50");
    assert!(res_file.exists());

    fs::remove_file(res_file).expect("Unable to remove test file");
}

#[test]
fn test10() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];

    let expected_output = "job 1 at Thu Nov 04 00:00:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

    let args2 = ["-r", "1"];
    let expected_output2 = "";
    run_test_at(&args2, expected_output2, "", 0);
}

#[test]
fn test11() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];

    let expected_output = "job 1 at Thu Nov 04 00:00:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

    let args2 = ["midnightNOV4,2099", "-f", &file];

    let expected_output2 = "job 2 at Wed Nov 04 00:00:00 2099\n";

    run_test_at(&args2, expected_output2, "", 0);

    let args3 = ["-l", "-q", "b"];

    let expected_output3 = "1      Thu Nov 04 00:00:00 2100    b\n";
    run_test_at(&args3, expected_output3, "", 0);
}

#[test]
fn test12() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];

    let expected_output = "job 1 at Thu Nov 04 00:00:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

    let args2 = ["midnightNOV4,2099", "-f", &file];

    let expected_output2 = "job 2 at Wed Nov 04 00:00:00 2099\n";

    run_test_at(&args2, expected_output2, "", 0);

    let args3 = ["-l"];

    let expected_output3 =
        "1      Thu Nov 04 00:00:00 2100    b\n2      Wed Nov 04 00:00:00 2099    a\n";
    run_test_at(&args3, expected_output3, "", 0);
}

#[test]
fn test13() {
    let _lock = TEST_MUTEX.lock().unwrap();
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    std::env::set_var("AT_JOB_DIR", &dir_path);
    std::env::set_var("LOGNAME", "root");

    let file = "test_files/at/cmd_for_job.txt".to_string();

    let args = ["midnightNOV4,2100", "-f", &file, "-q", "b"];

    let expected_output = "job 1 at Thu Nov 04 00:00:00 2100\n";

    run_test_at(&args, expected_output, "", 0);

    let args2 = ["midnightNOV4,2099", "-f", &file];

    let expected_output2 = "job 2 at Wed Nov 04 00:00:00 2099\n";

    run_test_at(&args2, expected_output2, "", 0);

    let args3 = ["-l", "2"];

    let expected_output3 = "2      Wed Nov 04 00:00:00 2099    a\n";
    run_test_at(&args3, expected_output3, "", 0);
}
